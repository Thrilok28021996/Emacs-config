;;; stage3-check.el --- Headless acceptance checks for Org-roam -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage3-check.el
;;
;; The whole run uses a temporary tree, so it never writes into ~/knowledge.
;; It checks that a file ID creates a node, a link creates a backlink, and the
;; index can be deleted and rebuilt from the Org files alone.

;;; Code:

(require 'cl-lib)
(require 'md5)

(defvar my/stage3-failures 0)
(defvar my/stage3-checks 0)

(defun my/stage3-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage3-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage3-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defun my/stage3-query-count (sql &rest args)
  "Run a count query SQL with ARGS and return the number."
  (or (caar (apply #'org-roam-db-query sql args)) 0))

(let* ((tree (make-temp-file "zz-stage3-" t))
       (old-directory org-roam-directory)
       (old-db org-roam-db-location)
       (a (expand-file-name "alpha.org" tree))
       (b (expand-file-name "beta.org" tree))
       (dash (expand-file-name "dashboards/ignored.org" tree)))
  (unwind-protect
      (progn
        (setq org-roam-directory tree
              org-roam-db-location (expand-file-name "index.db" tree))
        (make-directory (file-name-directory dash) t)
        (dolist (pair (list (cons a "#+TITLE: alpha\n* The idea\n")
                            (cons b "#+TITLE: beta\n* The idea\n")
                            (cons dash ":PROPERTIES:\n:ID: dashboard-id\n:END:\n#+TITLE: ignored\n")))
          (with-temp-file (car pair) (insert (cdr pair))))
        (let ((ia (with-current-buffer (find-file-noselect a) (my/roam-ensure-file-id)))
              (ib (with-current-buffer (find-file-noselect b) (my/roam-ensure-file-id))))
          (with-current-buffer (find-file-noselect b) (save-buffer))
          (org-roam-db-sync 'force)

          (my/stage3-check "a file ID becomes a node"
                           (and (stringp ia) (> (length ia) 20)
                                (org-roam-node-from-id ia)))
          (my/stage3-check "two notes get distinct IDs" (not (equal ia ib)))
          (my/stage3-check "the ID sits before the first heading"
                           (with-current-buffer (find-file-noselect a)
                             (save-excursion
                               (goto-char (point-min))
                               (and (looking-at-p ":PROPERTIES:")
                                    (= 0 (org-outline-level))))))
          (my/stage3-check "dashboards are excluded from the index"
                           (= 0 (my/stage3-query-count
                                 [:select (funcall count *) :from nodes :where (= id $s1)]
                                 "dashboard-id")))

          (with-current-buffer (find-file-noselect a)
            (goto-char (point-max))
            (insert (format "\n* Link\n- %s\n"
                            (org-link-make-string (concat "id:" ib) "beta")))
            (save-buffer))
          (org-roam-db-sync 'force)
          (my/stage3-check "a link is indexed with both endpoints"
                           (= 1 (my/stage3-query-count
                                 [:select (funcall count *) :from links
                                  :where (and (= source $s1) (= dest $s2))]
                                 ia ib)))
          (my/stage3-check "the target has a backlink"
                           (= 1 (my/stage3-query-count
                                 [:select (funcall count *) :from links :where (= dest $s1)]
                                 ib)))

          (let ((before-nodes (my/stage3-query-count [:select (funcall count *) :from nodes]))
                (before-links (my/stage3-query-count [:select (funcall count *) :from links]))
                (checksum (with-temp-buffer
                            (insert-file-contents-literally a)
                            (md5 (buffer-string)))))
            (org-roam-db--close-all)
            (delete-file org-roam-db-location)
            (my/stage3-check "the index is gone before rebuilding"
                             (not (file-exists-p org-roam-db-location)))
            (org-roam-db-sync 'force)
            (my/stage3-check "rebuilding restores the node count"
                             (= before-nodes (my/stage3-query-count
                                              [:select (funcall count *) :from nodes])))
            (my/stage3-check "rebuilding restores the link count"
                             (= before-links (my/stage3-query-count
                                              [:select (funcall count *) :from links])))
            (my/stage3-check "the note itself did not change"
                             (equal checksum
                                    (with-temp-buffer
                                      (insert-file-contents-literally a)
                                      (md5 (buffer-string))))))))
    (org-roam-db--close-all)
    (setq org-roam-directory old-directory
          org-roam-db-location old-db)
    (dolist (buf (list (get-file-buffer a) (get-file-buffer b) (get-file-buffer dash)))
      (when buf (with-current-buffer buf (set-buffer-modified-p nil) (kill-buffer))))
    (delete-directory tree t)))

(princ (format "\n%d checks, %d failures\n" my/stage3-checks my/stage3-failures))
(when (> my/stage3-failures 0)
  (kill-emacs 1))

;;; stage3-check.el ends here
