;;; stage3-check.el --- Headless acceptance checks for Stage 3 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage3-check.el
;;
;; Creates two knowledge files, links one to the other, and checks that the
;; index resolves the link as a backlink.  Then deletes the index entirely and
;; proves the same index comes back from the Org files alone.

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

(defvar my/stage3-note-a (expand-file-name "zz-stage3-alpha.org"
                                           (my/knowledge-subdir "notes/knowledge/software-engineering")))
(defvar my/stage3-note-b (expand-file-name "zz-stage3-beta.org"
                                           (my/knowledge-subdir "notes/knowledge/software-engineering")))
(defvar my/stage3-proj (expand-file-name "zz-stage3-project.org"
                                         (my/knowledge-subdir "notes/projects")))

(defun my/stage3-query-count (sql &rest args)
  "Run a count query SQL with ARGS and return the number."
  (or (caar (apply #'org-roam-db-query sql args)) 0))

(defun my/stage3-ids ()
  "Return the file-level IDs of the two notes, creating the files if needed."
  (list (my/roam-node-id my/stage3-note-a)
        (my/roam-node-id my/stage3-note-b)))

(defun my/stage3-cleanup ()
  "Remove the fixtures and re-index, leaving the tree as found."
  (dolist (f (list my/stage3-note-a my/stage3-note-b my/stage3-proj))
    (when (get-file-buffer f)
      (with-current-buffer (get-file-buffer f)
        (set-buffer-modified-p nil)
        (kill-buffer)))
    (when (file-exists-p f) (delete-file f)))
  (org-roam-db-sync 'force))

;;;; 0. Start clean

(my/stage3-cleanup)

;;;; 1. File creation gives documents identity

(my/new-knowledge-note "software-engineering" "zz stage3 alpha")
(my/new-knowledge-note "software-engineering" "zz stage3 beta")
(my/new-project "zz stage3 project")

(let* ((ids (my/stage3-ids))
       (a (car ids))
       (b (cadr ids)))
  (my/stage3-check "a new knowledge note carries a file-level ID"
                   (and a (stringp a) (> (length a) 20)) (format "%S" a))
  (my/stage3-check "the ID lives in a drawer before the first heading"
                   (with-current-buffer (find-file-noselect my/stage3-note-a)
                     (save-excursion
                       (goto-char (point-min))
                       (and (= (org-outline-level) 0)
                            (looking-at-p ":PROPERTIES:")))))
  (my/stage3-check "two notes get distinct IDs" (not (equal a b)))
  (my/stage3-check "a project file gets its own ID"
                   (let ((id (my/roam-node-id my/stage3-proj))) (and id (not (equal id a))))))

;;;; 2. The index sees them

(org-roam-db-sync 'force)
(my/stage3-check "the project is indexed as a file-level node"
                 (= 1 (my/stage3-query-count
                       [:select (funcall count *) :from nodes :where (= id $s1)]
                       (my/roam-node-id my/stage3-proj))))
(my/stage3-check "dashboards are not indexed"
                 (= 0 (my/stage3-query-count
                       [:select (funcall count *) :from files
                        :where (like file $s1)] "%dashboards%")))

(let* ((a (nth 0 (my/stage3-ids)))
       (b (nth 1 (my/stage3-ids)))
       (node (org-roam-node-from-id a)))
  (my/stage3-check "a node resolves from its ID" (and node (org-roam-node-p node)))
  (my/stage3-check "and carries the file title"
                   (equal (org-roam-node-title node) "zz stage3 alpha")
                   (format "%S" (and node (org-roam-node-title node))))
  (my/stage3-check "the node list contains both notes"
                   (let ((titles (mapcar #'org-roam-node-title (org-roam-node-list))))
                     (and (member "zz stage3 alpha" titles)
                          (member "zz stage3 beta" titles)))))

;;;; 3. A link creates a backlink

(let* ((a (nth 0 (my/stage3-ids)))
       (b (nth 1 (my/stage3-ids))))
  ;; Write the link the way org-roam-node-insert would: an id: link.
  (with-current-buffer (find-file-noselect my/stage3-note-a)
    (goto-char (point-max))
    (insert (format "\n* Link\n- %s\n" (org-link-make-string (concat "id:" b) "zz stage3 beta")))
    (save-buffer))
  (org-roam-db-sync 'force)
  (my/stage3-check "the link is indexed with both endpoints"
                   (= 1 (my/stage3-query-count
                         [:select (funcall count *) :from links
                          :where (and (= source $s1) (= dest $s2))]
                         a b)))
  (my/stage3-check "the target has a backlink back to the source"
                   (= 1 (my/stage3-query-count
                         [:select (funcall count *) :from links :where (= dest $s1)]
                         b)))
  (my/stage3-check "the link resolves to the target's file"
                   (let ((files (org-roam-db-query
                                 [:select dest :from links :where (= source $s1)] a)))
                     (and files (equal (caar files) b)))))

;;;; 4. The index is disposable, the knowledge is not

(let ((before (mapcar (lambda (f) (cons f (md5 (with-temp-buffer
                                                 (insert-file-contents-literally f)
                                                 (buffer-string)))))
                      (list my/stage3-note-a my/stage3-note-b my/stage3-proj)))
      nodes links)
  (setq nodes (my/stage3-query-count [:select (funcall count *) :from nodes])
        links (my/stage3-query-count [:select (funcall count *) :from links]))
  (org-roam-db--close-all)
  (when (file-exists-p org-roam-db-location) (delete-file org-roam-db-location))
  (my/stage3-check "the index is gone before rebuilding" (not (file-exists-p org-roam-db-location)))
  (org-roam-db-sync 'force)
  (my/stage3-check "rebuilding restores the same node count"
                   (= nodes (my/stage3-query-count [:select (funcall count *) :from nodes]))
                   (format "was %s now %s" nodes (my/stage3-query-count [:select (funcall count *) :from nodes])))
  (my/stage3-check "rebuilding restores the same link count"
                   (= links (my/stage3-query-count [:select (funcall count *) :from links]))
                   (format "was %s now %s" links (my/stage3-query-count [:select (funcall count *) :from links])))
  (my/stage3-check "not one knowledge byte changed"
                   (cl-every (lambda (pair)
                               (equal (cdr pair)
                                      (md5 (with-temp-buffer
                                             (insert-file-contents-literally (car pair))
                                             (buffer-string)))))
                             before)))

;;;; 5. Editing a note without an ID does not invent one

(let ((plain (expand-file-name "zz-stage3-plain.org"
                               (my/knowledge-subdir "notes/knowledge/software-engineering"))))
  (with-temp-file plain (insert "#+TITLE: plain\n* A heading\n"))
  (with-current-buffer (find-file-noselect plain)
    (my/stage3-check "a heading does not get an ID automatically"
                     (null (save-excursion (goto-char (point-min)) (org-id-get))))
    (let ((id (my/roam-ensure-file-id)))
      (my/stage3-check "my/roam-ensure-file-id adds one on request" (and id (stringp id)))
      (my/stage3-check "and it survives a re-read"
                       (equal id (my/roam-node-id plain)))))
  (kill-buffer (get-file-buffer plain))
  (delete-file plain))

;;;; 6. Cleanup

(my/stage3-cleanup)
(my/stage3-check "fixtures gone"
                 (and (not (file-exists-p my/stage3-note-a))
                      (not (file-exists-p my/stage3-note-b))
                      (not (file-exists-p my/stage3-proj))))
(my/stage3-check "index no longer references the fixtures"
                 (= 0 (my/stage3-query-count
                       [:select (funcall count *) :from files :where (like file $s1)]
                       "%zz-stage3%")))

(princ (format "\n%d checks, %d failures\n" my/stage3-checks my/stage3-failures))
(when (> my/stage3-failures 0)
  (kill-emacs 1))

;;; stage3-check.el ends here
