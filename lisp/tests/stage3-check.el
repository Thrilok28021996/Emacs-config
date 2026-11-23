;;; stage3-check.el --- Headless acceptance checks for Stage 3 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage3-check.el
;;
;; The zettelkasten has no capture template on purpose: a template cannot
;; know the next number.  So this stage checks the manual path a zettel
;; actually takes — write the file, press C-c k e (my/roam-ensure-file-id)
;; for the ID drawer — and that the index picks the zettel up.  Then it
;; links two zettels, checks the backlink, deletes the index entirely and
;; proves the same index comes back from the Org files alone.  Finally it
;; proves the projects tree in ~/projects is not part of the roam index.

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
                                           (expand-file-name "main-box" my/knowledge-dir)))
(defvar my/stage3-note-b (expand-file-name "zz-stage3-beta.org"
                                           (expand-file-name "main-box" my/knowledge-dir)))
(defvar my/stage3-proj (expand-file-name "zz-stage3-project.org"
                                         (expand-file-name "projects" my/projects-dir)))

(defun my/stage3-query-count (sql &rest args)
  "Run a count query SQL with ARGS and return the number."
  (or (caar (apply #'org-roam-db-query sql args)) 0))

(defun my/stage3-ids ()
  "Return the file-level IDs of the two zettels, or nil before they exist."
  (list (my/roam-node-id my/stage3-note-a)
        (my/roam-node-id my/stage3-note-b)))

(defun my/stage3-cleanup ()
  "Remove the fixtures and re-index, leaving the zettelkasten as found."
  (dolist (f (list my/stage3-note-a my/stage3-note-b my/stage3-proj))
    (when (get-file-buffer f)
      (with-current-buffer (get-file-buffer f)
        (set-buffer-modified-p nil)
        (kill-buffer)))
    (when (file-exists-p f) (delete-file f)))
  (org-roam-db-sync 'force))

;;;; 0. Start clean

(my/stage3-cleanup)

;;;; 1. A hand-written zettel gets identity from C-c k e

(with-temp-file my/stage3-note-a
  (insert "#+TITLE: zz stage3 alpha\n\n* A heading\n"))
(with-temp-file my/stage3-note-b
  (insert "#+TITLE: zz stage3 beta\n\n* A heading\n"))
(with-current-buffer (find-file-noselect my/stage3-note-a) (my/roam-ensure-file-id))
(with-current-buffer (find-file-noselect my/stage3-note-b) (my/roam-ensure-file-id))
(my/new-project "zz stage3 project")

(let* ((ids (my/stage3-ids))
       (a (car ids))
       (b (cadr ids)))
  (my/stage3-check "a hand-written zettel carries a file-level ID after C-c k e"
                   (and a (stringp a) (> (length a) 20)) (format "%S" a))
  (my/stage3-check "the ID lives in a drawer before the first heading"
                   (with-current-buffer (find-file-noselect my/stage3-note-a)
                     (save-excursion
                       (goto-char (point-min))
                       (and (= (org-outline-level) 0)
                            (looking-at-p ":PROPERTIES:")))))
  (my/stage3-check "two zettels get distinct IDs" (not (equal a b)))
  (my/stage3-check "a project file still carries an ID of its own"
                   (let ((id (my/roam-node-id my/stage3-proj))) (and id (not (equal id a))))))

;;;; 2. The index sees the zettels — and nothing outside the zettelkasten

(org-roam-db-sync 'force)
(my/stage3-check "the zettelkasten files are indexed"
                 (= 2 (my/stage3-query-count
                       [:select (funcall count *) :from files
                        :where (like file $s1)] "%zz-stage3%")))
(my/stage3-check "nothing in the projects tree is indexed"
                 (= 0 (my/stage3-query-count
                       [:select (funcall count *) :from files
                        :where (like file $s1)] (concat "%" (file-name-as-directory
                                                            (directory-file-name my/projects-dir)) "%"))))
(my/stage3-check "the projects fixture is not a node"
                 (= 0 (my/stage3-query-count
                       [:select (funcall count *) :from nodes
                        :where (= id $s1)]
                       (my/roam-node-id my/stage3-proj))))

(let* ((a (nth 0 (my/stage3-ids)))
       (b (nth 1 (my/stage3-ids)))
       (node (org-roam-node-from-id a)))
  (my/stage3-check "a node resolves from its ID" (and node (org-roam-node-p node)))
  (my/stage3-check "and carries the file title"
                   (equal (org-roam-node-title node) "zz stage3 alpha")
                   (format "%S" (and node (org-roam-node-title node))))
  (my/stage3-check "the node list contains both zettels"
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

;;;; 4. The index is disposable, the zettelkasten is not

(let ((before (mapcar (lambda (f) (cons f (md5 (with-temp-buffer
                                                 (insert-file-contents-literally f)
                                                 (buffer-string)))))
                      (list my/stage3-note-a my/stage3-note-b)))
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
  (my/stage3-check "not one zettel byte changed"
                   (cl-every (lambda (pair)
                               (equal (cdr pair)
                                      (md5 (with-temp-buffer
                                             (insert-file-contents-literally (car pair))
                                             (buffer-string)))))
                             before)))

;;;; 5. Editing a note without an ID does not invent one

(let ((plain (expand-file-name "zz-stage3-plain.org"
                               (expand-file-name "main-box" my/knowledge-dir))))
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
