;;; stage1-check.el --- Headless acceptance checks for Stage 1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Run it, don't read it and hope:
;;
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage1-check.el
;;
;; Exit status 0 means every check passed.  A fixture project file is created
;; and deleted again, so the knowledge tree is left exactly as found.

;;; Code:

(require 'cl-lib)

(defvar my/stage1-failures 0)
(defvar my/stage1-checks 0)

(defun my/stage1-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage1-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage1-failures)
    (princ (format "FAIL  %s%s\n" label
                   (if detail (format "  [%s]" detail) "")))))

(defun my/stage1-state-names ()
  "TODO state names as Org actually reports them, fast keys stripped."
  (seq-remove (lambda (s) (equal s "|"))
              (mapcar (lambda (s) (car (split-string s "(")))
                      (apply #'append (mapcar #'cdr org-todo-keywords)))))

(defun my/stage1-terminal-states ()
  "State names that come after the '|' separator."
  (let ((all (mapcar (lambda (s) (car (split-string s "(")))
                     (apply #'append (mapcar #'cdr org-todo-keywords)))))
    (cdr (member "|" all))))

;;; 1. Workflow

(let* ((expected '("BACKLOG" "TODO" "NEXT" "DOING" "WAITING" "REVIEW" "DONE" "CANCELLED"))
       (states (my/stage1-state-names)))
  (my/stage1-check "org-todo-keywords has the 8 states in order"
                   (equal states expected) (format "%S" states))
  (my/stage1-check "DONE and CANCELLED are terminal"
                   (equal (my/stage1-terminal-states) '("DONE" "CANCELLED"))
                   (format "%S" (my/stage1-terminal-states))))

(my/stage1-check "the #+TODO line matches the global workflow"
                 (equal (my/knowledge-todo-line)
                        (format "#+TODO: %s | DONE(d) CANCELLED(c)"
                                (string-join '("BACKLOG(b)" "TODO(t)" "NEXT(n)"
                                               "DOING(o)" "WAITING(w)" "REVIEW(r)") " ")))
                 (my/knowledge-todo-line))

;;; 2. Project creation

(defvar my/stage1-fixture
  (expand-file-name "zz-stage1-fixture.org" (my/knowledge-subdir "notes/projects")))

(when (file-exists-p my/stage1-fixture) (delete-file my/stage1-fixture))

(my/new-project "zz stage1 fixture")
(my/stage1-check "my/new-project creates notes/projects/<slug>.org"
                 (file-exists-p my/stage1-fixture))
(with-temp-buffer
  (insert-file-contents my/stage1-fixture)
  (let ((text (buffer-string)))
    (my/stage1-check "created file carries the #+TODO line"
                     (string-match-p (regexp-quote (my/knowledge-todo-line)) text))
    (my/stage1-check "created file carries a kanban block"
                     (string-match-p "#\\+BEGIN: kanban" text))
    (my/stage1-check "created file has its Tasks heading"
                     (string-match-p "^\\* Tasks$" text))))
;;; 2b. Learning subjects use the same code path but deserve their own check

(defvar my/stage1-subject
  (expand-file-name "zz-stage1-subject.org" (my/knowledge-subdir "notes/learning")))

(when (file-exists-p my/stage1-subject) (delete-file my/stage1-subject))
(my/new-learning-subject "zz stage1 subject")
(my/stage1-check "my/new-learning-subject creates notes/learning/<slug>.org"
                 (file-exists-p my/stage1-subject))
(with-temp-buffer
  (insert-file-contents my/stage1-subject)
  (my/stage1-check "the subject file mirrors the workflow and has a board"
                   (and (string-match-p (regexp-quote (my/knowledge-todo-line)) (buffer-string))
                        (string-match-p "#\\+BEGIN: kanban" (buffer-string))
                        (string-match-p "^\\* Board$" (buffer-string)))))
(my/stage1-check "a new subject is an agenda file"
                 (org-agenda-file-p my/stage1-subject))
(when (get-file-buffer my/stage1-subject)
  (with-current-buffer (get-file-buffer my/stage1-subject)
    (set-buffer-modified-p nil)
    (kill-buffer)))
(when (file-exists-p my/stage1-subject) (delete-file my/stage1-subject))

(my/stage1-check "my/new-project refuses to overwrite"
                 (condition-case nil
                     (progn (my/new-project "zz stage1 fixture") nil)
                   (user-error t)))

;;; 3. Discovery without config edits

(my/stage1-check "new project file is an agenda file with no config change"
                 (org-agenda-file-p my/stage1-fixture))
(my/stage1-check "inbox.org is an agenda file"
                 (org-agenda-file-p (expand-file-name "inbox.org" my/knowledge-dir)))
(my/stage1-check "dashboards are not agenda files"
                 (not (org-agenda-file-p (my/knowledge-subdir "dashboards/home.org"))))
(my/stage1-check "learning files are discovered from the directory"
                 (equal (my/knowledge-learning-files)
                        (my/knowledge-files-in "notes/learning")))

;;; 4. Capture targets

(defun my/stage1-check-target (key)
  "Return the expanded file for capture template KEY."
  (let* ((template (assoc key org-capture-templates))
         (target (nth 3 template))
         (kind (car target)))
    (when (memq kind '(file+headline file+olp+datetree file+olp file+regexp))
      (org-capture-expand-file (nth 1 target)))))

(dolist (spec '(("i" . "inbox.org")
                ("j" . "notes/daily/journal.org")
                ("r" . "notes/references/tracking.org")))
  (let* ((key (car spec))
         (expected (my/knowledge-subdir (cdr spec)))
         (actual (my/stage1-check-target key)))
    (my/stage1-check (format "capture template %S resolves to %s" key (cdr spec))
                     (equal actual expected)
                     (format "%S" actual))
    (my/stage1-check (format "capture target for %S exists" key)
                     (file-exists-p expected))))

;; Function-valued file specs get resolved the same way a real capture would.
;; `cl-letf' on the function cell, because a plain `let' cannot rebind a
;; function name for the interpreter.
(cl-letf (((symbol-function 'completing-read)
           (lambda (&rest _) "notes/projects/zz-stage1-fixture.org")))
  (my/stage1-check "project capture target resolves via the picker"
                   (equal (my/capture-project-file) my/stage1-fixture)))

;;; 5. Help buffer

(my/help-bindings)
(with-current-buffer "*My Keys*"
  (let ((text (buffer-string)))
    (my/stage1-check "C-c ? lists the knowledge bindings"
                     (and (string-match-p "C-c k p" text)
                          (string-match-p "C-c k l" text)
                          (string-match-p "C-c k a" text)))
    (my/stage1-check "help buffer documents the binding count"
                     (string-match-p "bindings under C-c" text))
    (kill-buffer)))

;;; 6. Cleanup

(when (get-file-buffer my/stage1-fixture)
  (with-current-buffer (get-file-buffer my/stage1-fixture)
    (set-buffer-modified-p nil)
    (kill-buffer)))
(when (file-exists-p my/stage1-fixture) (delete-file my/stage1-fixture))
(my/stage1-check "fixture removed, knowledge tree unchanged"
                 (not (file-exists-p my/stage1-fixture)))

(princ (format "\n%d checks, %d failures\n" my/stage1-checks my/stage1-failures))
(when (> my/stage1-failures 0)
  (kill-emacs 1))

;;; stage1-check.el ends here
