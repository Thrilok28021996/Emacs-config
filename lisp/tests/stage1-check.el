;;; stage1-check.el --- Workflow and keybinding acceptance checks -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage1-check.el
;;
;; Exit status 0 means every check passed.  The key lists are the contract:
;; one side must be gone, the other must still resolve to its command.

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

(defun my/stage1-global-binding (key)
  "Return the binding of KEY in a fundamental-mode buffer."
  (with-temp-buffer
    (fundamental-mode)
    (key-binding (kbd key))))

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

;;; 1. The workflow still exists

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

;;; 2. Removed bindings

(dolist (key '("C-c b b" "C-c s s" "C-c z" "C-c ?"
               "C-c o a" "C-c o c" "C-c o r" "C-c n k" "C-c n i"
               "C-c k i" "C-c k t" "C-c k p" "C-c k l" "C-c k n" "C-c k r"
               "C-c k a" "C-c k d" "C-c k D" "C-c k P" "C-c k L" "C-c k q"
               "C-c k m" "C-c k b" "C-c k B" "C-c k A" "C-c k o" "C-c k R"
               "C-c k S" "M-<up>" "M-<down>"))
  (my/stage1-check (format "%s is unbound" key)
                   (null (my/stage1-global-binding key))
                   (format "%S" (my/stage1-global-binding key))))

;;; 3. Bindings that must survive

(dolist (spec '((("C-c f r") . recentf-open-files)
                (("C-c s i") . imenu)
                (("C-c w u") . winner-undo)
                (("C-c w U") . winner-redo)
                (("C-c w h") . windmove-left)
                (("C-c w j") . windmove-down)
                (("C-c w k") . windmove-up)
                (("C-c w l") . windmove-right)
                (("C-c w =") . balance-windows)
                (("M-o") . other-window)
                (("C-x C-/") . undo-redo)
                (("C-c i r") . indent-rigidly-right-to-tab-stop)
                (("C-c i l") . indent-rigidly-left-to-tab-stop)
                (("C-c c c") . compile)
                (("C-c c d") . xref-find-references)
                (("C-c c p") . my/python-run-current-file)
                (("C-c c r") . eglot-rename)
                (("C-c c a") . eglot-code-actions)
                (("C-c c i") . eglot-find-implementation)
                (("C-c c h") . eglot-inlay-hints-mode)
                (("C-c c f") . eglot-format-buffer)
                (("C-c e n") . flymake-goto-next-error)
                (("C-c e p") . flymake-goto-prev-error)
                (("C-c e b") . flymake-show-buffer-diagnostics)
                (("C-c g g") . magit-status)
                (("C-c g c") . magit-commit)
                (("C-c g p") . magit-push)
                (("C-c g u") . magit-pull)
                (("C-c g f") . magit-fetch)
                (("C-c g l") . magit-log-current)
                (("C-c g d") . magit-diff-dwim)
                (("C-c g b") . magit-branch)
                (("C-c g a") . magit-blame)
                (("C-c g s") . magit-stash)
                (("C-c t t") . my/toggle-theme)
                (("C-c o I") . org-clock-in)
                (("C-c o O") . org-clock-out)
                (("C-c o R") . org-clock-report)
                (("C-c o i") . org-roam-node-insert)
                (("C-c v a") . conda-env-activate)
                (("C-c v d") . conda-env-deactivate)
                (("C-c k f") . my/roam-find-node)
                (("C-c k v") . my/roam-toggle-buffer)
                (("C-c k g") . my/roam-open-ui)
                (("C-c k s") . my/roam-sync)
                (("C-c k H") . my/help-open-doc)))
  (let* ((key (car (car spec)))
         (expected (cdr spec))
         (actual (my/stage1-global-binding key)))
    (my/stage1-check (format "%s still runs %s" key expected)
                     (eq actual expected)
                     (format "got %S" actual))))

(my/stage1-check "C-c M-g still runs magit-file-dispatch"
                 (eq (my/stage1-global-binding "C-c M-g") 'magit-file-dispatch))

(my/stage1-check "node finder has no capture template"
                 (null org-roam-capture-templates))

(my/stage1-check "C-c p is still a project prefix"
                 (let ((binding (my/stage1-global-binding "C-c p")))
                   (or (keymapp binding)
                       (and (symbolp binding) (fboundp binding)
                            (keymapp (symbol-function binding))))))

(princ (format "\n%d checks, %d failures\n" my/stage1-checks my/stage1-failures))
(when (> my/stage1-failures 0)
  (kill-emacs 1))

;;; stage1-check.el ends here
