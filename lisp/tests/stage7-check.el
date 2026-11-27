;;; stage7-check.el --- Documentation checks against the live keymaps -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage7-check.el
;;
;; Checks go both ways: every live C-c binding is listed in the cheatsheet,
;; and no document teaches a key that was removed.  The master table is written
;; by hand now, so this is what keeps it honest.

;;; Code:

(require 'cl-lib)

(defvar my/stage7-failures 0)
(defvar my/stage7-checks 0)

(defun my/stage7-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage7-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage7-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defvar my/stage7-cheatsheet (expand-file-name "cheatsheet.org" user-emacs-directory))
(defvar my/stage7-tutorial (expand-file-name "tutorial.org" user-emacs-directory))
(defvar my/stage7-architecture (expand-file-name "ORG-PKM.org" user-emacs-directory))
(defvar my/stage7-verification (expand-file-name "VERIFICATION.org" user-emacs-directory))

(defun my/stage7-text (file)
  "Return the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my/stage7-walk (map prefix acc)
  "Collect (KEY . COMMAND) pairs from MAP under PREFIX into ACC."
  (map-keymap
   (lambda (event definition)
     (let ((key (vconcat prefix (vector event))))
       (cond
        ((keymapp definition)
         (let ((sub (if (and (symbolp definition) (fboundp definition))
                        (symbol-function definition)
                      definition)))
           (when (keymapp sub) (setq acc (my/stage7-walk sub key acc)))))
        ((symbolp definition) (push (cons key definition) acc)))))
   map)
  acc)

(defun my/stage7-cc-keys ()
  "Return the key descriptions of every live C-c binding."
  (let (keys)
    (dolist (pair (my/stage7-walk (current-global-map) [] nil))
      (let ((desc (key-description (car pair))))
        (when (string-prefix-p "C-c" desc) (push desc keys))))
    (sort keys #'string<)))

(defun my/stage7-taught-keys (text)
  "Return C-c sequences taught in TEXT as =-quoted= inline markup."
  (let (keys)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "=\\(C-c [^=\n]+\\)=" nil t)
        (let ((key (string-trim (match-string 1))))
          (unless (string-match-p "bindings\\|prefix\\|→" key) (push key keys)))))
    (delete-dups (nreverse keys))))

(defun my/stage7-resolves-p (key)
  "Return non-nil when KEY resolves in an Org buffer."
  (condition-case nil
      (with-temp-buffer
        (org-mode)
        (not (null (key-binding (kbd key)))))
    (error nil)))

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial
                    my/stage7-architecture my/stage7-verification
                    (expand-file-name "PLAN.md" user-emacs-directory)))
  (my/stage7-check (format "%s exists" (file-name-nondirectory file))
                   (file-exists-p file)))

(let* ((text (my/stage7-text my/stage7-cheatsheet))
       (keys (my/stage7-cc-keys))
       (missing (cl-remove-if (lambda (key) (string-match-p (regexp-quote key) text)) keys)))
  (my/stage7-check "every live C-c binding is in the cheatsheet"
                   (null missing)
                   (format "%d missing, first: %S" (length missing) (car missing)))
  (my/stage7-check "the cheatsheet states the binding count"
                   (string-match-p (format "%d bindings under C-c" (length keys)) text)))

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial))
  (let* ((keys (my/stage7-taught-keys (my/stage7-text file)))
         (dead (cl-remove-if #'my/stage7-resolves-p keys)))
    (my/stage7-check (format "every key taught in %s resolves"
                             (file-name-nondirectory file))
                     (null dead)
                     (format "%d taught, dead: %S" (length keys) dead))
    (my/stage7-check (format "%s teaches a useful number of keys"
                             (file-name-nondirectory file))
                     (> (length keys) 20)
                     (format "%d keys" (length keys)))))

(let ((case-fold-search nil))
  (dolist (file (list my/stage7-cheatsheet my/stage7-tutorial
                      my/stage7-architecture my/stage7-verification
                      (expand-file-name "README.md" user-emacs-directory)
                      (expand-file-name "PLAN.md" user-emacs-directory)))
    (let ((text (my/stage7-text file)))
      (dolist (key '("C-c b b" "C-c s s" "C-c z" "C-c ?" "C-c o a" "C-c o c"
                     "C-c o r" "C-c n k" "C-c n i" "C-c k i" "C-c k t" "C-c k p"
                     "C-c k l" "C-c k n" "C-c k r" "C-c k a" "C-c k d" "C-c k D"
                     "C-c k P" "C-c k L" "C-c k q" "C-c k m" "C-c k b" "C-c k B"
                     "C-c k A" "C-c k o" "C-c k R" "C-c k S" "M-<up>" "M-<down>"))
        (my/stage7-check (format "%s omits removed key %s"
                                 (file-name-nondirectory file) key)
                         (null (string-match-p (regexp-quote key) text))))
      (dolist (stale '("~/knowledge/notes/" "~/Documents/garden/" "my/new-project"
                       "my/capture-inbox" "my/help-bindings" "my/dashboard-"
                       "my/kanban-" "my/reading-attach" "org-kanban"))
        (my/stage7-check (format "%s omits stale reference %s"
                                 (file-name-nondirectory file) stale)
                         (null (string-match-p (regexp-quote stale) text))))))
  (dolist (file (cons (expand-file-name "init.el" user-emacs-directory)
                      (file-expand-wildcards
                       (expand-file-name "lisp/my-*.el" user-emacs-directory))))
    (let ((text (my/stage7-text file)))
      (dolist (stale '("notes/knowledge/" "notes/projects/" "notes/learning/"
                       "notes/references/" "notes/daily/" "~/Documents/garden/"))
        (my/stage7-check (format "%s omits stale config path %s"
                                 (file-name-nondirectory file) stale)
                         (null (string-match-p (regexp-quote stale) text)))))))

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial))
  (let ((text (my/stage7-text file)))
    (dolist (target '("ORG-PKM.org" "VERIFICATION.org"))
      (my/stage7-check (format "%s points at %s"
                               (file-name-nondirectory file) target)
                       (string-match-p (regexp-quote target) text)))))

(my/stage7-check "VERIFICATION.org documents the gate command"
                 (string-match-p "emacs-sync.sh verify"
                                 (my/stage7-text my/stage7-verification)))

(princ (format "\n%d checks, %d failures\n" my/stage7-checks my/stage7-failures))
(when (> my/stage7-failures 0)
  (kill-emacs 1))

;;; stage7-check.el ends here
