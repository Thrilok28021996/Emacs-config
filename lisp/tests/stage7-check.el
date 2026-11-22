;;; stage7-check.el --- Documentation checks: do the docs match the keymaps? -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage7-check.el
;;
;; Prose cannot be checked automatically. Keys can, and keys are what ages
;; worst: a document that teaches a key which no longer exists is worse than no
;; document. So the checks here are bidirectional:
;;
;;   every binding in the keymaps  -> must appear in cheatsheet.org
;;   every key taught in the docs  -> must resolve to a command
;;
;; Anything after the "Historical" heading is deliberately exempt: that section
;; exists to describe things that are gone.

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

(defun my/stage7-text (file)
  "Return the contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my/stage7-before-historical (file)
  "Return FILE's text up to its Historical heading, or all of it.
The Historical section is allowed to name things that no longer exist."
  (let ((text (my/stage7-text file)))
    (if (string-match "^\\* .*[Hh]istorical" text)
        (substring text 0 (match-beginning 0))
      text)))

(defun my/stage7-taught-keys (text)
  "Return the C-c key sequences taught in TEXT as =-quoted= inline markup."
  (let (keys)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "=\\(C-c [^=\n]+\\)=" nil t)
        (let ((key (string-trim (match-string 1))))
          ;; Ignore prose like "C-c bindings".
          (unless (string-match-p "bindings\\|prefix\\|→" key)
            (push key keys)))))
    (delete-dups (nreverse keys))))

(defun my/stage7-resolves-p (key)
  "Return non-nil when KEY is bound.
Checked in an Org buffer, because most of the keys these documents teach
(C-c C-c, C-c C-t, C-c C-x ...) live in `org-mode-map', not the global map, and
a global-only lookup would report them as dead."
  (condition-case nil
      (with-temp-buffer
        (org-mode)
        (not (null (key-binding (kbd key)))))
    (error nil)))

;;;; 1. Both documents exist and carry the structure the checks rely on

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial))
  (my/stage7-check (format "%s exists" (file-name-nondirectory file))
                   (file-exists-p file)))

(my/stage7-check "the cheatsheet has a Historical section"
                 (string-match-p "^\\* .*Historical" (my/stage7-text my/stage7-cheatsheet)))
(my/stage7-check "the tutorial has a Historical section"
                 (string-match-p "^\\* .*Historical" (my/stage7-text my/stage7-tutorial)))

;;;; 2. The generated key table is present and covers the live keymaps

(let* ((text (my/stage7-text my/stage7-cheatsheet))
       (rows (with-temp-buffer
               (insert text)
               (goto-char (point-min))
               (how-many "^| =C-c "))))
  (my/stage7-check "the generated key table is present"
                   (string-match-p "^#\\+BEGIN: my-keys" text))
  (my/stage7-check "and has a row per binding"
                   (>= rows 60) (format "%d rows" rows))
  (my/stage7-check "and states how many bindings it lists"
                   (string-match-p "bindings under C-c" text)))

;;;; 3. Every live C-c binding appears in the cheatsheet

(let* ((text (my/stage7-text my/stage7-cheatsheet))
       (rows (my/help--rows))
       (missing (cl-remove-if (lambda (row) (string-match-p (regexp-quote (car row)) text))
                              rows)))
  (my/stage7-check "every live C-c binding is documented in the cheatsheet"
                   (null missing)
                   (format "%d missing, first: %S" (length missing) (car missing))))

;;;; 4. Every key the documents teach actually exists

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial))
  (let* ((keys (my/stage7-taught-keys (my/stage7-before-historical file)))
         (dead (cl-remove-if #'my/stage7-resolves-p keys)))
    (my/stage7-check (format "every key taught in %s resolves"
                             (file-name-nondirectory file))
                     (null dead)
                     (format "%d taught, dead: %S" (length keys) dead))
    (my/stage7-check (format "%s teaches a useful number of keys"
                             (file-name-nondirectory file))
                     (> (length keys) 20)
                     (format "%d keys" (length keys)))))

;;;; 5. The documents actually cover the system they describe

(let ((cheatsheet (my/stage7-text my/stage7-cheatsheet))
      (tutorial (my/stage7-text my/stage7-tutorial)))
  (dolist (topic '(("C-c k p" . "creating projects")
                   ("C-c k D" . "the home dashboard")
                   ("C-c k b" . "refreshing blocks")
                   ("C-c k A" . "attaching files")
                   ("C-c k v" . "backlinks")
                   ("kanban" . "kanban")
                   ("org-ql" . "org-ql queries")
                   ("nov-mode" . "EPUB reading")
                   ("backup" . "backups")
                   ("emacs-sync.sh" . "the gates")))
    (my/stage7-check (format "the cheatsheet covers %s" (cdr topic))
                     (string-match-p (regexp-quote (car topic)) cheatsheet))
    (my/stage7-check (format "the tutorial covers %s" (cdr topic))
                     (string-match-p (regexp-quote (car topic)) tutorial))))

;;;; 6. The documents point at each other instead of duplicating

(dolist (file (list my/stage7-cheatsheet my/stage7-tutorial))
  (let ((text (my/stage7-text file)))
    (dolist (target '("ORG-PKM.org" "VERIFICATION.org"))
      (my/stage7-check (format "%s points at %s" (file-name-nondirectory file) target)
                       (string-match-p (regexp-quote target) text)))))

(my/stage7-check "ORG-PKM.org exists" (file-exists-p (expand-file-name "ORG-PKM.org" user-emacs-directory)))
(my/stage7-check "VERIFICATION.org exists" (file-exists-p (expand-file-name "VERIFICATION.org" user-emacs-directory)))
(my/stage7-check "VERIFICATION.org documents the gate command"
                 (string-match-p "emacs-sync.sh verify"
                                 (my/stage7-text (expand-file-name "VERIFICATION.org" user-emacs-directory))))

(princ (format "\n%d checks, %d failures\n" my/stage7-checks my/stage7-failures))
(when (> my/stage7-failures 0)
  (kill-emacs 1))

;;; stage7-check.el ends here
