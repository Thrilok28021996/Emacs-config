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
;; The whole documents are checked. Removed software is deleted, not parked in a
;; section that would have to be exempted from checking.

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

(defun my/stage7-checkable (file)
  "Return the text of FILE that the checks apply to.
The whole document: dead lessons are gone rather than parked in a section that
would need to be exempted from checking."
  (my/stage7-text file))

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

(my/stage7-check "neither document keeps a section describing removed software"
                 (and (null (string-match-p "^\\* .*[Hh]istorical" (my/stage7-text my/stage7-cheatsheet)))
                      (null (string-match-p "^\\* .*[Hh]istorical" (my/stage7-text my/stage7-tutorial)))))
(my/stage7-check "the cheatsheet no longer names packages that are not loaded"
                 (let ((text (downcase (my/stage7-text my/stage7-cheatsheet))))
                   (cl-every (lambda (name) (null (string-match-p name text)))
                             '("vertico" "corfu" "embark" "marginalia"
                               "orderless" "yasnippet" "org-modern" "olivetti"
                               "dape" "apheleia" "gptel" "org-pomodoro"))))

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
  (let* ((keys (my/stage7-taught-keys (my/stage7-checkable file)))
         (dead (cl-remove-if #'my/stage7-resolves-p keys)))
    (my/stage7-check (format "every key taught in %s resolves"
                             (file-name-nondirectory file))
                     (null dead)
                     (format "%d taught, dead: %S" (length keys) dead))
    (my/stage7-check (format "%s teaches a useful number of keys"
                             (file-name-nondirectory file))
                     (> (length keys) 20)
                     (format "%d keys" (length keys)))))

;;;; 4b. The handbook's keys must be real keys somewhere

(defvar my/stage7-handbook (expand-file-name "HANDBOOK.org" user-emacs-directory))
(defvar my/stage7-inventory (expand-file-name "INVENTORY.org" user-emacs-directory))

(defun my/stage7-key-like (string)
  "Return non-nil when STRING looks like a key sequence rather than prose."
  (and (string-match-p "\\`[A-Za-z0-9<]-?\\|\\`<\\|\\`RET\\|\\`TAB\\|\\`SPC\\|\\`ESC" string)
       (string-match-p "\\(C-\\|M-\\|S-\\|RET\\|TAB\\|SPC\\|ESC\\|<[a-z]+>\\|\\`F[0-9]+\\)" string)
       (< (length string) 30)))

(my/stage7-check "the handbook exists" (file-exists-p my/stage7-handbook))
(my/stage7-check "the inventory exists" (file-exists-p my/stage7-inventory))

(when (and (file-exists-p my/stage7-handbook) (file-exists-p my/stage7-inventory))
  (let* ((handbook (my/stage7-text my/stage7-handbook))
         (inventory (my/stage7-text my/stage7-inventory))
         (keys (delete-dups
                (with-temp-buffer
                  (insert handbook)
                  (goto-char (point-min))
                  (let (found)
                    (while (re-search-forward "=\\([^=\n]+\\)=" nil t)
                      (let ((key (string-trim (match-string 1))))
                        ;; "M-x command" is a command claim, checked separately.
                        (when (and (my/stage7-key-like key)
                                   (not (string-prefix-p "M-x " key)))
                          (push key found))))
                    (nreverse found)))))
         (dead (cl-remove-if (lambda (key)
                               (or (my/stage7-resolves-p key)
                                   (string-match-p (regexp-quote key) inventory)))
                             keys)))
    (my/stage7-check "the handbook teaches a useful number of keys"
                     (> (length keys) 40) (format "%d keys" (length keys)))
    (my/stage7-check "every M-x command the handbook names is a real command"
                 (let (cmds)
                   (with-temp-buffer
                     (insert handbook)
                     (goto-char (point-min))
                     (while (re-search-forward "M-x \\([a-zA-Z0-9-]+\\)" nil t)
                       (push (match-string 1) cmds)))
                   (let ((dead (cl-remove-if
                                (lambda (name)
                                  (or (commandp (intern name))
                                      (fboundp (intern name))))
                                (delete-dups cmds))))
                     (if dead
                         (progn (princ (format "  dead commands: %S\n" dead)) nil)
                       t))))
(my/stage7-check "every key in the handbook is bound somewhere or listed in the inventory"
                     (null dead)
                     (format "%d dead: %S" (length dead) dead))))

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
    (dolist (target '("ORG-PKM.org" "VERIFICATION.org" "HANDBOOK.org" "INVENTORY.org"))
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
