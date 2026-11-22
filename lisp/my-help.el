;;; my-help.el --- Generated keybinding reference -*- lexical-binding: t; -*-

;;; Commentary:
;; The keymap listing is derived from the live global map every time it is
;; opened, so it cannot drift from the configuration the way a hand-written
;; cheat sheet does.  Nothing here is stored on disk.
;;
;; Commands:
;;   C-c ?    list every C-c binding, grouped by prefix
;;   C-c k H  open the ORG-PKM document

;;; Code:

(require 'subr-x)

(defgroup my/help nil
  "Generated help for this configuration."
  :group 'convenience
  :prefix "my/")

(defun my/help--walk (keymap prefix acc)
  "Collect (KEY . COMMAND) pairs from KEYMAP under PREFIX into ACC."
  (map-keymap
   (lambda (event definition)
     (let ((key (vconcat prefix (vector event))))
       (cond
        ;; A prefix keymap, or a prefix command whose function is a keymap.
        ;; Autoload stand-ins are skipped: descending would load the package
        ;; just to draw a help buffer.
        ((keymapp definition)
         (let ((map (cond ((eq (car-safe definition) 'autoload) nil)
                          ((and (symbolp definition) (fboundp definition))
                           (symbol-function definition))
                          (t definition))))
           (when (keymapp map)
             (setq acc (my/help--walk map key acc)))))
        ((symbolp definition)
         (push (cons key definition) acc)))))
   keymap)
  acc)

(defun my/help--rows ()
  "Return (KEY DESCRIPTION COMMAND) rows for every C-c binding, sorted."
  (let (rows)
    (dolist (pair (my/help--walk (current-global-map) [] nil))
      (let ((desc (key-description (car pair))))
        (when (string-prefix-p "C-c" desc)
          (push (list desc
                      (or (my/help--first-line (cdr pair)) "")
                      (symbol-name (cdr pair)))
                rows))))
    (sort rows (lambda (a b) (string< (car a) (car b))))))

(defun org-dblock-write:my-keys (_params)
  "Write a table of every C-c binding in the live global map.
Used by the reference documents so their key tables cannot go stale: refresh
with =C-c k b=, or =C-c C-c= on the block.
Org looks this up as `org-dblock-write:my-keys', so the name is fixed."
  (let ((rows (my/help--rows))
        (target (current-buffer)))
    (with-current-buffer target
      (insert (format "| Key | Command | What it does |\n|---+---+---|\n"))
      (dolist (row rows)
        (insert (format "| =%s= | =%s= | %s |\n" (car row) (nth 2 row) (nth 1 row))))
      (insert (format "\n%d bindings under C-c. Generated from the live keymaps.\n"
                      (length rows))))))

(defun my/help--first-line (command)
  "Return the first sentence of COMMAND's docstring."
  (let ((doc (ignore-errors (documentation command))))
    (if (stringp doc)
        (truncate-string-to-width
         (car (split-string (subst-char-in-string ?\n ?\s doc) "\\. ")) 90 nil nil "…")
      "")))

(defun my/help--group-key (key-description)
  "Return the group label for KEY-DESCRIPTION, i.e. its two-key prefix."
  (let ((parts (split-string key-description " ")))
    (string-join (seq-take parts (min 2 (length parts))) " ")))

(defun my/help-bindings ()
  "Show every C-c binding in the live global map, grouped by prefix.
Generation happens on each call, so new bindings appear here immediately."
  (interactive)
  (let* ((pairs (seq-filter
                 (lambda (pair) (string-prefix-p "C-c" (key-description (car pair))))
                 (my/help--walk (current-global-map) [] nil)))
         (sorted (sort pairs (lambda (a b)
                               (string< (key-description (car a))
                                        (key-description (car b))))))
         (groups (make-hash-table :test #'equal)))
    (dolist (pair sorted)
      (let* ((desc (key-description (car pair)))
             (group (my/help--group-key desc)))
        (push pair (gethash group groups))))
    (with-current-buffer (get-buffer-create "*My Keys*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "My keybindings\n"
                "==============\n\n"
                (format "Generated %s from the live keymaps. %d bindings under C-c.\n"
                        (format-time-string "%Y-%m-%d %H:%M") (length sorted))
                "Press RET on a command to see its documentation.\n\n")
        (dolist (group (sort (hash-table-keys groups) #'string<))
          (insert (propertize (format "%s\n" group) 'face 'org-level-2))
          (dolist (pair (sort (gethash group groups)
                              (lambda (a b)
                                (string< (key-description (car a))
                                         (key-description (car b))))))
            (insert (format "  %-14s "
                            (propertize (key-description (car pair)) 'face 'help-key-binding))
                    (propertize (symbol-name (cdr pair)) 'face 'font-lock-function-name-face)
                    "\n")
            (let ((doc (my/help--first-line (cdr pair))))
              (unless (string-empty-p doc)
                (insert (format "  %-14s %s\n" "" (propertize doc 'face 'shadow))))))
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)
        (setq-local revert-buffer-function
                    (lambda (&rest _) (my/help-bindings)))
        (display-buffer (current-buffer))))))

(defun my/help-open-doc ()
  "Open the ORG-PKM document that explains the knowledge system."
  (interactive)
  (let ((doc (expand-file-name "ORG-PKM.org" user-emacs-directory)))
    (if (file-readable-p doc)
        (find-file doc)
      (user-error "Documentation not found: %s" doc))))

(global-set-key (kbd "C-c ?") #'my/help-bindings)
(global-set-key (kbd "C-c k H") #'my/help-open-doc)

(setq my/knowledge-doc-files
      (list (expand-file-name "cheatsheet.org" user-emacs-directory)
            (expand-file-name "tutorial.org" user-emacs-directory)))

(provide 'my-help)
;;; my-help.el ends here
