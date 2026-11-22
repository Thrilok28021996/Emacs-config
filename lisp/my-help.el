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

(defun my/help--ascii (string)
  "Return STRING folded to ASCII.
Docstrings are full of curly quotes, dashes and arrows. A generated reference
should be readable in any terminal and must not depend on a coding system, so
typographic characters are translated and anything else non-ASCII becomes ?."
  (if (null string)
      ""
    (let ((fold '((?\u2018 . ?') (?\u2019 . ?') (?\u201c . ?") (?\u201d . ?")
                 (?\u2013 . ?-) (?\u2014 . ?-) (?\u2026 . ?.) (?\u00a0 . ?\s)
                 (?\u2192 . ?>)))
          (out nil))
      (dolist (ch (string-to-list string))
        (push (cond ((assq ch fold) (cdr (assq ch fold)))
                    ((< ch 128) ch)
                    (t ??))
              out))
      (apply #'string (nreverse out)))))

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

;;;; Inventory

(defvar my/inventory-keymaps
  '(("Global map" nil global-map)
    ("Org mode" org org-mode-map)
    ("Org agenda" org-agenda org-agenda-mode-map)
    ("Org capture" org-capture org-capture-mode-map)
    ("Clock" org-clock org-clock-mode-line-map)
    ("Magit status" magit-status magit-status-mode-map)
    ("Magit sections" magit-section magit-section-mode-map)
    ("Magit log" magit-log magit-log-mode-map)
    ("Magit diff" magit-diff magit-diff-mode-map)
    ("Eglot" eglot eglot-mode-map)
    ("Flymake" flymake flymake-mode-map)
    ("Dired" dired dired-mode-map)
    ("Isearch" isearch isearch-mode-map)
    ("Occur" replace occur-mode-map)
    ("Compilation" compile compilation-mode-map)
    ("Grep" grep grep-mode-map)
    ("Tab bar" tab-bar tab-bar-mode-map)
    ("Help" help-mode help-mode-map)
    ("Outline" outline outline-mode-map)
    ("Calc" calc calc-mode-map)
    ("Calc dispatch" calc calc-dispatch-map "C-x *")
    ("Eshell" esh-mode eshell-mode-map)
    ("Ediff" ediff ediff-mode-map)
    ("VC dir" vc-dir vc-dir-mode-map)
    ("Proced" proced proced-mode-map)
    ("Doc view" doc-view doc-view-mode-map)
    ("Calendar" calendar calendar-mode-map)
    ("Gnus summary" gnus-sum gnus-summary-mode-map)
    ("Gnus article" gnus-art gnus-article-mode-map)
    ("Bookmarks" bookmark bookmark-bmenu-mode-map)
    ("Info" info Info-mode-map)
    ("Man" man Man-mode-map)
    ("Package menu" package package-menu-mode-map)
    ("Customize" cus-edit custom-mode-map))
  "Keymaps the inventory walks: (LABEL FEATURE VARIABLE [PREFIX]).
FEATURE is required first so a lazily loaded package shows its real map; a
package that cannot be loaded is reported in the document, not dropped.  PREFIX
is for maps that are reachable only through a key (Calc's lives under C-x *).")

(defun my/help--inventory-walk (keymap prefix depth rows &optional seen)
  "Collect (KEY . COMMAND) pairs from KEYMAP into ROWS, descending DEPTH levels.
Walks `keymap-parent' too: most of Org's C-c tree lives in the parent map, and
`map-keymap' alone does not see it."
  (when (and (keymapp keymap) (not (memq keymap seen)))
    (let ((seen (cons keymap seen)))
      (map-keymap
       (lambda (event definition)
         (let ((key (vconcat prefix (vector event))))
           (cond
            ((memq event '(menu-bar tool-bar tab-bar mode-line)) nil)
            ((and (keymapp definition) (< depth 4)
                  (not (eq (car-safe definition) 'autoload)))
             ;; Take the return value: a recursive call binds its own ROWS, so
             ;; pushing there without assigning would lose every nested key.
             (setq rows (my/help--inventory-walk definition key (1+ depth) rows seen)))
            ((symbolp definition)
             (let ((desc (key-description key)))
               ;; Self-insert ranges describe character classes, not commands,
               ;; and their key names are not printable.
               (unless (string-match-p "[^ -~]" desc)
                 (push (cons desc definition) rows)))))))
       keymap)
      (let ((parent (keymap-parent keymap)))
        (when parent
          (setq rows (my/help--inventory-walk parent prefix depth rows seen))))))
  rows)

(defun my/help--inventory ()
  "Return (SECTIONS . MISSING) for every keymap in `my/inventory-keymaps'."
  (let (sections missing)
    (dolist (spec my/inventory-keymaps)
      (let* ((label (nth 0 spec))
             (feature (nth 1 spec))
             (variable (nth 2 spec))
             (prefix (if (nth 3 spec) (vconcat (kbd (nth 3 spec))) []))
             (available (or (null feature)
                            (condition-case nil (progn (require feature) t) (error nil))))
             (map (and available (boundp variable) (symbol-value variable))))
        (if (not (keymapp map))
            (push label missing)
          (let ((rows (sort (my/help--inventory-walk map prefix 0 nil)
                            (lambda (a b) (string< (car a) (car b))))))
            (when rows (push (cons label rows) sections))))))
    (cons (nreverse sections) (nreverse missing))))

(defun org-dblock-write:my-inventory (_params)
  "Write every command bound in every keymap `my/inventory-keymaps' lists.
Keys are written as they appear in a terminal; descriptions are the first line
of each command's docstring."
  (let* ((target (current-buffer))
         (data (my/help--inventory))
         (sections (car data))
         (missing (cdr data))
         (total (cl-loop for (_label . rows) in sections sum (length rows))))
    (with-current-buffer target
      (insert (format "%d bindings across %d keymaps. Generated from the live keymaps.\n\n"
                      total (length sections)))
      (dolist (section sections)
        (insert (format "** %s (%d)\n" (car section) (length (cdr section))))
        (insert "| Key | Command | What it does |\n|---+---+---|\n")
        (dolist (row (cdr section))
          (insert (format "| =%s= | =%s= | %s |\n"
                          (car row)
                          (cdr row)
                          (my/help--ascii (my/help--first-line (cdr row))))))
        (insert "\n"))
      (when missing
        (insert (format "* Keymaps not available in this session\n\n%s\n\n"
                        (mapconcat (lambda (m) (format "- %s" m)) missing "\n")))))))

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
        (insert (format "| =%s= | =%s= | %s |\n" (car row) (nth 2 row)
                        (my/help--ascii (nth 1 row)))))
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
            (expand-file-name "tutorial.org" user-emacs-directory)
            (expand-file-name "INVENTORY.org" user-emacs-directory)
            (expand-file-name "HANDBOOK.org" user-emacs-directory)))

(provide 'my-help)
;;; my-help.el ends here
