;;; my-kanban.el --- Kanban boards over the knowledge tree -*- lexical-binding: t; -*-

;;; Commentary:
;; org-kanban renders a dynamic block as an Org table whose columns are the
;; TODO keywords.  Three facts from its source drive everything here:
;;
;;   1. Columns come from `org-todo-keywords-1' of the files being scanned,
;;      so every knowledge file must carry the same #+TODO line.
;;   2. `:scope' directories are expanded with `directory-files', which is NOT
;;      recursive, and each scope element goes through `symbol-name' — so the
;;      parameter must be a list of path symbols, not strings.
;;   3. `:skip-columns' is compared against a list of strings.
;;
;; `:mirrored t' reverses the column order.  It is deliberately not used: a
;; board reads BACKLOG to DONE, left to right.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-kanban)
(require 'my-org-pkm)

(defcustom my/kanban-skip-states '("CANCELLED")
  "States rendered in no board.
Strings, because `org-todo-keywords-1' holds strings and org-kanban matches
columns with `-difference'."
  :type '(repeat string)
  :group 'my/knowledge)

(defun my/kanban-params ()
  "Dynamic block parameters for a single-file board."
  (format ":skip-columns (%s)"
          (mapconcat (lambda (s) (format "\"%s\"" s)) my/kanban-skip-states " ")))

(defun my/kanban-scope-params ()
  "Dynamic block parameters for a consolidated board.
Directories, not a file list, so a new project appears without regenerating
anything.  Paths are written with a leading ~ so the block stays readable."
  (format ":scope (%s) %s"
          (mapconcat (lambda (d)
                       (abbreviate-file-name
                        (directory-file-name (my/knowledge-subdir d))))
                     my/knowledge-agenda-dirs " ")
          (my/kanban-params)))

;;;; Moving rows

(defun my/kanban--exact-heading-marker (buffer heading)
  "Return a marker at HEADING in BUFFER, or nil when it is not there.
Searches every heading from the top of the buffer and compares the heading
text, so the result does not depend on where point happens to sit."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (found)
        (while (and (not found) (re-search-forward org-outline-regexp-bol nil t))
          (when (equal (string-trim (org-get-heading t t t t))
                       (string-trim heading))
            (setq found (point))))
        (when found (point-marker))))))

(defun my/kanban--find ()
  "Resolve the kanban row at point to (FILE . MARKER), or nil.
This replaces `org-kanban//find'.  Its file+heading branch calls
`org-find-exact-headline-in-buffer' in the target buffer without resetting
point, and that call can return a *neighbouring* heading.  The result is a
silent one-off move of the wrong task — the worst failure mode a task system
can have, so the resolution is reimplemented here and refuses to guess."
  (let* ((line (string-trim (thing-at-point 'line t)))
         (file (buffer-file-name))
         result heading)
    (cond
     ;; [[file:path::*heading][description]]
     ((string-match "\\[\\[file:\\(.+?\\)::\\*\\(.*\\)\\]\\[" line)
      (let ((target (expand-file-name (match-string 1 line)))
            (heading (org-kanban//unescape-heading (match-string 2 line))))
        (when (file-exists-p target)
          (let ((marker (my/kanban--exact-heading-marker
                         (find-file-noselect target) heading)))
            (when marker (setq result (list target marker)))))))
     ;; [[file:path::#custom-id][description]]
     ((string-match "\\[\\[file:\\(.+?\\)::#\\(.*\\)\\]\\[" line)
      (let ((target (expand-file-name (match-string 1 line)))
            (id (match-string 2 line)))
        (when (file-exists-p target)
          (with-current-buffer (find-file-noselect target)
            (let ((pos (save-excursion (org-find-property "CUSTOM_ID" id))))
              (when pos (setq result (list target (point-marker)))))))))
     ;; [[#custom-id][description]] — same file
     ((string-match "\\[\\[#\\(.*\\)\\]\\[" line)
      (let ((id (match-string 1 line)))
        (when file
          (let ((pos (save-excursion (org-find-property "CUSTOM_ID" id))))
            (when pos (setq result (list file (point-marker))))))))
     ;; [[id:uuid][description]]
     ((string-match "\\[\\[id:\\(.*\\)\\]\\[" line)
      (let ((target (org-id-find (match-string 1 line) 'marker)))
        (when target (setq result target))))
     ;; [[*heading][description]] — same file
     ((string-match "\\[\\[\\(\\*.*\\)\\]\\[" line)
      (setq heading (org-kanban//unescape-heading (match-string 1 line)))
      (when (and file heading (string-prefix-p "*" heading))
        (let ((marker (my/kanban--exact-heading-marker
                       (find-file-noselect file) (string-trim-left heading "\\*+ "))))
          (when marker (setq result (list file marker)))))))
    result))

(defvar my/kanban--last-source-buffer nil
  "Buffer holding the task behind the kanban row org-kanban last resolved.")

(defun my/kanban--remember-source (result)
  "Record the source buffer of RESULT so a move can be persisted."
  (setq my/kanban--last-source-buffer
        (and (nth 1 result) (marker-buffer (nth 1 result))))
  result)

(defun my/kanban--persist-source (&rest _)
  "Save the task file org-kanban just edited.
org-kanban changes the original buffer but never saves it; leaving the move
in memory would make the board and the file disagree after a restart."
  (when (buffer-live-p my/kanban--last-source-buffer)
    (with-current-buffer my/kanban--last-source-buffer
      (when (buffer-modified-p)
        (save-buffer)))))

(advice-add 'org-kanban//find :override #'my/kanban--find)
(advice-add 'org-kanban//find :filter-return #'my/kanban--remember-source)
(advice-add 'org-kanban//move :after #'my/kanban--persist-source)
(advice-add 'org-kanban//move-subtree :after #'my/kanban--persist-source)

(defun my/kanban-refresh-file (file)
  "Regenerate every kanban block in FILE and save it.  Returns the count."
  (with-current-buffer (find-file-noselect file)
    (let ((count 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#\\+BEGIN: kanban" nil t)
          (goto-char (match-beginning 0))
          (org-dblock-update)
          (cl-incf count)
          ;; Resume after the block we just rewrote.
          (when (re-search-forward "^#\\+END:" nil t)
            (forward-line 1))))
      (when (> count 0) (save-buffer))
      (set-buffer-modified-p nil)
      count)))

(defun my/kanban-refresh-buffer ()
  "Regenerate the kanban blocks in the current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file (user-error "Buffer is not visiting a file"))
    (message "%d kanban block(s) refreshed in %s"
             (my/kanban-refresh-file file) (file-name-nondirectory file))))

(defun my/kanban-update-all ()
  "Regenerate every board: per-project, per-subject, and the dashboards."
  (interactive)
  (let ((files (append (my/knowledge-actionable-files)
                       (my/knowledge-files-in "dashboards")))
        (blocks 0))
    (dolist (file files)
      (cl-incf blocks (my/kanban-refresh-file file)))
    (message "Refreshed %d kanban block(s) across %d file(s)"
             blocks (length files))))

(defun my/kanban-insert-board ()
  "Append a Board section with a kanban block to the current file.
Refuses when the file already has one, so it is safe to hit twice."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
  (when (save-excursion
          (goto-char (point-min))
          (re-search-forward "^#\\+BEGIN: kanban" nil t))
    (user-error "This file already has a kanban block"))
  (goto-char (point-max))
  (unless (bolp) (newline))
  (insert (format "\n* Board\n#+BEGIN: kanban %s\n#+END:\n" (my/kanban-params)))
  (forward-line -1)
  (org-dblock-update)
  (save-buffer)
  (message "Board added"))

(defun my/kanban-open-overview ()
  "Open the dashboard holding the consolidated project board."
  (interactive)
  (find-file (my/knowledge-subdir "dashboards/projects.org")))

(defun my/kanban-open-learning-overview ()
  "Open the learning dashboard, which holds the learning-only board."
  (interactive)
  (find-file (my/knowledge-subdir "dashboards/learning.org")))

(global-set-key (kbd "C-c k b") #'my/kanban-update-all)
(global-set-key (kbd "C-c k B") #'my/kanban-open-overview)

(provide 'my-kanban)
;;; my-kanban.el ends here
