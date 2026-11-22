;;; my-org-pkm.el --- Org knowledge system: layout, workflow, capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Stage 1 of the Org knowledge system.  Everything is derived from
;; `my/knowledge-dir', so a new project or learning file needs no config edit:
;; agenda discovery reads directories, not a hand-maintained file list.
;;
;; Rules this file must not break:
;;   - Org files are the source of truth; nothing here caches state on disk.
;;   - One global 8-state workflow; every file mirrors it with its own #+TODO.
;;   - Only built-in Org, no packages.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-id)
(require 'subr-x)

;;;; Layout

(defgroup my/knowledge nil
  "Personal Org knowledge system."
  :group 'org
  :prefix "my/")

(defcustom my/knowledge-dir (expand-file-name "~/knowledge/")
  "Root of the Org knowledge system."
  :type 'directory
  :group 'my/knowledge)

(defcustom my/knowledge-todo-states
  '(("BACKLOG" . ?b) ("TODO" . ?t) ("NEXT" . ?n) ("DOING" . ?o)
    ("WAITING" . ?w) ("REVIEW" . ?r) ("DONE" . ?d) ("CANCELLED" . ?c))
  "TODO states in board order, with their fast-selection keys."
  :type '(alist :key-type string :value-type character)
  :group 'my/knowledge)

(defcustom my/knowledge-done-states '("DONE" "CANCELLED")
  "States that end an item."
  :type '(repeat string)
  :group 'my/knowledge)

(defcustom my/knowledge-agenda-dirs '("notes/projects" "notes/learning")
  "Directories whose Org files are actionable, relative to `my/knowledge-dir'.
`org-agenda-files' expands directories itself, which is what makes a newly
created project file appear without any configuration change."
  :type '(repeat string)
  :group 'my/knowledge)

(defun my/knowledge-subdir (name)
  "Absolute path of NAME under `my/knowledge-dir'."
  (expand-file-name name my/knowledge-dir))

(defun my/--todo-split ()
  "Return (ACTIVE . DONE) state alists from `my/knowledge-todo-states'."
  (cons (seq-remove (lambda (s) (member (car s) my/knowledge-done-states))
                    my/knowledge-todo-states)
        (seq-filter (lambda (s) (member (car s) my/knowledge-done-states))
                    my/knowledge-todo-states)))

(defun my/knowledge-todo-keywords ()
  "Build the `org-todo-keywords' value: active states, '|', done states."
  (let* ((split (my/--todo-split))
         (fmt (lambda (s) (format "%s(%c)" (car s) (cdr s)))))
    (list (append (list 'sequence)
                  (mapcar fmt (car split))
                  (list "|")
                  (mapcar fmt (cdr split))))))

(defun my/knowledge-todo-line ()
  "The `#+TODO:' line every knowledge file mirrors so boards keep their shape."
  (let* ((split (my/--todo-split))
         (fmt (lambda (s) (format "%s(%c)" (car s) (cdr s)))))
    (format "#+TODO: %s | %s"
            (string-join (mapcar fmt (car split)) " ")
            (string-join (mapcar fmt (cdr split)) " "))))

;;;; Discovery

(defun my/knowledge-agenda-files ()
  "Return the `org-agenda-files' value for the knowledge system."
  (let ((inbox (expand-file-name "inbox.org" my/knowledge-dir)))
    (append (when (file-exists-p inbox) (list inbox))
            (mapcar #'my/knowledge-subdir my/knowledge-agenda-dirs))))

(defun my/knowledge-files-in (subdir)
  "Return regular Org files directly inside SUBDIR of the knowledge tree."
  (let ((dir (my/knowledge-subdir subdir)))
    (when (file-directory-p dir)
      (seq-filter #'file-regular-p
                  (directory-files dir t "\\`[^.#].*\\.org\\'")))))

(defun my/knowledge-project-files ()
  "Org files under notes/projects."
  (my/knowledge-files-in "notes/projects"))

(defun my/knowledge-learning-files ()
  "Org files under notes/learning."
  (my/knowledge-files-in "notes/learning"))

(defun my/knowledge-actionable-files ()
  "Every file whose TODOs belong on the consolidated board."
  (append (my/knowledge-project-files) (my/knowledge-learning-files)))

;;;; Org configuration

(setq org-directory my/knowledge-dir
      org-todo-keywords (my/knowledge-todo-keywords)
      org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "gray60"))
        ("TODO" . org-todo)
        ("NEXT" . (:foreground "orange" :weight bold))
        ("DOING" . (:foreground "gold" :weight bold))
        ("WAITING" . (:foreground "mediumorchid" :weight bold))
        ("REVIEW" . (:foreground "deepskyblue" :weight bold))
        ("DONE" . org-done)
        ("CANCELLED" . (:foreground "salmon")))
      ;; Emacs default: every not-done state blocks children, WAITING included.
      org-enforce-todo-dependencies t
      org-log-done 'time
      org-log-into-drawer t
      org-log-repeat 'time
      org-archive-location "%s_archive::* Archived"
      org-startup-indented t
      org-startup-folded 'overview
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-attach-id-dir (my/knowledge-subdir "attachments")
      org-id-locations-file (expand-file-name ".org-id-locations" my/knowledge-dir)
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      ;; A terminal cannot render LaTeX fragments; never attempt it.
      org-startup-with-latex-preview nil)

(setq org-agenda-files (my/knowledge-agenda-files))

;;;; Creating files from templates

(defun my/knowledge-slug (name)
  "Turn NAME into a filesystem-safe slug."
  (string-trim (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase name)) "-+" "-+"))

(defun my/knowledge-fill-template (template-path name)
  "Return TEMPLATE-PATH contents with NAME, the date and a fresh ID substituted."
  (unless (file-readable-p template-path)
    (user-error "Template not found: %s" template-path))
  (with-temp-buffer
    (insert-file-contents template-path)
    (let ((contents (buffer-string))
          (date (format-time-string "%Y-%m-%d")))
      (setq contents (replace-regexp-in-string "{{NAME}}" (regexp-quote name) contents t t)
            contents (replace-regexp-in-string "{{DATE}}" date contents t t)
            contents (replace-regexp-in-string "{{ID}}" (org-id-new) contents t t))
      (replace-regexp-in-string "{{RELATED}}" "" contents t t))))

(defun my/knowledge-create-file (path template name)
  "Create PATH from TEMPLATE for NAME and visit it.
Refuses to overwrite: a typo must not eat an existing file."
  (when (file-exists-p path)
    (user-error "Refusing to overwrite existing file: %s" path))
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert (my/knowledge-fill-template template name)))
  (find-file path)
  (message "Created %s" (abbreviate-file-name path))
  path)

(defun my/--create (subdir template-file name)
  "Create SUBDIR/NAME.org from templates/TEMPLATE-FILE."
  (my/knowledge-create-file
   (expand-file-name (concat (my/knowledge-slug name) ".org") (my/knowledge-subdir subdir))
   (my/knowledge-subdir (concat "templates/" template-file))
   name))

(defun my/new-project (name)
  "Create a project file called NAME under notes/projects."
  (interactive (list (read-string "Project name: ")))
  (my/--create "notes/projects" "project.org" name))

(defun my/new-learning-subject (name)
  "Create a learning subject file called NAME under notes/learning."
  (interactive (list (read-string "Learning subject: ")))
  (my/--create "notes/learning" "learning.org" name))

(defun my/knowledge-areas ()
  "Knowledge note areas: subdirectories of notes/knowledge."
  (let ((dir (my/knowledge-subdir "notes/knowledge")))
    (when (file-directory-p dir)
      (seq-filter (lambda (f) (file-directory-p (expand-file-name f dir)))
                  (directory-files dir nil "\\`[^.#].*\\'")))))

(defun my/new-knowledge-note (area name)
  "Create a permanent note called NAME inside AREA of notes/knowledge."
  (interactive
   (list (completing-read "Area: " (my/knowledge-areas) nil t)
         (read-string "Note title: ")))
  (my/--create (concat "notes/knowledge/" area) "knowledge-note.org" name))

(defun my/new-reference (name)
  "Create a source note called NAME under notes/references."
  (interactive (list (read-string "Source title: ")))
  (my/--create "notes/references" "reference.org" name))

;;;; Capture

(defun my/knowledge-component (path)
  "PATH relative to `my/knowledge-dir', used to label capture choices."
  (file-relative-name path my/knowledge-dir))

(defun my/capture--pick (files prompt)
  "Prompt for one of FILES using PROMPT and return its absolute path."
  (unless files (user-error "No files yet — create one with C-c k p or C-c k l"))
  (let* ((choices (mapcar (lambda (f) (cons (my/knowledge-component f) f)) files))
         (choice (completing-read prompt choices nil t)))
    (cdr (assoc choice choices))))

(defun my/capture-project-file ()
  "Capture file-spec function: pick a project file."
  (my/capture--pick (my/knowledge-project-files) "Project: "))

(defun my/capture-learning-file ()
  "Capture file-spec function: pick a learning subject file."
  (my/capture--pick (my/knowledge-learning-files) "Subject: "))

(defvar my/knowledge-task-body
  "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n"
  "Body shared by the task capture templates.")

(defvar my/knowledge-note-body
  (concat "* %^{Title}\n"
          ":PROPERTIES:\n"
          "  :CREATED: %U\n"
          "  :END:\n"
          "%?")
  "Body for free-form notes captured into a heading.")

(setq org-capture-templates
      `(("i" "Inbox item" entry
         (file+headline ,(expand-file-name "inbox.org" my/knowledge-dir) "Tasks")
         ,my/knowledge-task-body :empty-lines 1)
        ("t" "Task in a project" entry
         (file+headline my/capture-project-file "Tasks")
         ,my/knowledge-task-body :empty-lines 1)
        ("l" "Task in a learning subject" entry
         (file+headline my/capture-learning-file "Tasks")
         ,my/knowledge-task-body :empty-lines 1)
        ("j" "Journal entry" entry
         (file+olp+datetree ,(my/knowledge-subdir "notes/daily/journal.org"))
         ,(concat "* %U\n"
                  "** Focus\n%?\n"
                  "** Done\n"
                  "** Learned\n"
                  "** Next\n")
         :empty-lines 1 :tree-type week)
        ("n" "Someday idea" entry
         (file+headline ,(expand-file-name "inbox.org" my/knowledge-dir) "Ideas")
         ,my/knowledge-note-body :empty-lines 1)
        ("r" "Source / reading" entry
         (file+headline ,(my/knowledge-subdir "notes/references/tracking.org") "Reading")
         ,(concat "* %^{Source}\n"
                  ":PROPERTIES:\n"
                  "  :CREATED: %U\n"
                  "  :STATUS: %^{Status|unread|reading|done|abandoned}\n"
                  "  :END:\n"
                  "  %?\n")
         :empty-lines 1 :prepend t)))

;;;; Commands and keys

(defun my/capture-inbox ()
  "Capture an item into the inbox."
  (interactive)
  (org-capture nil "i"))

(defun my/open-home-dashboard ()
  "Open the home dashboard."
  (interactive)
  (find-file (my/knowledge-subdir "dashboards/home.org")))

(defun my/open-kb-agenda ()
  "Open the agenda over the knowledge system."
  (interactive)
  (setq org-agenda-files (my/knowledge-agenda-files))
  (org-agenda))

(global-set-key (kbd "C-c k i") #'my/capture-inbox)
(global-set-key (kbd "C-c k t") #'org-capture)
(global-set-key (kbd "C-c k p") #'my/new-project)
(global-set-key (kbd "C-c k l") #'my/new-learning-subject)
(global-set-key (kbd "C-c k n") #'my/new-knowledge-note)
(global-set-key (kbd "C-c k r") #'my/new-reference)
(global-set-key (kbd "C-c k a") #'my/open-kb-agenda)
(global-set-key (kbd "C-c k d") #'my/open-home-dashboard)

(provide 'my-org-pkm)
;;; my-org-pkm.el ends here
