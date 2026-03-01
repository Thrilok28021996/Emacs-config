;;; config/org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Complete org-mode ecosystem loaded with `:defer 2` (2 seconds idle).
;; Org is the most feature-rich module — it handles:
;;
;; Provides:
;;   Org core         — outlining, TODO tracking, agenda, capture, export
;;   Org-roam         — Zettelkasten-style knowledge graph (backlinks)
;;   Org-roam-dailies — daily journal notes within the roam graph
;;   Org-roam-ui      — visual knowledge graph in the browser
;;   Org-journal      — standalone daily journaling
;;   Org-modern       — modern visual styling (bullets, tags, tables)
;;   Org-appear       — reveal emphasis markers when cursor is on them
;;   Olivetti         — centered, distraction-free writing mode
;;   Org-babel        — execute code blocks (Python, C, shell, etc.)
;;   Org-super-agenda — group agenda items by category/priority/tag
;;   Org-download     — drag-and-drop images into org files
;;   Org-pomodoro     — 25min work / 5min break timer
;;   Evil-org         — Vim keybindings for org-mode
;;   Org-noter        — annotate PDFs/EPUBs synced with org notes
;;   Consult-org-roam — ripgrep search across all roam notes
;;   Denote           — simple flat-file note system (keyword+date filenames)
;;   Org-transclusion — embed/transclude content from other org files
;;
;; Directory structure:
;;   ~/Documents/org/          — org-agenda files, tasks, notes
;;   ~/Documents/roam-notes/   — org-roam knowledge base (Zettelkasten)
;;   ~/Documents/journal/      — org-journal daily entries
;;   ~/Documents/denote/       — denote flat-file notes (simpler, keyword-based)
;;
;; All keybindings are defined in evil-config.el (SPC o / SPC n prefix).

;;; Code:

;; Silence byte-compiler warnings for deferred functions
(declare-function org-roam-node-find "org-roam")
(declare-function org-roam-node-file "org-roam")
(declare-function org-roam-node-at-point "org-roam")
(declare-function org-roam-node-id "org-roam")
(declare-function org-roam-node-title "org-roam")
(declare-function org-roam-node-insert "org-roam")
(declare-function org-roam-buffer-toggle "org-roam")
(declare-function org-roam-db-autosync-mode "org-roam")
(declare-function org-roam-db-sync "org-roam")
(declare-function org-html-export-to-html "ox-html")
(declare-function evil-org-agenda-set-keys "evil-org-agenda")
(declare-function evil-org-set-key-theme "evil-org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-subtree "org")
(declare-function olivetti-mode "olivetti")
(declare-function org-noter "org-noter")
(declare-function org-noter-create-skeleton "org-noter")
(declare-function consult-org-roam-file-find "consult-org-roam")
(declare-function consult-org-roam-search "consult-org-roam")
(declare-function consult-org-roam-backlinks "consult-org-roam")
(declare-function consult-org-roam-forward-links "consult-org-roam")
(declare-function consult-org-roam-mode "consult-org-roam")
(declare-function denote "denote")
(declare-function denote-open-or-create "denote")
(declare-function denote-link "denote")
(declare-function denote-keywords-add "denote")
(declare-function denote-keywords-remove "denote")
(declare-function denote-rename-file "denote")
(declare-function denote-rename-file-using-front-matter "denote")
(declare-function denote-backlinks "denote")
(declare-function denote-find-backlink "denote")
(declare-function denote-find-file "denote")
(declare-function denote-sort-files "denote")
(declare-function denote-org-dblock-update "denote")
(declare-function list-denotes "denote-menu")
(declare-function org-transclusion-mode "org-transclusion")
(declare-function org-transclusion-add "org-transclusion")
(declare-function org-transclusion-add-all "org-transclusion")
(declare-function org-modern-mode "org-modern")
(declare-function org-modern-agenda "org-modern")
(declare-function org-bullets-mode "org-bullets")
(declare-function org-appear-mode "org-appear")
(declare-function org-roam-ui-mode "org-roam-ui")
(declare-function pdf-tools-install "pdf-tools")
(declare-function org-pomodoro "org-pomodoro")

;; Silence byte-compiler warnings for babel variables
(defvar org-babel-python-command)
(defvar org-babel-C++-compiler)
(defvar org-babel-C-compiler)
(defvar denote-directory)
(defvar org-noter-notes-window-location)
(defvar org-noter-always-create-frame)
(defvar org-noter-auto-save-last-location)
(defvar org-noter-doc-split-fraction)

;; ══════════════════════════════════════════════════════════════════
;;  Directory Setup
;; ══════════════════════════════════════════════════════════════════
;; Create the org directory tree on first load if it doesn't exist.
(defun my/ensure-org-directories ()
  "Create org directory structure if it doesn't exist."
  (let ((dirs '("~/Documents/org"
                "~/Documents/org/learning"
                "~/Documents/org/projects"
                "~/Documents/org/planning"
                "~/Documents/org/reviews"
                ;; roam-notes and its subdirectories
                "~/Documents/roam-notes"
                "~/Documents/roam-notes/course"
                "~/Documents/roam-notes/learning"
                "~/Documents/roam-notes/tutorials"
                "~/Documents/roam-notes/projects"
                "~/Documents/roam-notes/weekly"
                "~/Documents/roam-notes/notes"
                "~/Documents/roam-notes/literature"
                "~/Documents/roam-notes/fleeting"
                "~/Documents/roam-notes/evergreen"
                ;; denote flat-file notes
                "~/Documents/denote"
                "~/Documents/denote/attachments"
                ;; writing output
                "~/Documents/writing-output")))
    (dolist (dir dirs)
      (let ((expanded-dir (expand-file-name dir)))
        (unless (file-directory-p expanded-dir)
          (make-directory expanded-dir t)
          (message "Created directory: %s" expanded-dir))))))

;; Create directories on load
(my/ensure-org-directories)

;; ══════════════════════════════════════════════════════════════════
;;  1. Org Core Configuration
;; ══════════════════════════════════════════════════════════════════
;; Uses the built-in org (not a separately installed version).
;; `:defer 2` means org loads 2 seconds after startup, or immediately
;; when a .org file is opened (whichever comes first).
(use-package org
  :straight (:type built-in)
  :defer 2
  :mode ("\\.org\\'" . org-mode)
  :config
  ;; Core paths and display settings
  (setq org-directory "~/Documents/org"                  ; root org directory
        org-agenda-files (list org-directory)             ; files scanned for agenda
        org-default-notes-file (concat org-directory "/notes.org")
        org-log-done 'time                               ; timestamp when marking DONE
        org-log-into-drawer t                            ; put state changes in :LOGBOOK:
        org-startup-indented t                           ; indent headings visually
        org-hide-emphasis-markers t                      ; hide *bold* markers (show bold)
        org-pretty-entities t                            ; render \alpha as α, etc.
        org-ellipsis " ▾")                               ; folded heading indicator

  ;; TODO workflow: TODO → NEXT → WAITING/HOLD → DONE/CANCELLED
  ;; Letters in parens are quick-select keys in C-c C-t.
  ;; @ = add note on enter, ! = log timestamp, / = on exit.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "DONE(d!)" "CANCELLED(c@/!)")))

  ;; Color-code TODO states for visual scanning
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#ff6c6b" :weight bold))
          ("NEXT" . (:foreground "#da8548" :weight bold))
          ("WAITING" . (:foreground "#ecbe7b" :weight bold))
          ("HOLD" . (:foreground "#a9a1e1" :weight bold))
          ("DONE" . (:foreground "#98be65" :weight bold))
          ("CANCELLED" . (:foreground "#5B6268" :weight bold))))

  ;; ── Capture Templates ──────────────────────────────────────────
  ;; SPC o c activates org-capture.  Each template has a shortcut key:
  ;;   t = task, n = note, m = meeting
  ;;   l = learning (sub-menu: lc=course, le=extract, lt=tutorial, ln=note)
  ;;   p = project, f = weekly flow
  ;;   w = writing (sub-menu: wi=idea, wa=article, wj=journal, wr=research, wq=quote)
  ;; Templates marked with `(file ...)` pull structure from template files.
  (setq org-capture-templates
        `(("t" "Task" entry
           (file+headline ,(concat org-directory "/tasks.org") "Inbox")
           "* TODO %?\n  %u\n  %i")

          ("n" "Note" entry
           (file+headline ,(concat org-directory "/notes.org") "Quick Notes")
           "* %?\n  %u\n  %i")

          ("m" "Meeting" entry
           (file+headline ,(concat org-directory "/meetings.org") "Meetings")
           "* %?\n  %u\n  %i")

          ;; Learning Templates
          ("l" "Learning Templates")
          ("lc" "Course" entry
           (file ,(concat org-directory "/learning/courses.org"))
           (file ,(expand-file-name "templates/notes/course-template.org" user-emacs-directory)))

          ("le" "Learning Extract" entry
           (file ,(concat org-directory "/learning/extracts.org"))
           (file ,(expand-file-name "templates/notes/learning-extract.org" user-emacs-directory)))

          ("lt" "Tutorial" entry
           (file ,(concat org-directory "/learning/tutorials.org"))
           (file ,(expand-file-name "templates/notes/tutorial-template.org" user-emacs-directory)))

          ("ln" "Universal Note" entry
           (file ,(concat org-directory "/learning/notes.org"))
           (file ,(expand-file-name "templates/notes/universal-note.org" user-emacs-directory)))

          ;; Project Template
          ("p" "Project" entry
           (file ,(concat org-directory "/projects/projects.org"))
           (file ,(expand-file-name "templates/notes/project-template.org" user-emacs-directory)))

          ;; Weekly Flow
          ("f" "Weekly Flow" entry
           (file ,(concat org-directory "/planning/weekly.org"))
           (file ,(expand-file-name "templates/notes/weekly-flow.org" user-emacs-directory)))

          ;; Writing Templates
          ("w" "Writing Templates")
          ("wi" "Idea" entry
           (file+headline ,(concat org-directory "/writing.org") "Ideas")
           "* %?\n  %u\n  %i\n\n** Context\n\n** Development\n")

          ("wa" "Article Draft" entry
           (file+headline ,(concat org-directory "/writing.org") "Articles")
           "* DRAFT %?\n  %u\n\n** Outline\n   - \n\n** Introduction\n\n** Main Content\n\n** Conclusion\n\n** References\n")

          ("wj" "Journal Entry" entry
           (file+olp+datetree ,(concat org-directory "/journal.org"))
           "* %?\n  %u\n\n** Reflection\n\n** Learnings\n")

          ("wr" "Research Note" entry
           (file+headline ,(concat org-directory "/research.org") "Research Notes")
           "* %?\n  %u\n\n** Source: \n** Key Points\n   - \n\n** Analysis\n\n** Related Topics\n   - ")

          ("wq" "Quote" entry
           (file+headline ,(concat org-directory "/quotes.org") "Quotes")
           "* %?\n  %u\n  Source: \n\n** Context\n\n** Reflection\n")))
  
  ;; ── Export Settings ──────────────────────────────────────────────
  ;; Controls how org-export (C-c C-e) generates HTML, LaTeX, PDF.
  (setq org-export-with-toc 2
        org-export-with-section-numbers nil
        org-export-with-author nil
        org-export-with-email nil
        org-export-with-date t
        org-export-time-stamp-file nil
        org-export-with-broken-links 'mark
        org-export-with-smart-quotes t
        org-export-preserve-breaks nil)
  
  ;; HTML5 export — clean output without default styles/scripts
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-validation-link nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)
  
  ;; LaTeX/PDF export — runs pdflatex 3 times (for TOC and references)
  (setq org-latex-compiler "pdflatex"
        org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -interaction nonstopmode -output-directory %o %f"))
  
  ;; ── Writing & Editing Behavior ──────────────────────────────────
  (setq org-cycle-separator-lines 1
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-adapt-indentation nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-return-follows-link t
        org-mouse-1-follows-link t
        org-link-descriptive t)
  
  ;; Safety settings — prevent accidentally editing invisible text
  (setq org-fold-catch-invisible-edits 'show-and-error
        org-ctrl-k-protect-subtree t
        org-yank-adjusted-subtrees t
        org-yank-folded-subtrees nil
        org-insert-heading-respect-content t
        org-M-RET-may-split-line '((headline . nil) (item . t) (table . nil)))
  
  ;; ── Refile & Archive ────────────────────────────────────────────
  ;; Refile moves headings between files.  Targets go 3 levels deep
  ;; across all agenda files.  Archive sends completed items to
  ;; ~/Documents/org/archive.org.
  ;; Archive location set in org-archive use-package section below
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  ;; ── Custom Agenda Views ─────────────────────────────────────────
  ;; Access with C-c a then the shortcut key:
  ;;   d = daily agenda + unscheduled TODOs
  ;;   w = weekly review (agenda + completed + remaining)
  ;;   l = learning-tagged tasks only
  (setq org-agenda-custom-commands
        '(("d" "Daily Agenda and TODOs"
           ((agenda "" ((org-agenda-span 1)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Unscheduled TODOs:")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 7)))
            (todo "DONE"
                  ((org-agenda-overriding-header "Completed This Week:")))
            (todo "TODO"
                  ((org-agenda-overriding-header "Still TODO:")))))

          ("l" "Learning Tasks"
           ((tags-todo "learning"
                       ((org-agenda-overriding-header "Learning TODOs:")))))))

  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )

;; ══════════════════════════════════════════════════════════════════
;;  2. Visual Enhancements
;; ══════════════════════════════════════════════════════════════════

;; org-modern — replaces ASCII markers with Unicode symbols:
;; bullets become circles, tags get boxes, timestamps get icons.
;; Disabled for: keywords, checkboxes, tables (keep them functional).
(use-package org-modern
  :straight t
  :defer 3
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-keyword nil
        org-modern-checkbox nil
        org-modern-table nil))

;; org-bullets — replaces heading asterisks (***) with Unicode bullets.
;; Works alongside org-modern (modern handles other elements).
(use-package org-bullets
  :straight t
  :defer 3
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "▶" "▷")))

;; visual-line-mode — soft-wrap long lines at word boundaries instead
;; of hard-wrapping.  Essential for prose writing in org.
;; This is a built-in minor mode, configured via hook.
(add-hook 'org-mode-hook #'visual-line-mode)

;; org-appear — temporarily reveals hidden markup (bold markers,
;; link URLs, LaTeX fragments) when the cursor is on them.
;; Combined with org-hide-emphasis-markers, this gives you clean
;; display with full editing access.
(use-package org-appear
  :straight t
  :defer 3
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-inside-latex t))

;; olivetti — centers the buffer text in an 80-column column with
;; generous margins on both sides.  Toggle with my/org-writing-mode.
(use-package olivetti
  :straight t
  :defer 5
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 80
        olivetti-minimum-body-width 72
        olivetti-recall-visual-line-mode-entry-state t))

;; ══════════════════════════════════════════════════════════════════
;;  3. Org-Roam (Zettelkasten Knowledge Graph)
;; ══════════════════════════════════════════════════════════════════
;; Org-roam implements a Zettelkasten-style note system with backlinks.
;; Each note is an org file with a unique ID.  Notes link to each
;; other, and org-roam tracks all backlinks in an SQLite database.
;;
;; Directory structure inside ~/Documents/roam-notes/:
;;   course/     — course notes
;;   learning/   — learning extracts
;;   tutorials/  — tutorial walkthroughs
;;   projects/   — project documentation
;;   weekly/     — weekly planning notes
;;   notes/      — general notes
;;   daily/      — daily log entries (org-roam-dailies)
;;
;; PERFORMANCE: db-autosync is delayed 60 seconds to avoid
;; freezing on startup (SQLite scan of all roam files).
(use-package org-roam
  :straight t
  :defer 5
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture org-roam-db-sync)
  :config
  (setq org-roam-directory "~/Documents/roam-notes"
        org-roam-completion-everywhere t)

  ;; PERFORMANCE: Delay autosync to prevent freezing on startup
  ;; Starts 60 seconds after Emacs is idle instead of immediately
  ;; Use M-x org-roam-db-sync manually if needed sooner
  (run-with-idle-timer 60 nil #'org-roam-db-autosync-mode)
  
  ;; Enhanced Org-roam Capture Templates with better organization
  (setq org-roam-capture-templates
        `(;; Default note
          ("d" "Default Note" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: \n#+roam_aliases: \n\n")
           :unnarrowed t)

          ;; Course note (for org-roam)
          ("c" "Course" plain
           (file ,(expand-file-name "templates/notes/course-template.org" user-emacs-directory))
           :target (file+head "course/%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)

          ;; Learning extract (for org-roam)
          ("l" "Learning Extract" plain
           (file ,(expand-file-name "templates/notes/learning-extract.org" user-emacs-directory))
           :target (file+head "learning/%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)

          ;; Tutorial (for org-roam)
          ("t" "Tutorial" plain
           (file ,(expand-file-name "templates/notes/tutorial-template.org" user-emacs-directory))
           :target (file+head "tutorials/%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)

          ;; Universal note (for org-roam)
          ("n" "Universal Note" plain
           (file ,(expand-file-name "templates/notes/universal-note.org" user-emacs-directory))
           :target (file+head "notes/%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)

          ;; Project note (for org-roam)
          ("p" "Project" plain
           (file ,(expand-file-name "templates/notes/project-template.org" user-emacs-directory))
           :target (file+head "projects/%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)

          ;; Weekly planning note (for org-roam)
          ("w" "Weekly Flow" plain
           (file ,(expand-file-name "templates/notes/weekly-flow.org" user-emacs-directory))
           :target (file+head "weekly/week-%<%Y-W%V>-${slug}.org" "")
           :unnarrowed t)))
  
  ;; Enhanced node display
  (setq org-roam-node-display-template
        (concat "${type:12} ${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)))

  ;; Custom node type extraction based on directory structure
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Extract note type from directory or filename."
    (let* ((file (org-roam-node-file node))
           (relative-path (file-relative-name file org-roam-directory)))
      (cond
       ((string-match "^course/" relative-path) "📚 Course")
       ((string-match "^learning/" relative-path) "💡 Learning")
       ((string-match "^tutorials/" relative-path) "🎓 Tutorial")
       ((string-match "^projects/" relative-path) "🚀 Project")
       ((string-match "^weekly/" relative-path) "📅 Weekly")
       ((string-match "^notes/" relative-path) "📝 Note")
       (t "📄 Default"))))

  ;; Create subdirectories for org-roam organization
  (let ((roam-subdirs '("course" "learning" "tutorials" "projects" "weekly" "notes")))
    (dolist (subdir roam-subdirs)
      (let ((dir-path (expand-file-name subdir org-roam-directory)))
        (unless (file-directory-p dir-path)
          (make-directory dir-path t))))))

;; org-roam-dailies — create/navigate daily log entries.
;; Each day gets a single file (YYYY-MM-DD.org) with capture
;; templates for general entries, journal, learning, and meetings.
(use-package org-roam-dailies
  :straight nil
  :after org-roam
  :commands (org-roam-dailies-goto-today
             org-roam-dailies-goto-yesterday
             org-roam-dailies-goto-tomorrow
             org-roam-dailies-goto-date
             org-roam-dailies-capture-today)
  :config
  (setq org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:\n\n* Daily Log\n\n* Tasks\n\n* Notes\n\n* Learnings\n\n"))

          ("j" "journal" entry
           "* %<%H:%M> Journal\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:journal:\n\n"))

          ("l" "learning" entry
           "* %<%H:%M> Learning: %?\n** Context\n\n** Key Points\n   - \n\n** Connections\n   - \n\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:learning:\n\n"))

          ("m" "meeting" entry
           "* %<%H:%M> Meeting: %?\n** Attendees\n   - \n\n** Agenda\n\n** Notes\n\n** Action Items\n   - [ ] \n\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+filetags: :daily:meeting:\n\n")))))

;; org-roam-ui — opens a browser-based 3D graph visualization
;; of all roam notes and their connections.  Syncs live with Emacs.
(use-package org-roam-ui
  :straight t
  :after org-roam
  :commands org-roam-ui-mode
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;; Keybindings now handled in modules/evil-config.el to avoid conflicts

;; Filtered roam search — find notes by category (course/tutorial/learning)
(defun my/org-roam-find-course ()
  "Find course notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^course-" (file-name-base (org-roam-node-file node))))))

(defun my/org-roam-find-tutorial ()
  "Find tutorial notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^tutorial-" (file-name-base (org-roam-node-file node))))))

(defun my/org-roam-find-learning ()
  "Find learning extract notes."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^learn-" (file-name-base (org-roam-node-file node))))))

;; ══════════════════════════════════════════════════════════════════
;;  4. Org Journal (Standalone Daily Notes)
;; ══════════════════════════════════════════════════════════════════
;; Separate from org-roam-dailies — this is a simpler journal that
;; stores entries in ~/Documents/journal/ with one file per day.
(use-package org-journal
  :straight t
  :defer 10
  :commands (org-journal-new-entry org-journal-open-current-journal-file)
  :config
  (setq org-journal-dir "~/Documents/journal"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-date-format "%A, %d %B %Y")
  
  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )


;; ══════════════════════════════════════════════════════════════════
;;  5. Review & Writing Functions
;; ══════════════════════════════════════════════════════════════════
;; Weekly/monthly review templates inspired by the LYT (Linking Your
;; Thinking) framework.  Creates review files in ~/Documents/org/reviews/.
(defun my/org-roam-weekly-review ()
  "Create weekly review connecting to org-roam notes."
  (interactive)
  (let ((week-file (format "~/Documents/org/reviews/week-%s.org" 
                           (format-time-string "%Y-W%V"))))
    (find-file week-file)
    (when (= (buffer-size) 0)
      (insert "#+TITLE: Weekly Review - Week " (format-time-string "%V, %Y") "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* 📝 Week Summary\n\n")
      (insert "* 🔗 Key Connections Made\n\n")
      (insert "* 🎯 Focus Areas\n\n")
      (insert "* 📚 New MOCs or Hubs Needed\n\n")
      (insert "* 🌱 Notes to Develop\n\n")
      (insert "* 🔄 Review Actions\n\n"))))

(defun my/org-roam-monthly-review ()
  "Create monthly review for knowledge system health."
  (interactive)
  (let ((month-file (format "~/Documents/org/reviews/month-%s.org" 
                            (format-time-string "%Y-%m"))))
    (find-file month-file)
    (when (= (buffer-size) 0)
      (insert "#+TITLE: Monthly Knowledge Review - " (format-time-string "%B %Y") "\n")
      (insert "#+DATE: " (format-time-string "%Y-%m-%d") "\n\n")
      (insert "* 📊 System Health\n\n")
      (insert "* 🗺️ MOC Maintenance\n\n")
      (insert "* ⚛️ Atomic Note Quality\n\n")
      (insert "* 🔗 Connection Opportunities\n\n")
      (insert "* 🎯 Focus for Next Month\n\n"))))

;; Keybindings now handled in modules/evil-config.el to avoid conflicts

;; my/org-writing-mode — toggle distraction-free writing (olivetti +
;; text-scale).  my/org-word-count — count words in subtree/region.
(defun my/org-writing-mode ()
  "Toggle writing-focused mode for org files."
  (interactive)
  (if (bound-and-true-p olivetti-mode)
      (progn
        (olivetti-mode -1)
        (text-scale-adjust 0)
        (message "Writing mode disabled"))
    (progn
      (olivetti-mode 1)
      (text-scale-adjust 1)
      (message "Writing mode enabled"))))

(defun my/org-word-count ()
  "Count words in current org subtree or region."
  (interactive)
  (if (use-region-p)
      (message "Region contains %d words" 
               (count-words (region-beginning) (region-end)))
    (save-excursion
      (org-back-to-heading t)
      (let ((begin (point))
            (end (org-end-of-subtree t t)))
        (message "Subtree contains %d words" (count-words begin end))))))

(defun my/org-insert-writing-template ()
  "Insert a writing template at point."
  (interactive)
  (let ((template (completing-read "Template: "
                                   '("Essay" "Blog Post" "Research Note" "Meeting Notes"))))
    (pcase template
      ("Essay"
       (insert "* Essay: \n\n** Thesis\n\n** Outline\n   1. \n   2. \n   3. \n\n** Introduction\n\n** Body\n\n** Conclusion\n\n** References\n"))
      ("Blog Post"
       (insert "* Blog Post: \n\n** Hook\n\n** Key Points\n   - \n   - \n   - \n\n** Content\n\n** Call to Action\n\n** Tags\n"))
      ("Research Note"
       (insert "* Research: \n\n** Source\n\n** Summary\n\n** Key Findings\n   - \n\n** Methodology\n\n** Implications\n\n** Related Work\n"))
      ("Meeting Notes"
       (insert "* Meeting: \n\n** Attendees\n   - \n\n** Agenda\n   1. \n\n** Discussion\n\n** Action Items\n   - [ ] \n\n** Follow-up\n")))))

(defun my/org-export-to-writing-folder ()
  "Export current org file to a designated writing output folder."
  (interactive)
  (let ((output-dir "~/Documents/writing-output/")
        (current-file (buffer-file-name)))
    (unless (file-directory-p output-dir)
      (make-directory output-dir t))
    (when current-file
      (let ((base-name (file-name-sans-extension (file-name-nondirectory current-file))))
        (org-html-export-to-html)
        (let ((html-file (concat base-name ".html")))
          (when (file-exists-p html-file)
            (rename-file html-file (concat output-dir html-file) t)
            (message "Exported to %s%s" output-dir html-file)))))))

;; ══════════════════════════════════════════════════════════════════
;;  6. Productivity Plugins
;; ══════════════════════════════════════════════════════════════════

;; org-super-agenda — groups agenda items into sections (Today,
;; Important, Projects, etc.) instead of showing a flat list.
;; Groups are defined by priority, category, TODO state, or tags.
(use-package org-super-agenda
  :straight t
  :defer 5
  :after org-agenda
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t
           :scheduled today)
          (:name "Important"
           :priority "A")
          (:name "Projects"
           :category "project")
          (:name "Work"
           :category "work")
          (:name "Personal"
           :category "personal")
          (:name "Waiting"
           :todo "WAITING")
          (:auto-category t))))

;; org-download — drag-and-drop images into org buffers.
;; Images are saved to ./images/ relative to the org file.
(use-package org-download
  :straight t
  :defer 5
  :after org
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "./images/")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "_%Y%m%d_%H%M%S")
  ;; Drag and drop images into org-mode
  (add-hook 'dired-mode-hook 'org-download-enable))

;; org-cliplink — paste a URL from clipboard and auto-fetch the
;; page title to create an org link: [[url][Page Title]]
(use-package org-cliplink
  :straight t
  :defer 5
  :after org
  :commands org-cliplink)

;; org-table — built-in spreadsheet-like tables with auto-alignment
(use-package org-table
  :straight (:type built-in)
  :defer t
  :after org
  :config
  (setq org-table-automatic-realign t)
  (setq org-table-tab-recognizes-table.el t))

;; ══════════════════════════════════════════════════════════════════
;;  7. Org Babel (Literate Programming / Code Execution)
;; ══════════════════════════════════════════════════════════════════
;; Org Babel configuration — execute code blocks directly inside org files with C-c C-c.
;; Supports: Elisp, Python, Shell, C/C++, JS, SQL, LaTeX, Jupyter.
;; org-confirm-babel-evaluate nil = don't ask before running code.
;; Note: org-babel is a built-in feature of org-mode, not a separate package.
(with-eval-after-load 'org
  ;; Enable code execution for specific languages
  ;; Only include jupyter if emacs-jupyter is installed
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append '((emacs-lisp . t)
             (python . t)
             (shell . t)
             (C . t)           ; C/C++ support
             (js . t)
             (sql . t)
             (latex . t)
             (org . t))
           (when (executable-find "jupyter")
             '((jupyter . t)))))  ; Jupyter kernel support (requires jupyter CLI)

  ;; Don't ask for confirmation before executing code blocks
  (setq org-confirm-babel-evaluate nil)

  ;; Enhanced babel settings for programming
  (setq org-src-fontify-natively t          ; Syntax highlighting in source blocks
        org-src-tab-acts-natively t         ; Tab acts native in source blocks
        org-src-preserve-indentation t      ; Preserve indentation
        org-src-window-setup 'current-window ; Open source block in current window
        org-src-content-indentation 0)      ; No extra indentation (updated for Org 9.8+)

  ;; Python-specific babel settings
  (setq org-babel-python-command "python3")

  ;; C/C++ babel settings
  (setq org-babel-C++-compiler "g++")
  (setq org-babel-C-compiler "gcc"))

;; ob-async — run code blocks asynchronously (non-blocking).
;; Add `:async` header arg to a src block to run it in background.
(use-package ob-async
  :straight t
  :defer t
  :after org
  :config
  ;; Allow async execution for these languages
  (setq ob-async-no-async-languages-alist '("ipython" "jupyter-python"))
  ;; Use :async header arg to run blocks in background
  ;; e.g. #+begin_src python :async
  )

;; org-tempo — type `< s TAB` to expand into a #+begin_src block.
;; Custom shortcuts: `< py TAB` → python block, `< cpp TAB` → C++ block.
(use-package org-tempo
  :straight (:type built-in)
  :defer t
  :after org
  :config
  ;; Add more structure templates
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
  (add-to-list 'org-structure-template-alist '("c" . "src C"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; org-archive — archives completed items to archive/<filename>_archive
;; within the same directory, keeping the project structure clean.
(use-package org-archive
  :straight (:type built-in)
  :defer t
  :after org
  :config
  (setq org-archive-location "archive/%s_archive::"))

;; ══════════════════════════════════════════════════════════════════
;;  8. Evil Integration & Habits
;; ══════════════════════════════════════════════════════════════════

;; evil-org — proper Vim keybindings in org-mode:
;; navigation (gj/gk for visual lines), insert (o/O respect headings),
;; text objects (ih = inner heading), calendar, TODO cycling.

(use-package evil-org
  :straight t
  :defer t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar todo heading)))

;; org-habit — track recurring habits with consistency graphs.
;; Add SCHEDULED with a .+1d repeater to a TODO for daily habits.
;; The graph shows completion history in the agenda view.

(use-package org-habit
  :straight (:type built-in)
  :after org
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60
        org-habit-show-habits t
        org-habit-show-habits-only-for-today nil
        org-habit-show-all-today t))

;; org-pomodoro — 25-minute focus timer integrated with org clocking.
;; Start with M-x org-pomodoro on a clocked task.  After 25 min,
;; takes a 5-min break (15-min after every 4 pomodoros).

(use-package org-pomodoro
  :straight t
  :defer t
  :after org
  :commands org-pomodoro
  :config
  (setq org-pomodoro-length 25
        org-pomodoro-short-break-length 5
        org-pomodoro-long-break-length 15
        org-pomodoro-long-break-frequency 4
        org-pomodoro-play-sounds t
        org-pomodoro-keep-killed-pomodoro-time t))

;; ══════════════════════════════════════════════════════════════════
;;  9. Org-noter (PDF / EPUB / Document Annotation)
;; ══════════════════════════════════════════════════════════════════
;; org-noter lets you open a document (PDF, EPUB, HTML) side-by-side
;; with an org file.  As you move through the document, the org
;; notes follow.  Pressing `i` inserts a new note pinned to the
;; current page/position.
;;
;; Workflow:
;;   1. Open a PDF with M-x pdf-view-mode (or just open the file)
;;   2. M-x org-noter  — opens or creates a notes org file
;;   3. Navigate the PDF; press `i` to annotate at current position
;;   4. The org file stores headings with NOTER_PAGE properties
;;
;; The notes file is stored next to the PDF by default, or you can
;; point it at a roam note with #+ROAM_REFS: or NOTER_DOCUMENT property.

(use-package pdf-tools
  :straight t
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  ;; Night mode for comfortable reading
  (setq pdf-view-midnight-colors '("#f8f8f2" . "#282a36"))
  (setq pdf-view-use-unicode-ligatures nil)
  ;; Better scrolling in pdf-view
  (setq pdf-view-scroll-amount 5))

(use-package org-noter
  :straight t
  :defer t
  :after org
  :commands (org-noter org-noter-create-skeleton)
  :config
  ;; Store notes alongside the document by default.
  ;; Override per-document with: NOTER_DOCUMENT property in the org file.
  (setq org-noter-notes-window-location 'vertical-split ; side-by-side layout
        org-noter-always-create-frame nil               ; reuse current frame
        org-noter-auto-save-last-location t             ; remember reading position
        org-noter-doc-split-fraction '(0.55 0.45)       ; 55% doc, 45% notes
        org-noter-notes-search-path (list               ; search for notes here
                                     (expand-file-name "~/Documents/org")
                                     (expand-file-name "~/Documents/roam-notes")))

  ;; Integrate org-noter with org-roam: when a roam note has
  ;; #+ROAM_REFS: pointing to a PDF, open it with org-noter.
  (with-eval-after-load 'org-roam
    (defun my/org-noter-from-roam ()
      "Open org-noter for the current org-roam note's referenced document."
      (interactive)
      (let ((refs (org-entry-get (point) "ROAM_REFS")))
        (if refs
            (org-noter)
          (message "No ROAM_REFS found. Add a #+roam_refs: path/to/doc.pdf property."))))))

;; ══════════════════════════════════════════════════════════════════
;;  10. Consult-org-roam (Ripgrep search across all roam notes)
;; ══════════════════════════════════════════════════════════════════
;; consult-org-roam adds consult-based commands that search the entire
;; roam graph: full-text search, backlinks, forward-links, and node find.
;;
;; Key commands (bound in evil-config.el under SPC n):
;;   SPC n /  — full-text ripgrep search across all roam notes
;;   SPC n <  — list backlinks for current node
;;   SPC n >  — list forward links for current node

(use-package consult-org-roam
  :straight t
  :defer 6
  :after org-roam
  :config
  (consult-org-roam-mode 1)
  ;; Use ripgrep for searching (much faster than grep)
  (setq consult-org-roam-grep-func #'consult-ripgrep)
  ;; Show excerpts in search results
  (setq consult-org-roam-buffer-after-buffers t))

;; ══════════════════════════════════════════════════════════════════
;;  11. Org-transclusion (Embed content from other org files)
;; ══════════════════════════════════════════════════════════════════
;; org-transclusion lets you embed (transclude) content from another
;; org node or file directly in the current buffer.  The transcluded
;; content is read-only by default; toggle with C-c C-t to edit the
;; source.
;;
;; Usage: Insert a transclusion link:
;;   #+transclude: [[file:other.org::*Heading]] :level 2
;; Then run M-x org-transclusion-add (or enable org-transclusion-mode).

(use-package org-transclusion
  :straight t
  :defer 7
  :after org
  :commands (org-transclusion-mode org-transclusion-add org-transclusion-add-all)
  :config
  (setq org-transclusion-exclude-elements '(property-drawer keyword)))

;; ══════════════════════════════════════════════════════════════════
;;  12. Denote (Simple flat-file note system)
;; ══════════════════════════════════════════════════════════════════
;; Denote provides a lightweight alternative to org-roam for quick
;; notes that don't need a full knowledge graph.  Each note is a
;; file named: DATE--TITLE__KEYWORD1_KEYWORD2.org
;;
;; Advantages over org-roam for quick notes:
;;   • No SQLite database — plain files in a flat directory
;;   • Filenames are self-describing and sortable by date
;;   • Works with any file type (org, markdown, txt)
;;   • Simpler to understand and maintain
;;
;; Use org-roam for: long-form notes with backlinks, Zettelkasten
;; Use denote for: quick notes, fleeting thoughts, reference sheets
;;
;; Key commands (bound in evil-config.el under SPC N):
;;   SPC N n  — create new denote note (prompts for title + keywords)
;;   SPC N f  — find/open a denote note
;;   SPC N l  — link to another denote note
;;   SPC N k  — add keywords to current denote note
;;   SPC N r  — rename denote note (updates filename + front-matter)
;;   SPC N b  — show backlinks to current denote note

(use-package denote
  :straight t
  :defer 6
  :commands (denote denote-open-or-create denote-link
             denote-keywords-add denote-rename-file
             denote-backlinks denote-find-backlink)
  :config
  (setq denote-directory (expand-file-name "~/Documents/denote")
        ;; Preferred file type for new denote notes
        denote-file-type 'org
        ;; Prompt for keywords when creating notes
        denote-prompts '(title keywords)
        ;; Known keywords for completion (add your own common tags)
        denote-known-keywords '("emacs" "programming" "idea" "reading"
                                "meeting" "reference" "learning" "project"
                                "python" "research" "writing" "personal")
        ;; Store attachments (images, PDFs) alongside notes
        denote-save-buffers nil
        ;; org front-matter template for new notes
        denote-org-front-matter
        "#+title:      %s\n#+date:       %s\n#+filetags:   %s\n#+identifier: %s\n\n")

  ;; Auto-rename denote files when front-matter changes
  (add-hook 'denote-after-new-note-hook #'denote-rename-file-using-front-matter)

  ;; Fontify denote links in org buffers
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'denote-org-dblock-update))

  ;; Enable denote-backlinks in a side window
  (setq denote-link-backlinks-display-buffer-action
        '(display-buffer-in-side-window
          (side . right)
          (window-width . 0.35))))

;; denote-menu — browse denote notes in a tabulated-list buffer
;; (similar to org-roam-buffer but for denote)
(use-package denote-menu
  :straight t
  :defer t
  :after denote
  :commands (list-denotes denote-menu-filter denote-menu-clear-filters))

;; ══════════════════════════════════════════════════════════════════
;;  13. Enhanced Org-roam Setup
;; ══════════════════════════════════════════════════════════════════
;; Additional org-roam improvements for Obsidian-like experience:
;;   • org-roam-bibtex disabled (not installed) — see below
;;   • Better backlink buffer settings
;;   • Node slug helpers

(with-eval-after-load 'org-roam
  ;; ── Backlink buffer settings ──────────────────────────────────
  ;; Show backlinks in a persistent side window (like Obsidian's panel)
  (setq org-roam-buffer-window-parameters
        '((no-delete-other-windows . t)))

  ;; Display backlink buffer on the right side
  (add-to-list 'display-buffer-alist
               `("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.35)
                 (window-parameters . ((no-delete-other-windows . t)))))

  ;; ── Extra capture templates ───────────────────────────────────
  ;; Add a "literature note" template for book/article annotations
  ;; that stores the source reference and key insights.
  (let ((existing-templates org-roam-capture-templates))
    (unless (assoc "b" existing-templates)
      (setq org-roam-capture-templates
            (append org-roam-capture-templates
                    `(("b" "Book/Literature Note" plain
                       "#+title:      ${title}\n#+date:       %<%Y-%m-%d>\n#+filetags:   :literature:\n#+roam_refs:  \n\n* Summary\n\n* Key Ideas\n  - \n\n* Quotes\n\n* My Thoughts\n\n* Related Notes\n"
                       :target (file+head "literature/%<%Y%m%d%H%M%S>-${slug}.org" "")
                       :unnarrowed t)

                      ("f" "Fleeting Note" plain
                       "#+title:      ${title}\n#+date:       %<%Y-%m-%d>\n#+filetags:   :fleeting:\n\n%?"
                       :target (file+head "fleeting/%<%Y%m%d%H%M%S>-${slug}.org" "")
                       :unnarrowed t)

                      ("e" "Evergreen/Permanent Note" plain
                       "#+title:      ${title}\n#+date:       %<%Y-%m-%d>\n#+filetags:   :evergreen:\n\n* Claim\n\n%?\n\n* Evidence\n\n* Connections\n"
                       :target (file+head "evergreen/%<%Y%m%d%H%M%S>-${slug}.org" "")
                       :unnarrowed t))))))

  ;; ── Create additional roam subdirs ────────────────────────────
  (let ((extra-subdirs '("literature" "fleeting" "evergreen")))
    (dolist (subdir extra-subdirs)
      (let ((dir-path (expand-file-name subdir org-roam-directory)))
        (unless (file-directory-p dir-path)
          (make-directory dir-path t))))))

;; ── Convenience helpers ──────────────────────────────────────────

(defun my/org-roam-open-backlinks ()
  "Toggle the org-roam backlinks side window for the current node."
  (interactive)
  (if (get-buffer-window org-roam-buffer)
      (delete-window (get-buffer-window org-roam-buffer))
    (org-roam-buffer-toggle)))

(defun my/org-roam-node-insert-immediate (arg &rest args)
  "Create and insert a roam node immediately without showing capture buffer.
With prefix ARG, behave like the normal `org-roam-node-insert'."
  (interactive "P")
  (if arg
      (apply #'org-roam-node-insert args)
    (let ((org-roam-capture-templates
           '(("d" "default" plain "%?"
              :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                 "#+title: ${title}\n")
              :immediate-finish t
              :unnarrowed t))))
      (apply #'org-roam-node-insert args))))

(defun my/org-roam-copy-node-link ()
  "Copy an org-roam link to the current node to the clipboard."
  (interactive)
  (when-let* ((node (org-roam-node-at-point))
              (id (org-roam-node-id node))
              (title (org-roam-node-title node))
              (link (format "[[id:%s][%s]]" id title)))
    (kill-new link)
    (message "Copied: %s" link)))

(provide 'org-config)
;;; org-config.el ends here