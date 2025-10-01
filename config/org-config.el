;;; config/org-config.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Complete org-mode ecosystem with lazy loading
;;; Includes org-roam, org-journal, and productivity packages

;;; Code:


;; Core org-mode configuration (using stable org from GNU ELPA)
(use-package org
  :straight (:type built-in)
  :defer 2
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-directory "~/Documents/org"
        org-agenda-files (list org-directory)
        org-default-notes-file (concat org-directory "/notes.org")
        org-log-done 'time
        org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis " ▾")
  
  
  ;; Enhanced org capture templates for writing
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
          
          ("w" "Writing Templates")
          ("wi" "Idea" entry
           (file+headline ,(concat org-directory "/writing.org") "Ideas")
           "* %?\n  %u\n  %i\n\n** Context\n\n** Development\n")
          
          ("wa" "Article Draft" entry
           (file+headline ,(concat org-directory "/writing.org") "Articles")
           "* DRAFT %?\n  %u\n\n** Outline\n   - \n\n** Introduction\n\n** Main Content\n\n** Conclusion\n\n** References\n")
          
          ("wp" "Project Note" entry
           (file+headline ,(concat org-directory "/projects.org") "Active Projects")
           "* %?\n  %u\n\n** Objective\n\n** Progress\n\n** Next Steps\n\n** Resources\n")
          
          ("wj" "Journal Entry" entry
           (file+datetree ,(concat org-directory "/journal.org"))
           "* %?\n  %u\n\n** Reflection\n\n** Learnings\n")
          
          ("wr" "Research Note" entry
           (file+headline ,(concat org-directory "/research.org") "Research Notes")
           "* %?\n  %u\n\n** Source: \n** Key Points\n   - \n\n** Analysis\n\n** Related Topics\n   - ")
          
          ("wq" "Quote" entry
           (file+headline ,(concat org-directory "/quotes.org") "Quotes")
           "* %?\n  %u\n  Source: \n\n** Context\n\n** Reflection\n")))
  
  ;; Export options for better document output
  (setq org-export-with-toc 2
        org-export-with-section-numbers nil
        org-export-with-author nil
        org-export-with-email nil
        org-export-with-date t
        org-export-time-stamp-file nil
        org-export-with-broken-links 'mark
        org-export-with-smart-quotes t
        org-export-preserve-breaks nil)
  
  ;; HTML export settings
  (setq org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-validation-link nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil)
  
  ;; LaTeX export settings for better PDF output
  (setq org-latex-compiler "pdflatex"
        org-latex-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -interaction nonstopmode -output-directory %o %f"))
  
  ;; Writing-focused settings
  (setq org-cycle-separator-lines 1
        org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
        org-M-RET-may-split-line '((default . t))
        org-adapt-indentation nil
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-return-follows-link t
        org-mouse-1-follows-link t
        org-link-descriptive t)
  
  ;; Better text editing
  (setq org-catch-invisible-edits 'show-and-error
        org-ctrl-k-protect-subtree t
        org-yank-adjusted-subtrees t
        org-yank-folded-subtrees nil
        org-insert-heading-respect-content t
        org-M-RET-may-split-line '((headline . nil) (item . t) (table . nil)))
  
  ;; Auto-save settings for writing
  (setq org-archive-location "~/Documents/org/archive.org::"
        org-refile-targets '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)
  
  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )

;; Org modern for better visuals
(use-package org-modern
  :straight t
  :defer 3
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-keyword nil
        org-modern-checkbox nil
        org-modern-table nil))

;; Org bullets for better headers
(use-package org-bullets
  :straight t
  :defer 3
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "●" "▶" "▷")))

;; Visual Line Mode for better writing
(use-package visual-line-mode
  :straight (:type built-in)
  :hook (org-mode . visual-line-mode))

;; Org appear for better emphasis editing
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

;; Olivetti for distraction-free writing
(use-package olivetti
  :straight t
  :defer 5
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 80
        olivetti-minimum-body-width 72
        olivetti-recall-visual-line-mode-entry-state t))

;; Org roam for knowledge management
(use-package org-roam
  :straight t
  :defer 5
  :commands (org-roam-node-find org-roam-node-insert org-roam-capture)
  :config
  (setq org-roam-directory "~/Documents/roam-notes"
        org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode)
  
  ;; LYT + Zettelkasten Hybrid Capture Templates
  (setq org-roam-capture-templates
        `(;; Atomic notes (Zettelkasten style)
          ("a" "Atomic Note" plain
           (file ,(expand-file-name "templates/notes/hybrid-lyt-zettel/atomic-note.org" user-emacs-directory))
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Fluid/Emergent notes
          ("f" "Fluid Note" plain
           (file ,(expand-file-name "templates/notes/hybrid-lyt-zettel/fluid-note.org" user-emacs-directory))
           :target (file+head "fluid-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Synthesis notes
          ("s" "Synthesis Note" plain
           (file ,(expand-file-name "templates/notes/hybrid-lyt-zettel/synthesis-note.org" user-emacs-directory))
           :target (file+head "synthesis-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; MOC (Map of Content) - LYT style
          ("m" "MOC - Map of Content" plain
           (file ,(expand-file-name "templates/notes/hybrid-lyt-zettel/moc-template.org" user-emacs-directory))
           :target (file+head "MOC-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Knowledge Hub/Index
          ("h" "Knowledge Hub" plain
           (file ,(expand-file-name "templates/notes/hybrid-lyt-zettel/index-hub.org" user-emacs-directory))
           :target (file+head "HUB-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Literature notes (enhanced for hybrid system)
          ("l" "Literature Note" plain
           (file ,(expand-file-name "templates/notes/literature-note.org" user-emacs-directory))
           :target (file+head "lit-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Research notes (enhanced for hybrid system)
          ("r" "Research Note" plain
           (file ,(expand-file-name "templates/notes/research-note.org" user-emacs-directory))
           :target (file+head "research-%<%Y%m%d%H%M%S>-${slug}.org" "")
           :unnarrowed t)
          
          ;; Quick default note
          ("d" "Default Note" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  
  ;; Enhanced node display for hybrid system
  (setq org-roam-node-display-template
        (concat "${type:12} ${title:*} "
                (propertize "${tags:20}" 'face 'org-tag)
                " ${file:30}"))
  
  ;; Custom node type extraction
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Extract note type from filename or tags."
    (let ((file (org-roam-node-file node))
          (tags (org-roam-node-tags node)))
      (cond
       ((string-match "^MOC-" (file-name-base file)) "🗺️ MOC")
       ((string-match "^HUB-" (file-name-base file)) "🏠 Hub")
       ((string-match "^synthesis-" (file-name-base file)) "🔀 Synthesis")
       ((string-match "^fluid-" (file-name-base file)) "🌊 Fluid")
       ((string-match "^lit-" (file-name-base file)) "📚 Literature")
       ((string-match "^research-" (file-name-base file)) "🔬 Research")
       ((member "atomic" tags) "⚛️ Atomic")
       ((member "MOC" tags) "🗺️ MOC")
       ((member "hub" tags) "🏠 Hub")
       (t "📝 Note"))))
  
  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )

;; Enhanced org-roam utilities for LYT-Zettelkasten workflow
(defun my/org-roam-find-moc ()
  "Find or create a Map of Content (MOC)."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^MOC-" (file-name-base (org-roam-node-file node))))))

(defun my/org-roam-find-atomic ()
  "Find atomic notes (timestamped notes)."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^[0-9]\\{14\\}" (file-name-base (org-roam-node-file node))))))

(defun my/org-roam-find-hub ()
  "Find knowledge hubs."
  (interactive)
  (org-roam-node-find nil nil (lambda (node)
                                (string-match "^HUB-" (file-name-base (org-roam-node-file node))))))

(defun my/org-roam-review-orphans ()
  "Find orphaned notes that need connections."
  (interactive)
  (let ((orphaned-nodes (seq-filter
                         (lambda (node)
                           (= (length (org-roam-backlinks-get node)) 0))
                         (org-roam-node-list))))
    (if orphaned-nodes
        (org-roam-node-find nil nil (lambda (node) (member node orphaned-nodes)))
      (message "No orphaned notes found!"))))

(defun my/org-roam-capture-hybrid ()
  "Quick hybrid capture with type selection."
  (interactive)
  (let ((type (completing-read "Note type: "
                               '("Atomic" "Fluid" "Synthesis" "MOC" "Hub" "Literature" "Research"))))
    (pcase type
      ("Atomic" (org-roam-capture nil "a"))
      ("Fluid" (org-roam-capture nil "f"))
      ("Synthesis" (org-roam-capture nil "s"))
      ("MOC" (org-roam-capture nil "m"))
      ("Hub" (org-roam-capture nil "h"))
      ("Literature" (org-roam-capture nil "l"))
      ("Research" (org-roam-capture nil "r")))))

;; Org journal for daily notes
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


;; Weekly and Monthly review functions for LYT workflow
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

;; Writing-focused utility functions
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

;; --- Additional Org Productivity Plugins ---

;; Org Super Agenda for better agenda organization
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

;; Org Download for handling images and files
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

;; Org Cliplink for easy link insertion
(use-package org-cliplink
  :straight t
  :defer 5
  :after org
  :commands org-cliplink)

;; Better org-mode tables
(use-package org-table
  :straight (:type built-in)
  :defer t
  :after org
  :config
  (setq org-table-automatic-realign t)
  (setq org-table-tab-recognizes-table.el t))

;; Org Tempo for structure templates (< s TAB, etc.)
(use-package org-tempo
  :straight (:type built-in)
  :defer t
  :after org
  :config
  ;; Add more structure templates
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript"))
  (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("json" . "src json")))

;; Org Archive for better archiving
(use-package org-archive
  :straight (:type built-in)
  :defer t
  :after org
  :config
  (setq org-archive-location "archive/%s_archive::"))

(provide 'org-config)
;;; org-config.el ends here