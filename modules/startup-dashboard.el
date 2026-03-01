;;; modules/startup-dashboard.el --- Modern startup dashboard inspired by doom-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen loaded eagerly (:demand t) because it IS the first
;; visible buffer.  Shows recent files, bookmarks, projects, and agenda
;; items in a Doom-Emacs-style landing page.
;;
;; Provides:
;;   Dashboard sections — recent files, bookmarks, projects, agenda
;;   Navigator buttons  — GitHub, Settings (init.el), Package Update
;;   Evil integration   — j/k navigation, g r/g m/g p/g a section jumps, RET to open
;;   ASCII banners      — switchable banner styles (logo, ASCII, custom)
;;   Daemon support     — shows dashboard for new emacsclient frames
;;
;; Key bindings (in dashboard buffer, normal mode):
;;   g r  jump to recent files section
;;   g m  jump to bookmarks section
;;   g p  jump to projects section
;;   g a  jump to agenda section
;;   1-4  jump to sections 1-4 (number shortcuts)
;;   R    refresh dashboard
;;   q    quit dashboard
;;   RET  open item under cursor
;;
;; Packages:
;;   dashboard       — the dashboard framework (emacs-dashboard)
;;   nerd-icons      — file/section icons (configured in modern-ui.el)

;;; Code:

;; Silence byte-compiler warnings for deferred variables
(defvar dashboard-buffer-name)
(defvar dashboard-mode-map)
(defvar dashboard-startup-banner)
(defvar emacs-start-time)

;; Silence byte-compiler warnings for deferred functions
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function evil-local-set-key "evil-core")
(declare-function dashboard-refresh-buffer "dashboard")

;; ══════════════════════════════════════════════════════════════════
;;  Dashboard Configuration
;; ══════════════════════════════════════════════════════════════════

(use-package dashboard
  :straight t
  :demand t                                    ; load immediately — it's the startup screen
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs - Ultra-Modern Configuration v3.0")

  ;; Banner: 'official = Emacs logo, integer 1-4 = ASCII art,
  ;; string = path to custom image file
  (setq dashboard-startup-banner 'official)

  ;; Sections to show and how many items per section.
  ;; Order here determines section order on screen.
  (setq dashboard-items '((recents   . 10)     ; recently opened files
                          (bookmarks . 5)      ; C-x r m bookmarks
                          (projects  . 5)      ; project.el projects
                          (agenda    . 5)))    ; org-agenda items

  ;; Layout: center horizontally and vertically for a polished look
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)

  ;; Show shortcut keys beside each section header
  (setq dashboard-show-shortcuts t)
  (setq dashboard-navigation-cycle t)          ; wrap around at list ends

  ;; Use nerd-icons for file and section heading icons
  ;; (nerd-icons package is configured in modern-ui.el)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-display-icons-p t)

  ;; Shortcuts to jump to each section (displayed in section headers)
  ;; Note: Actual bindings are g r/g m/g p/g a to avoid shadowing Evil commands
  (setq dashboard-heading-shortcuts '((recents   . "g r")
                                      (bookmarks . "g m")
                                      (projects  . "g p")
                                      (agenda    . "g a")))

  ;; Footer — randomly picks one message each time the dashboard loads
  (setq dashboard-footer-messages
        '("Welcome back! Time to code."
          "Emacs - The extensible, customizable, self-documenting display editor"
          "Press 'g r' for recent files, 'g p' for projects, or 1-4 for section jumps"
          "Happy coding!"
          "One editor to rule them all"))

  (setq dashboard-footer-icon
        (if (and (display-graphic-p)
                 (or (fboundp 'nerd-icons-faicon)
                     (fboundp 'all-the-icons-faicon)))
            (nerd-icons-faicon "nf-fa-heart"
                               :height 1.1
                               :v-adjust -0.05
                               :face 'error)
          "♥"))

  ;; Show the full week's agenda (not just today)
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  ;; Rename section headers for friendlier labels
  (setq dashboard-item-names '(("Recent Files:" . "Recently Opened:")
                               ("Agenda for today:" . "Today's Agenda:")
                               ("Agenda for the coming week:" . "This Week:")))

  ;; Use built-in project.el as the project backend (not projectile)
  (setq dashboard-projects-backend 'project-el)

  ;; Show startup stats in the footer (package count + init time)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "Loaded %d packages in %.2fs"
                                    (if (bound-and-true-p straight--recipe-cache)
                                        (hash-table-count straight--recipe-cache)
                                      (length package-activated-list))
                                    (float-time (time-since emacs-start-time))))

  ;; Navigator buttons — row of clickable buttons below the banner.
  ;; Each button: (icon label tooltip action-fn)
  ;; "Update" calls the full Doom-style upgrade flow (my/upgrade)
  ;; which shows progress in the *Upgrade* buffer.
  (setq dashboard-startupify-list
        '(dashboard-insert-banner
          dashboard-insert-newline
          dashboard-insert-banner-title
          dashboard-insert-newline
          dashboard-insert-navigator
          dashboard-insert-newline
          dashboard-insert-init-info
          dashboard-insert-items
          dashboard-insert-newline
          dashboard-insert-footer))
  (setq dashboard-navigator-buttons
        `(;; Line 1
          ((,(when (and (display-graphic-p)
                        (fboundp 'nerd-icons-mdicon))
               (nerd-icons-mdicon "nf-md-github" :height 1.1 :v-adjust 0.0))
            "GitHub"
            "Browse GitHub"
            (lambda (&rest _) (browse-url "https://github.com")))

           (,(when (and (display-graphic-p)
                        (fboundp 'nerd-icons-mdicon))
               (nerd-icons-mdicon "nf-md-cog" :height 1.1 :v-adjust 0.0))
            "Settings"
            "Open init.el"
            (lambda (&rest _) (find-file user-init-file)))

           (,(when (and (display-graphic-p)
                        (fboundp 'nerd-icons-mdicon))
               (nerd-icons-mdicon "nf-md-update" :height 1.1 :v-adjust 0.0))
            "Update"
            "Update packages"
            (lambda (&rest _) (my/upgrade))))))

  ;; Customize heading faces for better visibility
  (defface dashboard-custom-heading
    '((t :inherit font-lock-keyword-face :weight bold))
    "Face for dashboard section headings."
    :group 'dashboard)

  ;; Register dashboard as the startup hook (replaces *scratch*)
  (dashboard-setup-startup-hook)

  ;; Daemon mode — when using `emacsclient -c`, new frames get the
  ;; dashboard instead of *scratch*.
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

  ;; Force dashboard to show at startup (not *scratch*)
  ;; This runs after init is complete and explicitly switches to dashboard
  (add-hook 'emacs-startup-hook
            (lambda ()
              ;; Show dashboard if no file buffers are open (i.e., just started Emacs)
              (unless (or (daemonp)
                          (cl-some #'buffer-file-name (buffer-list)))
                (dashboard-refresh-buffer)
                (switch-to-buffer dashboard-buffer-name)))))

;; ══════════════════════════════════════════════════════════════════
;;  Dashboard Helper Functions
;; ══════════════════════════════════════════════════════════════════

;; Open or switch to the dashboard from anywhere (SPC b h)
(defun my/open-dashboard ()
  "Open or switch to the dashboard buffer."
  (interactive)
  (if (get-buffer dashboard-buffer-name)
      (switch-to-buffer dashboard-buffer-name)
    (dashboard-refresh-buffer)))

;; Optional quick-commands section (uncomment the hook below to enable)

(defun my/dashboard-insert-custom-section ()
  "Insert a custom section in the dashboard."
  (interactive)
  (with-current-buffer dashboard-buffer-name
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert "\n")
      (insert (propertize "Quick Commands\n" 'face 'dashboard-custom-heading))
      (insert "  [SPC f f] Find file\n")
      (insert "  [SPC f r] Recent files\n")
      (insert "  [SPC p p] Switch project\n")
      (insert "  [SPC b b] Switch buffer\n"))))

;; Add custom section to dashboard (optional - uncomment if desired)
;; (add-hook 'dashboard-after-initialize-hook #'my/dashboard-insert-custom-section)

;; ══════════════════════════════════════════════════════════════════
;;  Dashboard Performance & Display
;; ══════════════════════════════════════════════════════════════════
;; Disable line numbers, trailing-whitespace display, and ensure
;; read-only in the dashboard buffer — they're meaningless here.
(add-hook 'dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local show-trailing-whitespace nil)
            (setq buffer-read-only t)))

(defun my/refresh-dashboard ()
  "Refresh the dashboard content."
  (interactive)
  (when (get-buffer dashboard-buffer-name)
    (with-current-buffer dashboard-buffer-name
      (dashboard-refresh-buffer)
      (message "Dashboard refreshed"))))

;; ══════════════════════════════════════════════════════════════════
;;  Evil Mode Integration
;; ══════════════════════════════════════════════════════════════════
;; Set dashboard to Evil normal state and define Vim-style bindings.
;; Keybindings are set BOTH via `evil-define-key` (for the keymap)
;; AND via `dashboard-mode-hook` (as a fallback).  This two-pronged
;; approach ensures bindings work even if evil-collection tries to
;; override them.
(with-eval-after-load 'dashboard
  (with-eval-after-load 'evil
    (evil-set-initial-state 'dashboard-mode 'normal)

    ;; Dashboard-specific keybindings
    ;; Sections are numbered based on dashboard-items order:
    ;; 1=recents, 2=bookmarks, 3=projects, 4=agenda
    ;; NOTE: Using g prefix to avoid shadowing Evil commands (p=paste, r=replace, etc.)
    (evil-define-key 'normal dashboard-mode-map
      (kbd "g r") 'dashboard-section-1        ; Jump to recents
      (kbd "g m") 'dashboard-section-2        ; Jump to bookmarks
      (kbd "g p") 'dashboard-section-3        ; Jump to projects
      (kbd "g a") 'dashboard-section-4        ; Jump to agenda
      (kbd "1") 'dashboard-section-1          ; Also allow numbers
      (kbd "2") 'dashboard-section-2
      (kbd "3") 'dashboard-section-3
      (kbd "4") 'dashboard-section-4
      (kbd "R") 'dashboard-refresh-buffer     ; Refresh (capital R)
      (kbd "q") 'quit-window                  ; Quit
      (kbd "n") 'dashboard-next-section       ; Next section
      (kbd "N") 'dashboard-previous-section   ; Previous section
      (kbd "j") 'dashboard-next-line          ; Next item
      (kbd "k") 'dashboard-previous-line      ; Previous item
      (kbd "RET") 'dashboard-return           ; Open item
      (kbd "<return>") 'dashboard-return      ; Alternative RET
      (kbd "TAB") 'dashboard-return           ; Tab also opens
      (kbd "l") 'dashboard-return             ; Vim-style: l to open
      (kbd "o") 'dashboard-return))           ; Vim-style: o to open

  ;; Also add to dashboard-mode-hook as a fallback
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (evil-normalize-keymaps)
              ;; Section shortcuts with g prefix and numbers
              (evil-local-set-key 'normal (kbd "g r") 'dashboard-section-1)
              (evil-local-set-key 'normal (kbd "g m") 'dashboard-section-2)
              (evil-local-set-key 'normal (kbd "g p") 'dashboard-section-3)
              (evil-local-set-key 'normal (kbd "g a") 'dashboard-section-4)
              (evil-local-set-key 'normal (kbd "1") 'dashboard-section-1)
              (evil-local-set-key 'normal (kbd "2") 'dashboard-section-2)
              (evil-local-set-key 'normal (kbd "3") 'dashboard-section-3)
              (evil-local-set-key 'normal (kbd "4") 'dashboard-section-4)
              ;; Navigation
              (evil-local-set-key 'normal (kbd "n") 'dashboard-next-section)
              (evil-local-set-key 'normal (kbd "N") 'dashboard-previous-section)
              (evil-local-set-key 'normal (kbd "j") 'dashboard-next-line)
              (evil-local-set-key 'normal (kbd "k") 'dashboard-previous-line)
              ;; Actions
              (evil-local-set-key 'normal (kbd "R") 'dashboard-refresh-buffer)
              (evil-local-set-key 'normal (kbd "q") 'quit-window)
              (evil-local-set-key 'normal (kbd "RET") 'dashboard-return)
              (evil-local-set-key 'normal (kbd "<return>") 'dashboard-return)
              (evil-local-set-key 'normal (kbd "TAB") 'dashboard-return)
              (evil-local-set-key 'normal (kbd "l") 'dashboard-return)
              (evil-local-set-key 'normal (kbd "o") 'dashboard-return))))

;; ══════════════════════════════════════════════════════════════════
;;  ASCII Art Banners
;; ══════════════════════════════════════════════════════════════════
;; Collection of ASCII banners that can be used instead of the
;; official Emacs logo.  Switch with M-x my/set-dashboard-banner.

(defvar my/dashboard-ascii-banners
  '(("Modern Emacs"
     "███╗   ███╗ ██████╗ ██████╗ ███████╗██████╗ ███╗   ██╗"
     "████╗ ████║██╔═══██╗██╔══██╗██╔════╝██╔══██╗████╗  ██║"
     "██╔████╔██║██║   ██║██║  ██║█████╗  ██████╔╝██╔██╗ ██║"
     "██║╚██╔╝██║██║   ██║██║  ██║██╔══╝  ██╔══██╗██║╚██╗██║"
     "██║ ╚═╝ ██║╚██████╔╝██████╔╝███████╗██║  ██║██║ ╚████║"
     "╚═╝     ╚═╝ ╚═════╝ ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝"
     "                  E M A C S                             ")
    ("Simple"
     "███████╗███╗   ███╗ █████╗  ██████╗███████╗"
     "██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝"
     "█████╗  ██╔████╔██║███████║██║     ███████╗"
     "██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║"
     "███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║"
     "╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝"))
  "Collection of ASCII art banners for the dashboard.")

(defun my/set-dashboard-banner (style)
  "Set dashboard banner STYLE. Options: \='logo, \='ascii, or \='custom."
  (interactive
   (list (intern (completing-read "Banner style: "
                                  '("logo" "ascii" "custom")))))
  (pcase style
    ('logo (setq dashboard-startup-banner 'official))
    ('ascii (setq dashboard-startup-banner 1))
    ('custom (setq dashboard-startup-banner
                   (read-file-name "Banner file: "
                                 (expand-file-name "banners/" user-emacs-directory)))))
  (my/refresh-dashboard))

;; Quick stats command — shows package count and init time in echo area

(defun my/dashboard-show-stats ()
  "Show Emacs statistics in dashboard."
  (interactive)
  (message "Packages: %d | Init time: %.2fs | Emacs version: %s"
           (if (bound-and-true-p straight--recipe-cache)
               (hash-table-count straight--recipe-cache)
             (length package-activated-list))
           (float-time (time-since emacs-start-time))
           emacs-version))

(provide 'startup-dashboard)
;;; startup-dashboard.el ends here
