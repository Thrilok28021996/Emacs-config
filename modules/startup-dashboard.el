;;; modules/startup-dashboard.el --- Modern startup dashboard inspired by doom-emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modern startup dashboard showing recent files, projects, and bookmarks
;;; Inspired by doom-emacs dashboard with minimal performance overhead
;;; Integrates with nerd-icons for modern appearance

;;; Code:

;; --- Dashboard Package ---

(use-package dashboard
  :straight t
  :demand t
  :config
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs - Ultra-Modern Configuration v3.0")

  ;; Set the banner
  ;; Use 'official for official Emacs logo, 'logo for alternative logo
  ;; Use an integer for ASCII art (1-4), or a string for custom banner path
  (setq dashboard-startup-banner 'official)

  ;; Dashboard items configuration
  ;; Show recent files, bookmarks, projects, and agenda items
  (setq dashboard-items '((recents   . 10)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))

  ;; Center content for better aesthetics
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)

  ;; Show navigation shortcuts
  (setq dashboard-show-shortcuts t)

  ;; Enable navigation cycling
  (setq dashboard-navigation-cycle t)

  ;; Icon configuration - use nerd-icons
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-display-icons-p t)

  ;; Heading shortcuts
  (setq dashboard-heading-shortcuts '((recents   . "r")
                                      (bookmarks . "m")
                                      (projects  . "p")
                                      (agenda    . "a")))

  ;; Footer configuration
  (setq dashboard-footer-messages
        '("Welcome back! Time to code."
          "Emacs - The extensible, customizable, self-documenting display editor"
          "Press 'r' for recent files, 'p' for projects, 'm' for bookmarks"
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

  ;; Agenda configuration
  (setq dashboard-week-agenda t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  ;; Item names customization
  (setq dashboard-item-names '(("Recent Files:" . "Recently Opened:")
                               ("Agenda for today:" . "Today's Agenda:")
                               ("Agenda for the coming week:" . "This Week:")))

  ;; Projects backend - use project.el (built-in) or projectile if available
  (setq dashboard-projects-backend 'project-el)

  ;; Show info about the packages loaded and init time
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "Loaded %d packages in %.2fs"
                                    (length package-activated-list)
                                    (float-time (time-subtract (current-time)
                                                              emacs-start-time))))

  ;; Navigator configuration
  (setq dashboard-set-navigator t)
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
            (lambda (&rest _)
              (if (fboundp 'straight-pull-all)
                  (straight-pull-all)
                (package-list-packages)))))))

  ;; Customize heading faces for better visibility
  (defface dashboard-custom-heading
    '((t :inherit font-lock-keyword-face :weight bold))
    "Face for dashboard section headings."
    :group 'dashboard)

  ;; Setup startup hook
  (dashboard-setup-startup-hook)

  ;; Refresh dashboard on startup
  (add-hook 'emacs-startup-hook #'dashboard-refresh-buffer)

  ;; Support for daemon mode - show dashboard when creating new frames
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;; --- Dashboard Keybindings ---

;; Define a function to open dashboard
(defun my/open-dashboard ()
  "Open or switch to the dashboard buffer."
  (interactive)
  (if (get-buffer dashboard-buffer-name)
      (switch-to-buffer dashboard-buffer-name)
    (dashboard-refresh-buffer)))

;; --- Dashboard Customization Functions ---

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

;; --- Performance Optimizations ---

;; Lazy load dashboard for better startup performance
;; Dashboard is already loaded on demand, but we can optimize further

(defun my/dashboard-optimize ()
  "Optimize dashboard for better performance."
  ;; Disable line numbers in dashboard
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local show-trailing-whitespace nil)))

  ;; Make dashboard buffer read-only
  (add-hook 'dashboard-mode-hook
            (lambda ()
              (setq buffer-read-only t))))

;; Apply optimizations
(my/dashboard-optimize)

;; --- Dashboard Refresh Function ---

(defun my/refresh-dashboard ()
  "Refresh the dashboard content."
  (interactive)
  (when (get-buffer dashboard-buffer-name)
    (with-current-buffer dashboard-buffer-name
      (dashboard-refresh-buffer)
      (message "Dashboard refreshed"))))

;; --- Integration with Evil Mode ---

(with-eval-after-load 'evil
  (evil-set-initial-state 'dashboard-mode 'normal)

  ;; Dashboard-specific keybindings
  (evil-define-key 'normal dashboard-mode-map
    (kbd "r") 'dashboard-jump-to-recents
    (kbd "p") 'dashboard-jump-to-projects
    (kbd "m") 'dashboard-jump-to-bookmarks
    (kbd "a") 'dashboard-jump-to-agenda
    (kbd "g") 'dashboard-refresh-buffer
    (kbd "q") 'quit-window
    (kbd "RET") 'widget-button-press))

;; --- ASCII Art Banners (Optional) ---

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
  "Set dashboard banner STYLE. Options: 'logo, 'ascii, or 'custom."
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

;; --- Useful Dashboard Commands ---

(defun my/dashboard-show-stats ()
  "Show Emacs statistics in dashboard."
  (interactive)
  (message "Packages: %d | Init time: %.2fs | Emacs version: %s"
           (length package-activated-list)
           (float-time (time-subtract (current-time) emacs-start-time))
           emacs-version))

(provide 'startup-dashboard)
;;; startup-dashboard.el ends here
