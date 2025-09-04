;;; modules/modern-ui.el --- Modern UI enhancements and visual polish -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modern UI improvements using the latest packages and techniques
;;; Includes nerd-icons, mood-line, popper, and other cutting-edge UI enhancements
;;; Focuses on clean, modern aesthetics with excellent usability

;;; Code:

(require 'utilities)  ; For buffer name constants

;; --- Modern Icon System ---

;; Nerd-icons: Modern successor to all-the-icons
(use-package nerd-icons
  :straight t
  :defer my/defer-fast
  :if (display-graphic-p)
  :config
  ;; The Nerd Font you want to use in GUI
  (setq nerd-icons-font-family "Symbols Nerd Font Mono")
  
  ;; Auto-install fonts if they don't exist (with error handling)
  (condition-case err
      (unless (find-font (font-spec :name nerd-icons-font-family))
        (when (and (display-graphic-p) 
                   (y-or-n-p "Nerd fonts not found. Install them now?"))
          (nerd-icons-install-fonts t)))
    (error
     (message "Warning: Could not check/install nerd fonts: %s" (error-message-string err)))))

;; Nerd-icons completion integration
(use-package nerd-icons-completion
  :straight t
  :defer my/defer-medium
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;; Nerd-icons dired integration
(use-package nerd-icons-dired
  :straight t
  :defer my/defer-slow
  :hook (dired-mode . nerd-icons-dired-mode))

;; Nerd-icons corfu integration (for completion)
(use-package nerd-icons-corfu
  :straight t
  :defer my/defer-medium
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; --- Modern Modeline ---

;; Mood-line: Lightweight, fast modeline
(use-package mood-line
  :straight t
  :defer my/defer-fast
  :config
  (mood-line-mode)
  
  ;; Configure mood-line segments
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-show-eol-style t)
  (setq mood-line-show-encoding-information t)
  (setq mood-line-show-cursor-point t))

;; Alternative: Keep doom-modeline but optimize it
;; (use-package doom-modeline
;;   :straight t
;;   :defer my/defer-fast
;;   :hook (after-init . doom-modeline-mode)
;;   :config
;;   ;; Optimize doom-modeline for better performance
;;   (setq doom-modeline-height 22)
;;   (setq doom-modeline-bar-width 3)
;;   (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
;;   (setq doom-modeline-icon t)
;;   (setq doom-modeline-major-mode-icon t)
;;   (setq doom-modeline-minor-modes nil)
;;   (setq doom-modeline-enable-word-count nil)
;;   (setq doom-modeline-continuous-word-count-modes nil)
;;   (setq doom-modeline-github nil)
;;   (setq doom-modeline-mu4e nil)
;;   (setq doom-modeline-gnus nil)
;;   (setq doom-modeline-irc nil)
;;   (setq doom-modeline-persp-name nil)
;;   (setq doom-modeline-workspace-name nil)
;;   (setq doom-modeline-env-version nil))

;; --- Better Popup Management ---

;; Popper: Better popup and side window management
(use-package popper
  :straight t
  :defer my/defer-medium
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        `(,my/buffer-messages
          "Output\\*$"
          ,my/buffer-async-shell
          ,my/buffer-compile-log
          ,my/buffer-completions
          ,my/buffer-warnings
          ,my/buffer-help
          ,my/buffer-apropos
          ,my/buffer-compilation
          ,my/buffer-flymake
          ,my/buffer-shell-output
          "\\*eshell.*\\*$" eshell-mode
          "\\*shell.*\\*$"  shell-mode
          "\\*term.*\\*$"   term-mode
          "\\*vterm.*\\*$"  vterm-mode
          "\\*Python.*\\*$" inferior-python-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; --- Enhanced Theme System ---

;; Doom-themes: Popular theme collection including doom-one
(use-package doom-themes
  :straight t
  :defer my/defer-fast
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  
  ;; Manual theme toggle function (automatic switching handled below)
  (defun my/toggle-theme ()
    "Manually toggle between doom-one and doom-one-light."
    (interactive)
    (if (eq (car custom-enabled-themes) 'doom-one)
        (progn
          (disable-theme 'doom-one)
          (load-theme 'doom-one-light t)
          ;; Silent theme switch
          )
      (progn
        (disable-theme 'doom-one-light)
        (load-theme 'doom-one t)
        ;; Silent theme switch
        ))))


;; --- Better Window and Buffer Management ---

;; Ace-window: Fast window switching
(use-package ace-window
  :straight t
  :defer my/defer-medium
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-scope 'frame)
  (setq aw-background t)
  (setq aw-leading-char-style 'char)
  
  ;; Custom face for ace-window
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))

;; Winner mode for window configuration history
(use-package winner
  :straight nil
  :defer my/defer-medium
  :config
  (winner-mode 1)
  (setq winner-boring-buffers-regexp my/buffer-pattern-any))

;; --- Enhanced Visual Elements ---

;; Hl-todo: Highlight TODO/FIXME keywords
(use-package hl-todo
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")
          ("NOTE"   . "#00CED1")
          ("HACK"   . "#FF6347"))))

;; Visual line mode improvements
(use-package visual-fill-column
  :straight t
  :defer my/defer-slow
  :hook ((visual-line-mode . visual-fill-column-mode)
         (org-mode . visual-line-mode))
  :config
  (setq visual-fill-column-width 100)
  (setq visual-fill-column-center-text t))

;; Better whitespace visualization
(use-package whitespace
  :straight nil
  :defer my/defer-slow
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs empty))
  (setq whitespace-line-column 120))

;; --- Enhanced Org Mode Visuals ---

;; Note: org-modern is configured in config/org-config.el to avoid dependency issues

;; --- Better Frame and Window Styling ---

;; Frame and window appearance
(when (display-graphic-p)
  ;; Remove window decorations for cleaner look
  (setq default-frame-alist
        '((tool-bar-lines . 0)
          (menu-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (alpha . 95)))
  
  ;; Better frame title
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))
          " - Emacs")))

;; --- Font Configuration ---

;; Set Fira Code font with size 14
(when (display-graphic-p)
  (condition-case err
      (progn
        ;; Check if Fira Code is available
        (if (find-font (font-spec :name "Fira Code"))
            (progn
              (set-face-attribute 'default nil
                                  :family "Fira Code"
                                  :height 160) ; Height is in 1/10pt, so 140 = 14pt
              
              ;; Set variable-pitch font for better text reading
              (set-face-attribute 'variable-pitch nil
                                  :family "Fira Code"
                                  :height 160)
              ;; Silent font setup
              )
          (progn
            ;; Fallback to Monaco on macOS if Fira Code not available
            (set-face-attribute 'default nil
                                :family "Monaco"
                                :height 160)
            (set-face-attribute 'variable-pitch nil
                                :family "Monaco" 
                                :height 160)
            ;; Silent font fallback
            )))
    (error
     (message "❌ Font configuration error: %s" (error-message-string err)))))

;; --- Advanced Color and Font Enhancements ---

;; Automatic dark/light theme switching based on system
(when (and (eq system-type 'darwin) (display-graphic-p))
  (defun my/auto-theme-switch ()
    "Automatically switch theme based on system appearance."
    (let ((appearance (shell-command-to-string
                       "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light")))
      (if (string-match "Dark" appearance)
          (progn
            (disable-theme 'doom-one-light)
            (load-theme 'doom-one t))
        (progn
          (disable-theme 'doom-one)
          (load-theme 'doom-one-light t)))))
  
  ;; Check theme on startup and periodically
  (my/auto-theme-switch)
  (run-with-timer 300 300 #'my/auto-theme-switch))

;; --- Improved Cursor and Selection ---

;; Better cursor configuration
(setq-default cursor-type 'bar)
(setq cursor-in-non-selected-windows 'hollow)
(setq blink-cursor-blinks 10)
(setq blink-cursor-interval 0.6)

;; Enhanced selection highlighting
(setq mouse-drag-copy-region t)
(setq select-enable-primary t)

;; --- Modern Completion Styling ---

;; Better completion styling
(setq completion-show-help nil)
(setq completions-detailed t)

;; --- UI keybindings are configured in evil-config.el ---

;; --- Performance Optimizations for UI ---

;; Optimize redisplay
(setq redisplay-dont-pause t)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)

;; Better font lock
(setq font-lock-maximum-decoration t)
(setq font-lock-maximum-size nil)

;; Optimize image display
(setq image-animate-loop t)
(setq image-auto-resize 'fit-width)

;; --- Final UI Setup ---

(defun my/setup-modern-ui ()
  "Set up modern UI enhancements."
  (interactive)
  ;; Silent UI setup
  
  ;; Enable useful modes
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1))
  
  ;; Configure window behavior
  (setq switch-to-buffer-obey-display-actions t)
  (setq display-buffer-alist
        `((,my/buffer-help display-buffer-reuse-window)
          (,my/buffer-completions display-buffer-below-selected)))
  
  ;; Silent UI complete
  )

;; Apply UI setup
(add-hook 'emacs-startup-hook #'my/setup-modern-ui)

(provide 'modern-ui)
;;; modern-ui.el ends here
