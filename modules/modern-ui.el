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

;; Note: Using mood-line instead of doom-modeline for better performance
;; doom-modeline can be enabled by uncommenting and customizing above

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
          ,my/buffer-compilation
          ,my/buffer-completions
          ,my/buffer-warnings
          ,my/buffer-help
          ,my/buffer-apropos
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
  :init
  ;; Load theme immediately for better visual experience
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))


;; Manual theme toggle function
(defun my/toggle-theme ()
  "Manually toggle between doom-one and doom-one-light."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-one)
      (progn
        (disable-theme 'doom-one)
        (load-theme 'doom-one-light t)
        (message "Switched to light theme"))
    (progn
      (disable-theme 'doom-one-light)
      (load-theme 'doom-one t)
      (message "Switched to dark theme"))))

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
;; REMOVED: visual-fill-column - adds overhead, rarely needed

;; Better whitespace visualization
(use-package whitespace
  :straight nil
  :defer my/defer-slow
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs empty))
  (setq whitespace-line-column 120))

;; --- Frame and Window Styling ---
;; Note: org-modern is configured in config/org-config.el to avoid dependency issues

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
                                  :height 160))
          (progn
            ;; Fallback to Monaco on macOS if Fira Code not available
            (set-face-attribute 'default nil
                                :family "Monaco"
                                :height 160)
            (set-face-attribute 'variable-pitch nil
                                :family "Monaco"
                                :height 160))))
    (error
     (message "❌ Font configuration error: %s" (error-message-string err)))))

;; --- Cursor and Selection ---

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

;; --- Performance Optimizations for UI ---
;; Note: UI keybindings are configured in evil-config.el

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
  ;; Enable useful modes
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1))
  
  ;; Configure window behavior
  (setq switch-to-buffer-obey-display-actions t)
  (setq display-buffer-alist
        `((,my/buffer-help display-buffer-reuse-window)
          (,my/buffer-completions display-buffer-below-selected))))

;; Apply UI setup
(add-hook 'emacs-startup-hook #'my/setup-modern-ui)

;; --- EPUB Reader ---

;; visual-fill-column: Better text centering and wrapping
(use-package visual-fill-column
  :straight t
  :defer t
  :config
  (setq-default visual-fill-column-width 80)
  (setq-default visual-fill-column-center-text t))

;; nov.el: EPUB reader for Emacs
(use-package nov
  :straight t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  ;; Text width settings - use full window width
  (setq nov-text-width t)

  ;; Customize unzip program if needed
  ;; (setq nov-unzip-program "unzip")

  ;; Font and visual setup for nov mode
  (defun my/nov-setup ()
    "Setup font and visual settings for nov mode."
    (face-remap-add-relative 'variable-pitch
                              :family "Fira Code"
                              :height 1.2)
    (visual-line-mode 1)
    (when (fboundp 'visual-fill-column-mode)
      (setq-local visual-fill-column-center-text t)
      (visual-fill-column-mode 1)))

  (add-hook 'nov-mode-hook #'my/nov-setup))

;; Nov.el keybindings (available in nov-mode):
;; n/p       - next/previous chapter
;; g         - go to chapter
;; t         - display table of contents
;; TAB       - jump to next link
;; <backtab> - jump to previous link
;; RET       - follow link
;; C-x C-+/- - increase/decrease font size

(provide 'modern-ui)
;;; modern-ui.el ends here
