;;; modules/modern-completion.el --- Modern completion with Vertico ecosystem -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modern completion system using Vertico + Consult + Embark + Corfu
;;; Replaces the older Ivy/Counsel/Swiper stack with faster, more modular alternatives
;;; This is the 2024-2025 standard for Emacs completion

;;; Code:

(require 'utilities)  ; For defer timing constants

;; --- Core Completion Framework ---

;; Vertico: VERTical Interactive COmpletion
(use-package vertico
  :straight t
  :defer my/defer-immediate
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize t
        vertico-count 12)
  ;; Improve directory navigation
  (with-eval-after-load 'rfn-eshadow
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)))

;; Vertico extensions
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Orderless: Flexible completion matching
(use-package orderless
  :straight t
  :defer my/defer-fast
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: Rich annotations in minibuffer
(use-package marginalia
  :straight t
  :defer my/defer-fast
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'right
        marginalia-max-relative-age 0))

;; --- Enhanced Search and Navigation ---

;; Consult: Enhanced search, navigation, and completion commands
(use-package consult
  :straight t
  :defer my/defer-fast
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Optionally configure preview key
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key
  (setq consult-narrow-key "<"))

;; --- Context Actions ---

;; Embark: Context-sensitive actions
(use-package embark
  :straight t
  :defer my/defer-fast
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Embark-Consult integration
(use-package embark-consult
  :straight t
  :defer my/defer-medium
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; --- In-Buffer Completion ---

;; Corfu: Completion Overlay Region FUnction
(use-package corfu
  :straight t
  :defer my/defer-fast
  :init
  (global-corfu-mode)
  :config
  ;; PERFORMANCE: Optimized for speed
  (setq corfu-cycle t                  ;; Enable cycling for `corfu-next/previous'
        corfu-auto t                   ;; Enable auto completion
        corfu-auto-delay 0.3           ;; Slightly longer delay for performance (was 0.2)
        corfu-auto-prefix 2            ;; Minimum prefix length for auto completion
        corfu-separator ?\s            ;; Orderless field separator
        corfu-quit-at-boundary nil     ;; Never quit at completion boundary
        corfu-quit-no-match nil        ;; Never quit, even if there is no match
        corfu-preview-current nil      ;; Disable current candidate preview
        corfu-preselect 'prompt        ;; Preselect the prompt
        corfu-on-exact-match nil       ;; Configure handling of exact matches
        corfu-scroll-margin 5          ;; Use scroll margin
        corfu-count 10)                ;; Limit candidates shown (performance)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key
  (setq tab-always-indent 'complete))

;; Corfu extensions
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package corfu-history
  :straight nil
  :after corfu
  :config
  (corfu-history-mode))

;; Kind-icon: Icons for completion candidates
(use-package kind-icon
  :straight t
  :defer my/defer-medium
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; --- Evil Integration ---

;; Completion keybindings are configured in evil-config.el

;; --- Performance Optimizations ---

;; A few more useful configurations for better performance
(setq enable-recursive-minibuffers t)

;; Support opening new minibuffers from inside existing minibuffers
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Hide commands in M-x which do not work in the current mode
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Persist history over Emacs restarts
(use-package savehist
  :straight nil
  :defer my/defer-fast
  :init
  (savehist-mode)
  :config
  (setq history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; --- Additional Completion Enhancements ---

;; Cape: Completion at point extensions
(use-package cape
  :straight t
  :defer my/defer-medium
  :init
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  :config
  ;; Configure cape for better completion
  (setq cape-dabbrev-min-length 3))

;; Abbrev for text expansion
(use-package abbrev
  :straight nil
  :defer my/defer-slow
  :hook (text-mode . abbrev-mode)
  :config
  (setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(provide 'modern-completion)
;;; modern-completion.el ends here