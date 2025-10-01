;;; modules/modern-languages.el --- Modern language support with Eglot and Tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modern language server and parsing support
;;; Uses Eglot (built-in LSP client) and Tree-sitter for enhanced language features
;;; Replaces LSP-mode with lightweight, built-in alternatives

;;; Code:

(require 'utilities)  ; For defer timing constants

;; --- Tree-sitter Configuration ---

;; Treesit-auto: Automatically use tree-sitter modes
(use-package treesit-auto
  :straight t
  :defer my/defer-fast
  :config
  (setq treesit-auto-install 'prompt)
  
  ;; Set compatible tree-sitter grammar sources
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/tree-sitter/tree-sitter-yaml")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "typescript/src" "typescript.h")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "tsx/src" "tsx.h")))
  
  (global-treesit-auto-mode)
  
  ;; Configure language mappings for tree-sitter modes
  (setq treesit-auto-langs '(c cpp css html javascript json python rust yaml typescript tsx))
  
  ;; Add more languages if available
  (when (treesit-language-available-p 'go)
    (add-to-list 'treesit-auto-langs 'go))
  (when (treesit-language-available-p 'php)
    (add-to-list 'treesit-auto-langs 'php))
  (when (treesit-language-available-p 'lua)
    (add-to-list 'treesit-auto-langs 'lua)))

;; Tree-sitter font-lock enhancement
(setq treesit-font-lock-level 4) ; Maximum highlighting level

;; --- Eglot: Built-in Language Server Protocol ---

(use-package eglot
  :straight nil ; Built into Emacs 29+
  :defer t
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (css-ts-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (html-ts-mode . eglot-ensure))
  :config
  ;; Eglot optimizations
  (setq eglot-autoshutdown t)           ; Shutdown server when last buffer is killed
  (setq eglot-sync-connect nil)         ; Don't block on connection
  (setq eglot-extend-to-xref t)         ; Extend eglot to xref locations
  (setq eglot-ignored-server-capabilities '(:hoverProvider :documentHighlightProvider))
  
  ;; Configure specific language servers
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((html-mode html-ts-mode) . ("vscode-html-language-server" "--stdio")))
  
  ;; Eglot keybindings are configured in evil-config.el
  )

;; --- Language-Specific Configurations ---

;; Python with Tree-sitter
(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-ts-mode) ; Use tree-sitter mode by default
  :config
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil))

;; Enhanced Python environment management
(use-package conda
  :straight t
  :defer t
  :config
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

;; C/C++ with Tree-sitter
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; JavaScript/TypeScript with Tree-sitter
(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))

;; CSS with Tree-sitter
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

;; HTML with Tree-sitter
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

;; JSON with Tree-sitter
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; YAML with Tree-sitter
(add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))

;; --- Modern Syntax Checking ---

;; Flymake (built-in) instead of Flycheck
(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  
  ;; Flymake keybindings are configured in evil-config.el
  )

;; --- Modern Code Formatting ---

;; Apheleia: Async code formatting
(use-package apheleia
  :straight t
  :defer 2
  :config
  (apheleia-global-mode +1)
  
  ;; Configure formatters for different languages
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(black))
  (setf (alist-get 'js-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'typescript-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'css-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'css-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'html-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'html-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'json-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist)
        '(prettier))
  (setf (alist-get 'rust-mode apheleia-mode-alist)
        '(rustfmt))
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist)
        '(rustfmt))
  (setf (alist-get 'go-mode apheleia-mode-alist)
        '(gofmt))
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(gofmt))
  
  ;; Apheleia keybindings are configured in evil-config.el
  )

;; --- Enhanced Programming Utilities ---

;; Smart jump with tree-sitter integration
(use-package dumb-jump
  :straight t
  :defer 3
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-aggressive nil)
  (setq dumb-jump-selector 'completing-read)
  
  ;; Integration with xref
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Enhanced parentheses handling
(use-package smartparens
  :straight t
  :defer 2
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil))

;; --- Tree-sitter Language Installation Helper ---

(defun my/install-tree-sitter-languages ()
  "Install essential tree-sitter language grammars."
  (interactive)
  (let ((languages '(c cpp css html javascript json python rust yaml typescript tsx))
        (failed-installs '())
        (successful-installs '()))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (when (y-or-n-p (format "Install tree-sitter grammar for %s? " lang))
          (condition-case err
              (progn
                (treesit-install-language-grammar lang)
                (push lang successful-installs)
                (message "✅ Successfully installed %s grammar" lang))
            (error
             (push (cons lang (error-message-string err)) failed-installs)
             (message "❌ Failed to install %s grammar: %s" lang (error-message-string err)))))))
    
    ;; Summary message
    (cond
     ((and successful-installs failed-installs)
      (message "Tree-sitter installation completed: %d successful, %d failed"
               (length successful-installs) (length failed-installs)))
     (successful-installs
      (message "✅ All requested tree-sitter grammars installed successfully!"))
     (failed-installs
      (message "❌ Some tree-sitter installations failed. Check messages for details."))
     (t
      (message "Tree-sitter language installation check completed - no new grammars needed!")))))

;; --- Snippet System ---

;; YASnippet: Template system for code snippets
(use-package yasnippet
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)

  ;; Load common snippets
  (use-package yasnippet-snippets
    :straight t
    :defer t)

  ;; Integrate with completion
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (setq yas-buffer-local-condition
                    '(not (memq this-command '(corfu-insert)))))))

;; --- Multiple Cursors ---

;; Multiple cursors for simultaneous editing
(use-package multiple-cursors
  :straight t
  :defer my/defer-slow
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :config
  (setq mc/always-run-for-all t))

;; --- Git Integration Enhancement ---

;; Git gutter for line-by-line git status
(use-package git-gutter
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "~")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")

  ;; Custom faces for git gutter
  (custom-set-faces
   '(git-gutter:modified ((t (:foreground "#e5c07b" :background nil))))
   '(git-gutter:added    ((t (:foreground "#98c379" :background nil))))
   '(git-gutter:deleted  ((t (:foreground "#e06c75" :background nil)))))

  ;; Enable in all modes where it makes sense
  (global-git-gutter-mode +1))

;; --- Better Terminal Integration ---

;; Vterm: Better terminal emulator (optional, requires compilation)
;; Commented out by default as it requires external compilation
;; (use-package vterm
;;   :straight t
;;   :defer t
;;   :commands vterm
;;   :config
;;   (setq vterm-max-scrollback 10000))

;; --- Workspace Management ---

;; Perspective: Workspace management
(use-package perspective
  :straight t
  :defer my/defer-slow
  :commands (persp-switch persp-kill persp-rename)
  :init
  (setq persp-mode-prefix-key (kbd "C-c p"))
  :config
  (persp-mode 1)
  (setq persp-auto-save-fname (expand-file-name "perspectives" user-emacs-directory))
  (add-hook 'kill-emacs-hook #'persp-state-save))

;; --- Additional Development Tools ---

;; Expand region for intelligent selection
(use-package expand-region
  :straight t
  :defer my/defer-medium
  :commands er/expand-region
  :config
  (setq expand-region-contract-fast-key "z")
  (setq expand-region-reset-fast-key "r"))

;; Aggressive indent for automatic indentation
(use-package aggressive-indent
  :straight t
  :defer my/defer-medium
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode))
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Rainbow mode moved to enhanced-colors.el to avoid duplication

;; Highlight symbol at point
(use-package highlight-symbol
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3)
  (setq highlight-symbol-on-navigation-p t))

;; Better diff highlighting
(use-package diff-hl
  :straight t
  :defer my/defer-medium
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; Auto-install tree-sitter grammars helper (disabled to prevent errors)
;; You can manually run M-x my/install-tree-sitter-languages if needed

(provide 'modern-languages)
;;; modern-languages.el ends here