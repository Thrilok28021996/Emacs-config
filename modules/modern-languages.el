;;; modules/modern-languages.el --- Modern language support with Eglot and Tree-sitter -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modern language server and parsing support
;;; Uses Eglot (built-in LSP client) and Tree-sitter for enhanced language features
;;; Replaces LSP-mode with lightweight, built-in alternatives

;;; Code:

(require 'utilities)  ; For defer timing constants

;; --- Tree-sitter Configuration ---

;; Set tree-sitter grammar sources BEFORE treesit-auto loads
;; This ensures grammars can be installed automatically
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.3")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (html "https://github.com/tree-sitter/tree-sitter-html")))

;; Treesit-auto: Automatically use tree-sitter modes
(use-package treesit-auto
  :straight t
  :defer my/defer-medium
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode)

  ;; PERFORMANCE: Only enable essential languages by default
  (setq treesit-auto-langs '(c cpp python json css html)))

;; Tree-sitter font-lock - balanced performance
(setq treesit-font-lock-level 3) ; Level 3 for speed (was 4)

;; --- Eglot: Built-in Language Server Protocol ---

(use-package eglot
  :straight nil ; Built into Emacs 29+
  :defer t
  :commands (eglot eglot-ensure)
  ;; PERFORMANCE: Manual Eglot activation via SPC l e
  ;; Hooks DISABLED to prevent freezing on file open
  ;; Use M-x eglot or SPC c l to start LSP manually when needed
  ;; :hook ((python-mode . eglot-ensure)
  ;;        (python-ts-mode . eglot-ensure)
  ;;        (c++-mode . eglot-ensure)
  ;;        (c++-ts-mode . eglot-ensure))
  :config
  ;; Eglot performance optimizations
  (setq eglot-autoshutdown t)                    ; Shutdown server when last buffer is killed
  (setq eglot-sync-connect nil)                  ; Don't block on connection (async)
  (setq eglot-events-buffer-size 0)              ; Disable event logging for performance
  (setq eglot-send-changes-idle-time 0.5)        ; Debounce change notifications
  (setq eglot-extend-to-xref t)                  ; Extend eglot to xref locations

  ;; Disable heavy features for better performance
  (setq eglot-ignored-server-capabilities
        '(:hoverProvider                          ; Disable hover (use manually with K)
          :documentHighlightProvider              ; Disable auto-highlight
          :documentOnTypeFormattingProvider       ; Disable format-on-type
          :colorProvider))
  
  ;; Configure specific language servers
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
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
  :hook (python-mode . (lambda ()
                         (setq-local tab-width 4)
                         (setq-local python-indent-offset 4)))
  :config
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil)

  ;; iPython if available
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")))

;; Enhanced Python environment management with conda (primary)
(use-package conda
  :straight t
  :defer t
  :config
  ;; Auto-detect conda installation
  (setq conda-anaconda-home
        (or (getenv "CONDA_PREFIX")
            (expand-file-name "~/anaconda3")
            (expand-file-name "~/miniconda3")
            (expand-file-name "~/opt/anaconda3")
            (expand-file-name "~/opt/miniconda3")))

  ;; Conda configuration
  (setq conda-env-home-directory conda-anaconda-home)

  ;; Only initialize if conda exists
  (when (file-directory-p conda-anaconda-home)
    ;; PERFORMANCE: Delay conda initialization to prevent freezing
    ;; Initialize only when actually needed (first Python file)
    ;; (conda-env-initialize-interactive-shells)
    ;; (conda-env-initialize-eshell)

    ;; Auto-activate conda env when opening Python files
    (defun my/auto-activate-conda-env ()
      "Auto-activate conda environment from project."
      (when-let* ((project-root (locate-dominating-file default-directory "environment.yml"))
                  (env-file (expand-file-name "environment.yml" project-root)))
        ;; Try to find env name from environment.yml
        (with-temp-buffer
          (insert-file-contents env-file)
          (goto-char (point-min))
          (when (re-search-forward "^name:\\s-*\\(.+\\)$" nil t)
            (let ((env-name (match-string 1)))
              (unless (string= conda-env-current-name env-name)
                (conda-env-activate env-name)
                (message "Activated conda environment: %s" env-name)))))))

    (add-hook 'python-mode-hook #'my/auto-activate-conda-env)
    (add-hook 'python-ts-mode-hook #'my/auto-activate-conda-env)))

;; Pyvenv (optional fallback for .venv users - disabled by default)
;; Uncomment if you also use standard venv alongside conda
;; (use-package pyvenv
;;   :straight t
;;   :defer t
;;   :config
;;   (pyvenv-mode 1))

;; Pytest integration for Python testing
(use-package python-pytest
  :straight t
  :after python
  :defer t
  :config
  (setq python-pytest-arguments
        '("--color"           ;; colored output
          "--failed-first"    ;; run failed tests first
          "--maxfail=5")))    ;; stop after 5 failures

;; py-isort: Automatic Python import sorting
(use-package py-isort
  :straight t
  :defer t
  :hook ((python-mode . py-isort-before-save)
         (python-ts-mode . py-isort-before-save)))

;; C/C++ with Tree-sitter
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; C/C++ mode configuration
(use-package cc-mode
  :straight nil
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.c\\'" . c-ts-mode))
  :config
  (setq c-default-style "linux"
        c-basic-offset 4)

  ;; Better C++ indentation
  (c-set-offset 'innamespace 0)   ;; Don't indent inside namespaces
  (c-set-offset 'case-label '+)   ;; Indent case labels

  ;; Quick compile function for C++
  (defun my/cpp-compile ()
    "Compile current C++ file with modern standards."
    (interactive)
    (let* ((file (buffer-file-name))
           (output (file-name-sans-extension file)))
      (compile (format "g++ -std=c++20 -Wall -Wextra -g %s -o %s"
                       (shell-quote-argument file)
                       (shell-quote-argument output)))))

  ;; Header/Source file switching
  (defun my/switch-header-source ()
    "Switch between header and source file."
    (interactive)
    (let* ((extension (file-name-extension (buffer-file-name)))
           (base-name (file-name-sans-extension (buffer-file-name)))
           (other-file
            (cond
             ((string= extension "cpp") (concat base-name ".hpp"))
             ((string= extension "hpp") (concat base-name ".cpp"))
             ((string= extension "cc") (concat base-name ".h"))
             ((string= extension "h")
              ;; Try .cc first, then .cpp
              (let ((cc-file (concat base-name ".cc"))
                    (cpp-file (concat base-name ".cpp")))
                (if (file-exists-p cc-file)
                    cc-file
                  cpp-file)))
             ((string= extension "c") (concat base-name ".h"))
             (t (concat base-name ".cpp")))))
      (if (file-exists-p other-file)
          (find-file other-file)
        (if (y-or-n-p (format "File %s doesn't exist. Create it? " other-file))
            (find-file other-file)
          (message "Other file not found: %s" other-file)))))

  ;; Keybindings
  (define-key c++-mode-map (kbd "C-c C-c") 'my/cpp-compile)
  (define-key c++-ts-mode-map (kbd "C-c C-c") 'my/cpp-compile)
  (define-key c++-mode-map (kbd "C-c o") 'my/switch-header-source)
  (define-key c++-ts-mode-map (kbd "C-c o") 'my/switch-header-source)
  (define-key c-mode-map (kbd "C-c o") 'my/switch-header-source)
  (define-key c-ts-mode-map (kbd "C-c o") 'my/switch-header-source))

;; CMake support
(use-package cmake-mode
  :straight t
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; Modern C++ font-lock (better syntax highlighting)
(use-package modern-cpp-font-lock
  :straight t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)
         (c++-ts-mode . modern-c++-font-lock-mode)))

;; CSS with Tree-sitter
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

;; HTML with Tree-sitter
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

;; JSON with Tree-sitter
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; --- Web Development Enhancements ---

;; Web-mode: Multi-language editing for HTML templates
(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jinja2?\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight nil))

;; Emmet: Abbreviation expansion for HTML/CSS
(use-package emmet-mode
  :straight t
  :defer t
  :hook ((html-mode . emmet-mode)
         (html-ts-mode . emmet-mode)
         (css-mode . emmet-mode)
         (css-ts-mode . emmet-mode)
         (web-mode . emmet-mode)
         (scss-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-expand-jsx-className? nil))

;; Auto-rename-tag: Sync opening/closing HTML tags
(use-package auto-rename-tag
  :straight t
  :defer t
  :hook ((html-mode . auto-rename-tag-mode)
         (html-ts-mode . auto-rename-tag-mode)
         (web-mode . auto-rename-tag-mode)))

;; SCSS/SASS support
(use-package scss-mode
  :straight t
  :defer t
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (setq scss-compile-at-save nil))

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
  (setf (alist-get 'c-mode apheleia-mode-alist)
        '(clang-format))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist)
        '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist)
        '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist)
        '(clang-format))
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

;; --- Lookup System ---
;; Doom-style lookup with Eglot -> xref fallback chain

(defun my/lookup-definition ()
  "Jump to definition using Eglot if available, falling back to xref."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-definition)
    (xref-find-definitions (thing-at-point 'symbol t))))

(defun my/lookup-references ()
  "Find references using Eglot if available, falling back to xref."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-references)
    (xref-find-references (thing-at-point 'symbol t))))

(defun my/lookup-documentation ()
  "Show documentation using Eglot eldoc, helpful, or eldoc fallback."
  (interactive)
  (cond
   ((and (bound-and-true-p eglot--managed-mode)
         (eglot-current-server))
    (eldoc-doc-buffer))
   ((fboundp 'helpful-at-point)
    (helpful-at-point))
   (t
    (eldoc))))

(defun my/lookup-type-definition ()
  "Jump to type definition using Eglot."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-typeDefinition)
    (message "Type definition requires an active LSP server (start with SPC l e)")))

;; Enhanced parentheses handling
(use-package smartparens
  :straight t
  :defer 2
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  ;; ENABLED: Show matching brackets with highlighting
  (setq sp-highlight-pair-overlay t)
  (setq sp-highlight-wrap-overlay t)
  (setq sp-highlight-wrap-tag-overlay t)
  ;; Show matching pairs immediately
  (setq sp-show-pair-delay 0)
  (setq sp-show-pair-from-inside t))

;; --- Tree-sitter Language Installation Helper ---

(defun my/install-tree-sitter-languages ()
  "Install essential tree-sitter language grammars interactively."
  (interactive)
  (let ((languages '(c cpp css html json python))
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

(defun my/auto-install-tree-sitter-languages ()
  "Automatically install missing tree-sitter language grammars on startup."
  (let ((languages '(c cpp css html json python))
        (failed-installs '())
        (successful-installs '())
        (skipped-installs '())
        (missing-count 0))

    ;; First, count how many are missing
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (setq missing-count (1+ missing-count))))

    ;; Only proceed if there are missing grammars
    (when (> missing-count 0)
      (message "🔧 Installing %d missing tree-sitter grammars..." missing-count)

      (dolist (lang languages)
        (unless (treesit-language-available-p lang)
          ;; Check if recipe exists
          (if (assoc lang treesit-language-source-alist)
              (condition-case err
                  (progn
                    ;; Suppress warnings during installation
                    (let ((warning-minimum-level :error))
                      (treesit-install-language-grammar lang))
                    (push lang successful-installs)
                    (message "  ✅ Installed %s grammar" lang))
                (error
                 (push (cons lang (error-message-string err)) failed-installs)
                 (message "  ❌ Failed to install %s: %s" lang (error-message-string err))))
            ;; No recipe found
            (push lang skipped-installs)
            (message "  ⏭️  Skipped %s (no recipe)" lang))))

      ;; Summary message
      (let ((total-attempted (+ (length successful-installs) (length failed-installs))))
        (cond
         ((> total-attempted 0)
          (if (> (length failed-installs) 0)
              (message "🌳 Tree-sitter: %d/%d installed successfully"
                       (length successful-installs) total-attempted)
            (message "🌳 Tree-sitter: All %d grammars installed successfully!"
                     (length successful-installs))))
         (t
          (message "🌳 Tree-sitter: All grammars already installed")))))))

;; Auto-install tree-sitter grammars on startup (runs after 3 seconds idle)
(run-with-idle-timer 3 nil #'my/auto-install-tree-sitter-languages)

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

  ;; Doom Emacs snippets
  (use-package doom-snippets
    :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
    :after yasnippet
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

;; CONFLICT RESOLVED: Using diff-hl instead of git-gutter for better performance
;; diff-hl integrates with VC and Magit, while git-gutter is standalone
;; git-gutter removed to avoid duplication and reduce prog-mode hooks

;; Better diff highlighting with VC integration
(use-package diff-hl
  :straight t
  :defer my/defer-medium
  :commands (diff-hl-mode diff-hl-dired-mode global-diff-hl-mode)
  ;; PERFORMANCE: Load only when needed, not on every prog-mode buffer
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  ;; Disable flydiff for performance (updates on save instead of live)
  (setq diff-hl-flydiff-delay 2)  ; Increase delay if flydiff is needed

  ;; Magit integration
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; Custom faces for diff-hl
  (custom-set-faces
   '(diff-hl-change ((t (:foreground "#e5c07b" :background nil))))
   '(diff-hl-insert ((t (:foreground "#98c379" :background nil))))
   '(diff-hl-delete ((t (:foreground "#e06c75" :background nil))))))

;; --- Workspace Management ---

;; Perspective: Workspace management
(use-package perspective
  :straight t
  :defer my/defer-slow
  :commands (persp-switch persp-kill persp-rename persp-switch-to-buffer)
  :init
  (setq persp-mode-prefix-key (kbd "C-c p"))
  :config
  (persp-mode 1)
  (setq persp-auto-save-fname (expand-file-name "perspectives" user-emacs-directory))
  (setq persp-show-modestring t)
  (setq persp-modestring-short t)
  (add-hook 'kill-emacs-hook #'persp-state-save))

(defun my/persp-switch-by-number (n)
  "Switch to perspective number N."
  (let ((names (persp-names)))
    (if (<= n (length names))
        (persp-switch (nth (1- n) names))
      (message "Workspace %d does not exist" n))))

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

;; --- Project Management ---

;; Projectile for project-aware operations
(use-package projectile
  :straight t
  :defer 2
  :init
  (projectile-mode +1)
  :config
  ;; Project search paths
  (setq projectile-project-search-path '("~/projects/" "~/Documents/" "~/work/"))

  ;; Use default completion system (works with vertico/consult)
  (setq projectile-completion-system 'default)

  ;; Enable caching for better performance
  (setq projectile-enable-caching t)

  ;; Indexing method
  (setq projectile-indexing-method 'alien)  ; Use external tools (faster)

  ;; Project file markers
  (setq projectile-project-root-files
        '(".projectile" ".git" ".hg" ".svn" "Makefile" "CMakeLists.txt"
          "setup.py" "requirements.txt" "environment.yml"))

  ;; Compilation commands for different project types
  (setq projectile-project-compilation-cmd
        '((cmake . "cmake --build build/")
          (make . "make")
          (python . "python -m pytest")))

  ;; Test commands
  (setq projectile-project-test-cmd
        '((cmake . "cd build && ctest")
          (make . "make test")
          (python . "python -m pytest")))

  ;; Run commands
  (setq projectile-project-run-cmd
        '((python . "python main.py"))))

;; --- File Templates (Auto-Insert) ---

(defvar my/template-dir (expand-file-name "templates/code" user-emacs-directory)
  "Directory containing file templates.")

(defun my/template-expand ()
  "Expand placeholders in the current buffer after auto-insert."
  (let ((filename (file-name-nondirectory (or buffer-file-name "")))
        (date (format-time-string "%Y-%m-%d")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "__FILENAME__" nil t)
        (replace-match filename t t))
      (goto-char (point-min))
      (while (search-forward "__DATE__" nil t)
        (replace-match date t t))
      (goto-char (point-min))
      (while (search-forward "__GUARD__" nil t)
        (replace-match
         (upcase (replace-regexp-in-string
                  "[^a-zA-Z0-9]" "_"
                  (file-name-nondirectory (or buffer-file-name "HEADER"))))
         t t)))))

(use-package autoinsert
  :straight nil
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (setq auto-insert-directory my/template-dir)

  (define-auto-insert "\\.py\\'" ["python.py" my/template-expand])
  (define-auto-insert "\\.\\(cpp\\|cc\\)\\'" ["cpp.cpp" my/template-expand])
  (define-auto-insert "\\.\\(h\\|hpp\\)\\'" ["header.h" my/template-expand])
  (define-auto-insert "\\.html\\'" ["html.html" my/template-expand])
  (define-auto-insert "\\.css\\'" ["css.css" my/template-expand]))

(provide 'modern-languages)
;;; modern-languages.el ends here