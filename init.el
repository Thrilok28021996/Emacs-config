;;; init.el --- Emacs 30.2 Configuration -*- lexical-binding: t; -*-

;;; ─────────────────────────────────────────────
;;; 0. FRAME — start fullscreen
;;; ─────────────────────────────────────────────

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;;; ─────────────────────────────────────────────
;;; 1. PACKAGE BOOTSTRAP
;;; ─────────────────────────────────────────────

(require 'package)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")
        ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil)

;;; ─────────────────────────────────────────────
;;; 2. PATH INHERITANCE (macOS — must run early)
;;; ─────────────────────────────────────────────

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (dolist (var '("PATH" "MANPATH" "PYTHONPATH" "CONDA_PREFIX"
                 "CONDA_DEFAULT_ENV" "GOPATH" "CARGO_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; ─────────────────────────────────────────────
;;; 3. BASIC UI
;;; ─────────────────────────────────────────────

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(menu-bar-mode   -1)
(tool-bar-mode   -1)
(scroll-bar-mode -1)

;; Relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Font
(defun my/set-font ()
  (cond
   ((find-font (font-spec :name "Victor Mono"))
    (set-face-attribute 'default nil :font "Victor Mono-18"))
   ((find-font (font-spec :name "Menlo"))
    (set-face-attribute 'default nil :font "Menlo-12"))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f) (with-selected-frame f (my/set-font))))
  (my/set-font))

;;; ─────────────────────────────────────────────
;;; 4. EVIL MODE + GENERAL (leader key)
;;; ─────────────────────────────────────────────

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu
        evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode 1))

(use-package general
  :after evil
  :config
  (general-evil-setup t)

  (general-create-definer my/leader-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-def
    ;; ── Top-level ────────────────────────────
    "SPC" '(execute-extended-command             :wk "M-x")
    "TAB" '(evil-switch-to-windows-last-buffer   :wk "last buffer")
    ";"   '(evil-commentary-line                 :wk "comment line")

    ;; ── Files ────────────────────────────────
    "f f" '(find-file           :wk "find file")
    "f r" '(consult-recent-file :wk "recent files")

    ;; ── Buffers ──────────────────────────────
    "b b" '(consult-buffer      :wk "switch buffer")
    "b k" '(kill-current-buffer :wk "kill buffer")

    ;; ── Windows ──────────────────────────────
    "w h" '(evil-window-left        :wk "left")
    "w j" '(evil-window-down        :wk "down")
    "w k" '(evil-window-up          :wk "up")
    "w l" '(evil-window-right       :wk "right")
    "w s" '(split-window-below      :wk "split horiz")
    "w v" '(split-window-right      :wk "split vert")
    "w d" '(delete-window           :wk "close")
    "w o" '(delete-other-windows    :wk "only")
    "w =" '(balance-windows         :wk "balance")
    "w f" '(toggle-frame-fullscreen :wk "fullscreen")

    ;; ── Search ───────────────────────────────
    "s s" '(consult-line    :wk "search buffer")
    "s r" '(consult-ripgrep :wk "search project")
    "s i" '(consult-imenu   :wk "jump to symbol")

    ;; ── Code ─────────────────────────────────
    "c f" '(apheleia-format-buffer          :wk "format buffer")
    "c p" '(my/python-run-current-file      :wk "run python")
    "c c" '(my/cpp-compile-run-current-file :wk "compile & run C++")

    ;; ── LSP ──────────────────────────────────
    "l r" '(lsp-rename              :wk "rename")
    "l d" '(lsp-find-references     :wk "references")
    "l a" '(lsp-execute-code-action :wk "code action")
    "l i" '(lsp-find-implementation :wk "implementation")

    ;; ── Errors ───────────────────────────────
    "e l" '(flycheck-list-errors :wk "list errors")

    ;; ── Git ──────────────────────────────────
    "g g" '(magit-status       :wk "magit")
    "g c" '(magit-commit       :wk "commit")
    "g p" '(magit-push         :wk "push")
    "g l" '(magit-log-current  :wk "log")
    "g d" '(magit-diff-dwim    :wk "diff")
    "g f" '(magit-fetch        :wk "fetch")
    "g b" '(magit-branch       :wk "branch")
    "g s" '(magit-stage        :wk "stage")
    "g u" '(magit-unstage      :wk "unstage")
    "g a" '(magit-blame        :wk "blame")
    "g r" '(magit-rebase       :wk "rebase")
    "g m" '(magit-merge        :wk "merge")
    "g t" '(magit-stash        :wk "stash")
    "g i" '(magit-pull         :wk "pull")

    ;; ── Project ──────────────────────────────
    "p p" '(projectile-switch-project :wk "switch project")
    "p f" '(projectile-find-file      :wk "find file")

    ;; ── Org / Notes ──────────────────────────
    "o a" '(org-agenda              :wk "agenda")
    "o c" '(org-capture             :wk "capture")
    "o r" '(org-roam-node-find      :wk "roam find")
    "o i" '(org-roam-node-insert    :wk "roam insert")
    "o s" '(consult-org-roam-search :wk "roam search")
    "o d" '(deft                    :wk "deft")
    "o u" '(org-roam-ui-open        :wk "roam graph")

    ;; ── Conda ────────────────────────────────
    "m a" '(conda-env-activate   :wk "activate env")
    "m d" '(conda-env-deactivate :wk "deactivate env")

    ;; ── Jump ─────────────────────────────────
    "j j" '(avy-goto-char-2 :wk "jump to char")

    ;; ── Help ─────────────────────────────────
    "h k" '(helpful-key      :wk "key")
    "h f" '(helpful-callable :wk "function")
    "h v" '(helpful-variable :wk "variable")
    "h ." '(helpful-at-point :wk "at point")

    ;; ── Quit ─────────────────────────────────
    "q q" '(save-buffers-kill-terminal :wk "quit")
    "q r" '(restart-emacs              :wk "restart"))

  ;; ── Unbound navigation: ]h/[h hunks, ]e/[e errors ──
  (general-define-key
    :states '(normal visual)
    "]h" '(diff-hl-next-hunk     :wk "next hunk")
    "[h" '(diff-hl-previous-hunk :wk "prev hunk")
    "]e" '(flycheck-next-error   :wk "next error")
    "[e" '(flycheck-previous-error :wk "prev error")))

;;; ─────────────────────────────────────────────
;;; 5. VIM MOVEMENT EXTRAS
;;; ─────────────────────────────────────────────

(with-eval-after-load 'evil
  ;; j/k by visual line (important for wrapped org/markdown)
  (evil-define-key 'normal 'global
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "gj") 'evil-next-line
    (kbd "gk") 'evil-previous-line
    (kbd "Q") 'evil-execute-last-recorded-macro
    (kbd "U") 'evil-redo
    (kbd "Y") (kbd "y$")
    (kbd "g h") 'evil-beginning-of-line
    (kbd "g l") 'evil-end-of-line
    ;; Center after search jump
    (kbd "n")  (lambda () (interactive) (evil-ex-search-next)     (evil-scroll-line-to-center nil))
    (kbd "N")  (lambda () (interactive) (evil-ex-search-previous) (evil-scroll-line-to-center nil))
    (kbd "*")  (lambda () (interactive) (evil-ex-search-word-forward)  (evil-scroll-line-to-center nil))
    (kbd "#")  (lambda () (interactive) (evil-ex-search-word-backward) (evil-scroll-line-to-center nil)))

  ;; Keep selection after indent
  (evil-define-key 'visual 'global
    (kbd ">") (lambda () (interactive) (evil-shift-right (region-beginning) (region-end)) (evil-visual-restore))
    (kbd "<") (lambda () (interactive) (evil-shift-left  (region-beginning) (region-end)) (evil-visual-restore))))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;;; ─────────────────────────────────────────────
;;; 6. COMPLETION (vertico / orderless / consult / marginalia)
;;; ─────────────────────────────────────────────

(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 15))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-s" . consult-line))
  :custom
  (consult-preview-key "M-.")
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;; ─────────────────────────────────────────────
;;; 7. THEME & MODELINE
;;; ─────────────────────────────────────────────

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t))

(use-package nerd-icons)

;;; ─────────────────────────────────────────────
;;; 8. EDITING ESSENTIALS
;;; ─────────────────────────────────────────────

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package yasnippet
  :config (yas-global-mode 1))
(use-package yasnippet-snippets :after yasnippet)

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?\│)
  (highlight-indent-guides-responsive 'top))

;;; ─────────────────────────────────────────────
;;; 9. LSP
;;; ─────────────────────────────────────────────

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((c++-mode . lsp-deferred)
         (c-mode   . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-idle-delay 0.5)
  (lsp-enable-symbol-highlighting t)
  (lsp-signature-auto-activate t)
  (lsp-prefer-flymake nil)
  (lsp-keymap-prefix "C-c l")
  :config
  ;; Evil gd/gr/K → LSP (per-buffer, avoids keymap priority issues)
  (add-hook 'lsp-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "gd") #'lsp-find-definition)
              (evil-local-set-key 'normal (kbd "gD") #'lsp-find-declaration)
              (evil-local-set-key 'normal (kbd "gr") #'lsp-find-references)
              (evil-local-set-key 'normal (kbd "gi") #'lsp-find-implementation)
              (evil-local-set-key 'normal (kbd "K")  #'lsp-describe-thing-at-point))))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-peek-enable t))

;;; ─────────────────────────────────────────────
;;; 10. TREE-SITTER (built-in treesit, Emacs 30)
;;; ─────────────────────────────────────────────

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ─────────────────────────────────────────────
;;; 11. IDE TOOLS
;;; ─────────────────────────────────────────────

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode 1))

(use-package avy
  :commands (avy-goto-char-2 avy-goto-char avy-goto-line))

(use-package rg
  :commands (rg rg-project rg-dwim))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/" "~/code/")
        projectile-completion-system 'default))

(use-package magit
  :commands (magit-status magit-commit magit-push magit-pull magit-fetch
             magit-branch magit-log-current magit-diff-dwim magit-stage
             magit-unstage magit-blame magit-rebase magit-merge magit-stash)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers 'dontask
        magit-diff-refine-hunk 'all))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

(use-package restart-emacs
  :commands restart-emacs)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

;;; ─────────────────────────────────────────────
;;; 12. ORG MODE
;;; ─────────────────────────────────────────────

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-log-done 'time)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-agenda-files '("~/org/"))
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)

  (setq org-capture-templates
        '(("t" "Todo"    entry (file+headline "~/org/inbox.org" "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note"    entry (file+headline "~/org/notes.org" "Notes")
           "* %?\n  %U")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\n  Entered on %U"))))

(use-package org-super-agenda
  :after org
  :config (org-super-agenda-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (expand-file-name "~/org-roam/"))
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

(use-package consult-org-roam
  :after org-roam
  :config (consult-org-roam-mode 1)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep))

(use-package org-roam-ui
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-pomodoro
  :after org
  :custom (org-pomodoro-length 25))

;;; ─────────────────────────────────────────────
;;; 13. MARKDOWN
;;; ─────────────────────────────────────────────

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-command "pandoc"))

(use-package pandoc-mode
  :hook (markdown-mode . pandoc-mode))

(use-package deft
  :commands deft
  :custom
  (deft-directory (expand-file-name "~/org-roam/"))
  (deft-extensions '("org" "md" "txt"))
  (deft-recursive t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]"
           "\\|^#\\+[[:alpha:]_]+:.*$"
           "\\|^:PROPERTIES:.*"
           "\\|^:END:.*"
           "\\|^\\* "
           "\\)"))
  :config
  (defun my/deft-new-note-via-roam ()
    "Create a new org-roam node from Deft's filter string."
    (interactive)
    (let ((title (or (and (> (length deft-filter-regexp) 0)
                          (car deft-filter-regexp))
                     (read-string "Note title: "))))
      (deft-filter-clear)
      (quit-window)
      (org-roam-capture- :node (org-roam-node-create :title title)
                         :props '(:immediate-finish nil))))
  (define-key deft-mode-map (kbd "C-c C-n") #'my/deft-new-note-via-roam))

;;; ─────────────────────────────────────────────
;;; 14. WRITING MODE
;;; ─────────────────────────────────────────────

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 90))

;;; ─────────────────────────────────────────────
;;; 15. PYTHON / CONDA
;;; ─────────────────────────────────────────────

(use-package conda
  :custom
  (conda-anaconda-home        (expand-file-name "~/miniconda3/"))
  (conda-env-home-directory   (expand-file-name "~/miniconda3/"))
  (conda-env-subdirectory     "envs")
  :config
  (conda-env-autoactivate-mode 1))

;;; ─────────────────────────────────────────────
;;; 16. COMPILE / RUN HELPERS
;;; ─────────────────────────────────────────────

(defun my/python-run-current-file ()
  "Run the current Python file."
  (interactive)
  (when (buffer-file-name)
    (let ((python-exe (or (executable-find "python3")
                          (executable-find "python")
                          "python3")))
      (compile (format "%s %s"
                       python-exe
                       (shell-quote-argument buffer-file-name))))))

(defun my/cpp-compile-run-current-file ()
  "Compile and run the current C++ file."
  (interactive)
  (when (buffer-file-name)
    (let* ((src (buffer-file-name))
           (exe (concat (file-name-sans-extension src) ".out")))
      (compile (format "g++ -std=c++17 -Wall %s -o %s && ./%s"
                       (shell-quote-argument src)
                       (shell-quote-argument exe)
                       (shell-quote-argument exe))))))

;;; ─────────────────────────────────────────────
;;; 17. FORMATTERS (apheleia)
;;; ─────────────────────────────────────────────

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setf (alist-get 'python-mode     apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode  apheleia-mode-alist) 'ruff)
  (setf (alist-get 'c-mode          apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode        apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode     apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'js-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode     apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'markdown-mode   apheleia-mode-alist) 'prettier)
  (setf (alist-get 'sh-mode         apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'rust-mode       apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'go-mode         apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) nil))

;;; ─────────────────────────────────────────────
;;; 18. PERSISTENCE
;;; ─────────────────────────────────────────────

(setq auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(save-place-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 50
      recentf-max-saved-items 100)

(savehist-mode 1)

;;; ─────────────────────────────────────────────
;;; 19. DEFAULTS
;;; ─────────────────────────────────────────────

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq use-short-answers t
      scroll-conservatively 101
      scroll-margin 3)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)

;;; ─────────────────────────────────────────────
;;; 20. CUSTOM FILE (keep init.el clean)
;;; ─────────────────────────────────────────────

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
