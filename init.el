;;; init.el --- Emacs 30.2 Configuration -*- lexical-binding: t; -*-
;;
;; Built-ins: use-package, eglot, flymake, project, electric-pair,
;;            treesit, which-key, pixel-scroll-precision, repeat,
;;            savehist, recentf, save-place, winner, sqlite.

;;; ─────────────────────────────────────────────
;;; 1. PACKAGE BOOTSTRAP
;;; ─────────────────────────────────────────────

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; ─────────────────────────────────────────────
;;; 2. PATH (macOS)
;;; ─────────────────────────────────────────────

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer 1
  :custom
  (exec-path-from-shell-arguments '("-l"))   ; login shell only, not interactive — skips ~/.zshrc
  :config
  (dolist (var '("PATH" "MANPATH" "PYTHONPATH" "CONDA_PREFIX"
                 "CONDA_DEFAULT_ENV" "GOPATH" "CARGO_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; ─────────────────────────────────────────────
;;; 3. UI
;;; ─────────────────────────────────────────────

(setq inhibit-startup-message t)

;; *scratch* in lisp-interaction fires every prog-mode hook at startup —
;; fundamental-mode skips that; M-x lisp-interaction-mode when needed
(setq initial-major-mode 'fundamental-mode)
;; inhibit-startup-echo-area-message only works as a literal (setq ... "username")
;; in the init file — override the printer instead, stays portable
(advice-add 'display-startup-echo-area-message :override #'ignore)

(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(pixel-scroll-precision-mode 1)

(defun my/set-font ()
  (cond
   ((find-font (font-spec :name "Victor Mono"))
    (set-face-attribute 'default nil :font "Victor Mono-18"))
   ((find-font (font-spec :name "Menlo"))
    (set-face-attribute 'default nil :font "Menlo-12"))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (f) (with-selected-frame f (my/set-font))))
  (my/set-font)
  (add-hook 'after-make-frame-functions
            (lambda (f) (with-selected-frame f (my/set-font)))))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config (tab-bar-mode 1))

;;; ─────────────────────────────────────────────
;;; 4. KEYBINDINGS
;;; ─────────────────────────────────────────────

;; Redo — Emacs 28+ builtin undo-redo
(global-set-key (kbd "C-?") #'undo-redo)

(global-set-key (kbd "M-o") #'other-window)

(global-set-key (kbd "C-c i r") #'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-c i l") #'indent-rigidly-left-to-tab-stop)

;; ── Buffers (b) ──────────────────────────────
(global-set-key (kbd "C-c b b")   #'consult-buffer)
(global-set-key (kbd "C-c b k")   #'kill-current-buffer)
(global-set-key (kbd "C-c b TAB") #'mode-line-other-buffer)

;; ── Code (c): LSP + format + compile ─────────
(global-set-key (kbd "C-c c r") #'eglot-rename)
(global-set-key (kbd "C-c c a") #'eglot-code-actions)
(global-set-key (kbd "C-c c i") #'eglot-find-implementation)
(global-set-key (kbd "C-c c d") #'xref-find-references)
(global-set-key (kbd "C-c c f") #'apheleia-format-buffer)
(global-set-key (kbd "C-c c p") #'my/python-run-current-file)
(global-set-key (kbd "C-c c c") #'my/cpp-compile-run-current-file)
(global-set-key (kbd "C-c c b") #'dape-breakpoint-toggle)
(global-set-key (kbd "C-c c B") #'dape)

;; ── Errors (e): flymake ───────────────────────
(global-set-key (kbd "C-c e l") #'consult-flymake)
(global-set-key (kbd "C-c e n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") #'flymake-goto-prev-error)

;; ── Files (f) ─────────────────────────────────
(global-set-key (kbd "C-c f r") #'consult-recent-file)

;; ── Git (g): magit + hunks ────────────────────
(global-set-key (kbd "C-c g g") #'magit-status)
(global-set-key (kbd "C-c g c") #'magit-commit)
(global-set-key (kbd "C-c g p") #'magit-push)
(global-set-key (kbd "C-c g u") #'magit-pull)
(global-set-key (kbd "C-c g f") #'magit-fetch)
(global-set-key (kbd "C-c g l") #'magit-log-current)
(global-set-key (kbd "C-c g d") #'magit-diff-dwim)
(global-set-key (kbd "C-c g b") #'magit-branch)
(global-set-key (kbd "C-c g a") #'magit-blame)
(global-set-key (kbd "C-c g s") #'magit-stash)
(global-set-key (kbd "C-c g n") #'diff-hl-next-hunk)
(global-set-key (kbd "C-c g N") #'diff-hl-previous-hunk)

;; ── Jump (j) ──────────────────────────────────
(global-set-key (kbd "C-c j") #'avy-goto-char-2)

;; ── Notes (n): capture shortcuts ──────────────
(global-set-key (kbd "C-c n i") (lambda () (interactive) (org-capture nil "i")))
(global-set-key (kbd "C-c n j") (lambda () (interactive) (org-capture nil "j")))
(global-set-key (kbd "C-c n v") (lambda () (interactive) (org-capture nil "v")))
(global-set-key (kbd "C-c n r") (lambda () (interactive) (org-capture nil "r")))
(global-set-key (kbd "C-c n w") (lambda () (interactive) (org-capture nil "w")))
(global-set-key (kbd "C-c n W") (lambda () (interactive) (org-capture nil "W")))
(global-set-key (kbd "C-c n t") (lambda () (interactive) (org-capture nil "t")))
(global-set-key (kbd "C-c n p") (lambda () (interactive) (org-capture nil "p")))
(global-set-key (kbd "C-c n k") #'my/roam-capture-concept)
(global-set-key (kbd "C-c n q") #'my/roam-capture-question)
(global-set-key (kbd "C-c n P") #'my/roam-capture-person)
(global-set-key (kbd "C-c n x") #'my/roam-log-interaction)
(global-set-key (kbd "C-c n a") #'my/learn-review)
(global-set-key (kbd "C-c n d") #'my/learn-reviewed)
(global-set-key (kbd "C-c n s") #'my/learn-search)

;; ── Org (o) ───────────────────────────────────
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o r") #'org-roam-node-find)
(global-set-key (kbd "C-c o i") #'org-roam-node-insert)
(global-set-key (kbd "C-c o s") #'consult-org-roam-search)
(global-set-key (kbd "C-c o u") #'org-roam-ui-open)
(global-set-key (kbd "C-c o l") #'org-cliplink)
(global-set-key (kbd "C-c o t") #'org-transclusion-mode)
(global-set-key (kbd "C-c o y") #'org-download-yank)
(global-set-key (kbd "C-c o I") #'org-clock-in)
(global-set-key (kbd "C-c o O") #'org-clock-out)
(global-set-key (kbd "C-c o R") #'org-clock-report)
(global-set-key (kbd "C-c o e") #'org-set-effort)
(global-set-key (kbd "C-c o q") #'org-ql-find)
(global-set-key (kbd "C-c o k") #'org-kanban/initialize)
(global-set-key (kbd "C-c o A") #'org-archive-subtree)

;; ── Projects (p) ──────────────────────────────
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)
(global-set-key (kbd "C-c p s") #'project-eshell)
(global-set-key (kbd "C-c p c") #'project-compile)

;; ── Search (s) ────────────────────────────────
(global-set-key (kbd "C-c s s") #'consult-line)
(global-set-key (kbd "C-c s r") #'consult-ripgrep)
(global-set-key (kbd "C-c s i") #'consult-imenu)
(global-set-key (kbd "C-c s t") #'consult-todo)

;; ── Virtual env (v): conda ────────────────────
(global-set-key (kbd "C-c v a") #'conda-env-activate)
(global-set-key (kbd "C-c v d") #'conda-env-deactivate)

;; ── Windows (w) ───────────────────────────────
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w U") #'winner-redo)
(global-set-key (kbd "C-c w f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w H") #'shrink-window-horizontally)
(global-set-key (kbd "C-c w L") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c w K") #'shrink-window)
(global-set-key (kbd "C-c w J") #'enlarge-window)

;; ── AI (a) ────────────────────────────────────
(global-set-key (kbd "C-c a c") #'gptel)
(global-set-key (kbd "C-c a s") #'gptel-send)
(global-set-key (kbd "C-c a r") #'gptel-rewrite)
(global-set-key (kbd "C-c a m") #'gptel-menu)

;; ── Help (h) ──────────────────────────────────
(global-set-key (kbd "C-c h k") #'helpful-key)
(global-set-key (kbd "C-c h f") #'helpful-callable)
(global-set-key (kbd "C-c h v") #'helpful-variable)
(global-set-key (kbd "C-c h .") #'helpful-at-point)

;; ── Quit (q) ──────────────────────────────────
(global-set-key (kbd "C-c q q") #'save-buffers-kill-terminal)
(global-set-key (kbd "C-c q r") #'restart-emacs)

;; which-key — built-in since Emacs 30
(use-package which-key
  :ensure nil
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode 1))

(repeat-mode 1)

;;; ─────────────────────────────────────────────
;;; 5. COMPLETION (vertico + corfu)
;;; ─────────────────────────────────────────────

(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file       (styles basic partial-completion))
                                   (eglot      (styles orderless))
                                   (eglot-capf (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind ("C-s" . consult-line)
  :custom
  (consult-preview-key "M-.")
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  :config
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  :bind (:map corfu-map
         ("C-n"      . corfu-next)
         ("C-p"      . corfu-previous)
         ("<tab>"    . corfu-insert)
         ("TAB"      . corfu-insert)
         ("<escape>" . corfu-quit)
         ("C-g"      . corfu-quit))
  :config
  (keymap-unset corfu-map "RET")
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-history-mode 1)
  (require 'savehist)
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-keyword nil t)
              (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t))))

(use-package embark
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :bind ("C-." . embark-act)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :commands wgrep-change-to-wgrep-mode)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-todo
  :vc (:url "https://github.com/liuyinz/consult-todo" :rev :newest)
  :after (consult hl-todo))

;;; ─────────────────────────────────────────────
;;; 6. THEME & MODELINE
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
  (doom-modeline-major-mode-icon t)
  (doom-modeline-which-function t)
  :config
  (which-function-mode 1)
  (add-hook 'after-make-frame-functions
            (lambda (f) (with-selected-frame f
                          (setq doom-modeline-icon (display-graphic-p))))))

(use-package nerd-icons :defer t)

;;; ─────────────────────────────────────────────
;;; 7. EDITING
;;; ─────────────────────────────────────────────

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets :after yasnippet)

(use-package ws-butler :hook (prog-mode . ws-butler-mode))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'top))

(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el" :rev :newest)
  :config
  (ligature-set-ligatures 'prog-mode
    '("->" "=>" "!=" ">=" "<=" "==" "===" "!==" "::" "..."
      "++" "--" "||" "&&" "??" ":=" "<-" "<>" "<<" ">>" "<=>" "/**" "/*" "*/"))
  (when (display-graphic-p) (global-ligature-mode t)))

(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo" :rev :newest)
  :hook (prog-mode . hl-todo-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
         ("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-Q" . symbol-overlay-query-replace))
  :config
  ;; transient map (active on a highlighted symbol): q removes all highlights
  (define-key symbol-overlay-map (kbd "q") #'symbol-overlay-remove-all))

;;; ─────────────────────────────────────────────
;;; 8. LSP — eglot (built-in)
;;; ─────────────────────────────────────────────

(use-package eglot
  :ensure nil
  :hook ((python-mode       . eglot-ensure)
         (python-ts-mode    . eglot-ensure)
         (c-mode            . eglot-ensure)
         (c++-mode          . eglot-ensure)
         (c-ts-mode         . eglot-ensure)
         (c++-ts-mode       . eglot-ensure)
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :custom
  (eglot-events-buffer-config '(:size 0))
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-extend-to-xref t))

;;; ─────────────────────────────────────────────
;;; 9. FLYMAKE (built-in)
;;; ─────────────────────────────────────────────

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom (flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; byte-compile checker fires on save — too noisy for init.el editing
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions
                           #'elisp-flymake-byte-compile t))))

;;; ─────────────────────────────────────────────
;;; 10. TREE-SITTER (built-in Emacs 29+)
;;; ─────────────────────────────────────────────

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ─────────────────────────────────────────────
;;; 11. PROJECT.EL (built-in)
;;; ─────────────────────────────────────────────

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '("pyproject.toml" "setup.py" "Cargo.toml" "go.mod"
     "package.json" "CMakeLists.txt" "Makefile"))
  (project-switch-commands
   '((project-find-file    "Find file"      ?f)
     (project-find-regexp  "Find regexp"    ?g)
     (consult-ripgrep      "Ripgrep"        ?s)
     (project-dired        "Dired"          ?d)
     (project-eshell       "Eshell"         ?e)
     (magit-project-status "Magit"          ?m))))

;;; ─────────────────────────────────────────────
;;; 12. IDE TOOLS
;;; ─────────────────────────────────────────────

(use-package avy :commands (avy-goto-char-2 avy-goto-char avy-goto-line))

(use-package magit
  :commands (magit-status magit-commit magit-push magit-pull magit-fetch
             magit-branch magit-log-current magit-diff-dwim magit-stage
             magit-unstage magit-blame magit-rebase magit-merge magit-stash
             magit-project-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  (magit-revision-insert-related-refs nil)
  (transient-default-level 5))

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (diff-hl-flydiff-mode 1))

(use-package restart-emacs :commands restart-emacs)

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package dape
  :commands (dape dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  (dape-breakpoint-global-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

;;; ─────────────────────────────────────────────
;;; 13. ORG MODE
;;; ─────────────────────────────────────────────

(defvar my/garden-dir   (expand-file-name "~/Documents/garden/"))
(defvar my/work-dir     (expand-file-name "~/Documents/garden/work/"))
(defvar my/personal-dir (expand-file-name "~/Documents/garden/personal/"))
(defvar my/work-projects-dir     (expand-file-name "~/Documents/garden/work/projects/"))
(defvar my/personal-projects-dir (expand-file-name "~/Documents/garden/personal/projects/"))

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)
  :custom
  (org-directory            "~/Documents/garden/")
  (org-log-done             'time)
  (org-log-into-drawer      t)
  (org-clock-mode-line-total 'today)
  (org-startup-indented     t)
  (org-hide-emphasis-markers t)
  (org-return-follows-link  t)
  (org-agenda-files         (append
                             (list (concat my/garden-dir   "inbox.org")
                                   (concat my/garden-dir   "journal.org")
                                   (concat my/garden-dir   "reading.org")
                                   (concat my/garden-dir   "reviews.org")
                                   (concat my/work-dir     "tasks.org")
                                   (concat my/work-dir     "projects.org")
                                   (concat my/personal-dir "tasks.org")
                                   (concat my/personal-dir "projects.org"))
                             (when (file-directory-p my/work-projects-dir)
                               (directory-files my/work-projects-dir t "\\.org$"))
                             (when (file-directory-p my/personal-projects-dir)
                               (directory-files my/personal-projects-dir t "\\.org$"))))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-clock-persist t)
  (org-enforce-todo-dependencies t)
  (org-agenda-window-setup 'current-window)
  (org-image-actual-width nil)
  (org-agenda-inhibit-startup t)          ; skip per-file startup → faster agenda
  (org-fold-catch-invisible-edits 'smart) ; never silently edit folded text
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (setq org-habit-graph-column           55
        org-habit-show-habits-only-for-today t)
  (require 'org-id)
  (org-clock-persistence-insinuate)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (shell . t) (emacs-lisp . t)))

  (setq org-refile-targets                    '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path            'file
        org-outline-path-complete-in-steps     nil
        org-refile-use-cache                   t   ; cache targets → faster refile (C-0 C-c C-w to clear)
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "NEW(N)" "LEARNING(l)" "REVIEW(r)" "APPLY(a)" "|" "MASTERED(m)" "DROPPED(x)")))

  (setq org-todo-keyword-faces
        '(("NEW"      . (:foreground "#ff6c6b" :weight bold))
          ("LEARNING" . (:foreground "#ecbe7b" :weight bold))
          ("REVIEW"   . (:foreground "#51afef" :weight bold))
          ("APPLY"    . (:foreground "#c678dd" :weight bold))
          ("MASTERED" . (:foreground "#98be65" :weight bold))
          ("DROPPED"  . (:foreground "#5B6268" :weight bold))))

  ;; Archive completed items to <file>_archive in the same dir → keeps agenda files lean
  (setq org-archive-location "%s_archive::* Archived")

  ;; Daily dashboard: agenda + next actions + items due for review (C-c o a → d)
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))
            (todo "REVIEW" ((org-agenda-overriding-header "Due for Review")))))))

  (setq org-capture-templates
        `(;; Shared
          ("i" "Inbox"   entry (file+headline ,(concat my/garden-dir "inbox.org") "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("j" "Journal" entry (file+datetree ,(concat my/garden-dir "journal.org"))
           "* %U\n** Worked On\n%?\n** Notes\n\n** Reading Insight\n\n** Momentum: /10\n"
           :empty-lines 1)
          ("v" "Review"  entry (file+headline ,(concat my/garden-dir "reviews.org") "NEW Items")
           "* REVIEW %^{What to review}\nSCHEDULED: %^t\n:PROPERTIES:\n:REVIEW_COUNT: 0\n:CREATED: %U\n:END:\n%?"
           :empty-lines 1)
          ("r" "Reading" entry (file+headline ,(concat my/garden-dir "reading.org") "Reading")
           "* %^{Book/Resource}\n:PROPERTIES:\n:START_FROM: %^{Start from page}\n:CREATED: %U\n:END:\n** Insights\n%?"
           :empty-lines 1)
          ;; Work
          ("w" "Work Task"    entry (file+headline ,(concat my/work-dir "tasks.org") "Tasks")
           "* TODO %?\n  %U\n  %a" :empty-lines 1)
          ("W" "Work Project" entry (file+headline ,(concat my/work-dir "projects.org") "Projects")
           "* NEW %^{Project} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** Goal\n%?\n\n** Tasks\n- [ ] \n\n** Outcome\n"
           :empty-lines 1)
          ;; Personal
          ("t" "Personal Task"    entry (file+headline ,(concat my/personal-dir "tasks.org") "Tasks")
           "* TODO %?\n  %U\n  %a" :empty-lines 1)
          ("p" "Personal Project" entry (file+headline ,(concat my/personal-dir "projects.org") "Projects")
           "* NEW %^{Project} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** Goal\n%?\n\n** Tasks\n- [ ] \n\n** Outcome\n"
           :empty-lines 1))))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Overdue"
           :deadline past
           :scheduled past)
          (:name "Today"
           :time-grid t
           :scheduled today
           :deadline today)
          (:name "Next Actions"
           :todo "NEXT")
          (:name "Learning"
           :todo ("NEW" "LEARNING" "REVIEW" "APPLY"))
          (:name "Work"
           :file-path "work/")
          (:name "Personal"
           :file-path "personal/")
          (:name "Inbox"
           :file-path "inbox\\.org")
          (:discard (:anything t)))))

(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view org-ql-find))

(use-package org-kanban
  :after org
  :commands (org-kanban/initialize org-kanban/shift))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory          (expand-file-name "~/Documents/garden/"))
  (org-roam-completion-everywhere t)
  (org-roam-database-connector 'sqlite-builtin)   ; Emacs 30 native sqlite
  (org-roam-db-gc-threshold most-positive-fixnum) ; fewer GC pauses during cache build
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("c" "concept" plain
      "#+FILETAGS: :concept:\n\n* What\n%?\n\n* Why\n\n* When\n\n* Code\n#+begin_src python\n\n#+end_src\n\n* Links\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("q" "question" plain
      "#+FILETAGS: :question:\n\n* Question\n%?\n\n* Answer\n\n* Related Concepts\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("P" "person" plain
      "#+FILETAGS: :person:\n#+AREA: \n\n* Who\n\n* Find Them\n- \n\n* Interactions\n** %U\n%?\n\n* Follow-up\n- [ ] \n\n* Notes\n"
      :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode)

  (defun my/roam--template (key)
    (cl-find key org-roam-capture-templates :key #'car :test #'string=))

  (defun my/roam-capture-concept ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create
                              :title (read-string "Concept: "))
                       :templates (list (my/roam--template "c"))))

  (defun my/roam-capture-question ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create
                              :title (read-string "Question topic: "))
                       :templates (list (my/roam--template "q"))))

  (defun my/roam-capture-person ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create
                              :title (read-string "Person name: "))
                       :templates (list (my/roam--template "P"))))

  (defun my/roam-log-interaction ()
    (interactive)
    (let* ((node (org-roam-node-read nil
                   (lambda (n) (member "person" (org-roam-node-tags n)))))
           (file (org-roam-node-file node)))
      (find-file file)
      (goto-char (point-min))
      (if (search-forward "* Interactions" nil t)
          (progn
            (org-end-of-subtree)
            (insert "\n** " (format-time-string "[%Y-%m-%d %a]") "\n"))
        (error "No Interactions heading in %s" file)))))

(use-package consult-org-roam
  :after org-roam
  :custom (consult-org-roam-grep-func #'consult-ripgrep)
  :config (consult-org-roam-mode 1))

(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-sync-theme    t)
  (org-roam-ui-follow        t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✦")))

(use-package org-pomodoro
  :after org
  :custom (org-pomodoro-length 25))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks      t)
  (org-appear-autosubmarkers t))

(use-package org-download
  :hook ((org-mode   . org-download-enable)
         (dired-mode . org-download-enable))
  :custom
  (org-download-method    'directory)
  (org-download-image-dir (expand-file-name "~/Documents/garden/images/"))
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_"))

(use-package org-transclusion
  :after org
  :bind (:map org-mode-map
         ("C-c t a" . org-transclusion-add)))

(use-package org-cliplink :after org)

;;; ─────────────────────────────────────────────
;;; 14. MARKDOWN
;;; ─────────────────────────────────────────────

(add-to-list 'treesit-language-source-alist
             '(json "https://github.com/tree-sitter/tree-sitter-json"))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :hook (markdown-mode . visual-line-mode)
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-urls t))

(use-package grip-mode
  :vc (:url "https://github.com/seagle0128/grip-mode" :rev :newest)
  :after markdown-mode
  :commands grip-mode)

;;; ─────────────────────────────────────────────
;;; 15. WRITING
;;; ─────────────────────────────────────────────

(use-package olivetti
  :hook ((org-mode      . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom (olivetti-body-width 90))

(use-package jinx
  :vc (:url "https://github.com/minad/jinx" :rev :newest)
  :hook ((org-mode      . jinx-mode)
         (markdown-mode . jinx-mode)
         (text-mode     . jinx-mode))
  :bind ("M-$" . jinx-correct))

;;; ─────────────────────────────────────────────
;;; 16. PYTHON / CONDA
;;; ─────────────────────────────────────────────

(use-package conda
  :custom
  (conda-anaconda-home      (expand-file-name "~/miniconda3/"))
  (conda-env-home-directory (expand-file-name "~/miniconda3/"))
  (conda-env-subdirectory   "envs")
  (conda-message-on-environment-switch nil)
  :config (conda-env-autoactivate-mode 1))

;;; ─────────────────────────────────────────────
;;; 17. COMPILE / RUN
;;; ─────────────────────────────────────────────

(defun my/python-run-current-file ()
  (interactive)
  (when (buffer-file-name)
    (compile (format "%s %s"
                     (or (executable-find "python3") "python3")
                     (shell-quote-argument buffer-file-name)))))

(defun my/cpp-compile-run-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let* ((src (buffer-file-name))
           (exe (concat (file-name-sans-extension src) ".out")))
      (compile (format "g++ -std=c++17 -Wall %s -o %s && %s"
                       (shell-quote-argument src)
                       (shell-quote-argument exe)
                       (shell-quote-argument exe))))))

;;; ─────────────────────────────────────────────
;;; 18. FORMATTERS
;;; ─────────────────────────────────────────────

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setf (alist-get 'python-mode        apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode     apheleia-mode-alist) 'ruff)
  (setf (alist-get 'c-mode             apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode           apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode          apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode        apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'js-mode            apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode    apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-ts-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'markdown-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'sh-mode            apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode       apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'rust-mode          apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode       apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'go-mode            apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode         apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'emacs-lisp-mode    apheleia-mode-alist) nil))

;;; ─────────────────────────────────────────────
;;; 19. PERSISTENCE & DEFAULTS (built-ins)
;;; ─────────────────────────────────────────────

(setq auto-save-default nil)
(auto-save-visited-mode 1)

(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 100)
  (recentf-exclude '("/elpa/" "/backups/" "/\\.git/" "/tmp/"
                     "saveplace" "recentf" "history" "custom\\.el")))

(savehist-mode 1)
(winner-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq use-short-answers     t
      scroll-conservatively 101
      scroll-margin         8
      isearch-lazy-count    t)

;; compilation-environment is defined in compile.el (loaded on first compile)
(with-eval-after-load 'compile
  (add-to-list 'compilation-environment "NO_COLOR=1"))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying    t
      version-control      t
      delete-old-versions  t
      kept-new-versions    6
      kept-old-versions    2)

(show-paren-mode 1)
(delete-selection-mode 1)
(global-so-long-mode 1)

;; Sync kill ring with macOS clipboard in terminal mode
(unless (display-graphic-p)
  (setq interprogram-cut-function
        (lambda (text)
          (with-temp-buffer
            (insert text)
            (call-process-region (point-min) (point-max) "pbcopy"))))
  (setq interprogram-paste-function
        (lambda ()
          (with-temp-buffer
            (call-process "pbpaste" nil t nil)
            (buffer-string)))))
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; ─────────────────────────────────────────────
;;; 20. WORKFLOW FUNCTIONS
;;; ─────────────────────────────────────────────

(defun my/learn-review ()
  (interactive)
  (let ((org-agenda-files (list (concat my/garden-dir "reviews.org"))))
    (org-agenda nil "a")))

(defun my/learn-reviewed ()
  (interactive)
  (let* ((count (string-to-number (or (org-entry-get nil "REVIEW_COUNT") "0")))
         (next  (1+ count))
         (days  (or (nth count '(1 3 7 14 30 60)) 90)))
    (org-entry-put nil "REVIEW_COUNT" (number-to-string next))
    (org-schedule nil (format "+%dd" days))
    (when (>= next 6) (org-todo "MASTERED"))
    (message "Review %d done. Next in %d days." next days)))

(defun my/learn-search ()
  (interactive)
  (consult-ripgrep my/garden-dir nil))

;;; ─────────────────────────────────────────────
;;; 21. AI ASSISTANT (gptel + LM Studio)
;;; ─────────────────────────────────────────────

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :commands (gptel gptel-send gptel-rewrite gptel-menu)
  :config
  (setq gptel-backend
        (gptel-make-openai "lmstudio"
          :host     "localhost:1234"
          :protocol "http"
          :models   '(gemma-4-e4b)
          :stream   t)
        gptel-model        'gemma-4-e4b
        gptel-default-mode 'org-mode))

;;; ─────────────────────────────────────────────
;;; 22. CUSTOM FILE
;;; ─────────────────────────────────────────────

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;;; init.el ends here
