;;; init.el --- Emacs 30.2 Configuration -*- lexical-binding: t; -*-
;;
;; Built-ins: use-package, eglot, flymake, project, electric-pair,
;;            treesit, which-key, pixel-scroll-precision, repeat,
;;            savehist, recentf, save-place, winner, sqlite.

;;; ─────────────────────────────────────────────
;;; 0. FRAME
;;; ─────────────────────────────────────────────

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;;; ─────────────────────────────────────────────
;;; 1. PACKAGE BOOTSTRAP
;;; ─────────────────────────────────────────────

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil)

;;; ─────────────────────────────────────────────
;;; 2. PATH (macOS)
;;; ─────────────────────────────────────────────

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer 1
  :config
  (dolist (var '("PATH" "MANPATH" "PYTHONPATH" "CONDA_PREFIX"
                 "CONDA_DEFAULT_ENV" "GOPATH" "CARGO_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; ─────────────────────────────────────────────
;;; 3. UI
;;; ─────────────────────────────────────────────

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(pixel-scroll-precision-mode 1)   ; built-in Emacs 29+

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
;;; 4. EVIL + LEADER
;;; ─────────────────────────────────────────────

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll  t
        evil-undo-system      'undo-fu
        evil-search-module    'evil-search)
  :config
  (evil-mode 1)

  (evil-define-key 'normal 'global
    (kbd "j")   #'evil-next-visual-line
    (kbd "k")   #'evil-previous-visual-line
    (kbd "gj")  #'evil-next-line
    (kbd "gk")  #'evil-previous-line
    (kbd "Q")   #'evil-execute-last-recorded-macro
    (kbd "U")   #'evil-redo
    (kbd "Y")   (kbd "y$")
    (kbd "g h") #'evil-beginning-of-line
    (kbd "g l") #'evil-end-of-line
    ;; search + center
    (kbd "n")   (lambda () (interactive) (evil-ex-search-next)            (evil-scroll-line-to-center nil))
    (kbd "N")   (lambda () (interactive) (evil-ex-search-previous)        (evil-scroll-line-to-center nil))
    (kbd "*")   (lambda () (interactive) (evil-ex-search-word-forward)    (evil-scroll-line-to-center nil))
    (kbd "#")   (lambda () (interactive) (evil-ex-search-word-backward)   (evil-scroll-line-to-center nil))
    ;; xref/eldoc — work globally (elisp, eglot, etags)
    (kbd "gd") #'xref-find-definitions
    (kbd "gD") #'xref-find-definitions-other-window
    (kbd "gr") #'xref-find-references
    (kbd "K")  #'eldoc-doc-buffer)

  (evil-define-key 'visual 'global
    (kbd ">") (lambda () (interactive) (evil-shift-right (region-beginning) (region-end)) (evil-visual-restore))
    (kbd "<") (lambda () (interactive) (evil-shift-left  (region-beginning) (region-end)) (evil-visual-restore))))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-mode-list '(magit dired org helpful xref))
  :config
  (evil-collection-init)
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
      (kbd "R") #'dired-do-rename)))

(use-package evil-commentary :after evil :config (evil-commentary-mode 1))
(use-package evil-surround   :after evil :config (global-evil-surround-mode 1))
(use-package evil-matchit    :after evil :config (global-evil-matchit-mode 1))

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
    "SPC" '(execute-extended-command           :wk "M-x")
    "TAB" '(evil-switch-to-windows-last-buffer :wk "last buffer")
    ";"   '(evil-commentary-line               :wk "comment line")

    "f f" '(find-file           :wk "find file")
    "f r" '(consult-recent-file :wk "recent files")

    "b b" '(consult-buffer      :wk "switch buffer")
    "b k" '(kill-current-buffer :wk "kill buffer")

    "w h" '(evil-window-left        :wk "←")
    "w j" '(evil-window-down        :wk "↓")
    "w k" '(evil-window-up          :wk "↑")
    "w l" '(evil-window-right       :wk "→")
    "w s" '(split-window-below      :wk "split h")
    "w v" '(split-window-right      :wk "split v")
    "w d" '(delete-window           :wk "close")
    "w o" '(delete-other-windows    :wk "only")
    "w =" '(balance-windows         :wk "balance")
    "w u" '(winner-undo             :wk "undo layout")
    "w f" '(toggle-frame-fullscreen :wk "fullscreen")

    "s s" '(consult-line    :wk "search buffer")
    "s r" '(consult-ripgrep :wk "search project")
    "s i" '(consult-imenu   :wk "jump to symbol")

    "c f" '(apheleia-format-buffer          :wk "format")
    "c p" '(my/python-run-current-file      :wk "run python")
    "c c" '(my/cpp-compile-run-current-file :wk "compile c++")

    "l r" '(eglot-rename              :wk "rename")
    "l a" '(eglot-code-actions        :wk "code action")
    "l i" '(eglot-find-implementation :wk "implementation")
    "l d" '(xref-find-references      :wk "references")
    "l f" '(eglot-format-buffer       :wk "format")

    "e l" '(consult-flymake         :wk "list errors")
    "e n" '(flymake-goto-next-error :wk "next error")
    "e p" '(flymake-goto-prev-error :wk "prev error")

    "g g" '(magit-status      :wk "magit")
    "g c" '(magit-commit      :wk "commit")
    "g p" '(magit-push        :wk "push")
    "g i" '(magit-pull        :wk "pull")
    "g f" '(magit-fetch       :wk "fetch")
    "g l" '(magit-log-current :wk "log")
    "g d" '(magit-diff-dwim   :wk "diff")
    "g b" '(magit-branch      :wk "branch")
    "g a" '(magit-blame       :wk "blame")
    "g t" '(magit-stash       :wk "stash")

    "p p" '(project-switch-project   :wk "switch")
    "p f" '(project-find-file        :wk "find file")
    "p b" '(project-switch-to-buffer :wk "buffer")
    "p k" '(project-kill-buffers     :wk "kill")
    "p s" '(project-eshell           :wk "eshell")
    "p c" '(project-compile          :wk "compile")

    "o a" '(org-agenda           :wk "agenda")
    "o c" '(org-capture          :wk "capture")
    "o r" '(org-roam-node-find   :wk "roam find")
    "o i" '(org-roam-node-insert :wk "roam insert")
    "o s" '(consult-org-roam-search :wk "roam search")
    "o d" '(deft                 :wk "deft")
    "o u" '(org-roam-ui-open     :wk "roam graph")
    "o l" '(org-cliplink         :wk "paste url")
    "o t" '(org-transclusion-mode :wk "transclusion")
    "o y" '(org-download-yank    :wk "paste image")

    "n i" '((lambda () (interactive) (org-capture nil "i")) :wk "inbox")
    "n j" '((lambda () (interactive) (org-capture nil "j")) :wk "journal")
    "n v" '((lambda () (interactive) (org-capture nil "v")) :wk "review")
    "n r" '((lambda () (interactive) (org-capture nil "r")) :wk "reading")
    "n w" '((lambda () (interactive) (org-capture nil "w")) :wk "work task")
    "n W" '((lambda () (interactive) (org-capture nil "W")) :wk "work project")
    "n t" '((lambda () (interactive) (org-capture nil "t")) :wk "personal task")
    "n p" '((lambda () (interactive) (org-capture nil "p")) :wk "personal project")
    "n k" '(my/roam-capture-concept  :wk "concept node")
    "n q" '(my/roam-capture-question :wk "question node")
    "n a" '(my/learn-review          :wk "review agenda")
    "n d" '(my/learn-reviewed        :wk "mark reviewed")
    "n s" '(my/learn-search          :wk "search")

    "a c" '(gptel         :wk "ai chat")
    "a s" '(gptel-send    :wk "send")
    "a r" '(gptel-rewrite :wk "rewrite")
    "a m" '(gptel-menu    :wk "menu")

    "m a" '(conda-env-activate   :wk "activate env")
    "m d" '(conda-env-deactivate :wk "deactivate env")

    "j j" '(avy-goto-char-2 :wk "jump")

    "h k" '(helpful-key      :wk "key")
    "h f" '(helpful-callable :wk "function")
    "h v" '(helpful-variable :wk "variable")
    "h ." '(helpful-at-point :wk "at point")

    "q q" '(save-buffers-kill-terminal :wk "quit")
    "q r" '(restart-emacs              :wk "restart"))

  (general-define-key
    :states '(normal visual)
    "]h" '(diff-hl-next-hunk      :wk "next hunk")
    "[h" '(diff-hl-previous-hunk  :wk "prev hunk")
    "]e" '(flymake-goto-next-error :wk "next error")
    "[e" '(flymake-goto-prev-error :wk "prev error")))

;; which-key — built-in since Emacs 30
(use-package which-key
  :ensure nil
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode 1))

;; repeat-mode — built-in, repeat window/error navigation without prefix
(repeat-mode 1)

;;; ─────────────────────────────────────────────
;;; 5. COMPLETION (vertico + corfu)
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

(use-package marginalia :init (marginalia-mode 1))

(use-package consult
  :bind ("C-s" . consult-line)
  :custom (consult-preview-key "M-.")
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
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))))

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
  (doom-modeline-major-mode-icon t))

(use-package nerd-icons :defer t)

;;; ─────────────────────────────────────────────
;;; 7. EDITING
;;; ─────────────────────────────────────────────

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)

(add-hook 'prog-mode-hook #'subword-mode)

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

;;; ─────────────────────────────────────────────
;;; 8. LSP — eglot (built-in)
;;; ─────────────────────────────────────────────

(use-package eglot
  :ensure nil
  :hook ((python-mode    . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (c-mode         . eglot-ensure)
         (c++-mode       . eglot-ensure)
         (c-ts-mode      . eglot-ensure)
         (c++-ts-mode    . eglot-ensure))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)
  (eglot-extend-to-xref t)
  :config
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "gi") #'eglot-find-implementation))))

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
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ─────────────────────────────────────────────
;;; 11. PROJECT.EL (built-in)
;;; ─────────────────────────────────────────────

(use-package project
  :ensure nil
  :custom
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
(use-package rg  :commands (rg rg-project rg-dwim))

(use-package magit
  :commands (magit-status magit-commit magit-push magit-pull magit-fetch
             magit-branch magit-log-current magit-diff-dwim magit-stage
             magit-unstage magit-blame magit-rebase magit-merge magit-stash
             magit-project-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all))

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (diff-hl-flydiff-mode 1))

(use-package restart-emacs :commands restart-emacs)

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
                             (directory-files my/work-projects-dir t "\\.org$")
                             (directory-files my/personal-projects-dir t "\\.org$")))
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (require 'org-id)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (shell . t) (emacs-lisp . t)))

  (setq org-refile-targets         '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

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
  :config (org-super-agenda-mode))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory          (expand-file-name "~/Documents/garden/"))
  (org-roam-completion-everywhere t)
  (org-roam-database-connector 'sqlite-builtin)   ; Emacs 30 native sqlite
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
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode)

  (defun my/roam-capture-concept ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create
                              :title (read-string "Concept: "))
                       :templates (list (nth 1 org-roam-capture-templates))))

  (defun my/roam-capture-question ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create
                              :title (read-string "Question topic: "))
                       :templates (list (nth 2 org-roam-capture-templates)))))

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
  (org-modern-star '("◉" "○" "◈" "◇" "✦"))
  (org-modern-hide-stars nil))

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
         ("C-c t a" . org-transclusion-add)
         ("C-c t t" . org-transclusion-mode)))

(use-package org-cliplink
  :after org
  :bind (:map org-mode-map ("C-c l" . org-cliplink)))

;;; ─────────────────────────────────────────────
;;; 14. MARKDOWN
;;; ─────────────────────────────────────────────

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-command "pandoc"))

(use-package pandoc-mode :hook (markdown-mode . pandoc-mode))

(use-package deft
  :commands deft
  :custom
  (deft-directory   (expand-file-name "~/Documents/garden/"))
  (deft-extensions  '("org" "md" "txt"))
  (deft-recursive   t)
  (deft-use-filename-as-title nil)
  (deft-use-filter-string-for-filename t)
  (deft-strip-summary-regexp
   (concat "\\(" "[\n\t]"
           "\\|^#\\+[[:alpha:]_]+:.*$"
           "\\|^:PROPERTIES:.*"
           "\\|^:END:.*"
           "\\|^\\* " "\\)"))
  :config
  (defun my/deft-new-note-via-roam ()
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
;;; 15. WRITING
;;; ─────────────────────────────────────────────

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :custom (olivetti-body-width 90))

;;; ─────────────────────────────────────────────
;;; 16. PYTHON / CONDA
;;; ─────────────────────────────────────────────

(use-package conda
  :custom
  (conda-anaconda-home      (expand-file-name "~/miniconda3/"))
  (conda-env-home-directory (expand-file-name "~/miniconda3/"))
  (conda-env-subdirectory   "envs")
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
      (compile (format "g++ -std=c++17 -Wall %s -o %s && ./%s"
                       (shell-quote-argument src)
                       (shell-quote-argument exe)
                       (shell-quote-argument exe))))))

;;; ─────────────────────────────────────────────
;;; 18. FORMATTERS
;;; ─────────────────────────────────────────────

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setf (alist-get 'python-mode     apheleia-mode-alist) 'ruff)
  (setf (alist-get 'python-ts-mode  apheleia-mode-alist) 'ruff)
  (setf (alist-get 'c-mode          apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode        apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode       apheleia-mode-alist) 'clang-format)
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
;;; 19. PERSISTENCE & DEFAULTS (built-ins)
;;; ─────────────────────────────────────────────

(setq auto-save-default  t
      auto-save-timeout  20
      auto-save-interval 200)

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
      scroll-margin         3)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying    t
      version-control      t
      delete-old-versions  t
      kept-new-versions    6
      kept-old-versions    2)

(show-paren-mode 1)
(delete-selection-mode 1)
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
  :ensure nil
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
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
