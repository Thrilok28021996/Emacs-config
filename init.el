;;; Package --- Summary -*- lexical-binding: t; -*-
;;; Commentary

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Tell straight.el to use use-package
(straight-use-package 'use-package)
;; Ensure use-package is installed before other packages that depend on it
(setq straight-use-package-by-default t)

;; --- Configure use-package ---
;; This ensures use-package is set up to work with straight.el
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; Ensures packages are installed if missing
(setq package-enable-at-startup nil)


;; Ensure Git is found by Emacs, especially on macOS or when Emacs is GUI
(use-package exec-path-from-shell
  ;; :straight t ; This is now the default due to straight-use-package-by-default
  :if (memq window-system '(mac ns x pgtk))
  :config
  (exec-path-from-shell-initialize))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; --- Evil Mode (Vim Emulation) ---
(use-package evil
  :straight t ; Explicitly tell straight.el to manage this
  :init ;; Code to run before the package is loaded
  (setq evil-want-integration t) ;; This is important for good integration with Emacs
  (setq evil-want-keybinding nil) ;; We will set our own initial state if needed
  (setq evil-undo-system 'undo-redo) ;; Use Emacs's undo system for consistency
  :config ;; Code to run after the package is loaded
  (evil-mode 1) ; Enable Evil mode globally
  ;; Set the initial state to normal mode for most buffers
  (setq evil-default-state 'normal)
  ;; But use emacs state in specific modes where Vim bindings might be awkward or less useful
  (evil-set-initial-state 'messages-buffer-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs) ; If you use a dashboard
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs) ; For *scratch*, *Messages*, etc.
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs) ; Personal preference, some like Vim keys in Dired via evil-collection
  (evil-set-initial-state 'term-mode 'emacs) ; For terminal emulators inside Emacs
  (evil-set-initial-state 'eshell-mode 'emacs) ; For Eshell
  (evil-set-initial-state 'vterm-mode 'emacs) ; For vterm, if you use it
  (evil-set-initial-state 'org-agenda-mode 'emacs) ; Often better with Emacs keys for quick agenda navigation
  (evil-set-initial-state 'magit-mode 'emacs) ; Magit has excellent default bindings
  (evil-set-initial-state 'magit-popup-mode 'emacs)
  (evil-set-initial-state 'lsp-lens-mode 'emacs) ; For LSP code lenses, mouse interaction or Emacs keys are fine
  (evil-set-initial-state 'custom-mode 'emacs) ; For customize interface
  )

;; Optional but highly recommended: evil-collection integrates Evil better with various Emacs packages
(use-package evil-collection
  :straight t
  :after evil ; Ensure evil is loaded first
  :config
  (evil-collection-init))

;; Enhanced Evil Leader Keybindings
(use-package evil-leader
  :straight t
  :after evil
  :config
  (global-evil-leader-mode)
  ;; Set Space as the leader key
  (evil-leader/set-leader "<SPC>")
  
  ;; File operations
  (evil-leader/set-key
    "f f" 'find-file
    "f r" 'recentf-open-files
    "f s" 'save-buffer
    "f S" 'save-some-buffers
    "f d" 'dired
    "f D" 'delete-file
    "f R" 'rename-file
    "f c" 'copy-file
    
    ;; Buffer operations
    "b b" 'switch-to-buffer
    "b k" 'kill-buffer
    "b K" 'kill-buffer-and-window
    "b n" 'next-buffer
    "b p" 'previous-buffer
    "b r" 'revert-buffer
    "b s" 'scratch-buffer
    "b l" 'list-buffers
    
    ;; Window operations
    "w s" 'split-window-below
    "w v" 'split-window-right
    "w d" 'delete-window
    "w o" 'delete-other-windows
    "w w" 'other-window
    "w h" 'windmove-left
    "w j" 'windmove-down
    "w k" 'windmove-up
    "w l" 'windmove-right
    "w =" 'balance-windows
    "w m" 'maximize-window
    
    ;; Project operations (Projectile)
    "p f" 'projectile-find-file
    "p p" 'projectile-switch-project
    "p b" 'projectile-switch-to-buffer
    "p d" 'projectile-find-dir
    "p r" 'projectile-recentf
    "p g" 'projectile-grep
    "p s" 'projectile-ag  ; or projectile-ripgrep if you have it
    "p k" 'projectile-kill-buffers
    "p c" 'projectile-compile-project
    "p t" 'projectile-test-project
    
    ;; Compile operations
    "c c" 'compile
    "c r" 'recompile
    "c k" 'kill-compilation
    "c n" 'next-error
    "c p" 'previous-error
    "c e" 'first-error
    
    ;; Git operations (Magit)
    "g s" 'magit-status
    "g b" 'magit-blame
    "g l" 'magit-log
    "g d" 'magit-diff
    "g c" 'magit-commit
    "g p" 'magit-push
    "g P" 'magit-pull
    "g f" 'magit-fetch
    
    ;; LSP operations
    "l r" 'lsp-rename
    "l d" 'lsp-find-definition
    "l D" 'lsp-find-declaration
    "l i" 'lsp-find-implementation
    "l t" 'lsp-find-type-definition
    "l r" 'lsp-find-references
    "l s" 'lsp-signature-help
    "l h" 'lsp-hover
    "l a" 'lsp-execute-code-action
    "l f" 'lsp-format-buffer
    "l F" 'lsp-format-region
    "l w r" 'lsp-workspace-restart
    "l w s" 'lsp-workspace-shutdown
    
    ;; Error/diagnostics navigation
    "e n" 'flycheck-next-error
    "e p" 'flycheck-previous-error
    "e l" 'flycheck-list-errors
    "e c" 'flycheck-clear
    "e v" 'flycheck-verify-setup
    
    ;; Search operations
    "s s" 'swiper  ; if you add swiper
    "s p" 'projectile-grep
    "s r" 'query-replace
    "s R" 'query-replace-regexp
    
    ;; Org mode operations
    "o a" 'org-agenda
    "o c" 'org-capture
    "o l" 'org-store-link
    "o i" 'org-insert-link
    "o t" 'org-todo
    "o s" 'org-schedule
    "o d" 'org-deadline
    "o r" 'org-refile
    "o A" 'org-archive-subtree
    
    ;; Note-taking operations
    "n n" 'my/quick-note
    "n d" 'my/daily-note
    "n w" 'my/weekly-review
    "n s" 'my/search-notes
    "n b" 'my/note-backlinks
    "n f" 'deft
    "n j" 'org-journal-new-entry
    "n J" 'org-journal-open-current-journal-file
    
    ;; Org-roam operations
    "n r f" 'org-roam-node-find
    "n r i" 'org-roam-node-insert
    "n r c" 'org-roam-capture
    "n r b" 'org-roam-buffer-toggle
    "n r g" 'org-roam-graph
    "n r t" 'org-roam-tag-add
    "n r T" 'org-roam-tag-remove
    "n r r" 'org-roam-refile
    "n r s" 'org-roam-db-sync
    
    ;; Conda environment operations
    "v a" 'conda-env-activate
    "v d" 'conda-env-deactivate
    "v l" 'conda-env-list
    
    ;; Evaluation (useful for Emacs Lisp development)
    "e e" 'eval-expression
    "e b" 'eval-buffer
    "e r" 'eval-region
    "e l" 'eval-last-sexp
    
    ;; Help and documentation
    "h f" 'describe-function
    "h v" 'describe-variable
    "h k" 'describe-key
    "h m" 'describe-mode
    "h p" 'describe-package
    "h b" 'describe-bindings
    "h a" 'apropos
    "h i" 'info
    
    ;; Toggles
    "t l" 'display-line-numbers-mode
    "t r" (lambda () (interactive) 
            (if (eq display-line-numbers-type 'relative)
                (setq display-line-numbers-type t)
              (setq display-line-numbers-type 'relative)))
    "t w" 'whitespace-mode
    "t h" 'global-hl-line-mode
    "t f" 'auto-fill-mode
    
    ;; System operations
    "q q" 'save-buffers-kill-terminal
    "q r" 'restart-emacs  ; if you have restart-emacs package
    "q k" 'kill-emacs
    
    ;; Comments
    ";" 'comment-line
    "/ r" 'comment-region
    "/ u" 'uncomment-region
    )
  
  ;; Mode-specific keybindings
  (evil-leader/set-key-for-mode 'python-mode
    "r r" 'python-shell-send-region
    "r b" 'python-shell-send-buffer
    "r f" 'python-shell-send-file
    "r s" 'python-shell-switch-to-shell
    "r i" 'run-python
    "t t" 'python-pytest  ; if you add pytest package
    "t f" 'python-pytest-file
    "t p" 'python-pytest-popup)
  
  (evil-leader/set-key-for-mode 'org-mode
    "i l" 'org-insert-link
    "i t" 'org-insert-todo-heading
    "i h" 'org-insert-heading
    "i s" 'org-insert-subheading
    "t t" 'org-todo
    "t s" 'org-schedule
    "t d" 'org-deadline
    "e e" 'org-export-dispatch
    "c t" 'org-ctrl-c-ctrl-c
    "r" 'org-refile
    "A" 'org-archive-subtree
    "p" 'org-set-property
    "P" 'org-delete-property
    ":" 'org-set-tags-command)
  
  (evil-leader/set-key-for-mode 'deft-mode
    "n" 'deft-new-file
    "r" 'deft-rename-file
    "d" 'deft-delete-file
    "g" 'deft-refresh
    "s" 'deft-filter
    "c" 'deft-filter-clear)
    
  (evil-leader/set-key-for-mode 'org-roam-mode
    "i" 'org-roam-node-insert
    "f" 'org-roam-node-find
    "b" 'org-roam-buffer-toggle
    "t" 'org-roam-tag-add
    "T" 'org-roam-tag-remove)
    
  (evil-leader/set-key-for-mode 'dired-mode
    "m" 'dired-mark
    "u" 'dired-unmark
    "d" 'dired-flag-file-deletion
    "x" 'dired-do-flagged-delete
    "c" 'dired-do-copy
    "r" 'dired-do-rename
    "g" 'revert-buffer)
)

;; Additional Evil keybindings outside of leader key
(use-package evil
  :config
  ;; Better window navigation in normal state
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  
  ;; Better search highlighting
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
  
  ;; Quick save
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  
  ;; Better increment/decrement
  (define-key evil-normal-state-map (kbd "g +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "g -") 'evil-numbers/dec-at-pt)
  
  ;; Visual line mode navigation
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  
  ;; Insert mode mappings
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
  )

;; Optional: Add evil-numbers for increment/decrement functionality
(use-package evil-numbers
  :straight t
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

;; Optional: Add evil-surround for text object manipulation
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Optional: Add evil-commentary for easy commenting
(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode 1))

;; Optional: Better undo with undo-tree
(use-package undo-tree
  :straight t
  :after evil
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq evil-undo-system 'undo-tree)
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo))


;; --- Performance Tweaks & Disable Backups/Auto-Save ---
(setq make-backup-files nil)          ; Stop creating ~ backup files
(setq auto-save-default nil)          ; Stop creating #auto-save# files
(setq create-lockfiles nil)           ; Stop creating .#lock files (use with caution, can be risky in some NFS situations)
(setq auto-save-list-file-prefix nil) ; Stop auto-saving session data

;; More performance tweaks
(setq gc-cons-threshold (* 100 1024 1024)) ; 100 MB garbage collection threshold (start high, adjust if needed)
(setq read-process-output-max (* 1024 1024)) ; 1 MB for reading process output
(setq inhibit-startup-screen t)            ; Disable the startup screen for faster loading
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; --- Aesthetics: Theme and Font ---
(use-package doom-themes
  :config
  ;; Configure the theme (e.g., doom-one, doom-molokai, doom-dracula, etc.)
  (load-theme 'doom-one t)
  ;; Enable custom modeline theme
  (doom-themes-visual-bell-config)
  ;; Corrects (mouse) hover backgrounds in completions and tooltips
  ;; (doom-themes-neotree-config) ; If you use neotree
  (doom-themes-org-config)   ; Extra configuration for org-mode
  )

;; Set a nice font (adjust font name and size to your preference)
;; Make sure the font is installed on your system
(set-face-attribute 'default nil
                    :font "Fira Code" ; Or "JetBrains Mono", "Source Code Pro", etc.
                    :height 160)     ; Corresponds to roughly 11pt, adjust as needed

;; Ligature support for fonts like Fira Code (optional)
(use-package ligature
  :config
  ;; Enable ligatures in all modes (or specify modes)
  (ligature-set-ligatures 't '(;; List of ligatures to enable
                               "->" "<=" ">=" "=>" "==" "!=" "->" "<-"
                               "::" "->" "<-" "=>" "->" "<-" "->" "<-"
                               "->" "<-" "->" "<-" "->" "<-" "->" "<-"
                               "www" "**" "***" "&&" "||" "===" "!=="
                               "##" "###" "####" "/*" "*/" "//" "/="
                               ":=" "<>" "<<<" ">>>" "++" "--" "~~"
                               "::" ";;" "..." ":::" "::" "->" "<-"
                               "<=>" "<|>" "<->" "---" "~~~" "SPC"))
  (global-ligature-mode t))


;; --- Python Programming Setup ---

;; General programming helpers
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map) ; Super-p (Win-p or Cmd-p)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :defer 2 ; Defer loading slightly for faster startup
  :diminish company-mode
  :after lsp-mode ; Ensure lsp-mode is loaded if company-lsp is used
  :config
  (setq company-idle-delay 0.2)             ; How long to wait before showing completions
  (setq company-minimum-prefix-length 2)    ; Show completions after 2 characters
  (setq company-selection-wrap-around t)    ; Wrap around completion candidates
  (global-company-mode t)
  ;; Optional: Use company-box for a more graphical completion UI
  ;; (use-package company-box
  ;;   :hook (company-mode . company-box-mode))
  )

(use-package flycheck
  :straight t
  :defer 2
  :diminish flycheck-mode
  :config
  (global-flycheck-mode t)
  ;; Flycheck can also use ruff. This is useful if LSP isn't active
  ;; or if you want redundant checking.
  ;; Flycheck usually has built-in support for ruff if ruff is in your PATH.
  ;; We'll select it in python-mode-hook later if LSP isn't the primary source of diagnostics.
)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  ;; Ensure lsp-deferred is called for python-mode
  :hook ((python-mode . lsp-deferred)
         ;; Add hook for lsp-mode itself to enable format-on-save
         (lsp-mode . (lambda ()
                       (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :init
  (setq lsp-keymap-prefix "C-c l") ; Or your preferred prefix "s-l" etc.
  ;; Optional: Disable file watchers if you experience high CPU usage on large projects
  ;; (setq lsp-enable-file-watchers nil)
  :config
  (setq lsp-eldoc-render-all nil) ; Show less verbose eldoc messages
  (setq lsp-idle-delay 0.5)     ; Delay before sending requests to server
  (setq lsp-log-io nil)         ; Set to t for debugging LSP communication

  ;; --- Python LSP Server Configuration ---
  ;; We recommend using ruff-lsp for linting and formatting,
  ;; and pyright for comprehensive type checking and other language features.
  ;;
(use-package lsp-pyright
  :if (executable-find "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config
  (setq lsp-pyright-executable (executable-find "pyright")))
  ;; Installation (in your Python environment):
  ;;   pip install ruff ruff-lsp
  ;;   pip install pyright  (or npm install -g pyright)

  ;; lsp-mode will typically auto-detect ruff-lsp and pyright if they are installed
  ;; and available in the current environment's PATH.
  ;; You can explicitly set server priorities if you have multiple Python LSPs installed
  ;; and want to control which one lsp-mode prefers for certain capabilities.
  (setq lsp-clients-python-server-priority '(ruff-lsp pyright pylsp))

  ;; Note: The (add-hook 'before-save-hook #'lsp-format-buffer nil t) in the
  ;; :hook (lsp-mode ...) section above will enable format-on-save.
  ;; If ruff-lsp is active and provides formatting, it will be used.
)

;; --- Conda Integration ---
;; (Keep your existing conda.el configuration)
(use-package conda
  :straight t
  :config
  (setq conda-anaconda-home (expand-file-name "~/miniconda3")) ; ADJUST THIS PATH
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  (add-hook 'python-mode-hook 'my-python-conda-activate-hook)
  (add-hook 'projectile-after-switch-project-hook 'my-python-conda-activate-hook))

;; (Keep your my-python-conda-activate-hook function)
(defun my-python-conda-activate-hook ()
  "Activate conda environment for the current Python project if not already."
  (when (and (derived-mode-p 'python-mode) (projectile-project-p))
    (let ((project-root (projectile-project-root)))
      (when project-root
        (unless (conda-env-active-p)
          (let ((env-name (conda-env-from-file project-root)))
            (if env-name
                (conda-env-activate env-name)
              (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
                (when (conda-env-exists-p project-name)
                  (conda-env-activate project-name))))))))))


;; --- Python Mode Specific Configurations ---
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)      ; Use spaces, not tabs
            (setq python-indent-offset 4)    ; Standard 4 spaces (Ruff will enforce this)
            (setq tab-width 4)

            ;; Flycheck configuration for Python:
            ;; If LSP (e.g., ruff-lsp) is providing diagnostics, Flycheck's own linting
            ;; might be redundant. However, it can serve as a backup or for checkers
            ;; not covered by your LSP.
            ;; If ruff is installed, try to make it the default checker for Flycheck in Python buffers.
            (when (executable-find "ruff")
              (flycheck-select-checker 'python-ruff)
              ;; If you want to chain checkers, e.g., ruff then mypy (if mypy is not via LSP)
              ;; (flycheck-add-next-checker 'python-ruff 'python-mypy 'append)
              )
            ;; If ruff (via LSP or another formatter) handles formatting on save,
            ;; ensure Emacs's electric indentation doesn't interfere too much.
            ;; The formatting on save will be the source of truth.
            ))

;; --- Note-Taking with Org Mode ---
;; Deft for quick note searching and creation
(use-package deft
  :straight t
  :config
  (setq deft-directory "~/notes")
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-file-naming-rules '((noslash . "-")
                                 (nospace . "-")
                                 (case-fn . downcase)))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^#\\* " ;; org-mode headings
                "\\)"))
  ;; Create notes directory if it doesn't exist
  (unless (file-directory-p deft-directory)
    (make-directory deft-directory t)))

;; Org-roam for interconnected notes (Zettelkasten method)
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam-notes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: \n\n")
      :unnarrowed t)
     ("r" "reference" plain
      "* Source\n\n%?\n\n* Summary\n\n* Notes\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :reference:\n\n")
      :unnarrowed t)
     ("p" "project" plain
      "* Goals\n\n%?\n\n* Tasks\n\n* Resources\n\n* Notes\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :project:\n\n")
      :unnarrowed t)
     ("m" "meeting" plain
      "* Attendees\n\n%?\n\n* Agenda\n\n* Discussion\n\n* Action Items\n\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n#+filetags: :meeting:\n\n")
      :unnarrowed t)))
  :config
  (org-roam-setup)
  ;; Create roam directory if it doesn't exist
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  
  ;; Build the cache on first load
  (org-roam-db-autosync-mode))

;; Org-roam-server for web-based note browsing (optional)
;; (use-package org-roam-server
;;   :straight t
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20))

;; Org-journal for daily journaling
(use-package org-journal
  :straight t
  :defer t
  :custom
  (org-journal-dir "~/journal/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-type 'daily)
  (org-journal-find-file 'find-file)
  (org-journal-time-format "")
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"")
  :config
  ;; Create journal directory if it doesn't exist
  (unless (file-directory-p org-journal-dir)
    (make-directory org-journal-dir t)))

;; Org-download for easier image handling in notes
(use-package org-download
  :straight t
  :after org
  :config
  (setq org-download-method 'directory)
  (setq org-download-image-dir "~/org/images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 600)
  ;; Create images directory if it doesn't exist
  (unless (file-directory-p org-download-image-dir)
    (make-directory org-download-image-dir t)))

;; Enhanced org capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a\n  :CREATED: %U")
        
        ("n" "Note" entry (file+headline "~/org/inbox.org" "Notes")
         "* %?\n  %i\n  %a\n  :CREATED: %U")
        
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %<%H:%M> %?\n%i")
        
        ("m" "Meeting" entry (file+headline "~/org/meetings.org" "Meetings")
         "* %? :meeting:\n  :CREATED: %U\n  :ATTENDEES: \n  :AGENDA: \n  :NOTES: \n  :ACTION_ITEMS: ")
        
        ("r" "Reading" entry (file+headline "~/org/reading.org" "Reading List")
         "* TODO Read %?\n  :CREATED: %U\n  :SOURCE: %a\n  :NOTES: ")
        
        ("p" "Project" entry (file+headline "~/org/projects.org" "Projects")
         "* %? :project:\n  :CREATED: %U\n  :DESCRIPTION: \n  :GOALS: \n  :TASKS: \n  :RESOURCES: ")
        
        ("i" "Idea" entry (file+headline "~/org/ideas.org" "Ideas")
         "* %?\n  :CREATED: %U\n  :CONTEXT: %a\n  :DESCRIPTION: ")
        
        ("l" "Link" entry (file+headline "~/org/links.org" "Links")
         "* %? :link:\n  :CREATED: %U\n  :URL: %c\n  :DESCRIPTION: ")
        
        ("c" "Code Snippet" entry (file+headline "~/org/code.org" "Code Snippets")
         "* %? :code:\n  :CREATED: %U\n  :LANGUAGE: \n  :CONTEXT: %a\n  #+BEGIN_SRC \n%i\n  #+END_SRC\n  :NOTES: ")))

;; Quick note-taking functions
(defun my/quick-note ()
  "Quickly create a timestamped note."
  (interactive)
  (let ((note-file (format "~/notes/quick-%s.org" 
                          (format-time-string "%Y%m%d-%H%M%S"))))
    (find-file note-file)
    (insert (format "#+title: Quick Note - %s\n#+date: %s\n\n"
                   (format-time-string "%Y-%m-%d %H:%M")
                   (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (goto-char (point-max))))

(defun my/daily-note ()
  "Open or create today's daily note."
  (interactive)
  (let ((daily-file (format "~/notes/daily-%s.org" 
                           (format-time-string "%Y-%m-%d"))))
    (find-file daily-file)
    (when (= (buffer-size) 0)
      (insert (format "#+title: Daily Note - %s\n#+date: %s\n\n* Tasks\n\n* Notes\n\n* Journal\n\n"
                     (format-time-string "%Y-%m-%d")
                     (format-time-string "[%Y-%m-%d %a]")))
      (goto-char (point-max)))))

(defun my/weekly-review ()
  "Create a weekly review template."
  (interactive)
  (let ((week-file (format "~/notes/weekly-%s.org" 
                          (format-time-string "%Y-W%U"))))
    (find-file week-file)
    (when (= (buffer-size) 0)
      (insert (format "#+title: Weekly Review - Week %s\n#+date: %s\n\n* Accomplishments\n\n* Challenges\n\n* Lessons Learned\n\n* Next Week Goals\n\n* Action Items\n\n"
                     (format-time-string "%U")
                     (format-time-string "[%Y-%m-%d %a]")))
      (goto-char (point-max)))))

(defun my/search-notes ()
  "Search through all notes using grep."
  (interactive)
  (let ((search-term (read-string "Search notes for: ")))
    (grep (format "grep -r -n --include=\"*.org\" --include=\"*.md\" --include=\"*.txt\" \"%s\" ~/notes/ ~/org/ ~/roam-notes/ ~/journal/" search-term))))

(defun my/note-backlinks ()
  "Find all notes that link to the current note."
  (interactive)
  (if (buffer-file-name)
      (let ((filename (file-name-base (buffer-file-name))))
        (grep (format "grep -r -n --include=\"*.org\" \"\\[\\[.*%s\" ~/notes/ ~/org/ ~/roam-notes/" filename)))
    (message "Current buffer is not a file")))

;; Org-mode enhancements for better note-taking
(use-package org
  :config
  (setq org-ellipsis " ▼") ; Nicer looking ellipsis for folded headlines
  (setq org-log-done 'time) ; Record when a TODO item is marked DONE
  (setq org-hide-emphasis-markers t) ; Hide the *, /, _, etc. markers for emphasis
  ;; Better org-mode defaults for note-taking
  (setq org-startup-indented t)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  (setq org-return-follows-link t)
  (setq org-mouse-1-follows-link t)
  (setq org-link-descriptive t)
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-whole-heading-line t)
  (setq org-image-actual-width '(600))
  
  ;; Better tag alignment
  (setq org-tags-column -80)
  (setq org-auto-align-tags t)
  
  ;; Archive settings
  (setq org-archive-location "~/org/archive/%s_archive::")
  
  ;; Refile settings
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                            ("~/org/projects.org" :maxlevel . 2)
                            ("~/org/someday.org" :level . 1)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))

;; Auto-save org files
(add-hook 'org-mode-hook 
          (lambda ()
            (auto-save-mode 1)
            (setq auto-save-timeout 60))) ; Auto-save every 60 seconds

;;--- General UI and Keybindings ---
;; Which-key shows available keybindings interactively
(use-package which-key
  :defer 0.5 ; Defer slightly
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Enable disabled commands so you can use 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative) ; Or 'absolute or 'visual

;; Highlight current line
(global-hl-line-mode t)

;; More intuitive split window commands (optional, personal preference)
(global-set-key (kbd "C-x 2") 'split-window-below)
(global-set-key (kbd "C-x 3") 'split-window-right)
(global-set-key (kbd "C-x o") 'other-window) ; Already default, but good to know

;; Better scrolling
(setq scroll-step 1) ;; Scroll one line at a time with C-v and M-v
(setq scroll-conservatively 10000) ;; Avoid jumping when scrolling with mouse/touchpad

;; Save history
(use-package savehist
  :config
  (savehist-mode 1))

;; Save place in files
(use-package saveplace
  :config
  (save-place-mode 1))

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 100)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Diminish minor mode indicators in the modeline
(use-package diminish
  :config
  ;; Examples (add more for other modes you want to diminish)
  ;; (diminish 'auto-fill-function)
  ;; (diminish 'eldoc-mode)
  )

;; Modeline configuration (optional, consider a dedicated modeline package like doom-modeline or telephone-line later if desired)
(setq-default mode-line-format
              (list
               " "
               'mode-line-modified 'mode-line-buffer-identification " "
               'mode-line-position
               " (" 'mode-line-process "%n" ") "
               'mode-line-modes ; Display minor modes
               "   "
               'mode-line-misc-info
               ;; Add battery status if on a laptop
               ;; (if (display-battery-mode) 'battery-status "")
               " "
               ))

(provide 'init.el)
