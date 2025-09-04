;;; modules/evil-config.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Complete Evil (Vim emulation) configuration with leader keys
;;; Includes evil-collection, evil-leader, and related packages

;;; Code:

(require 'utilities)  ; For defer timing constants

;; --- Quick Quit Functions ---

(defun my/quick-quit ()
  "Quit Emacs immediately without saving."
  (interactive)
  (if (y-or-n-p "Really quit Emacs without saving? ")
      (kill-emacs)
    (message "Quit cancelled")))

(defun my/smart-quit ()
  "Smart quit that saves automatically or prompts."
  (interactive)
  (if nil  ; Removed dashboard check
      nil
    ;; Otherwise, handle file saving
    (if (and (buffer-modified-p) (buffer-file-name))
        (if (y-or-n-p "Save buffer and quit? ")
            (progn (save-buffer) (kill-emacs))
          (if (y-or-n-p "Quit without saving? ")
              (kill-emacs)
            (message "Quit cancelled")))
      (kill-emacs))))

;; --- Evil Mode (Vim Emulation) ---
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-search-module 'evil-search)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (setq evil-default-state 'normal)
  
  ;; Set initial states for various modes
  (dolist (mode-state '((messages-buffer-mode . emacs)
                        (help-mode . emacs)
                        (Man-mode . emacs)
                        (Info-mode . emacs)
                        (special-mode . emacs)
                        (compilation-mode . emacs)
                        (dired-mode . emacs)
                        (term-mode . emacs)
                        (eshell-mode . emacs)
                        (vterm-mode . emacs)
                        (org-agenda-mode . emacs)
                        (magit-mode . emacs)
                        (magit-popup-mode . emacs)
                        (lsp-lens-mode . emacs)
                        (custom-mode . emacs)))
    (evil-set-initial-state (car mode-state) (cdr mode-state)))
  
  
  ;; Better window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  
  ;; Visual line mode navigation
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  
  ;; Select all keybinding
  (define-key evil-normal-state-map (kbd "C-a") 'mark-whole-buffer)
  (define-key evil-visual-state-map (kbd "C-a") 'mark-whole-buffer)
  
  ;; Quick save
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)
  
  ;; Insert mode improvements
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-insert-state-map (kbd "C-u") 'backward-kill-line)
  
  ;; Global quit keybindings that work everywhere (safe bindings)
  (define-key evil-normal-state-map (kbd "C-c q") 'my/smart-quit)
  (define-key evil-emacs-state-map (kbd "C-c q") 'my/smart-quit))

;; Evil collection for consistent evil bindings across modes
(use-package evil-collection
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (evil-collection-init))

;; Evil leader for <SPC> prefix commands
(use-package evil-leader
  :straight t
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  
  ;; File operations (aligned with init.el)
  (evil-leader/set-key
    "f f" 'find-file
    "f r" 'consult-recent-file
    "f s" 'save-buffer
    "f S" 'save-some-buffers
    "f d" 'dired
    "f D" 'delete-file
    "f R" 'rename-file
    "f c" 'copy-file
    "f y" 'my/copy-file-path
    "f n" 'my/copy-file-name
    "f F" 'consult-find
    "f L" 'consult-locate
    "f j" 'dired-jump
    "f J" 'dired-jump-other-window
    "f o" 'dired-other-window
    "f m" 'dired-create-directory
    
    ;; Buffer operations (aligned with init.el)
    "b b" 'consult-buffer
    "b k" 'kill-buffer
    "b K" 'kill-buffer-and-window
    "b n" 'next-buffer
    "b p" 'consult-project-buffer
    "b r" 'revert-buffer
    "b s" 'scratch-buffer
    "b l" 'list-buffers
    "b a" 'mark-whole-buffer
    "b m" 'consult-bookmark
    
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
    "w t" 'my/toggle-window-split
    "w a" 'ace-window
    "w u" 'winner-undo
    "w r" 'winner-redo
    "w p" 'popper-toggle-latest
    "w P" 'popper-cycle
    
    ;; Toggles
    "t l" 'display-line-numbers-mode
    "t r" 'my/toggle-relative-line-numbers
    "t w" 'whitespace-mode
    "t h" 'global-hl-line-mode
    "t f" 'auto-fill-mode
    "t T" 'my/toggle-theme
    "t L" 'load-theme
    "t c" 'my/toggle-color-identifiers
    "t R" 'my/toggle-rainbow-delimiters
    "t C" 'my/enable-all-color-enhancements
    "t X" 'my/reset-color-enhancements
    "t m" 'minimap-mode
    "t F" 'my/toggle-focus-mode
    "t v" 'visual-line-mode
    "t V" 'visual-fill-column-mode
    "t H" 'hl-todo-mode
    
    ;; Project operations
    "p f" 'consult-find
    "p s" 'consult-ripgrep
    "p b" 'consult-project-buffer
    "p p" 'project-switch-project
    
    ;; Search operations
    "s s" 'consult-line
    "s S" 'consult-line-multi
    "s r" 'consult-ripgrep
    "s g" 'consult-grep
    "s i" 'consult-imenu
    "s I" 'consult-imenu-multi
    "s o" 'consult-outline
    
    ;; Search and replace operations
    "r r" 'query-replace
    "r R" 'query-replace-regexp
    "r w" 'query-replace-word
    "r s" 'replace-string
    "r p" 'project-query-replace-regexp
    "r d" 'dired-do-find-regexp-and-replace
    "r g" 'rgrep
    
    ;; Navigation
    "j j" 'consult-line
    "j i" 'consult-imenu
    "j o" 'consult-outline
    "j g" 'consult-goto-line
    "j m" 'consult-mark
    "j k" 'consult-global-mark
    
    ;; Modern features
    "m a" 'embark-act
    "m d" 'embark-dwim
    "m x" 'embark-export
    
    ;; System operations (improved quit behavior)
    "q q" 'my/smart-quit          ; Smart quit
    "q !" 'my/quick-quit          ; Force quit (with confirmation)  
    "q s" 'save-buffers-kill-terminal  ; Save all and quit
    "q r" 'restart-emacs          ; Restart Emacs
    "q x" 'save-buffers-kill-emacs    ; Save all and quit (alternative)
    "q d" 'bury-buffer           ; Hide current buffer
    
    ;; Environment management (Conda/Python)
    "V a" 'conda-env-activate
    "V d" 'conda-env-deactivate
    "V l" 'conda-env-list
    "V c" 'conda-env-activate-for-buffer
    
    ;; Compile operations
    "c c" 'compile
    "c r" 'recompile
    "c k" 'kill-compilation
    "c n" 'next-error
    "c p" 'previous-error
    "c l" 'compilation-mode
    
    ;; Comment operations
    ";" 'comment-line
    "/ /" 'comment-or-uncomment-region
    "/ l" 'comment-line
    "/ r" 'comment-or-uncomment-region
    "/ b" 'comment-box
    "/ d" 'comment-kill
    
    ;; Global search and navigation
    "/" 'consult-line
    "?" 'consult-line-multi
    "SPC" 'execute-extended-command
    
    ;; LSP/Language server operations
    "l r" 'eglot-find-references
    "l R" 'eglot-rename
    "l d" 'eglot-find-definition
    "l i" 'eglot-find-implementation
    "l t" 'eglot-find-typeDefinition
    "l s" 'eldoc
    "l h" 'eldoc-doc-buffer
    "l a" 'eglot-code-actions
    "l f" 'eglot-format
    "l F" 'eglot-format-buffer
    "l w r" 'eglot-reconnect
    "l w s" 'eglot-shutdown
    "l w A" 'eglot-shutdown-all
    
    ;; Error/diagnostic navigation
    "e n" 'flymake-goto-next-error
    "e p" 'flymake-goto-prev-error
    "e l" 'flymake-show-buffer-diagnostics
    "e L" 'flymake-show-project-diagnostics
    
    ;; Code formatting
    "c f" 'apheleia-format-buffer
    
    ;; Git operations (if magit is available)
    "g s" 'magit-status
    "g b" 'magit-blame
    "g l" 'magit-log
    "g f" 'magit-file-dispatch
    "g c" 'magit-commit
    "g p" 'magit-push
    "g u" 'magit-pull
    
    ;; Terminal and shell operations  
    "T t" 'term
    "T e" 'eshell
    "T s" 'shell
    "T a" 'async-shell-command
    "T c" 'shell-command-on-region
    
    ;; Org-mode operations (comprehensive)
    "o o" 'org-agenda
    "o c" 'org-capture
    "o l" 'org-store-link
    "o i" 'org-insert-link
    "o t" 'org-todo
    "o s" 'org-schedule
    "o d" 'org-deadline
    "o r" 'org-refile
    "o a" 'org-archive-subtree
    "o x" 'org-clock-in
    "o z" 'org-clock-out
    
    ;; Note-taking and roam (if available)
    "n f" 'org-roam-node-find
    "n i" 'org-roam-node-insert
    "n c" 'org-roam-capture
    "n b" 'org-roam-buffer-toggle
    "n g" 'org-roam-graph
    "n j" 'org-journal-new-entry
    "n n" 'org-capture
    
    ;; Markdown operations
    "M m" 'markdown-mode
    "M p" 'markdown-preview
    "M t" 'markdown-toc-generate-toc
    "M g" 'grip-mode
    
    ;; Text operations
    "x d" 'delete-trailing-whitespace
    "x w" 'whitespace-cleanup
    "x s" 'sort-lines
    "x u" 'upcase-region
    "x l" 'downcase-region
    "x c" 'capitalize-region
    
    ;; Bookmarks (moved to avoid backup confusion)
    "k s" 'bookmark-set
    "k j" 'bookmark-jump
    "k d" 'bookmark-delete
    "k l" 'bookmark-bmenu-list
    
    ;; Rectangle operations
    "R r" 'rectangle-mark-mode
    "R k" 'kill-rectangle
    "R y" 'yank-rectangle
    "R o" 'open-rectangle
    "R c" 'clear-rectangle
    "R s" 'string-rectangle
    
    
    ;; Help and documentation
    "h f" 'describe-function
    "h V" 'describe-variable
    "h k" 'describe-key
    "h m" 'describe-mode
    "h P" 'my/monitor-performance-continuously
    "h g" 'garbage-collect
    "h M" 'memory-usage
    "h b" 'describe-bindings
    "h a" 'apropos
    "h i" 'info
    "h h" 'helpful-at-point
    "h s" 'my/show-startup-history
    "h e" 'my/show-error-report
    "h x" 'my/reinstall-failed-packages
    "h v" 'my/validate-critical-packages
    "h r" 'my/show-resource-report
    "h w" 'my/optimize-for-low-memory
    "h n" 'my/check-network-connectivity
    "h z" 'my/show-security-report
    "h o" 'my/toggle-offline-mode
    "h t" 'my/test-configuration
    
    ;; Backup management
    "U c" 'my/create-config-backup
    "U r" 'my/restore-from-backup
    "U l" 'my/list-available-backups
    
    ;; Quick utilities
    "R" 'revert-buffer-quick
    "=" 'count-words-region
    "I" 'indent-region
    
    ;; Universal prefix shortcuts
    "u" 'universal-argument
    
    ;; Advanced editing
    "a l" 'align-regexp
    "a r" 'align-region
    "a c" 'align-current
    
    ;; Registers and macros
    "M" 'call-last-kbd-macro
    "@ r" 'copy-to-register
    "@ i" 'insert-register
    "@ j" 'jump-to-register
    "@ w" 'window-configuration-to-register
    "@ f" 'frameset-to-register
    
    ;; Calculator and tools
    "k k" 'calc
    "k q" 'quick-calc
    "k r" 'calc-region
    
    ;; Spelling and grammar
    "z z" 'ispell-word
    "z b" 'ispell-buffer
    "z r" 'ispell-region
    "z d" 'ispell-change-dictionary
    
    ;; System and utilities
    "y c" 'calendar
    "y w" 'eww
    "y p" 'proced
    "y l" 'list-packages
    "y s" 'shell-command
    "y m" 'man
    "y M" 'woman
    "y i" 'ibuffer
    "y d" 'apropos-documentation
    "y C" 'customize
    "y G" 'customize-group
    
    ;; Diff and comparison
    "d d" 'diff
    "d b" 'diff-buffer-with-file
    "d e" 'ediff
    "d f" 'ediff-files
    "d B" 'ediff-buffers
    "d w" 'compare-windows
    
    ;; Search and analysis operations
    "S o" 'occur
    "S m" 'multi-occur
    "S f" 'flush-lines
    "S k" 'keep-lines
    "S h" 'how-many
    "S w" 'what-line
    "S g" 'goto-char
    "S u" 'browse-url
    "S p" 'browse-url-at-point
    "S F" 'ffap
    
    ;; Additional mode toggles (integrated above)
    "t S" 'flyspell-mode
    "t A" 'auto-revert-mode
    "t o" 'follow-mode
    "t B" 'abbrev-mode
    "t P" 'show-paren-mode
    "t E" 'electric-pair-mode))

;; Evil surround for vim-surround functionality
(use-package evil-surround
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (global-evil-surround-mode 1))

;; Evil commentary for commenting
(use-package evil-commentary
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (evil-commentary-mode))

;; Evil numbers for incrementing/decrementing
(use-package evil-numbers
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; Magit for Git integration (optional)
(use-package magit
  :straight t
  :defer t
  :commands (magit-status magit-blame magit-log magit-file-dispatch magit-commit magit-push magit-pull))

(provide 'evil-config)
;;; evil-config.el ends here