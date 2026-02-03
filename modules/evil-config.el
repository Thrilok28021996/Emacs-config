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
  ;; Handle file saving
  (if (and (buffer-modified-p) (buffer-file-name))
        (if (y-or-n-p "Save buffer and quit? ")
            (progn (save-buffer) (kill-emacs))
          (if (y-or-n-p "Quit without saving? ")
              (kill-emacs)
            (message "Quit cancelled")))
    (kill-emacs)))

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

;; General.el for keybindings (modern replacement for evil-leader)
(use-package general
  :straight t
  :after evil
  :demand t
  :config
  ;; Create a leader key definer with SPC as prefix
  (general-create-definer my/leader-keys
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")  ; Also works in insert/emacs states

  ;; Define all leader keybindings
  (my/leader-keys
    "f f" 'find-file
    "f r" 'consult-recent-file
    "f s" 'save-buffer
    "f S" 'save-some-buffers
    "f d" 'dired
    "f D" 'delete-file
    "f R" 'rename-file
    "f c" 'copy-file
    ;; File utilities integrated with modern completion
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
    "t F" 'my/toggle-focus-mode
    "t v" 'visual-line-mode
    "t H" 'hl-todo-mode
    "t d" 'diff-hl-mode

    ;; Project operations (Projectile + built-in)
    "p f" 'projectile-find-file          ; Find file in project
    "p s" 'consult-ripgrep               ; Search in project
    "p b" 'consult-project-buffer        ; Switch project buffer
    "p p" 'projectile-switch-project     ; Switch project
    "p c" 'projectile-compile-project    ; Compile project
    "p t" 'projectile-test-project       ; Test project
    "p r" 'projectile-run-project        ; Run project
    "p d" 'projectile-dired              ; Open project root in dired
    "p k" 'projectile-kill-buffers       ; Kill project buffers
    "p i" 'projectile-invalidate-cache   ; Refresh project cache
    "p g" 'consult-find                  ; Generic find (fallback)

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

    ;; Python REPL operations (capital P for Python)
    "P e" 'python-shell-send-buffer       ; Execute buffer in REPL
    "P r" 'python-shell-send-region       ; Execute region
    "P d" 'python-shell-send-defun        ; Execute function
    "P l" 'python-shell-send-statement    ; Execute statement
    "P s" 'run-python                     ; Start Python REPL
    "P i" 'python-shell-switch-to-shell   ; Switch to REPL

    ;; Code operations (compile, format, actions)
    "c c" 'compile                       ; Compile project
    "c r" 'recompile                     ; Recompile
    "c k" 'kill-compilation              ; Kill compilation
    "c f" 'apheleia-format-buffer        ; Format buffer
    "c a" 'eglot-code-actions            ; Code actions (LSP)
    "c R" 'eglot-rename                  ; Rename symbol (LSP)

    ;; Compilation errors
    "c n" 'next-error                    ; Next error
    "c p" 'previous-error                ; Previous error
    "c l" 'compilation-mode              ; Compilation mode

    ;; Debug operations (DAP mode)
    "d d" 'my/dap-debug-current-file     ; Debug current file (smart)
    "d D" 'dap-debug                     ; Debug with template selection
    "d b" 'dap-breakpoint-toggle         ; Toggle breakpoint
    "d B" 'dap-breakpoint-condition      ; Conditional breakpoint
    "d l" 'dap-breakpoint-log-message    ; Log point
    "d a" 'dap-breakpoint-add            ; Add breakpoint
    "d r" 'dap-breakpoint-delete         ; Remove breakpoint
    "d R" 'dap-breakpoint-delete-all     ; Remove all breakpoints

    ;; Debug execution
    "d c" 'dap-continue                  ; Continue execution
    "d n" 'dap-next                      ; Step over
    "d s" 'dap-step-in                   ; Step into
    "d o" 'dap-step-out                  ; Step out
    "d t" 'dap-restart-frame             ; Restart frame
    "d q" 'dap-disconnect                ; Stop debugging
    "d Q" 'dap-delete-all-sessions       ; Kill all debug sessions

    ;; Debug inspection
    "d e" 'dap-eval                      ; Evaluate expression
    "d E" 'dap-eval-thing-at-point       ; Evaluate at point
    "d i" 'dap-ui-inspect-thing-at-point ; Inspect at point
    "d w" 'dap-ui-expressions-add        ; Add watch expression
    "d W" 'dap-ui-expressions-remove     ; Remove watch expression

    ;; Debug UI
    "d u" 'dap-ui-repl                   ; Open REPL
    "d h" 'dap-hydra                     ; Show all debug commands
    "d L" 'dap-ui-locals                 ; Show local variables
    "d S" 'dap-ui-sessions               ; Show debug sessions

    ;; Python-specific debugging
    "d p" 'my/dap-python-test-method     ; Debug current test method

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
    "l e" 'eglot                        ; Start Eglot manually
    "l E" 'eglot-ensure                 ; Ensure Eglot started
    "l r" 'eglot-find-references        ; Find references
    "l d" 'eglot-find-definition        ; Go to definition
    "l i" 'eglot-find-implementation    ; Find implementation
    "l t" 'eglot-find-typeDefinition    ; Go to type definition
    "l s" 'eldoc                        ; Show documentation
    "l h" 'eldoc-doc-buffer             ; Open doc buffer
    "l f" 'eglot-format-buffer          ; Format buffer
    "l R" 'eglot-reconnect              ; Reconnect LSP
    "l S" 'eglot-shutdown               ; Shutdown LSP
    "l K" 'eglot-shutdown-all           ; Shutdown all LSP
    
    ;; Error/diagnostic navigation
    "e n" 'flymake-goto-next-error
    "e p" 'flymake-goto-prev-error
    "e l" 'flymake-show-buffer-diagnostics
    "e L" 'flymake-show-project-diagnostics

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
    "n u" 'org-roam-ui-mode              ; Open knowledge graph UI
    "n j" 'org-journal-new-entry
    "n n" 'org-capture
    "n l" 'org-cliplink
    "n s" 'org-super-agenda-mode

    ;; Org-roam dailies
    "n d" 'org-roam-dailies-goto-today
    "n D" 'org-roam-dailies-goto-date
    "n y" 'org-roam-dailies-goto-yesterday
    "n t" 'org-roam-dailies-goto-tomorrow
    "n C" 'org-roam-dailies-capture-today

    ;; Org download (moved to different prefix to avoid conflict)
    "n S" 'org-download-screenshot
    "n Y" 'org-download-yank

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

    ;; Multiple cursors
    "m n" 'mc/mark-next-like-this
    "m p" 'mc/mark-previous-like-this
    "m a" 'mc/mark-all-like-this
    "m u" 'mc/unmark-next-like-this
    "m U" 'mc/unmark-previous-like-this
    "m s" 'mc/skip-to-next-like-this
    "m S" 'mc/skip-to-previous-like-this

    ;; Snippets
    "i s" 'yas-insert-snippet
    "i n" 'yas-new-snippet
    "i v" 'yas-visit-snippet-file

    ;; Workspace management
    "TAB TAB" 'persp-switch
    "TAB n" 'persp-next
    "TAB p" 'persp-prev
    "TAB c" 'persp-kill
    "TAB r" 'persp-rename

    ;; Git diff operations (using diff-hl)
    "g g" 'diff-hl-next-hunk
    "g G" 'diff-hl-previous-hunk
    "g r" 'diff-hl-revert-hunk
    "g d" 'diff-hl-diff-goto-hunk


    ;; Help and documentation
    "h f" 'describe-function
    "h V" 'describe-variable
    "h k" 'describe-key
    "h m" 'describe-mode
    "h g" 'garbage-collect
    "h M" 'memory-usage
    "h b" 'describe-bindings
    "h a" 'apropos
    "h i" 'info
    "h h" 'helpful-at-point

    ;; Available diagnostic functions
    "h D" 'my/validate-modern-config    ; Validation from init.el
    "h e" 'my/show-error-summary        ; Error summary from robustness-enhancements.el
    "h s" 'my/show-startup-errors       ; Startup errors from init.el
    "h w" 'my/optimize-for-low-memory   ; Memory optimization from utilities.el
    "h r" 'my/recover-from-errors       ; Error recovery from robustness-enhancements.el
    
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
    "a e" 'er/expand-region
    "a i" 'aggressive-indent-mode
    "a s" 'highlight-symbol-at-point
    "a n" 'highlight-symbol-next
    "a p" 'highlight-symbol-prev
    
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
    
    ;; Diff and comparison (moved from 'd' to 'D' prefix to avoid debug conflict)
    "D d" 'diff
    "D b" 'diff-buffer-with-file
    "D e" 'ediff
    "D f" 'ediff-files
    "D B" 'ediff-buffers
    "D w" 'compare-windows
    
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

;; Mode-specific keybindings using general.el
(with-eval-after-load 'nov
  (general-define-key
   :states 'normal
   :keymaps 'nov-mode-map
   :prefix "SPC"
   "n n" 'nov-next-document
   "n p" 'nov-previous-document
   "n g" 'nov-goto-document
   "n t" 'nov-display-metadata
   "n r" 'nov-render-document
   "n b" 'bookmark-set
   "n l" 'bookmark-bmenu-list))

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

;; --- Nov-mode (EPUB Reader) Evil Keybindings ---

(with-eval-after-load 'nov
  (evil-define-key 'normal nov-mode-map
    ;; Navigation - Chapter level
    (kbd "n") 'nov-next-document          ; Next chapter
    (kbd "p") 'nov-previous-document      ; Previous chapter
    (kbd "N") 'nov-next-document          ; Alternative: Next chapter
    (kbd "P") 'nov-previous-document      ; Alternative: Previous chapter

    ;; Navigation - Scrolling (preserve visual line movement)
    (kbd "j") 'evil-next-visual-line      ; Down one visual line
    (kbd "k") 'evil-previous-visual-line  ; Up one visual line
    (kbd "C-d") 'evil-scroll-down         ; Scroll down half page
    (kbd "C-u") 'evil-scroll-up           ; Scroll up half page
    (kbd "C-f") 'evil-scroll-page-down    ; Scroll down full page
    (kbd "C-b") 'evil-scroll-page-up      ; Scroll up full page
    (kbd "g g") 'beginning-of-buffer      ; Go to beginning
    (kbd "G") 'end-of-buffer              ; Go to end

    ;; Jump to chapter
    (kbd "g c") 'nov-goto-document        ; Go to chapter number
    (kbd "g t") 'nov-display-metadata     ; Display table of contents/metadata

    ;; Links and references
    (kbd "RET") 'nov-browse-url           ; Follow link
    (kbd "TAB") 'shr-next-link            ; Next link
    (kbd "S-TAB") 'shr-previous-link      ; Previous link
    (kbd "<backtab>") 'shr-previous-link  ; Previous link (alternative)

    ;; View controls
    (kbd "+") 'text-scale-increase        ; Increase font size
    (kbd "-") 'text-scale-decrease        ; Decrease font size
    (kbd "=") 'text-scale-adjust          ; Reset font size
    (kbd "0") (lambda () (interactive) (text-scale-set 0))  ; Reset to default size

    ;; Text width adjustment
    (kbd "w +") 'visual-fill-column-adjust ; Increase text width
    (kbd "w -") (lambda () (interactive)   ; Decrease text width
                  (setq-local visual-fill-column-width
                             (max 40 (- visual-fill-column-width 5)))
                  (visual-fill-column-adjust))
    (kbd "w =") (lambda () (interactive)   ; Reset text width
                  (setq-local visual-fill-column-width 80)
                  (visual-fill-column-adjust))

    ;; Bookmarks
    (kbd "m m") 'bookmark-set             ; Set bookmark
    (kbd "m l") 'bookmark-bmenu-list      ; List bookmarks
    (kbd "' '") 'bookmark-jump            ; Jump to bookmark

    ;; Search
    (kbd "/") 'isearch-forward            ; Search forward
    (kbd "?") 'isearch-backward           ; Search backward
    (kbd "*") 'isearch-forward-symbol-at-point  ; Search for symbol at point
    (kbd "#") 'isearch-backward-symbol-at-point ; Search backward for symbol

    ;; Refresh and reload
    (kbd "R") 'nov-render-document        ; Re-render current chapter
    (kbd "g r") 'nov-render-document      ; Alternative: Re-render

    ;; Copy and yank
    (kbd "y y") 'kill-ring-save           ; Copy line (in visual mode)

    ;; Quit
    (kbd "q") 'quit-window                ; Quit nov buffer
    (kbd "Z Z") 'quit-window              ; Vim-style quit
    (kbd "Z Q") 'kill-current-buffer      ; Force quit

    ;; Help
    (kbd "?") 'describe-mode              ; Show nov-mode help
    (kbd "H") 'nov-display-metadata       ; Show metadata/TOC
    )

  ;; Visual state keybindings for selection
  (evil-define-key 'visual nov-mode-map
    (kbd "y") 'kill-ring-save             ; Yank/copy selection
    (kbd "d") 'delete-region              ; Delete selection
    )

  ;; Motion state keybindings
  (evil-define-key 'motion nov-mode-map
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "C-d") 'evil-scroll-down
    (kbd "C-u") 'evil-scroll-up))

;; Set nov-mode to use normal state by default
(with-eval-after-load 'nov
  (evil-set-initial-state 'nov-mode 'normal))

(provide 'evil-config)
;;; evil-config.el ends here