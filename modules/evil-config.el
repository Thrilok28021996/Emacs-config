;;; modules/evil-config.el --- Evil mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil (Vim emulation) with a Doom-style SPC leader key layout.
;;
;; Why Evil? Vim's modal editing (normal/insert/visual) is more
;; ergonomic for heavy editing — movements don't require modifier
;; keys.  Evil provides a nearly complete Vim implementation inside
;; Emacs, letting us use Vim muscle memory with Emacs's ecosystem.
;;
;; Architecture:
;;   evil-core     — modal editing engine (normal/insert/visual states)
;;   evil-collection — fixes keybindings in non-editing buffers (dired, magit, etc.)
;;   general.el    — declarative leader-key system (SPC prefix, like Doom/Spacemacs)
;;
;; Keybinding reference (SPC prefix):
;;   f  files        b  buffers      w  windows     t  toggles
;;   p  project      s  search       r  replace     j  jump
;;   c  code/compile d  debug (DAP)  l  LSP/Eglot   e  errors
;;   g  git (magit)  o  org-mode     n  notes/roam   q  quit
;;   v  conda/venv   P  python REPL  J  jupyter      o  open/org
;;   m  embark/mc/mode  x  text ops   i  snippets
;;   D  diff/ediff   S  search-analysis  k  bookmarks/calc
;;   a  align/edit   @  registers    z  spelling
;;   y  system utils TAB workspaces  h  help/diag    /  comments
;;
;; Quick-access single keys (no prefix group):
;;   SPC SPC  M-x                '  eshell        .  find-file
;;   ,  persp-switch-to-buffer   <  consult-buffer
;;   ?  consult-line-multi       ;  comment-line
;;   u  universal-argument       =  count-words-region
;;
;; Important remappings from Emacs/Vim defaults:
;;   C-u     — scroll up (not universal-argument; use SPC u instead)
;;   C-h     — window-left in normal state (not help; use SPC h instead)
;;   C-a/C-s — mark-all/save in normal state (non-standard)

;;; Code:

(require 'utilities)

;; Silence byte-compiler warnings for deferred keymaps
(defvar nov-mode-map)

;; Silence byte-compiler warnings for deferred functions
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key "evil-core")
(declare-function evil-define-key* "evil-core")
(declare-function evil-collection-init "evil-collection")
(declare-function general-create-definer "general")
(declare-function general-define-key "general")
(declare-function global-evil-surround-mode "evil-surround")
(declare-function evil-commentary-mode "evil-commentary")
(declare-function visual-fill-column-adjust "visual-fill-column")

;; ══════════════════════════════════════════════════════════════════
;;  Quit Helpers
;; ══════════════════════════════════════════════════════════════════

(defun my/quick-quit ()
  "Quit Emacs immediately without saving."
  (interactive)
  (if (y-or-n-p "Really quit Emacs without saving? ")
      (kill-emacs)
    (message "Quit cancelled")))

(defun my/smart-quit ()
  "Save the current buffer if modified, then quit Emacs."
  (interactive)
  (if (and (buffer-modified-p) (buffer-file-name))
      (if (y-or-n-p "Save buffer and quit? ")
          (progn (save-buffer) (kill-emacs))
        (if (y-or-n-p "Quit without saving? ")
            (kill-emacs)
          (message "Quit cancelled")))
    (kill-emacs)))

;; ══════════════════════════════════════════════════════════════════
;;  Evil Core
;; ══════════════════════════════════════════════════════════════════

(use-package evil
  :straight t
  :init
  ;; These must be set BEFORE evil loads (they're read during initialization)
  (setq evil-want-integration t          ; enable evil in core Emacs buffers
        evil-want-keybinding nil         ; DON'T set keybindings in dired/magit/etc — evil-collection does this better
        evil-want-C-u-scroll t           ; C-u scrolls up (like Vim), not universal-argument
        evil-want-C-d-scroll t           ; C-d scrolls down half page
        evil-want-C-i-jump t             ; C-i jumps forward in jump list (like Vim)
        evil-respect-visual-line-mode t  ; j/k move by visual lines when visual-line-mode is on
        evil-undo-system 'undo-redo      ; use Emacs 28+ native undo-redo (not undo-tree)
        evil-search-module 'evil-search  ; use evil's own search (supports /, ?, n, N, *)
        evil-split-window-below t        ; :sp puts new window below (like Vim's splitbelow)
        evil-vsplit-window-right t)      ; :vsp puts new window right (like Vim's splitright)
  :config
  (evil-mode 1)
  (setq evil-default-state 'normal)

  ;; ── System clipboard integration ────────────────────────────
  ;; Make Evil yank/paste use system clipboard by default (like
  ;; Vim's clipboard=unnamedplus).  Without this, yanked text
  ;; stays in Emacs kill-ring and doesn't reach system clipboard.
  (setq select-enable-clipboard t        ; use system clipboard for kill/yank
        select-enable-primary t          ; also use PRIMARY selection (X11 middle-click)
        save-interprogram-paste-before-kill t)  ; preserve clipboard when killing in Emacs

  ;; Modes that should start in Emacs state (non-editing buffers).
  ;; These are read-only or interactive buffers where Vim keys would
  ;; conflict with their native bindings (e.g. 'g' in magit, 'q' in
  ;; help).  evil-collection provides proper bindings for these modes.
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

  ;; ── Window navigation (C-h/j/k/l) ────────────────────────────
  ;; Overrides C-h (help prefix) in normal state — use SPC h instead.
  ;; This mirrors tmux/i3 directional navigation muscle memory.
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

  ;; ── Visual-line-aware j/k ────────────────────────────────────
  ;; Move by visual (wrapped) lines instead of logical lines.
  ;; Without this, j/k skip over long wrapped lines in one jump.
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

  ;; ── Convenience overrides ────────────────────────────────────
  ;; C-a/C-s use familiar CUA-style shortcuts even in normal state,
  ;; making it easier for non-Vim users to select-all and save.
  (define-key evil-normal-state-map (kbd "C-a") 'mark-whole-buffer)
  (define-key evil-visual-state-map (kbd "C-a") 'mark-whole-buffer)
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") 'save-buffer)

  ;; ── Insert-mode editing shortcuts ────────────────────────────
  ;; These mirror Vim's insert-mode shortcuts: C-h = backspace,
  ;; C-w = delete word backward, C-u = delete to start of line.
  (define-key evil-insert-state-map (kbd "C-h") 'backward-delete-char)
  (define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-insert-state-map (kbd "C-u") (lambda () (interactive) (kill-line 0)))

  ;; ── Lookup bindings (g d / g D / K) ──────────────────────────
  ;; These mirror Vim-LSP conventions.  The lookup functions
  ;; (defined in modern-languages.el) try Eglot first, then fall
  ;; back to xref/dumb-jump when no LSP server is active.
  (define-key evil-normal-state-map (kbd "g d") 'my/lookup-definition)
  (define-key evil-normal-state-map (kbd "g D") 'my/lookup-references)
  (define-key evil-normal-state-map (kbd "K") 'my/lookup-documentation)

  ;; ── Bracket navigation (] / [) ───────────────────────────────
  ;; Doom/unimpaired-style ]x/[x pairs for jumping between things.
  ;; ]e/[e — flymake diagnostics (errors/warnings)
  ;; ]h/[h — git diff hunks (shown by diff-hl in gutter)
  ;; ]b/[b — buffer navigation
  ;; ]q/[q — compilation errors (quickfix-style)
  (define-key evil-normal-state-map (kbd "] e") 'flymake-goto-next-error)
  (define-key evil-normal-state-map (kbd "[ e") 'flymake-goto-prev-error)
  (define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
  (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "] q") 'next-error)
  (define-key evil-normal-state-map (kbd "[ q") 'previous-error)
  (define-key evil-normal-state-map (kbd "] s") 'flyspell-goto-next-error)
  (define-key evil-normal-state-map (kbd "[ s") 'flyspell-check-previous-highlighted-word)

  ;; ── Global quit (C-c q) ──────────────────────────────────────
  (define-key evil-normal-state-map (kbd "C-c q") 'my/smart-quit)
  (define-key evil-emacs-state-map (kbd "C-c q") 'my/smart-quit))

;; ══════════════════════════════════════════════════════════════════
;;  Evil Collection — selective mode initialization
;; ══════════════════════════════════════════════════════════════════
;; evil-collection provides Vim-style keybindings for 100+ non-editing
;; modes (dired, magit, help, etc.).  By default `evil-collection-init`
;; with no args initializes ALL of them, which takes ~0.3s.  We only
;; initialize the 13 modes we actually use.  Add entries here as you
;; adopt new modes (run M-x evil-collection-init to see all available).

(use-package evil-collection
  :straight t
  :after evil
  :defer 0.5
  :config
  (evil-collection-init
   '(dired magit compile info help custom
     flymake xref minibuffer corfu vertico
     embark consult dashboard)))

;; ══════════════════════════════════════════════════════════════════
;;  Leader Keys (SPC) — via general.el
;; ══════════════════════════════════════════════════════════════════

(use-package general
  :straight t
  :after evil
  :demand t
  :config
  ;; Create a definer that binds under SPC in normal/visual state and
  ;; under C-SPC in insert/emacs state (so leader keys are always
  ;; accessible).  The override keymap ensures these bindings take
  ;; priority over any mode-local keymaps.
  (general-create-definer my/leader-keys
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (my/leader-keys
    ;; ── Quick access ──────────────────────────────────────────
    "SPC" 'execute-extended-command
    "'"   'eshell
    "."   'find-file
    ","   'persp-switch-to-buffer
    "<"   'consult-buffer
    "?"   'consult-line-multi
    ";"   'comment-line
    "u"   'universal-argument
    "="   'count-words-region

    ;; ── f: Files ──────────────────────────────────────────────
    "f f" 'find-file
    "f r" 'consult-recent-file
    "f s" 'save-buffer
    "f S" 'save-some-buffers
    "f d" 'dired
    "f D" 'delete-file
    "f R" 'rename-file
    "f c" 'copy-file
    "f F" 'consult-find
    "f L" 'consult-locate
    "f j" 'dired-jump
    "f J" 'dired-jump-other-window
    "f o" 'dired-other-window
    "f m" 'dired-create-directory
    "f y" '(lambda () (interactive) (kill-new (buffer-file-name)))  ; copy file path
    "f Y" '(lambda () (interactive) (kill-new (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))  ; copy with line

    ;; ── b: Buffers ────────────────────────────────────────────
    "b b" 'consult-buffer
    "b k" 'kill-buffer
    "b K" 'kill-buffer-and-window
    "b n" 'next-buffer
    "b p" 'previous-buffer
    "b P" 'consult-project-buffer
    "b r" 'revert-buffer
    "b s" 'scratch-buffer
    "b l" 'list-buffers
    "b a" 'mark-whole-buffer
    "b m" 'consult-bookmark

    ;; ── w: Windows ────────────────────────────────────────────
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

    ;; ── t: Toggles ────────────────────────────────────────────
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
    "t S" 'flyspell-mode
    "t A" 'auto-revert-mode
    "t o" 'follow-mode
    "t B" 'abbrev-mode
    "t P" 'show-paren-mode
    "t E" 'electric-pair-mode

    ;; ── p: Project (projectile + consult) ─────────────────────
    "p f" 'projectile-find-file
    "p s" 'consult-ripgrep
    "p b" 'consult-project-buffer
    "p p" 'projectile-switch-project
    "p c" 'projectile-compile-project
    "p t" 'projectile-test-project
    "p r" 'projectile-run-project
    "p d" 'projectile-dired
    "p k" 'projectile-kill-buffers
    "p i" 'projectile-invalidate-cache
    "p g" 'consult-find

    ;; ── s: Search ─────────────────────────────────────────────
    "s s" 'consult-line
    "s S" 'consult-line-multi
    "s r" 'consult-ripgrep
    "s g" 'consult-grep
    "s i" 'consult-imenu
    "s I" 'consult-imenu-multi
    "s o" 'consult-outline

    ;; ── r: Replace ────────────────────────────────────────────
    "r r" 'query-replace
    "r R" 'query-replace-regexp
    "r w" '(lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'query-replace)))
    "r s" 'replace-string
    "r p" 'project-query-replace-regexp
    "r d" 'dired-do-find-regexp-and-replace
    "r g" 'rgrep

    ;; ── j: Jump / Navigate ────────────────────────────────────
    "j j" 'consult-line
    "j i" 'consult-imenu
    "j o" 'consult-outline
    "j g" 'consult-goto-line
    "j m" 'consult-mark
    "j k" 'consult-global-mark

    ;; ── m: Embark / Multiple cursors ──────────────────────────
    ;; Embark (contextual actions on completion candidates)
    "m a" 'embark-act
    "m d" 'embark-dwim
    "m x" 'embark-export
    ;; Multiple cursors
    "m n" 'mc/mark-next-like-this
    "m p" 'mc/mark-previous-like-this
    "m A" 'mc/mark-all-like-this
    "m u" 'mc/unmark-next-like-this
    "m U" 'mc/unmark-previous-like-this
    "m s" 'mc/skip-to-next-like-this
    "m S" 'mc/skip-to-previous-like-this

    ;; ── q: Quit / Session ─────────────────────────────────────
    "q q" 'my/smart-quit                          ; save if modified, then quit
    "q !" 'my/quick-quit                          ; quit without saving (confirms)
    "q s" 'save-buffers-kill-terminal
    "q r" 'restart-emacs                          ; requires restart-emacs package (init.el)
    "q x" 'save-buffers-kill-emacs
    "q d" 'bury-buffer

    ;; ── v: Conda/virtualenv ───────────────────────────────────
    "v a" 'conda-env-activate
    "v d" 'conda-env-deactivate
    "v l" 'conda-env-list
    "v c" 'conda-env-activate-for-buffer

    ;; ── P: Python REPL ────────────────────────────────────────
    "P e" 'python-shell-send-buffer
    "P r" 'python-shell-send-region
    "P d" 'python-shell-send-defun
    "P l" 'python-shell-send-statement
    "P s" 'run-python
    "P i" 'python-shell-switch-to-shell
    "P R" '(lambda () (interactive) (python-shell-kill-process) (run-python))  ; restart REPL

    ;; ── J: Jupyter ───────────────────────────────────────────
    "J r" 'my/jupyter-run-python            ; start Python kernel REPL
    "J c" 'jupyter-connect-repl             ; connect to running kernel
    "J e" 'jupyter-eval-buffer              ; send entire buffer
    "J s" 'my/jupyter-send-region-or-line   ; send region or current line
    "J d" 'jupyter-eval-defun               ; send current function
    "J i" 'jupyter-repl-pop-to-buffer       ; switch to REPL buffer
    "J a" 'my/jupyter-associate-python      ; associate buffer with REPL
    "J R" 'my/jupyter-restart-kernel        ; restart kernel
    "J I" 'jupyter-repl-interrupt-kernel    ; interrupt running computation

    ;; ── c: Code / Compile ─────────────────────────────────────
    "c c" 'compile
    "c r" 'recompile
    "c k" 'kill-compilation
    "c f" 'apheleia-format-buffer
    "c a" 'eglot-code-actions
    "c R" 'eglot-rename
    "c i" 'py-isort-buffer
    "c n" 'compilation-next-error
    "c p" 'compilation-previous-error
    "c l" 'compilation-mode

    ;; ── d: Debug (DAP) ────────────────────────────────────────
    "d d" 'my/dap-debug-current-file
    "d D" 'dap-debug
    "d b" 'dap-breakpoint-toggle
    "d B" 'dap-breakpoint-condition
    "d l" 'dap-breakpoint-log-message
    "d a" 'dap-breakpoint-add
    "d r" 'dap-breakpoint-delete
    "d R" 'dap-breakpoint-delete-all
    "d c" 'dap-continue
    "d n" 'dap-next
    "d s" 'dap-step-in
    "d o" 'dap-step-out
    "d t" 'dap-restart-frame
    "d q" 'dap-disconnect
    "d Q" 'dap-delete-all-sessions
    "d e" 'dap-eval
    "d E" 'dap-eval-thing-at-point
    "d i" 'dap-ui-inspect-thing-at-point
    "d w" 'dap-ui-expressions-add
    "d W" 'dap-ui-expressions-remove
    "d u" 'dap-ui-repl
    "d h" 'dap-hydra
    "d L" 'dap-ui-locals
    "d S" 'dap-ui-sessions
    "d p" 'my/dap-python-test-method

    ;; ── /: Comments ───────────────────────────────────────────
    "/ /" 'comment-or-uncomment-region
    "/ l" 'comment-line
    "/ r" 'comment-or-uncomment-region
    "/ b" 'comment-box
    "/ d" 'comment-kill

    ;; ── l: LSP / Eglot ───────────────────────────────────────
    "l e" 'eglot
    "l E" 'eglot-ensure
    "l r" 'eglot-find-references
    "l d" 'eglot-find-definition
    "l i" 'eglot-find-implementation
    "l t" 'eglot-find-typeDefinition
    "l s" 'eldoc
    "l h" 'eldoc-doc-buffer
    "l f" 'eglot-format-buffer
    "l R" 'eglot-reconnect
    "l S" 'eglot-shutdown
    "l K" 'eglot-shutdown-all

    ;; ── e: Errors / Diagnostics (flymake) ─────────────────────
    "e n" 'flymake-goto-next-error
    "e p" 'flymake-goto-prev-error
    "e l" 'flymake-show-buffer-diagnostics
    "e L" 'flymake-show-project-diagnostics

    ;; ── g: Git (magit + diff-hl) ──────────────────────────────
    "g s" 'magit-status
    "g b" 'magit-blame
    "g l" 'magit-log
    "g f" 'magit-file-dispatch
    "g c" 'magit-commit
    "g p" 'magit-push
    "g P" 'magit-pull
    "g F" 'magit-fetch
    "g g" 'diff-hl-next-hunk
    "g G" 'diff-hl-previous-hunk
    "g r" 'diff-hl-revert-hunk
    "g d" 'diff-hl-diff-goto-hunk

    ;; ── o: Org-mode & Open ────────────────────────────────────
    ;; Org operations
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
    "o p" 'org-pomodoro
    "o w" 'my/org-writing-mode            ; toggle olivetti distraction-free mode
    "o W" 'my/org-word-count             ; count words in subtree/region
    "o e" 'my/org-export-to-writing-folder ; export to writing-output/ folder
    "o n" 'org-noter                      ; open org-noter for document annotation
    "o N" 'org-noter-create-skeleton      ; create note skeleton from PDF headings
    ;; Terminal / Shell (moved from T)
    "o T" 'term                           ; terminal
    "o E" 'eshell                         ; eshell
    "o S" 'shell                          ; shell
    "o A" 'async-shell-command            ; async command
    "o C" 'shell-command-on-region        ; command on region

    ;; ── n: Notes / Org-roam ───────────────────────────────────
    "n f" 'org-roam-node-find
    "n i" 'org-roam-node-insert
    "n I" 'my/org-roam-node-insert-immediate   ; insert node, skip capture buffer
    "n c" 'org-roam-capture
    "n b" 'my/org-roam-open-backlinks          ; toggle backlinks side window
    "n B" 'org-roam-buffer-toggle              ; raw roam buffer toggle
    "n g" 'org-roam-graph
    "n u" 'org-roam-ui-mode
    "n j" 'org-journal-new-entry
    "n n" 'org-capture
    "n l" 'org-cliplink
    "n L" 'my/org-roam-copy-node-link          ; copy roam link to clipboard
    "n s" 'org-super-agenda-mode
    "n d" 'org-roam-dailies-goto-today
    "n D" 'org-roam-dailies-goto-date
    "n y" 'org-roam-dailies-goto-yesterday
    "n t" 'org-roam-dailies-goto-tomorrow
    "n C" 'org-roam-dailies-capture-today
    "n S" 'org-download-screenshot
    "n Y" 'org-download-yank
    ;; consult-org-roam (full-text search)
    "n /" 'consult-org-roam-search             ; ripgrep across all roam notes
    "n <" 'consult-org-roam-backlinks          ; backlinks for current node
    "n >" 'consult-org-roam-forward-links      ; forward links from current node
    "n F" 'consult-org-roam-file-find          ; find roam file by name
    ;; transclusion
    "n T" 'org-transclusion-mode               ; toggle transclusion mode
    "n a" 'org-transclusion-add                ; add transclusion at point

    ;; ── N: Denote (flat-file quick notes) ─────────────────────
    ;; Denote uses DATE--TITLE__KEYWORDS.org filenames.
    ;; Use for quick/fleeting notes; use SPC n for roam long-form notes.
    "N n" 'denote                              ; new note (prompts title + keywords)
    "N N" 'denote-open-or-create              ; open existing or create new
    "N f" 'denote-find-file                   ; find a denote note by name
    "N l" 'denote-link                        ; insert link to another denote note
    "N b" 'denote-backlinks                   ; show backlinks to current note
    "N B" 'denote-find-backlink               ; find a backlink interactively
    "N k" 'denote-keywords-add                ; add keywords to current note
    "N K" 'denote-keywords-remove             ; remove keywords
    "N r" 'denote-rename-file                 ; rename (updates filename + front-matter)
    "N R" 'denote-rename-file-using-front-matter ; sync filename from front-matter
    "N m" 'list-denotes                       ; browse all denote notes (tabulated)
    "N s" 'denote-sort-files                  ; sort notes by date/title/keywords

    ;; ── M: Markdown / Macros ───────────────────────────────────
    "M M" 'call-last-kbd-macro                    ; replay last macro
    "M m" 'markdown-mode
    "M p" 'markdown-preview
    "M t" 'markdown-toc-generate-toc
    "M g" 'grip-mode
    "M e" 'my/markdown-edit-code-block

    ;; ── x: Text manipulation ──────────────────────────────────
    "x d" 'delete-trailing-whitespace
    "x w" 'whitespace-cleanup
    "x s" 'sort-lines
    "x u" 'upcase-region
    "x l" 'downcase-region
    "x c" 'capitalize-region
    "x i" 'indent-region                  ; indent (moved from I)
    "x r" 'rectangle-mark-mode            ; rectangle (moved from R r)
    "x R k" 'kill-rectangle               ; rectangle operations
    "x R y" 'yank-rectangle
    "x R o" 'open-rectangle
    "x R c" 'clear-rectangle
    "x R s" 'string-rectangle

    ;; ── k: Bookmarks / Calculator ─────────────────────────────
    "k s" 'bookmark-set
    "k j" 'bookmark-jump
    "k d" 'bookmark-delete
    "k l" 'bookmark-bmenu-list
    "k k" 'calc
    "k q" 'quick-calc
    "k r" 'calc-grab-region

    ;; ── i: Snippets (yasnippet) ───────────────────────────────
    "i s" 'yas-insert-snippet
    "i n" 'yas-new-snippet
    "i v" 'yas-visit-snippet-file

    ;; ── TAB: Workspaces (perspective) ─────────────────────────
    "TAB TAB" 'persp-switch
    "TAB n" 'persp-next
    "TAB p" 'persp-prev
    "TAB c" 'persp-kill
    "TAB r" 'persp-rename
    "TAB 1" '(lambda () (interactive) (my/persp-switch-by-number 1))
    "TAB 2" '(lambda () (interactive) (my/persp-switch-by-number 2))
    "TAB 3" '(lambda () (interactive) (my/persp-switch-by-number 3))
    "TAB 4" '(lambda () (interactive) (my/persp-switch-by-number 4))
    "TAB 5" '(lambda () (interactive) (my/persp-switch-by-number 5))
    "TAB 6" '(lambda () (interactive) (my/persp-switch-by-number 6))
    "TAB 7" '(lambda () (interactive) (my/persp-switch-by-number 7))
    "TAB 8" '(lambda () (interactive) (my/persp-switch-by-number 8))
    "TAB 9" '(lambda () (interactive) (my/persp-switch-by-number 9))

    ;; ── h: Help / Diagnostics / Upgrade ─────────────────────────
    ;; Emacs built-in help
    "h f" 'describe-function
    "h v" 'describe-variable
    "h k" 'describe-key
    "h m" 'describe-mode
    "h b" 'describe-bindings
    "h a" 'apropos
    "h i" 'info
    "h h" 'helpful-at-point
    "h p" 'describe-package
    "h t" 'consult-theme
    "h w" 'where-is
    ;; Memory & diagnostics
    "h g" 'garbage-collect
    "h M" 'memory-report
    "h W" 'my/optimize-for-low-memory
    "h d" 'my/doctor
    "h e" 'my/show-error-summary
    "h r" 'my/recover-from-errors
    ;; Doom-style upgrade system (see utilities.el)
    "h S" 'my/sync                                ; freeze + byte-compile
    "h U" 'my/upgrade                             ; full upgrade with progress
    "h C" 'my/upgrade-check                       ; fetch-only dry run

    ;; ── a: Align / Advanced editing ───────────────────────────
    "a l" 'align-regexp
    "a r" 'align-region
    "a c" 'align-current
    "a e" 'er/expand-region
    "a i" 'aggressive-indent-mode
    "a s" 'highlight-symbol-at-point
    "a n" 'hi-lock-find-patterns
    "a p" 'unhighlight-regexp

    ;; ── @: Registers ──────────────────────────────────────────
    "@ r" 'copy-to-register
    "@ i" 'insert-register
    "@ j" 'jump-to-register
    "@ w" 'window-configuration-to-register
    "@ f" 'frameset-to-register

    ;; ── z: Spelling ───────────────────────────────────────────
    "z z" 'ispell-word
    "z b" 'ispell-buffer
    "z r" 'ispell-region
    "z d" 'ispell-change-dictionary
    "z n" 'flyspell-goto-next-error
    "z p" 'flyspell-check-previous-highlighted-word

    ;; ── y: System utilities ───────────────────────────────────
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

    ;; ── D: Diff / Comparison ──────────────────────────────────
    "D d" 'diff
    "D b" 'diff-buffer-with-file
    "D e" 'ediff
    "D f" 'ediff-files
    "D B" 'ediff-buffers
    "D w" 'compare-windows

    ;; ── S: Search analysis ────────────────────────────────────
    "S o" 'occur
    "S m" 'multi-occur
    "S f" 'flush-lines
    "S k" 'keep-lines
    "S h" 'how-many
    "S w" 'what-line
    "S g" 'goto-char
    "S u" 'browse-url
    "S p" 'browse-url-at-point
    "S F" 'ffap))

;; ══════════════════════════════════════════════════════════════════
;;  Evil Extension Packages
;; ══════════════════════════════════════════════════════════════════

;; evil-surround — Vim-surround port: cs"' changes surrounding " to ',
;; ds" deletes surrounding ", ysiw" wraps inner word in ".
(use-package evil-surround
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (global-evil-surround-mode 1))

;; evil-commentary — gc{motion} to toggle comments (like tpope/vim-commentary).
;; gcc comments current line, gc in visual mode comments selection.
(use-package evil-commentary
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (evil-commentary-mode))

;; evil-numbers — increment/decrement numbers at point.
;; C-c + increments, C-c - decrements (works on decimal, hex, binary).
(use-package evil-numbers
  :straight t
  :after evil
  :defer my/defer-fast
  :config
  (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt))

;; ══════════════════════════════════════════════════════════════════
;;  Magit — loaded on demand via SPC g bindings
;; ══════════════════════════════════════════════════════════════════
;; Magit is a complete Git porcelain.  `:defer t` with `:commands`
;; means it only loads when you first invoke a magit command (e.g.
;; SPC g s for magit-status).  This saves ~0.5s at startup since
;; magit is one of the largest Emacs packages.

(use-package magit
  :straight t
  :defer t
  :commands (magit-status magit-blame magit-log magit-file-dispatch
             magit-commit magit-push magit-pull))

;; ══════════════════════════════════════════════════════════════════
;;  Nov-mode (EPUB Reader) — Evil keybindings
;; ══════════════════════════════════════════════════════════════════
;; nov.el renders EPUB files but has no Evil support by default.
;; These bindings provide a Vim-like reading experience:
;;   n/p = next/prev chapter, j/k = scroll, / = search,
;;   +/- = font size, m = bookmarks, q = quit.
;; `with-eval-after-load` ensures these only run when nov.el loads.

(with-eval-after-load 'nov
  (evil-set-initial-state 'nov-mode 'normal)

  ;; Normal state
  (evil-define-key 'normal nov-mode-map
    ;; Chapter navigation - use C-n/C-p to avoid shadowing Evil paste (p/P)
    (kbd "C-n") 'nov-next-document
    (kbd "C-p") 'nov-previous-document
    (kbd "] c") 'nov-next-document     ; bracket navigation (]c = next chapter)
    (kbd "[ c") 'nov-previous-document  ; [c = previous chapter

    ;; Scrolling
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "C-d") 'evil-scroll-down
    (kbd "C-u") 'evil-scroll-up
    (kbd "C-f") 'evil-scroll-page-down
    (kbd "C-b") 'evil-scroll-page-up
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer

    ;; Jump to chapter / TOC
    (kbd "g c") 'nov-goto-document
    (kbd "g t") 'nov-display-metadata

    ;; Links
    (kbd "RET") 'nov-browse-url
    (kbd "TAB") 'shr-next-link
    (kbd "S-TAB") 'shr-previous-link
    (kbd "<backtab>") 'shr-previous-link

    ;; Font size
    (kbd "+") 'text-scale-increase
    (kbd "-") 'text-scale-decrease
    (kbd "=") 'text-scale-adjust
    (kbd "0") (lambda () (interactive) (text-scale-set 0))

    ;; Text width
    (kbd "w +") 'visual-fill-column-adjust
    (kbd "w -") (lambda () (interactive)
                  (setq-local visual-fill-column-width
                             (max 40 (- visual-fill-column-width 5)))
                  (visual-fill-column-adjust))
    (kbd "w =") (lambda () (interactive)
                  (setq-local visual-fill-column-width 80)
                  (visual-fill-column-adjust))

    ;; Bookmarks
    (kbd "m m") 'bookmark-set
    (kbd "m l") 'bookmark-bmenu-list
    (kbd "' '") 'bookmark-jump

    ;; Search
    (kbd "/") 'isearch-forward
    (kbd "*") 'isearch-forward-symbol-at-point
    (kbd "#") 'isearch-backward-symbol-at-point

    ;; Misc
    (kbd "R") 'nov-render-document
    (kbd "g r") 'nov-render-document
    (kbd "y y") 'kill-ring-save
    (kbd "q") 'quit-window
    (kbd "Z Z") 'quit-window
    (kbd "Z Q") 'kill-current-buffer
    (kbd "H") 'nov-display-metadata)

  ;; Visual state
  (evil-define-key 'visual nov-mode-map
    (kbd "y") 'kill-ring-save
    (kbd "d") 'delete-region)

  ;; Motion state
  (evil-define-key 'motion nov-mode-map
    (kbd "j") 'evil-next-visual-line
    (kbd "k") 'evil-previous-visual-line
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "C-d") 'evil-scroll-down
    (kbd "C-u") 'evil-scroll-up))

(provide 'evil-config)
;;; evil-config.el ends here
