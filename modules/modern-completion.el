;;; modules/modern-completion.el --- Modern completion with Vertico ecosystem -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion system loaded in Phase 1 (fast-defer at 0.5s).
;; Uses the modern Vertico ecosystem instead of the older Ivy/Counsel/Swiper
;; or Helm stacks.  Each package has a single, focused responsibility:
;;
;; Architecture (how the pieces fit together):
;;
;;   Layer          Package      Role
;;   ─────────────  ───────────  ──────────────────────────────────
;;   Minibuffer UI  Vertico      Vertical candidate list in minibuffer
;;   Matching       Orderless    Space-separated fuzzy/regex matching
;;   Annotations    Marginalia   Rich metadata beside each candidate
;;   Commands       Consult      Enhanced search, jump, buffer switching
;;   Actions        Embark       Right-click-style context actions on any target
;;   In-buffer      Corfu        Popup completion overlay while typing
;;   Sources        Cape         Extra completion backends (dabbrev, file, etc.)
;;   History        Savehist     Persist minibuffer history across restarts
;;
;; Why this stack over Ivy/Helm?
;;   - Each package is independent — swap or disable any piece
;;   - Native completing-read API — works with every Emacs command
;;   - Faster startup — no monolithic framework to load
;;   - Orderless matching is more flexible than Ivy's regex

;;; Code:

(require 'utilities)  ; For defer timing constants

;; Silence byte-compiler warnings for deferred functions
(declare-function consult--customize-put "consult")
(declare-function vertico-mode "vertico")
(declare-function marginalia-mode "marginalia")
(declare-function consult-customize "consult")
(declare-function consult-register-format "consult")
(declare-function consult-register-window "consult")
(declare-function consult-xref "consult")
(declare-function embark-prefix-help-command "embark")
(declare-function embark-eldoc-first-target "embark")
(declare-function global-corfu-mode "corfu")
(declare-function corfu-history-mode "corfu")
(declare-function kind-icon-margin-formatter "kind-icon")
(declare-function cape-dabbrev "cape")
(declare-function cape-file "cape")
(declare-function cape-elisp-block "cape")

;; Silence byte-compiler warnings for package variables
(defvar vertico-cycle)
(defvar vertico-resize)
(defvar vertico-count)
(defvar vertico-map)
(defvar marginalia-align)
(defvar marginalia-max-relative-age)
;; consult-narrow-key is a user option, not a function
(defvar consult-narrow-key)
(defvar corfu-cycle)
(defvar corfu-auto)
(defvar corfu-auto-delay)
(defvar corfu-auto-prefix)
(defvar corfu-separator)
(defvar corfu-quit-at-boundary)
(defvar corfu-quit-no-match)
(defvar corfu-preview-current)
(defvar corfu-preselect)
(defvar corfu-on-exact-match)
(defvar corfu-scroll-margin)
(defvar corfu-count)
(defvar corfu-popupinfo-delay)
(defvar corfu-margin-formatters)
(defvar kind-icon-default-face)
(defvar kind-icon-blend-background)
(defvar kind-icon-blend-frac)
(defvar cape-dabbrev-min-length)

;; ══════════════════════════════════════════════════════════════════
;;  1. Minibuffer Completion (Vertico + Orderless + Marginalia)
;; ══════════════════════════════════════════════════════════════════

;; Vertico — shows completion candidates in a vertical list inside
;; the minibuffer.  Replaces the default *Completions* buffer.
;; `vertico-cycle` wraps around at list ends; `vertico-count 12`
;; shows 12 candidates at once (enough without being overwhelming).
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

;; vertico-directory — special keybindings for file path navigation.
;; DEL deletes one path component (like shell behavior), RET enters
;; a directory instead of completing immediately.
(use-package vertico-directory
  :straight nil
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Orderless — match candidates by typing space-separated words in
;; any order.  "buf sw" matches "switch-to-buffer".  Falls back to
;; `basic` for TRAMP paths and file completion (partial-completion).
(use-package orderless
  :straight t
  :defer my/defer-fast
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia — adds contextual info next to each candidate:
;; file sizes, docstrings, key bindings, git status, etc.
;; M-A cycles between annotation styles (light/heavy/none).
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

;; ══════════════════════════════════════════════════════════════════
;;  2. Enhanced Search & Navigation (Consult)
;; ══════════════════════════════════════════════════════════════════
;; Consult provides enhanced versions of built-in commands with
;; live preview.  Key commands:
;;   consult-buffer     — switch buffer with preview (replaces C-x b)
;;   consult-ripgrep    — project-wide grep with live results (M-s r)
;;   consult-line       — search current buffer lines (M-s l)
;;   consult-imenu      — jump to symbol in buffer (M-g i)
;;   consult-flymake    — jump to diagnostic (M-g f)
;;   consult-goto-line  — goto line with preview (M-g g)
;;   consult-yank-pop   — browse kill ring (M-y)
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
  ;; preview is now enabled automatically in newer consult versions
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
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key
  (setq consult-narrow-key "<"))

;; ══════════════════════════════════════════════════════════════════
;;  3. Context Actions (Embark)
;; ══════════════════════════════════════════════════════════════════
;; Embark — acts on the "thing at point" or minibuffer candidate.
;; Think of it as a right-click context menu for anything:
;;   C-.   on a file candidate → open, rename, delete, copy path…
;;   C-.   on a URL → browse, copy, curl…
;;   C-.   on a symbol → describe, find definition, grep…
;;   C-;   "do what I mean" — picks the most likely action
;;   C-h B shows all bindings for the current prefix
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

;; Embark-Consult glue — enables live preview in Embark collect
;; buffers (the "export" results from consult-ripgrep, etc.).
(use-package embark-consult
  :straight t
  :defer my/defer-medium
  ;; preview is now enabled automatically in newer consult versions
  )

;; ══════════════════════════════════════════════════════════════════
;;  4. In-Buffer Completion (Corfu + Cape)
;; ══════════════════════════════════════════════════════════════════
;; Corfu — popup completion overlay that appears while typing.
;; Replaces company-mode with a lighter, faster alternative that
;; uses the native `completion-at-point-functions` (capf) API.
;;
;; Key settings:
;;   corfu-auto t         — popup appears automatically after typing
;;   corfu-auto-delay 0.3 — 300ms delay before popup (avoids flicker)
;;   corfu-auto-prefix 2  — need 2 chars before popup triggers
;;   corfu-count 10       — show max 10 candidates (keeps popup small)
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

;; corfu-popupinfo — shows documentation popup beside the
;; completion candidate (like company-quickhelp).
(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; corfu-history — rank frequently-used completions higher.
(use-package corfu-history
  :straight nil
  :after corfu
  :config
  (corfu-history-mode))

;; kind-icon — shows icons (function, variable, class, etc.) in
;; the left margin of each completion candidate for visual scanning.
(use-package kind-icon
  :straight t
  :defer my/defer-medium
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default
        kind-icon-blend-background t
        kind-icon-blend-frac 0.08)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; ══════════════════════════════════════════════════════════════════
;;  5. Completion Behavior & History
;; ══════════════════════════════════════════════════════════════════
;; Global settings that affect all completion (minibuffer + in-buffer).
;; Evil-specific completion keybindings live in evil-config.el.

;; Allow opening a new minibuffer while one is already active.
;; Needed for Embark actions that prompt inside the minibuffer.
(setq enable-recursive-minibuffers t)

;; Only show commands in M-x that are relevant to the current mode.
;; Hides hundreds of irrelevant commands from the completion list.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Case-insensitive matching everywhere — "buf" matches "Buffer".
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; savehist — persist minibuffer history (M-x commands, search strings,
;; file paths) across Emacs restarts.  Also saves corfu-history so
;; frequently-used completions rank higher in future sessions.
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

;; ══════════════════════════════════════════════════════════════════
;;  6. Additional Completion Sources (Cape + Abbrev)
;; ══════════════════════════════════════════════════════════════════
;; Cape — adds extra `completion-at-point-functions` backends:
;;   cape-dabbrev    — complete from words in open buffers
;;   cape-file       — complete file paths (~/Doc…)
;;   cape-elisp-block — complete Elisp in org src blocks
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

;; abbrev — built-in text expansion (e.g., type "btw" → "by the way").
;; Only active in text-mode (prose, not code).  Abbreviations are
;; saved to ~/.emacs.d/abbrev_defs between sessions.
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