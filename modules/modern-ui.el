;;; modules/modern-ui.el --- Modern UI enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual polish layer loaded in Phase 4 (1s idle).
;; None of these packages are needed for the initial frame, so they
;; load in the background after startup.
;;
;; Provides:
;;   Icons       — nerd-icons + integrations (dired, corfu, completion)
;;   Modeline    — mood-line (lightweight, no external dependencies)
;;   Popups      — popper (manage ephemeral buffers like *Help*, *compile*)
;;   Window mgmt — ace-window (jump to windows by letter), winner (undo layout)
;;   Visuals     — hl-todo (highlight TODO/FIXME), whitespace display
;;   Fonts       — Fira Code 16pt with ligatures, Monaco fallback
;;   EPUB        — nov.el with visual-fill-column for comfortable reading
;;
;; The doom-one theme itself is loaded early in init.el (Phase 1) to
;; prevent a flash of un-themed UI; only the toggle lives here.

;;; Code:

(require 'utilities)

;; Silence byte-compiler warnings for image variables
(defvar image-animate-loop)
(defvar image-auto-resize)

;; ══════════════════════════════════════════════════════════════════
;;  Icons — nerd-icons (successor to all-the-icons)
;; ══════════════════════════════════════════════════════════════════
;; nerd-icons uses a single "Symbols Nerd Font Mono" font file for
;; all icons (vs. all-the-icons which needs 5+ fonts).  This makes
;; installation simpler and rendering more consistent.
;;
;; First-time setup: M-x nerd-icons-install-fonts
;; The font file is ~2MB and installs to your OS font directory.

(use-package nerd-icons
  :straight t
  :defer my/defer-fast
  :if (display-graphic-p)                      ; icons need a GUI — skip in terminal
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Show icons next to completion candidates in minibuffer (vertico)
(use-package nerd-icons-completion
  :straight t
  :defer my/defer-medium
  :after (nerd-icons marginalia)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;; Show file-type icons in dired listings
(use-package nerd-icons-dired
  :straight t
  :defer my/defer-slow
  :hook (dired-mode . nerd-icons-dired-mode))

;; Show icons in corfu (inline completion popup) margins
(use-package nerd-icons-corfu
  :straight t
  :defer my/defer-medium
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; ══════════════════════════════════════════════════════════════════
;;  Modeline — mood-line (lightweight alternative to doom-modeline)
;; ══════════════════════════════════════════════════════════════════
;; mood-line is ~200 lines of code vs doom-modeline's ~3000.  It uses
;; Fira Code glyphs for separators and shows file status, encoding,
;; line/column, and cursor position without external dependencies.

(use-package mood-line
  :straight t
  :defer my/defer-fast
  :config
  (mood-line-mode)
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code  ; use Fira Code symbols
        mood-line-show-eol-style t            ; show LF/CRLF/CR
        mood-line-show-encoding-information t  ; show UTF-8/Latin-1/etc
        mood-line-show-cursor-point t))        ; show absolute buffer position

;; ══════════════════════════════════════════════════════════════════
;;  Popup Management — popper
;; ══════════════════════════════════════════════════════════════════
;; Popper treats ephemeral buffers (*Help*, *compilation*, *Messages*)
;; as "popups" that can be toggled/cycled/dismissed with backtick keys:
;;   C-`   = toggle the most recent popup
;;   M-`   = cycle through all popups
;;   C-M-` = promote a popup to a regular window (or demote back)

(use-package popper
  :straight t
  :defer my/defer-medium
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  ;; Buffers matching these patterns are treated as dismissable popups.
  ;; String entries are regex patterns; symbol entries are major modes.
  (setq popper-reference-buffers
        `(,my/buffer-messages
          "Output\\*$"
          ,my/buffer-async-shell
          ,my/buffer-compilation
          ,my/buffer-completions
          ,my/buffer-warnings
          ,my/buffer-help
          ,my/buffer-apropos
          ,my/buffer-flymake
          ,my/buffer-shell-output
          "\\*eshell.*\\*$" eshell-mode
          "\\*shell.*\\*$"  shell-mode
          "\\*term.*\\*$"   term-mode
          "\\*vterm.*\\*$"  vterm-mode
          "\\*Python.*\\*$" inferior-python-mode
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; ══════════════════════════════════════════════════════════════════
;;  Theme Toggle (doom-one <-> doom-one-light)
;; ══════════════════════════════════════════════════════════════════

(defun my/toggle-theme ()
  "Toggle between doom-one (dark) and doom-one-light."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-one)
      (progn
        (disable-theme 'doom-one)
        (load-theme 'doom-one-light t)
        (message "Switched to light theme"))
    (progn
      (disable-theme 'doom-one-light)
      (load-theme 'doom-one t)
      (message "Switched to dark theme"))))

;; ══════════════════════════════════════════════════════════════════
;;  Window & Buffer Management
;; ══════════════════════════════════════════════════════════════════

;; ace-window — press M-o to see letter labels on each window, then
;; press that letter to jump there.  Much faster than C-x o cycling
;; when you have 3+ windows open.  Uses home-row keys for labels.
(use-package ace-window
  :straight t
  :defer my/defer-medium
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)  ; home-row labels (not numbers)
        aw-scope 'frame                          ; only label windows in current frame
        aw-background t                          ; dim non-target windows
        aw-leading-char-style 'char)             ; show single char (not path)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))))  ; large overlay char

;; winner — undo/redo window layout changes with C-c left/right.
;; Also bound to SPC w u (undo) and SPC w r (redo).
(use-package winner
  :straight nil                                  ; built-in, no download needed
  :defer my/defer-medium
  :config
  (winner-mode 1)
  (setq winner-boring-buffers-regexp my/buffer-pattern-any))  ; ignore popup buffers

;; ══════════════════════════════════════════════════════════════════
;;  Visual Enhancements
;; ══════════════════════════════════════════════════════════════════

;; hl-todo — highlight TODO/FIXME/etc keywords in source code with
;; distinct colors so they stand out at a glance.  Toggle per-buffer
;; with SPC t H.
(use-package hl-todo
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")    ; red — action needed
          ("FIXME"  . "#FF0000")    ; red — broken, must fix
          ("DEBUG"  . "#A020F0")    ; purple — temporary debug code
          ("GOTCHA" . "#FF4500")    ; orange — non-obvious caveat
          ("STUB"   . "#1E90FF")    ; blue — placeholder implementation
          ("NOTE"   . "#00CED1")    ; cyan — informational
          ("HACK"   . "#FF6347")))) ; tomato — ugly but works

;; whitespace — make invisible characters visible.  Only shows
;; trailing whitespace, tabs, and empty lines at start/end of buffer.
;; Not enabled by default — toggle with SPC t w.
(use-package whitespace
  :straight nil
  :defer my/defer-slow
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face trailing tabs empty)  ; what to visualize
        whitespace-line-column 120))                   ; long-line threshold

;; ══════════════════════════════════════════════════════════════════
;;  Frame & Font Configuration (GUI only)
;; ══════════════════════════════════════════════════════════════════

(when (display-graphic-p)
  ;; Frame chrome — redundant with early-init.el settings, but needed
  ;; here too because `default-frame-alist` is also used for NEW frames
  ;; created later (e.g. C-x 5 2).  `alpha 95` = 5% transparency.
  ;; Merge into default-frame-alist (don't clobber entries from
  ;; early-init.el and core-ui.el like fullscreen/width/height).
  (dolist (pair '((tool-bar-lines . 0)
                  (menu-bar-lines . 0)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (alpha . 95)))
    (setf (alist-get (car pair) default-frame-alist) (cdr pair)))

  ;; Title bar shows the file path (abbreviated with ~) or the buffer
  ;; name for non-file buffers, making it easy to identify windows.
  (setq frame-title-format
        '((:eval (if (buffer-file-name)
                     (abbreviate-file-name (buffer-file-name))
                   "%b"))
          " - Emacs")))

;; Font — Fira Code 16pt (height 160 = 16.0pt in Emacs's 1/10pt units).
;; Falls back to Monaco (macOS system font) if Fira Code isn't installed.
;; Both `default` and `variable-pitch` faces use the same font so prose
;; and code look consistent.  Wrapped in condition-case so a font error
;; doesn't prevent the rest of the config from loading.
(when (display-graphic-p)
  (condition-case err
      (let ((family (if (find-font (font-spec :name "Fira Code"))
                        "Fira Code"
                      "Monaco")))
        (set-face-attribute 'default nil :family family :height 160)
        (set-face-attribute 'variable-pitch nil :family family :height 160))
    (error
     (message "Font configuration error: %s" (error-message-string err)))))

;; ══════════════════════════════════════════════════════════════════
;;  Cursor & Selection
;; ══════════════════════════════════════════════════════════════════

(setq-default cursor-type 'bar)                ; thin I-beam cursor (like VS Code)
(setq cursor-in-non-selected-windows 'hollow   ; unfocused windows show outline cursor
      blink-cursor-blinks 10                   ; stop blinking after 10 cycles
      blink-cursor-interval 0.6)               ; blink every 0.6s (calmer than default 0.5)

;; X11/macOS selection integration:
;; mouse-drag-copy-region = selecting text with mouse auto-copies it
;; select-enable-primary  = kill/yank uses PRIMARY selection (middle-click paste)
(setq mouse-drag-copy-region t
      select-enable-primary t)

;; ══════════════════════════════════════════════════════════════════
;;  Rendering Performance
;; ══════════════════════════════════════════════════════════════════

;; fast-but-imprecise-scrolling — skip fontification during fast scrolling
;; inhibit-compacting-font-caches — trade memory for speed; prevents
;; GC from compacting font glyph caches (helps with nerd-icons)
;; fast-but-imprecise-scrolling is set in modern-performance.el
(setq inhibit-compacting-font-caches t)

;; Use maximum font-lock decoration (all keywords, not just basics).
;; font-lock-maximum-size nil removes the file-size limit for fontification.
(setq font-lock-maximum-decoration t)

;; Hide the "Possible completions are:" header in *Completions* buffer.
;; completions-detailed shows docstrings next to completion candidates.
(setq completion-show-help nil
      completions-detailed t)

;; Loop animated images (GIFs) and auto-resize images to fit the window.
(setq image-animate-loop t
      image-auto-resize 'fit-width)

;; ══════════════════════════════════════════════════════════════════
;;  Window Placement Rules
;; ══════════════════════════════════════════════════════════════════
;; `display-buffer-alist` controls where special buffers appear.
;; Without these rules, *Help* would split your current window; with
;; them, it opens in a right side-window at 42% width instead.
;; This gives an IDE-like layout: code center, help right, shells bottom.

(defun my/setup-modern-ui ()
  "Apply window placement rules and enable modern UI modes."
  (interactive)
  ;; pixel-scroll-precision-mode (Emacs 29+) — smooth trackpad scrolling
  ;; on macOS, like a native app.  Without it, scroll is line-by-line.
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))
  ;; context-menu-mode (Emacs 28+) — right-click context menus that
  ;; adapt to the current major mode (e.g. refactor in prog-mode).
  (when (fboundp 'context-menu-mode)
    (context-menu-mode 1))

  ;; Make `switch-to-buffer` respect display-buffer-alist rules
  ;; (otherwise programmatic buffer switches bypass the rules).
  (setq switch-to-buffer-obey-display-actions t)

  ;; Window placement rules: each entry is (PATTERN ACTIONS ALIST).
  ;; `display-buffer-reuse-window` reuses an existing window showing
  ;; the buffer; `display-buffer-in-side-window` creates a dedicated
  ;; side window that doesn't get split by further operations.
  (setq display-buffer-alist
        `(;; Help/documentation → right side (42% of frame width)
          ("\\*[Hh]elp\\*\\|\\*helpful"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . right) (window-width . 0.42))
          ;; Compilation output → bottom (30% of frame height)
          ("\\*compilation\\*\\|\\*Compile-Log\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.30))
          ;; Diagnostics → bottom (25%)
          ("\\*Flymake\\|\\*flymake"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.25))
          ;; Shells & terminals → bottom (35%, larger for interactive use)
          ("\\*e?shell\\*\\|\\*term\\*\\|\\*vterm\\*\\|\\*Shell Command"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.35))
          ;; Python REPL → bottom (35%)
          ("\\*Python\\*\\|\\*inferior-python\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.35))
          ;; Search results → bottom (30%)
          ("\\*grep\\*\\|\\*Occur\\*\\|\\*rg\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.30))
          ;; Log buffers → bottom (20%, compact since they're just logs)
          ("\\*Messages\\*\\|\\*Warnings\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.20))
          ;; Documentation popups → bottom (25%)
          ("\\*eldoc\\*\\|\\*Eldoc"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.25))
          ;; Cross-references → bottom (30%)
          ("\\*xref\\*"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom) (window-height . 0.30))
          ;; Completions → directly below the current window
          (,my/buffer-completions display-buffer-below-selected))))

(add-hook 'emacs-startup-hook #'my/setup-modern-ui)

;; ══════════════════════════════════════════════════════════════════
;;  EPUB Reader (nov.el)
;; ══════════════════════════════════════════════════════════════════
;; nov.el renders EPUB files as browsable HTML.  We pair it with
;; visual-fill-column to center text in an 80-column reading column,
;; similar to a Kindle or Calibre reader.

;; visual-fill-column — wraps text visually at a column width and
;; optionally centers it, creating a comfortable reading experience.
(use-package visual-fill-column
  :straight t
  :defer t
  :config
  (setq-default visual-fill-column-width 80   ; 80 chars wide reading column
                visual-fill-column-center-text t))  ; center it in the window

(defun my/nov-setup ()
  "Configure fonts and visual-fill-column for comfortable reading."
  ;; Slightly larger font (1.2x) for prose readability
  (face-remap-add-relative 'variable-pitch :family "Fira Code" :height 1.2)
  (visual-line-mode 1)                       ; wrap at window edge
  (when (fboundp 'visual-fill-column-mode)
    (setq-local visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))            ; center the text column

(use-package nov
  :straight t
  :defer t
  :mode ("\\.epub\\'" . nov-mode)              ; auto-open .epub files
  :hook (nov-mode . my/nov-setup)
  :config
  (setq nov-text-width t))

;; ══════════════════════════════════════════════════════════════════
;;  Ligatures (Fira Code)
;; ══════════════════════════════════════════════════════════════════
;; Ligatures combine multi-character sequences into single glyphs:
;;   ->  becomes →     =>  becomes ⇒     !=  becomes ≠
;; Requires a ligature-capable font (Fira Code) AND the ligature
;; package.  Only active in prog-mode buffers (not prose/org/etc).

(use-package ligature
  :straight t
  :defer my/defer-medium
  :config
  (ligature-set-ligatures
   'prog-mode
   '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "[]" "::"
     ":::" ":=" "!!" "!=" "!==" "-}" "--" "---" "-->" "->" "->>" "-<"
     "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_(" ".-"
     ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/==" "/>"
     "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>" "++" "+++" "+>"
     "=:=" "==" "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
     ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>"
     "<!--" "<-" "<--" "<->" "<+" "<+>" "<=" "<==" "<=>" "<=<" "<>"
     "<<" "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>" "~@" "~-" "~="
     "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

(provide 'modern-ui)
;;; modern-ui.el ends here
