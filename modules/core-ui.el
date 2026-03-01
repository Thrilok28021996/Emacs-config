;;; modules/core-ui.el --- UI and visual configurations -*- lexical-binding: t; -*-

;;; Commentary:
;; Core UI settings loaded in Phase 1 (eager).  These run immediately
;; because they affect the first visible frame — line numbers, bracket
;; matching, toolbar removal, and frame sizing.
;;
;; Provides:
;;   Bracket matching  — show-paren-mode + electric-pair-mode
;;   Line numbers      — relative line numbers (for Vim-style motions)
;;   Frame setup       — fullscreen on macOS, no toolbar/menubar
;;   Focus mode        — distraction-free editing (SPC t F)
;;   Window split      — toggle horizontal/vertical split (SPC w t)

;;; Code:

;; Silence byte-compiler warnings for display-line-numbers variables
(defvar display-line-numbers-type)
(defvar display-line-numbers-mode)

;; ══════════════════════════════════════════════════════════════════
;;  Bracket Highlighting & Auto-Pairing
;; ══════════════════════════════════════════════════════════════════

;; show-paren-mode — highlights the matching bracket when cursor is
;; on or next to a paren/bracket/brace.  'mixed style highlights the
;; matching bracket if visible, otherwise highlights the entire
;; enclosed expression.  Zero delay = instant feedback.
(use-package paren
  :straight nil
  :init
  (show-paren-mode 1)
  :config
  (setq show-paren-delay 0                      ; instant highlight (no delay)
        show-paren-style 'mixed                  ; bracket if visible, expression if not
        show-paren-when-point-inside-paren t     ; show match even when cursor is between parens
        show-paren-when-point-in-periphery t))   ; show match when cursor is adjacent to paren

;; electric-pair-mode — auto-insert closing bracket when you type an
;; opening one: type ( and get (), type { and get {}, etc.
;; `open-newline-between-pairs` means pressing RET between {} gives
;; you a properly indented blank line between the braces.
(use-package elec-pair
  :straight nil
  :init
  (electric-pair-mode 1)
  :config
  (setq electric-pair-preserve-balance t         ; don't insert close if it would unbalance
        electric-pair-delete-adjacent-pairs t     ; delete both parens when backspacing empty ()
        electric-pair-open-newline-between-pairs t))  ; RET between {} adds indented newline

;; ══════════════════════════════════════════════════════════════════
;;  Line Numbers, Chrome, and Global UI
;; ══════════════════════════════════════════════════════════════════

;; Relative line numbers — shows the distance from cursor to each
;; line, making Vim motions like 12j or 5k easy to calculate.
;; Toggle between relative/absolute with SPC t r.
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Remove GUI chrome — we use keyboard for everything.
;; These supplement early-init.el settings (which prevent the initial
;; flash) and also apply to terminal Emacs.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)            ; use echo area instead of floating tooltips
(set-fringe-mode 10)         ; 10px fringe for breathing room (git gutter, errors)
(menu-bar-mode -1)

;; Frame sizing — start maximized on all platforms.
;; macOS also gets explicit width/height as a fallback.
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 40)))
(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Make ESC behave like C-g (quit) everywhere — more intuitive than
;; the default ESC ESC ESC behavior.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Accept 'y' or 'n' instead of typing out 'yes' or 'no'.
(setq use-short-answers t)

;; Highlight the line containing the cursor — makes it easier to
;; find your cursor in a large buffer.  Toggle with SPC t h.
(global-hl-line-mode t)

;; Window splitting — prefer vertical splits (side-by-side).
;; `split-width-threshold 0` means Emacs always splits horizontally
;; (side-by-side) when it auto-splits, which is better for widescreen.
(setq split-height-threshold nil
      split-width-threshold 0)

;; ══════════════════════════════════════════════════════════════════
;;  Window & Display Utility Functions
;; ══════════════════════════════════════════════════════════════════

;; SPC w t — rotate a 2-window layout between horizontal and vertical.
;; Useful when you want to switch between side-by-side and stacked views.
(defun my/toggle-window-split ()
  "Toggle between horizontal and vertical split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun my/toggle-relative-line-numbers ()
  "Toggle between relative and absolute line numbers."
  (interactive)
  (if (eq display-line-numbers-type 'relative)
      (setq display-line-numbers-type t)
    (setq display-line-numbers-type 'relative))
  (when display-line-numbers-mode
    (display-line-numbers-mode 1))
  (message "Line numbers: %s" 
           (if (eq display-line-numbers-type 'relative) "relative" "absolute")))

;; SPC t F — focus mode hides line numbers, git gutter, and modeline
;; for distraction-free editing.  Toggling again restores everything.
(defun my/toggle-focus-mode ()
  "Toggle focus mode for distraction-free editing."
  (interactive)
  (if (bound-and-true-p my/focus-mode-active)
      (my/exit-focus-mode)
    (my/focus-mode)))

(defvar my/focus-mode-active nil
  "Whether focus mode is currently active.")

(defun my/focus-mode ()
  "Enter focus mode for distraction-free editing."
  (interactive)
  (setq my/focus-mode-active t)
  (display-line-numbers-mode -1)
  (when (fboundp 'diff-hl-mode)
    (diff-hl-mode -1))
  (when (fboundp 'mood-line-mode)
    (mood-line-mode -1))
  (message "🎯 Focus mode activated"))

(defun my/exit-focus-mode ()
  "Exit focus mode."
  (interactive)
  (setq my/focus-mode-active nil)
  (display-line-numbers-mode 1)
  (when (fboundp 'diff-hl-mode)
    (diff-hl-mode 1))
  (when (fboundp 'mood-line-mode)
    (mood-line-mode 1))
  (message "🎯 Focus mode deactivated"))

(provide 'core-ui)
;;; core-ui.el ends here