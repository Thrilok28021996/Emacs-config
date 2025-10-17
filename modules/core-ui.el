;;; modules/core-ui.el --- UI and visual configurations -*- lexical-binding: t; -*-

;;; Commentary:
;;; Core UI settings, themes, and visual enhancements
;;; Includes line numbers, scrolling, window management

;;; Code:

;; --- Built-in Bracket Highlighting ---

;; Show matching parentheses (built-in)
(use-package paren
  :straight nil
  :init
  (show-paren-mode 1)
  :config
  (setq show-paren-delay 0)                    ;; No delay
  (setq show-paren-style 'mixed)               ;; Highlight bracket or entire expression
  (setq show-paren-when-point-inside-paren t)  ;; Show even when inside
  (setq show-paren-when-point-in-periphery t)) ;; Show when cursor is next to paren

;; Electric pair mode for auto-closing brackets (built-in)
(use-package elec-pair
  :straight nil
  :init
  (electric-pair-mode 1)
  :config
  (setq electric-pair-preserve-balance t)
  (setq electric-pair-delete-adjacent-pairs t)
  (setq electric-pair-open-newline-between-pairs t))

;; --- UI Configuration ---
;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

;; Frame setup for different systems
(when (eq system-type 'darwin)
  ;; macOS-specific frame setup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(width . 120))
  (add-to-list 'default-frame-alist '(height . 40)))

;; Non-macOS systems
(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Enable y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight current line
(global-hl-line-mode t)

;; Improved window splitting behavior
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Custom utility functions for UI
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