;;; modules/enhanced-colors.el --- Enhanced color coding and syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;; Color and face customizations loaded in Phase 2 (medium defer at 1s).
;; Layers custom colors on top of the doom-one theme for better code
;; readability and visual distinction.
;;
;; Provides:
;;   Rainbow delimiters  — color-code nested parens/brackets by depth
;;   Font-lock faces     — custom keyword/comment/string/type colors
;;   Highlight-numbers   — distinct color for numeric literals
;;   Diff/magit colors   — green=added, red=removed, yellow=changed
;;   Rainbow-mode        — inline color swatches in CSS/HTML (#ff0000)
;;   Search highlighting — visible isearch/lazy-highlight faces
;;   Bracket matching    — show-paren and smartparens match faces
;;   Theme hook          — auto-adjust hl-line for light/dark themes
;;
;; All hex colors are from the One Dark palette (doom-one):
;;   #e06c75=red  #61afef=blue  #98c379=green  #e5c07b=yellow
;;   #c678dd=purple  #56b6c2=cyan  #d19a66=orange  #282c34=bg

;;; Code:

(require 'utilities)  ; For defer timing constants

;; Silence byte-compiler warnings for mode variables and package vars
(defvar rainbow-delimiters-mode)
(defvar rainbow-x-colors-major-mode-list)

;; ══════════════════════════════════════════════════════════════════
;;  1. Rainbow Delimiters
;; ══════════════════════════════════════════════════════════════════
;; Assign a unique color to each nesting level of parentheses,
;; brackets, and braces.  Essential for Lisp and deeply nested code.
;; Hooks into prog-mode so it activates in all programming buffers.

(use-package rainbow-delimiters
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Enhanced rainbow colors with better contrast
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#e06c75"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#56b6c2"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#c678dd"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#e5c07b"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#98c379"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#d19a66"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#61afef"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#be5046"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#528bff"))))))

;; ══════════════════════════════════════════════════════════════════
;;  2. Font-Lock Face Overrides
;; ══════════════════════════════════════════════════════════════════
;; Override Emacs' built-in font-lock faces with One Dark palette
;; colors.  `font-lock-maximum-decoration t` enables the most
;; detailed level of syntax highlighting (all font-lock keywords).

(use-package font-lock
  :straight nil
  :config
  ;; font-lock-maximum-decoration is set in modern-ui.el
  (global-font-lock-mode 1)

  ;; Custom font-lock faces for better code readability
  (custom-set-faces
   ;; Comments
   '(font-lock-comment-face ((t (:foreground "#5c6370" :slant italic))))
   '(font-lock-doc-face ((t (:foreground "#98c379" :slant italic))))
   
   ;; Keywords
   '(font-lock-keyword-face ((t (:foreground "#c678dd" :weight bold))))
   '(font-lock-builtin-face ((t (:foreground "#e06c75"))))
   
   ;; Types and constants
   '(font-lock-type-face ((t (:foreground "#e5c07b" :weight semi-bold))))
   '(font-lock-constant-face ((t (:foreground "#d19a66"))))
   
   ;; Functions and variables
   '(font-lock-function-name-face ((t (:foreground "#61afef" :weight bold))))
   '(font-lock-variable-name-face ((t (:foreground "#e06c75"))))
   
   ;; Strings and literals
   '(font-lock-string-face ((t (:foreground "#98c379"))))
   '(font-lock-regexp-grouping-backslash ((t (:foreground "#c678dd" :weight bold))))
   '(font-lock-regexp-grouping-construct ((t (:foreground "#e5c07b" :weight bold))))
   
   ;; Preprocessor
   '(font-lock-preprocessor-face ((t (:foreground "#828997"))))
   
   ;; Warning and error
   '(font-lock-warning-face ((t (:foreground "#ff6c6b" :weight bold :underline t))))))


;; ══════════════════════════════════════════════════════════════════
;;  3. Line & Region Highlighting
;; ══════════════════════════════════════════════════════════════════
;; hl-line-mode is enabled in core-ui.el; the face color is set here
;; so all color definitions stay in one file.
(custom-set-faces
 '(hl-line ((t (:background "#2c323c" :extend t)))))

;; ══════════════════════════════════════════════════════════════════
;;  4. Numeric Literal Highlighting
;; ══════════════════════════════════════════════════════════════════
;; highlight-numbers — gives numeric literals (42, 0xFF, 3.14) a
;; distinct color so they stand out from identifiers.

(use-package highlight-numbers
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . highlight-numbers-mode)
  :config
  ;; Custom face for numbers
  (custom-set-faces
   '(highlight-numbers-number ((t (:foreground "#da8548" :weight normal))))))

;; ══════════════════════════════════════════════════════════════════
;;  5. Diff & Magit Colors
;; ══════════════════════════════════════════════════════════════════
;; Custom diff faces with subtle tinted backgrounds for better
;; readability.  Both built-in `diff-mode` and `magit-diff` faces
;; are set so diffs look consistent everywhere.

(custom-set-faces
 ;; Diff colors
 '(diff-added ((t (:background "#1a3d2e" :foreground "#98c379"))))
 '(diff-removed ((t (:background "#3d1a1a" :foreground "#e06c75"))))
 '(diff-changed ((t (:background "#3d3a1a" :foreground "#e5c07b"))))
 '(diff-header ((t (:background "#282c34" :foreground "#61afef" :weight bold))))
 '(diff-file-header ((t (:background "#21252b" :foreground "#c678dd" :weight bold))))
 '(diff-hunk-header ((t (:background "#2c323c" :foreground "#56b6c2"))))
 
 ;; Magit-specific diff colors (if using magit)
 '(magit-diff-added ((t (:background "#1a3d2e" :foreground "#98c379"))))
 '(magit-diff-removed ((t (:background "#3d1a1a" :foreground "#e06c75"))))
 '(magit-diff-added-highlight ((t (:background "#2a5d3e" :foreground "#98c379"))))
 '(magit-diff-removed-highlight ((t (:background "#5d2a2a" :foreground "#e06c75"))))
 '(magit-diff-context-highlight ((t (:background "#2c323c"))))
 '(magit-diff-hunk-heading ((t (:background "#282c34" :foreground "#56b6c2"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#3c424c" :foreground "#56b6c2")))))

;; ══════════════════════════════════════════════════════════════════
;;  6. Rainbow Mode (Inline Color Previews)
;; ══════════════════════════════════════════════════════════════════
;; rainbow-mode — shows color values (#ff0000, rgb(…), etc.) with
;; a background swatch of that color.  Only enabled in web-related
;; modes where color values are common.

(use-package rainbow-mode
  :straight t
  :defer my/defer-medium
  :hook ((css-mode . rainbow-mode)
         (css-ts-mode . rainbow-mode)
         (scss-mode . rainbow-mode)
         (html-mode . rainbow-mode)
         (html-ts-mode . rainbow-mode)
         (web-mode . rainbow-mode))
  :config
  (setq rainbow-x-colors-major-mode-list
        '(css-mode css-ts-mode scss-mode html-mode html-ts-mode web-mode)))

;; ══════════════════════════════════════════════════════════════════
;;  7. Language-Specific Enhancements
;; ══════════════════════════════════════════════════════════════════
;; Extra font-lock rules for languages where the defaults miss some
;; syntax.  Adds highlighting for `self`, decorators (`@foo`), and
;; boolean constants (True/False/None) in Python.
(defun my/enhance-python-colors ()
  "Enhance Python syntax highlighting."
  (font-lock-add-keywords
   nil
   '(("\\bself\\b" . font-lock-variable-name-face)
     ("\\b\\(def\\|class\\|import\\|from\\|as\\)\\b" . font-lock-keyword-face)
     ("\\b\\(True\\|False\\|None\\)\\b" . font-lock-constant-face)
     ("\\(@\\w+\\)" . font-lock-preprocessor-face))))

;; Apply language-specific enhancements
(add-hook 'python-mode-hook #'my/enhance-python-colors)
(add-hook 'python-ts-mode-hook #'my/enhance-python-colors)

;; ══════════════════════════════════════════════════════════════════
;;  8. Bracket & Paren Match Faces
;; ══════════════════════════════════════════════════════════════════
;; Custom faces for matching bracket pairs.  Both smartparens
;; (sp-show-pair) and built-in show-paren-mode get the same colors
;; so bracket highlighting is consistent regardless of which mode
;; is active.  Mismatches use a red background to stand out.

(with-eval-after-load 'smartparens
  (custom-set-faces
   '(sp-show-pair-match-face ((t (:background "#3e4451" :foreground "#61afef" :weight bold))))
   '(sp-show-pair-mismatch-face ((t (:background "#be5046" :foreground "#ffffff" :weight bold))))))

;; Built-in show-paren-mode faces (configured in core-ui.el)
(custom-set-faces
 '(show-paren-match ((t (:background "#3e4451" :foreground "#61afef" :weight bold))))
 '(show-paren-mismatch ((t (:background "#be5046" :foreground "#ffffff" :weight bold))))
 '(show-paren-match-expression ((t (:background "#2c323c" :weight normal)))))

;; ══════════════════════════════════════════════════════════════════
;;  9. Selection & Search Faces
;; ══════════════════════════════════════════════════════════════════
;; Region (visual selection) uses a subtle gray background without
;; changing foreground color — this keeps syntax highlighting visible
;; even while text is selected.

(custom-set-faces
 '(region ((t (:background "#3e4451" :foreground nil))))
 '(secondary-selection ((t (:background "#2c323c")))))

;; Isearch — the active match is highlighted with a bold blue
;; background; other matches get a subtle gray (lazy-highlight).
;; Query-replace uses purple to distinguish from regular search.

(custom-set-faces
 ;; Isearch faces
 '(isearch ((t (:background "#61afef" :foreground "#282c34" :weight bold))))
 '(lazy-highlight ((t (:background "#3e4451" :foreground "#e5c07b"))))
 
 ;; Query replace
 '(query-replace ((t (:background "#c678dd" :foreground "#282c34"))))
 
 ;; Match faces
 '(match ((t (:background "#e5c07b" :foreground "#282c34" :weight bold)))))

;; ══════════════════════════════════════════════════════════════════
;;  10. Compilation Buffer Colors
;; ══════════════════════════════════════════════════════════════════
;; Distinct colors for errors (red), warnings (yellow), and info
;; (blue) in *compilation* buffers.

(custom-set-faces
 '(compilation-error ((t (:foreground "#e06c75" :weight bold))))
 '(compilation-warning ((t (:foreground "#e5c07b" :weight bold))))
 '(compilation-info ((t (:foreground "#61afef" :weight bold))))
 '(compilation-line-number ((t (:foreground "#5c6370"))))
 '(compilation-column-number ((t (:foreground "#828997")))))

;; ══════════════════════════════════════════════════════════════════
;;  11. Toggle & Utility Functions
;; ══════════════════════════════════════════════════════════════════
;; Interactive commands for enabling/disabling color modes.
;; Accessible via SPC t keybindings (see evil-config.el).

(defun my/toggle-color-identifiers ()
  "Toggle color-identifiers-mode (removed - placeholder for compatibility)."
  (interactive)
  (message "⚠️ color-identifiers-mode has been removed for performance reasons"))

(defun my/toggle-rainbow-delimiters ()
  "Toggle rainbow-delimiters-mode."
  (interactive)
  (rainbow-delimiters-mode 'toggle)
  (message "Rainbow delimiters: %s" 
           (if rainbow-delimiters-mode "enabled" "disabled")))

(defun my/reset-color-enhancements ()
  "Reset all color enhancement modes."
  (interactive)
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode -1))
  (when (fboundp 'rainbow-mode)
    (rainbow-mode -1))
  (message "All color enhancements reset"))

(defun my/enable-all-color-enhancements ()
  "Enable all color enhancement modes for current buffer."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (when (fboundp 'rainbow-delimiters-mode)
      (rainbow-delimiters-mode 1))
    (when (fboundp 'highlight-numbers-mode)
      (highlight-numbers-mode 1))
    (when (fboundp 'highlight-operators-mode)
      (highlight-operators-mode 1))
    (when (fboundp 'hes-mode)
      (hes-mode 1))
    (when (fboundp 'smartparens-mode)
      (smartparens-mode 1))
    (message "All color enhancements enabled")))

;; ══════════════════════════════════════════════════════════════════
;;  12. Theme-Aware Color Adjustments
;; ══════════════════════════════════════════════════════════════════
;; Automatically adjust hl-line and other faces when the theme
;; changes (e.g., switching between doom-one dark and light).
;; Hooks into `load-theme-hook` to run after every theme switch.

(defun my/adjust-colors-for-theme ()
  "Adjust color enhancements based on current theme."
  (interactive)
  (let ((theme (car custom-enabled-themes)))
    (cond
     ((memq theme '(doom-one doom-dark+))
      ;; Dark theme adjustments
      (custom-set-faces
       '(hl-line ((t (:background "#2c323c" :extend t))))))
     
     ((memq theme '(doom-one-light doom-tomorrow-day))
      ;; Light theme adjustments
      (custom-set-faces
       '(hl-line ((t (:background "#f0f0f0" :extend t))))))
     
     (t
      ;; Default/unknown theme
      (message "Color adjustments not defined for theme: %s" theme)))))

;; Auto-adjust colors when theme changes (Emacs 29+ hook)
(add-hook 'enable-theme-functions (lambda (_theme) (my/adjust-colors-for-theme)))

(provide 'enhanced-colors)
;;; enhanced-colors.el ends here