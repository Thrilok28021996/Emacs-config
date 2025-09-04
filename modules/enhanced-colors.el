;;; modules/enhanced-colors.el --- Enhanced color coding and syntax highlighting -*- lexical-binding: t; -*-

;;; Commentary:
;;; Advanced color enhancements including syntax highlighting, rainbow delimiters,
;;; semantic highlighting, and better visual distinction for code elements

;;; Code:

;; --- Rainbow Delimiters ---
;; Colorize parentheses, brackets, and braces by depth level

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

;; --- Enhanced Syntax Highlighting ---
;; Improve built-in syntax highlighting with better colors

(use-package font-lock
  :straight nil
  :config
  ;; Enable maximum font-lock decoration
  (setq font-lock-maximum-decoration t)
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


;; --- Color Identifiers ---
;; Assign unique colors to different identifiers/variables
;; TEMPORARILY DISABLED due to elisp parsing bug

;; (use-package color-identifiers-mode
;;   :straight t
;;   :defer my/defer-slow
;;   :hook (prog-mode . color-identifiers-mode)
;;   :config
;;   ;; Disable coloring of certain faces to avoid conflicts
;;   (setq color-identifiers:modes-alist
;;         '((js-mode . (:foreground))
;;           (js2-mode . (:foreground))
;;           (rjsx-mode . (:foreground))
;;           (python-mode . (:foreground))
;;           (emacs-lisp-mode . (:foreground))
;;           (lisp-mode . (:foreground))
;;           (c-mode . (:foreground))
;;           (c++-mode . (:foreground))
;;           (java-mode . (:foreground))
;;           (go-mode . (:foreground))
;;           (rust-mode . (:foreground))))
;;   
;;   ;; Fine-tune color selection
;;   (setq color-identifiers:num-colors 10)
;;   (setq color-identifiers:color-luminance 0.3)
;;   (setq color-identifiers:min-color-saturation 0.2)
;;   (setq color-identifiers:max-color-saturation 0.8))

;; --- Enhanced Line Highlighting ---
;; Improve current line highlighting with subtle colors

(use-package hl-line
  :straight nil
  :config
  (global-hl-line-mode 1)
  
  ;; Custom face for current line highlighting
  (custom-set-faces
   '(hl-line ((t (:background "#2c323c" :extend t))))))

;; --- Highlight Numbers ---
;; Syntax highlighting for numbers in code

(use-package highlight-numbers
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . highlight-numbers-mode)
  :config
  ;; Custom face for numbers
  (custom-set-faces
   '(highlight-numbers-number ((t (:foreground "#da8548" :weight normal))))))

;; --- Highlight Operators ---
;; Syntax highlighting for operators

(use-package highlight-operators
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . highlight-operators-mode)
  :config
  ;; Custom face for operators
  (custom-set-faces
   '(highlight-operators-face ((t (:foreground "#c678dd" :weight normal))))))

;; --- Highlight Escape Sequences ---
;; Better visibility for escape sequences in strings

(use-package highlight-escape-sequences
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . hes-mode)
  :config
  ;; Custom face for escape sequences
  (custom-set-faces
   '(hes-escape-backslash-face ((t (:foreground "#c678dd" :weight bold))))
   '(hes-escape-sequence-face ((t (:foreground "#e5c07b" :weight bold))))))

;; --- Rainbow Mode ---
;; Display color codes (hex, rgb) with their actual colors

(use-package rainbow-mode
  :straight t
  :defer my/defer-slow
  :hook ((css-mode . rainbow-mode)
         (scss-mode . rainbow-mode)
         (less-mode . rainbow-mode)
         (web-mode . rainbow-mode)
         (html-mode . rainbow-mode))
  :config
  ;; Configure rainbow-mode for better performance
  (setq rainbow-x-colors nil)
  (setq rainbow-r-colors nil)
  (setq rainbow-html-colors nil)
  (setq rainbow-latex-colors nil))

;; --- Better Diff Colors ---
;; Enhanced colors for version control diffs

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

;; --- Language-Specific Color Enhancements ---

;; JavaScript/TypeScript enhanced highlighting
(defun my/enhance-js-colors ()
  "Enhance JavaScript/TypeScript syntax highlighting."
  (font-lock-add-keywords
   nil
   '(("\\(console\\)\\." 1 font-lock-builtin-face)
     ("\\(\\w+\\)(" 1 font-lock-function-name-face)
     ("\\b\\(const\\|let\\|var\\)\\b" . font-lock-keyword-face)
     ("\\b\\(async\\|await\\)\\b" . font-lock-keyword-face)
     ("\\b\\(import\\|export\\|from\\|as\\)\\b" . font-lock-keyword-face))))

;; Python enhanced highlighting
(defun my/enhance-python-colors ()
  "Enhance Python syntax highlighting."
  (font-lock-add-keywords
   nil
   '(("\\bself\\b" . font-lock-variable-name-face)
     ("\\b\\(def\\|class\\|import\\|from\\|as\\)\\b" . font-lock-keyword-face)
     ("\\b\\(True\\|False\\|None\\)\\b" . font-lock-constant-face)
     ("\\(@\\w+\\)" . font-lock-preprocessor-face))))

;; Apply language-specific enhancements
(add-hook 'js-mode-hook #'my/enhance-js-colors)
(add-hook 'js2-mode-hook #'my/enhance-js-colors)
(add-hook 'typescript-mode-hook #'my/enhance-js-colors)
(add-hook 'python-mode-hook #'my/enhance-python-colors)

;; --- Bracket Pair Highlighting ---
;; Highlight matching brackets/parentheses

(use-package smartparens
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  
  ;; Custom faces for matching pairs
  (custom-set-faces
   '(sp-show-pair-match-face ((t (:background "#3e4451" :foreground "#61afef" :weight bold))))
   '(sp-show-pair-mismatch-face ((t (:background "#be5046" :foreground "#ffffff" :weight bold))))))

;; --- Region Selection Enhancement ---
;; Better colors for selected regions

(custom-set-faces
 '(region ((t (:background "#3e4451" :foreground nil))))
 '(secondary-selection ((t (:background "#2c323c")))))

;; --- Search and Match Highlighting ---
;; Enhanced colors for search results

(custom-set-faces
 ;; Isearch faces
 '(isearch ((t (:background "#61afef" :foreground "#282c34" :weight bold))))
 '(lazy-highlight ((t (:background "#3e4451" :foreground "#e5c07b"))))
 
 ;; Query replace
 '(query-replace ((t (:background "#c678dd" :foreground "#282c34"))))
 
 ;; Match faces
 '(match ((t (:background "#e5c07b" :foreground "#282c34" :weight bold)))))

;; --- Enhanced Compilation Colors ---
;; Better colors for compilation buffers

(custom-set-faces
 '(compilation-error ((t (:foreground "#e06c75" :weight bold))))
 '(compilation-warning ((t (:foreground "#e5c07b" :weight bold))))
 '(compilation-info ((t (:foreground "#61afef" :weight bold))))
 '(compilation-line-number ((t (:foreground "#5c6370"))))
 '(compilation-column-number ((t (:foreground "#828997")))))

;; --- Utility Functions ---

(defun my/toggle-color-identifiers ()
  "Toggle color-identifiers-mode."
  (interactive)
  (if (fboundp 'color-identifiers-mode)
      (progn
        (color-identifiers-mode 'toggle)
        (message "Color identifiers: %s" 
                 (if color-identifiers-mode "enabled" "disabled")))
    (message "Color identifiers mode not available")))

(defun my/toggle-rainbow-delimiters ()
  "Toggle rainbow-delimiters-mode."
  (interactive)
  (rainbow-delimiters-mode 'toggle)
  (message "Rainbow delimiters: %s" 
           (if rainbow-delimiters-mode "enabled" "disabled")))

(defun my/reset-color-enhancements ()
  "Reset all color enhancement modes."
  (interactive)
  (when (fboundp 'color-identifiers-mode)
    (color-identifiers-mode -1))
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode -1))
  (when (fboundp 'rainbow-mode)
    (rainbow-mode -1))
  (message "All color enhancements reset"))

(defun my/enable-all-color-enhancements ()
  "Enable all color enhancement modes for current buffer."
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (when (fboundp 'color-identifiers-mode)
      (color-identifiers-mode 1))
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

;; --- Color Theme Integration ---

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

;; Auto-adjust colors when theme changes
(add-hook 'load-theme-hook #'my/adjust-colors-for-theme)

(provide 'enhanced-colors)
;;; enhanced-colors.el ends here