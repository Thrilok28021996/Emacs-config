;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Modular Emacs configuration with Vertico, Tree-sitter, Eglot, and modern UI

;;; Code:

;; --- Bootstrap and Early Setup ---

;; Record startup time
(defconst emacs-start-time (current-time))

;; Error handling
(defun my/handle-startup-error (error-data)
  "Handle startup errors gracefully."
  (message "Startup error: %s" error-data)
  (with-current-buffer (get-buffer-create "*Startup Errors*")
    (goto-char (point-max))
    (insert (format "[%s] %s\n" (current-time-string) error-data))
    (display-buffer (current-buffer))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(setq straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-vc-git-force-protocol 'https
      straight-use-package-by-default t
      load-prefer-newer t)

;; Set up use-package
(straight-use-package 'use-package)

;; Essential early settings
(set-language-environment "UTF-8")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; --- Module System ---

(defvar my/modules
  '((performance)
    (ui)
    (evil)
    (completion)
    (languages +python +cpp +html +css +lsp +tree-sitter)
    (colors)
    (dashboard)
    (utilities)
    (robustness))
  "List of enabled modules with feature flags.
Each entry is (MODULE-NAME +FLAG1 +FLAG2 ...).")

(defun my/module-enabled-p (module)
  "Return non-nil if MODULE is enabled in `my/modules'."
  (assq module my/modules))

(defun my/module-flag-p (module flag)
  "Return non-nil if MODULE has FLAG enabled.
FLAG should be a symbol like +python."
  (when-let ((entry (assq module my/modules)))
    (memq flag (cdr entry))))

(defun my/lang-enabled-p (lang)
  "Return non-nil if language LANG is enabled.
LANG should be a symbol like python, cpp, html, css."
  (my/module-flag-p 'languages
                    (intern (concat "+" (symbol-name lang)))))

;; --- Platform-Specific Setup ---

;; macOS PATH setup
(when (eq system-type 'darwin)
  (when (memq window-system '(mac ns x pgtk))
    (straight-use-package 'exec-path-from-shell)
    (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH" "CONDA_PREFIX"))
    (exec-path-from-shell-initialize)))

;; --- Load Path Setup ---

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; --- Module Loading ---

(defun my/safe-require-module (module description)
  "Safely require a MODULE with DESCRIPTION."
  (condition-case err
      (progn
        (require module)
        (message "✅ %s" description)
        t)
    (error
     (message "❌ Failed to load %s: %s" module (error-message-string err))
     nil)))

;; Load modules conditionally based on my/modules
(when (my/module-enabled-p 'performance)
  (my/safe-require-module 'modern-performance "Performance optimizations"))
(my/safe-require-module 'core-ui "UI settings")
(when (my/module-enabled-p 'evil)
  (my/safe-require-module 'evil-config "Evil mode"))
(when (my/module-enabled-p 'completion)
  (my/safe-require-module 'modern-completion "Completion system"))
(when (my/module-enabled-p 'languages)
  (my/safe-require-module 'modern-languages "Language support"))
(when (my/module-enabled-p 'ui)
  (my/safe-require-module 'modern-ui "Modern UI"))
(when (my/module-enabled-p 'colors)
  (my/safe-require-module 'enhanced-colors "Syntax highlighting"))
(when (my/module-enabled-p 'dashboard)
  (my/safe-require-module 'startup-dashboard "Startup dashboard"))
(my/safe-require-module 'utilities "Utilities")
(when (my/module-enabled-p 'robustness)
  (my/safe-require-module 'robustness-enhancements "Robustness"))

;; Essential packages
(use-package which-key
  :straight t
  :defer 0.5
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25
        which-key-separator " → "))

(use-package helpful
  :straight t
  :defer 2
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

(use-package undo-tree
  :straight t
  :defer 1
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))))

(use-package diminish
  :straight t
  :defer 1)

;; Load additional configs
(load (expand-file-name "config/org-config.el" user-emacs-directory) t)
(load (expand-file-name "config/markdown.el" user-emacs-directory) t)
(load (expand-file-name "modules/debug-support.el" user-emacs-directory) t)

;; Startup hook
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %.2fs"
                     (float-time (time-subtract (current-time) emacs-start-time)))))

(provide 'init)
;;; init.el ends here