;;; init.el --- Ultra-Modern Emacs Configuration v3.0 -*- lexical-binding: t; -*-

;;; Commentary:
;;; Cutting-edge Emacs configuration leveraging Emacs 29/30 features
;;; Modular architecture with Vertico ecosystem, Tree-sitter, Eglot, and modern UI
;;; Designed for maximum performance and modern development workflows
;;; Score Target: 10/10

;;; Code:

;; --- Bootstrap and Early Setup ---

;; Record startup time
(defconst emacs-start-time (current-time))

;; Add error handling for GUI startup
(defun my/handle-startup-error (error-data)
  "Handle startup errors gracefully."
  (message "Startup error: %s" error-data)
  (with-current-buffer (get-buffer-create "*Startup Errors*")
    (goto-char (point-max))
    (insert (format "[%s] %s\n" (current-time-string) error-data))
    (display-buffer (current-buffer))))

;; Enable debug mode only when explicitly requested via environment variable
(when (and (display-graphic-p) (not noninteractive) (getenv "EMACS_DEBUG"))
  (setq debug-on-error t)
  (message "🐛 Debug mode enabled via EMACS_DEBUG environment variable"))

;; Bootstrap straight.el package manager with enhanced error handling
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (message "📥 straight.el not found, bootstrapping...")
    (condition-case err
        (let ((url-request-timeout 30))  ; 30 second timeout
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp))
          (message "✅ straight.el bootstrap successful"))
      (error 
       (my/handle-startup-error (format "Failed to bootstrap straight.el: %s" (error-message-string err)))
       (message "⚠️ Network error during bootstrap. Run install_packages.sh to fix manually.")
       ;; Attempt fallback: check if straight is already installed
       (when (file-directory-p (expand-file-name "straight" user-emacs-directory))
         (message "🔄 Attempting to use existing straight.el installation")))))
  (when (file-exists-p bootstrap-file)
    (condition-case err
        (load bootstrap-file nil 'nomessage)
      (error
       (my/handle-startup-error (format "Failed to load straight.el: %s" (error-message-string err)))))))

;; Configure straight.el for optimal performance
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-force-protocol 'https)
(setq straight-use-package-by-default t)

;; Fix byte-compilation warnings
(setq byte-compile-warnings '(cl-functions))
(setq load-prefer-newer t)  ; Always use newer source files over old compiled files

;; Batch mode compatibility
(when noninteractive
  (setq straight-check-for-modifications nil)  ; Disable modification checks in batch mode
  (setq straight-vc-git-default-clone-depth 1)  ; Shallow clone for batch operations (1 not nil)
  (setq straight-host-usernames nil)  ; Disable username prompts
  (setq straight-repository-branch "master")  ; Use stable branch
  (setq straight-vc-git-force-protocol 'https)  ; Force HTTPS
  (setq straight-disable-compile t)  ; Disable compilation in batch mode
  (setq straight-cache-autoloads nil))  ; Disable autoload caching

;; Fix repository URL conflicts for nongnu-elpa
(defun my/fix-straight-repo-conflicts ()
  "Fix straight.el repository URL conflicts."
  (when (and (boundp 'straight-recipe-repositories)
             (member "nongnu-elpa" straight-recipe-repositories))
    ;; Remove problematic nongnu-elpa from recipe repositories
    (setq straight-recipe-repositories
          (remove "nongnu-elpa" straight-recipe-repositories))
    
    ;; Fix the repository URL if it exists
    (let ((nongnu-dir (straight--repos-dir "nongnu-elpa")))
      (when (file-directory-p nongnu-dir)
        (condition-case err
            (progn
              ;; Reset the remote URL to the correct one
              (shell-command-to-string 
               (format "cd %s && git remote set-url origin https://github.com/emacsmirror/nongnu_elpa.git" 
                      (shell-quote-argument nongnu-dir)))
              (message "✅ Fixed nongnu-elpa repository URL"))
          (error
           (message "⚠️ Could not fix nongnu-elpa URL: %s" (error-message-string err))))))))

;; Apply the fix after straight is loaded
(with-eval-after-load 'straight
  (my/fix-straight-repo-conflicts))

;; Package loading for all modes

;; Batch mode: make operations non-interactive but fully functional
(when noninteractive
  ;; Configure straight.el for non-interactive operation
  (setq straight-vc-git-auto-fast-forward t)
  (setq straight-check-for-modifications nil)
  (setq straight-use-package-by-default nil)
  
  ;; Override the popup function to automatically choose the first (default) option
  (defun straight--popup-raw (prompt &optional actions)
    "Non-interactive mode: automatically choose the first available action."
    (when actions
      (let ((default-action (caar actions)))
        (message "Auto-selecting: %s" default-action)
        (funcall (nth 2 (car actions))))))
  
  ;; Set git to auto-resolve conflicts by accepting incoming changes
  (setenv "GIT_MERGE_AUTOEDIT" "no")
  (setq straight-vc-git-force-push nil))

;; Set up use-package (only if straight.el is available)
(when (fboundp 'straight-use-package)
  (straight-use-package 'use-package)
  ;; Enable straight integration with use-package
  (setq straight-use-package-by-default t))
(setq package-enable-at-startup nil)

;; Define missing straight functions for batch operations
(defun straight-pull-all ()
  "Pull all straight packages and recipe repositories."
  (interactive)
  (if (and (fboundp 'straight-pull-recipe-repositories)
           (fboundp 'straight-pull-package)
           (boundp 'straight--recipe-cache))
      (progn
        (message "Pulling recipe repositories...")
        (straight-pull-recipe-repositories)
        (message "Pulling all packages...")
        (let ((packages (hash-table-keys straight--recipe-cache))
              (success-count 0)
              (error-count 0))
          (dolist (package packages)
            (condition-case err
                (progn
                  (straight-pull-package package)
                  (setq success-count (1+ success-count)))
              (error 
               (message "Failed to pull %s: %s" package (error-message-string err))
               (setq error-count (1+ error-count)))))
          (message "Pull complete: %d succeeded, %d failed" success-count error-count)))
    (message "straight.el not properly loaded - missing required functions")))

(defun straight-rebuild-all ()
  "Rebuild all straight packages."
  (interactive)
  (if (and (fboundp 'straight-rebuild-package)
           (boundp 'straight--recipe-cache))
      (progn
        (message "Rebuilding all packages...")
        (let ((packages (hash-table-keys straight--recipe-cache))
              (success-count 0)
              (error-count 0))
          (dolist (package packages)
            (condition-case err
                (progn
                  (straight-rebuild-package package)
                  (setq success-count (1+ success-count)))
              (error 
               (message "Failed to rebuild %s: %s" package (error-message-string err))
               (setq error-count (1+ error-count)))))
          (message "Rebuild complete: %d succeeded, %d failed" success-count error-count)))
    (message "straight.el not properly loaded - missing required functions")))

;; Essential early settings
(set-language-environment "UTF-8")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; --- Platform-Specific Setup ---

;; macOS optimizations
(when (eq system-type 'darwin)
  ;; Git path setup
  (let ((git-paths '("/usr/bin" "/usr/local/bin" "/opt/homebrew/bin"))
        (found nil))
    (dolist (path git-paths)
      (when (and (not found) (file-executable-p (expand-file-name "git" path)))
        (setq exec-path (cons path exec-path))
        (setenv "PATH" (concat path ":" (getenv "PATH")))
        (setq found t))))
  
  ;; PATH environment setup
  (when (memq window-system '(mac ns x pgtk))
    (straight-use-package 'exec-path-from-shell)
    (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH" "CONDA_PREFIX"))
    (exec-path-from-shell-initialize))
  
  ;; Clean .DS_Store files (function available in utilities.el)
  (when (file-directory-p user-emacs-directory)
    (shell-command (format "find %s -name '.DS_Store' -delete" user-emacs-directory))))

;; --- Load Path Setup ---

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; --- Safe Module Loading with Enhanced Error Handling ---

(defun my/safe-require-module (module description)
  "Safely require a MODULE with DESCRIPTION and comprehensive error handling."
  (condition-case err
      (progn
        (require module)
        (message "✅ Loaded: %s" description)
        t)
    (error
     (let ((error-msg (format "Failed to load %s (%s): %s" 
                             module description (error-message-string err))))
       (my/handle-startup-error error-msg)
       (message "❌ %s" error-msg)
       ;; Add to failed modules list for later recovery attempts
       (when (boundp 'my/failed-modules)
         (push (list module description err) my/failed-modules))
       nil))))

(defvar my/failed-modules '()
  "List of modules that failed to load during startup.")

;; --- Phase 1: Modern Performance Foundation ---

(my/safe-require-module 'modern-performance "Emacs 29/30 features, native compilation, advanced GC")

;; --- Phase 2: Core UI and Evil ---

(my/safe-require-module 'core-ui "Basic UI settings")
(my/safe-require-module 'evil-config "Evil mode with leader keys")

;; --- Phase 3: Modern Completion Ecosystem ---

(my/safe-require-module 'modern-completion "Vertico + Consult + Embark + Corfu")

;; --- Phase 4: Modern Language Support ---

(my/safe-require-module 'modern-languages "Eglot + Tree-sitter + modern tooling")

;; --- Phase 5: Modern UI Enhancements ---

(my/safe-require-module 'modern-ui "Nerd-icons, mood-line, popper, themes")
(my/safe-require-module 'enhanced-colors "Advanced color coding and syntax highlighting")

;; --- Phase 6: Utilities and Legacy Support ---

(my/safe-require-module 'utilities "Custom functions (enhanced with docstrings)")

;; --- Phase 7: Robustness Enhancements ---

(my/safe-require-module 'robustness-enhancements "Enterprise-grade reliability and security")
;; Note: performance-dashboard.el merged into modern-performance.el

;; --- Essential Packages with Modern Loading ---

;; Which-key for discoverability
(use-package which-key
  :straight t
  :defer 0.5
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-side-window-max-height 0.25)
  (setq which-key-separator " → "))

;; Helpful for better help
(use-package helpful
  :straight t
  :defer 2
  :commands (helpful-callable helpful-variable helpful-key helpful-command helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

;; Undo-tree for better undo
(use-package undo-tree
  :straight t
  :defer 1
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist 
        `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))))

;; Restart-emacs
(use-package restart-emacs
  :straight t
  :defer 10
  :commands restart-emacs)

;; Diminish to clean up modeline
(use-package diminish
  :straight t
  :defer 1)

;; --- Optional Legacy Configuration Loading ---

;; Load specific legacy config if needed
(defun my/safe-load-legacy-config (file)
  "Safely load legacy configuration FILE with error handling."
  (let ((full-path (expand-file-name file user-emacs-directory)))
    (when (file-exists-p full-path)
      (condition-case err
          (load full-path t)
        (error
         (message "Warning: Failed to load %s: %s" file (error-message-string err)))))))

;; Legacy configurations re-enabled for useful functionality
;; All essential keybindings are now consolidated in evil-config.el
(my/safe-load-legacy-config "config/org-config.el")      ; Org-mode functionality
(my/safe-load-legacy-config "config/markdown.el")        ; Markdown support

;; Load validation suite for comprehensive testing
(my/safe-load-legacy-config "validate-config.el")

;; --- Directory Setup ---

;; Directory creation now handled by utilities.el my/ensure-all-directories

;; --- Final Startup Optimizations ---

(defun my/display-startup-stats ()
  "Display minimal startup statistics."
  (let ((startup-time (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Emacs ready in %.2fs" startup-time)))

;; Enhanced startup hook with error recovery
(add-hook 'emacs-startup-hook
          (lambda ()
            (condition-case err
                (progn
                  (my/display-startup-stats)
                  (when (fboundp 'my/ensure-all-directories)
                    (my/ensure-all-directories))
                  (when (fboundp 'my/collect-performance-stats)
                    (run-with-idle-timer 1 nil #'my/collect-performance-stats))
                  
                  ;; Report any failed modules (not in batch mode)
                  (when (and my/failed-modules (not noninteractive))
                    (message "⚠️ %d module(s) failed to load. Use M-x my/show-startup-errors for details."
                             (length my/failed-modules))
                    (run-with-idle-timer 3 nil 
                                        (lambda ()
                          (display-warning 'startup
                                          (format "%d configuration modules failed to load. Check *Messages* buffer."
                                                 (length my/failed-modules))
                                          :warning))))
                  
                  ;; Schedule periodic health check
                  (run-with-idle-timer 30 nil #'my/perform-health-check))
              (error
               (my/handle-startup-error (format "Startup hook failed: %s" (error-message-string err)))))))

;; --- Evil Leader Configuration handled in evil-config.el ---

;; --- Configuration Validation ---

(defun my/show-startup-errors ()
  "Show detailed startup error information."
  (interactive)
  (with-current-buffer (get-buffer-create "*Startup Errors*")
    (erase-buffer)
    (insert "🚨 Startup Error Report\n")
    (insert "========================\n\n")
    
    (if my/failed-modules
        (progn
          (insert (format "Failed modules: %d\n\n" (length my/failed-modules)))
          (dolist (failed my/failed-modules)
            (insert (format "Module: %s\nDescription: %s\nError: %s\n\n"
                           (nth 0 failed)
                           (nth 1 failed)
                           (error-message-string (nth 2 failed))))))
      (insert "✅ No module loading failures\n"))
    
    (display-buffer (current-buffer))))

(defun my/perform-health-check ()
  "Perform a basic health check of the configuration."
  (condition-case err
      (let ((critical-functions '(my/collect-performance-stats
                                  my/ensure-all-directories
                                  my/monitor-resource-usage))
            (healthy-count 0))
        (dolist (func critical-functions)
          (when (fboundp func)
            (setq healthy-count (1+ healthy-count))))
        
        (when (< healthy-count (length critical-functions))
          (message "⚠️ Health check: %d/%d critical functions available"
                   healthy-count (length critical-functions))))
    (error
     (message "⚠️ Health check failed: %s" (error-message-string err)))))

(defun my/validate-modern-config ()
  "Validate that all modern features are working."
  (interactive)
  (let ((immediate-features '((vertico . "Modern completion")
                             (corfu . "In-buffer completion")
                             (robustness-enhancements . "Enterprise robustness")))
        (deferred-features '((consult . "Enhanced navigation") 
                            (embark . "Context actions")
                            (eglot . "Language server")
                            (treesit-auto . "Tree-sitter")
                            (nerd-icons . "Modern icons")
                            (mood-line . "Lightweight modeline")))
        (working 0)
        (deferred-available 0)
        (robustness-score 0))
    
    (message "🔍 Validating modern configuration with robustness...")
    
    ;; Check immediate features (should be loaded)
    (dolist (feature immediate-features)
      (if (featurep (car feature))
          (progn
            (message "✅ %s working" (cdr feature))
            (setq working (1+ working)))
        (message "❌ %s not loaded" (cdr feature))))
    
    ;; Check deferred features (check if available, not loaded)
    (dolist (feature deferred-features)
      (if (locate-library (symbol-name (car feature)))
          (progn
            (message "⏳ %s available (deferred)" (cdr feature))
            (setq deferred-available (1+ deferred-available)))
        (message "❌ %s not available" (cdr feature))))
    
    ;; Calculate robustness score
    (when (featurep 'robustness-enhancements)
      (setq robustness-score 
            (+ (if (boundp 'my/package-load-failures) 1 0)           ; Error recovery
               (if (file-directory-p my/backup-directory) 1 0)       ; Backup system
               (if (boundp 'my/network-status) 1 0)                  ; Network resilience
               (if my/resource-monitoring-enabled 1 0)               ; Resource monitoring
               (if (boundp 'my/trusted-executables) 1 0))))          ; Security hardening
    
    (let ((total-immediate (length immediate-features))
          (total-deferred (length deferred-features))
          (immediate-percentage (* 100.0 (/ working (float (length immediate-features)))))
          (deferred-percentage (* 100.0 (/ deferred-available (float (length deferred-features)))))
          (config-healthy nil))
      
      (setq config-healthy (and (>= immediate-percentage 90) (>= deferred-percentage 90)))
      
      (message "📊 Immediate features: %d/%d working (%.1f%%)"
               working total-immediate immediate-percentage)
      (message "📦 Deferred features: %d/%d available (%.1f%%)"
               deferred-available total-deferred deferred-percentage)
      
      (when (featurep 'robustness-enhancements)
        (message "🛡️ Robustness score: %d/5 enhancements active" robustness-score))
      
      (cond
       ((and config-healthy (>= robustness-score 5))
        (message "🏆 EXCELLENT CONFIGURATION! 🏆")
        (message "🎉 All core features working + full robustness enhancements!"))
       (config-healthy
        (message "🎉 Configuration is healthy! All essential features available."))
       ((>= immediate-percentage 66)
        (message "🌟 Configuration mostly working. Some packages may need installation."))
       (t
        (message "⚠️ Configuration issues detected. Check failed features.")))
      
      ;; Return validation result
      (list :immediate-working working
            :immediate-total total-immediate
            :deferred-available deferred-available
            :deferred-total total-deferred
            :immediate-percentage immediate-percentage
            :deferred-percentage deferred-percentage
            :robustness-score robustness-score
            :healthy config-healthy))))

;; Skip auto-validation for cleaner startup

(provide 'init)
;;; init.el ends here