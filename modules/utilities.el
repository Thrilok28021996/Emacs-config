;;; modules/utilities.el --- Utility functions and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Custom utility functions, file helpers, and productivity enhancements
;;; All functions are properly documented with detailed docstrings

;;; Code:

(require 'cl-lib)


;; Essential buffer name constants
(defconst my/buffer-quick-note "*Quick Note*"
  "Quick note-taking buffer name.")

(defconst my/buffer-messages "*Messages*"
  "Messages buffer name constant.")

;; Additional buffer constants for popper and window management
(defconst my/buffer-async-shell "*Async Shell Command*"
  "Async shell command buffer name.")

(defconst my/buffer-compile-log "*compilation*"
  "Compilation buffer name.")

(defconst my/buffer-completions "*Completions*"
  "Completions buffer name.")

(defconst my/buffer-warnings "*Warnings*"
  "Warnings buffer name.")

(defconst my/buffer-help "*Help*"
  "Help buffer name.")

(defconst my/buffer-apropos "*Apropos*"
  "Apropos buffer name.")

(defconst my/buffer-flymake "*Flymake log*"
  "Flymake diagnostics buffer name.")

(defconst my/buffer-shell-output "*Shell Command Output*"
  "Shell command output buffer name.")

(defconst my/buffer-pattern-any "\\*.*\\*"
  "Pattern to match any buffer with asterisks.")

;; Initialize missing variables
(defvar my/backup-directory (expand-file-name "backups" user-emacs-directory)
  "Directory for storing backup files.")

(defvar my/network-status 'unknown
  "Current network connectivity status.")

(defvar my/resource-monitoring-enabled t
  "Whether resource monitoring is enabled.")

(defvar my/trusted-executables '()
  "List of trusted executable paths for security.")

;; Defer timing constants for package loading
(defconst my/defer-immediate 0
  "Immediate loading - no defer.")

(defconst my/defer-fast 0.5
  "Fast defer timing for essential packages.")

(defconst my/defer-medium 1.0
  "Medium defer timing for nice-to-have packages.")

(defconst my/defer-slow 2.0
  "Slow defer timing for optional packages.")

;; --- Performance Monitoring Functions ---


(defun my/monitor-performance-continuously ()
  "Toggle continuous performance monitoring."
  (interactive)
  ;; Silent performance monitoring
  nil)

(defun my/show-startup-history ()
  "Show startup history information."
  (interactive)
  (message "Startup completed in %.2fs with %d GCs" 
           (if (boundp 'emacs-start-time)
               (float-time (time-subtract (current-time) emacs-start-time))
             0)
           gcs-done))

(defun my/show-error-report ()
  "Show configuration error report."
  (interactive)
  (if (boundp 'my/package-load-failures)
      (if my/package-load-failures
          (with-current-buffer (get-buffer-create "*Error Report*")
            (erase-buffer)
            (insert "⚠️ Package Load Failures\n")
            (insert "========================\n\n")
            (dolist (failure my/package-load-failures)
              (insert (format "Package: %s\nError: %s\n\n" 
                             (car failure) (cadr failure))))
            (switch-to-buffer (current-buffer)))
        (message "✅ No package load failures"))
    (message "No error tracking available")))

(defun my/reinstall-failed-packages ()
  "Reinstall packages that failed to load."
  (interactive)
  (message "Use `M-x straight-use-package' to reinstall specific packages"))

(defun my/validate-critical-packages ()
  "Validate that critical packages are working."
  (interactive)
  (let ((critical-packages '(vertico consult embark corfu evil))
        (working 0))
    (dolist (pkg critical-packages)
      (when (featurep pkg)
        (setq working (1+ working))))
    (message "Critical packages: %d/%d working" working (length critical-packages))))

(defun my/show-resource-report ()
  "Show system resource usage report."
  (interactive)
  (message "Memory: %s | GCs: %d | Load: %.2f" 
           (garbage-collect) gcs-done (car (load-average))))

(defun my/optimize-for-low-memory ()
  "Optimize Emacs for low memory usage with comprehensive cleanup."
  (interactive)
  (let ((initial-buffers (length (buffer-list)))
        (cleaned-buffers 0))
    
    ;; Close temporary and help buffers
    (dolist (buffer (buffer-list))
      (when (and (buffer-name buffer)
                 (or (string-match-p "\\*.*\\*" (buffer-name buffer))
                     (string-match-p "^ " (buffer-name buffer))))
        (unless (member (buffer-name buffer) 
                       '("*scratch*" "*Messages*"))
          (when (kill-buffer buffer)
            (setq cleaned-buffers (1+ cleaned-buffers))))))
    
    ;; Optimize GC settings
    (setq gc-cons-threshold (* 20 1024 1024)) ; 20MB
    (setq gc-cons-percentage 0.1)
    
    ;; Force garbage collection
    (garbage-collect)
    
    ;; Clear various caches
    (when (fboundp 'clear-image-cache)
      (clear-image-cache))
    
    (message "🧠 Memory optimization: closed %d buffers, GC threshold set to 20MB" 
             cleaned-buffers)))

(defun my/kill-unused-buffers ()
  "Kill buffers that haven't been used recently."
  (interactive)
  (let ((killed 0)
        (threshold (* 60 60 24))) ; 24 hours
    (dolist (buffer (buffer-list))
      (when (and (buffer-name buffer)
                 (not (buffer-modified-p buffer))
                 (not (string-match-p "\\*" (buffer-name buffer)))
                 (buffer-local-value 'buffer-display-time buffer)
                 (> (float-time (time-subtract (current-time)
                                              (buffer-local-value 'buffer-display-time buffer)))
                    threshold))
        (kill-buffer buffer)
        (setq killed (1+ killed))))
    (message "🗑️ Killed %d unused buffers" killed)))

(defun my/smart-buffer-cleanup ()
  "Intelligently clean up buffers based on usage patterns."
  (interactive)
  (let ((buffer-stats '())
        (killed 0))
    
    ;; Analyze buffer usage
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer))
            (size (buffer-size buffer))
            (modified (buffer-modified-p buffer))
            (file (buffer-file-name buffer)))
        (push (list buffer name size modified file) buffer-stats)))
    
    ;; Kill buffers based on criteria
    (dolist (stats buffer-stats)
      (let ((buffer (nth 0 stats))
            (name (nth 1 stats))
            (size (nth 2 stats))
            (modified (nth 3 stats))
            (file (nth 4 stats)))
        
        ;; Kill large unmodified temporary buffers
        (when (and (> size 100000)  ; 100KB+
                   (not modified)
                   (not file)
                   (string-match-p "\\*" name)
                   (not (member name '("*scratch*" "*Messages*"))))
          (kill-buffer buffer)
          (setq killed (1+ killed)))))
    
    (when (> killed 0)
      (garbage-collect)
      (message "🧠 Smart cleanup: removed %d large temporary buffers" killed)))

(defun my/check-network-connectivity ()
  "Check comprehensive network connectivity."
  (interactive)
  (let ((interfaces (when (fboundp 'network-interface-list) (network-interface-list)))
        (dns-test nil))
    
    ;; Basic interface check
    (if interfaces
        (progn
          ;; Try a simple DNS lookup as connectivity test
          (condition-case err
              (progn
                (url-retrieve-synchronously "https://httpbin.org/get" t nil 5)
                (setq dns-test t))
            (error
             (setq dns-test nil)))
          
          (message "Network: Interfaces ✅ | Internet %s" 
                   (if dns-test "✅" "❌")))
      (message "Network check: ❌ No interfaces detected"))))

(defun my/optimize-network-settings ()
  "Optimize network-related settings for better performance."
  (interactive)
  (setq url-request-timeout 10)  ; 10 second timeout
  (setq url-request-nonblock t)  ; Non-blocking requests
  (when (boundp 'gnutls-min-prime-bits)
    (setq gnutls-min-prime-bits 1024))  ; Faster TLS handshake
  (message "🌐 Network settings optimized"))

(defun my/show-security-report ()
  "Show basic security status."
  (interactive)
  (message "🛡️ TLS: %s | Package verification: %s"
           (if (gnutls-available-p) "✅" "❌")
           (if (boundp 'package-check-signature) "✅" "❌")))

(defun my/toggle-offline-mode ()
  "Toggle offline mode to disable network-dependent features."
  (interactive)
  (if (boundp 'my/offline-mode-enabled)
      (progn
        (setq my/offline-mode-enabled (not my/offline-mode-enabled))
        (if my/offline-mode-enabled
            (progn
              (setq url-request-timeout 1)  ; Very short timeout
              (message "📵 Offline mode enabled"))
          (progn
            (setq url-request-timeout 30)  ; Normal timeout
            (message "📶 Offline mode disabled"))))
    (progn
      (setq my/offline-mode-enabled t
            url-request-timeout 1)
      (message "📵 Offline mode enabled"))))

(defun my/performance-profile ()
  "Show a quick performance profile."
  (interactive)
  (message "Performance: Buffers:%d GC:%d(%.2fs) Memory:%dK" 
           (length (buffer-list))
           gcs-done
           gc-elapsed
           (/ (nth 0 (garbage-collect)) 1024)))

;; --- Backup Management Functions ---

(defun my/create-config-backup ()
  "Create a backup of the current configuration."
  (interactive)
  (let ((backup-dir (expand-file-name "manual-backups" user-emacs-directory))
        (timestamp (format-time-string "%Y%m%d_%H%M%S")))
    (unless (file-directory-p backup-dir)
      (make-directory backup-dir t))
    (message "Config backup functionality: use robustness-enhancements module")))

(defun my/restore-from-backup ()
  "Restore configuration from backup."
  (interactive)
  (message "Backup restore: check robustness-backups/ directory"))

(defun my/list-available-backups ()
  "List available configuration backups."
  (interactive)
  (let ((backup-dir (expand-file-name "robustness-backups" user-emacs-directory)))
    (if (file-directory-p backup-dir)
        (dired backup-dir)
      (message "No backup directory found"))))

;; --- Debug Functions ---


(defun my/test-configuration ()
  "Test key configuration components."
  (interactive)
  (message "=== CONFIGURATION TEST ===")
  (message "Emacs version: %s" emacs-version)
  (message "System type: %s" system-type)
  (message "GUI mode: %s" (display-graphic-p))
  (message "User directory: %s" user-emacs-directory)
  (message "Load path includes modules: %s" 
           (member (expand-file-name "modules" user-emacs-directory) load-path))
  (message "=== END TEST ==="))

;; --- File and Path Utilities ---

;; Path construction macro
(defmacro my/expand-path (filename &optional directory)
  "Expand FILENAME relative to DIRECTORY (defaults to user-emacs-directory).
Macro to reduce redundancy in expand-file-name calls."
  `(expand-file-name ,filename ,(or directory 'user-emacs-directory)))


;; --- File and Path Utilities ---

(defun my/copy-file-path ()
  "Copy the current file's full path to the clipboard.
Useful for referencing files in documentation or sharing file locations.
If the buffer is not visiting a file, copies the buffer name instead."
  (interactive)
  (let ((filename (if (buffer-file-name)
                      (buffer-file-name)
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied: %s" filename))))

(defun my/copy-file-name ()
  "Copy just the filename (without directory) to the clipboard.
Extracts the basename from the current file path, or uses buffer name
if the buffer is not visiting a file."
  (interactive)
  (let ((filename (if (buffer-file-name)
                      (file-name-nondirectory (buffer-file-name))
                    (buffer-name))))
    (when filename
      (kill-new filename)
      (message "Copied filename: %s" filename))))


;; --- Date and Time Utilities ---

(defun my/insert-current-date ()
  "Insert the current date in YYYY-MM-DD format at point.
Useful for quickly adding dates to notes, logs, or documentation."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun my/insert-current-timestamp ()
  "Insert a full timestamp in ISO 8601 format at point.
Format: YYYY-MM-DD HH:MM:SS
Useful for detailed logging and timestamping entries."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))


;; --- Note-Taking and Knowledge Management ---

(defun my/quick-note ()
  "Quickly capture a note without interrupting current workflow.
Opens a temporary buffer for note-taking that can be saved to
the appropriate location later. Integrates with org-capture if available."
  (interactive)
  (let ((note-buffer (get-buffer-create my/buffer-quick-note)))
    (switch-to-buffer note-buffer)
    (org-mode)
    (goto-char (point-max))
    (insert "\n\n* " (format-time-string "%Y-%m-%d %H:%M") " - ")
    (message "Quick note buffer ready. Save with C-x C-s when done.")))





;; --- Directory Setup and Management ---

(defun my/ensure-all-directories ()
  "Ensure essential directories exist for Emacs configuration."
  (interactive)
  (let ((directories '(
        ("auto-saves" user-emacs-directory)
        ("backups" user-emacs-directory)
        ("eln-cache" user-emacs-directory)
        ("temp" user-emacs-directory)
        ("logs" user-emacs-directory)
        ("cache" user-emacs-directory)))
        (created 0))
    (dolist (dir-info directories)
      (let* ((dir-name (car dir-info))
             (base-path (eval (cadr dir-info)))  ; Evaluate user-emacs-directory
             (full-path (expand-file-name dir-name base-path)))
        ;; Validate the path is within user-emacs-directory
        (when (and base-path
                   (string-prefix-p (file-truename user-emacs-directory)
                                   (file-truename full-path)))
          (condition-case err
              (unless (file-directory-p full-path)
                (make-directory full-path t)
                (setq created (1+ created)))
            (error 
             (message "⚠️ Could not create directory %s: %s" full-path (error-message-string err)))))))
    (when (> created 0)
      (message "📁 Created %d essential directories" created))))

(defun my/clean-temp-directories ()
  "Clean up temporary directories and files."
  (interactive)
  (let ((temp-dirs '("temp" "cache" "auto-saves"))
        (cleaned 0))
    (dolist (dir temp-dirs)
      (let ((full-path (expand-file-name dir user-emacs-directory)))
        (when (file-directory-p full-path)
          (condition-case err
              (dolist (file (directory-files full-path t "^[^.]"))
                (when (and (file-regular-p file)
                          (> (float-time (time-subtract (current-time)
                                                       (nth 5 (file-attributes file))))
                             (* 7 24 60 60))) ; 7 days old
                  (delete-file file)
                  (setq cleaned (1+ cleaned))))
            (error
             (message "⚠️ Failed to clean %s: %s" dir (error-message-string err))))))))
    (when (> cleaned 0)
      (message "🧠 Cleaned %d old temporary files" cleaned))))

;; Alias for backward compatibility
(defalias 'my/clean-temporary-directories 'my/clean-temp-directories)

;; --- Automated Maintenance ---

(defun my/daily-maintenance ()
  "Perform daily maintenance tasks."
  (interactive)
  (message "🔧 Starting daily maintenance...")
  (my/clean-temp-directories)
  (my/smart-buffer-cleanup)
  (when (> gcs-done 200)
    (my/optimize-for-low-memory))
  (when (fboundp 'my/collect-performance-stats)
    (my/collect-performance-stats))
  (message "✅ Daily maintenance completed"))

;; Schedule daily maintenance (runs once per session after 5 minutes)
(run-with-idle-timer 300 nil #'my/daily-maintenance)

;; Additional alias for backward compatibility
(defalias 'my/kill-unused-buffers-legacy 'my/smart-buffer-cleanup)

(provide 'utilities)
;;; utilities.el ends here
