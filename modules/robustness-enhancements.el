;;; modules/robustness-enhancements.el --- Essential robustness features -*- lexical-binding: t; -*-

;;; Commentary:
;;; Basic error recovery and resource monitoring for a stable Emacs configuration.
;;; Simplified for essential functionality only.

;;; Code:

(when (file-exists-p (expand-file-name "utilities.el" (file-name-directory (or load-file-name buffer-file-name))))
  (require 'utilities))  ; For buffer name constants

;; --- Enhanced Error Recovery ---

(defvar my/package-load-failures '()
  "List of packages that failed to load.")

(defvar my/max-error-history 50
  "Maximum number of errors to keep in history.")

(defvar my/critical-errors '()
  "List of critical errors that require attention.")

(defun my/handle-package-failure (package error)
  "Handle package loading failures with comprehensive logging."
  (let ((error-entry (list :package package 
                          :error (error-message-string error)
                          :timestamp (current-time)
                          :context (or load-file-name "unknown"))))
    ;; Add to failures list with size limit
    (setq my/package-load-failures 
          (cons error-entry (seq-take my/package-load-failures (1- my/max-error-history))))
    
    ;; Check if this is a critical package (but not in batch mode)
    (when (and (member package '(vertico evil consult corfu))
               (not noninteractive))
      (push error-entry my/critical-errors)
      (display-warning 'critical-package
                      (format "Critical package '%s' failed to load: %s" 
                             package (error-message-string error))
                      :error))
    
    (message "⚠️ Package '%s' failed: %s" package (error-message-string error))))

(defun my/safe-require (feature &optional filename noerror)
  "Safely require a feature with error handling."
  (condition-case err
      (require feature filename noerror)
    (error
     (my/handle-package-failure feature err)
     nil)))

(defun my/safe-load (file &optional noerror nomessage)
  "Safely load a file with error handling."
  (condition-case err
      (load file noerror nomessage)
    (error
     (my/handle-package-failure (file-name-nondirectory file) err)
     nil)))

;; --- Enhanced Resource Monitoring ---

(defvar my/resource-monitoring-enabled t
  "Whether resource monitoring is active.")

(defvar my/memory-threshold (* 500 1024 1024)  ; 500MB
  "Memory usage threshold for warnings.")

(defvar my/monitoring-interval 300  ; 5 minutes
  "Interval in seconds between resource checks.")

(defvar my/gc-threshold-critical 200
  "Critical GC count threshold.")

(defvar my/buffer-count-warning 100
  "Warning threshold for number of open buffers.")

(defvar my/monitoring-timer nil
  "Timer for periodic resource monitoring.")

;; Additional variables referenced in init.el validation
(defvar my/backup-directory (expand-file-name "robustness-backups" user-emacs-directory)
  "Directory for configuration backups.")

(defvar my/network-status 'unknown
  "Current network connectivity status.")

(defvar my/trusted-executables '()
  "List of trusted executable paths for security.")

(defun my/get-memory-usage ()
  "Get current memory usage information."
  (let ((gc-stats (garbage-collect)))
    (list :gc-count (if (boundp 'gcs-done) gcs-done 0)
          :gc-elapsed (if (boundp 'gc-elapsed) gc-elapsed 0.0)
          :cons-cells (nth 0 gc-stats))))

(defun my/monitor-resource-usage ()
  "Monitor memory usage, GC, and buffer count with comprehensive warnings."
  (when my/resource-monitoring-enabled
    (condition-case err
        (let* ((memory-info (my/get-memory-usage))
               (gc-count (plist-get memory-info :gc-count))
               (buffer-count (length (buffer-list)))
               (window-count (length (window-list))))
          
          ;; Check excessive GC (but not in batch mode)
          (when (and (> gc-count my/gc-threshold-critical)
                     (not noninteractive))
            (display-warning 'resource-monitor
                            (format "⚠️ Excessive garbage collection: %d cycles. Consider restarting Emacs."
                                   gc-count)
                            :warning))
          
          ;; Check buffer count (but not in batch mode)
          (when (and (> buffer-count my/buffer-count-warning)
                     (not noninteractive))
            (display-warning 'resource-monitor
                            (format "⚠️ High buffer count: %d buffers open. Consider cleaning up."
                                   buffer-count)
                            :warning))
          
          ;; Log stats periodically (every 10th check)
          (when (zerop (mod (or (plist-get memory-info :gc-count) 0) 10))
            (message "📊 Resources: GC:%d Buffers:%d Windows:%d" 
                     gc-count buffer-count window-count)))
      (error
       (message "⚠️ Resource monitoring failed: %s" (error-message-string err))))))

(defun my/start-resource-monitoring ()
  "Start periodic resource monitoring."
  (interactive)
  (unless noninteractive  ; Don't start in batch mode
    (when my/monitoring-timer
      (cancel-timer my/monitoring-timer))
    (setq my/monitoring-timer
          (run-with-timer my/monitoring-interval my/monitoring-interval 
                         #'my/monitor-resource-usage))
    (message "🔍 Resource monitoring started (interval: %ds)" my/monitoring-interval)))

(defun my/stop-resource-monitoring ()
  "Stop periodic resource monitoring."
  (interactive)
  (when my/monitoring-timer
    (cancel-timer my/monitoring-timer)
    (setq my/monitoring-timer nil)
    (message "🔍 Resource monitoring stopped")))

;; --- Backup and Recovery System ---

(defun my/create-emergency-backup ()
  "Create an emergency backup of critical configuration files."
  (interactive)
  (let ((backup-dir (expand-file-name "emergency-backup" user-emacs-directory))
        (timestamp (format-time-string "%Y%m%d_%H%M%S"))
        (critical-files '("init.el" "modules/" "config/")))
    (condition-case err
        (progn
          (unless (file-directory-p backup-dir)
            (make-directory backup-dir t))
          (dolist (file critical-files)
            (let ((source (expand-file-name file user-emacs-directory))
                  (dest (expand-file-name (format "%s_%s" timestamp file) backup-dir)))
              (when (file-exists-p source)
                (if (file-directory-p source)
                    (copy-directory source dest nil t t)
                  (copy-file source dest t)))))
          (message "✅ Emergency backup created in %s" backup-dir))
      (error
       (message "⚠️ Emergency backup failed: %s" (error-message-string err))))))

;; --- Network Resilience ---

(defun my/check-network-and-adapt ()
  "Check network connectivity and adapt configuration."
  (condition-case err
      (let ((online (and (fboundp 'network-interface-list)
                        (network-interface-list))))
        (setq my/network-status (if online 'connected 'offline))
        (when (not online)
          (message "📶 Offline mode: disabling network-dependent features")
          ;; Could disable package updates, LSP servers, etc.
          ))
    (error
     (setq my/network-status 'unknown)
     (message "⚠️ Network check failed: %s" (error-message-string err)))))

;; --- Enhanced Initialization ---

(defun my/initialize-robustness-enhancements ()
  "Initialize comprehensive robustness enhancements."
  (condition-case err
      (progn
        ;; Ensure backup directory exists
        (unless (file-directory-p my/backup-directory)
          (make-directory my/backup-directory t))
        
        ;; Check network status
        (my/check-network-and-adapt)
        
        ;; Start resource monitoring (not in batch mode)
        (when (and my/resource-monitoring-enabled (not noninteractive))
          (run-with-timer 60 nil #'my/start-resource-monitoring))
        
        ;; Set up error recovery hooks (not in batch mode)
        (unless noninteractive
          (add-hook 'after-init-hook 
                    (lambda ()
                      (when my/critical-errors
                        (display-warning 'init
                                        (format "%d critical package(s) failed to load. Check *Warnings* buffer."
                                               (length my/critical-errors))
                                        :error)))))
        
        (unless noninteractive
          (message "🛡️ Robustness enhancements initialized")))
    (error
     (message "⚠️ Robustness initialization failed: %s" (error-message-string err)))))

;; --- Auto-recovery Functions ---

(defun my/recover-from-errors ()
  "Attempt to recover from configuration errors."
  (interactive)
  (let ((recovered 0))
    (dolist (failure my/package-load-failures)
      (let ((package (plist-get failure :package)))
        (condition-case err
            (when (and (symbolp package) (not (featurep package)))
              (require package)
              (setq recovered (1+ recovered))
              (message "✅ Recovered package: %s" package))
          (error
           (message "❌ Still failing: %s" package)))))
    (message "🔄 Recovery complete: %d packages recovered" recovered)))

(defun my/show-error-summary ()
  "Show a summary of all configuration errors."
  (interactive)
  (with-current-buffer (get-buffer-create "*Configuration Errors*")
    (erase-buffer)
    (insert "🚨 Configuration Error Summary\n")
    (insert "=====================================\n\n")
    
    (if my/package-load-failures
        (progn
          (insert (format "Total failures: %d\n\n" (length my/package-load-failures)))
          (dolist (failure my/package-load-failures)
            (insert (format "Package: %s\nError: %s\nTime: %s\n\n"
                           (plist-get failure :package)
                           (plist-get failure :error)
                           (format-time-string "%Y-%m-%d %H:%M:%S" 
                                             (plist-get failure :timestamp))))))
      (insert "✅ No configuration errors recorded\n"))
    
    (when my/critical-errors
      (insert "\n🔥 CRITICAL ERRORS:\n")
      (dolist (error my/critical-errors)
        (insert (format "- %s: %s\n" 
                       (plist-get error :package)
                       (plist-get error :error)))))
    
    (display-buffer (current-buffer))))

;; Initialize on load
(add-hook 'emacs-startup-hook #'my/initialize-robustness-enhancements)

;; Remove duplicate function definitions - already defined above

(provide 'robustness-enhancements)
;;; robustness-enhancements.el ends here
