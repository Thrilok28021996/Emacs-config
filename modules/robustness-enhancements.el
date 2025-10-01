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



;; --- Future Enhancement Areas ---
;; Backup and recovery system can be added here
;; Network resilience features can be added here


;; --- Enhanced Initialization ---

(defun my/initialize-robustness-enhancements ()
  "Initialize basic robustness enhancements."
  (condition-case err
      (progn
        ;; Ensure backup directory exists
        (let ((backup-dir (expand-file-name "backups" user-emacs-directory)))
          (unless (file-directory-p backup-dir)
            (make-directory backup-dir t)))

        ;; Resource monitoring and network status are handled by utilities.el
        ;; Basic initialization complete
        
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


(provide 'robustness-enhancements)
;;; robustness-enhancements.el ends here
