;;; modules/utilities.el --- Utility functions and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;;; Custom utility functions, file helpers, and productivity enhancements
;;; All functions are properly documented with detailed docstrings

;;; Code:

(require 'cl-lib)


;; Buffer name constants for popper and window management
(defconst my/buffer-messages "*Messages*"
  "Messages buffer name constant.")

;; Additional buffer constants for popper and window management
(defconst my/buffer-async-shell "*Async Shell Command*"
  "Async shell command buffer name.")

(defconst my/buffer-compilation "*compilation*"
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

;; Variables for future robustness features
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

;; --- Memory and Performance Functions ---

(defun my/optimize-for-low-memory ()
  "Optimize Emacs for low memory usage with comprehensive cleanup."
  (interactive)
  (let ((cleaned-buffers 0))
    
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
    (setq gc-cons-threshold (* 16 1024 1024)) ; 16MB - standard for low memory
    (setq gc-cons-percentage 0.1)             ; Standard percentage
    
    ;; Force garbage collection
    (garbage-collect)
    
    ;; Clear various caches
    (when (fboundp 'clear-image-cache)
      (clear-image-cache))
    
    (message "🧠 Memory optimization: closed %d buffers, GC threshold set to 16MB"
             cleaned-buffers)))


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
      (message "🧠 Smart cleanup: removed %d large temporary buffers" killed))))




;; --- Directory Management ---

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
             (message "⚠️ Failed to clean %s: %s" dir (error-message-string err)))))))
    (when (> cleaned 0)
      (message "🧠 Cleaned %d old temporary files" cleaned))))


;; --- Maintenance Functions ---

(defun my/daily-maintenance ()
  "Perform daily maintenance tasks."
  (interactive)
  (message "🔧 Starting daily maintenance...")
  (my/clean-temp-directories)
  (my/smart-buffer-cleanup)
  (when (> gcs-done 200)
    (my/optimize-for-low-memory))
  (message "✅ Daily maintenance completed"))

(defun my/immediate-gc-optimization ()
  "Immediately optimize GC settings if excessive GC is detected."
  (interactive)
  (when (> gcs-done 200)
    (setq gc-cons-threshold (* 50 1024 1024))  ; 50MB
    (setq gc-cons-percentage 0.2)
    (my/smart-buffer-cleanup)
    (garbage-collect)
    (message "🚀 Applied immediate GC optimization due to %d GC cycles" gcs-done)))

;; Schedule daily maintenance - OPTIMIZED interval
;; Changed from 600 (10 min) to 1800 (30 min) for less interruption
(run-with-idle-timer 1800 nil #'my/daily-maintenance)

;; --- Performance Monitoring Functions ---

(defvar my/performance-stats '()
  "Performance statistics collected during runtime.")

(defun my/collect-performance-stats ()
  "Collect performance statistics for monitoring."
  (interactive)
  (let ((stats (list :timestamp (current-time)
                     :gc-count gcs-done
                     :gc-time gc-elapsed
                     :memory-usage (memory-use-counts)
                     :buffer-count (length (buffer-list)))))
    (push stats my/performance-stats)
    ;; Keep only last 100 entries
    (when (> (length my/performance-stats) 100)
      (setq my/performance-stats (seq-take my/performance-stats 100)))
    stats))

(defun my/monitor-resource-usage ()
  "Monitor and report resource usage."
  (interactive)
  (let ((buffer-count (length (buffer-list)))
        (gc-count gcs-done)
        (memory (memory-use-counts)))
    (message "📊 Buffers: %d | GC cycles: %d | Memory: %s"
             buffer-count gc-count memory)))

(defun my/show-performance-stats ()
  "Display collected performance statistics."
  (interactive)
  (if my/performance-stats
      (with-current-buffer (get-buffer-create "*Performance Stats*")
        (erase-buffer)
        (insert "📊 Performance Statistics\n")
        (insert "=========================\n\n")
        (dolist (stat my/performance-stats)
          (insert (format "Time: %s\nGC: %d cycles (%.2fs)\nBuffers: %d\n\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S" (plist-get stat :timestamp))
                         (plist-get stat :gc-count)
                         (plist-get stat :gc-time)
                         (plist-get stat :buffer-count))))
        (display-buffer (current-buffer)))
    (message "No performance statistics collected yet")))


(provide 'utilities)
;;; utilities.el ends here
