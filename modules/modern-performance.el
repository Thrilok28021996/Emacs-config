;;; modules/modern-performance.el --- Emacs 29/30 features and advanced performance -*- lexical-binding: t; -*-

;;; Commentary:
;;; Advanced performance optimizations and modern Emacs features
;;; Leverages Emacs 29/30 capabilities including native compilation, pixel-perfect scrolling,
;;; and enhanced built-in features for maximum performance

;;; Code:

;; No external dependencies required

;; --- Native Compilation Optimizations (Emacs 28+) ---

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Enable native compilation for better performance
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-async-jobs-number (max 1 (/ (num-processors) 2)))
  
  ;; Optimize native compilation for startup
  (setq native-comp-speed 2) ; Balance between compilation time and performance
  (setq native-comp-debug 0) ; Disable debug info for better performance
  
  ;; Ensure native compilation directory is added to path
  (let ((native-dir (expand-file-name "eln-cache" user-emacs-directory)))
    (add-to-list 'native-comp-eln-load-path native-dir))
  
  ;; Silent native compilation setup
  )

;; --- Advanced Garbage Collection Tuning ---

(defvar my/gc-cons-threshold-normal (* 20 1024 1024)
  "Normal GC threshold for interactive use (20MB).")

(defvar my/gc-cons-threshold-high (* 100 1024 1024)
  "High GC threshold for heavy operations (100MB).")

(defvar my/gc-cons-percentage-normal 0.1
  "Normal GC percentage for balanced performance.")

(defvar my/gc-cons-percentage-high 0.6
  "High GC percentage for heavy operations.")

(defun my/gc-optimize-for-startup ()
  "Optimize GC for startup performance with reasonable limits."
  (setq gc-cons-threshold (* 100 1024 1024)  ; 100MB instead of max value
        gc-cons-percentage my/gc-cons-percentage-high)
  (when (fboundp 'gcmh-mode)
    (gcmh-mode -1))  ; Disable GCMH if present during startup
  ;; Silent GC optimization
  )

(defun my/gc-restore-normal ()
  "Restore normal GC settings after startup."
  (setq gc-cons-threshold my/gc-cons-threshold-normal
        gc-cons-percentage my/gc-cons-percentage-normal)
  (when (fboundp 'gcmh-mode)
    (gcmh-mode 1))  ; Re-enable GCMH after startup
  (run-with-idle-timer 2 nil #'garbage-collect)  ; Clean up after startup
  ;; Silent GC restoration
  )

(defun my/gc-temporarily-increase ()
  "Temporarily increase GC threshold for heavy operations."
  (setq gc-cons-threshold my/gc-cons-threshold-high))

(defun my/gc-restore-after-operation ()
  "Restore GC threshold after heavy operation."
  (setq gc-cons-threshold my/gc-cons-threshold-normal))

;; Hook into heavy operations
(add-hook 'minibuffer-setup-hook #'my/gc-temporarily-increase)
(add-hook 'minibuffer-exit-hook #'my/gc-restore-after-operation)

;; --- Modern Scrolling and Display Optimizations (Emacs 29+) ---

(when (boundp 'pixel-scroll-precision-mode)
  ;; Enable pixel-perfect scrolling in Emacs 29+
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 40.0)
  (setq pixel-scroll-precision-interpolation-factor 30.0)
  (setq pixel-scroll-precision-use-momentum t)
  ;; Silent pixel scrolling setup
  )

;; Enhanced scrolling settings with error handling
(condition-case err
    (progn
      (setq scroll-preserve-screen-position t)
      (setq scroll-conservatively 101)
      (setq scroll-margin 0)
      (setq scroll-step 1)
      (setq auto-window-vscroll nil)
      (setq fast-but-imprecise-scrolling t)
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
      (setq mouse-wheel-progressive-speed nil))
  (error
   (message "⚠️ Some scrolling optimizations failed: %s" (error-message-string err))))

;; --- Font and Display Optimizations ---

;; Optimize font rendering
(setq-default line-spacing nil)
(setq x-underline-at-descent-line t)

;; Better frame resizing
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

;; Improve large file handling with reasonable threshold
(setq large-file-warning-threshold 25000000) ; 25MB (more reasonable than 100MB)
(setq vc-handled-backends '(Git)) ; Only use Git for version control

;; --- Advanced I/O Optimizations ---

;; Increase process output buffer size for better LSP performance
(setq read-process-output-max (* 3 1024 1024)) ; 3MB
(setq process-adaptive-read-buffering nil)      ; Disable adaptive buffering for consistent performance

;; Optimize file I/O while preserving essential safety features
(setq create-lockfiles nil        ; Don't create .# lock files (optional)
      make-backup-files t         ; Re-enable backup files for safety
      backup-by-copying t         ; Use copying instead of renaming
      backup-directory-alist      ; Store backups in dedicated directory
      `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      delete-old-versions t       ; Delete old backup versions
      version-control t          ; Use numbered backups
      kept-new-versions 5        ; Keep 5 new versions
      kept-old-versions 2        ; Keep 2 old versions
      auto-save-default t)       ; Re-enable auto-save for safety

;; Better UTF-8 performance with error handling
(condition-case err
    (progn
      (set-default-coding-systems 'utf-8)
      (setq locale-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)
      (prefer-coding-system 'utf-8)
      ;; Additional UTF-8 optimizations
      (setq-default buffer-file-coding-system 'utf-8-unix)
      (setq default-file-name-coding-system 'utf-8))
  (error
   (message "⚠️ UTF-8 setup failed: %s" (error-message-string err))))



;; --- Advanced Startup Optimizations ---

(defun my/defer-garbage-collection ()
  "Defer garbage collection during startup with reasonable limits."
  (setq gc-cons-threshold (* 100 1024 1024)))  ; 100MB instead of max value

(defun my/restore-garbage-collection ()
  "Restore garbage collection after startup."
  (run-with-idle-timer 2 nil
                      (lambda ()
                        (setq gc-cons-threshold my/gc-cons-threshold-normal)
                        (garbage-collect))))

;; Apply startup optimizations
(my/defer-garbage-collection)
(add-hook 'emacs-startup-hook #'my/restore-garbage-collection)

;; --- Enhanced File Handling ---

;; Optimize file operations
(setq confirm-kill-emacs nil) ; Don't confirm on exit
(setq confirm-kill-processes nil) ; Don't confirm killing processes

;; Basic auto-save handling (backups handled by robustness-enhancements.el)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Ensure auto-save directory exists
(let ((auto-save-dir (expand-file-name "auto-saves" user-emacs-directory)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; --- Modern Built-in Enhancements (Emacs 29+) ---

;; Modern built-in enhancements with error handling
(condition-case err
    (progn
      ;; Use built-in tab-bar if available
      (when (fboundp 'tab-bar-mode)
        (setq tab-bar-show 1)
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-new-button-show nil))

      ;; Enhanced repeat mode (Emacs 28+)
      (when (fboundp 'repeat-mode)
        (repeat-mode 1)
        (setq repeat-exit-timeout 2))

      ;; Use built-in project.el enhancements
      (when (fboundp 'project-remember-projects-under)
        (setq project-vc-merge-submodules nil)))
  (error
   (message "⚠️ Some modern feature initialization failed: %s" (error-message-string err))))

;; --- Performance Statistics Collection ---

(defvar my/performance-stats-history '()
  "History of performance statistics.")

(defvar my/performance-monitoring-enabled t
  "Whether performance monitoring is active.")

(defun my/collect-performance-stats ()
  "Collect and store performance statistics with error handling."
  (when my/performance-monitoring-enabled
    (condition-case err
        (let ((stats (list
                      :timestamp (current-time)
                      :gc-count gcs-done
                      :gc-elapsed gc-elapsed
                      :memory-usage (length (garbage-collect))  ; Just get length for efficiency
                      :uptime (if (boundp 'emacs-start-time)
                                 (float-time (time-subtract (current-time) emacs-start-time))
                               0)
                      :buffer-count (length (buffer-list))
                      :window-count (length (window-list)))))
          ;; Keep only last 50 entries for memory efficiency
          (setq my/performance-stats-history 
                (cons stats (seq-take my/performance-stats-history 49)))
          stats)
      (error
       (message "⚠️ Performance stats collection failed: %s" (error-message-string err))
       nil))))

(defun my/show-performance-stats ()
  "Show collected performance statistics."
  (interactive)
  (if my/performance-stats-history
      (let ((latest (car my/performance-stats-history)))
        (message "Performance: GC:%d(%.2fs) Uptime:%.2fs Buffers:%d Windows:%d" 
                 (plist-get latest :gc-count)
                 (plist-get latest :gc-elapsed)
                 (plist-get latest :uptime)
                 (plist-get latest :buffer-count)
                 (plist-get latest :window-count)))
    (message "No performance stats collected yet")))

(defun my/reset-performance-stats ()
  "Reset performance statistics history."
  (interactive)
  (setq my/performance-stats-history '())
  (message "Performance statistics reset"))

(defun my/toggle-performance-monitoring ()
  "Toggle performance monitoring on/off."
  (interactive)
  (setq my/performance-monitoring-enabled (not my/performance-monitoring-enabled))
  (message "Performance monitoring: %s" 
           (if my/performance-monitoring-enabled "enabled" "disabled")))

(provide 'modern-performance)
;;; modern-performance.el ends here
