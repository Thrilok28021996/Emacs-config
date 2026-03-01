;;; modules/utilities.el --- Utility functions and helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared utility layer loaded in Phase 1 (eager).  This is the FIRST
;; module loaded — everything else depends on it.
;;
;; Provides:
;;   1. Buffer name constants — used by popper (popup management) and
;;      display-buffer-alist (window placement) across multiple modules.
;;   2. Defer timing constants — standardized idle-timer delays so all
;;      modules use consistent values (fast=0.5s, medium=1s, slow=2s).
;;   3. Memory/performance utilities — buffer cleanup, GC optimization,
;;      resource monitoring for long-running sessions.
;;   4. Directory management — ensure config subdirectories exist,
;;      clean up stale temp files.
;;   5. Maintenance — daily cleanup, byte-compilation, package sync/upgrade.
;;   6. Doom-style upgrade system — visual *Upgrade* buffer with colored
;;      step counters, elapsed time, per-step error isolation, post-upgrade
;;      validation of critical packages, and auto-restart prompt.
;;      Commands: my/upgrade (SPC h U), my/sync (SPC h S),
;;               my/upgrade-check (SPC h C), my/rollback.
;;
;; This module has NO external dependencies (only cl-lib).

;;; Code:

(require 'cl-lib)

;; ══════════════════════════════════════════════════════════════════
;;  Buffer Name Constants
;; ══════════════════════════════════════════════════════════════════
;; Centralized buffer name strings used by popper (modern-ui.el) and
;; display-buffer-alist (window placement rules).  Using constants
;; avoids typos and makes renaming easy.

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

;; ══════════════════════════════════════════════════════════════════
;;  Global State Variables
;; ══════════════════════════════════════════════════════════════════
;; Shared state used by robustness-enhancements.el and other modules.

(defvar my/backup-directory (expand-file-name "backups" user-emacs-directory)
  "Directory for storing backup files.")

(defvar my/network-status 'unknown
  "Current network connectivity status.")

(defvar my/resource-monitoring-enabled t
  "Whether resource monitoring is enabled.")

(defvar my/trusted-executables '()
  "List of trusted executable paths for security.")

;; ══════════════════════════════════════════════════════════════════
;;  Defer Timing Constants
;; ══════════════════════════════════════════════════════════════════
;; Standardized idle-timer delays used as `:defer` values across all
;; use-package declarations.  Using named constants instead of raw
;; numbers makes it easy to tune all packages at once.

(defconst my/defer-immediate 0
  "Immediate loading — no defer.")

(defconst my/defer-fast 0.5
  "Fast defer (0.5s) — essential UI packages like icons, modeline.")

(defconst my/defer-medium 1.0
  "Medium defer (1s) — completion integrations, visual enhancements.")

(defconst my/defer-slow 2.0
  "Slow defer (2s) — optional packages like multiple-cursors, perspective.")

;; ══════════════════════════════════════════════════════════════════
;;  Memory and Performance Functions
;; ══════════════════════════════════════════════════════════════════
;; Interactive commands for managing memory in long-running sessions.
;; Accessible via SPC h W (optimize), or called by daily maintenance.

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
    (setq gc-cons-threshold (* 16 1024 1024)  ; 16MB
          gc-cons-percentage 0.1)
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
  (let ((killed 0))
    ;; Kill large (>100KB) unmodified temporary buffers
    (dolist (buffer (buffer-list))
      (let ((name (buffer-name buffer))
            (size (buffer-size buffer)))
        (when (and (> size 100000)
                   (not (buffer-modified-p buffer))
                   (not (buffer-file-name buffer))
                   (string-match-p "\\*" name)
                   (not (member name '("*scratch*" "*Messages*"))))
          (kill-buffer buffer)
          (setq killed (1+ killed)))))
    (when (> killed 0)
      (garbage-collect)
      (message "🧠 Smart cleanup: removed %d large temporary buffers" killed))))

;; ══════════════════════════════════════════════════════════════════
;;  Directory Management
;; ══════════════════════════════════════════════════════════════════
;; Ensure config subdirectories exist (auto-saves, backups, cache).
;; Called during sync and daily maintenance.

(defun my/ensure-all-directories ()
  "Ensure essential directories exist for Emacs configuration."
  (interactive)
  (let ((subdirs '("auto-saves" "backups" "eln-cache" "temp" "logs" "cache"))
        (created 0))
    (dolist (name subdirs)
      (let ((full-path (expand-file-name name user-emacs-directory)))
        (condition-case err
            (unless (file-directory-p full-path)
              (make-directory full-path t)
              (setq created (1+ created)))
          (error
           (message "⚠️ Could not create directory %s: %s"
                    full-path (error-message-string err))))))
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
                          (> (float-time (time-since
                                          (file-attribute-modification-time
                                           (file-attributes file))))
                             (* 7 24 60 60))) ; 7 days old
                  (delete-file file)
                  (setq cleaned (1+ cleaned))))
            (error
             (message "⚠️ Failed to clean %s: %s" dir (error-message-string err)))))))
    (when (> cleaned 0)
      (message "🧠 Cleaned %d old temporary files" cleaned))))

;; ══════════════════════════════════════════════════════════════════
;;  Maintenance Functions
;; ══════════════════════════════════════════════════════════════════
;; Automated cleanup that runs after 30 minutes of idle time.
;; Cleans temp files, large buffers, and optimizes GC if needed.

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

;; Run maintenance after 30 minutes of idle (1800s).
;; Non-repeating (nil) — runs once per session when Emacs sits idle.
(run-with-idle-timer 1800 nil #'my/daily-maintenance)

;; ══════════════════════════════════════════════════════════════════
;;  Performance Monitoring
;; ══════════════════════════════════════════════════════════════════
;; Collects GC counts, memory usage, and buffer counts over time.
;; View with M-x my/show-performance-stats.

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

;; ══════════════════════════════════════════════════════════════════
;;  Byte Compilation
;; ══════════════════════════════════════════════════════════════════
;; Byte-compiling (.el -> .elc) speeds up loading by ~2-5x.
;; Run M-x my/byte-compile-config after major changes.

(defun my/byte-compile-config ()
  "Byte-compile all Emacs Lisp files in the configuration."
  (interactive)
  (let ((default-directory user-emacs-directory)
        (compiled 0)
        (failed 0))
    ;; Compile init.el
    (condition-case nil
        (when (byte-compile-file (expand-file-name "init.el" user-emacs-directory))
          (setq compiled (1+ compiled)))
      (error (setq failed (1+ failed))))
    ;; Compile early-init.el
    (condition-case nil
        (when (byte-compile-file (expand-file-name "early-init.el" user-emacs-directory))
          (setq compiled (1+ compiled)))
      (error (setq failed (1+ failed))))
    ;; Compile modules
    (dolist (file (directory-files (expand-file-name "modules" user-emacs-directory) t "\\.el$"))
      (condition-case nil
          (when (byte-compile-file file)
            (setq compiled (1+ compiled)))
        (error (setq failed (1+ failed)))))
    (message "Byte-compilation: %d compiled, %d failed" compiled failed)))

(defun my/clean-compiled-files ()
  "Remove all .elc byte-compiled files from the configuration."
  (interactive)
  (let ((cleaned 0))
    (dolist (file (directory-files user-emacs-directory t "\\.elc$"))
      (delete-file file)
      (setq cleaned (1+ cleaned)))
    (dolist (file (directory-files (expand-file-name "modules" user-emacs-directory) t "\\.elc$"))
      (delete-file file)
      (setq cleaned (1+ cleaned)))
    (message "Cleaned %d compiled files" cleaned)))

;; ══════════════════════════════════════════════════════════════════
;;  Doom-Style Upgrade System
;; ══════════════════════════════════════════════════════════════════
;; Visual upgrade buffer with colored output, step counters, elapsed
;; time, and a summary bar — modeled after `doom upgrade`.
;;
;; Commands:
;;   SPC h U  my/upgrade        Full upgrade (freeze -> pull -> rebuild -> validate)
;;   SPC h S  my/sync           Sync config (dirs + freeze + byte-compile)
;;   SPC h C  my/upgrade-check  Dry-run fetch to preview updates
;;            my/rollback       Thaw lockfile versions and rebuild

;; ── Faces ────────────────────────────────────────────────────────
;; Custom faces for the *Upgrade* buffer.  Inherit from standard
;; Emacs faces (success, error, shadow) so they follow the active
;; theme.  Customize via M-x customize-group RET my/upgrade RET.
(defgroup my/upgrade nil
  "Doom-style upgrade UI."
  :group 'emacs)

(defface my/upgrade-header
  '((t :inherit font-lock-constant-face :weight bold :height 1.2))
  "Face for upgrade buffer header."
  :group 'my/upgrade)

(defface my/upgrade-step-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for step names."
  :group 'my/upgrade)

(defface my/upgrade-success
  '((t :inherit success :weight bold))
  "Face for success indicators."
  :group 'my/upgrade)

(defface my/upgrade-error
  '((t :inherit error :weight bold))
  "Face for failure indicators."
  :group 'my/upgrade)

(defface my/upgrade-dim
  '((t :inherit shadow))
  "Face for timestamps and secondary text."
  :group 'my/upgrade)

(defface my/upgrade-section
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section dividers."
  :group 'my/upgrade)

;; ── State ────────────────────────────────────────────────────────
;; Internal variables tracking the current upgrade run.  Reset by
;; `my/upgrade-buffer-init' at the start of each operation.
;; Double-dash naming (my/upgrade--*) signals private/internal use.
(defvar my/upgrade--start-time nil
  "Timestamp when the current operation started.")

(defvar my/upgrade--step-count 0
  "Current step number in the running operation.")

(defvar my/upgrade--step-total 0
  "Total steps in the running operation.")

(defvar my/upgrade--pass 0
  "Number of steps that succeeded.")

(defvar my/upgrade--fail 0
  "Number of steps that failed.")

;; ── Buffer & Logging ─────────────────────────────────────────────
;; Two logging functions:
;;   my/upgrade-log       — plain text, also echoed to minibuffer
;;   my/upgrade-log-styled — propertized text (colored), buffer only
;; The buffer uses `special-mode' (read-only, q to quit) so output
;; is preserved for review after the operation completes.

(defun my/upgrade-log (fmt &rest args)
  "Log a message to the *Upgrade* buffer.
FMT and ARGS are passed to `format'.  Also echoes to minibuffer."
  (let ((buf (get-buffer-create "*Upgrade*"))
        (msg (apply #'format fmt args)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert msg "\n")))
    (message "%s" msg)))

(defun my/upgrade-log-styled (text face)
  "Insert TEXT with FACE into the *Upgrade* buffer."
  (let ((buf (get-buffer-create "*Upgrade*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize text 'face face))))))

(defun my/upgrade-buffer-init (title total-steps)
  "Create and display the *Upgrade* buffer for a new TITLE run with TOTAL-STEPS."
  (setq my/upgrade--start-time (current-time)
        my/upgrade--step-count 0
        my/upgrade--step-total total-steps
        my/upgrade--pass 0
        my/upgrade--fail 0)
  (let ((buf (get-buffer-create "*Upgrade*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert (propertize (format "  %s\n" title) 'face 'my/upgrade-header))
        (insert (propertize (format "  %s  |  %d steps\n"
                                    (format-time-string "%Y-%m-%d %H:%M:%S")
                                    total-steps)
                            'face 'my/upgrade-dim))
        (insert (propertize "  ────────────────────────────────────────────\n\n"
                            'face 'my/upgrade-dim)))
      (unless (derived-mode-p 'special-mode)
        (special-mode)))
    (pop-to-buffer buf)
    buf))

;; ── Step Runner ──────────────────────────────────────────────────
;; Wraps each upgrade step in `condition-case' for error isolation.
;; A failing step (e.g. network error during pull) is logged as
;; FAILED but does NOT prevent subsequent steps from running.
;; Returns t on success, nil on failure.

(defun my/upgrade-step (name func)
  "Run step NAME via FUNC.  Shows [N/M] counter, elapsed time, result.
Failures are logged but never signal — subsequent steps still run."
  (setq my/upgrade--step-count (1+ my/upgrade--step-count))
  (let* ((n my/upgrade--step-count)
         (m my/upgrade--step-total)
         (prefix (format "  [%d/%d] " n m))
         (elapsed-before (float-time (time-since my/upgrade--start-time))))
    ;; Print step start
    (my/upgrade-log-styled prefix 'my/upgrade-dim)
    (my/upgrade-log-styled (format "%s " name) 'my/upgrade-step-name)
    (redisplay t)
    (condition-case err
        (progn
          (funcall func)
          (let ((dt (- (float-time (time-since my/upgrade--start-time))
                       elapsed-before)))
            (my/upgrade-log-styled "done" 'my/upgrade-success)
            (my/upgrade-log-styled (format " (%.1fs)\n" dt) 'my/upgrade-dim))
          (setq my/upgrade--pass (1+ my/upgrade--pass))
          (message "[%d/%d] %s...done" n m name)
          t)
      (error
       (my/upgrade-log-styled "FAILED" 'my/upgrade-error)
       (my/upgrade-log-styled (format " %s\n" (error-message-string err)) 'my/upgrade-dim)
       (setq my/upgrade--fail (1+ my/upgrade--fail))
       (message "[%d/%d] %s...FAILED" n m name)
       nil))))

;; ── Summary ──────────────────────────────────────────────────────
;; Prints a colored footer: pass/fail counts, total elapsed time,
;; and a hint to press q to close the buffer.

(defun my/upgrade-summary ()
  "Print a summary footer in the *Upgrade* buffer."
  (let* ((elapsed (float-time (time-since my/upgrade--start-time)))
         (buf (get-buffer "*Upgrade*")))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (propertize "\n  ────────────────────────────────────────────\n"
                              'face 'my/upgrade-dim))
          (insert (propertize "  Summary: " 'face 'my/upgrade-section))
          (insert (propertize (format "%d passed" my/upgrade--pass) 'face 'my/upgrade-success))
          (when (> my/upgrade--fail 0)
            (insert ", ")
            (insert (propertize (format "%d failed" my/upgrade--fail) 'face 'my/upgrade-error)))
          (insert (propertize (format "  |  %.1fs elapsed\n" elapsed) 'face 'my/upgrade-dim))
          (if (= my/upgrade--fail 0)
              (insert (propertize "  All steps completed successfully.\n" 'face 'my/upgrade-success))
            (insert (propertize "  Some steps failed — review output above.\n" 'face 'my/upgrade-error)))
          (insert "\n")
          (insert (propertize "  Press q to close this buffer.\n" 'face 'my/upgrade-dim)))))))

;; ── Validation ───────────────────────────────────────────────────
;; After upgrade/rollback, try `require'-ing each critical package
;; to verify it still loads.  Results are printed as a columnar
;; table in the *Upgrade* buffer (OK / MISSING / FAILED per package).
;; Add packages to `my/critical-packages' as your config grows.

(defvar my/critical-packages
  '(evil vertico consult corfu doom-themes general)
  "Packages to validate after an upgrade.")

(defun my/upgrade-validate ()
  "Validate that critical packages can be loaded.  Prints a sub-table."
  (let ((buf (get-buffer-create "*Upgrade*"))
        (ok 0) (fail 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n")
        (insert (propertize "  Package Validation\n" 'face 'my/upgrade-section))
        (dolist (pkg my/critical-packages)
          (insert (propertize (format "    %-20s " pkg) 'face 'my/upgrade-dim))
          (condition-case _err
              (progn
                (require pkg nil t)
                (if (featurep pkg)
                    (progn
                      (insert (propertize "OK\n" 'face 'my/upgrade-success))
                      (setq ok (1+ ok)))
                  (insert (propertize "MISSING\n" 'face 'my/upgrade-error))
                  (setq fail (1+ fail))))
            (error
             (insert (propertize "FAILED\n" 'face 'my/upgrade-error))
             (setq fail (1+ fail)))))
        (insert (propertize (format "    %d OK" ok) 'face 'my/upgrade-success))
        (when (> fail 0)
          (insert ", ")
          (insert (propertize (format "%d failed" fail) 'face 'my/upgrade-error)))
        (insert "\n")))))

;; ── Auto-Restart ─────────────────────────────────────────────────
;; Uses `restart-emacs' package if available (declared in init.el),
;; falls back to `save-buffers-kill-emacs' otherwise.

(defun my/upgrade-maybe-restart ()
  "Prompt to restart Emacs after upgrade or rollback."
  (when (y-or-n-p "Restart Emacs now? ")
    (if (fboundp 'restart-emacs)
        (restart-emacs)
      (save-buffers-kill-emacs))))

;; ══════════════════════════════════════════════════════════════════
;;  Sync / Upgrade / Rollback / Check
;; ══════════════════════════════════════════════════════════════════
;; Public interactive commands — these are the entry points.
;;
;;   my/sync          SPC h S   Freeze lockfile + byte-compile (no pull)
;;   my/upgrade       SPC h U   Full upgrade: freeze -> pull -> rebuild -> validate
;;   my/rollback      —         Thaw lockfile -> rebuild -> validate
;;   my/upgrade-check SPC h C   Fetch-only dry run (preview changes)
;;
;; Upgrade freezes versions BEFORE pulling so there is always a
;; lockfile to roll back to if the new versions break something.

(defun my/sync ()
  "Sync configuration: ensure directories, freeze versions, byte-compile."
  (interactive)
  (my/upgrade-buffer-init "Sync Configuration" 3)
  (my/upgrade-step "Ensure directories" #'my/ensure-all-directories)
  (my/upgrade-step "Freeze versions"
                   (lambda ()
                     (when (fboundp 'straight-freeze-versions)
                       (straight-freeze-versions))))
  (my/upgrade-step "Byte-compile config" #'my/byte-compile-config)
  (my/upgrade-summary))

(defun my/upgrade ()
  "Upgrade all packages — Doom-style with visual progress.

  1. Freeze current versions (rollback safety net)
  2. Clean compiled files
  3. Pull all packages
  4. Rebuild all packages
  5. Ensure config directories
  6. Freeze updated versions
  7. Byte-compile config

Then validates critical packages and offers to restart."
  (interactive)
  (when (y-or-n-p "Upgrade all packages? ")
    (my/upgrade-buffer-init "Upgrade Packages" 7)
    (my/upgrade-step "Freeze current versions (backup)"
                     (lambda ()
                       (when (fboundp 'straight-freeze-versions)
                         (straight-freeze-versions))))
    (my/upgrade-step "Clean compiled files" #'my/clean-compiled-files)
    (my/upgrade-step "Pull all packages"
                     (lambda ()
                       (when (fboundp 'straight-pull-all)
                         (straight-pull-all))))
    (my/upgrade-step "Rebuild all packages"
                     (lambda ()
                       (when (fboundp 'straight-rebuild-all)
                         (straight-rebuild-all))))
    (my/upgrade-step "Ensure directories" #'my/ensure-all-directories)
    (my/upgrade-step "Freeze updated versions"
                     (lambda ()
                       (when (fboundp 'straight-freeze-versions)
                         (straight-freeze-versions))))
    (my/upgrade-step "Byte-compile config" #'my/byte-compile-config)
    (my/upgrade-validate)
    (my/upgrade-summary)
    (my/upgrade-maybe-restart)))

(defun my/rollback ()
  "Rollback packages to the last frozen lockfile versions."
  (interactive)
  (when (y-or-n-p "Rollback all packages to lockfile versions? ")
    (my/upgrade-buffer-init "Rollback Packages" 3)
    (my/upgrade-step "Thaw versions from lockfile"
                     (lambda ()
                       (if (fboundp 'straight-thaw-versions)
                           (straight-thaw-versions)
                         (error "straight-thaw-versions not available"))))
    (my/upgrade-step "Rebuild all packages"
                     (lambda ()
                       (when (fboundp 'straight-rebuild-all)
                         (straight-rebuild-all))))
    (my/upgrade-step "Byte-compile config" #'my/byte-compile-config)
    (my/upgrade-validate)
    (my/upgrade-summary)
    (my/upgrade-maybe-restart)))

(defun my/upgrade-check ()
  "Fetch (but don't merge) all packages to preview available updates.
Review *straight-process* afterward to see what repos have new commits."
  (interactive)
  (my/upgrade-buffer-init "Check for Updates" 1)
  (my/upgrade-step "Fetch all repos"
                   (lambda ()
                     (if (fboundp 'straight-fetch-all)
                         (straight-fetch-all)
                       (error "straight-fetch-all not available"))))
  (my/upgrade-summary)
  (my/upgrade-log "")
  (my/upgrade-log "  Review *straight-process* for available changes.")
  (my/upgrade-log "  Run SPC h U to apply updates."))

(provide 'utilities)
;;; utilities.el ends here
