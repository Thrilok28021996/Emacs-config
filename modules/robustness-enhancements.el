;;; modules/robustness-enhancements.el --- Essential robustness features -*- lexical-binding: t; -*-

;;; Commentary:
;; Error handling and recovery system loaded at startup via
;; `emacs-startup-hook`.  Catches and logs package load failures so
;; a single broken package doesn't take down the entire config.
;;
;; Provides:
;;   my/safe-require      — require with error logging (use instead of require)
;;   my/safe-load         — load with error logging (use instead of load)
;;   my/recover-from-errors — retry loading failed packages
;;   my/show-error-summary  — display all recorded failures
;;   my/doctor             — Doom-style health check (tools, fonts, LSP, etc.)
;;
;; Critical packages (vertico, evil, consult, corfu) get extra
;; treatment — failures are shown as `:error` warnings in *Warnings*.
;;
;; Depends on: utilities.el (for buffer name constants)

;;; Code:

;; Load utilities if available (may not exist on first clone)
(when (file-exists-p (expand-file-name "utilities.el" (file-name-directory (or load-file-name buffer-file-name))))
  (require 'utilities))

;; ══════════════════════════════════════════════════════════════════
;;  1. Error Tracking State
;; ══════════════════════════════════════════════════════════════════
;; All package load failures are recorded as plists with :package,
;; :error, :timestamp, and :context.  The list is capped at 50
;; entries to prevent unbounded growth.

(defvar my/package-load-failures '()
  "List of packages that failed to load.")

(defvar my/max-error-history 50
  "Maximum number of errors to keep in history.")

(defvar my/critical-errors '()
  "List of critical errors that require attention.")

;; ══════════════════════════════════════════════════════════════════
;;  2. Safe Loading Functions
;; ══════════════════════════════════════════════════════════════════
;; Drop-in replacements for `require` and `load` that catch errors
;; and log them instead of aborting init.

(defun my/handle-package-failure (package error)
  "Log a package failure and promote critical ones to *Warnings*."
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
  "Like `require` but catches errors and logs them via my/handle-package-failure."
  (condition-case err
      (require feature filename noerror)
    (error
     (my/handle-package-failure feature err)
     nil)))

(defun my/safe-load (file &optional noerror nomessage)
  "Like `load` but catches errors and logs them via my/handle-package-failure."
  (condition-case err
      (load file noerror nomessage)
    (error
     (my/handle-package-failure (file-name-nondirectory file) err)
     nil)))

;; ══════════════════════════════════════════════════════════════════
;;  3. Initialization & Hooks
;; ══════════════════════════════════════════════════════════════════
;; Runs on `emacs-startup-hook` (after init.el finishes).
;; Ensures the backup directory exists and sets up a post-init
;; hook that warns the user if any critical packages failed.

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
        
        ;; Check for critical errors immediately (after-init-hook has
        ;; already fired by the time emacs-startup-hook runs)
        (unless noninteractive
          (when my/critical-errors
            (display-warning 'init
                            (format "%d critical package(s) failed to load. Check *Warnings* buffer."
                                   (length my/critical-errors))
                            :error)))

        (unless noninteractive
          (message "🛡️ Robustness enhancements initialized")))
    (error
     (message "⚠️ Robustness initialization failed: %s" (error-message-string err)))))

;; ══════════════════════════════════════════════════════════════════
;;  4. Recovery & Diagnostics
;; ══════════════════════════════════════════════════════════════════
;; my/recover-from-errors — retries requiring each failed package.
;; Useful after installing a missing dependency.
;; my/show-error-summary — displays a formatted report of all
;; failures in a dedicated *Configuration Errors* buffer.

(defun my/recover-from-errors ()
  "Attempt to recover from configuration errors."
  (interactive)
  (let ((recovered 0))
    (dolist (failure my/package-load-failures)
      (let ((package (plist-get failure :package)))
        (condition-case _err
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

;; ══════════════════════════════════════════════════════════════════
;;  5. Config Doctor (Doom-style Health Check)
;; ══════════════════════════════════════════════════════════════════
;; M-x my/doctor — checks the system for:
;;   - External tools (git, rg, python3, g++, cmake, clang-format, black)
;;   - LSP servers (pyright, clangd, vscode-html/css)
;;   - Fonts (Fira Code, Symbols Nerd Font Mono)
;;   - Tree-sitter grammars (c, cpp, python, json, css, html)
;;   - Native compilation availability
;;   - Package load failures from this session
;; Results are shown in *Config Doctor* with OK/MISS/WARN/FAIL status.

(defun my/doctor ()
  "Run a health check on the configuration, Doom-style.
Checks external tools, LSP servers, fonts, tree-sitter grammars,
native compilation, and package load failures."
  (interactive)
  (let ((buf (get-buffer-create "*Config Doctor*"))
        (pass 0) (warn 0) (fail 0))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Config Doctor\n")
      (insert (make-string 60 ?=) "\n\n")

      ;; --- External tools ---
      (insert "External Tools\n")
      (insert (make-string 40 ?-) "\n")
      (dolist (tool '("git" "rg" "python3" "g++" "cmake" "clang-format" "black"))
        (if (executable-find tool)
            (progn
              (insert (format "  OK  %s (%s)\n" tool (executable-find tool)))
              (setq pass (1+ pass)))
          (insert (format "  MISS %s\n" tool))
          (setq fail (1+ fail))))
      (insert "\n")

      ;; --- LSP servers ---
      (insert "LSP Servers\n")
      (insert (make-string 40 ?-) "\n")
      (dolist (server '("pyright-langserver" "clangd"
                        "vscode-html-language-server" "vscode-css-language-server"))
        (if (executable-find server)
            (progn
              (insert (format "  OK  %s\n" server))
              (setq pass (1+ pass)))
          (insert (format "  MISS %s\n" server))
          (setq warn (1+ warn))))
      (insert "\n")

      ;; --- Fonts ---
      (insert "Fonts\n")
      (insert (make-string 40 ?-) "\n")
      (if (display-graphic-p)
          (dolist (font '("Fira Code" "Symbols Nerd Font Mono"))
            (if (find-font (font-spec :name font))
                (progn
                  (insert (format "  OK  %s\n" font))
                  (setq pass (1+ pass)))
              (insert (format "  MISS %s\n" font))
              (setq warn (1+ warn))))
        (progn
          (insert "  SKIP (non-graphical session)\n")
          (setq warn (1+ warn))))
      (insert "\n")

      ;; --- Tree-sitter grammars ---
      (insert "Tree-sitter Grammars\n")
      (insert (make-string 40 ?-) "\n")
      (if (fboundp 'treesit-language-available-p)
          (dolist (lang '(c cpp python json css html))
            (if (treesit-language-available-p lang)
                (progn
                  (insert (format "  OK  %s\n" lang))
                  (setq pass (1+ pass)))
              (insert (format "  MISS %s\n" lang))
              (setq warn (1+ warn))))
        (progn
          (insert "  SKIP (tree-sitter not available)\n")
          (setq warn (1+ warn))))
      (insert "\n")

      ;; --- Native compilation ---
      (insert "Native Compilation\n")
      (insert (make-string 40 ?-) "\n")
      (if (and (fboundp 'native-comp-available-p)
               (native-comp-available-p))
          (progn
            (insert "  OK  Native compilation available\n")
            (setq pass (1+ pass)))
        (insert "  WARN Native compilation NOT available\n")
        (setq warn (1+ warn)))
      (insert "\n")

      ;; --- Package load failures ---
      (insert "Package Load Failures\n")
      (insert (make-string 40 ?-) "\n")
      (if (and (boundp 'my/package-load-failures) my/package-load-failures)
          (dolist (failure my/package-load-failures)
            (insert (format "  FAIL %s: %s\n"
                           (plist-get failure :package)
                           (plist-get failure :error)))
            (setq fail (1+ fail)))
        (progn
          (insert "  OK  No package failures recorded\n")
          (setq pass (1+ pass))))
      (insert "\n")

      ;; --- Summary ---
      (insert (make-string 60 ?=) "\n")
      (insert (format "Summary: %d passed, %d warnings, %d failures\n" pass warn fail))
      (when (= fail 0)
        (insert "Your config looks healthy!\n")))

    (display-buffer buf)
    (with-current-buffer buf
      (goto-char (point-min)))))

;; Run initialization after Emacs startup completes
(add-hook 'emacs-startup-hook #'my/initialize-robustness-enhancements)

(provide 'robustness-enhancements)
;;; robustness-enhancements.el ends here
