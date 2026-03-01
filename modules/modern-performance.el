;;; modules/modern-performance.el --- Emacs 29/30 features and advanced performance -*- lexical-binding: t; -*-

;;; Commentary:
;; Performance optimizations loaded in Phase 1 (eager).
;; Tunes GC, I/O, scrolling, and native compilation for fast editing.
;;
;; Key optimizations:
;;   GC (gcmh)       — runs GC only when idle, not during editing
;;   Native comp     — JIT-compiles Elisp to machine code in background
;;   Scrolling       — pixel-precise scrolling for smooth trackpad UX
;;   File I/O        — reduced backup versions, dedicated backup directory
;;   Process I/O     — 1MB read buffer for fast LSP communication
;;   UTF-8           — system-wide UTF-8 with unix line endings
;;   Repeat mode     — press a key repeatedly without re-pressing prefix

;;; Code:

;; Silence byte-compiler warnings for native-comp variables (only
;; exist when Emacs is built with libgccjit support).
(defvar native-comp-jit-compilation)
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-async-jobs-number)
(defvar native-comp-speed)
(defvar native-comp-debug)
(defvar native-comp-eln-load-path)

;; Silence byte-compiler warnings for pixel-scroll variables (Emacs 29+)
(defvar pixel-scroll-precision-large-scroll-height)
(defvar pixel-scroll-precision-interpolation-factor)
(defvar pixel-scroll-precision-use-momentum)

;; Silence byte-compiler warnings for repeat-mode and project.el variables
(defvar repeat-exit-timeout)
(defvar project-vc-merge-submodules)

;; ══════════════════════════════════════════════════════════════════
;;  Native Compilation (Emacs 28+)
;; ══════════════════════════════════════════════════════════════════
;; libgccjit compiles Elisp to native machine code, giving ~2-5x
;; speedup for compute-heavy packages (magit, treesit, org).

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-jit-compilation t               ; compile on first use
        native-comp-async-report-warnings-errors nil ; suppress *Warnings* popups
        native-comp-async-jobs-number (max 1 (/ (num-processors) 2))  ; use half the CPU cores
        native-comp-speed 2                          ; optimization level (0-3, 2=balanced)
        native-comp-debug 0)                         ; no debug symbols (smaller .eln files)

  ;; Ensure our eln-cache directory is in the search path
  (let ((native-dir (expand-file-name "eln-cache" user-emacs-directory)))
    (add-to-list 'native-comp-eln-load-path native-dir))

  (message "✅ Native compilation enabled"))

;; ══════════════════════════════════════════════════════════════════
;;  Garbage Collection Tuning
;; ══════════════════════════════════════════════════════════════════
;; Emacs's default GC threshold (800KB) causes frequent pauses during
;; editing.  We raise it to 32MB for normal use.  gcmh handles the
;; idle-GC strategy so you never notice GC pauses.

(defvar my/gc-cons-threshold-normal (* 32 1024 1024)
  "Normal GC threshold (32MB) — balances memory usage and pause frequency.")

(defvar my/gc-cons-percentage-normal 0.15
  "Normal GC percentage — controls how much heap growth triggers GC.")

;; ── GCMH (GC Magic Hack) ──────────────────────────────────────────
;; The single most impactful performance package.  Strategy:
;;   - While you're actively typing/navigating: GC threshold stays HIGH
;;     (32MB) so GC never triggers during editing.
;;   - When Emacs is idle: threshold drops and GC runs in the background.
;; This eliminates the 50-200ms freezes caused by default GC behavior.

(use-package gcmh
  :straight t
  :demand t                                    ; load immediately (needed from the start)
  :config
  (setq gcmh-idle-delay 'auto                  ; auto-tune idle delay based on usage
        gcmh-auto-idle-delay-factor 10         ; multiplier for auto delay calculation
        gcmh-high-cons-threshold (* 32 1024 1024))  ; 32MB threshold during active use
  (gcmh-mode 1))

;; ══════════════════════════════════════════════════════════════════
;;  Scrolling & Display (Emacs 29+)
;; ══════════════════════════════════════════════════════════════════

;; Pixel-precision scrolling — makes trackpad scrolling smooth like
;; a native macOS app instead of jumping line-by-line.
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 40.0   ; threshold for "large" scroll
        pixel-scroll-precision-interpolation-factor 30.0   ; smoothing factor
        pixel-scroll-precision-use-momentum t))            ; inertial scrolling

;; Conservative scrolling — scroll just enough to keep the cursor
;; visible instead of recentering.  This prevents the jarring
;; "jump to center" behavior when cursor reaches screen edge.
(condition-case err
    (progn
      (setq scroll-preserve-screen-position t   ; keep cursor at same screen position
            scroll-conservatively 101            ; never recenter (scroll minimally)
            scroll-margin 0                      ; no margin at screen edges
            scroll-step 1                        ; scroll one line at a time
            auto-window-vscroll nil              ; don't auto-adjust for tall lines
            fast-but-imprecise-scrolling t        ; skip fontification during fast scroll
            mouse-wheel-scroll-amount '(1 ((shift) . 1))  ; 1 line per wheel click
            mouse-wheel-progressive-speed nil))  ; don't accelerate wheel speed
  (error
   (message "⚠️ Some scrolling optimizations failed: %s" (error-message-string err))))

;; ══════════════════════════════════════════════════════════════════
;;  Font & Display Rendering
;; ══════════════════════════════════════════════════════════════════
;; Fine-tune how Emacs renders text and resizes frames.

;; nil = use the font's natural line spacing (no extra padding).
;; Extra line-spacing slows redisplay because more lines must be
;; rendered to fill the same screen height.
(setq-default line-spacing nil)

;; Draw underlines at the descent line (bottom of character cell)
;; instead of the baseline.  Looks better with most programming fonts.
(setq x-underline-at-descent-line t)

;; Allow pixel-exact frame resizing instead of snapping to character
;; grid.  This lets the window manager tile Emacs precisely.
;; Window resizing stays character-aligned to avoid partial-line display.
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

;; Warn before opening files larger than 25MB.  Emacs can handle large
;; files but they trigger expensive fontification and undo tracking.
(setq large-file-warning-threshold 25000000) ; 25MB

;; Only check Git for version control status — skip SVN, Hg, Bazaar,
;; etc.  Reduces the number of subprocess calls when visiting files.
(setq vc-handled-backends '(Git))

;; ══════════════════════════════════════════════════════════════════
;;  Process & File I/O
;; ══════════════════════════════════════════════════════════════════
;; Tune how Emacs communicates with subprocesses (LSP, formatters)
;; and how it handles file backups / auto-saves.

;; Increase the chunk size Emacs reads from subprocesses per cycle.
;; Default is 4KB which means many read() syscalls for LSP responses.
;; 1MB lets Eglot/LSP ingest large JSON payloads in fewer reads.
(setq read-process-output-max (* 1 1024 1024)) ; 1MB

;; Disable adaptive read buffering — Emacs normally delays reading
;; subprocess output to batch it up.  Disabling gives lower latency
;; for interactive tools like LSP and compilation buffers.
(setq process-adaptive-read-buffering nil)

;; File backup strategy:
;;   - No .#lockfiles (interfere with file watchers like webpack/vite)
;;   - Backups enabled and stored in ~/.emacs.d/backups/ (not alongside files)
;;   - Copy-based backups preserve hard links and ownership
;;   - Numbered backups keep a rolling history (3 recent + 1 oldest)
(setq create-lockfiles nil
      make-backup-files t
      backup-by-copying t
      backup-directory-alist
      `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      delete-old-versions t
      version-control t
      kept-new-versions 3
      kept-old-versions 1
      auto-save-default t         ; auto-save protects against crashes
      auto-save-interval 200      ; auto-save every 200 keystrokes
      auto-save-timeout 20)       ; auto-save after 20s idle

;; ══════════════════════════════════════════════════════════════════
;;  UTF-8 Everywhere
;; ══════════════════════════════════════════════════════════════════
;; Force UTF-8 as the default encoding at every level — file I/O,
;; terminal, clipboard, and new buffers.  This prevents Emacs from
;; wasting time probing encodings and avoids mojibake in mixed
;; environments.  `utf-8-unix` uses LF line endings (no CR overhead).
(condition-case err
    (progn
      (set-default-coding-systems 'utf-8)
      (setq locale-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)       ; clipboard paste encoding
      (prefer-coding-system 'utf-8)              ; priority when auto-detecting
      (setq-default buffer-file-coding-system 'utf-8-unix)  ; LF line endings
      (setq default-file-name-coding-system 'utf-8))
  (error
   (message "⚠️ UTF-8 setup failed: %s" (error-message-string err))))



;; ══════════════════════════════════════════════════════════════════
;;  File Handling & Auto-Save
;; ══════════════════════════════════════════════════════════════════
;; Note: GC threshold is set to most-positive-fixnum in early-init.el
;; during startup; gcmh-mode restores sane values after init completes.

;; Skip confirmation prompts when quitting — Emacs auto-saves protect
;; against accidental data loss, so the "really quit?" dialog is just
;; friction for keyboard-driven workflows.
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)

;; Redirect auto-save files (#filename#) to a dedicated directory
;; instead of cluttering project directories.  The `t` at the end
;; means use just the basename (flatten the directory structure).
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; Create the auto-save directory on first load if it doesn't exist
(let ((auto-save-dir (expand-file-name "auto-saves" user-emacs-directory)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir t)))

;; ══════════════════════════════════════════════════════════════════
;;  Modern Built-in Features (Emacs 28+/29+)
;; ══════════════════════════════════════════════════════════════════
;; Enable useful built-in features that ship with modern Emacs.
;; Each is guarded by `fboundp` so this file works on older versions.

(condition-case err
    (progn
      ;; tab-bar — workspace tabs at the top of the frame.
      ;; Show the tab bar only when 2+ tabs exist (tab-bar-show 1).
      ;; Hide close/new buttons — we use keybindings instead.
      (when (fboundp 'tab-bar-mode)
        (setq tab-bar-show 1)
        (setq tab-bar-close-button-show nil))

      ;; repeat-mode — after pressing a prefix like C-x o (other-window),
      ;; you can keep pressing just `o` to repeat.  The repeat map exits
      ;; after 2 seconds of inactivity.
      (when (fboundp 'repeat-mode)
        (repeat-mode 1)
        (setq repeat-exit-timeout 2))

      ;; project.el — don't merge git submodule contents into the
      ;; parent project's file list.  Keeps project-find-file fast
      ;; in monorepos with vendored dependencies.
      (when (fboundp 'project-remember-projects-under)
        (setq project-vc-merge-submodules nil)))
  (error
   (message "⚠️ Some modern feature initialization failed: %s" (error-message-string err))))


;; ══════════════════════════════════════════════════════════════════
;;  GC Monitoring & Recovery
;; ══════════════════════════════════════════════════════════════════
;; Safety net: if Emacs accumulates too many GC cycles (indicating
;; the threshold is too low or a package is allocating excessively),
;; bump the threshold to 50MB.  Use M-x my/reset-gc-settings to
;; restore defaults, or M-x my/handle-excessive-gc to check now.

(defun my/handle-excessive-gc ()
  "Detect excessive GC and raise the threshold to reduce pauses.
Triggers when `gcs-done' exceeds 300 cycles (a sign the threshold
is too low for the current workload)."
  (interactive)
  (when (> gcs-done 300)
    (setq gc-cons-threshold (* 50 1024 1024))  ; bump to 50MB
    (setq gc-cons-percentage 0.2)
    (message "🗑️ Excessive GC detected (%d cycles), adjusting thresholds" gcs-done)))

(defun my/reset-gc-settings ()
  "Reset GC settings to the normal 32MB threshold.
Use this after M-x my/handle-excessive-gc to go back to defaults."
  (interactive)
  (setq gc-cons-threshold my/gc-cons-threshold-normal
        gc-cons-percentage my/gc-cons-percentage-normal)
  (message "GC settings reset to normal (threshold: %dMB)"
           (/ my/gc-cons-threshold-normal 1024 1024)))

(provide 'modern-performance)
;;; modern-performance.el ends here
