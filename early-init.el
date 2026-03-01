;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; This file runs BEFORE init.el, BEFORE the package system loads, and
;; BEFORE any UI is drawn.  It is the only place where certain settings
;; take full effect (e.g. suppressing toolbars before the first frame).
;;
;; Strategy: temporarily disable every expensive subsystem during init,
;; then restore defaults once Emacs is idle.  This shaves ~0.3-0.5s off
;; startup compared to setting these in init.el.
;;
;; Sections:
;;   1. GC threshold          — prevent garbage collection during init
;;   2. Package system        — disable package.el (we use straight.el)
;;   3. Bidirectional text    — skip expensive RTL/bidi computation
;;   4. UI chrome             — hide toolbars before the frame is drawn
;;   5. File-name handler     — skip .gz/.el.gz handler regex matching
;;   6. Native compilation    — silence async warnings, enable JIT
;;   7. Startup suppression   — hide splash screen and echo area message

;;; Code:

;; ──────────────────────────────────────────────────────────────────
;; 1. GC — raise threshold to maximum during init (restored by gcmh)
;; ──────────────────────────────────────────────────────────────────
;; Default gc-cons-threshold is 800KB, which triggers dozens of GC
;; pauses during init.  Setting it to `most-positive-fixnum` (≈2^61)
;; effectively disables GC until the gcmh package restores a sane
;; value (~16MB) after startup.  gc-cons-percentage is raised to
;; allow more heap growth per cycle when GC does eventually run.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ──────────────────────────────────────────────────────────────────
;; 2. Package system — disabled; we use straight.el instead
;; ──────────────────────────────────────────────────────────────────
;; package.el scans ~/.emacs.d/elpa/ and builds autoloads at startup.
;; Since we use straight.el for all package management, disabling it
;; avoids that overhead entirely.
(setq package-enable-at-startup nil)
;; site-run-file points to the system-wide site-start.el (e.g. in
;; /usr/share/emacs/).  It often loads system packages we don't need.
(setq site-run-file nil)

;; ──────────────────────────────────────────────────────────────────
;; 3. Bidirectional text — optimize for left-to-right-only editing
;; ──────────────────────────────────────────────────────────────────
;; Emacs supports right-to-left scripts (Arabic, Hebrew) but the bidi
;; reordering engine is expensive, especially on long lines.  Since
;; this config only edits LTR text (English, code), we tell Emacs
;; every paragraph is LTR.  `bidi-inhibit-bpa` disables the Unicode
;; Bidirectional Parenthesis Algorithm, which is O(n) per line.
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ──────────────────────────────────────────────────────────────────
;; 4. UI — suppress chrome before the first frame is drawn
;; ──────────────────────────────────────────────────────────────────
;; Setting these in early-init.el prevents the toolbar/menubar from
;; ever being drawn, avoiding the visual flicker you get when
;; disabling them later in init.el.  `frame-inhibit-implied-resize`
;; prevents Emacs from resizing the frame when the toolbar disappears
;; — without it the window jumps by ~40px.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; ──────────────────────────────────────────────────────────────────
;; 5. File-name handler — disable during init, restore after startup
;; ──────────────────────────────────────────────────────────────────
;; `file-name-handler-alist` maps regex patterns to handler functions
;; (e.g. jka-compr for .gz files, tramp for /ssh:).  Every `load`,
;; `require`, and `expand-file-name` call tests against ALL patterns.
;; During init this is pure overhead since we never load compressed or
;; remote files.  We save the alist, set it to nil, and restore it
;; after startup so normal file handling works again.
(defvar my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/saved-file-name-handler-alist)))

;; ──────────────────────────────────────────────────────────────────
;; 6. Native compilation (Emacs 28+)
;; ──────────────────────────────────────────────────────────────────
;; Emacs 28+ can compile Elisp to native machine code via libgccjit.
;; `native-comp-jit-compilation` compiles functions on first use in
;; a background process.  `silent` suppresses the *Warnings* buffer
;; that pops up with harmless "free variable" warnings during async
;; compilation.  `deferred-compilation` allows compilation to happen
;; lazily rather than at package install time.
;; Silence byte-compiler warnings for native-comp variables
(defvar native-comp-async-report-warnings-errors)
(defvar native-comp-jit-compilation)

(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t))

;; ──────────────────────────────────────────────────────────────────
;; 7. Startup suppression — hide splash screen and echo area message
;; ──────────────────────────────────────────────────────────────────
;; `inhibit-startup-message` hides the GNU Emacs splash/about buffer.
;; `inhibit-startup-echo-area-message` must be set to your login name
;; (a hardcoded security check) to suppress "For information about
;; GNU Emacs..." in the echo area.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

(provide 'early-init)
;;; early-init.el ends here
