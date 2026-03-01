;;; init.el --- Modern Emacs Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for the entire configuration.  Bootstraps straight.el,
;; defines the module system, and orchestrates phased loading.
;;
;; Architecture: Instead of loading everything eagerly (which would
;; take 3-5s), we split the config into 5 phases using idle timers.
;; Only what's visible on screen loads synchronously; the rest loads
;; in the background once Emacs is idle.  This gives a ~0.3s startup.
;;
;;   Phase 1 (eager)  — utilities, performance, core-ui, theme, evil
;;                      These must load before the first frame is shown.
;;   Phase 2 (0.1s)   — dashboard (so it appears almost instantly)
;;   Phase 3 (0.5s)   — completion (vertico/consult/corfu for M-x)
;;   Phase 4 (1s)     — languages, ui polish, syntax colors
;;   Phase 5 (2s)     — robustness, org/markdown configs
;;
;; The module system (my/modules) lets you toggle features by adding
;; or removing entries, without touching the rest of this file.
;;
;; Stack: straight.el, use-package, Evil, Vertico, Eglot, Tree-sitter

;;; Code:

;; Silence byte-compiler warnings for deferred variables/functions
(defvar exec-path-from-shell-variables)
(declare-function exec-path-from-shell-initialize "exec-path-from-shell")

;; Silence byte-compiler warnings for straight.el variables
(defvar straight-vc-git-default-clone-depth)
(defvar straight-check-for-modifications)
(defvar straight-vc-git-force-protocol)
(defvar straight-use-package-by-default)
(declare-function straight-use-package "straight")

;; ══════════════════════════════════════════════════════════════════
;;  Bootstrap
;; ══════════════════════════════════════════════════════════════════

;; Record the exact moment init.el starts — used at the bottom of
;; this file to report total startup time in the echo area.
(defconst emacs-start-time (current-time)
  "Timestamp recorded at the very start of init.el.")

;; Suppress ALL screen updates during init.  This prevents the
;; partially-configured frame from flickering while packages load.
;; Restored in the `emacs-startup-hook` at the bottom of this file.
(setq inhibit-redisplay t)

(defun my/handle-startup-error (error-data)
  "Log ERROR-DATA to *Startup Errors* and display the buffer."
  (message "Startup error: %s" error-data)
  (with-current-buffer (get-buffer-create "*Startup Errors*")
    (goto-char (point-max))
    (insert (format "[%s] %s\n" (current-time-string) error-data))
    (display-buffer (current-buffer))))

;; ── straight.el ──────────────────────────────────────────────────
;; Functional package manager that clones git repos and builds
;; packages locally.  Advantages over package.el: reproducible
;; builds, easy fork-and-patch, no reliance on MELPA tarballs.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1   ; shallow clones save disk & time
      straight-check-for-modifications nil     ; skip file-watch overhead (rebuild with M-x straight-rebuild-all)
      straight-vc-git-force-protocol 'https    ; works behind corporate firewalls (unlike SSH)
      straight-use-package-by-default t        ; every use-package form auto-installs via straight
      load-prefer-newer t)                     ; prefer .el over stale .elc byte-compiled files

;; ── use-package ──────────────────────────────────────────────────
(straight-use-package 'use-package)

;; ── Org mode — load early to prevent version mismatch ───────────
;; The dashboard (Phase 1) loads Org for agenda items, which would
;; trigger the built-in Org version. Loading the newer Org here
;; ensures the correct version is used throughout.
(straight-use-package 'org)

;; ── Essentials ───────────────────────────────────────────────────
(set-language-environment "UTF-8")             ; default encoding for all buffers
;; Redirect M-x customize writes to a separate file so they don't
;; pollute init.el.  The file is never loaded — we manage all
;; settings explicitly in this config.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; ══════════════════════════════════════════════════════════════════
;;  Module System
;; ══════════════════════════════════════════════════════════════════
;; Central registry of enabled modules and their feature flags.
;; To disable a module, comment out or remove its entry.
;; To disable a language, remove its +flag (e.g. remove +python).
;; The phased loading section below checks this list before loading.

(defvar my/modules
  '((performance)                                ; GC tuning, process optimizations
    (ui)                                         ; icons, modeline, ligatures, fonts
    (evil)                                       ; Vim emulation + SPC leader keys
    (completion)                                 ; vertico, consult, corfu, embark
    (languages +python +cpp +html +css +lsp +tree-sitter)
    (colors)                                     ; rainbow-delimiters, color-identifiers
    (dashboard)                                  ; startup screen with recent files
    (utilities)                                  ; shared constants, helper functions
    (robustness))                                ; error recovery, health checks
  "List of enabled modules with feature flags.
Each entry is (MODULE-NAME +FLAG1 +FLAG2 ...).")

(defun my/module-enabled-p (module)
  "Return non-nil if MODULE is enabled in `my/modules'."
  (assq module my/modules))

(defun my/module-flag-p (module flag)
  "Return non-nil if MODULE has FLAG enabled.
FLAG should be a symbol like +python."
  (when-let ((entry (assq module my/modules)))
    (memq flag (cdr entry))))

(defun my/lang-enabled-p (lang)
  "Return non-nil if language LANG is enabled.
LANG should be a symbol like python, cpp, html, css."
  (my/module-flag-p 'languages
                    (intern (concat "+" (symbol-name lang)))))

;; ══════════════════════════════════════════════════════════════════
;;  Platform — macOS PATH
;; ══════════════════════════════════════════════════════════════════
;; On macOS, GUI Emacs doesn't inherit the shell PATH.
;; Defer the shell spawn to 0.5s idle so it doesn't block startup;
;; PATH is only needed when compiling, running shells, or using Eglot.

(when (eq system-type 'darwin)
  (when (memq window-system '(mac ns x pgtk))
    (straight-use-package 'exec-path-from-shell)
    (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH" "CONDA_PREFIX"))
    (if noninteractive
        (exec-path-from-shell-initialize)
      (run-with-idle-timer 0.5 nil #'exec-path-from-shell-initialize))))

;; ══════════════════════════════════════════════════════════════════
;;  Module Loader
;; ══════════════════════════════════════════════════════════════════
;; Two functions provide safe, logged module loading:
;;   my/safe-require-module — loads immediately, catches errors
;;   my/deferred-require   — wraps the above in an idle timer
;; Errors are caught per-module so one broken module won't prevent
;; the rest of Emacs from starting.

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defun my/safe-require-module (module description)
  "Require MODULE; log DESCRIPTION on success or the error on failure."
  (condition-case err
      (progn
        (require module)
        (message "✅ %s" description)
        t)
    (error
     (message "❌ Failed to load %s: %s" module (error-message-string err))
     nil)))

(defun my/deferred-require (delay module description)
  "Load MODULE after DELAY seconds of idle time.
In batch mode (`emacs --batch`), load immediately because idle
timers never fire in non-interactive sessions."
  (if noninteractive
      (my/safe-require-module module description)
    (run-with-idle-timer delay nil
      (lambda ()
        (my/safe-require-module module description)))))

;; ══════════════════════════════════════════════════════════════════
;;  Phased Module Loading
;; ══════════════════════════════════════════════════════════════════

;; ── Phase 1: Eager (blocks startup) ─────────────────────────────
;; Only the absolute essentials — everything the user sees immediately.
(my/safe-require-module 'utilities "Utilities")
(when (my/module-enabled-p 'performance)
  (my/safe-require-module 'modern-performance "Performance optimizations"))
(my/safe-require-module 'core-ui "UI settings")

;; Load theme in Phase 1 so the first visible frame has colors.
;; If this were deferred (like in Phase 4), the user would see a
;; white/default Emacs for ~1 second before doom-one applies.
(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t              ; allow bold faces
        doom-themes-enable-italic t            ; allow italic faces
        doom-themes-padded-modeline t)         ; add padding around modeline text
  (load-theme 'doom-one t)                     ; dark theme — toggled via SPC t T
  (doom-themes-visual-bell-config)             ; flash modeline instead of audible bell
  (doom-themes-org-config))                    ; better org-mode fontification

(when (my/module-enabled-p 'evil)
  (my/safe-require-module 'evil-config "Evil mode"))

;; Dashboard must load in Phase 1 (not deferred) so it displays
;; immediately instead of showing *scratch* first.
(when (my/module-enabled-p 'dashboard)
  (my/safe-require-module 'startup-dashboard "Startup dashboard"))

;; ── Phase 2: Idle 0.5s — completion (needed for M-x) ─────────────
(when (my/module-enabled-p 'completion)
  (my/deferred-require 0.5 'modern-completion "Completion system"))

;; ── Phase 4: Idle 1s — languages, UI polish, colors ─────────────
(when (my/module-enabled-p 'languages)
  (my/deferred-require 1 'modern-languages "Language support"))
(when (my/module-enabled-p 'ui)
  (my/deferred-require 1 'modern-ui "Modern UI"))
(when (my/module-enabled-p 'colors)
  (my/deferred-require 1 'enhanced-colors "Syntax highlighting"))

;; ── Phase 5: Idle 2s — non-critical ─────────────────────────────
(when (my/module-enabled-p 'robustness)
  (my/deferred-require 2 'robustness-enhancements "Robustness"))

;; ══════════════════════════════════════════════════════════════════
;;  Standalone Packages (not tied to a module)
;; ══════════════════════════════════════════════════════════════════
;; Packages that are broadly useful and don't belong to any specific
;; module.  Each is deferred to avoid slowing startup.

;; which-key — shows available keybindings in a popup after pressing
;; a prefix key (e.g. press SPC and wait 0.3s to see all SPC bindings).
;; Essential for discovering the leader key layout without memorization.
(use-package which-key
  :straight t
  :defer 0.5
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3              ; show popup 0.3s after prefix key
        which-key-popup-type 'side-window     ; display at bottom, not in minibuffer
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.25 ; use at most 25% of frame height
        which-key-separator " → "))           ; visual separator between key and command

;; helpful — enhanced *Help* buffers with source code, references,
;; and better formatting.  Remaps built-in describe-* commands so
;; C-h f, C-h v, C-h k all use the improved versions.
(use-package helpful
  :straight t
  :defer 2
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command))

;; undo-tree — visualize undo history as a tree (C-x u).
;; Emacs's default undo is linear; undo-tree preserves ALL branches
;; so you never lose work when you undo past a branch point.
;; History is persisted to disk so undo survives across sessions.
(use-package undo-tree
  :straight t
  :defer 1
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t          ; persist undo history to disk
        undo-tree-history-directory-alist       ; store all history files in one place
        `(("." . ,(expand-file-name "undo-tree" user-emacs-directory)))))

;; diminish — hide minor-mode lighters from the modeline to reduce
;; clutter (e.g. "yas" "SP" "undo-tree" no longer shown).
(use-package diminish
  :straight t
  :defer 1)

;; restart-emacs — provides `restart-emacs` command used by upgrade
;; and rollback flows (SPC q r, post-upgrade prompt).
(use-package restart-emacs
  :straight t
  :defer t
  :commands restart-emacs)

;; ── Deferred config files (org, markdown, debug) ─────────────────
;; These are standalone config files that don't fit the module system.
;; They load after 2s idle since org-mode, markdown, and DAP debugging
;; are never needed immediately at startup.  The second arg `t` to
;; `load` suppresses "file not found" errors so missing files are fine.
(let ((load-configs (lambda ()
                      (load (expand-file-name "config/org-config.el" user-emacs-directory) t)
                      (load (expand-file-name "config/markdown.el" user-emacs-directory) t)
                      (load (expand-file-name "modules/debug-support.el" user-emacs-directory) t))))
  (if noninteractive
      (funcall load-configs)
    (run-with-idle-timer 2 nil load-configs)))

;; ══════════════════════════════════════════════════════════════════
;;  Profiling (opt-in: EMACS_BENCHMARK=1 emacs)
;; ══════════════════════════════════════════════════════════════════
;; Run `EMACS_BENCHMARK=1 emacs` to see a table of require/load
;; times in the *benchmark-init results* buffer.  Deactivated after
;; init to avoid overhead during normal editing.

;; eval prevents the byte-compiler from expanding use-package at compile time,
;; which would cause "Cannot load benchmark-init" when not yet installed.
(when (getenv "EMACS_BENCHMARK")
  (eval
   '(use-package benchmark-init
      :straight t
      :demand t
      :config
      (add-hook 'after-init-hook #'benchmark-init/deactivate))))

;; ══════════════════════════════════════════════════════════════════
;;  Startup Complete — restore redisplay, report timing
;; ══════════════════════════════════════════════════════════════════
;; This hook fires after init.el finishes AND after all startup files
;; have been processed.  We re-enable redisplay (suppressed at the top
;; of this file) and report how long the synchronous portion took.
;; Note: deferred modules (Phases 2-5) continue loading in the
;; background and are NOT included in this timing.

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
            (redisplay)
            (message "Emacs ready in %.2fs"
                     (float-time (time-since emacs-start-time)))))

(provide 'init)
;;; init.el ends here
