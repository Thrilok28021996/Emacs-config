;;; modules/debug-support.el --- Debugging support with DAP mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Debug Adapter Protocol (DAP) integration, loaded on-demand when
;; you first call `dap-debug` or `dap-breakpoint-toggle`.
;;
;; Architecture:
;;   dap-mode        — core DAP client (talks to debug adapters)
;;   dap-ui          — sidebar windows (locals, breakpoints, REPL)
;;   dap-python      — Python adapter using debugpy
;;   dap-gdb-lldb    — C/C++ adapter using LLDB (macOS) or GDB
;;
;; Requirements (install externally):
;;   Python:  pip install debugpy
;;   C/C++:   lldb-vscode (ships with Xcode on macOS)
;;            or gdb (install via Homebrew)
;;
;; Usage:
;;   M-x my/dap-debug-current-file  — auto-detect language and debug
;;   SPC d b                         — toggle breakpoint at point
;;   SPC d d                         — debug current file (smart)
;;   SPC d h                         — show all debug commands (hydra)
;;
;; Debug templates are pre-configured for:
;;   Python: run file, run with args, pytest, Flask, Django
;;   C/C++:  run binary (LLDB), run with args, GDB fallback

;;; Code:

(require 'utilities)  ; For defer timing constants

;; Silence byte-compiler warnings for deferred functions/variables
(declare-function dap-debug "dap-mode")
(declare-function dap-register-debug-template "dap-mode")
(declare-function which-function "which-func")
(defvar compilation-in-progress)

;; ══════════════════════════════════════════════════════════════════
;;  1. DAP Core Configuration
;; ══════════════════════════════════════════════════════════════════
;; dap-mode implements the Debug Adapter Protocol — the same protocol
;; VS Code uses for debugging.  It communicates with language-specific
;; debug adapters over JSON-RPC.

(use-package dap-mode
  :straight t
  :defer t                                     ; only load when a debug command is invoked
  :commands (dap-debug dap-hydra dap-breakpoint-toggle)
  :config
  (dap-mode 1)                                 ; enable DAP protocol handling
  (dap-ui-mode 1)                              ; show locals/breakpoints/sessions sidebars
  (dap-tooltip-mode 1)                         ; hover over variable → show value
  (tooltip-mode 1)                             ; ensure tooltips are enabled globally

  ;; Which UI panels to auto-configure when a debug session starts
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))

  ;; Window layout for debug UI panels.
  ;; Right side: locals, expressions, sessions (stacked vertically)
  ;; Left side: breakpoints list
  ;; Bottom: REPL and console output
  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*" . ((side . right) (slot . 1) (window-width . 0.30)))
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.30)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.30)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . 0.25)))
          (,"*dap-ui-repl*" . ((side . bottom) (slot . 1) (window-height . 0.25)))
          (,"*dap-ui-console*" . ((side . bottom) (slot . 2) (window-height . 0.25)))))

  ;; Automatically show debug windows when debugging starts
  (setq dap-auto-show-output t)

  ;; Don't ask for confirmation when debugging
  (setq dap-auto-configure-mode t))

;; ══════════════════════════════════════════════════════════════════
;;  2. Python Debugging (debugpy)
;; ══════════════════════════════════════════════════════════════════
;; Uses debugpy (Microsoft's Python debug adapter, same as VS Code).
;; Install: pip install debugpy
;; Templates cover common scenarios: script, pytest, Flask, Django.

(use-package dap-python
  :straight nil  ; Included with dap-mode
  :after dap-mode
  :config
  ;; Use debugpy (modern Python debugger)
  (setq dap-python-debugger 'debugpy)

  ;; Auto-detect Python executable from conda/venv
  (setq dap-python-executable "python3")

  ;; Default debug template
  (dap-register-debug-template
   "Python :: Run file"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :request "launch"
         :name "Python :: Run file"))

  (dap-register-debug-template
   "Python :: Run file with arguments"
   (list :type "python"
         :args "-i"
         :cwd nil
         :program nil
         :request "launch"
         :name "Python :: Run file with arguments"))

  (dap-register-debug-template
   "Python :: Run pytest"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :module "pytest"
         :request "launch"
         :name "Python :: Run pytest"))

  ;; Flask debugging template
  (dap-register-debug-template
   "Python :: Flask"
   (list :type "python"
         :args "--no-debugger --no-reload"
         :cwd nil
         :env '(("FLASK_APP" . "app.py"))
         :target-module "flask"
         :request "launch"
         :name "Python :: Flask"))

  ;; Django debugging template
  (dap-register-debug-template
   "Python :: Django"
   (list :type "python"
         :args "runserver --noreload --nothreading"
         :cwd nil
         :program nil
         :module "django"
         :request "launch"
         :name "Python :: Django")))

;; ══════════════════════════════════════════════════════════════════
;;  3. C/C++ Debugging (LLDB / GDB)
;; ══════════════════════════════════════════════════════════════════
;; Uses lldb-vscode on macOS (ships with Xcode command line tools).
;; GDB template is available as a fallback for Linux or preference.

(use-package dap-gdb-lldb
  :straight nil  ; Included with dap-mode
  :after dap-mode
  :config
  ;; Use LLDB on macOS (better than GDB on macOS)
  (setq dap-gdb-lldb-debug-program '("lldb-vscode"))

  ;; C++ debug templates
  (dap-register-debug-template
   "C++ :: Run binary"
   (list :type "lldb"
         :request "launch"
         :name "C++ :: Run binary"
         :target nil
         :cwd nil))

  (dap-register-debug-template
   "C++ :: Run with arguments"
   (list :type "lldb"
         :request "launch"
         :name "C++ :: Run with arguments"
         :args ""
         :target nil
         :cwd nil))

  ;; GDB template (if user prefers GDB)
  (dap-register-debug-template
   "C++ :: GDB Run"
   (list :type "gdb"
         :request "launch"
         :name "C++ :: GDB Run"
         :target nil
         :cwd nil)))

;; ══════════════════════════════════════════════════════════════════
;;  4. Smart Debug Helpers
;; ══════════════════════════════════════════════════════════════════
;; my/dap-debug-current-file — detects the file extension and
;; launches the appropriate debug adapter.  For C++, it compiles
;; with -g -O0 first if the binary is missing or outdated.

(defun my/dap-debug-current-file ()
  "Debug the current Python or C++ file intelligently."
  (interactive)
  (let ((extension (file-name-extension (buffer-file-name))))
    (cond
     ;; Python
     ((string= extension "py")
      (dap-debug
       (list :type "python"
             :args ""
             :cwd (file-name-directory (buffer-file-name))
             :program (buffer-file-name)
             :request "launch"
             :name "Python :: Debug current file")))

     ;; C++
     ((member extension '("cpp" "cc" "c"))
      (let* ((source-file (buffer-file-name))
             (binary-file (file-name-sans-extension source-file)))
        ;; Compile first if needed
        (when (or (not (file-exists-p binary-file))
                  (file-newer-than-file-p source-file binary-file))
          (compile (format "g++ -std=c++20 -g -O0 %s -o %s"
                           (shell-quote-argument source-file)
                           (shell-quote-argument binary-file)))
          (while compilation-in-progress
            (sit-for 0.1)))
        ;; Debug
        (dap-debug
         (list :type "lldb"
               :request "launch"
               :name "C++ :: Debug current file"
               :target binary-file
               :cwd (file-name-directory source-file)))))

     ;; Unsupported
     (t (message "Debugging not supported for .%s files" extension)))))

(defun my/dap-python-test-method ()
  "Debug the current Python test method under cursor."
  (interactive)
  (let* ((test-file (buffer-file-name))
         (test-method (which-function)))
    (dap-debug
     (list :type "python"
           :args (format "%s::%s" test-file test-method)
           :cwd (file-name-directory test-file)
           :program nil
           :module "pytest"
           :request "launch"
           :name "Python :: Debug test method"))))

;; ══════════════════════════════════════════════════════════════════
;;  5. Keybinding Reference (SPC d prefix)
;; ══════════════════════════════════════════════════════════════════
;; These keybindings should be defined in evil-config.el under the
;; SPC d prefix.  Listed here as documentation.

;; These keybindings should be added to evil-leader/set-key in evil-config.el:
;;
;; Debug operations
;; "d d" 'my/dap-debug-current-file    ; Debug current file (smart)
;; "d D" 'dap-debug                    ; Debug with template selection
;; "d b" 'dap-breakpoint-toggle        ; Toggle breakpoint
;; "d B" 'dap-breakpoint-condition     ; Conditional breakpoint
;; "d l" 'dap-breakpoint-log-message   ; Log point
;; "d a" 'dap-breakpoint-add           ; Add breakpoint
;; "d r" 'dap-breakpoint-delete        ; Remove breakpoint
;; "d R" 'dap-breakpoint-delete-all    ; Remove all breakpoints
;;
;; Debug execution
;; "d c" 'dap-continue                 ; Continue execution
;; "d n" 'dap-next                     ; Step over
;; "d s" 'dap-step-in                  ; Step into
;; "d o" 'dap-step-out                 ; Step out
;; "d t" 'dap-restart-frame            ; Restart frame
;; "d q" 'dap-disconnect               ; Stop debugging
;; "d Q" 'dap-delete-all-sessions      ; Kill all debug sessions
;;
;; Debug inspection
;; "d e" 'dap-eval                     ; Evaluate expression
;; "d E" 'dap-eval-thing-at-point      ; Evaluate at point
;; "d i" 'dap-ui-inspect-thing-at-point ; Inspect at point
;; "d w" 'dap-ui-expressions-add       ; Add watch expression
;; "d W" 'dap-ui-expressions-remove    ; Remove watch expression
;;
;; Debug UI
;; "d u" 'dap-ui-repl                  ; Open REPL
;; "d h" 'dap-hydra                    ; Show all debug commands
;; "d L" 'dap-ui-locals                ; Show local variables
;; "d S" 'dap-ui-sessions              ; Show debug sessions
;; "d B" 'dap-ui-breakpoints           ; Show breakpoints window
;;
;; Python-specific
;; "d p" 'my/dap-python-test-method    ; Debug current test method

(provide 'debug-support)
;;; debug-support.el ends here
