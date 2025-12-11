;;; modules/debug-support.el --- Debugging support with DAP mode -*- lexical-binding: t; -*-

;;; Commentary:
;;; DAP (Debug Adapter Protocol) integration for Python and C++ debugging
;;; Provides breakpoints, step-through debugging, and variable inspection

;;; Code:

(require 'utilities)  ; For defer timing constants

;; --- DAP Mode (Debug Adapter Protocol) ---

(use-package dap-mode
  :straight t
  :defer t
  :commands (dap-debug dap-hydra dap-breakpoint-toggle)
  :config
  ;; Enable DAP features
  (dap-mode 1)
  (dap-ui-mode 1)        ; Enable UI features (locals, breakpoints windows)
  (dap-tooltip-mode 1)   ; Show values on hover
  (tooltip-mode 1)       ; Enable tooltips globally for dap

  ;; UI Configuration
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl controls tooltip))

  ;; Better UI layout
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

;; --- Python Debugging ---

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

;; --- C/C++ Debugging ---

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

;; --- Debugging Helper Functions ---

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

;; --- Keybindings (to be added to evil-config.el) ---

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
