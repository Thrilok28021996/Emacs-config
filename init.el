;;; init.el --- Emacs 31.1 built-in-only configuration -*- lexical-binding: t; -*-

;; This configuration intentionally uses only libraries shipped with Emacs.
;; External packages previously used here (ELPA/MELPA/:vc) are not loaded.

(when (eq system-type 'darwin)
  (when (and (not noninteractive)
             (fboundp 'redirect-debugging-output))
    (redirect-debugging-output (expand-file-name "emacs-stderr.log" user-emacs-directory)))
  (let ((mac-version (string-trim (shell-command-to-string "sw_vers -productVersion"))))
    (unless (string-empty-p mac-version)
      (setenv "MACOSX_DEPLOYMENT_TARGET" mac-version))))

;;; Startup and UI

(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      initial-scratch-message
      (concat ";;; GNU Emacs " emacs-version "\n"
              ";;; Built-in-only configuration\n\n")
      read-process-output-max (* 1024 1024)
      fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      redisplay-skip-fontification-on-input t
      scroll-conservatively 101
      scroll-margin 8
      use-short-answers t
      load-prefer-newer t)

(advice-add 'display-startup-echo-area-message :override #'ignore)

(defun my/set-font ()
  (cond
   ((find-font (font-spec :name "Victor Mono"))
    (set-face-attribute 'default nil :font "Victor Mono-18"))
   ((find-font (font-spec :name "VictorMono Nerd Font"))
    (set-face-attribute 'default nil :font "VictorMono Nerd Font-18"))
   ((find-font (font-spec :name "Menlo"))
    (set-face-attribute 'default nil :font "Menlo-14"))))

(unless (daemonp) (my/set-font))
(add-hook 'after-make-frame-functions
          (lambda (frame) (with-selected-frame frame (my/set-font))))

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(defvar pixel-scroll-precision-interpolate-page nil)
(defvar pixel-scroll-precision-use-momentum nil)
(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-use-momentum t)
(tab-bar-mode 1)
(setq tab-bar-show 1
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-close-button-show nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'subword-mode)
(electric-pair-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(global-so-long-mode 1)
(repeat-mode 1)
(defvar which-key-idle-delay 0.35)
(add-hook 'after-init-hook
          (lambda ()
            (require 'which-key)
            (which-key-mode 1)))

;;; Built-in completion and navigation

(require 'icomplete)
(fido-vertical-mode 1)
(setq completion-styles '(flex basic)
      completion-category-defaults nil
      completion-category-overrides
      '((file (styles basic partial-completion))))

(require 'completion-preview)
;; Completion previews are useful for code but unnecessarily noisy in prose.
(add-hook 'prog-mode-hook #'completion-preview-mode)
(setq completion-preview-minimum-symbol-length 2)

(require 'savehist)
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      recentf-exclude '("/elpa/" "/backups/" "/\\.git/" "recentf" "history" "saveplace"))
(require 'saveplace)
(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(require 'winner)
(winner-mode 1)

(global-set-key (kbd "C-c b b") #'switch-to-buffer)
(global-set-key (kbd "C-c f r") #'recentf-open-files)
(global-set-key (kbd "C-c s s") #'isearch-forward)
(global-set-key (kbd "C-c s i") #'imenu)
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w U") #'winner-redo)
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "M-o") #'other-window)
(global-set-key (kbd "C-x C-/") #'undo-redo)
(global-set-key (kbd "C-c z") #'undo-redo)

(defun my/move-line-up ()
  "Move the current line or active region up."
  (interactive)
  (if (use-region-p)
      (let ((text (delete-and-extract-region (region-beginning) (region-end))))
        (forward-line -1) (insert text))
    (transpose-lines 1) (forward-line -2)))

(defun my/move-line-down ()
  "Move the current line or active region down."
  (interactive)
  (if (use-region-p)
      (let ((text (delete-and-extract-region (region-beginning) (region-end))))
        (forward-line 1) (insert text))
    (forward-line 1) (transpose-lines 1) (forward-line -1)))

(global-set-key (kbd "M-<up>") #'my/move-line-up)
(global-set-key (kbd "M-<down>") #'my/move-line-down)
(global-set-key (kbd "C-c i r") #'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-c i l") #'indent-rigidly-left-to-tab-stop)

;;; Optional packages selected for capability, not startup speed

(defun my/add-elpa-package (pattern)
  "Add the newest installed ELPA directory matching PATTERN to `load-path'."
  (let ((dirs (sort (file-expand-wildcards
                    (expand-file-name (concat "elpa/" pattern) user-emacs-directory)
                    t)
                   #'string>)))
    (when dirs (add-to-list 'load-path (car dirs)))))

;; Doom themes, Org-roam, Conda, and Magit are the selected external packages.
(dolist (pattern '("doom-themes-[0-9]*" "org-roam-[0-9]*" "emacsql-[0-9]*"
                   "magit-section-[0-9]*" "magit-[0-9]*" "with-editor-[0-9]*"
                   "cond-let-[0-9]*" "llama-[0-9]*"
                   "conda-[0-9]*" "dash-[0-9]*" "s-[0-9]*" "f-[0-9]*"
                   "pythonic-[0-9]*"))
  (my/add-elpa-package pattern))

;;; Project, version control, compilation, and diagnostics

(require 'project)
(require 'vc)
(define-key ctl-x-map "p" project-prefix-map)
(global-set-key (kbd "C-c p") project-prefix-map)
(setq project-vc-extra-root-markers
      '("pyproject.toml" "setup.py" "package.json" "CMakeLists.txt" "Makefile"))

(require 'eglot)
(require 'flymake)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-ts-mode-hook #'eglot-ensure)
(add-hook 'c++-ts-mode-hook #'eglot-ensure)
(setq eglot-events-buffer-config '(:size 0)
      eglot-autoshutdown t
      eglot-sync-connect nil
      eglot-extend-to-xref t
      eglot-report-progress t)
(global-set-key (kbd "C-c c r") #'eglot-rename)
(global-set-key (kbd "C-c c a") #'eglot-code-actions)
(global-set-key (kbd "C-c c i") #'eglot-find-implementation)
(global-set-key (kbd "C-c c h") #'eglot-inlay-hints-mode)
(global-set-key (kbd "C-c e n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c e b") #'flymake-show-buffer-diagnostics)

(require 'treesit)
(setq treesit-font-lock-level 3)
(dolist (mapping '((python-mode . python-ts-mode)
                   (c-mode . c-ts-mode)
                   (c++-mode . c++-ts-mode)
                   (js-mode . js-ts-mode)
                   (json-mode . json-ts-mode)
                   (css-mode . css-ts-mode)
                   (typescript-mode . typescript-ts-mode)
                   (sh-mode . bash-ts-mode)))
  (when (treesit-language-available-p
         (pcase (cdr mapping)
           ('python-ts-mode 'python) ('c-ts-mode 'c) ('c++-ts-mode 'cpp)
           ('js-ts-mode 'javascript) ('json-ts-mode 'json) ('css-ts-mode 'css)
           ('typescript-ts-mode 'typescript) ('bash-ts-mode 'bash)))
    (add-to-list 'major-mode-remap-alist mapping)))

(global-set-key (kbd "C-c c c") #'compile)
(global-set-key (kbd "C-c c d") #'xref-find-references)
(global-set-key (kbd "C-c c f") #'eglot-format-buffer)

(defun my/python-run-current-file ()
  "Run the current Python file with python3."
  (interactive)
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (compile (format "python3 %s" (shell-quote-argument buffer-file-name))))
(global-set-key (kbd "C-c c p") #'my/python-run-current-file)

;; Magit replaces VC for the interactive Git workflow, but is loaded lazily.
(load "magit-autoloads" nil t)
(declare-function magit-status "magit-status" ())
(declare-function magit-commit "magit-commit" ())
(declare-function magit-push "magit-push" ())
(declare-function magit-pull "magit-pull" ())
(declare-function magit-fetch "magit-fetch" ())
(declare-function magit-log-current "magit-log" ())
(declare-function magit-diff-dwim "magit-diff" ())
(declare-function magit-branch "magit" ())
(declare-function magit-blame "magit-blame" ())
(declare-function magit-stash "magit-stash" ())
(global-set-key (kbd "C-c g g") #'magit-status)
(global-set-key (kbd "C-c g c") #'magit-commit)
(global-set-key (kbd "C-c g p") #'magit-push)
(global-set-key (kbd "C-c g u") #'magit-pull)
(global-set-key (kbd "C-c g f") #'magit-fetch)
(global-set-key (kbd "C-c g l") #'magit-log-current)
(global-set-key (kbd "C-c g d") #'magit-diff-dwim)
(global-set-key (kbd "C-c g b") #'magit-branch)
(global-set-key (kbd "C-c g a") #'magit-blame)
(global-set-key (kbd "C-c g s") #'magit-stash)

;;; Theme

(if (require 'doom-themes nil t)
    (progn
      (defvar doom-themes-enable-bold nil)
      (defvar doom-themes-enable-italic nil)
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t)
      (load-theme 'doom-one t))
  ;; Fallback when the optional theme package is not installed.
  (load-theme 'modus-vivendi t))

(defun my/toggle-theme ()
  "Toggle between the preferred Doom theme and a light fallback."
  (interactive)
  (cond
   ((custom-theme-enabled-p 'doom-one)
    (disable-theme 'doom-one)
    (if (custom-theme-p 'doom-one-light)
        (load-theme 'doom-one-light t)
      (load-theme 'modus-operandi t)))
   ((custom-theme-enabled-p 'doom-one-light)
    (disable-theme 'doom-one-light)
    (load-theme 'doom-one t))
   ((custom-theme-enabled-p 'modus-vivendi)
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t))
   (t
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t))))
(global-set-key (kbd "C-c t t") #'my/toggle-theme)

;;; Org: built-in notes, capture, agenda, and clocking

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)

(defvar my/garden-dir (expand-file-name "~/Documents/garden/"))
(defvar my/work-dir (expand-file-name "~/Documents/garden/work/"))
(defvar my/personal-dir (expand-file-name "~/Documents/garden/personal/"))
(setq org-directory my/garden-dir
      org-log-done 'time
      org-log-into-drawer t
      org-clock-mode-line-total 'today
      org-startup-indented t
      org-hide-emphasis-markers t
      org-return-follows-link t
      org-agenda-files nil
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-clock-persist t
      org-enforce-todo-dependencies t
      org-agenda-window-setup 'current-window
      org-image-actual-width nil
      org-agenda-inhibit-startup t
      org-fold-catch-invisible-edits 'smart
      org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)"))
      org-archive-location "%s_archive::* Archived")
(org-clock-persistence-insinuate)

(defun my/capture-inbox () (interactive) (org-capture nil "i"))
(defun my/capture-journal () (interactive) (org-capture nil "j"))
(defun my/capture-reading () (interactive) (org-capture nil "r"))
(defun my/capture-work-task () (interactive) (org-capture nil "w"))
(defun my/capture-personal-task () (interactive) (org-capture nil "t"))
(setq org-capture-templates
      `(("i" "Inbox" entry (file+headline ,(expand-file-name "inbox.org" my/garden-dir) "Tasks")
         "* TODO %?\n  %U\n  %a")
        ("j" "Journal" entry (file+olp+datetree ,(expand-file-name "journal.org" my/garden-dir))
         "* %U\n%?" :empty-lines 1)
        ("r" "Reading" entry (file+headline ,(expand-file-name "reading.org" my/garden-dir) "Reading")
         "* %^{Book/Resource}\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?" :empty-lines 1)
        ("w" "Work task" entry (file+headline ,(expand-file-name "tasks.org" my/work-dir) "Tasks")
         "* TODO %?\n  %U\n  %a" :empty-lines 1)
        ("t" "Personal task" entry (file+headline ,(expand-file-name "tasks.org" my/personal-dir) "Tasks")
         "* TODO %?\n  %U\n  %a" :empty-lines 1)))

(global-set-key (kbd "C-c n i") #'my/capture-inbox)
(global-set-key (kbd "C-c n j") #'my/capture-journal)
(global-set-key (kbd "C-c n r") #'my/capture-reading)
(global-set-key (kbd "C-c n w") #'my/capture-work-task)
(global-set-key (kbd "C-c n t") #'my/capture-personal-task)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o I") #'org-clock-in)
(global-set-key (kbd "C-c o O") #'org-clock-out)
(global-set-key (kbd "C-c o R") #'org-clock-report)

;;; Org-roam: linked notes and SQLite index

(autoload 'org-roam-node-find "org-roam-node" nil t)
(autoload 'org-roam-node-insert "org-roam-node" nil t)
(autoload 'org-roam-capture "org-roam-capture" nil t)
(autoload 'org-roam-db-autosync-mode "org-roam-db" nil t)
(global-set-key (kbd "C-c o r") #'org-roam-node-find)
(global-set-key (kbd "C-c o i") #'org-roam-node-insert)
(global-set-key (kbd "C-c n k") #'org-roam-capture)

(defvar org-roam-directory nil)
(defvar org-roam-db-gc-threshold nil)
(defun my/org-roam-enable ()
  "Enable Org-roam after startup using Emacs's built-in SQLite support."
  (when (and (file-directory-p my/garden-dir)
             (require 'org-roam nil t))
    (setq org-roam-directory my/garden-dir)
    (setq org-roam-db-gc-threshold most-positive-fixnum)
    (org-roam-db-autosync-mode 1)))
(run-with-idle-timer 5 nil #'my/org-roam-enable)

;;; Conda environment switching

(load "conda-autoloads" nil t)
(declare-function conda-env-activate "conda" (&optional name))
(declare-function conda-env-deactivate "conda" ())
(defvar conda-anaconda-home nil)
(defvar conda-env-home-directory nil)
(defvar conda-env-subdirectory "envs")
(defvar conda-message-on-environment-switch t)
(setq conda-anaconda-home "/opt/homebrew/Caskroom/miniconda/base"
      conda-env-home-directory conda-anaconda-home
      conda-message-on-environment-switch nil)
(global-set-key (kbd "C-c v a") #'conda-env-activate)
(global-set-key (kbd "C-c v d") #'conda-env-deactivate)

;;; Persistence and macOS integration

(setq auto-save-default nil
      backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)
(auto-save-visited-mode 1)
(require 'autorevert)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (setq interprogram-cut-function
        (lambda (text) (with-temp-buffer (insert text) (call-process-region (point-min) (point-max) "pbcopy")))
        interprogram-paste-function
        (lambda () (with-temp-buffer (call-process "pbpaste" nil t nil) (buffer-string)))))

(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              c-basic-offset 2
              python-indent-offset 2
              js-indent-level 2
              css-indent-offset 2)

;;; Org knowledge system (see lisp/ and ORG-PKM.org)
;; Loaded last so it can override the garden-oriented Org settings above.
;; Each module is small; together they add a few milliseconds of load time.

(let ((my/lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
  (when (file-directory-p my/lisp-dir)
    (add-to-list 'load-path my/lisp-dir)
    (dolist (module '(my-org-pkm my-help))
      (require module nil t))))

;;; init.el ends here
