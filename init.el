;;; init.el --- Emacs 30.2 Configuration -*- lexical-binding: t; -*-
;;
;; Built-ins: use-package, eglot, flymake, project, electric-pair,
;;            treesit, which-key, pixel-scroll-precision, repeat,
;;            savehist, recentf, save-place, winner, sqlite.

;; macOS: some native modules drag in a second glib, and the objc runtime
;; prints a duplicate-class warning straight to stderr — over the tty in -nw.
;; No env var silences it; repoint this process's stderr to a log instead.
(when (eq system-type 'darwin)
  (when (and (not noninteractive)
             (fboundp 'redirect-debugging-output))
    (redirect-debugging-output "/tmp/emacs-stderr.log"))
  ;; Fix native-comp "invalid version number in -mmacosx-version-min" error
  ;; by explicitly overriding the deployment target to match the current OS.
  (let ((mac-version (string-trim (shell-command-to-string "sw_vers -productVersion"))))
    (setenv "MACOSX_DEPLOYMENT_TARGET" mac-version)))

;;; ─────────────────────────────────────────────
;;; 1. PACKAGE BOOTSTRAP
;;; ─────────────────────────────────────────────

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

;;; ─────────────────────────────────────────────
;;; 2. PATH (macOS)
;;; ─────────────────────────────────────────────

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :defer 1
  :custom
  (exec-path-from-shell-arguments '("-l"))   ; login shell only, not interactive — skips ~/.zshrc
  :config
  (dolist (var '("PATH" "MANPATH" "PYTHONPATH" "CONDA_PREFIX"
                 "CONDA_DEFAULT_ENV"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;; ─────────────────────────────────────────────
;;; 3. UI & PERFORMANCE
;;; ─────────────────────────────────────────────

(setq inhibit-startup-message t)

;; *scratch* in fundamental-mode skips prog-mode startup hooks
(setq initial-major-mode 'fundamental-mode)
(advice-add 'display-startup-echo-area-message :override #'ignore)

;; Modern, minimal scratch buffer launcher
(defun my/scratch-banner ()
  (concat
   ";;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
   ";;;  GNU Emacs " emacs-version "  •  " (format-time-string "%A, %d %B %Y") "\n"
   ";;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
   ";;;\n"
   ";;;  [C-x p p]  Projects           [C-c f r]  Recent files\n"
   ";;;  [C-c o a]  Org Agenda         [C-c o r]  Roam Zettelkasten\n"
   ";;;  [C-c g g]  Magit Status       [C-c a c]  GPTel AI Assistant\n"
   ";;;  [C-c t t]  Toggle Dark/Light  [M-o]      Switch Window\n"
   ";;;  [M-up/dn]  Move Line / Block  [C-c c h]  Toggle Inlay Hints\n"
   ";;;\n\n"))
(setq initial-scratch-message (my/scratch-banner))

(setq display-line-numbers-type t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; 1MB subprocess buffer (default is 4KB) — massive throughput boost for eglot & ripgrep
(setq read-process-output-max (* 1024 1024))

;; Fast, stutter-free rendering on macOS
(setq fast-but-imprecise-scrolling t
      auto-window-vscroll nil
      redisplay-skip-fontification-on-input t)

;; Garbage Collector Magic Hack — raise GC threshold during activity, collect on idle
(use-package gcmh
  :demand t
  :config
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 128 1024 1024))
  (gcmh-mode 1))

;; Silky trackpad pixel scrolling — enable unconditionally so daemon +
;; emacsclient -c gets it too (harmless in terminal: no wheel events there)
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-use-momentum t)

(defun my/set-font ()
  (cond
   ((find-font (font-spec :name "Victor Mono"))
    (set-face-attribute 'default nil :font "Victor Mono-24"))
   ((find-font (font-spec :name "VictorMono Nerd Font"))
    (set-face-attribute 'default nil :font "VictorMono Nerd Font-24"))
   ((find-font (font-spec :name "VictorMono NF"))
    (set-face-attribute 'default nil :font "VictorMono NF-24"))
   ((find-font (font-spec :name "Menlo"))
    (set-face-attribute 'default nil :font "Menlo-16"))))

(unless (daemonp) (my/set-font))
(add-hook 'after-make-frame-functions
          (lambda (f) (with-selected-frame f (my/set-font))))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config (tab-bar-mode 1))

;;; ─────────────────────────────────────────────
;;; 4. KEYBINDINGS
;;; ─────────────────────────────────────────────

;; Silence native-compiler: these built-ins live in autoloaded files not yet
;; require'd at compile time — declare-function tells AOT they're real.
(declare-function winner-undo                        "winner")
(declare-function winner-redo                        "winner")
(declare-function electric-pair-conservative-inhibit "elec-pair")

;; Silence native-compiler: my/toggle-theme is defun'd inside doom-themes :config
(declare-function my/toggle-theme nil)

;; Undo / Redo — terminal-safe bindings
;; C-? sends DEL in terminal (-nw) → use C-x C-/ instead which reliably reaches Emacs.
;; C-/ is the built-in undo; C-x C-/ is redo.
(global-set-key (kbd "C-x C-/") #'undo-redo)
;; Also bind on C-c z for quick redo (mnemonic: z = undo in many editors, Z = redo)
(global-set-key (kbd "C-c z") #'undo-redo)

(global-set-key (kbd "M-o") #'other-window)

;; ── Navigation Pulse (tactile visual feedback) ─
(require 'pulse)
(defun my/pulse-momentary-line (&rest _)
  "Briefly pulse the current line with a subtle highlight."
  (pulse-momentary-highlight-one-line (point) 'highlight))

(dolist (cmd '(other-window
               windmove-left windmove-right windmove-up windmove-down
               consult-line consult-ripgrep
               xref-find-definitions xref-find-references
               avy-goto-char-2))
  (advice-add cmd :after #'my/pulse-momentary-line))

;; ── Line & Region Moving (M-up / M-down) ──────
(defun my/move-line-up ()
  "Move current line or active region up."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (let ((text (delete-and-extract-region start end)))
          (forward-line -1)
          (let ((new-start (point)))
            (insert text)
            (set-mark new-start)
            (setq deactivate-mark nil))))
    (transpose-lines 1)
    (forward-line -2)))

(defun my/move-line-down ()
  "Move current line or active region down."
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))
        (let ((text (delete-and-extract-region start end)))
          (forward-line 1)
          (let ((new-start (point)))
            (insert text)
            (set-mark new-start)
            (setq deactivate-mark nil))))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-<up>")   #'my/move-line-up)
(global-set-key (kbd "M-<down>") #'my/move-line-down)

(global-set-key (kbd "C-c i r") #'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-c i l") #'indent-rigidly-left-to-tab-stop)

;; ── Buffers (b) ──────────────────────────────
;; Remap built-in switch-to-buffer so C-x b invokes consult-buffer everywhere
(global-set-key [remap switch-to-buffer] #'consult-buffer)
(global-set-key (kbd "C-c b b")   #'consult-buffer)
(global-set-key (kbd "C-c b TAB") #'mode-line-other-buffer)

;; ── Code (c): LSP + format + compile ─────────
(global-set-key (kbd "C-c c r") #'eglot-rename)
(global-set-key (kbd "C-c c a") #'eglot-code-actions)
(global-set-key (kbd "C-c c i") #'eglot-find-implementation)
(global-set-key (kbd "C-c c d") #'xref-find-references)  ; also on built-in M-?
(global-set-key (kbd "C-c c h") #'eglot-inlay-hints-mode)
(global-set-key (kbd "C-c c f") #'apheleia-format-buffer)
(global-set-key (kbd "C-c c p") #'my/python-run-current-file)
(global-set-key (kbd "C-c c c") #'my/cpp-compile-run-current-file)
(global-set-key (kbd "C-c c b") #'dape-breakpoint-toggle)
(global-set-key (kbd "C-c c B") #'dape)

;; ── Errors (e): flymake (built-in M-g map + C-c e shortcuts) ──
(global-set-key (kbd "M-g f")   #'consult-flymake)
(global-set-key (kbd "M-g b")   #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c e l") #'consult-flymake)
(global-set-key (kbd "C-c e n") #'flymake-goto-next-error)  ; built-in M-g n
(global-set-key (kbd "C-c e p") #'flymake-goto-prev-error)  ; built-in M-g p
(global-set-key (kbd "C-c e b") #'flymake-show-buffer-diagnostics)

;; ── Files (f) ─────────────────────────────────
(global-set-key (kbd "C-c f r") #'consult-recent-file)

;; ── Git (g): magit + hunks ────────────────────
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
(global-set-key (kbd "C-c g h") #'diff-hl-show-hunk)
(global-set-key (kbd "C-c g n") #'diff-hl-next-hunk)
(global-set-key (kbd "C-c g N") #'diff-hl-previous-hunk)

;; ── Theme (t) ─────────────────────────────────
(global-set-key (kbd "C-c t t") #'my/toggle-theme)

;; ── Jump (j) ──────────────────────────────────
(global-set-key (kbd "C-c j") #'avy-goto-char-2)

;; ── Notes (n): capture shortcuts ──────────────
(defun my/capture-inbox ()
  "Quick capture into inbox."
  (interactive) (org-capture nil "i"))
(defun my/capture-journal ()
  "Capture into daily journal."
  (interactive) (org-capture nil "j"))
(defun my/capture-reading ()
  "Capture a book or reading resource."
  (interactive) (org-capture nil "r"))
(defun my/capture-work-task ()
  "Capture a work task."
  (interactive) (org-capture nil "w"))
(defun my/capture-work-project ()
  "Capture a work project."
  (interactive) (org-capture nil "W"))
(defun my/capture-personal-task ()
  "Capture a personal task."
  (interactive) (org-capture nil "t"))
(defun my/capture-personal-project ()
  "Capture a personal project."
  (interactive) (org-capture nil "p"))

(global-set-key (kbd "C-c n i") #'my/capture-inbox)
(global-set-key (kbd "C-c n j") #'my/capture-journal)
(global-set-key (kbd "C-c n r") #'my/capture-reading)
(global-set-key (kbd "C-c n w") #'my/capture-work-task)
(global-set-key (kbd "C-c n W") #'my/capture-work-project)
(global-set-key (kbd "C-c n t") #'my/capture-personal-task)
(global-set-key (kbd "C-c n p") #'my/capture-personal-project)

(defun my/roam--template (key)
  (cl-find key org-roam-capture-templates :key #'car :test #'string=))

(defun my/roam-capture (key prompt)
  (require 'org-roam)
  (org-roam-capture- :node (org-roam-node-create
                            :title (read-string prompt))
                     :templates (list (my/roam--template key))))

(defun my/roam-capture-concept ()
  "Capture an org-roam concept."
  (interactive)
  (require 'org-roam)
  (my/roam-capture "c" "Concept: "))

(defun my/roam-capture-question ()
  "Capture an org-roam question."
  (interactive)
  (require 'org-roam)
  (my/roam-capture "q" "Question topic: "))

(defun my/roam-capture-person ()
  "Capture an org-roam person."
  (interactive)
  (require 'org-roam)
  (my/roam-capture "P" "Person name: "))

(defun my/roam-log-interaction ()
  "Log an interaction with an org-roam person."
  (interactive)
  (require 'org-roam)
  (let* ((node (org-roam-node-read nil
                                   (lambda (n) (member "person" (org-roam-node-tags n)))))
         (file (org-roam-node-file node)))
    (find-file file)
    (goto-char (point-min))
    (if (search-forward "* Interactions" nil t)
        (progn
          (org-end-of-subtree)
          (insert "\n** " (format-time-string "[%Y-%m-%d %a]") "\n"))
      (error "No Interactions heading in %s" file))))

(global-set-key (kbd "C-c n k") #'my/roam-capture-concept)
(global-set-key (kbd "C-c n q") #'my/roam-capture-question)
(global-set-key (kbd "C-c n P") #'my/roam-capture-person)
(global-set-key (kbd "C-c n x") #'my/roam-log-interaction)


;; ── Org (o) ───────────────────────────────────
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
(global-set-key (kbd "C-c o r") #'org-roam-node-find)
(global-set-key (kbd "C-c o i") #'org-roam-node-insert)
(global-set-key (kbd "C-c o s") #'consult-org-roam-search)
(global-set-key (kbd "C-c o u") #'org-roam-ui-open)
(global-set-key (kbd "C-c o l") #'org-cliplink)
(global-set-key (kbd "C-c o t") #'org-transclusion-mode)
(global-set-key (kbd "C-c o I") #'org-clock-in)
(global-set-key (kbd "C-c o O") #'org-clock-out)
(global-set-key (kbd "C-c o R") #'org-clock-report)
(global-set-key (kbd "C-c o e") #'org-set-effort)
(global-set-key (kbd "C-c o q") #'org-ql-find)
(global-set-key (kbd "C-c o k") #'org-kanban/initialize)
(global-set-key (kbd "C-c o A") #'org-archive-subtree)

;; ── Projects (p): built-in C-x p map + C-c p alias ────────
;; Emacs 28+ built-in project-prefix-map provides:
;; C-x p p: switch-project    C-x p f: find-file
;; C-x p b: switch-to-buffer  C-x p k: kill-buffers
;; C-x p e: eshell            C-x p c: compile
(define-key ctl-x-map "p" project-prefix-map)
(global-set-key (kbd "C-c p") project-prefix-map)

;; ── Search (s): built-in M-s search-map + consult ─────────
;; C-s remains standard isearch-forward!
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s i") #'consult-imenu)
(global-set-key (kbd "M-s t") #'consult-todo)
(global-set-key (kbd "C-c s s") #'consult-line)
(global-set-key (kbd "C-c s r") #'consult-ripgrep)
(global-set-key (kbd "C-c s i") #'consult-imenu)
(global-set-key (kbd "C-c s t") #'consult-todo)

;; ── Virtual env (v): conda ────────────────────
(global-set-key (kbd "C-c v a") #'conda-env-activate)
(global-set-key (kbd "C-c v d") #'conda-env-deactivate)

;; ── Windows (w) ───────────────────────────────
(global-set-key (kbd "C-c w u") #'winner-undo)
(global-set-key (kbd "C-c w U") #'winner-redo)
(global-set-key (kbd "C-c w f") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c w =") #'balance-windows)
(global-set-key (kbd "C-c w h") #'windmove-left)
(global-set-key (kbd "C-c w l") #'windmove-right)
(global-set-key (kbd "C-c w k") #'windmove-up)
(global-set-key (kbd "C-c w j") #'windmove-down)
(global-set-key (kbd "C-c w H") #'shrink-window-horizontally)
(global-set-key (kbd "C-c w L") #'enlarge-window-horizontally)
(global-set-key (kbd "C-c w K") #'shrink-window)
(global-set-key (kbd "C-c w J") #'enlarge-window)

;; ── AI (a) ────────────────────────────────────
(global-set-key (kbd "C-c a c") #'gptel)
(global-set-key (kbd "C-c a s") #'gptel-send)
(global-set-key (kbd "C-c a r") #'gptel-rewrite)
(global-set-key (kbd "C-c a m") #'gptel-menu)
(global-set-key (kbd "C-c a a") #'gptel-add)             ; add context
(global-set-key (kbd "C-c a x") #'gptel-context-remove-all) ; clear context

;; ── Help: Helpful remapped on built-in C-h (see section 12) ──
;; C-h f: helpful-callable   C-h v: helpful-variable
;; C-h k: helpful-key        C-h x: helpful-command
;; C-h .: helpful-at-point

;; ── Quit (q) ──────────────────────────────────
;; Built-in C-x C-c exits; C-c q r restarts
(global-set-key (kbd "C-c q r") #'restart-emacs)

;; which-key — built-in since Emacs 30
(use-package which-key
  :ensure nil
  :custom (which-key-idle-delay 0.3)
  :config (which-key-mode 1))

(repeat-mode 1)

;;; ─────────────────────────────────────────────
;;; 5. COMPLETION (vertico + corfu)
;;; ─────────────────────────────────────────────

(use-package vertico
  :init (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-multiform-categories
   '((file (vertico-sort-function . vertico-sort-directories-first))))
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (vertico-multiform-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file       (styles basic partial-completion))
                                   (eglot      (styles orderless))
                                   (eglot-capf (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package consult
  :custom
  (consult-preview-key "M-.")
  (consult-narrow-key "<")
  (consult-line-numbers-widen t)
  (consult-async-min-input 2)
  (consult-async-refresh-delay 0.15)
  (consult-async-input-throttle 0.2)
  (consult-async-input-debounce 0.1)
  (consult-grep-max-columns 500)
  :config
  (when (boundp 'consult-preview-allowed-hooks)
    (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode-hook))
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)   ; show doc popup automatically
  (corfu-history-mode 1)     ; rank by recency (auto-saves via savehist)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  ;; Emacs 30+: disable Ispell capf in text modes (use cape-dict instead)
  (text-mode-ispell-word-completion nil)
  :bind (:map corfu-map
              ("C-n"      . corfu-next)
              ("C-p"      . corfu-previous)
              ("<tab>"    . corfu-insert)
              ("TAB"      . corfu-insert)
              ("<escape>" . corfu-quit)
              ("C-g"      . corfu-quit)
              ;; popupinfo scrolling
              ("C-j"      . corfu-popupinfo-scroll-down)
              ("C-k"      . corfu-popupinfo-scroll-up)
              ("M-h"      . corfu-popupinfo-toggle))
  :config
  (keymap-unset corfu-map "RET")
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-keyword nil t)
              (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))

(use-package embark
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :bind (("C-."   . embark-act)
         ("C-c ." . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :vc (:url "https://github.com/oantolin/embark" :rev :newest)
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep :commands wgrep-change-to-wgrep-mode)

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-todo
  :vc (:url "https://github.com/liuyinz/consult-todo" :rev :newest)
  :after (consult hl-todo))

;;; ─────────────────────────────────────────────
;;; 6. THEME & MODELINE
;;; ─────────────────────────────────────────────

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  (defun my/toggle-theme ()
    "Toggle between doom-one (dark) and doom-one-light (light)."
    (interactive)
    (if (custom-theme-enabled-p 'doom-one)
        (progn
          (disable-theme 'doom-one)
          (load-theme 'doom-one-light t)
          (message "Switched to doom-one-light (Day Mode)"))
      (disable-theme 'doom-one-light)
      (load-theme 'doom-one t)
      (message "Switched to doom-one (Night Mode)"))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-which-function t)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-window-width-limit 85)
  :config
  (which-function-mode 1)
  (add-hook 'after-make-frame-functions
            (lambda (f) (with-selected-frame f
                          (setq doom-modeline-icon (display-graphic-p))))))

(use-package nerd-icons)

;;; ─────────────────────────────────────────────
;;; 7. EDITING
;;; ─────────────────────────────────────────────

(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode)
         (org-mode  . yas-minor-mode))
  :custom
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config (yas-reload-all))
(use-package yasnippet-snippets :after yasnippet)

(use-package ws-butler :hook (prog-mode . ws-butler-mode))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-prefer-character (not (display-graphic-p)))
  (indent-bars-color-by-depth nil)
  (indent-bars-highlight-current-depth '(:blend 0.5)))

(use-package ligature
  :vc (:url "https://github.com/mickeynp/ligature.el" :rev :newest)
  :config
  (ligature-set-ligatures 'prog-mode
                          '("->" "=>" "!=" ">=" "<=" "==" "===" "!==" "::" "..."
                            "++" "--" "||" "&&" "??" ":=" "<-" "<>" "<<" ">>" "<=>" "/**" "/*" "*/"))
  (when (display-graphic-p) (global-ligature-mode t)))

(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo" :rev :newest)
  :hook (prog-mode . hl-todo-mode))

(use-package expand-region
  ;; C-= is not reliably sent by terminal emulators (-nw), so use C-c = instead.
  ;; Avoids shadowing M-= (count-words-region) and M-- (negative-argument).
  :bind (("C-c =" . er/expand-region)
         ("C-c -" . er/contract-region)))

(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode)
  :bind (:map symbol-overlay-mode-map
              ("M-i" . symbol-overlay-put)
              ("M-n" . symbol-overlay-jump-next)
              ("M-p" . symbol-overlay-jump-prev)
              ("M-Q" . symbol-overlay-query-replace))
  :config
  ;; transient map (active on a highlighted symbol): q removes all highlights
  (define-key symbol-overlay-map (kbd "q") #'symbol-overlay-remove-all))

;;; ─────────────────────────────────────────────
;;; 8. LSP — eglot (built-in)
;;; ─────────────────────────────────────────────

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure eglot-rename eglot-code-actions eglot-find-implementation eglot-inlay-hints-mode)
  :hook ((python-mode        . eglot-ensure)
         (python-ts-mode     . eglot-ensure)
         (c-mode             . eglot-ensure)
         (c++-mode           . eglot-ensure)
         (c-ts-mode          . eglot-ensure)
         (c++-ts-mode        . eglot-ensure)
         (eglot-managed-mode . eglot-inlay-hints-mode))
  :custom
  (eglot-events-buffer-config '(:size 0))
  (eglot-autoshutdown t)
  (eglot-sync-connect 1)
  (eglot-extend-to-xref t)
  ;; Emacs 30+: show LSP progress in the echo area
  (eglot-report-progress t)
  :config
  ;; Python: tell basedpyright/pylsp to not duplicate what ruff already reports
  (setq-default eglot-workspace-configuration
                '(:basedpyright (:analysis (:typeCheckingMode "standard"
                                                              :diagnosticMode "workspace")))))

;; eglot-booster: converts JSON-RPC to elisp bytecode via emacs-lsp-booster
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :after eglot
  :config
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode 1)))

;;; ─────────────────────────────────────────────
;;; 9. FLYMAKE (built-in)
;;; ─────────────────────────────────────────────

(use-package flymake
  :ensure nil
  :commands (flymake-mode flymake-goto-next-error flymake-goto-prev-error flymake-show-buffer-diagnostics)
  :hook (prog-mode . flymake-mode)
  :custom (flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; byte-compile checker fires on save — too noisy for init.el editing
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (remove-hook 'flymake-diagnostic-functions
                           #'elisp-flymake-byte-compile t))))

;;; ─────────────────────────────────────────────
;;; 10. TREE-SITTER (built-in Emacs 29+)
;;; ─────────────────────────────────────────────

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-font-lock-level 4)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ─────────────────────────────────────────────
;;; 11. PROJECT.EL (built-in)
;;; ─────────────────────────────────────────────

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers
   '("pyproject.toml" "setup.py" "package.json" "CMakeLists.txt" "Makefile"))
  (project-switch-commands
   '((project-find-file    "Find file"      ?f)
     (project-find-regexp  "Find regexp"    ?g)
     (consult-ripgrep      "Ripgrep"        ?s)
     (project-dired        "Dired"          ?d)
     (project-eshell       "Eshell"         ?e)
     (magit-project-status "Magit"          ?m))))

;;; ─────────────────────────────────────────────
;;; 12. IDE TOOLS
;;; ─────────────────────────────────────────────

(use-package avy :commands (avy-goto-char-2 avy-goto-char avy-goto-line))

(use-package magit
  :commands (magit-status magit-commit magit-push magit-pull magit-fetch
                          magit-branch magit-log-current magit-diff-dwim magit-stage
                          magit-unstage magit-blame magit-rebase magit-merge magit-stash
                          magit-project-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-save-repository-buffers 'dontask)
  (magit-diff-refine-hunk 'all)
  (magit-revision-insert-related-refs nil)
  (transient-default-level 5)
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-diff-use-indicator-faces t)
  (magit-refresh-status-buffer nil))

(use-package diff-hl
  :commands (diff-hl-next-hunk diff-hl-previous-hunk diff-hl-show-hunk global-diff-hl-mode)
  :hook ((after-init         . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config (diff-hl-flydiff-mode 1))


(use-package restart-emacs :commands restart-emacs)

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package dape
  :commands (dape dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'right)
  :config
  (dape-breakpoint-global-mode 1)
  ;; Show info buffers automatically on dape session start
  (add-hook 'dape-on-start-hooks
            (lambda () (save-excursion (dape-info) (dape-repl)))))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point helpful-command)
  :custom
  (helpful-set-variable-function #'setopt)
  :bind
  ("C-h x"                   . helpful-command)
  ("C-h ."                   . helpful-at-point)

  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

;;; ─────────────────────────────────────────────
;;; 13. ORG MODE
;;; ─────────────────────────────────────────────

(defvar my/garden-dir   (expand-file-name "~/Documents/garden/"))
(defvar my/work-dir     (expand-file-name "~/Documents/garden/work/"))
(defvar my/personal-dir (expand-file-name "~/Documents/garden/personal/"))
(defvar my/work-projects-dir     (expand-file-name "~/Documents/garden/work/projects/"))
(defvar my/personal-projects-dir (expand-file-name "~/Documents/garden/personal/projects/"))

(use-package org
  :ensure nil
  :commands (org-mode org-agenda org-capture org-clock-in org-clock-out org-clock-report org-set-effort org-archive-subtree)
  :hook (org-mode . visual-line-mode)
  :custom
  (org-directory            "~/Documents/garden/")
  (org-log-done             'time)
  (org-log-into-drawer      t)
  (org-clock-mode-line-total 'today)
  (org-startup-indented     t)
  (org-hide-emphasis-markers t)
  (org-return-follows-link  t)
  (org-agenda-files         nil)   ; add files via C-c [ or setq org-agenda-files
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-clock-persist t)
  (org-enforce-todo-dependencies t)
  (org-agenda-window-setup 'current-window)
  (org-image-actual-width nil)
  (org-agenda-inhibit-startup t)          ; skip per-file startup → faster agenda
  (org-fold-catch-invisible-edits 'smart) ; never silently edit folded text
  :config
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-habit)
  (setq org-habit-graph-column           55
        org-habit-show-habits-only-for-today t)
  (require 'org-id)
  (org-clock-persistence-insinuate)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (shell . t) (emacs-lisp . t)))

  (setq org-refile-targets                    '((org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path            'file
        org-outline-path-complete-in-steps     nil
        org-refile-use-cache                   t   ; cache targets → faster refile (C-0 C-c C-w to clear)
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")))

  ;; Archive completed items to <file>_archive in the same dir → keeps agenda files lean
  (setq org-archive-location "%s_archive::* Archived")

  ;; Daily dashboard: agenda + next actions (C-c o a → d)
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)))
            (todo "NEXT" ((org-agenda-overriding-header "Next Actions")))))))

  (defvar my/task-capture-body    "* TODO %?\n  %U\n  %a")
  (defvar my/project-capture-body "* NEW %^{Project} %^g\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** Goal\n%?\n\n** Tasks\n- [ ] \n\n** Outcome\n")

  (setq org-capture-templates
        `(;; Shared
          ("i" "Inbox"   entry (file+headline ,(concat my/garden-dir "inbox.org") "Tasks")
           ,my/task-capture-body)
          ("j" "Journal" entry (file+olp+datetree ,(concat my/garden-dir "journal.org"))
           "* %U\n** Worked On\n%?\n** Notes\n\n** Reading Insight\n\n** Momentum: /10\n"
           :empty-lines 1)

          ("r" "Reading" entry (file+headline ,(concat my/garden-dir "reading.org") "Reading")
           "* %^{Book/Resource}\n:PROPERTIES:\n:START_FROM: %^{Start from page}\n:CREATED: %U\n:END:\n** Insights\n%?"
           :empty-lines 1)
          ;; Work
          ("w" "Work Task"    entry (file+headline ,(concat my/work-dir "tasks.org") "Tasks")
           ,my/task-capture-body :empty-lines 1)
          ("W" "Work Project" entry (file+headline ,(concat my/work-dir "projects.org") "Projects")
           ,my/project-capture-body :empty-lines 1)
          ;; Personal
          ("t" "Personal Task"    entry (file+headline ,(concat my/personal-dir "tasks.org") "Tasks")
           ,my/task-capture-body :empty-lines 1)
          ("p" "Personal Project" entry (file+headline ,(concat my/personal-dir "projects.org") "Projects")
           ,my/project-capture-body :empty-lines 1))))

(use-package org-super-agenda
  :after org
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Overdue"
                 :deadline past
                 :scheduled past)
          (:name "Today"
                 :time-grid t
                 :scheduled today
                 :deadline today)
          (:name "Next Actions"
                 :todo "NEXT")
          (:name "Habits"
                 :habit t)
          (:name "Work"
                 :file-path "work/")
          (:name "Personal"
                 :file-path "personal/")
          (:name "Inbox"
                 :file-path "inbox\\.org")
          (:discard (:anything t)))))

(use-package org-ql
  :after org
  :commands (org-ql-search org-ql-view org-ql-find))

(use-package org-kanban
  :after org
  :commands (org-kanban/initialize org-kanban/shift))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory          (expand-file-name "~/Documents/garden/"))
  (org-roam-completion-everywhere t)
  (org-roam-database-connector 'sqlite-builtin)   ; Emacs 30 native sqlite
  (org-roam-db-gc-threshold most-positive-fixnum) ; fewer GC pauses during cache build
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("c" "concept" plain
      "#+FILETAGS: :concept:\n\n* What\n%?\n\n* Why\n\n* When\n\n* Code\n#+begin_src python\n\n#+end_src\n\n* Links\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("q" "question" plain
      "#+FILETAGS: :question:\n\n* Question\n%?\n\n* Answer\n\n* Related Concepts\n"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)
     ("P" "person" plain
      "#+FILETAGS: :person:\n#+AREA: \n\n* Who\n\n* Find Them\n- \n\n* Interactions\n** %U\n%?\n\n* Follow-up\n- [ ] \n\n* Notes\n"
      :target (file+head "people/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+TITLE: ${title}\n#+CREATED: %U\n")
      :unnarrowed t)))
  :config
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after org-roam
  :custom (consult-org-roam-grep-func #'consult-ripgrep)
  :config (consult-org-roam-mode 1))

(use-package org-roam-ui
  :after org-roam
  :defer t
  :custom
  (org-roam-ui-sync-theme    t)
  (org-roam-ui-follow        t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start nil))

(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("◉" "○" "◈" "◇" "✦"))
  (org-modern-cycle-stars t)
  (org-modern-hide-stars 'leading)
  (org-modern-label-border 0.3)
  (org-modern-table t)
  (org-modern-table-vertical 1)
  (org-modern-table-horizontal 0.2))

(with-eval-after-load 'org-modern
  (set-face-attribute 'org-modern-habit nil
                      :foreground "#98be65"
                      :background nil))

(use-package org-pomodoro
  :after org
  :custom (org-pomodoro-length 25))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks      t)
  (org-appear-autosubmarkers t))

(use-package org-transclusion
  :after org
  :bind (:map org-mode-map
              ("C-c t a" . org-transclusion-add)))

(use-package org-cliplink :after org)

;;; ─────────────────────────────────────────────
;;; 14. MARKDOWN
;;; ─────────────────────────────────────────────

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . (lambda () (setq tab-width 2))))
  :custom
  (markdown-command "pandoc")
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-hide-urls t)
  (markdown-list-indent-width 2))

;;; ─────────────────────────────────────────────
;;; 15. WRITING
;;; ─────────────────────────────────────────────

(use-package olivetti
  :hook ((org-mode      . olivetti-mode)
         (markdown-mode . olivetti-mode))
  :custom (olivetti-body-width 90)
  :config
  ;; Olivetti's built-in margin scroll bindings use the literal symbol
  ;; 'mouse-wheel-down-event instead of its value, so they never match real
  ;; events on Emacs 31+.  Patch the keymap with the actual wheel events so
  ;; trackpad scrolling works over the wide centering margins.
  (dolist (area '(left-margin right-margin))
    (dolist (ev '(wheel-down wheel-up mouse-4 mouse-5))
      (define-key olivetti-mode-map (vector area ev) #'pixel-scroll-precision))))

;;; ─────────────────────────────────────────────
;;; 16. PYTHON / CONDA
;;; ─────────────────────────────────────────────

(use-package conda
  :custom
  (conda-anaconda-home      (expand-file-name "~/miniconda3/"))
  (conda-env-home-directory (expand-file-name "~/miniconda3/"))
  (conda-env-subdirectory   "envs")
  (conda-message-on-environment-switch nil))

;;; ─────────────────────────────────────────────
;;; 17. COMPILE / RUN
;;; ─────────────────────────────────────────────

(defun my/python-run-current-file ()
  (interactive)
  (when (buffer-file-name)
    (compile (format "%s %s"
                     (or (executable-find "python3") "python3")
                     (shell-quote-argument buffer-file-name)))))

(defun my/cpp-compile-run-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let* ((src (buffer-file-name))
           (exe (concat (file-name-sans-extension src) ".out")))
      (compile (format "g++ -std=c++17 -Wall %s -o %s && %s"
                       (shell-quote-argument src)
                       (shell-quote-argument exe)
                       (shell-quote-argument exe))))))

;; Modern, fast terminal emulator (alternative to vterm/eshell)
(use-package eat
  :commands (eat eat-other-window)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-yank-to-terminal t))

;;; ─────────────────────────────────────────────
;;; 18. FORMATTERS
;;; ─────────────────────────────────────────────

(use-package apheleia
  :config
  (apheleia-global-mode 1)
  (setf (alist-get 'python-mode        apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode     apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'c-mode             apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode           apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode          apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode        apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'prettier apheleia-formatters) '("prettierd" filepath))
  (setf (alist-get 'mhtml-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-mode            apheleia-mode-alist) 'prettier)
  (setf (alist-get 'js-ts-mode         apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode    apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'tsx-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode           apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-ts-mode        apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode          apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-ts-mode       apheleia-mode-alist) 'prettier)
  (setf (alist-get 'markdown-mode      apheleia-mode-alist) 'prettier)
  (setf (alist-get 'sh-mode            apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode       apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'emacs-lisp-mode    apheleia-mode-alist) nil))

;;; ─────────────────────────────────────────────
;;; 19. PERSISTENCE & DEFAULTS (built-ins)
;;; ─────────────────────────────────────────────

(setq auto-save-default nil)
(auto-save-visited-mode 1)

(save-place-mode 1)
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))

(use-package recentf
  :ensure nil
  :init (recentf-mode 1)
  :custom
  (recentf-max-menu-items 50)
  (recentf-max-saved-items 100)
  (recentf-exclude '("/elpa/" "/backups/" "/\\.git/" "/tmp/"
                     "saveplace" "recentf" "history" "custom\\.el")))

(savehist-mode 1)
(winner-mode 1)

;; Default 2-space indentation across all modes
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2
              c-basic-offset 2
              c-ts-mode-indent-offset 2
              sh-basic-offset 2
              js-indent-level 2
              typescript-indent-level 2
              css-indent-offset 2
              python-indent-offset 2)

(setq use-short-answers     t
      scroll-conservatively 101
      scroll-margin         8
      isearch-lazy-count    t)

;; compilation-environment is defined in compile.el (loaded on first compile)
(with-eval-after-load 'compile
  (add-to-list 'compilation-environment "NO_COLOR=1"))
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      backup-by-copying    t
      version-control      t
      delete-old-versions  t
      kept-new-versions    6
      kept-old-versions    2)

(show-paren-mode 1)
(delete-selection-mode 1)
(global-so-long-mode 1)

(use-package dired
  :ensure nil
  :custom
  (dired-kill-when-opening-new-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t))

;; Sync kill ring with macOS clipboard in terminal mode
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (setq interprogram-cut-function
        (lambda (text)
          (with-temp-buffer
            (insert text)
            (call-process-region (point-min) (point-max) "pbcopy"))))
  (setq interprogram-paste-function
        (lambda ()
          (with-temp-buffer
            (call-process "pbpaste" nil t nil)
            (buffer-string)))))
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;;; ─────────────────────────────────────────────
;;; 21. AI ASSISTANT (gptel + LM Studio)
;;; ─────────────────────────────────────────────

(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :commands (gptel gptel-send gptel-rewrite gptel-menu gptel-add gptel-context-remove-all)
  :config
  (setq gptel-backend
        (gptel-make-openai "lmstudio"
          :host     "localhost:1234"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :key      "not-needed"   ; LM Studio doesn't require an API key
          :models   '(gemma-4-e4b)
          :stream   t)
        gptel-model        'gemma-4-e4b
        gptel-default-mode 'org-mode
        gptel-system-message "You are a helpful assistant living in Emacs. Respond concisely."
        gptel-temperature 0.7
        gptel-include-reasoning nil)
  (gptel-highlight-mode 1))

;;; ─────────────────────────────────────────────
;;; 22. PACKAGE UPDATES
;;; ─────────────────────────────────────────────
;; Handled from the terminal via emacs-sync.sh:
;;   ./emacs-sync.sh sync -u   (install + upgrade + purge + snapshot)
;;   ./emacs-sync.sh check     (dry-run status check)
;;   ./emacs-sync.sh doctor    (verify external tools)

;;; ─────────────────────────────────────────────
;;; 23. CUSTOM FILE
;;; ─────────────────────────────────────────────

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;;; init.el ends here
