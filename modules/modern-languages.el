;;; modules/modern-languages.el --- Language support -*- lexical-binding: t; -*-

;;; Commentary:
;; Language-specific configuration loaded in Phase 4 (1s idle).
;; Design philosophy: prefer Emacs 29+ built-ins over third-party
;; packages wherever they're equivalent.  This reduces dependencies
;; and startup cost.
;;
;;   Layer        Tool          Why this one?
;;   ─────────    ────────────  ──────────────────────────────────────
;;   Parsing      Tree-sitter   Built-in (Emacs 29+), faster than regex
;;   LSP          Eglot         Built-in (Emacs 29+), lighter than lsp-mode
;;   Diagnostics  Flymake       Built-in, integrates with Eglot natively
;;   Formatting   Apheleia      Async (doesn't freeze Emacs while formatting)
;;   Navigation   dumb-jump     Works without LSP (regex-based fallback via rg)
;;
;; Languages: Python (+conda), C/C++ (+clangd), HTML, CSS, JSON
;; Web templates: web-mode (handles mixed PHP/JS/HTML in one buffer)

;;; Code:

(require 'utilities)

;; Silence byte-compiler warnings for tree-sitter variables (Emacs 29+)
(defvar treesit-language-source-alist)
(defvar treesit-font-lock-level)

;; Silence byte-compiler warnings for deferred/autoloaded functions
(declare-function global-treesit-auto-mode "treesit-auto")
(declare-function eglot-current-server "eglot")
(declare-function eglot-find-definition "eglot")
(declare-function eglot-find-references "eglot")
(declare-function eglot-find-typeDefinition "eglot")
(declare-function jupyter-eval-region "jupyter-repl")
(declare-function jupyter-eval-line-or-region "jupyter-repl")
(declare-function jupyter-repl-restart-kernel "jupyter-repl")
(declare-function persp-names "perspective")
(declare-function conda-env-activate "conda")

;; Silence byte-compiler warnings for package variables
(defvar conda-env-current-name)
(defvar conda-anaconda-home)
(defvar conda-env-home-directory)
(defvar treesit-auto-install)
(defvar treesit-auto-langs)
(defvar jupyter-repl-echo-eval-p)
(defvar jupyter-repl-allow-RET-when-busy)
(defvar jupyter-repl-maximum-size)
(defvar python-pytest-arguments)
(defvar web-mode-markup-indent-offset)
(defvar web-mode-css-indent-offset)
(defvar web-mode-code-indent-offset)
(defvar web-mode-enable-auto-pairing)
(defvar web-mode-enable-auto-closing)
(defvar web-mode-enable-auto-quoting)
(defvar web-mode-enable-css-colorization)
(defvar web-mode-enable-current-element-highlight)
(defvar web-mode-enable-current-column-highlight)
(defvar emmet-move-cursor-between-quotes)
(defvar emmet-expand-jsx-className?)
(defvar scss-compile-at-save)
(defvar flymake-proc-compilation-prevents-syntax-check)
(defvar apheleia-mode-alist)
(defvar dumb-jump-prefer-searcher)
(defvar dumb-jump-aggressive)
(defvar dumb-jump-selector)
(defvar sp-highlight-pair-overlay)
(defvar sp-highlight-wrap-overlay)
(defvar sp-highlight-wrap-tag-overlay)
(defvar sp-show-pair-delay)
(defvar sp-show-pair-from-inside)
(defvar yas-buffer-local-condition)
(defvar mc/always-run-for-all)
(defvar diff-hl-flydiff-delay)
(defvar persp-mode-prefix-key)
(defvar persp-auto-save-fname)

;; ══════════════════════════════════════════════════════════════════
;;  Tree-sitter (Emacs 29+ built-in)
;; ══════════════════════════════════════════════════════════════════
;; Tree-sitter provides incremental parsing via C libraries.  It's
;; faster and more accurate than regex-based font-lock, especially
;; for nested structures (e.g. template strings in JS, raw strings
;; in Python).  Each language needs a compiled grammar (.so/.dylib).

;; Grammar sources — git repos for `treesit-install-language-grammar`.
;; Pinned versions ensure reproducible builds across machines.
;; Must be set before treesit-auto loads so it knows where to find them.
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c" "v0.20.7")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.20.3")
        (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (html "https://github.com/tree-sitter/tree-sitter-html")))

;; treesit-auto — automatically remap legacy major modes (e.g. python-mode)
;; to their tree-sitter variants (python-ts-mode) when the grammar is available.
;; `prompt` asks before downloading missing grammars via the recipes above.
(use-package treesit-auto
  :straight t
  :defer my/defer-medium
  :config
  (setq treesit-auto-install 'prompt          ; ask before downloading grammars
        treesit-auto-langs '(c cpp python json css html))  ; only these languages
  (global-treesit-auto-mode))

;; Font-lock level: 1=minimal, 2=moderate, 3=rich, 4=maximum.
;; Level 3 highlights types, functions, variables, and keywords
;; without the overhead of level 4 (which highlights every identifier).
(setq treesit-font-lock-level 3)

;; ══════════════════════════════════════════════════════════════════
;;  Eglot (built-in LSP client, Emacs 29+)
;; ══════════════════════════════════════════════════════════════════
;; Eglot is Emacs's built-in LSP client — lighter than lsp-mode
;; (~2000 vs ~15000 lines).  Activation is manual (SPC l e) so
;; opening a file doesn't block while the server starts.
;;
;; Required language servers (install separately):
;;   Python: pyright-langserver  (npm install -g pyright)
;;   C/C++:  clangd              (brew install llvm)
;;   CSS:    vscode-css-language-server   (npm install -g vscode-css-languageservice)
;;   HTML:   vscode-html-language-server  (npm install -g vscode-html-languageservice)

(use-package eglot
  :straight nil                                ; built-in, no download needed
  :defer t
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t                   ; kill server when last buffer closes
        eglot-sync-connect nil                 ; connect asynchronously (don't freeze Emacs)
        eglot-events-buffer-config '(:size 0)  ; don't log LSP JSON-RPC events (saves memory)
        eglot-send-changes-idle-time 0.5       ; wait 0.5s after typing before sending changes
        eglot-extend-to-xref t)                ; use xref for cross-file navigation

  ;; Disable LSP capabilities that add visual noise or CPU overhead.
  ;; These run on every cursor movement, which slows down large files.
  ;; You can still invoke them manually via M-x.
  (setq eglot-ignored-server-capabilities
        '(:hoverProvider                       ; auto-popup on hover (we use K instead)
          :documentHighlightProvider            ; highlight all occurrences of symbol
          :documentOnTypeFormattingProvider     ; auto-format as you type
          :colorProvider))                     ; inline color swatches in CSS

  ;; Language server mappings — which executable to run for each mode.
  ;; Eglot has built-in defaults for many languages; these override them.
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) . ("clangd")))
  (add-to-list 'eglot-server-programs
               '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((html-mode html-ts-mode) . ("vscode-html-language-server" "--stdio"))))

;; ══════════════════════════════════════════════════════════════════
;;  Python
;; ══════════════════════════════════════════════════════════════════

(use-package python
  :straight nil                                ; built-in
  :mode ("\\.py\\'" . python-ts-mode)          ; use tree-sitter mode for .py files
  :hook (python-mode . (lambda ()
                         (setq-local tab-width 4
                                     python-indent-offset 4)))
  :config
  (setq python-indent-offset 4                 ; PEP 8 standard indent
        python-shell-completion-native-enable nil)  ; readline completion causes hangs on macOS
  ;; Prefer IPython as the REPL — better multiline editing, %magic
  ;; commands, and tab completion than the default python3 REPL.
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")))

;; ── Jupyter (emacs-jupyter) ───────────────────────────────────────
;; Full Jupyter kernel integration: REPL, Org-Babel, and inline
;; image display.  Connects to any installed Jupyter kernel (Python,
;; R, Julia, etc.).  Requires: pip install jupyter
;;
;; Usage:
;;   M-x jupyter-run-repl        — start a REPL for any kernel
;;   M-x jupyter-connect-repl    — connect to a running kernel
;;   In Org: #+begin_src jupyter-python  — execute with rich output
;;
;; Keybindings (SPC J):
;;   SPC J r  — start Jupyter REPL
;;   SPC J c  — connect to running kernel
;;   SPC J e  — send buffer to Jupyter
;;   SPC J s  — send region to Jupyter
;;   SPC J d  — send defun to Jupyter
;;   SPC J i  — switch to Jupyter REPL buffer

(use-package jupyter
  :straight t
  :defer t
  :if (executable-find "jupyter")            ; only load if jupyter CLI is installed
  :commands (jupyter-run-repl
             jupyter-connect-repl
             jupyter-repl-associate-buffer
             jupyter-repl-pop-to-buffer)
  :config
  ;; Display inline images in the REPL buffer (matplotlib plots, etc.)
  (setq jupyter-repl-echo-eval-p t        ; echo input in REPL
        jupyter-repl-allow-RET-when-busy t ; don't block RET while kernel is busy
        jupyter-repl-maximum-size 5000)    ; truncate REPL after 5000 lines

  ;; Associate Python buffers with their Jupyter REPL for send-to-repl
  (defun my/jupyter-associate-python ()
    "Associate current Python buffer with a running Jupyter REPL."
    (interactive)
    (call-interactively #'jupyter-repl-associate-buffer))

  ;; Helper to start a Python kernel specifically
  (defun my/jupyter-run-python ()
    "Start a Jupyter REPL with the Python kernel."
    (interactive)
    (jupyter-run-repl "python3" nil t))

  ;; Send region or current line to Jupyter REPL
  (defun my/jupyter-send-region-or-line ()
    "Send active region to Jupyter REPL, or current line if no region."
    (interactive)
    (if (use-region-p)
        (jupyter-eval-region (region-beginning) (region-end))
      (jupyter-eval-line-or-region)))

  ;; Restart kernel without confirmation
  (defun my/jupyter-restart-kernel ()
    "Restart the Jupyter kernel associated with this buffer."
    (interactive)
    (jupyter-repl-restart-kernel)))

;; ── Conda environment management ─────────────────────────────────
;; conda.el lets you activate/deactivate conda environments from
;; within Emacs (SPC V a/d/l).  This sets the Python interpreter,
;; updates exec-path, and sets environment variables so Eglot and
;; shell commands use the correct Python version.

(use-package conda
  :straight t
  :defer t
  :config
  ;; Auto-detect conda installation path — checks CONDA_PREFIX env var
  ;; first, then common installation directories on macOS/Linux.
  (setq conda-anaconda-home
        (or (getenv "CONDA_PREFIX")
            (expand-file-name "~/anaconda3")
            (expand-file-name "~/miniconda3")
            (expand-file-name "~/opt/anaconda3")
            (expand-file-name "~/opt/miniconda3")))
  (setq conda-env-home-directory conda-anaconda-home)

  (add-hook 'python-mode-hook #'my/auto-activate-conda-env)
  (add-hook 'python-ts-mode-hook #'my/auto-activate-conda-env))

;; Auto-activate: when opening a Python file, look for environment.yml
;; in parent directories and activate that conda environment.
(defun my/auto-activate-conda-env ()
  "Activate conda env from project's environment.yml if present."
  (when (and (boundp 'conda-anaconda-home)
             (file-directory-p conda-anaconda-home))
    (when-let* ((project-root (locate-dominating-file default-directory "environment.yml"))
                (env-file (expand-file-name "environment.yml" project-root)))
      (with-temp-buffer
        (insert-file-contents env-file)
        (goto-char (point-min))
        (when (re-search-forward "^name:\\s-*\\(.+\\)$" nil t)
          (let ((env-name (match-string 1)))
            (unless (string= conda-env-current-name env-name)
              (conda-env-activate env-name)
              (message "Activated conda environment: %s" env-name))))))))

;; ── Testing & imports ────────────────────────────────────────────

;; python-pytest — run pytest from Emacs with smart test discovery.
;; `--failed-first` reruns failing tests first for faster feedback.
;; `--maxfail=5` stops after 5 failures to avoid overwhelming output.
(use-package python-pytest
  :straight t
  :after python
  :defer t
  :config
  (setq python-pytest-arguments '("--color" "--failed-first" "--maxfail=5")))

;; py-isort — auto-sort Python imports on save (PEP 8 style).
;; Requires `isort` CLI tool: pip install isort
(use-package py-isort
  :straight t
  :defer t
  :hook ((python-mode . py-isort-before-save)
         (python-ts-mode . py-isort-before-save)))

;; ══════════════════════════════════════════════════════════════════
;;  C / C++
;; ══════════════════════════════════════════════════════════════════

;; Remap legacy modes to tree-sitter variants.  When Emacs would
;; normally use c-mode (regex-based), it uses c-ts-mode instead.
;; This gives better syntax highlighting and structural navigation.
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

(use-package cc-mode
  :straight nil
  :mode (("\\.cpp\\'" . c++-ts-mode)
         ("\\.hpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.h\\'" . c++-ts-mode)
         ("\\.c\\'" . c-ts-mode))
  :config
  (setq c-default-style "linux"                ; K&R-style braces (brace on same line)
        c-basic-offset 4)                     ; 4-space indent
  (c-set-offset 'innamespace 0)               ; don't indent inside namespace {}
  (c-set-offset 'case-label '+)               ; indent case labels one level

  (defun my/cpp-compile ()
    "Compile the current C++ file with C++20 and debug flags."
    (interactive)
    (let* ((file (buffer-file-name))
           (output (file-name-sans-extension file)))
      (compile (format "g++ -std=c++20 -Wall -Wextra -g %s -o %s"
                       (shell-quote-argument file)
                       (shell-quote-argument output)))))

  (defun my/switch-header-source ()
    "Toggle between a C/C++ header and its corresponding source file."
    (interactive)
    (let* ((extension (file-name-extension (buffer-file-name)))
           (base-name (file-name-sans-extension (buffer-file-name)))
           (other-file
            (cond
             ((string= extension "cpp") (concat base-name ".hpp"))
             ((string= extension "hpp") (concat base-name ".cpp"))
             ((string= extension "cc") (concat base-name ".h"))
             ((string= extension "h")
              (let ((cc-file (concat base-name ".cc"))
                    (cpp-file (concat base-name ".cpp")))
                (if (file-exists-p cc-file) cc-file cpp-file)))
             ((string= extension "c") (concat base-name ".h"))
             (t (concat base-name ".cpp")))))
      (if (file-exists-p other-file)
          (find-file other-file)
        (if (y-or-n-p (format "File %s doesn't exist. Create it? " other-file))
            (find-file other-file)
          (message "Other file not found: %s" other-file)))))

  (define-key c++-mode-map (kbd "C-c C-c") 'my/cpp-compile)
  (define-key c++-mode-map (kbd "C-c o") 'my/switch-header-source)
  (define-key c-mode-map (kbd "C-c o") 'my/switch-header-source))

;; Tree-sitter mode keymaps are only defined when the modes load
(defvar c++-ts-mode-map)
(defvar c-ts-mode-map)
(with-eval-after-load 'c-ts-mode
  (define-key c++-ts-mode-map (kbd "C-c C-c") 'my/cpp-compile)
  (define-key c++-ts-mode-map (kbd "C-c o") 'my/switch-header-source)
  (define-key c-ts-mode-map (kbd "C-c o") 'my/switch-header-source))

;; CMake support — syntax highlighting and indentation for CMakeLists.txt
(use-package cmake-mode
  :straight t
  :defer t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; modern-cpp-font-lock — highlights C++11/14/17/20 keywords that
;; the built-in cc-mode doesn't recognize (auto, constexpr, nullptr, etc.)
(use-package modern-cpp-font-lock
  :straight t
  :defer t
  :hook ((c++-mode . modern-c++-font-lock-mode)
         (c++-ts-mode . modern-c++-font-lock-mode)))

;; ══════════════════════════════════════════════════════════════════
;;  Web (HTML, CSS, JSON, templates)
;; ══════════════════════════════════════════════════════════════════

;; Tree-sitter remaps for web languages
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))
(add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))

;; web-mode — handles template files that mix HTML with server-side
;; languages (PHP, Jinja2, ERB, etc.).  Regular html-mode can't parse
;; these because it doesn't understand the template syntax.
(use-package web-mode
  :straight t
  :defer t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.ejs\\'" . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.jinja2?\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.svelte\\'" . web-mode)
         ("\\.vue\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2       ; HTML indent
        web-mode-css-indent-offset 2          ; CSS indent (inside <style> blocks)
        web-mode-code-indent-offset 2         ; JS/PHP indent (inside <script> blocks)
        web-mode-enable-auto-pairing t        ; auto-close tags and brackets
        web-mode-enable-auto-closing t        ; auto-close </tag> when you type >
        web-mode-enable-auto-quoting t        ; auto-quote attribute values
        web-mode-enable-css-colorization t    ; show color swatches in CSS
        web-mode-enable-current-element-highlight t  ; highlight matching open/close tags
        web-mode-enable-current-column-highlight nil))  ; column highlight is too noisy

;; emmet-mode — expand abbreviations like "div.container>ul>li*3"
;; into full HTML with C-j.  Dramatically speeds up HTML authoring.
(use-package emmet-mode
  :straight t
  :defer t
  :hook ((html-mode . emmet-mode)
         (html-ts-mode . emmet-mode)
         (css-mode . emmet-mode)
         (css-ts-mode . emmet-mode)
         (web-mode . emmet-mode)
         (scss-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t     ; cursor lands inside attr=""
        emmet-expand-jsx-className? nil))      ; don't use className (we're not using React)

;; auto-rename-tag — when you edit <div>, the matching </div> updates
;; automatically.  Like VS Code's auto-rename-tag extension.
(use-package auto-rename-tag
  :straight t
  :defer t
  :hook ((html-mode . auto-rename-tag-mode)
         (html-ts-mode . auto-rename-tag-mode)
         (web-mode . auto-rename-tag-mode)))

;; SCSS support — don't auto-compile on save (we use build tools for that)
;; scss-mode references obsolete flymake variables; define them so loading doesn't error.
(defvar flymake-allowed-file-name-masks nil)
(defvar flymake-err-line-patterns nil)
(use-package scss-mode
  :straight t
  :defer t
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (setq scss-compile-at-save nil))

;; ══════════════════════════════════════════════════════════════════
;;  Diagnostics — Flymake (built-in)
;; ══════════════════════════════════════════════════════════════════
;; Flymake is Emacs's built-in diagnostic framework.  When Eglot is
;; active, it feeds LSP diagnostics into Flymake.  Navigate errors
;; with ]e/[e or SPC e n/p.  View all diagnostics with SPC e l.

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)             ; auto-enable in all programming modes
  :config
  (setq flymake-fringe-indicator-position 'right-fringe  ; error markers on right side
        flymake-suppress-zero-counters t       ; don't show "0 errors" in modeline
        flymake-start-on-flymake-mode t        ; check immediately when mode starts
        flymake-no-changes-timeout nil         ; only check on save, not while typing
        flymake-start-on-save-buffer t         ; re-check after every save
        flymake-proc-compilation-prevents-syntax-check t))  ; don't check during compilation

;; ══════════════════════════════════════════════════════════════════
;;  Formatting — Apheleia (async, non-blocking)
;; ══════════════════════════════════════════════════════════════════
;; Apheleia formats code on save using external tools (black, clang-format,
;; prettier).  Unlike format-all-the-code, Apheleia runs formatters
;; ASYNCHRONOUSLY in a subprocess — your Emacs never freezes while
;; a slow formatter runs.  It also preserves cursor position.
;;
;; Required formatters (install separately):
;;   Python: black       (pip install black)
;;   C/C++:  clang-format (brew install clang-format)
;;   Web:    prettier    (npm install -g prettier)

(use-package apheleia
  :straight t
  :defer 2
  :config
  (apheleia-global-mode +1)                    ; enable for all supported modes

  ;; Map each major mode to its formatter.  Both legacy (python-mode)
  ;; and tree-sitter (python-ts-mode) variants need explicit entries.
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'c-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) '(clang-format))
  (setf (alist-get 'css-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'html-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'html-ts-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'json-mode apheleia-mode-alist) '(prettier))
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) '(prettier)))

;; ══════════════════════════════════════════════════════════════════
;;  Code Navigation & Editing Utilities
;; ══════════════════════════════════════════════════════════════════

;; dumb-jump — regex-based "go to definition" that works WITHOUT an LSP
;; server.  It uses ripgrep to search for function/class definitions.
;; Registered as an xref backend, so g d (go to definition) auto-falls
;; back to it when Eglot isn't running.
(use-package dumb-jump
  :straight t
  :defer 3
  :config
  (setq dumb-jump-prefer-searcher 'rg         ; use ripgrep (fastest)
        dumb-jump-aggressive nil               ; don't jump without confirmation
        dumb-jump-selector 'completing-read)   ; use vertico for disambiguation
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; ── Lookup system (Eglot -> xref fallback chain) ─────────────────
;; These functions provide unified "go to definition/references/docs"
;; commands that work with or without an LSP server:
;;   With Eglot:    uses LSP protocol (precise, cross-file)
;;   Without Eglot: falls back to xref + dumb-jump (regex-based)
;; Bound to g d, g D, and K in evil normal state (see evil-config.el).

(defun my/lookup-definition ()
  "Jump to definition via Eglot if active, otherwise xref."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-definition)
    (xref-find-definitions (thing-at-point 'symbol t))))

(defun my/lookup-references ()
  "Find references via Eglot if active, otherwise xref."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-references)
    (xref-find-references (thing-at-point 'symbol t))))

(defun my/lookup-documentation ()
  "Show documentation via Eglot eldoc, helpful, or plain eldoc."
  (interactive)
  (cond
   ((and (bound-and-true-p eglot--managed-mode)
         (eglot-current-server))
    (eldoc-doc-buffer))
   ((fboundp 'helpful-at-point)
    (helpful-at-point))
   (t
    (eldoc))))

(defun my/lookup-type-definition ()
  "Jump to type definition (requires an active Eglot server)."
  (interactive)
  (if (and (bound-and-true-p eglot--managed-mode)
           (eglot-current-server))
      (eglot-find-typeDefinition)
    (message "Type definition requires an active LSP server (start with SPC l e)")))

;; ── Smartparens ──────────────────────────────────────────────────
;; Structural editing for parentheses, brackets, quotes, and tags.
;; Auto-inserts matching delimiters and provides navigation commands
;; (e.g. sp-forward-sexp, sp-unwrap-sexp).  `smartparens-config`
;; loads sensible defaults for all major languages.

(use-package smartparens
  :straight t
  :defer 2
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)                ; language-specific pair definitions
  (setq sp-highlight-pair-overlay t            ; highlight matching pair
        sp-highlight-wrap-overlay t            ; highlight during wrap operation
        sp-highlight-wrap-tag-overlay t        ; highlight during tag wrap
        sp-show-pair-delay 0                   ; show match immediately (no delay)
        sp-show-pair-from-inside t))           ; show match even when cursor is inside pair

;; ══════════════════════════════════════════════════════════════════
;;  Tree-sitter Grammar Auto-Install (30s idle)
;; ══════════════════════════════════════════════════════════════════
;; Grammars are compiled C libraries (.so/.dylib) that tree-sitter
;; needs for each language.  This auto-installs missing ones 30s
;; after startup (not sooner, to avoid network calls during init).
;; Manual install: M-x my/install-tree-sitter-languages

(defun my/install-tree-sitter-languages ()
  "Interactively install missing tree-sitter grammars."
  (interactive)
  (let ((languages '(c cpp css html json python))
        (failed '())
        (succeeded '()))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (when (y-or-n-p (format "Install tree-sitter grammar for %s? " lang))
          (condition-case err
              (progn
                (treesit-install-language-grammar lang)
                (push lang succeeded))
            (error
             (push (cons lang (error-message-string err)) failed))))))
    (cond
     ((and succeeded failed)
      (message "Tree-sitter: %d installed, %d failed" (length succeeded) (length failed)))
     (succeeded
      (message "Tree-sitter: all requested grammars installed"))
     (failed
      (message "Tree-sitter: installations failed — check *Messages*"))
     (t
      (message "Tree-sitter: all grammars already installed")))))

(defun my/auto-install-tree-sitter-languages ()
  "Silently install any missing tree-sitter grammars."
  (let ((languages '(c cpp css html json python))
        (failed '())
        (succeeded '())
        (missing 0))
    (dolist (lang languages)
      (unless (treesit-language-available-p lang)
        (setq missing (1+ missing))))
    (when (> missing 0)
      (message "Installing %d missing tree-sitter grammars..." missing)
      (dolist (lang languages)
        (unless (treesit-language-available-p lang)
          (if (assoc lang treesit-language-source-alist)
              (condition-case err
                  (let ((warning-minimum-level :error))
                    (treesit-install-language-grammar lang)
                    (push lang succeeded))
                (error
                 (push (cons lang (error-message-string err)) failed)))
            (message "  Skipped %s (no recipe)" lang))))
      (if (> (length failed) 0)
          (message "Tree-sitter: %d/%d installed" (length succeeded)
                   (+ (length succeeded) (length failed)))
        (message "Tree-sitter: all %d grammars installed" (length succeeded))))))

(run-with-idle-timer 30 nil #'my/auto-install-tree-sitter-languages)

;; ══════════════════════════════════════════════════════════════════
;;  Snippets — YASnippet
;; ══════════════════════════════════════════════════════════════════
;; YASnippet expands short triggers into code templates (e.g. type
;; "def" + TAB in Python to get a full function skeleton).  Insert
;; snippets interactively with SPC i s.

(use-package yasnippet
  :straight t
  :defer my/defer-medium
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-global-mode 1)

  ;; Community snippet collections
  (use-package yasnippet-snippets              ; ~500 snippets for common languages
    :straight t
    :defer t)

  (use-package doom-snippets                   ; Doom Emacs's curated snippet collection
    :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*"))
    :after yasnippet
    :defer t)

  ;; Prevent TAB from expanding snippets when corfu's completion popup
  ;; is active — otherwise TAB tries to expand a snippet instead of
  ;; accepting the corfu completion candidate.
  (add-hook 'yas-minor-mode-hook
            (lambda ()
              (setq yas-buffer-local-condition
                    '(not (memq this-command '(corfu-insert)))))))

;; ══════════════════════════════════════════════════════════════════
;;  Multiple Cursors
;; ══════════════════════════════════════════════════════════════════
;; Edit multiple locations simultaneously — select a word, then
;; SPC m n to add cursors to the next occurrence (like Ctrl-D in VS Code).
;; `mc/always-run-for-all` applies commands to all cursors without asking.

(use-package multiple-cursors
  :straight t
  :defer my/defer-slow
  :commands (mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :config
  (setq mc/always-run-for-all t))

;; ══════════════════════════════════════════════════════════════════
;;  Git — diff-hl (VC-integrated gutter indicators)
;; ══════════════════════════════════════════════════════════════════
;; Shows colored indicators in the left fringe for git changes:
;;   yellow = modified line, green = added line, red = deleted line.
;; Navigate hunks with ]h/[h, revert a hunk with SPC g r.

(use-package diff-hl
  :straight t
  :defer my/defer-medium
  :commands (diff-hl-mode diff-hl-dired-mode global-diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)      ; show changed-file indicators in dired
  :config
  (setq diff-hl-flydiff-delay 2)               ; wait 2s before rechecking (saves CPU)

  ;; Refresh gutter indicators after magit operations (commit, checkout, etc.)
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

  ;; Colors matched to doom-one theme palette (foreground only, no background)
  (custom-set-faces
   '(diff-hl-change ((t (:foreground "#e5c07b" :background nil))))  ; yellow
   '(diff-hl-insert ((t (:foreground "#98c379" :background nil))))  ; green
   '(diff-hl-delete ((t (:foreground "#e06c75" :background nil))))))  ; red

;; ══════════════════════════════════════════════════════════════════
;;  Workspaces — perspective.el
;; ══════════════════════════════════════════════════════════════════
;; Perspectives are named workspaces, each with its own buffer list.
;; Switch with SPC TAB TAB, create with SPC TAB n, close with SPC TAB c.
;; Each perspective remembers which buffers belong to it, so you can
;; work on multiple projects without buffer-list pollution.

(use-package perspective
  :straight t
  :defer my/defer-slow
  :commands (persp-switch persp-kill persp-rename persp-switch-to-buffer)
  :init
  (setq persp-mode-prefix-key (kbd "C-c p"))   ; native prefix (rarely used — we use SPC TAB)
  :config
  (persp-mode 1)
  (setq persp-auto-save-fname (expand-file-name "perspectives" user-emacs-directory)
        persp-show-modestring t                ; show workspace name in modeline
        persp-modestring-short t)              ; abbreviated name
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (and (fboundp 'persp-mode)
                         (boundp 'persp-mode)
                         persp-mode
                         (boundp 'persp-auto-save-fname)
                         persp-auto-save-fname)
                (condition-case _err
                    (persp-state-save)
                  (error nil))))))  ; silently ignore if no perspectives to save

;; SPC TAB 1-9 jumps directly to workspace N (defined in evil-config.el)
(defun my/persp-switch-by-number (n)
  "Switch to the Nth workspace."
  (let ((names (persp-names)))
    (if (<= n (length names))
        (persp-switch (nth (1- n) names))
      (message "Workspace %d does not exist" n))))

;; ══════════════════════════════════════════════════════════════════
;;  Editing Helpers
;; ══════════════════════════════════════════════════════════════════

;; expand-region — press SPC a e to progressively expand the selection
;; (word → symbol → expression → function → ...).  Press "z" to contract.
(use-package expand-region
  :straight t
  :defer my/defer-medium
  :commands er/expand-region
  :config
  (setq expand-region-contract-fast-key "z"    ; z to shrink selection
        expand-region-reset-fast-key "r"))      ; r to reset to initial

;; aggressive-indent — re-indents code as you type.  Only enabled for
;; Lisp-family languages where indentation is semantically meaningful.
;; Excluded from HTML where it would fight with web-mode's indentation.
(use-package aggressive-indent
  :straight t
  :defer my/defer-medium
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode))
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; ══════════════════════════════════════════════════════════════════
;;  Project Management — Projectile
;; ══════════════════════════════════════════════════════════════════
;; Projectile provides project-scoped commands: find file in project
;; (SPC p f), switch project (SPC p p), compile (SPC p c), test (SPC p t).
;; It detects project roots by looking for .git, Makefile, setup.py, etc.

(use-package projectile
  :straight t
  :defer 2
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/projects/" "~/Documents/" "~/work/")  ; scan these for projects
        projectile-completion-system 'default   ; use vertico (our completing-read)
        projectile-enable-caching t             ; cache file lists (faster for large repos)
        projectile-indexing-method 'alien        ; use external tools (fd/find) for file listing
        ;; Files that identify a directory as a project root
        projectile-project-root-files
        '(".projectile" ".git" ".hg" ".svn" "Makefile" "CMakeLists.txt"
          "setup.py" "requirements.txt" "environment.yml")
        ;; Per-project-type commands for SPC p c (compile)
        projectile-project-compilation-cmd
        '((cmake . "cmake --build build/")
          (make . "make")
          (python . "python -m pytest"))
        ;; Per-project-type commands for SPC p t (test)
        projectile-project-test-cmd
        '((cmake . "cd build && ctest")
          (make . "make test")
          (python . "python -m pytest"))
        ;; Per-project-type commands for SPC p r (run)
        projectile-project-run-cmd
        '((python . "python main.py"))))

;; ══════════════════════════════════════════════════════════════════
;;  File Templates (auto-insert)
;; ══════════════════════════════════════════════════════════════════
;; When you create a new .py, .cpp, .h, .html, or .css file, Emacs
;; auto-inserts a template with boilerplate (header guards, docstrings,
;; HTML5 skeleton, etc.).  Templates live in templates/code/ and use
;; __FILENAME__, __DATE__, __GUARD__ placeholders that get expanded.

(defvar my/template-dir (expand-file-name "templates/code" user-emacs-directory)
  "Directory containing file templates.")

(defun my/template-expand ()
  "Replace __FILENAME__, __DATE__, __GUARD__ placeholders in the buffer."
  (let ((filename (file-name-nondirectory (or buffer-file-name "")))
        (date (format-time-string "%Y-%m-%d")))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "__FILENAME__" nil t)
        (replace-match filename t t))
      (goto-char (point-min))
      (while (search-forward "__DATE__" nil t)
        (replace-match date t t))
      (goto-char (point-min))
      (while (search-forward "__GUARD__" nil t)
        (replace-match
         (upcase (replace-regexp-in-string
                  "[^a-zA-Z0-9]" "_"
                  (file-name-nondirectory (or buffer-file-name "HEADER"))))
         t t)))))

(use-package autoinsert
  :straight nil                                ; built-in
  :config
  (auto-insert-mode 1)
  (setq auto-insert-query nil                  ; don't ask "Apply template?" — just do it
        auto-insert-directory my/template-dir) ; where to find template files
  ;; Map file extensions to template files + expander function
  (define-auto-insert "\\.py\\'" ["python.py" my/template-expand])
  (define-auto-insert "\\.\\(cpp\\|cc\\)\\'" ["cpp.cpp" my/template-expand])
  (define-auto-insert "\\.\\(h\\|hpp\\)\\'" ["header.h" my/template-expand])
  (define-auto-insert "\\.html\\'" ["html.html" my/template-expand])
  (define-auto-insert "\\.css\\'" ["css.css" my/template-expand]))

(provide 'modern-languages)
;;; modern-languages.el ends here
