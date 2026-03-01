;;; config/markdown.el --- Markdown support configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown editing loaded on-demand when opening .md/.markdown files.
;;
;; Provides:
;;   markdown-mode     — syntax highlighting, outline folding, export
;;   gfm-mode          — GitHub-Flavored Markdown (for README.md)
;;   grip-mode         — live preview via GitHub's API (requires grip)
;;   markdown-toc      — auto-generate table of contents
;;   evil-markdown     — Vim-style text objects and motions for markdown
;;   edit-indirect     — edit fenced code blocks in their native major mode
;;
;; Key bindings (see evil-config.el for SPC m prefix):
;;   C-c '   edit the code block under cursor in its native mode

;;; Code:

;; Silence byte-compiler warnings for deferred variables and functions
(defvar markdown-mode-map)
(declare-function edit-indirect-region "edit-indirect")

;; ══════════════════════════════════════════════════════════════════
;;  Markdown Mode
;; ══════════════════════════════════════════════════════════════════
;; Core markdown support.  Uses `multimarkdown` as the external
;; renderer (install: brew install multimarkdown).  README.md files
;; use gfm-mode for GitHub-flavored syntax (task lists, tables, etc.).
(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "multimarkdown")            ; external renderer
  (setq markdown-fontify-code-blocks-natively t)     ; syntax-highlight fenced code
  (setq markdown-enable-wiki-links t)                ; [[wiki-style]] links
  (setq markdown-italic-underscore t)                ; _italic_ (not *italic*)
  (setq markdown-asymmetric-header t)                ; # Header (no trailing #)
  (setq markdown-gfm-additional-languages '("sh"))   ; recognize ```sh blocks
  )

;; grip-mode — live preview using GitHub's Markdown API.
;; Requires: pip install grip.  Opens a local web server and
;; renders the markdown exactly as GitHub would display it.
(use-package grip-mode
  :straight t
  :defer t
  :commands grip-mode
  :config
  (setq grip-preview-use-webkit t))                  ; use webkit instead of browser

;; markdown-toc — auto-generate a table of contents from headings.
;; M-x markdown-toc-generate-toc to insert, *-refresh-toc to update.
(use-package markdown-toc
  :straight t
  :defer t
  :after markdown-mode
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-refresh-toc))

;; evil-markdown — adds Evil text objects for markdown elements
;; (e.g., `vi*` for italic, `vib` for bold, header motions).
(use-package evil-markdown
  :straight (evil-markdown :type git :host github :repo "Somelauw/evil-markdown")
  :after (evil markdown-mode)
  :defer t
  :hook (markdown-mode . evil-markdown-mode))

;; edit-indirect — C-c ' opens the fenced code block under cursor
;; in a separate buffer with the appropriate major mode (e.g.,
;; ```python block opens in python-mode).  Changes are written back.
(use-package edit-indirect
  :straight t
  :defer t
  :commands edit-indirect-region)

(defun my/markdown-edit-code-block ()
  "Edit the current markdown code block in its native major mode."
  (interactive)
  (require 'edit-indirect)
  (save-excursion
    (let ((start (progn
                   (re-search-backward "^```" nil t)
                   (forward-line 1)
                   (point)))
          (end (progn
                 (re-search-forward "^```" nil t)
                 (forward-line 0)
                 (point))))
      (edit-indirect-region start end t))))

;; Bind code block editing in markdown-mode
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c '") #'my/markdown-edit-code-block))

(provide 'markdown)
;;; markdown.el ends here