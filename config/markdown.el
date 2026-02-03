;;; config/markdown.el --- Markdown support configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Markdown editing support with live preview and formatting

;;; Code:

(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-gfm-additional-languages '("sh"))
  
  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )

(use-package grip-mode
  :straight t
  :defer t
  :commands grip-mode
  :config
  (setq grip-preview-use-webkit t))

;; markdown-toc: Auto-generate table of contents
(use-package markdown-toc
  :straight t
  :defer t
  :after markdown-mode
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-refresh-toc))

;; evil-markdown: Better Evil keybindings in Markdown
(use-package evil-markdown
  :straight (evil-markdown :type git :host github :repo "Somelauw/evil-markdown")
  :after (evil markdown-mode)
  :defer t
  :hook (markdown-mode . evil-markdown-mode))

;; edit-indirect: Edit embedded code blocks in their native major mode
(use-package edit-indirect
  :straight t
  :defer t
  :commands edit-indirect-region
  :config
  (defun my/markdown-edit-code-block ()
    "Edit the current markdown code block in its native major mode."
    (interactive)
    (save-excursion
      (let ((start (progn
                     (re-search-backward "^```" nil t)
                     (forward-line 1)
                     (point)))
            (end (progn
                   (re-search-forward "^```" nil t)
                   (forward-line 0)
                   (point))))
        (edit-indirect-region start end t)))))

;; Bind code block editing in markdown-mode
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c '") #'my/markdown-edit-code-block))

(provide 'markdown)
;;; markdown.el ends here