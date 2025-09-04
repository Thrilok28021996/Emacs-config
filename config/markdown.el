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
  (setq grip-preview-use-webkit t)
  
  ;; Keybindings now handled in modules/evil-config.el to avoid conflicts
  )

(provide 'markdown)
;;; markdown.el ends here