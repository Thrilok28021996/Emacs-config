;;; my-help.el --- Documentation opener -*- lexical-binding: t; -*-

;;; Commentary:
;; C-c k H opens ORG-PKM.org from any buffer.  The generated key listing is
;; gone: the cheatsheet is checked against the live keymaps by stage7-check.el.

;;; Code:

(defgroup my/help nil
  "Help for this configuration."
  :group 'convenience
  :prefix "my/")

(defun my/help-open-doc ()
  "Open the ORG-PKM document that explains the remaining Org setup."
  (interactive)
  (let ((doc (expand-file-name "ORG-PKM.org" user-emacs-directory)))
    (if (file-readable-p doc)
        (find-file doc)
      (user-error "Documentation not found: %s" doc))))

(global-set-key (kbd "C-c k H") #'my/help-open-doc)

(provide 'my-help)
;;; my-help.el ends here
