;;; my-reading.el --- EPUB file routing -*- lexical-binding: t; -*-

;;; Commentary:
;; nov.el renders EPUB as text, so it works in a terminal.  The reading queue,
;; attachments and status commands went away with the old note tree; only the
;; file association remains, since it still works on any EPUB.

;;; Code:

(defun my/reading-setup-epub ()
  "Route .epub files to nov-mode.
Emacs maps .epub to `archive-mode' by itself, so this has to come first in
`auto-mode-alist' rather than merely being present."
  (when (require 'nov nil t)
    (setq auto-mode-alist
          ;; `auto-mode-alist' entries are not always (REGEXP . FUNCTION): some
          ;; carry an extra element, so compare with `eq' on `cdr-safe'.
          (cons '("\\.epub\\'" . nov-mode)
                (seq-remove (lambda (entry) (eq (cdr-safe entry) 'nov-mode))
                            auto-mode-alist)))))

(my/reading-setup-epub)

(provide 'my-reading)
;;; my-reading.el ends here
