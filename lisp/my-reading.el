;;; my-reading.el --- Reading: EPUB sources, attachments, reading tracker -*- lexical-binding: t; -*-

;;; Commentary:
;; Reading lives in two places and they do different jobs:
;;
;;   references/<slug>.org            one file per source: summary, critique,
;;                                    quotes, and the attached file
;;   references/tracking.org          the queue: what is unread, reading, done
;;
;; EPUB is rendered as text by nov.el, which works in a terminal.  PDF is out of
;; scope by decision: a terminal cannot display pages, and without pdf-tools
;; there is no text layer either.  Annotations are not a feature of nov.el, so
;; notes about a book are taken in Org and linked, not attached to the text.
;;
;; Attachments go through `org-attach' into ~/projects/attachments/, which is
;; already the configured store, so a source file is never duplicated.

;;; Code:

(require 'org)
(require 'org-attach)
(require 'my-org-pkm)

(defcustom my/reading-tracker (expand-file-name "references/tracking.org" my/projects-dir)
  "The file that queues sources."
  :type 'file
  :group 'my/knowledge)

(defcustom my/reading-statuses '("unread" "reading" "done" "abandoned")
  "Allowed values of the STATUS property on a source."
  :type '(repeat string)
  :group 'my/knowledge)

;;;; EPUB

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

;;;; Attachments

(defun my/reading-attach (file)
  "Attach FILE to the current Org entry and report where it went.
The file is copied into `org-attach-id-dir', so the source note and the book
stay together and neither is duplicated."
  (interactive "fAttach file: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Open the source note first"))
  (unless (org-id-get) (org-id-get-create))
  (org-attach-attach file nil 'cp)
  (let* ((dir (org-attach-dir))
         (stored (expand-file-name (file-name-nondirectory file) dir)))
    (message "Attached to %s" (abbreviate-file-name stored))
    stored))

(defun my/reading-open-attachment ()
  "Open the first file attached to the current entry."
  (interactive)
  (let ((files (org-attach-file-list (org-attach-dir))))
    (unless files (user-error "Nothing attached here"))
    (find-file (expand-file-name (car files) (org-attach-dir)))))

;;;; Status and tracking

(defun my/reading-set-status (status)
  "Set the STATUS property of the entry at point and timestamp the change."
  (interactive (list (completing-read "Status: " my/reading-statuses nil t)))
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
  (org-set-property "STATUS" status)
  (org-set-property "STATUS_CHANGED" (format-time-string "%Y-%m-%d"))
  (when (equal status "done")
    (org-set-property "FINISHED" (format-time-string "%Y-%m-%d")))
  (message "Status: %s" status))

(defun my/reading-queue-source (title)
  "Add TITLE to the reading queue with STATUS unread."
  (interactive "sSource title: ")
  (let ((file my/reading-tracker))
    (make-directory (file-name-directory file) t)
    (unless (file-exists-p file)
      (with-temp-file file
        (insert (concat "#+TITLE: Reading tracker\n" (my/knowledge-todo-line)
                        "\n#+STARTUP: overview\n\n* Reading\n* Up next\n* Finished\n"))))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Reading$" nil t)
        (goto-char (point-max))
        (insert "\n* Reading\n"))
      (end-of-line)
      (insert (format "\n** %s\n:PROPERTIES:\n:STATUS: unread\n:ADDED: %s\n:END:\n"
                      title (format-time-string "%Y-%m-%d")))
      (save-buffer)
      (message "Queued %s" title))))

(defun my/reading-open-tracker ()
  "Open the reading tracker."
  (interactive)
  (find-file my/reading-tracker))

;;;; Keys

(global-set-key (kbd "C-c k A") #'my/reading-attach)
(global-set-key (kbd "C-c k o") #'my/reading-open-attachment)
(global-set-key (kbd "C-c k R") #'my/reading-open-tracker)
(global-set-key (kbd "C-c k S") #'my/reading-set-status)

(provide 'my-reading)
;;; my-reading.el ends here
