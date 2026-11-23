;;; my-roam.el --- Org-roam over the knowledge tree -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-roam indexes the zettelkasten (~/knowledge: literature-box/ and
;; main-box/) into a SQLite database.  The Org files are the source of truth:
;; delete the database and `org-roam-db-sync' rebuilds it from scratch, which
;; the stage-3 checks prove.
;;
;; Zettel files are created and numbered by hand — there is deliberately no
;; capture template, because a template cannot know the next number.  The
;; only machine help a new zettel needs is `my/roam-ensure-file-id' (C-c k e),
;; which adds the ID drawer Org-roam reads.
;;
;; Node policy: a file-level node for every document that carries a top-of-file
;; ID property drawer, plus a node for any heading that has an ID.  Headings
;; are not given IDs automatically — a node nobody links to is a maintenance
;; cost with no payoff.
;;
;; `org-roam-capture-' is refused outright: node-find and node-insert both
;; route a missing title into it, and its template machinery cannot know the
;; next number in a box.  Closing it here is the one guarantee that no
;; unnumbered file lands in the zettelkasten.

;;; Code:

(require 'org-roam)
(require 'org-roam-node)
(require 'org-roam-db)
(require 'my-org-pkm)

(defcustom my/roam-db-location (locate-user-emacs-file "org-roam.db")
  "Where the Org-roam index lives.
Deliberately outside the knowledge repository: it is a rebuildable index, not
knowledge, and it must never be committed."
  :type 'file
  :group 'my/knowledge)

(setq org-roam-directory (directory-file-name my/knowledge-dir)
      org-roam-db-location my/roam-db-location
      org-roam-completion-everywhere t
      ;; The zettelkasten holds nothing but notes; there is nothing to exclude.
      org-roam-file-exclude-regexp nil
      org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:12}" 'face 'org-tag))
      org-roam-buffer-position 'right
      org-roam-buffer-width 0.35)

;;;; Nodes

(defun my/roam-node-id (file)
  "Return the file-level ID of FILE, or nil."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (org-id-get))))

(defun my/roam-ensure-file-id ()
  "Give the current file a top-of-file ID, creating the drawer if needed.
Org-roam reads the file-level node from the property drawer at point-min, so
the drawer must come before the first heading."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((existing (org-id-get)))
      (if existing
          existing
        (let ((id (org-id-new)))
          (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" id))
          (save-buffer)
          id)))))

(defun my/roam-sync ()
  "Re-index the zettelkasten and report what is in the index."
  (interactive)
  (org-roam-db-sync 'force)
  (message "Org-roam indexed %d nodes, %d links"
           (caar (org-roam-db-query [:select (funcall count *) :from nodes]))
           (caar (org-roam-db-query [:select (funcall count *) :from links]))))

(defun my/roam-enable ()
  "Turn on autosync after startup, so it never slows a cold start."
  (when (and (file-directory-p org-roam-directory)
             (file-exists-p org-roam-db-location))
    (org-roam-db-autosync-mode 1)))

;;;; Links and backlinks

(defun my/roam-insert-node ()
  "Insert a link to a node of the zettelkasten."
  (interactive)
  (require 'org-roam-node)
  (org-roam-node-insert))

(defun my/roam-find-node ()
  "Jump to a node of the zettelkasten."
  (interactive)
  (org-roam-node-find))

(defun my/roam-toggle-buffer ()
  "Show or hide the backlinks buffer for the current note."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Backlinks are for Org buffers"))
  (org-roam-buffer-toggle))

(defun my/roam-open-ui ()
  "Open the Org-roam graph in a browser, if org-roam-ui is installed."
  (interactive)
  (if (require 'org-roam-ui nil t)
      (progn
        (org-roam-ui-mode 1)
        (org-roam-ui-open))
    (user-error "org-roam-ui is not installed; graph export needs it")))

;;;; Capture is refused

(defun my/roam-refuse-capture (&rest _)
  "Refuse Org-roam capture: zettel files are created and numbered by hand.
C-x C-f into a box, then C-c k e for the ID drawer."
  (user-error "Zettel files are created by hand: C-x C-f into a box, then C-c k e"))

(advice-add 'org-roam-capture- :override #'my/roam-refuse-capture)

;;;; Keys

;; Creating a zettel is manual: C-x C-f, a #+TITLE:, then C-c k e for the ID.
(global-set-key (kbd "C-c k e") #'my/roam-ensure-file-id)
(global-set-key (kbd "C-c k f") #'my/roam-find-node)
(global-set-key (kbd "C-c k v") #'my/roam-toggle-buffer)
(global-set-key (kbd "C-c k g") #'my/roam-open-ui)
(global-set-key (kbd "C-c k s") #'my/roam-sync)

(run-with-idle-timer 5 nil #'my/roam-enable)

(provide 'my-roam)
;;; my-roam.el ends here
