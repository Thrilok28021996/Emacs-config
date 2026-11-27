;;; my-org-pkm.el --- Shared Org settings for the zettelkasten -*- lexical-binding: t; -*-

;;; Commentary:
;; Zettel files are created and numbered by hand in ~/knowledge.  This file
;; only holds the directory, the workflow states and the Org settings every
;; note shares.  Org-roam itself is configured in my-roam.el.

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'cl-lib)

(defgroup my/knowledge nil
  "Personal Org knowledge system."
  :group 'org
  :prefix "my/")

(defcustom my/knowledge-dir (expand-file-name "~/knowledge/")
  "Root of the zettelkasten: literature notes and permanent notes."
  :type 'directory
  :group 'my/knowledge)

(defcustom my/knowledge-todo-states
  '(("BACKLOG" . ?b) ("TODO" . ?t) ("NEXT" . ?n) ("DOING" . ?o)
    ("WAITING" . ?w) ("REVIEW" . ?r) ("DONE" . ?d) ("CANCELLED" . ?c))
  "TODO states in workflow order, with their fast-selection keys."
  :type '(alist :key-type string :value-type character)
  :group 'my/knowledge)

(defcustom my/knowledge-done-states '("DONE" "CANCELLED")
  "States that end an item."
  :type '(repeat string)
  :group 'my/knowledge)

(defun my/--todo-split ()
  "Return (ACTIVE . DONE) state alists from `my/knowledge-todo-states'."
  (cons (seq-remove (lambda (s) (member (car s) my/knowledge-done-states))
                    my/knowledge-todo-states)
        (seq-filter (lambda (s) (member (car s) my/knowledge-done-states))
                    my/knowledge-todo-states)))

(defun my/knowledge-todo-keywords ()
  "Build the `org-todo-keywords' value: active states, '|', done states."
  (let* ((split (my/--todo-split))
         (fmt (lambda (s) (format "%s(%c)" (car s) (cdr s)))))
    (list (append (list 'sequence)
                  (mapcar fmt (car split))
                  (list "|")
                  (mapcar fmt (cdr split))))))

(defun my/knowledge-todo-line ()
  "The `#+TODO:' line that keeps the workflow identical across files."
  (let* ((split (my/--todo-split))
         (fmt (lambda (s) (format "%s(%c)" (car s) (cdr s)))))
    (format "#+TODO: %s | %s"
            (string-join (mapcar fmt (car split)) " ")
            (string-join (mapcar fmt (cdr split)) " "))))

(setq org-directory my/knowledge-dir
      org-todo-keywords (my/knowledge-todo-keywords)
      org-todo-keyword-faces
      '(("BACKLOG" . (:foreground "gray60"))
        ("TODO" . org-todo)
        ("NEXT" . (:foreground "orange" :weight bold))
        ("DOING" . (:foreground "gold" :weight bold))
        ("WAITING" . (:foreground "mediumorchid" :weight bold))
        ("REVIEW" . (:foreground "deepskyblue" :weight bold))
        ("DONE" . org-done)
        ("CANCELLED" . (:foreground "salmon")))
      org-enforce-todo-dependencies t
      org-log-done 'time
      org-log-into-drawer t
      org-log-repeat 'time
      org-archive-location "%s_archive::* Archived"
      org-startup-indented t
      org-startup-folded 'overview
      org-attach-id-dir (expand-file-name "attachments" my/knowledge-dir)
      org-id-locations-file (expand-file-name ".org-id-locations" my/knowledge-dir)
      org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
      org-startup-with-latex-preview nil)

(provide 'my-org-pkm)
;;; my-org-pkm.el ends here
