;;; stage5-check.el --- Headless acceptance checks for Stage 5 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage5-check.el
;;
;; Builds a real EPUB with lisp/tests/make-test-epub.sh, opens it with nov,
;; attaches it to a source note, moves the status through its values, and
;; checks the reading query on the home dashboard sees it.

;;; Code:

(require 'cl-lib)

(defvar my/stage5-failures 0)
(defvar my/stage5-checks 0)

(defun my/stage5-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage5-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage5-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defvar my/stage5-epub (expand-file-name "zz-stage5-book.epub" temporary-file-directory))
(defvar my/stage5-note (expand-file-name "zz-stage5-reading-fixture.org"
                                         (my/projects-subdir "references"))
  "The path `my/new-reference' will produce for the fixture title.")
(defvar my/stage5-tracker (my/projects-subdir "references/tracking.org"))

(defun my/stage5-remove-empty-attachment-dirs ()
  "Delete empty directories under the attachment store, keeping the root.
`org-attach' nests by ID prefix, so a fixture can be two levels deep and a
single-level wildcard would miss it."
  (let* ((root (expand-file-name "attachments" my/projects-dir))
         (dirs (directory-files-recursively root "" t)))
    (dolist (dir (sort dirs (lambda (a b) (> (length a) (length b)))))
      (when (and (file-directory-p dir)
                 (not (equal (directory-file-name dir) (directory-file-name root)))
                 (null (directory-files dir nil "\\`[^.]")))
        (delete-directory dir)))))

(defun my/stage5-cleanup ()
  "Remove fixtures, their attachments and their tracker entries.
Tracker entries are removed with `org-cut-subtree', not a regexp: a regexp
that misses the property drawer leaves orphan drawers behind, which is exactly
what an earlier version of this file did."
  (when (get-file-buffer my/stage5-note)
    (with-current-buffer (get-file-buffer my/stage5-note)
      (set-buffer-modified-p nil)
      (kill-buffer)))
  (dolist (f (list my/stage5-note my/stage5-epub))
    (when (file-exists-p f) (delete-file f)))
  (let ((root (expand-file-name "attachments" my/projects-dir)))
    (when (file-directory-p root)
      (dolist (f (directory-files-recursively root "" nil))
        (when (and (file-regular-p f)
                   (string-match-p "zz-stage5" (file-name-nondirectory f)))
          (delete-file f)))
      (my/stage5-remove-empty-attachment-dirs)))
  (when (file-exists-p my/stage5-tracker)
    (with-current-buffer (find-file-noselect my/stage5-tracker)
      (let (marks)
        (org-map-entries
         (lambda ()
           (when (string-match-p "zz-stage5" (or (org-get-heading t t t t) ""))
             (push (point-marker) marks))))
        (dolist (mark (sort marks (lambda (a b) (> a b))))
          (goto-char mark)
          (org-cut-subtree))
        (when marks (save-buffer))))))

;;;; 1. A real EPUB opens in nov-mode and renders as text

(my/stage5-cleanup)

(let ((builder (expand-file-name "lisp/tests/make-test-epub.sh" user-emacs-directory)))
  (my/stage5-check "the EPUB fixture builder exists" (file-executable-p builder))
  (my/stage5-check "it produces an EPUB"
                   (and (zerop (call-process builder nil nil nil my/stage5-epub))
                        (file-exists-p my/stage5-epub))))

(let ((buffer (find-file-noselect my/stage5-epub)))
  (with-current-buffer buffer
    ;; A batch frame has no useful width; a terminal has one.  80 is a
    ;; terminal, so the render is measured against a realistic width.
    (setq nov-text-width 80)
    (my/stage5-check "an .epub buffer uses nov-mode"
                     (eq major-mode 'nov-mode) (format "%S" major-mode))
    (my/stage5-check "metadata is read from the package"
                     (equal (cdr (assq 'title nov-metadata)) "Test Book On Retrieval")
                     (format "%S" (cdr (assq 'title nov-metadata))))
    (my/stage5-check "the author is read too"
                     (equal (cdr (assq 'creator nov-metadata)) "Fixture Author"))
    (my/stage5-check "the spine is parsed into documents"
                     (> (length nov-documents) 1))
    ;; Jump to the first chapter and check the text rendered.
    (let ((chapter-index nil))
      ;; nov stores each spine item as (ID . PATH).
      (cl-loop for doc across nov-documents for i from 0
               when (string-match-p "chapter1" (format "%s" (cdr-safe doc)))
               do (setq chapter-index i))
      (when chapter-index
        (setq nov-documents-index chapter-index)
        (nov-render-document))
      (my/stage5-check "the chapter is located in the spine"
                       (integerp chapter-index) (format "%S" chapter-index))
      (let ((text (replace-regexp-in-string
                   "[ \t\n]+" " "
                   (buffer-substring-no-properties (point-min) (point-max)))))
        (my/stage5-check "the chapter text is rendered for a terminal"
                         (string-match-p "Retrieval finds the passages" text)
                         (format "%S" (substring text 0 (min 120 (length text))))))
      (kill-buffer buffer))))

;;;; 2. Attachments land in the store, not next to the note

(my/new-reference "zz stage5 reading fixture")
(with-current-buffer (find-file-noselect my/stage5-note)
  (let ((attached (condition-case e (my/reading-attach my/stage5-epub)
                    (error (princ (format "  (attach error: %S)\n" e)) nil))))
    (my/stage5-check "the EPUB is copied into the attachment store"
                     (and attached (string-match-p "/attachments/" attached))
                     (format "%S" attached))
    (my/stage5-check "and the file really is there"
                     (and attached (file-exists-p attached)))
    (my/stage5-check "no copy is left beside the note"
                     (not (file-exists-p (expand-file-name "zz-stage5-book.epub"
                                                           (file-name-directory my/stage5-note)))))
    (save-buffer))

  ;; Status moves through its values and records when.
  (goto-char (point-min))
  (my/reading-set-status "reading")
  (my/stage5-check "status can be set to reading"
                   (equal "reading" (org-entry-get nil "STATUS")))
  (my/reading-set-status "done")
  (my/stage5-check "status can be set to done"
                   (equal "done" (org-entry-get nil "STATUS")))
  (my/stage5-check "finishing records the date"
                   (let ((v (org-entry-get nil "FINISHED")))
                     (and v (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\'" v))))
  (save-buffer))

;;;; 3. A queued source appears in the tracker and on the dashboard

(my/reading-queue-source "zz-stage5 reading fixture")
(with-current-buffer (find-file-noselect my/stage5-tracker)
  (my/stage5-check "a queued source becomes a tracker entry"
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^\\*\\* zz-stage5 reading fixture$" nil t)))
  (my/stage5-check "with STATUS unread"
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward "^\\*\\* zz-stage5 reading fixture$" nil t)
                     (equal "unread" (org-entry-get nil "STATUS")))))

(my/refresh-dblocks-in-file (my/projects-subdir "dashboards/home.org"))
(with-current-buffer (find-file-noselect (my/projects-subdir "dashboards/home.org"))
  (my/stage5-check "the reading block on the home dashboard lists it"
                   (save-excursion
                     (goto-char (point-min))
                     (and (re-search-forward "^#\\+BEGIN: my-tasks :query (not (done)) :files my/reference-files" nil t)
                          (let ((end (save-excursion (re-search-forward "^#\\+END:" nil t) (point))))
                            (string-match-p "zz-stage5 reading fixture"
                                            (buffer-substring-no-properties (point) end)))))))

;;;; 4. Cleanup

(my/stage5-cleanup)
(my/stage5-check "the source note is gone"
                 (not (file-exists-p my/stage5-note)))
(my/stage5-check "the attachment directory is gone"
                 (not (cl-some (lambda (dir)
                                 (and (file-directory-p dir)
                                      (cl-some (lambda (f) (string-match-p "zz-stage5-book" f))
                                               (directory-files dir))))
                               (file-expand-wildcards
                                (expand-file-name "attachments/*" my/knowledge-dir)))))
(my/refresh-dblocks-in-file (my/projects-subdir "dashboards/home.org"))
(my/stage5-check "the dashboard is clean again"
                 (with-current-buffer (find-file-noselect (my/projects-subdir "dashboards/home.org"))
                   (not (string-match-p "zz-stage5 reading fixture"
                                        (buffer-substring-no-properties (point-min) (point-max))))))

(princ (format "\n%d checks, %d failures\n" my/stage5-checks my/stage5-failures))
(when (> my/stage5-failures 0)
  (kill-emacs 1))

;;; stage5-check.el ends here
