;;; stage5-check.el --- Headless acceptance checks for EPUB routing -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage5-check.el
;;
;; Builds a real EPUB in a temporary directory and opens it with nov.  The
;; reading queue and attachment commands were removed with the old tree.

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

(when (file-exists-p my/stage5-epub) (delete-file my/stage5-epub))

(let ((builder (expand-file-name "lisp/tests/make-test-epub.sh" user-emacs-directory)))
  (my/stage5-check "the EPUB fixture builder exists" (file-executable-p builder))
  (my/stage5-check "it produces an EPUB"
                   (and (zerop (call-process builder nil nil nil my/stage5-epub))
                        (file-exists-p my/stage5-epub))))

(let ((buffer (find-file-noselect my/stage5-epub)))
  (with-current-buffer buffer
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
    (let ((chapter-index nil))
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
                         (format "%S" (substring text 0 (min 120 (length text)))))))
    (kill-buffer)))

(when (file-exists-p my/stage5-epub) (delete-file my/stage5-epub))

(princ (format "\n%d checks, %d failures\n" my/stage5-checks my/stage5-failures))
(when (> my/stage5-failures 0)
  (kill-emacs 1))

;;; stage5-check.el ends here
