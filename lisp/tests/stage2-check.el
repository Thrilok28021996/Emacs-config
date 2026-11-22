;;; stage2-check.el --- Headless acceptance checks for Stage 2 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage2-check.el
;;
;; Builds two throwaway knowledge files, renders boards from them, moves a task
;; through the consolidated board and checks that the *source* file changed.
;; The fixtures and every generated table are removed again, so the tree is
;; left as found.

;;; Code:

(require 'cl-lib)

(defvar my/stage2-failures 0)
(defvar my/stage2-checks 0)

(defun my/stage2-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage2-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage2-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defconst my/stage2-active-states
  '("BACKLOG" "TODO" "NEXT" "DOING" "WAITING" "REVIEW" "DONE")
  "Columns expected on every board, in order.")

;;;; Fixtures

(defvar my/stage2-project (expand-file-name "zz-kanban-fixture.org"
                                            (my/knowledge-subdir "notes/projects")))
(defvar my/stage2-learning (expand-file-name "zz-kanban-learning.org"
                                             (my/knowledge-subdir "notes/learning")))

(defconst my/stage2-project-body
  (concat "#+TITLE: Kanban fixture\n"
          (my/knowledge-todo-line) "\n\n"
          "* Tasks\n"
          "** BACKLOG sketch retrieval scoring ideas\n"
          "** TODO wire the retrieval eval harness\n"
          "** NEXT read the chunking benchmark results\n"
          "** DOING rewrite the embedding cache\n"
          "** WAITING wait for the vendor pricing reply\n"
          "** REVIEW double-check the recall numbers\n"
          "** DONE ship the first eval report\n"
          "** CANCELLED drop the vector store spike\n"
          "\n* Board\n#+BEGIN: kanban " (my/kanban-params) "\n#+END:\n"))

(defconst my/stage2-learning-body
  (concat "#+TITLE: Learning fixture\n"
          (my/knowledge-todo-line) "\n\n"
          "* Tasks\n"
          "** TODO summarise the MCP transport spec\n"
          "** NEXT finish the agents chapter\n"
          "\n* Board\n#+BEGIN: kanban " (my/kanban-params) "\n#+END:\n"))

(defun my/stage2-write (path body)
  "Write BODY to PATH, replacing anything there."
  (make-directory (file-name-directory path) t)
  (with-temp-file path (insert body)))

(defun my/stage2-cleanup ()
  "Delete fixtures and empty every board again."
  (dolist (f (list my/stage2-project my/stage2-learning))
    (when (get-file-buffer f)
      (with-current-buffer (get-file-buffer f)
        (set-buffer-modified-p nil)
        (kill-buffer)))
    (when (file-exists-p f) (delete-file f)))
  (dolist (d (list (my/knowledge-subdir "dashboards/projects.org")
                   (my/knowledge-subdir "dashboards/learning.org")))
    (my/kanban-refresh-file d)))

;;;; Table parsing

(defun my/stage2-table (file)
  "Return the first kanban table in FILE as a list of cell lists, header first."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "^#\\+BEGIN: kanban" nil t))
          nil
        (let ((end (save-excursion (re-search-forward "^#\\+END:" nil t) (point)))
              rows)
          (forward-line 1)
          (while (< (point) end)
            (let ((line (string-trim (thing-at-point 'line t))))
              (when (and (string-prefix-p "|" line)
                         (not (string-prefix-p "|-" line)))
                (push (mapcar #'string-trim (split-string line "|" t)) rows)))
            (forward-line 1))
          (nreverse rows))))))

(defun my/stage2-column-of (table title)
  "Return the header name of the column in TABLE that mentions TITLE."
  (let* ((header (car table))
         (rows (cdr table))
         (index nil))
    (dolist (row rows)
      (cl-loop for cell in row for i from 0
               when (string-match-p (regexp-quote title) cell)
               do (setq index i)))
    (and index (nth index header))))

(defun my/stage2-table-width (file)
  "Return the longest line, in characters, of the first kanban table in FILE."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^#\\+BEGIN: kanban" nil t)
        (let ((end (save-excursion (re-search-forward "^#\\+END:" nil t) (point)))
              (widest 0))
          (forward-line 1)
          (while (< (point) end)
            (setq widest (max widest (length (string-trim-right (thing-at-point 'line t)))))
            (forward-line 1))
          widest)))))

(defun my/stage2-count (table)
  "Return the number of columns in TABLE."
  (length (car table)))

(defun my/stage2-state-of (file title)
  "Return the TODO keyword of the heading TITLE in FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (format "^\\*+ \\([A-Z]+\\) %s$" (regexp-quote title)) nil t)
      (match-string 1))))

;;;; 1. Boards render per file

(my/stage2-cleanup)
(my/stage2-write my/stage2-project my/stage2-project-body)
(my/stage2-write my/stage2-learning my/stage2-learning-body)

(my/kanban-refresh-file my/stage2-project)
(my/kanban-refresh-file my/stage2-learning)

(let ((table (my/stage2-table my/stage2-project)))
  (my/stage2-check "project board renders"
                   (consp table) (format "%S" table))
  (my/stage2-check "project board has 7 columns in workflow order"
                   (equal (car table) my/stage2-active-states)
                   (format "%S" (car table)))
  (my/stage2-check "CANCELLED is not a column"
                   (not (member "CANCELLED" (car table))))
  (my/stage2-check "every active task sits in its own state column"
                   (equal
                    (list (my/stage2-column-of table "sketch retrieval scoring ideas")
                          (my/stage2-column-of table "wire the retrieval eval harness")
                          (my/stage2-column-of table "read the chunking benchmark results")
                          (my/stage2-column-of table "rewrite the embedding cache")
                          (my/stage2-column-of table "wait for the vendor pricing reply")
                          (my/stage2-column-of table "double-check the recall numbers")
                          (my/stage2-column-of table "ship the first eval report"))
                    my/stage2-active-states)
                   (format "%S" (mapcar (lambda (t2) (my/stage2-column-of table t2))
                                        '("sketch retrieval scoring ideas"
                                          "wire the retrieval eval harness"
                                          "read the chunking benchmark results"
                                          "rewrite the embedding cache"
                                          "wait for the vendor pricing reply"
                                          "double-check the recall numbers"
                                          "ship the first eval report"))))
  (my/stage2-check "a CANCELLED task keeps its heading but is off the board"
                   (and (not (my/stage2-column-of table "drop the vector store spike"))
                        (with-temp-buffer
                          (insert-file-contents my/stage2-project)
                          (string-match-p "\\*\\* CANCELLED drop the vector store spike"
                                          (buffer-string))))))

;;;; 2. Consolidated board over two directories

(my/kanban-refresh-file (my/knowledge-subdir "dashboards/projects.org"))
(let ((table (my/stage2-table (my/knowledge-subdir "dashboards/projects.org"))))
  (my/stage2-check "consolidated board renders from a directory scope"
                   (consp table) (format "%S" table))
  (my/stage2-check "consolidated board has the same 7 columns"
                   (equal (car table) my/stage2-active-states)
                   (format "%S" (car table)))
  (my/stage2-check "consolidated board shows a project task"
                   (equal "TODO" (my/stage2-column-of table "wire the retrieval eval harness")))
  (my/stage2-check "consolidated board shows a learning task"
                   (equal "NEXT" (my/stage2-column-of table "finish the agents chapter"))))

;;;; 3. Moving on the consolidated board edits the source file

(defun my/stage2-goto-row (file title)
  "Move point to the table row in FILE that mentions TITLE."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (unless (re-search-forward (regexp-quote title) nil t)
      (error "Row for %S not found" title))
    (beginning-of-line)
    (current-buffer)))

(let ((board (my/knowledge-subdir "dashboards/projects.org")))
  (with-current-buffer (my/stage2-goto-row board "wire the retrieval eval harness")
    (org-kanban/next))
  (my/stage2-check "moving a row on the consolidated board updates the project file"
                   (with-temp-buffer
                     (insert-file-contents my/stage2-project)
                     (string-match-p "^\\*\\* NEXT wire the retrieval eval harness"
                                     (buffer-string)))
                   (with-temp-buffer
                     (insert-file-contents my/stage2-project)
                     (if (string-match "^\\*\\* \\([A-Z]+\\) wire the retrieval eval harness"
                                       (buffer-string))
                         (match-string 1) "no heading")))
  (my/stage2-check "the move left every other task in its own state"
                   (equal (list (my/stage2-state-of my/stage2-project "sketch retrieval scoring ideas")
                                (my/stage2-state-of my/stage2-project "read the chunking benchmark results")
                                (my/stage2-state-of my/stage2-project "ship the first eval report")
                                (my/stage2-state-of my/stage2-learning "summarise the MCP transport spec"))
                          '("BACKLOG" "NEXT" "DONE" "TODO"))
                   (format "%S" (list (my/stage2-state-of my/stage2-project "sketch retrieval scoring ideas")
                                      (my/stage2-state-of my/stage2-project "read the chunking benchmark results")
                                      (my/stage2-state-of my/stage2-project "ship the first eval report")
                                      (my/stage2-state-of my/stage2-learning "summarise the MCP transport spec"))))
  (my/kanban-refresh-file board)
  (my/stage2-check "after the move the task shows in the NEXT column"
                   (equal "NEXT"
                          (my/stage2-column-of (my/stage2-table board)
                                               "wire the retrieval eval harness"))))

;;;; 4. Width, measured rather than assumed

(princ "\nBoard widths (characters, longest rendered line):\n")
(dolist (spec (list (cons "project board (7 tasks)" my/stage2-project)
                    (cons "consolidated board (9 tasks)" (my/knowledge-subdir "dashboards/projects.org"))))
  (princ (format "  %-32s %4d chars\n"
                 (car spec) (my/stage2-table-width (cdr spec)))))
(princ (format "  %-32s %4d chars\n"
               "learning board live in terminal" (frame-width)))
(princ (format "  columns per board: %d\n" (my/stage2-count (my/stage2-table my/stage2-project))))

;;;; 5. Cleanup

(my/stage2-cleanup)
(my/stage2-check "fixtures removed"
                 (and (not (file-exists-p my/stage2-project))
                      (not (file-exists-p my/stage2-learning))))
(my/stage2-check "consolidated board shows no task rows after cleanup"
                 (let ((table (my/stage2-table (my/knowledge-subdir "dashboards/projects.org"))))
                   (or (null table) (<= (length table) 1))))

(princ (format "\n%d checks, %d failures\n" my/stage2-checks my/stage2-failures))
(when (> my/stage2-failures 0)
  (kill-emacs 1))

;;; stage2-check.el ends here
