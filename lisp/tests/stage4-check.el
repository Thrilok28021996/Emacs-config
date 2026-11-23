;;; stage4-check.el --- Headless acceptance checks for Stage 4 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage4-check.el
;;
;; Builds a project and a learning subject with a spread of states and
;; deadlines, regenerates the dashboards, and checks the numbers in the tables
;; against the fixtures. Then builds the three agenda dashboards and checks what
;; each one contains.

;;; Code:

(require 'cl-lib)

(defvar my/stage4-failures 0)
(defvar my/stage4-checks 0)

(defun my/stage4-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage4-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage4-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defvar my/stage4-project (expand-file-name "zz-stage4-project.org"
                                            (my/projects-subdir "projects")))
(defvar my/stage4-learning (expand-file-name "zz-stage4-subject.org"
                                             (my/projects-subdir "learning")))
(defvar my/stage4-idle-project (expand-file-name "zz-stage4-idle.org"
                                                 (my/projects-subdir "projects")))

(defun my/stage4-day (n)
  "Return the date N days from today as an Org timestamp string."
  (format-time-string "<%Y-%m-%d %a>" (time-add nil (* n 86400))))

(defun my/stage4-write ()
  "Write the fixtures."
  (let ((today (my/knowledge-todo-line)))
    (with-temp-file my/stage4-project
      (insert (concat (my/knowledge-todo-line) "\n\n* Tasks\n"
                      "** NEXT wire the eval harness\n"
                      "** TODO write the scoring notes\n   DEADLINE: " (my/stage4-day -5) "\n"
                      "** DOING rewrite the cache\n"
                      "** DONE ship the first report\n"
                      "** BACKLOG consider a vector store\n")))
    (with-temp-file my/stage4-learning
      (insert (concat (my/knowledge-todo-line) "\n\n* Tasks\n"
                      "** NEXT finish the agents chapter\n"
                      "** REVIEW redo the MCP summary\n"
                      "** DONE read the transport spec\n")))
    (with-temp-file my/stage4-idle-project
      (insert (concat (my/knowledge-todo-line) "\n\n* Tasks\n"
                      "** TODO something with no next action\n")))
    (ignore today)))

(defun my/stage4-cleanup ()
  "Delete fixtures and regenerate the dashboards."
  (dolist (f (list my/stage4-project my/stage4-learning my/stage4-idle-project))
    (when (get-file-buffer f)
      (with-current-buffer (get-file-buffer f)
        (set-buffer-modified-p nil)
        (kill-buffer)))
    (when (file-exists-p f) (delete-file f)))
  (my/refresh-all-dblocks))

(defun my/stage4-block-rows (file block)
  "Return the table rows of BLOCK in FILE, header first, as lists of strings."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward (format "^#\\+BEGIN: %s" (regexp-quote block)) nil t)
        (error "Block %s not found in %s" block file))
      (let ((end (save-excursion (re-search-forward "^#\\+END:" nil t) (point)))
            rows)
        (forward-line 1)
        (while (< (point) end)
          (let ((line (string-trim (thing-at-point 'line t))))
            (when (and (string-prefix-p "|" line)
                       (not (string-prefix-p "|-" line)))
              (push (mapcar #'string-trim (split-string line "|" t)) rows)))
          (forward-line 1))
        (nreverse rows)))))

(defun my/stage4-cell (rows column-name row-label)
  "Return the COLUMN-NAME cell of the row labelled ROW-LABEL in ROWS."
  (let* ((header (car rows))
         (index (cl-position column-name header :test #'equal)))
    (when index
      (cl-loop for row in (cdr rows)
               when (equal (car row) row-label) return (nth index row)))))

(defun my/stage4-block-text (file block)
  "Return the raw text of every BLOCK in FILE, concatenated.
A dashboard holds several blocks of the same type, so returning only the first
one would test the wrong section."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (let ((chunks nil))
        (while (re-search-forward (format "^#\\+BEGIN: %s" (regexp-quote block)) nil t)
          (let ((beg (point)))
            (re-search-forward "^#\\+END:" nil t)
            (push (buffer-substring-no-properties beg (match-beginning 0)) chunks)))
        (when (null chunks) (error "Block %s not found" block))
        (mapconcat #'identity (nreverse chunks) "\n")))))

;;;; 1. Fixtures render into the dashboard tables

(my/stage4-cleanup)
(my/stage4-write)
(my/refresh-all-dblocks)
(setq org-agenda-files (my/projects-agenda-files))

(let* ((projects (my/projects-subdir "dashboards/projects.org"))
       (rows (my/stage4-block-rows projects "project-health"))
       (row (cl-find-if (lambda (r) (equal (car r) "zz-stage4-project")) (cdr rows))))
  (my/stage4-check "project health has a row per project file"
                   (and row (cl-find-if (lambda (r) (equal (car r) "zz-stage4-idle")) (cdr rows))))
  (my/stage4-check "state counts match the fixture"
                   (equal (list (my/stage4-cell rows "BACKLOG" "zz-stage4-project")
                                (my/stage4-cell rows "TODO" "zz-stage4-project")
                                (my/stage4-cell rows "NEXT" "zz-stage4-project")
                                (my/stage4-cell rows "DOING" "zz-stage4-project")
                                (my/stage4-cell rows "DONE" "zz-stage4-project"))
                          '("1" "1" "1" "1" "1"))
                   (format "%S" row))
  (my/stage4-check "open items are counted without DONE"
                   (equal "4" (my/stage4-cell rows "Open" "zz-stage4-project"))
                   (format "%S" (my/stage4-cell rows "Open" "zz-stage4-project")))
  (my/stage4-check "the overdue deadline is counted"
                   (equal "1" (my/stage4-cell rows "Overdue" "zz-stage4-project")))
  (my/stage4-check "a project with a NEXT action says yes"
                   (equal "yes" (my/stage4-cell rows "Next?" "zz-stage4-project")))
  (my/stage4-check "a project without one says NO"
                   (equal "NO" (my/stage4-cell rows "Next?" "zz-stage4-idle")))
  (my/stage4-check "and it is named in the list below the table"
                   (string-match-p "zz-stage4-idle.*has no NEXT action"
                                   (my/stage4-block-text projects "project-health"))))

(let* ((learning (my/projects-subdir "dashboards/learning.org"))
       (rows (my/stage4-block-rows learning "learning-progress")))
  (my/stage4-check "learning progress counts open and done"
                   (equal (list (my/stage4-cell rows "Open" "zz-stage4-subject")
                                (my/stage4-cell rows "Done" "zz-stage4-subject"))
                          '("2" "1")))
  (my/stage4-check "and computes a percentage"
                   (equal "33%" (my/stage4-cell rows "Progress" "zz-stage4-subject"))
                   (format "%S" (my/stage4-cell rows "Progress" "zz-stage4-subject"))))

;;;; 2. The task lists are queries, not copies

(let ((home (my/projects-subdir "dashboards/home.org")))
  (my/stage4-check "the overdue list names the overdue task"
                   (string-match-p "write the scoring notes"
                                   (my/stage4-block-text home "my-tasks")))
  (my/stage4-check "the next-actions list names NEXT tasks from both trees"
                   (let ((text (my/stage4-block-text home "my-tasks")))
                     (and (string-match-p "wire the eval harness" text)
                          (string-match-p "finish the agents chapter" text))))
  (my/stage4-check "an empty section says so instead of inventing an item"
                   (string-match-p "Nothing here" (my/stage4-block-text home "my-tasks"))))

;; The per-tree sections scope by :files, not by a (path ...) regexp: a
;; path that no longer matches renders a tidy "Nothing here." forever, so
;; they are checked against fixtures rather than trusted.
(my/stage4-check "the projects dashboard lists its fixture task"
                 (string-match-p "wire the eval harness"
                                 (my/stage4-block-text (my/projects-subdir "dashboards/projects.org")
                                                       "my-tasks")))
(my/stage4-check "the learning dashboard lists its fixture task"
                 (let ((text (my/stage4-block-text (my/projects-subdir "dashboards/learning.org")
                                                   "my-tasks")))
                   (and (string-match-p "redo the MCP summary" text)
                        (string-match-p "finish the agents chapter" text))))

;;;; 3. Agenda dashboards

(defun my/stage4-agenda (key)
  "Build the agenda dashboard KEY and return its buffer text."
  (setq org-agenda-files (my/projects-agenda-files))
  (org-agenda nil key)
  (with-current-buffer "*Org Agenda*"
    (buffer-substring-no-properties (point-min) (point-max))))

(let ((text nil))
  (condition-case err (setq text (my/stage4-agenda "h"))
    (error (princ (format "  (home agenda error: %S)\n" err))))
  (my/stage4-check "home dashboard builds"
                   (and text (string-match-p "Next actions" text))
                   (format "%S" (and text (substring text 0 (min 120 (length text))))))
  (my/stage4-check "home dashboard shows the NEXT task"
                   (and text (string-match-p "wire the eval harness" text)))
  (my/stage4-check "home dashboard shows the deadline that is due"
                   (and text (string-match-p "write the scoring notes" text))))

(let ((text nil))
  (condition-case err (setq text (my/stage4-agenda "p"))
    (error (princ (format "  (projects agenda error: %S)\n" err))))
  (my/stage4-check "projects dashboard builds"
                   (and text (string-match-p "Project tasks" text)))
  (my/stage4-check "projects dashboard shows project tasks only"
                   (and text
                        (string-match-p "wire the eval harness" text)
                        (not (string-match-p "finish the agents chapter" text))))
  (my/stage4-check "projects dashboard is grouped by state"
                   (and text (string-match-p "Next" text)
                        (string-match-p "Doing" text))))

(let ((text nil))
  (condition-case err (setq text (my/stage4-agenda "l"))
    (error (princ (format "  (learning agenda error: %S)\n" err))))
  (my/stage4-check "learning dashboard builds"
                   (and text (string-match-p "Learning tasks" text)))
  (my/stage4-check "learning dashboard shows learning tasks only"
                   (and text
                        (string-match-p "finish the agents chapter" text)
                        (not (string-match-p "wire the eval harness" text)))))

;;;; 4. Cleanup

(my/stage4-cleanup)
(my/stage4-check "fixtures removed"
                 (and (not (file-exists-p my/stage4-project))
                      (not (file-exists-p my/stage4-learning))
                      (not (file-exists-p my/stage4-idle-project))))
(my/stage4-check "dashboards go back to placeholders"
                 (let ((rows (my/stage4-block-rows (my/projects-subdir "dashboards/projects.org")
                                                   "project-health")))
                   (equal "_no projects yet_" (caadr rows))))

(princ (format "\n%d checks, %d failures\n" my/stage4-checks my/stage4-failures))
(when (> my/stage4-failures 0)
  (kill-emacs 1))

;;; stage4-check.el ends here
