;;; my-dashboards.el --- Dashboards: generated tables and agenda views -*- lexical-binding: t; -*-

;;; Commentary:
;; Two halves, and the split is deliberate:
;;
;;   1. Generated dynamic blocks inside the dashboard files.  These are tables
;;      and lists *derived* from the project and learning files at refresh time,
;;      so the dashboard cannot drift from the tasks.
;;   2. Agenda commands built from Org's own command types (agenda, todo,
;;      alltodo), grouped with org-super-agenda.
;;
;; org-ql does the querying for the blocks.  It cannot be used as an
;; `org-agenda-custom-commands' block: Org 31's `org-agenda-run-series' accepts
;; only agenda/agenda* /alltodo/stuck/search/tags/tags-todo/todo and rejects
;; anything else with "Invalid type in command series".  That was verified by
;; running it, not assumed.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'my-org-pkm)

;; org-ql and org-super-agenda are large and only needed when a dashboard is
;; actually built.  They are loaded on first use, not at startup: this module
;; costs a few milliseconds, the dashboards cost a second when you open them.
(defun my/dashboard--ensure-ql ()
  "Load org-ql."
  (require 'org-ql))

(defun my/dashboard--ensure-super-agenda ()
  "Load org-super-agenda and switch it on."
  (require 'org-super-agenda)
  (unless org-super-agenda-mode (org-super-agenda-mode 1)))

;;;; Rendering entries

(defvar my/dashboard-buffer-name "*My Dashboard*"
  "Buffer used when a dashboard is opened on its own.")

(defun my/dashboard--query (query)
  "Return markers for QUERY over the knowledge agenda files."
  (my/dashboard--ensure-ql)
  (org-ql-select (org-agenda-files) query :action '(point-marker)))

(defun my/dashboard--state-counts (file)
  "Return an alist of (STATE . COUNT) for the open and done items in FILE."
  (let ((counts nil))
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (let* ((state (or (org-get-todo-state) ""))
                (cell (assoc state counts)))
           (if cell (cl-incf (cdr cell)) (push (cons state 1) counts))))))
    counts))

(defun my/dashboard-file-stats (file)
  "Return a plist describing the actionable state of FILE.
Keys: :name, :counts (alist), :open, :done, :next, :overdue."
  (let* ((counts (my/dashboard--state-counts file))
         (state-count (lambda (s) (or (cdr (assoc s counts)) 0)))
         (open (cl-loop for (state . n) in counts
                        unless (member state '("DONE" "CANCELLED" ""))
                        sum n))
         (done (funcall state-count "DONE"))
         (next (funcall state-count "NEXT"))
         (overdue 0))
    (with-current-buffer (find-file-noselect file)
      (org-map-entries
       (lambda ()
         (unless (member (org-get-todo-state) '("DONE" "CANCELLED"))
           (let ((raw (org-entry-get nil "DEADLINE")))
             (when (and raw
                        (< (org-time-string-to-absolute raw) (org-today)))
               (cl-incf overdue)))))))
    (list :name (file-name-base file)
          :file file
          :counts counts
          :open open
          :done done
          :next next
          :overdue overdue)))

(defvar my/dashboard-state-columns
  '("BACKLOG" "TODO" "NEXT" "DOING" "WAITING" "REVIEW" "DONE")
  "Columns of the project health table, in workflow order.")

;;;; Dynamic blocks

(defun my/dashboard--relative-link (file heading)
  "Return an Org link to HEADING in FILE, relative to the current buffer."
  (let ((base (file-name-directory (or (buffer-file-name) default-directory))))
    (org-link-make-string (format "file:%s::*%s" (file-relative-name file base) heading)
                          heading)))

(defun my/dashboard--table (header rows)
  "Insert an Org table with HEADER and ROWS, then align it.
The separator is generated from the header length so a column cannot fall out
of alignment when the shape changes."
  (insert "| " (mapconcat #'identity header " | ") " |\n")
  (insert "|-" (mapconcat (lambda (_) "") header "-+-") "-|\n")
  (dolist (row rows)
    (insert "| " (mapconcat #'identity row " | ") " |\n"))
  (org-table-align))

(defun my/dashboard--query-lines (query base-dir)
  "Return formatted lines for QUERY, with links relative to BASE-DIR.
`org-ql-select' visits the files it searches, so nothing here may assume it is
still running in the dashboard buffer."
  (let (lines)
    (dolist (marker (my/dashboard--query query))
      (org-with-point-at marker
        (let ((heading (org-get-heading t t t t))
              (todo (or (org-get-todo-state) ""))
              (deadline (org-entry-get nil "DEADLINE"))
              (scheduled (org-entry-get nil "SCHEDULED")))
          (push (concat (format "- %-9s %s" todo
                                (org-link-make-string
                                 (format "file:%s::*%s"
                                         (file-relative-name (buffer-file-name) base-dir)
                                         heading)
                                 heading))
                        (if scheduled (format "  sched %s" scheduled) "")
                        (if deadline (format "  due %s" deadline) ""))
                lines))))
    (nreverse lines)))

(defun org-dblock-write:my-tasks (params)
  "Write the `my-tasks' block: a list of QUERY results.
PARAMS: :query (an org-ql sexp), :empty (text when nothing matches).
No heading is inserted: the surrounding Org heading already names the list,
and two labels for one list is how dashboards start to drift."
  (let* ((target (current-buffer))
         (base (file-name-directory (or (buffer-file-name) default-directory)))
         (query (plist-get params :query))
         (lines (when query (my/dashboard--query-lines query base)))
         (text (if lines
                   (concat (mapconcat #'identity lines "\n") "\n\n")
                 (concat (or (plist-get params :empty) "Nothing here.") "\n\n"))))
    (with-current-buffer target (insert text))))

(defun my/dashboard--health-rows ()
  "Return (ROWS . MISSING) for the project health table."
  (let (rows missing)
    (dolist (file (my/knowledge-project-files))
      (let* ((stats (my/dashboard-file-stats file))
             (count (lambda (s) (or (cdr (assoc s (plist-get stats :counts))) 0))))
        (push (append (list (plist-get stats :name))
                      (mapcar (lambda (s) (number-to-string (funcall count s)))
                              my/dashboard-state-columns)
                      (list (number-to-string (plist-get stats :open))
                            (number-to-string (plist-get stats :overdue))
                            (if (> (plist-get stats :next) 0) "yes" "NO")))
              rows)
        (unless (> (plist-get stats :next) 0)
          (push (cons (plist-get stats :name) file) missing))))
    (cons (nreverse rows) (nreverse missing))))

(defun org-dblock-write:project-health (_params)
  "Write the `project-health' block: one table row per project file."
  (let* ((target (current-buffer))
         (base (file-name-directory (or (buffer-file-name) default-directory)))
         (header (append '("File") my/dashboard-state-columns
                         '("Open" "Overdue" "Next?")))
         (data (my/dashboard--health-rows))
         (rows (car data))
         (missing (cdr data)))
    (with-current-buffer target
      (if (null rows)
          (my/dashboard--table header (list (append '("_no projects yet_")
                                                   (make-list 10 ""))))
        (my/dashboard--table header rows))
      (insert "\n")
      (when missing
        (insert (mapconcat (lambda (pair)
                             (format "- [[file:%s][%s]] has no NEXT action"
                                     (file-relative-name (cdr pair) base)
                                     (car pair)))
                           missing "\n"))
        (insert "\n\n")))))

(defun my/dashboard--progress-rows ()
  "Return the rows of the learning progress table."
  (let (rows)
    (dolist (file (my/knowledge-learning-files))
      (let* ((stats (my/dashboard-file-stats file))
             (open (plist-get stats :open))
             (done (plist-get stats :done))
             (total (+ open done)))
        (push (list (plist-get stats :name)
                    (number-to-string open)
                    (number-to-string done)
                    (if (> total 0)
                        (format "%d%%" (round (* 100.0 (/ (float done) total))))
                      "-")
                    (if (> (plist-get stats :next) 0) "yes" "NO"))
              rows)))
    (nreverse rows)))

(defun org-dblock-write:learning-progress (_params)
  "Write the `learning-progress' block: one table row per learning subject."
  (let* ((target (current-buffer))
         (header '("Subject" "Open" "Done" "Progress" "Next?"))
         (rows (my/dashboard--progress-rows)))
    (with-current-buffer target
      (if rows
          (my/dashboard--table header rows)
        (my/dashboard--table header (list '("_no subjects yet_" "" "" "" ""))))
      (insert "\n"))))

;;;; Agenda views

(defvar my/dashboard-home-groups
  '((:name "Overdue" :deadline past :scheduled past)
    (:name "Today" :deadline today :scheduled today :time-grid t)
    (:name "Soon" :deadline future))
  "Grouping for the home dashboard.
No :discard group on purpose: `org-super-agenda' groups every block in the
series, so discarding anything un-dated would silently empty the Next actions
and Waiting blocks.")

(defvar my/dashboard-work-groups
  `((:name "Overdue" :deadline past)
    (:name "Doing" :todo "DOING")
    (:name "Next" :todo "NEXT")
    (:name "Waiting" :todo "WAITING")
    (:name "Review" :todo "REVIEW")
    (:name "Backlog and todo" :todo ("BACKLOG" "TODO")))
  "Shared grouping for the project and learning dashboards.")

(defun my/dashboard-commands ()
  "Return the dashboard entries to prepend to `org-agenda-custom-commands'."
  `(("h" "Home dashboard"
     ((agenda "" ((org-agenda-span 'day)
                  (org-agenda-overriding-header "Today")))
      (todo "NEXT" ((org-agenda-overriding-header "Next actions")))
      (todo "DOING" ((org-agenda-overriding-header "In progress")))
      (todo "WAITING" ((org-agenda-overriding-header "Waiting on others")))
      (todo "REVIEW" ((org-agenda-overriding-header "To review"))))
     ((org-super-agenda-groups my/dashboard-home-groups)))
    ("p" "Projects dashboard"
     ((alltodo "" ((org-agenda-overriding-header "Project tasks")))
      (todo "WAITING" ((org-agenda-overriding-header "Waiting")))
      (todo "NEXT" ((org-agenda-overriding-header "Next actions"))))
     ((org-agenda-files (my/knowledge-project-files))
      (org-super-agenda-groups my/dashboard-work-groups)))
    ("l" "Learning dashboard"
     ((alltodo "" ((org-agenda-overriding-header "Learning tasks")))
      (todo "REVIEW" ((org-agenda-overriding-header "Revision")))
      (todo "NEXT" ((org-agenda-overriding-header "Next actions"))))
     ((org-agenda-files (my/knowledge-learning-files))
      (org-super-agenda-groups my/dashboard-work-groups)))))

(setq org-agenda-custom-commands
      (append (my/dashboard-commands)
              ;; Drop any previous copy so reloading does not duplicate entries.
              (seq-remove (lambda (entry) (member (car entry) '("h" "p" "l")))
                          org-agenda-custom-commands)))

;;;; Opening things

(defun my/dashboard-open-home ()
  "Open the home dashboard file and refresh it."
  (interactive)
  (my/dashboard-open (my/knowledge-subdir "dashboards/home.org")))

(defun my/dashboard-open (file)
  "Open dashboard FILE, refresh its blocks and leave point at the top."
  (find-file file)
  (my/refresh-dblocks-in-file file)
  (goto-char (point-min)))

(defun my/dashboard-agenda (key)
  "Open the agenda dashboard KEY, refreshing the agenda files first."
  (interactive)
  (my/dashboard--ensure-super-agenda)
  (setq org-agenda-files (my/knowledge-agenda-files))
  (org-agenda nil key))

(defun my/dashboard-home ()
  "Home dashboard: today, next actions, waiting and review."
  (interactive)
  (my/dashboard-agenda "h"))

(defun my/dashboard-projects ()
  "Projects dashboard: every open project task, grouped by state."
  (interactive)
  (my/dashboard-agenda "p"))

(defun my/dashboard-learning ()
  "Learning dashboard: every open learning task, grouped by state."
  (interactive)
  (my/dashboard-agenda "l"))

(global-set-key (kbd "C-c k D") #'my/dashboard-home)
(global-set-key (kbd "C-c k P") #'my/dashboard-projects)
(global-set-key (kbd "C-c k L") #'my/dashboard-learning)
(defun my/dashboard-search ()
  "Ask for an org-ql query and show the results."
  (interactive)
  (my/dashboard--ensure-ql)
  (call-interactively #'org-ql-search))

(global-set-key (kbd "C-c k q") #'my/dashboard-search)

(provide 'my-dashboards)
;;; my-dashboards.el ends here
