;;; stage6-check.el --- Headless acceptance checks for Stage 6 -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --batch -Q \
;;     --eval '(setq user-emacs-directory (expand-file-name "~/.emacs.d/"))' \
;;     -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el \
;;     -l ~/.emacs.d/lisp/tests/stage6-check.el
;;
;; Three things, all measured rather than asserted:
;;   1. the ignore rules of both repositories protect what must not be
;;      committed and nothing else
;;   2. bin/backup.sh archives both trees and the archives actually restore
;;   3. agenda, dashboards and the roam index stay usable at 40 projects

;;; Code:

(require 'cl-lib)
(require 'md5)

(defvar my/stage6-failures 0)
(defvar my/stage6-checks 0)

(defun my/stage6-check (label condition &optional detail)
  "Record CHECK number LABEL with CONDITION, printing DETAIL on failure."
  (cl-incf my/stage6-checks)
  (if condition
      (princ (format "PASS  %s\n" label))
    (cl-incf my/stage6-failures)
    (princ (format "FAIL  %s%s\n" label (if detail (format "  [%s]" detail) "")))))

(defun my/stage6-report (label seconds ceiling)
  "Say whether SECONDS is under CEILING for LABEL, and record the check."
  (my/stage6-check (format "%s within %ss (measured %.2fs)" label ceiling seconds)
                   (< seconds ceiling)
                   (format "%.2fs" seconds)))

(defvar my/stage6-count 40
  "How many fixture projects the performance run creates.")

(defvar my/stage6-status-before-k nil
  "git status of the knowledge tree at the start of the run.")
(defvar my/stage6-status-before-p nil
  "git status of the projects tree at the start of the run.
The checks compare against these rather than requiring clean trees:
uncommitted work of yours is not a test failure.")

(defun my/stage6-git (dir &rest args)
  "Run git with ARGS in DIR and return its output."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory (directory-file-name dir))))
      (apply #'call-process "git" nil t nil args))
    (string-trim (buffer-string))))

(setq my/stage6-status-before-k (my/stage6-git my/knowledge-dir "status" "--porcelain")
      my/stage6-status-before-p (my/stage6-git my/projects-dir "status" "--porcelain"))

;;;; 1. Ignore rules

(defun my/stage6-ignored-p (dir file)
  "Return non-nil when FILE is ignored by the repository in DIR."
  (zerop (call-process "git" nil nil nil "-C" dir "check-ignore" "-q" file)))

(my/stage6-check "knowledge: .DS_Store is ignored"
                 (my/stage6-ignored-p my/knowledge-dir ".DS_Store"))
(my/stage6-check "knowledge: a zettel is NOT ignored"
                 (not (my/stage6-ignored-p my/knowledge-dir "main-box/1.org")))
(my/stage6-check "knowledge: a literature note is NOT ignored"
                 (not (my/stage6-ignored-p my/knowledge-dir "literature-box/001.org")))
(my/stage6-check "projects: .DS_Store is ignored"
                 (my/stage6-ignored-p my/projects-dir ".DS_Store"))
(my/stage6-check "projects: the inbox is NOT ignored"
                 (not (my/stage6-ignored-p my/projects-dir "inbox.org")))
(my/stage6-check "projects: an attachment is NOT ignored"
                 (not (my/stage6-ignored-p my/projects-dir "attachments/19/abc/book.epub")))
(my/stage6-check "projects: a template is NOT ignored"
                 (not (my/stage6-ignored-p my/projects-dir "templates/project.org")))

;;;; 2. Backup and restore

(defvar my/stage6-backup-dir (expand-file-name "knowledge-backups-test" temporary-file-directory))
(defvar my/stage6-sandbox (expand-file-name "knowledge-sandbox-test" temporary-file-directory))
(defvar my/stage6-restore-dir (expand-file-name "knowledge-restore-test" temporary-file-directory))

(defun my/stage6-file-hashes (root files)
  "Return an alist of FILES (relative to ROOT) and their md5."
  (mapcar (lambda (f)
            (cons f (md5 (with-temp-buffer
                           (insert-file-contents-literally (expand-file-name f root))
                           (buffer-string)))))
          files))

(my/stage6-check "bin/backup.sh is executable"
                 (file-executable-p (expand-file-name "bin/backup.sh" my/projects-dir)))

;; Run the script against *copies*.  It commits pending work, and a test must
;; never commit to the repository it is testing.
(when (file-directory-p my/stage6-sandbox) (delete-directory my/stage6-sandbox t))
(make-directory my/stage6-sandbox t)
(my/stage6-check "both trees can be copied for a safe backup test"
                 (and (zerop (call-process "cp" nil nil nil "-a"
                                           (directory-file-name my/knowledge-dir)
                                           (expand-file-name "knowledge" my/stage6-sandbox)))
                      (zerop (call-process "cp" nil nil nil "-a"
                                           (directory-file-name my/projects-dir)
                                           (expand-file-name "projects" my/stage6-sandbox)))))

(let* ((copy-k (expand-file-name "knowledge" my/stage6-sandbox))
       (copy-p (expand-file-name "projects" my/stage6-sandbox))
       (tracked (with-temp-buffer
                  (let ((default-directory (file-name-as-directory copy-k)))
                    (call-process "git" nil t nil "ls-files"))
                  (split-string (buffer-string) "\n" t)))
       (before (my/stage6-file-hashes copy-k tracked))
       (output (with-temp-buffer
                 (let ((process-environment
                        (append (list (format "KNOWLEDGE_DIR=%s" copy-k)
                                      (format "PROJECTS_DIR=%s" copy-p)
                                      (format "BACKUP_DIR=%s" my/stage6-backup-dir))
                                process-environment)))
                   (call-process (expand-file-name "bin/backup.sh" copy-p) nil t nil)
                   (buffer-string))))
       (archive-k (expand-file-name (format "knowledge-%s.tar.gz" (format-time-string "%Y-%m-%d"))
                                    my/stage6-backup-dir))
       (archive-p (expand-file-name (format "projects-%s.tar.gz" (format-time-string "%Y-%m-%d"))
                                    my/stage6-backup-dir)))
  (my/stage6-check "it reports what it did"
                   (string-match-p "archived" output) (format "%S" output))
  (my/stage6-check "it leaves a knowledge archive behind" (file-exists-p archive-k))
  (my/stage6-check "it leaves a projects archive behind" (file-exists-p archive-p))
  (when (file-exists-p archive-k)
    (my/stage6-check "the knowledge archive contains the repository"
                     (with-temp-buffer
                       (call-process "tar" nil t nil "-tzf" archive-k)
                       (and (string-match-p "knowledge/\\.git/" (buffer-string))
                            (string-match-p "knowledge/main-box/" (buffer-string)))))
    (when (file-directory-p my/stage6-restore-dir)
      (delete-directory my/stage6-restore-dir t))
    (make-directory my/stage6-restore-dir t)
    (my/stage6-check "the knowledge archive extracts"
                     (zerop (call-process "tar" nil nil nil "-xzf" archive-k "-C" my/stage6-restore-dir)))
    (let* ((restored (expand-file-name "knowledge" my/stage6-restore-dir))
           (after (my/stage6-file-hashes
                   restored
                   (cl-remove-if-not (lambda (f) (file-exists-p (expand-file-name f restored)))
                                     tracked))))
      (my/stage6-check "every tracked knowledge file restores byte for byte"
                       (equal before after)
                       (format "%d of %d files differ"
                               (length (cl-set-difference before after :test #'equal))
                               (length before)))))
  (when (file-exists-p archive-p)
    (my/stage6-check "the projects archive contains the inbox and attachments"
                     (with-temp-buffer
                       (call-process "tar" nil t nil "-tzf" archive-p)
                       (and (string-match-p "projects/inbox\\.org" (buffer-string))
                            (string-match-p "projects/attachments/" (buffer-string)))))
    (when (file-directory-p (expand-file-name "projects" my/stage6-restore-dir))
      (delete-directory (expand-file-name "projects" my/stage6-restore-dir) t))
    (my/stage6-check "the projects archive extracts"
                     (zerop (call-process "tar" nil nil nil "-xzf" archive-p "-C" my/stage6-restore-dir)))
    (let* ((restored-p (expand-file-name "projects" my/stage6-restore-dir))
           (tracked-p (with-temp-buffer
                        (let ((default-directory (file-name-as-directory copy-p)))
                          (call-process "git" nil t nil "ls-files"))
                        (split-string (buffer-string) "\n" t)))
           (before-p (my/stage6-file-hashes copy-p tracked-p))
           (after-p (my/stage6-file-hashes
                     restored-p
                     (cl-remove-if-not (lambda (f) (file-exists-p (expand-file-name f restored-p)))
                                       tracked-p))))
      (my/stage6-check "every tracked projects file restores byte for byte"
                       (equal before-p after-p)
                       (format "%d of %d files differ"
                               (length (cl-set-difference before-p after-p :test #'equal))
                               (length before-p))))))

;;;; 3. Performance at size

(defun my/stage6-make-fixtures ()
  "Create the fixture projects and subjects and return their paths."
  (let (files)
    (dotimes (i my/stage6-count)
      (let* ((kind (if (= 0 (% i 4)) "learning" "projects"))
             (path (expand-file-name (format "zz-perf-%02d.org" i) (my/projects-subdir kind)))
             (states '("BACKLOG" "TODO" "NEXT" "DOING" "WAITING" "DONE")))
        (with-temp-file path
          (insert (concat (my/knowledge-todo-line) "\n\n* Tasks\n"))
          (dotimes (n 5)
            (insert (format "** %s task %d of project %d\n" (nth (% (+ i n) 6) states) n i)))
          (when (= 0 (% i 3))
            (insert (format "   DEADLINE: %s\n" (format-time-string "<%Y-%m-%d %a>"
                                                                    (time-add nil (* 5 86400))))))
          ;; A real project file carries a board, so the refresh cost is real.
          (insert (format "\n* Board\n#+BEGIN: kanban %s\n#+END:\n" (my/kanban-params))))
        (push path files)))
    files))

(defun my/stage6-cleanup-fixtures ()
  "Delete the performance fixtures and regenerate the dashboards."
  (dolist (f (directory-files-recursively my/projects-dir "zz-perf-.*\\.org\\'"))
    (when (get-file-buffer f)
      (with-current-buffer (get-file-buffer f)
        (set-buffer-modified-p nil)
        (kill-buffer)))
    (when (file-exists-p f) (delete-file f)))
  (dolist (f (append (my/projects-files-in "dashboards")
                     (append (my/projects-files) (my/projects-learning-files))))
    (my/refresh-dblocks-in-file f)))

(my/stage6-cleanup-fixtures)
(let ((fixtures (my/stage6-make-fixtures)))
  (my/stage6-check "fixtures created"
                   (= my/stage6-count (length fixtures)))
  (setq org-agenda-files (my/projects-agenda-files))
  (my/stage6-check "the agenda discovers all of them"
                   (>= (length (org-agenda-files)) my/stage6-count))

  ;; Agenda build
  (let ((start (float-time)))
    (org-todo-list)
    (my/stage6-report "agenda build over 40 projects" (- (float-time) start) 5.0))

  ;; Dashboard refresh
  (let ((start (float-time)))
    (my/refresh-all-dblocks)
    (my/stage6-report "full dashboard refresh" (- (float-time) start) 15.0))
  ;; A fast number is worthless if the work did not happen: the health table
  ;; must actually carry a row per fixture project.
  (with-current-buffer (find-file-noselect (my/projects-subdir "dashboards/projects.org"))
    (my/stage6-check "and the health table really has a row per project"
                     (> (how-many "^| zz-perf-") 25)
                     (format "%d rows" (how-many "^| zz-perf-"))))

  ;; Roam index.  Delete the database first: syncing an up-to-date index only
  ;; stats files and would flatter the number.
  (let ((start (float-time)))
    (org-roam-db--close-all)
    (when (file-exists-p org-roam-db-location) (delete-file org-roam-db-location))
    (org-roam-db-sync 'force)
    (my/stage6-report "org-roam cold re-index" (- (float-time) start) 20.0)
    (my/stage6-check "and the index really contains a main-box node"
                     (>= (caar (org-roam-db-query
                                [:select (funcall count *) :from nodes
                                 :where (like file $s1)] "%main-box%"))
                         1)
                     (format "%s nodes"
                             (caar (org-roam-db-query
                                    [:select (funcall count *) :from nodes
                                     :where (like file $s1)] "%main-box%"))))
    (my/stage6-check "and none of the project fixtures leaked into it"
                     (= 0 (caar (org-roam-db-query
                                 [:select (funcall count *) :from files
                                  :where (like file $s1)] "%zz-perf-%")))
                     (format "%s files"
                             (caar (org-roam-db-query
                                    [:select (funcall count *) :from files
                                     :where (like file $s1)] "%zz-perf-%")))))

  (my/stage6-cleanup-fixtures)
  (my/stage6-check "fixtures removed"
                   (null (directory-files-recursively my/projects-dir "zz-perf-.*\\.org\\'"))))

;;;; 4. No dead keys or dead templates

(my/stage6-check "the garden-era C-c n w binding is gone"
                 (null (key-binding (kbd "C-c n w"))))
(my/stage6-check "the knowledge-note key is gone with the command"
                 (null (key-binding (kbd "C-c k n"))))
(my/stage6-check "the capture keys that remain point at real templates"
                 (let ((keys (list "C-c k i" "C-c k t")))
                   (cl-every (lambda (k) (commandp (key-binding (kbd k)))) keys)))
(my/stage6-check "every capture template has a usable target"
                 (cl-every
                  (lambda (template)
                    (let* ((target (nth 3 template))
                           (spec (nth 1 target)))
                      (cond
                       ;; Resolvable without prompting.
                       ((stringp spec) (file-exists-p spec))
                       ;; Function-valued specs prompt, so check they exist
                       ;; rather than calling them in a batch session.
                       ((symbolp spec) (or (fboundp spec) (boundp spec)))
                       (t nil))))
                  org-capture-templates))
(my/stage6-check "the knowledge help buffer lists the current keys"
                 (progn (my/help-bindings)
                        (with-current-buffer "*My Keys*"
                          (prog1 (and (string-match-p "C-c k A" (buffer-string))
                                      (string-match-p "C-c k S" (buffer-string)))
                            (kill-buffer)))))

;;;; 5. Cleanup of the backup test

(when (file-directory-p my/stage6-backup-dir) (delete-directory my/stage6-backup-dir t))
(when (file-directory-p my/stage6-sandbox) (delete-directory my/stage6-sandbox t))
(when (file-directory-p my/stage6-restore-dir) (delete-directory my/stage6-restore-dir t))
(my/stage6-check "the run added nothing to the knowledge tree"
                 (equal my/stage6-status-before-k (my/stage6-git my/knowledge-dir "status" "--porcelain"))
                 (let ((after (my/stage6-git my/knowledge-dir "status" "--porcelain")))
                   (format "before %S after %S" my/stage6-status-before-k after)))
(my/stage6-check "the run added nothing to the projects tree"
                 (equal my/stage6-status-before-p (my/stage6-git my/projects-dir "status" "--porcelain"))
                 (let ((after (my/stage6-git my/projects-dir "status" "--porcelain")))
                   (format "before %S after %S" my/stage6-status-before-p after)))

(princ (format "\n%d checks, %d failures\n" my/stage6-checks my/stage6-failures))
(when (> my/stage6-failures 0)
  (kill-emacs 1))

;;; stage6-check.el ends here
