;; Org mode configurations
(require 'org-habit)

;The following customization sets a default target file for notes, and
;defines a global for capturing new material.
(setq org-directory "~/Dropbox/org")

(unless (file-exists-p org-directory)
 (make-directory org-directory))

(global-set-key "\C-cc" 'org-capture)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d@)")
	(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("HOLD" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("MEETING" :foreground "forest green" :weight bold)
	("PHONE" :foreground "forest green" :weight bold)))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)


(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("HOLD" ("WAITING") ("HOLD" . t))
	(done ("WAITING") ("HOLD"))
	("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))


;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings
(setq org-capture-templates
      '(("t" "todo" entry (file "refile.org")
	 "* TODO %U\n%a\n" :clock-in t :clock-resume t)
	("r" "respond" entry (file "refile.org")
	 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
	("n" "note" entry (file "refile.org")
	 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
	("j" "Journal" entry (file+datetree "diary.org")
	 "* %?\n%U\n" :clock-in t :clock-resume t)
	("m" "Meeting" entry (file "refile.org")
	 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
	("p" "Phone call" entry (file "refile.org")
	 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
	("h" "Habit" entry (file "refile.org")
	 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (unless (or (equal major-mode 'org-mode)
	      (equal major-mode 'org-agenda-mode))
    (save-excursion
      (beginning-of-line 0)
      (org-remove-empty-drawer-at (point)))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

;Set org agenda files
(setq org-agenda-files (list (symbol-value 'org-directory)))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-root-project-p ()
  "Any project is not in a project subtree"
  (save-excursion
    (widen)
    (and (bh/is-project-p) (not (bh/is-project-subtree-p)))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/skip-projects-tasks-and-habits ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((bh/is-project-subtree-p)
	next-headline)
       ((not (bh/is-task-p))
        next-headline)
       (t
        nil)))))

(defun bh/skip-habits-and-projects ()
  "Skip projects and habits"
  (save-excursion
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
	next-headline)
       ((bh/is-project-p)
	next-headline)
       ((not (bh/is-task-p))
	next-headline)
       (t
	nil)))))


(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(defun bh/make-week-date-string ()
  (let* ((today (string-to-int (format-time-string "%w" (current-time))))
	 (date-list (number-sequence 0 today))
         (date-string (mapconcat (function (lambda (x)
			   (let ((date (time-subtract (current-time) (seconds-to-time (* 60 60 24 x)))))
			     (format-time-string "%Y-%m-%d %a \\|" date))))
	       date-list
	       "")))
    (substring date-string 0 -3)))

(defun bh/skip-non-finished-this-week-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let ((date-string (bh/make-week-date-string))
                    (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward
						  date-string subtree-end t)))))
                (if subtree-is-current
                    nil ; Task I have done this week
                  subtree-end))
            (or subtree-end (point-max)))
        next-headline))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
	  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
		 (has-next ))
	    (save-excursion
	      (forward-line 1)
	      (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
		(unless (member "WAITING" (org-get-tags-at))
		  (setq has-next t))))
	    (if has-next
		next-headline
	      nil)) ; a stuck project, has subtasks but no next task
	next-headline))))

(setq org-agenda-tags-todo-honor-ignore-options t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	("h" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	("r" "Review"
	 ((tags "-MEETING-PHONE"
                ((org-agenda-overriding-header "Done by this week")
		 (org-agenda-skip-function 'bh/skip-non-finished-this-week-tasks)))
	 
	 (tags-todo "WAITING|HOLD-CANCELLED/!"
	       ((org-agenda-overriding-header "Pending and Postponed tasks")
		(org-agenda-skip-function 'bh/skip-habits-and-projects)
		(org-agenda-sorting-strategy
		       '(category-keep))))
	 (tags-todo "-CANCELLED/!" 
	       ((org-agenda-overriding-header "Pending Projects")
		(org-agenda-skip-function 'bh/skip-non-stuck-projects)
		(org-agenda-sorting-strategy
		       '(category-keep))))

	  ))
	(" " "Agenda"
	 ((agenda "" ((org-agenda-ndays 1)))
	  (tags "REFILE"
		((org-agenda-overriding-header "Tasks to Refile")
		 (org-tags-match-list-sublevels nil)))
	  (tags-todo "-CANCELLED-HOLD-WAITING/!NEXT"
		     ((org-agenda-overriding-header "Next Actions")
		      (org-agenda-skip-function 'bh/skip-habits-and-projects)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-HOLD-CANCELLED-WAITING/!TODO"
		     ((org-agenda-overriding-header "TODO lists")
		      (org-agenda-skip-function 'bh/skip-projects-tasks-and-habits)
		      (org-agenda-todo-ignore-scheduled t)
		      (org-agenda-todo-ignore-deadlines t)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags "-REFILE/"
		     ((org-agenda-overriding-header "Tasks to Archive")
	              (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
	              (org-tags-match-list-sublevels nil))))
	  nil)))

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (and (cond
        ((string= tag "hold")
         t)
        ((string= tag "farm")
         t))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

;;To save the clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("STATELOG" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))
(global-set-key "\C-zi" 'bh/punch-in)

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))
(global-set-key "\C-zo" 'bh/punch-out)


(defun bh/clock-in-default-task ()
  (save-excursion
    (unless (marker-buffer org-clock-default-task)
     (bh/clock-in-organization-task-as-default))
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(setq org-agenda-clock-consistency-checks
      '(:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00")))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
			      ("STYLE_ALL" . "habit")))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items '(closed state))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")


