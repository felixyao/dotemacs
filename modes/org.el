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
      '((sequence "TODO(t)" "NEXT(n)" "DELEGATE(e@/!)" "SOMEDAY(s)" "SCHEDULED(h)" "|" "CANCELLED(c@/!)" "DONE(d@)")
	(sequence "PROJECT(p)" "SUBPROJECT(u)" "|")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("DELEGATE" :foreground "orange" :weight bold)
	("SOMEDAY" :oreground "light orange" :weight bold)
	("SCHEDULED" :foreground "magenta" :weight bold)
	("DONE" :foreground "dark grey" :weight bold)
	("CANCELLED" :foreground "dark grey" :weight bold)
	("PROJECT" :foreground "forest green" :weight bold)
	("SUBPROJECT" :foreground "forest green" :weight bold)))


(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
       '( ("PROJECT" ("PROJECT". t))
	  ("SUBPROJECT" ("PROJECT"))
	  ("TODO" ("PROJECT"))
	  ("NEXT" ("PROJECT"))
	  ("DELEGATE" ("PROJECT"))
	  ("SCHEDULED" ("PROJECT"))
	  ("DONE" ("PROJECT"))
	  ("CANCELLED" ("PROJECT"))
	  ("SOMEDAY" ("PROJECT"))))
	

;; Capture templates for: TODO tasks
(setq org-capture-templates
      '(("t" "todo" entry (file+olp "~/Dropbox/org/todo.org" "Tasks")
	 "* TODO %?\n" :clock-in t :clock-resume t)
	("w" "work todo" entry (file+olp "~/Dropbox/org/work.org" "Tasks")
	 "* TODO %?\n" :clock-in t :clock-resume t)
	("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
	 "* %?\n%U\n" :clock-in t :clock-resume t)
	("m" "Meeting" entry (file+olp "~/Dropbox/org/work.org" "Meetings")
	 "*  Meeting:%? \n" :clock-in t :clock-keep t)
	("b" "Buy something" entry (file+olp "~/Dropbox/org/todo.org" "Shopping")
	 "* SOMEDAY Buy: %? %^g \n" :prepend)
	("i" "Interruptions" entry (file+olp "~/Dropbox/org/work.org" "Interruptions")
	 "* TODO Interrupt by:%? for:  \n%U" :clock-in t :clock-keep t)))
	

(setq org-log-done 'time)

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
(setq org-refile-targets '((nil :level . 1)))
			   

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


(setq org-agenda-tags-todo-honor-ignore-options t)

(setq org-stuck-projects '("PROJECT/+PROJECT" ("NEXT" "SCHEDULED")))
                                    
;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(
	("r" "Review"
	 ((agenda "" 
               ((org-agenda-start-on-weekday 0)
		(org-agenda-ndays 7)
		(org-agenda-entry-types '(:closed))))
	  
          (tags-todo  "DEADLINE<\"<today>\"" 
	       ((org-agenda-overriding-header "Delayed tasks")
		(org-agenda-sorting-strategy
		       '(category-keep))))
	  
	 (todo "DELEGATE|SOMEDAY" 
	       ((org-agenda-overriding-header "Delegated tasks and some ideas")
		(org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
	 	(org-agenda-sorting-strategy
		       '(category-keep))))
	 (stuck ""
             ((org-agenda-todo-list-sublevels nil)))

         (todo "TODO|PROJECT|SUBPROJECT|SOMEDAY"
             ((org-agenda-overriding-header "Wrongly scheduled tasks")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
	      (org-agenda-sorting-strategy
		       '(category-keep))))
	 
	 (tags-todo "/SCHEDULED"
	       ((org-agenda-overriding-header "Scheduled no date tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		(org-agenda-sorting-strategy
		 '(category-keep))))))

	(" " "Agenda"
	 (
          (agenda "" ((org-agenda-ndays 1)
		      (org-agenda-entry-types '(:scheduled :deadline))
		      (org-agenda-repeating-timestamp-show-all nil)
		      (org-deadline-warning-days 0)
		      (org-agenda-show-all-dates nil)))

	  (tags-todo "-PROJECT/TODO"
		((org-agenda-overriding-header "Task planning")
		 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		 (org-agenda-sorting-strategy
		       '(priority-down category-down))))
	  (tags-todo "/NEXT"
		((org-agenda-overriding-header "Next Actions")
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
		 (org-agenda-sorting-strategy
		       '(priority-down category-down))))

	  (tags "-PROJECT+CLOSED<\"<-14d>\""
		((org-agenda-overriding-header "Tasks to Archive")
		 (org-tags-match-list-sublevels nil)))
	  )
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
     ((if  (member (org-get-todo-state) (list "TODO"))
	  "NEXT")))))

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
        :gap-ok-around "4:00"))

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

(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
	(ps-landscape-mode t)
	(org-agenda-add-entry-text-maxlines 5)
	(htmlize-output-type 'css)))
