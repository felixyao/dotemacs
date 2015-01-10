;; Org mode configurations
(require 'org-habit)


;;To save the clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


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
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
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

(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("WAITING" . t))
	("HOLD" ("WAITING") ("HOLD" . t))
	(done ("WAITING") ("HOLD"))
	("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
	("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
	("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

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

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
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
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

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

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("N" "Notes" tags "NOTE"
	 ((org-agenda-overriding-header "Notes")
	  (org-tags-match-list-sublevels t)))
	("h" "Habits" tags-todo "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
	(" " "Agenda"
	 ((agenda "" nil)
	  (tags "REFILE"
		((org-agenda-overriding-header "Tasks to Refile")
		 (org-tags-match-list-sublevels nil)))
	  (tags-todo "-CANCELLED/!"
		     ((org-agenda-overriding-header "Stuck Projects")
		      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-HOLD-CANCELLED/!"
		     ((org-agenda-overriding-header "Projects")
		      (org-agenda-skip-function 'bh/skip-non-projects)
		      (org-tags-match-list-sublevels 'indented)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-CANCELLED/!NEXT"
		     ((org-agenda-overriding-header (concat "Project Next Tasks"
							    (if bh/hide-scheduled-and-waiting-next-tasks
								""
							      " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
		      (org-tags-match-list-sublevels t)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(todo-state-down effort-up category-keep))))
	  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
		     ((org-agenda-overriding-header (concat "Project Subtasks"
							    (if bh/hide-scheduled-and-waiting-next-tasks
								""
							      " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-non-project-tasks)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
		     ((org-agenda-overriding-header (concat "Standalone Tasks"
							    (if bh/hide-scheduled-and-waiting-next-tasks
								""
							      " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-project-tasks)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-sorting-strategy
		       '(category-keep))))
	  (tags-todo "-CANCELLED+WAITING|HOLD/!"
		     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
							    (if bh/hide-scheduled-and-waiting-next-tasks
								""
							      " (including WAITING and SCHEDULED tasks)")))
		      (org-agenda-skip-function 'bh/skip-non-tasks)
		      (org-tags-match-list-sublevels nil)
		      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
		      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
	  (tags "-REFILE/"
		((org-agenda-overriding-header "Tasks to Archive")
		 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
		 (org-tags-match-list-sublevels nil))))
	 nil)))

