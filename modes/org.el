;; Org mode configurations

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

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
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
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)


; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


