;; Org mode configurations

;;To save the clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;The following customization sets a default target file for notes, and
;defines a global for capturing new material.
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)



     (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/inbox.org")
             "* %?\nEntered on %U\n  %i\n  %a")))


