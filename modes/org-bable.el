(setq org-ditaa-jar-path "~/Dropbox/utilities/ditaa.jar")
(setq org-plantuml-jar-path "~/Dropbox/utilities/plantuml.jar")

(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (dot . t)
   (emacs-lisp . t)))

