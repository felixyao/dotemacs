(require 'ede)

(projectile-global-mode)

(setq projectile-indexing-method 'alien)

(setq projectile-enable-cathing t)

(setq projectile-switch-project-action 'projectile-dired)

(setq projectile-globally-ignored-file-suffixes '("o" "cache" "a" "dll" "exe" "rar" "doc"))

(setq projectile-project-root-files (append '(".projectile") projectile-project-root-files))


;; Using .dir-locals.el to define project include files 
;; Enable EDE (Project Management) features
(global-ede-mode 1)

(setq enable-local-variables :all)


(defvar my-current-project nil
  "The ede project current file is in")

(make-variable-buffer-local 'my-current-project)

(defvar my-current-include  nil
  "This is used when a file is not belong to any projects")

(make-variable-buffer-local 'my-current-include)

(defun my-get-local-file-direcotry (dirs)
  (let ((cur (locate-dominating-file default-directory ".dir-locals.el")))
	(mapcar (lambda (item)
			  (concat cur item)
			  )
			dirs)))

(defun my-parser-local-variable-project ()
  (cond ((boundp 'my-project)  ;this file is belong to some project
		 (let ((project (ede-directory-get-toplevel-open-project default-directory)))
		   (if  project
			   (setq my-current-project project)
			 (let* ((project-root (projectile-project-p))
					(project-name my-project))
			   (setq my-current-project (ede-cpp-root-project my-project 
															  :name (concat project-name "-project")
															  :file (concat project-root ".projectile")))))))))
(defun my-parser-local-variable-include ()
  (cond ((boundp 'my-includes)
		 (let ((dirs (my-get-local-file-direcotry (eval my-includes))))
		   (if my-current-project
			   (let ((old-dirs (oref my-current-project include-path)))
				 (oset my-current-project include-path (delete-dups (append old-dirs dirs))))
			 (setq my-current-include (delete-dups (append my-current-include dirs))))
		   ))))

(defun my-get-project-include-directories ()
  (if my-current-project
	  (oref my-current-project include-path)
	my-current-include))

(add-hook 'hack-local-variables-hook
		  (lambda ()
			(my-parser-local-variable-project)
			(my-parser-local-variable-include)
			(setq ac-clang-flags (delete-dups
								  (append ac-clang-flags
										  (mapcar (lambda (item)
													(concat "-I" item))
												  (my-get-project-include-directories)
												  ))))
			))
