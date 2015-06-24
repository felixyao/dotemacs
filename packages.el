;; This File automaticlly check the package and load them
(require 'cl)
(require 'package)

(defvar my-emacs-auto-generate "~/.emacs.d/auto"
"Place all the generated files here ")

(unless (file-exists-p my-emacs-auto-generate)
 (make-directory my-emacs-auto-generate))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives 
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives 
      '("org" . "http://orgmode.org/elpa/") t)
 
(setq ac-comphist-file (expand-file-name "ac-comphist.dat"  my-emacs-auto-generate))

;; Default url directory
(setq url-configuration-directory (expand-file-name "url" my-emacs-auto-generate))

;; Packages I like
(defvar my-packages '(desktop 
                      magit 
					  projectile
					  paredit 
					  smex 
					  idomenu
					  autopair 
					  htmlize 
					  org 
					  org-plus-contrib 
					  ggtags 
					  yasnippet 
					  auto-complete 
					  auto-complete-c-headers 
					  auto-complete-clang  
					  auto-complete-nxml))

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(package-initialize)

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (condition-case nil
    (package-refresh-contents)
  (error nil))
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (condition-case nil
        (package-install p)
       (error nil)))))
