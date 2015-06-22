;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

(defvar my-emacs-prefix-keymap nil
"My personal prefix keymap")
(define-prefix-command 'my-emacs-prefix-keymap)
;;Define preferable key 
(global-set-key (kbd "C-z") 'my-emacs-prefix-keymap)

;;Configuration from custom 
(setq custom-file "~/.emacs.d/custom.el")
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/emacs/display")
(load "~/.emacs.d/emacs/mark")
(load "~/.emacs.d/emacs/buffers")
(load "~/.emacs.d/emacs/edit")
(load "~/.emacs.d/emacs/files")
(load "~/.emacs.d/emacs/minibuffer")
(load "~/.emacs.d/emacs/completion")
(load "~/.emacs.d/emacs/bookmark")
(load "~/.emacs.d/emacs/project.el")

(load "~/.emacs.d/modes/shell")
(load "~/.emacs.d/modes/magit")
(load "~/.emacs.d/modes/sexp")
(load "~/.emacs.d/modes/org/org")
(load "~/.emacs.d/modes/org/org-bable")
(load "~/.emacs.d/modes/org/org-projects")
(load "~/.emacs.d/modes/common-c/common")
(load "~/.emacs.d/modes/common-c/c-mode")
(load "~/.emacs.d/modes/common-c/c++-mode")
(load custom-file)
