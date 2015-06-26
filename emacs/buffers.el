;;revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(global-set-key (kbd "<f5>") 'revert-buffer)


;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
(evil-ex-define-cmd "[b]uffer" 'ibuffer)

;;Ibuffer group settings
(setq ibuffer-saved-filter-groups
      '(("default"
	 ("code " (or
		   (mode . c-mode)
		   (mode . c++-mode)
		   (mode . python-mode)
		   (mode . cperl-mode)))
	 ("org" (or
		 (mode . org-mode)
		 (name . "^\\*Org Agenda\\*$")))
	 
	 ("shell" (or
		   (mode . shell-mode)
		   (mode . term-mode)
		   (mode . eshell-mode)))
	 ("dired" (mode . dired-mode))
	 ("lisp" (or
		 (mode . emacs-lisp-mode)
		 (mode . scheme-mode)))
	 ("New" (or
		   (name . "^\\*NEW\\*\\(<[0-9]+>\\)?$")))
	 ("emacs" (or
		   (name . "^\\*scratch\\*$")
		   (name . "^\\*Help\\*$")
		   (name . "^\\*Completions\\*$")
		   (name . "^\\*Backtrace\\*$")
		   (name . "^\\*Messages\\*$")))
	 ("gnus" (or
		  (mode . message-mode)
		  (mode . bbdb-mode)
		  (mode . mail-mode)
		  (mode . gnus-group-mode)
		  (mode . gnus-summary-mode)
		  (mode . gnus-article-mode)
		  (name . "^\\.bbdb$")
		  (name . "^\\.newsrc-dribble"))))
	))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil)

(defun my-emacs-create-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*NEW*")))
    (text-mode)))
(global-set-key (kbd "<f1>") 'my-emacs-create-buffer)

;; Always create new buffer the buffer name doesn't exist
(setq ido-create-new-buffer 'always)

;;Make buffer name with directory
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)	
(setq uniquify-ignore-buffers-re "^\\*")

(require 'desktop)
(setq desktop-path (list my-emacs-auto-generate))
(setq desktop-dirname my-emacs-auto-generate)
(setq desktop-save t)
(desktop-save-mode +1)

(setq desktop-base-lock-name
       (format "%s-%d" desktop-base-lock-name (emacs-pid)))
 
