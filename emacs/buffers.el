;;revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
(global-set-key (kbd "<f5>") 'revert-buffer)


;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


(defun my-emacs-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))
(global-set-key (kbd "<f1>") 'my-emacs-create-scratch-buffer)

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
 
