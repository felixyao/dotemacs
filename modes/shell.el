;; My shell mode configurations including eshell shell ansi-term

(defun my-emacs-start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
	(split-window-sensibly (selected-window))
	(other-window 1)
	(funcall function))
    (switch-to-buffer-other-window buffer-name)))
 


(defun my-emacs-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (my-emacs-start-or-switch-to (lambda ()
				(ansi-term (getenv "SHELL")))
			      "*ansi-term*"))

(setq eshell-directory-name (expand-file-name "eshell" my-emacs-auto-generate))

(define-key my-emacs-prefix-keymap (kbd "t") 'my-emacs-term-buffer)
(define-key my-emacs-prefix-keymap (kbd "e") 'eshell)
;; Start a new eshell even if one is active.
(define-key my-emacs-prefix-keymap (kbd "E") (lambda () (interactive) (eshell t)))
(define-key my-emacs-prefix-keymap (kbd "s") 'shell)

