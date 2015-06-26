;; My shell mode configurations including eshell shell ansi-term
(require 'shell)
(require 'term)
;get rid of the C-M
(add-hook 'comint-output-filter-functions
            'comint-strip-ctrl-m)

(defun my-create-shell ()
  "Create another shell buffer"
  (interactive)
  (cond ((or (equal major-mode 'term-mode)
	     (equal major-mode 'shell-mode))
	 (shell (generate-new-buffer-name "*shell*")))
	((equal major-mode 'eshell-mode)
	 (eshell t))
	(t (shell))))

(defun my-term-switch-to-shell-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (progn
        (shell-mode)
        (set-process-filter  (get-buffer-process (current-buffer)) 'comint-output-filter )
        (local-set-key (kbd "C-j") 'my-term-switch-to-shell-mode)
        (compilation-shell-minor-mode 1)
        (comint-send-input)
	)
    (progn
      (compilation-shell-minor-mode -1)
      (font-lock-mode -1)
      (set-process-filter  (get-buffer-process (current-buffer)) 'term-emulate-terminal)
      (term-mode)
      (term-char-mode)
      (term-send-raw-string (kbd "C-l"))
      )))

(defun my-clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;remap C-z to real shell
(add-hook 'shell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-z") 'self-insert-command)
	    (local-set-key (kbd "<f1>") 'my-create-shell)
	    (local-set-key (kbd "C-j") 'my-term-switch-to-shell-mode)
	    (local-set-key (kbd "C-l") 'my-clear-shell)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-z") 'self-insert-command)
	    (local-set-key (kbd "<f1>") 'my-create-shell)))

(add-hook 'term-mode-hook
	  (lambda ()
		(yas-minor-mode -1)
	    (define-key term-raw-map (kbd "C-z") 'self-insert-command)
	    (define-key term-raw-map (kbd "<f1>") 'my-create-shell)
	    (define-key term-raw-map (kbd "C-j") 'my-term-switch-to-shell-mode)
	    (define-key term-raw-map (kbd "C-l") (lambda ()
						   (interactive)
						   (term-send-raw-string (kbd "C-l"))))))

(setq eshell-directory-name (expand-file-name "eshell" my-emacs-auto-generate))

