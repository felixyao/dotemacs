;; My shell mode configurations including eshell shell ansi-term
(require 'shell)
(require 'multi-term)

;;Windows use the normal shell
;;Linux use multi-term

(if (string= system-type "windows-nt")
     (setq multi-term-program "sh.exe"))

(defvar my-term-bind-key-alist
 '(
   ("<M-left>" . multi-term-prev )
   ("<M-right>" . multi-term-next)
   ))

(defvar my-term-unbind-key-alist
 '(
   "C-b"
   "C-f"
   ))

(setq term-bind-key-alist (append term-bind-key-alist my-term-bind-key-alist))
(setq term-unbind-key-list (append term-unbind-key-list my-term-unbind-key-alist))

(setq multi-term-dedicated-skip-other-window-p t)
(defun my-create-shell ()
  "Create another shell buffer"
  (interactive)
  (cond
    ((equal major-mode 'shell-mode)
	 (shell (generate-new-buffer-name "*shell*")))
	((equal major-mode 'eshell-mode)
	 (eshell t))
	((equal major-mode 'term-mode)
	 (multi-term))
	(t (shell))))

(defun my-create-terminal ()
  "Create a dedicated terminal if not exist otherwise jump to it"
  (interactive)
  (if (not (eq nil current-prefix-arg))
      (multi-term-dedicated-close)
      (if  (multi-term-dedicated-exist-p)
          (multi-term-dedicated-select)
        (multi-term-dedicated-toggle ))))

(define-key my-emacs-prefix-keymap (kbd "e") 'my-create-terminal)

(defun my-clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(add-hook 'shell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f1>") 'my-create-shell)
	    (local-set-key (kbd "C-l") 'my-clear-shell)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f1>") 'my-create-shell)))

(add-hook 'term-mode-hook
	  (lambda ()
	    (define-key term-raw-map (kbd "<f1>") 'my-create-shell)))

(setq eshell-directory-name (expand-file-name "eshell" my-emacs-auto-generate))

