;; My shell mode configurations including eshell shell ansi-term
(require 'shell)
(require 'multi-term)
(require 'term)
(require 'eshell)

;;Windows use the normal shell
;;Linux use multi-term

(if (string= system-type "windows-nt")
     (setq multi-term-program "sh.exe"))

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

(defun my-toggle-line-mode ()
 "change between line mode and char mode"
 (interactive)
 (if (term-in-char-mode)
     (term-line-mode)
   (term-char-mode)))

(defun my-clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defvar my-term-bind-key-alist
 '(
   ("<escape>" . term-send-esc)
   ("<M-left>" . multi-term-prev )
   ("<M-right>" . multi-term-next)
   ("<f1>" . my-create-shell)
   ("M-z" . my-toggle-line-mode)
   ))

(defvar my-term-unbind-key-alist
 '(
   ;"C-b"
   ;"C-f"
   ))

(defvar my-eshell-term-commands
 '(
   "ssh"
   "telnet"
   "mutt"
   ))


(defun my-ido-eshll-history-list ()
  (interactive)
  (insert
   (ido-completing-read "History Commands:"
                        (delete-dups
                         (ring-elements eshell-history-ring)))))

(defun eshell/s (&optional path)
  (if (not (equal path nil))
      (magit-status path)
    (magit-status (eshell/pwd))))


(setq term-bind-key-alist (append term-bind-key-alist my-term-bind-key-alist))
(setq term-unbind-key-list (append term-unbind-key-list my-term-unbind-key-alist))

(setq multi-term-dedicated-skip-other-window-p t)


(define-key my-emacs-prefix-keymap (kbd "e") 'my-create-terminal)

(add-hook 'shell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f1>") 'my-create-shell)
	    (local-set-key (kbd "C-l") 'my-clear-shell)))

(add-hook 'term-mode-hook
	  (lambda ()
        (define-key term-mode-map (kbd "<f1>") 'my-create-shell)
        (define-key term-mode-map (kbd "M-z") 'my-toggle-line-mode)))

(add-hook 'eshell-mode-hook
	  (lambda ()
	    (setq eshell-visual-commands (append eshell-visual-commands  my-eshell-term-commands))
	    (local-set-key (kbd "<f1>") 'my-create-shell)
	    (local-set-key (kbd "M-r") 'my-ido-eshll-history-list)))

(setq eshell-directory-name (expand-file-name "eshell" my-emacs-auto-generate))
