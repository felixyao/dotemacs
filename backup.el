


;; outlook configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)
;; disable startup screen
(setq inhibit-startup-screen t)
(global-linum-mode t)
(column-number-mode t)
(size-indication-mode t)
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
p(global-set-key (kbd "<f5>") 'revert-buffer)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;Set fullscreen
(defun my-emacs-fullscreen ()
  "Make Emacs window fullscreen.
This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'my-emacs-fullscreen))



(defun my-emacs-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'my-emacs-kill-whole-line)

(defvar my-emacs-prefix-keymap nil
"My personal prefix keymap")

(define-prefix-command 'my-emacs-prefix-keymap)

(global-set-key (kbd "<f8>") 'my-emacs-prefix-keymap)
(global-set-key (kbd "C-;") 'my-emacs-prefix-keymap)
 
(defun my-emacs-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
 
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
 
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
 
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
 
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
(global-set-key [remap move-beginning-of-line] 'my-emacs-move-beginning-of-line)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq eshell-directory-name (expand-file-name "eshell" my-emacs-auto-generate))
 
(setq package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/"))))

(setq ac-comphist-file (expand-file-name "ac-comphist.dat"  my-emacs-auto-generate))

;; Default url directory
(setq url-configuration-directory (expand-file-name "url" my-emacs-auto-generate))
 
 
;; Always load newest byte code
(setq load-prefer-newer t)
 
;; highlight the current line
(global-hl-line-mode +1)
 
;;builtin-package initializaiton
;; Enable the windows key banding and rectangle edit
(cua-mode)
 
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
 
 
;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


(setq bookmark-default-file (expand-file-name "bookmarks" my-emacs-auto-generate))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,my-emacs-tempoary-direcotry)))


(setq auto-save-file-name-transforms
      `((".*" ,my-emacs-tempoary-direcotry t)))

(setq auto-save-list-file-prefix
        my-emacs-tempoary-direcotry)

(setq tramp-auto-save-directory my-emacs-tempoary-direcotry)
       
;;external pacaage initializaion
;;enable the desktop mode
(require 'cl)
 
(defvar my-emacs-packages-global-run 
  '(uniquify
    auto-complete-config
    anzu
    dired-x
    midnight
    magit
    volatile-highlights
    desktop
   ) 
"All external packages default on")
 
(defun my-emacs-require-packages (packages)
  "Load all the external packages if need"
  (dolist (element packages)
    (require element)))
    
(eval-after-load "desktop" 
 '(progn
 (setq desktop-save t)
 (setq desktop-path (list my-emacs-auto-generate))
 (setq desktop-dirname my-emacs-auto-generate)
 (desktop-save-mode +1)
 (desktop-revert)))
 

(eval-after-load "smartparens-config"
  '(progn (setq sp-base-key-bindings 'paredit)
	 (setq sp-autoskip-closing-pair 'always)
	 (setq sp-hybrid-kill-entire-symbol nil)
	 (sp-use-paredit-bindings)
	 (show-smartparens-global-mode +1)))



(eval-after-load "uniquify"
  '(progn (setq uniquify-buffer-name-style 'forward)
	 (setq uniquify-separator "/")
	 (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
	 (setq uniquify-ignore-buffers-re "^\\*"))) ; don't muck with special buffers

(eval-after-load "anzu" 
  '(global-anzu-mode))

(eval-after-load "volatile-highlights"
  '(volatile-highlights-mode +1))


(add-hook 'after-init-hook '(lambda () (my-emacs-require-packages my-emacs-packages-global-run)))

;;Configuration from custom 
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)



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
(define-key my-emacs-prefix-keymap (kbd "t") 'my-emacs-term-buffer)

(define-key my-emacs-prefix-keymap (kbd "e") 'eshell)

;; Start a new eshell even if one is active.
(define-key my-emacs-prefix-keymap (kbd "E") (lambda () (interactive) (eshell t)))
(define-key my-emacs-prefix-keymap (kbd "s") 'shell)

(define-key my-emacs-prefix-keymap (kbd "g") 'magit-status)

(define-key my-emacs-prefix-keymap (kbd "j l") 'goto-line)
(define-key my-emacs-prefix-keymap (kbd "j b") 'switch-to-buffer-other-window)
(define-key my-emacs-prefix-keymap (kbd "j w") 'ace-jump-word-mode)
(define-key my-emacs-prefix-keymap (kbd "j c") 'ace-jump-char-mode)
(define-key my-emacs-prefix-keymap (kbd "j r") 'jump-to-register)
(define-key my-emacs-prefix-keymap (kbd "j m") 'bookmark-bmenu-list)

 
(defun my-emacs-insert-date ()
  "Insert a timestamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%c" (current-time))))
(define-key my-emacs-prefix-keymap (kbd "i t") 'my-emacs-insert-date)

 
(defun my-emacs-create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (progn
    (switch-to-buffer
     (get-buffer-create (generate-new-buffer-name "*scratch*")))
    (emacs-lisp-mode)))
(global-set-key (kbd "<f6>") 'my-emacs-create-scratch-buffer)
 

