;; This file configurate Emacs buffer complete feature

;; hippie expand is dabbrev expand on steroids
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

(require 'auto-complete-config)
(setq ac-delay 0.5)
(ac-config-default)
(global-auto-complete-mode t)

(require 'yasnippet)
(yas-global-mode 1)

;Ido imenu
(autoload 'idomenu "idomenu" nil t)
(define-key my-emacs-prefix-keymap (kbd "t") 'idomenu)


