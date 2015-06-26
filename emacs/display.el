;; outlook configuration
(tool-bar-mode -1)
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
(global-set-key (kbd "C--") 'text-scale-decrease)
;; show matched parent
(show-paren-mode t)
(cond
 ((string= system-type "gnu/linux")
  (set-face-attribute 'default nil :height 180))
 ((string= system-type "windows-nt")
  (set-face-attribute 'default nil :height 120))
 ((string= system-type "darwin")
  (set-face-attribute 'default nil :height 170)))
;;Set fullscreen
(defun my-emacs-fullscreen ()
  "Make Emacs window fullscreen.
This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

;; Turn on the truncate line mode by default
(set-default 'truncate-lines t)

;;Diminish some modes 
(require 'diminish)
(diminish 'abbrev-mode)
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "ggtags" '(diminish 'ggtags-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
