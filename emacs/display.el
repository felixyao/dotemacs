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
;;Set fullscreen
(defun my-emacs-fullscreen ()
  "Make Emacs window fullscreen.
This follows freedesktop standards, should work in X servers."
  (interactive)
  (if (eq window-system 'x)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0))
    (error "Only X server is supported")))

