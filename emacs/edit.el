(defun my-emacs-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))
(global-set-key [remap kill-whole-line] 'my-emacs-kill-whole-line)

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

;; Always load newest byte code
(setq load-prefer-newer t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; enable cua rectangle mode
(cua-selection-mode t)

;; try to using ido complete the filename
(setq ido-use-filename-at-point 'guess)

;; Suit for Nodic layout 
(define-key my-emacs-prefix-keymap (kbd "e") 'end-of-buffer)
(define-key my-emacs-prefix-keymap (kbd "a") 'beginning-of-buffer)
