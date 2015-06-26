;;This file contains the configurations for all s-expression including schemem racket elisp


(require 'cmuscheme)
(require 'evil-paredit)

(defun kh/get-scheme-proc-create ()
  "Create one scheme process if no one is created."
  (unless (and scheme-buffer
               (get-buffer scheme-buffer)
               (comint-check-proc scheme-buffer))
    (save-window-excursion
      (run-scheme scheme-program-name))))

(defun kh/scheme-send-last-sexp ()
  "A replacement of original `scheme-send-last-sexp':
1. check if scheme process exists, otherwise create one
2. make sure the frame is splitted into two windows, current one is the scheme
   source code window, the other one is the scheme process window
3. run `scheme-send-last-sexp'"

  (interactive)
  (kh/get-scheme-proc-create)
  (cond ((= 2 (count-windows))
         (other-window 1)
         (unless (string= (buffer-name)
                          scheme-buffer)
           (switch-to-buffer scheme-buffer))
         (other-window 1))
        (t
         (delete-other-windows)
         (split-window-vertically (floor (* 0.68 (window-height))))
         (other-window 1)
         (switch-to-buffer scheme-buffer)
         (other-window 1)))
  (scheme-send-last-sexp))

(if (or (string= system-type "windows-nt")
        (string= system-type "darwin"))
    (setq scheme-program-name "plt-r5rs")
  (setq scheme-program-name "guile"))


(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-<left>") 'paredit-backward-slurp-sexp)))

(add-hook 'paredit-mode-hook
	  (lambda ()
	    (local-set-key (kbd "M-<right>") 'paredit-backward-barf-sexp)))

(if (string= system-type "darwin")
    (progn
      (add-hook 'paredit-mode-hook
		(lambda ()
		  (local-set-key (kbd "H-M-<right>") 'paredit-forward-slurp-sexp)))
      (add-hook 'paredit-mode-hook
		(lambda ()
		  (local-set-key (kbd "H-M-<left>") 'paredit-forward-barf-sexp)))))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
			(paredit-mode 1)
			(evil-paredit-mode 1)))

(add-hook 'scheme-mode-hook
          (lambda ()
             (paredit-mode 1)
			 (evil-paredit-mode 1)
			 (local-set-key (kbd "C-x C-e") 'kh/scheme-send-last-sexp)))
