

(defvar my-emacs-tempoary-direcotry (expand-file-name "tmp" my-emacs-auto-generate)
"Auto save directory")


(unless (file-exists-p my-emacs-tempoary-direcotry)
 (make-directory my-emacs-tempoary-direcotry))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,my-emacs-tempoary-direcotry)))


(setq auto-save-file-name-transforms
      `((".*" ,my-emacs-tempoary-direcotry t)))

(setq auto-save-list-file-prefix
        my-emacs-tempoary-direcotry)

(setq tramp-auto-save-directory my-emacs-tempoary-direcotry)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq ido-max-directory-size 100000)
(ido-mode 1)
