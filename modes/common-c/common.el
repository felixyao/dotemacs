;basic setting for python c++ and c mode
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

(require 'auto-complete-clang)
(require 'auto-complete-c-headers)
(require 'ggtags)

(setq ac-clang-executable  "clang")

(add-hook 'c-mode-common-hook
          (lambda ()
			(setq ac-sources (append '(ac-source-c-headers ac-source-clang ac-source-yasnippet) ac-sources))
			(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(add-hook 'hack-local-variables-hook
		  (lambda ()
			(when (boundp 'my-project-includes)
			  (progn
				(setq ac-clang-flags (mapcar (lambda (item)
											   (concat "-I" item)) (eval my-project-includes) ))
				))))

(setq enable-local-variables :all)

(defun my-get-include-directories ()
  (if (boundp 'my-project-includes)
	  (append achead:include-directories (eval my-project-includes))
	achead:include-directories))

(setq achead:get-include-directories-function 'my-get-include-directories)
