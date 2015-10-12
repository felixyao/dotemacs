;basic setting for python c++ and c mode
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode nil)

(require 'auto-complete-clang)
(require 'auto-complete-c-headers)
(require 'ggtags)
(require 'cedet-global)
(require 'semantic)

(setq ac-clang-executable  "clang")

(add-hook 'c-mode-common-hook
          (lambda ()
			(setq ac-sources (append '(ac-source-c-headers ac-source-clang) ac-sources))
			(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(set-default 'ac-clang-flags nil)
(define-key evil-normal-state-map (kbd "M-.") 'ggtags-find-tag-dwim)
(defun my-get-include-directories ()
  (append achead:include-directories (my-get-project-include-directories))
  )

(setq achead:get-include-directories-function 'my-get-include-directories)


