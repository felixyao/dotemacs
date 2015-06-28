;basic setting for python c++ and c mode
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)

(require 'auto-complete-clang)
(require 'auto-complete-c-headers)
(require 'ggtags)
(require 'cedet-global)
(require 'semantic)

(setq ac-clang-executable  "clang")

(global-semantic-idle-scheduler-mode)
(global-semantic-idle-completions-mode)
(global-semantic-decoration-mode)
(global-semantic-highlight-func-mode)
(global-semantic-show-unmatched-syntax-mode)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-summary-mode 1)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

(semantic-mode 1)


(add-hook 'c-mode-common-hook
          (lambda ()
			(setq ac-sources (append '(ac-source-c-headers ac-source-clang ac-source-yasnippet) ac-sources))
			(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
			(when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))
(set-default 'ac-clang-flags '("-std=c99"))
(define-key evil-normal-state-map (kbd "M-.") 'ggtags-find-tag-dwim)
(defun my-get-include-directories ()
  (append achead:include-directories (my-get-project-include-directories))
  )

(setq achead:get-include-directories-function 'my-get-include-directories)


