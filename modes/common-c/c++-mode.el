;; Configuration for my c mode 

(add-hook 'c-mode-hook 
	  (lambda ()
	    (ggtags-mode)
	    (local-set-key (kbd "M-)") 'ggtags-find-reference)
	    ))
