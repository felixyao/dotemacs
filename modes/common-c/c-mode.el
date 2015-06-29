;; Configuration for my c mode 

(add-hook 'c-mode-hook 
	  (lambda ()
	    (local-set-key (kbd "M-)") 'ggtags-find-reference)
	    (modify-syntax-entry ?_ "w" c-mode-syntax-table)
	    ))
