;All my projects source code store in github export stroed in dropbox
(defvar fy/source-folder "~/org_source"
"source code folder")

(unless (file-exists-p fy/source-folder)
 (make-directory fy/source-folder))

(unless (file-exists-p (expand-file-name "personal" fy/source-folder))
 (make-directory (expand-file-name "personal" fy/source-folder)))

(defvar fy/publich-folder "~/Dropbox/org_publish"
"publish folder")

(unless (file-exists-p fy/publich-folder)
 (make-directory fy/publich-folder))

(unless (file-exists-p (expand-file-name "personal" fy/publich-folder))
 (make-directory (expand-file-name "personal" fy/publich-folder)))

(require 'ox-publish)
(setq org-publish-project-alist
  `(
    ("org-personal"
     :base-directory ,(expand-file-name "personal" fy/source-folder)
     :base-extension "org"
     :publishing-directory ,(expand-file-name "personal" fy/publich-folder)
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4 
     :auto-preamble t
     )
    ("org-personal-static"
     :base-directory ,(expand-file-name "personal" fy/source-folder)
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory ,(expand-file-name "personal" fy/publich-folder)
     :recursive t
     :publishing-function org-publish-attachment
    )

    ,(if (file-exists-p (expand-file-name "work" fy/source-folder))
	 `("org-work"
	   :base-directory ,(expand-file-name "work" fy/source-folder)
	   :base-extension "org"
	   :publishing-directory ,(expand-file-name "work" fy/publich-folder)
	   :recursive t
	   :publishing-function org-html-publish-to-html
	   :headline-levels 4 
	   :auto-preamble t
	   ))

    ,(if (file-exists-p (expand-file-name "work" fy/source-folder))
      `("org-work"
         :base-directory ,(expand-file-name "work" fy/source-folder)
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,(expand-file-name "work" fy/publich-folder)
         :recursive t
         :publishing-function org-publish-attachment
       ))))
