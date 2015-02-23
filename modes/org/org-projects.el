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


(defun fy/html-head-extra ()
  (concat  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
       "<link rel='stylesheet' href='../css/org-mannul.css'/>"))

(require 'ox-publish)
(setq org-publish-project-alist
  `(
    ("org-css"
     :base-directory ,(expand-file-name "css" fy/source-folder)
     :base-extension "css"
     :publishing-directory ,(expand-file-name "css" fy/publich-folder)
     :recursive f
     :publishing-function org-publish-attachment     
     )
     ("org-js"
     :base-directory ,(expand-file-name "js" fy/source-folder)
     :base-extension "js"
     :publishing-directory ,(expand-file-name "js" fy/publich-folder)
     :recursive nil
     :publishing-function org-publish-attachment     
     )
     ("org-img"
     :base-directory ,(expand-file-name "img" fy/source-folder)
     :base-extension "png\\|jpg\\|gif"
     :publishing-directory ,(expand-file-name "img" fy/publich-folder)
     :recursive nil
     :publishing-function org-publish-attachment     
     )
    ("org-personal"
     :base-directory ,(expand-file-name "personal" fy/source-folder)
     :base-extension "org"
     :publishing-directory ,(expand-file-name "personal" fy/publich-folder)
     :recursive t
     :publishing-function org-html-publish-to-html
     :headline-levels 4 
     :html-preamble nil
     :html-postamble nil
     :html-head-include-default-style nil
     :html-head-include-scripts nil
     :with-footnotes t
     :with-toc nil
     :auto-sitemap  t
     :sitemap-title " "
     :sitemap-sort-folders "last"
     :sitemap-sort-files "anti-chronologically"
     :sitemap-file-entry-format "%t"
     )
     ("org-personal-project"
      :components ("org-css" "org-js" "org-personal")
      )
    ,(if (file-exists-p (expand-file-name "work" fy/source-folder))
	 `("org-work"
	   :base-directory ,(expand-file-name "work" fy/source-folder)
	   :base-extension "org"
	   :publishing-directory ,(expand-file-name "work" fy/publich-folder)
	   :recursive t
	   :publishing-function org-html-publish-to-html
	   :headline-levels 4 
	   :html-preamble nil
	   :html-postamble nil
	   :html-head-include-default-style nil
	   :html-head-include-scripts nil
	   :with-footnotes t
	   :with-toc nil
	   :auto-sitemap  t
	   :sitemap-title " "
	   :sitemap-sort-folders "last"
	   :sitemap-sort-files "anti-chronologically"
	   :sitemap-file-entry-format "%t"
	   ))

    ,(if (file-exists-p (expand-file-name "work" fy/source-folder))
      `("org-work-project"
         :components ("org-css" "org-js" "org-work")
       ))))
