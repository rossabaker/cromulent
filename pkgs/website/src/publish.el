(require 'ox-publish)
(require 'ox-html)

(defun rossabaker.com/out ()
  (or (car command-line-args-left) (expand-file-name "../html")))

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-html-validation-link nil)

(setq org-publish-project-alist
      `(("pages"
	 :base-directory ,(expand-file-name "org/")
	 :base-extension "org"
	 :recursive nil
	 :publishing-directory ,(rossabaker.com/out)
	 :publishing-function org-html-publish-to-html
   	 :html-head "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/typebase.css/0.5.1/typebase.min.css\">
           <link href=\"http://fonts.googleapis.com/css?family=Source+Sans+Pro:300|Cantata+One\" rel=\"stylesheet\" type=\"text/css\">
           <style>
             #content { max-width: 50em; }
             html { font-family: \"Source Sans Pro\", sans-serif; }
           </style>")
	("blog"
	 :base-directory ,(expand-file-name "org/blog/")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/blog/")
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
   	 :html-head "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/typebase.css/0.5.1/typebase.min.css\">
           <link href=\"http://fonts.googleapis.com/css?family=Source+Sans+Pro:300|Cantata+One\" rel=\"stylesheet\" type=\"text/css\">
           <style>
             #content { max-width: 50em; }
             html { font-family: \"Source Sans Pro\", sans-serif; }
           </style>")
	("rossabaker.com" :components ("blog" "pages"))))

(org-publish-all t)
