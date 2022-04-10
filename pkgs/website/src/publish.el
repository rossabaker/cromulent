(require 'ox-publish)
(require 'ox-html)
(require 'esxml)

(defun rossabaker.com/out ()
  (or (car command-line-args-left) (expand-file-name "../html")))

(defun rossabaker.com/header ()
  (sxml-to-xml
   '(nav
     (ul
      (li (a (@ (href "/")) "Home"))
      (li (a (@ (href "/talks")) "Talks"))))))

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
	 :html-head ,(rossabaker.com/header))
	("blog"
	 :base-directory ,(expand-file-name "org/blog/")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/blog/")
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
	 :html-head ,(rossabaker.com/header))
	("talks"
	 :base-directory ,(expand-file-name "org/talks/")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/talks/")
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Talks"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically
	 :html-head ,(rossabaker.com/header))
	("rossabaker.com" :components ("blog" "talks" "pages"))))

(org-publish-all t)
