(require 'ox-publish)
(require 'ox-html)

(setq org-export-with-section-numbers nil
      org-export-with-toc nil)

(setq org-publish-project-alist
      '(("pages"
	 :base-directory "~/src/rossabaker.com/org/"
	 :base-extension "org"
	 :recursive nil
	 :publishing-directory "~/src/rossabaker.com/html/"
	 :publishing-function org-html-publish-to-html)
	("blog"
	 :base-directory "~/src/rossabaker.com/org/blog/"
	 :base-extension "org"
	 :publishing-directory "~/src/rossabaker.com/html/blog/"
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically)
	("rossabaker.com" :components ("blog" "pages"))))

(delete-directory "~/src/rossabaker.com/html" t t)
(make-directory "~/src/rossabaker.com/html")
(make-directory "~/src/rossabaker.com/html/blog")
(org-publish "rossabaker.com" t)
