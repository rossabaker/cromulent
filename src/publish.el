(require 'ox-publish)
(require 'ox-html)

(defun rossabaker.com/out ()
  (or (car command-line-args-left) (expand-file-name "../html")))

(setq org-export-with-section-numbers nil
      org-export-with-toc nil)

(setq org-publish-project-alist
      `(("pages"
	 :base-directory ,(expand-file-name "org/")
	 :base-extension "org"
	 :recursive nil
	 :publishing-directory ,(rossabaker.com/out)
	 :publishing-function org-html-publish-to-html)
	("blog"
	 :base-directory ,(expand-file-name "org/blog/")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/blog/")
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Blog Posts"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically)
	("rossabaker.com" :components ("blog" "pages"))))

(org-publish-all t)
