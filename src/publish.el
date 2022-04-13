(require 'ox-publish)
(require 'ox-html)
(require 'esxml)

(defun rossabaker.com/out ()
  (or (car command-line-args-left) (expand-file-name "../html")))

(defvar rossabaker.com/head
  (mapconcat 'esxml-to-xml
   '((link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
     (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "anonymous")))
     (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/css2?family=Bitter:wght@800&family=Fira+Code:wght@500&family=Fira+Sans&display=swap")))
     (link ((rel . "stylesheet") (href . "/css/style.css"))))
   ""))

(defun rossabaker.com/preamble (info)
  (esxml-to-xml
   '(nav ()
	 (a ((href . "/") (id . "wordmark")) "~rossabaker/")
	 (ul ()
	     (li () (a ((href . "/")) "Home"))
	     (li () (a ((href . "/talks/")) "Talks"))
	     (li () (a ((href . "/config/")) "Config"))))))

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-html-validation-link nil
      org-html-head rossabaker.com/head
      org-html-preamble #'rossabaker.com/preamble)

(setq org-publish-project-alist
      `(("pages"
	 :base-directory ,(expand-file-name "org/")
	 :base-extension "org"
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
	("talks"
	 :base-directory ,(expand-file-name "org/talks/")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/talks/")
	 :publishing-function org-html-publish-to-html
	 :auto-sitemap t
	 :sitemap-title "Talks"
	 :sitemap-filename "index.org"
	 :sitemap-sort-files anti-chronologically)
	("config"
	 :base-directory ,(expand-file-name "org/config")
	 :base-extension "org"
	 :publishing-directory ,(concat (rossabaker.com/out) "/config/")
	 :publishing-function org-html-publish-to-html)
	("assets"
	 :base-directory ,(expand-file-name "org/")
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg"
	 :publishing-directory ,(rossabaker.com/out)
	 :publishing-function org-publish-attachment
	 :recursive t)
	("rossabaker.com" :components ("assets" "blog" "talks" "pages"))))

(org-publish-all t)
