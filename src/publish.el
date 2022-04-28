(require 'ox-publish)
(require 'ox-html)
(require 'esxml)
(require 'htmlize)

(defun rossabaker.com/out ()
  (or (car (reverse command-line-args-left))
      (expand-file-name "../html")))

(defun rossabaker.com/cache-buster (path)
  "Busts the cache by appending the Nix sources' hash as a query string."
  (concat path "?" (string-remove-prefix "/nix/store/" (string-remove-suffix "-src" (getenv "srcs")))))

(defvar rossabaker.com/head
  (mapconcat 'esxml-to-xml
   `((link ((rel . "preconnect") (href . "https://fonts.googleapis.com")))
     (link ((rel . "preconnect") (href . "https://fonts.gstatic.com") (crossorigin . "anonymous")))
     (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/css2?family=Fira+Code:wght@500&family=Fira+Sans&display=swap")))
     (link ((href . "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css")
	    (rel . "stylesheet")
	    (integrity . "sha384-1BmE4kWBq78iYhFldvKuhfTAU6auU8tT94WrHftjDbrCEXSU1oBoqyl2QvZ6jIW3")
	    (crossorigin . "anonymous")))
     (link ((rel . "stylesheet") (href . ,(rossabaker.com/cache-buster "/css/style.css")))))
   "\n"))

(defun rossabaker.com/preamble (info)
  (esxml-to-xml
   '(nav ((class . "navbar navbar-expand-lg navbar-light bg-light"))
	 (div ((class . "container-fluid"))
	      (a ((class . "navbar-brand")
		  (href . "/"))
		 "~rossabaker/")
	      (button ((class . "navbar-toggler")
		       (type . "button")
		       (data-bs-toggle . "collapse")
		       (data-bs-target . "#navbarSupportedContent")
		       (aria-controls . "navbarSupportedContent")
		       (aria-expanded . "false")
		       (aria-label . "Toggle navigation"))
		      (span ((class . "navbar-toggler-icon"))))
	      (div ((class . "collapse navbar-collapse")
		    (id . "navbarSupportedContent"))
		   (ul ((class . "navbar-nav"))
		       (li ((class . "nav-item")) (a ((href . "/talks/") (class . "nav-link")) "Talks"))
		       (li ((class . "nav-item")) (a ((href . "/config/") (class . "nav-link")) "Config"))))))))

(defun rossabaker.com/postamble (info)
  (esxml-to-xml
   '(script ((src . "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js")
	     (integrity . "sha384-ka7Sk0Gln4gmtz2MlQnikT1wXgYsOg+OMhuP+IlRH9sENBO0LRn5q+8nbTov4+1p")
	     (crossorigin . "anonymous")))))

(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-html-validation-link nil
      org-html-doctype "html5"
      org-html-head rossabaker.com/head
      org-html-preamble #'rossabaker.com/preamble
      org-html-postamble #'rossabaker.com/postamble
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil
      org-html-content-class "container")

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
	 :include (".well-known/matrix/server"
		   ".well-known/matrix/client")
	 :publishing-directory ,(rossabaker.com/out)
	 :publishing-function org-publish-attachment
	 :recursive t)
	("rossabaker.com" :components ("assets" "blog" "talks" "pages"))))

(org-publish-all t)
