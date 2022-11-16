(require 'ob-shell)
(require 'ox-hugo)
(require 'ox-publish)
(require 'htmlize)

(defvar ross-www/src-directory
  (expand-file-name "src" (getenv "PRJ_ROOT"))
  "Source directory for rossabaker.com.")

(defvar ross-www/tmp-directory
  (expand-file-name "tmp" (getenv "PRJ_ROOT"))
  "Source directory for rossabaker.com.")

(defvar ross-www/public-html-directory
  (expand-file-name "public_html" ross-www/tmp-directory)
  "Source directory for rossabaker.com.")

(defun ross-www/legacy-publish ()
  "Build the legacy ox-hugo website."
  (interactive)
  (find-file (expand-file-name "org/rossabaker.org" ross-www/src-directory))
  (let ((org-confirm-babel-evaluate nil))
    (message "Tangling legacy ox-hugo files")
    (org-babel-tangle)
    (mkdir (expand-file-name "hugo/static" ross-www/tmp-directory) t)
    (message "Exporting legacy ox-hugo pages")
    (org-hugo-export-wim-to-md t))
  (message "Running hugo")
  (let ((default-directory (or (getenv "PRJ_ROOT") (getenv "NIX_BUILD_TOP") default-directory)))
    (message "Default directory: %s" default-directory)
    (process-file (executable-find "hugo")
                  nil
                  (if (getenv "NIX_BUILD_TOP") '(:file "/dev/stdout") t)
                  nil
                  "--config"
                  (expand-file-name "hugo/config.toml" ross-www/tmp-directory)
                  "--destination"
                  ross-www/public-html-directory
                  "-F")))

(org-export-define-backend 'ross-www/redirects
  nil
  :options-alist
  '((:redirect-from "REDIRECT_FROM" nil nil t)))

(defun ross-www/publish-redirects (plist filename pub-dir)
  "Publish the _redirects file."
  (let* ((buffer (get-buffer-create " *ross-www/publish-redirects*"))
         (ext (or (plist-get plist :html-extension) ".html"))
         (output-abs (with-temp-buffer
                       (insert-file-contents filename)
                       (set-visited-file-name filename t)
                       (not-modified)
                       (org-export-output-file-name ext nil pub-dir)))
         (output-rel (file-relative-name output-abs ross-www/public-html-directory))
         (output (concat "/" (string-remove-suffix "/index.html" output-rel))))
    (with-current-buffer buffer
      (when-let ((redirect-from (org-publish-find-property filename :redirect-from nil 'ross-www/redirects)))
        (insert redirect-from)
        (insert "\t")
        (insert output)
        (insert "\n")))))

(defun ross-www/publish-redirects-prepare (plist)
  "Creates an empty buffer for publishing redirects."
  (let ((buffer (get-buffer-create " *ross-www/publish-redirects*")))
    (with-current-buffer buffer
      (erase-buffer))))

(defun ross-www/publish-redirects-complete (plist)
  "Writes the redirects buffer."
  (let ((buffer (get-buffer-create " *ross-www/publish-redirects*"))
        (redirects-file-name (expand-file-name "_redirects" ross-www/public-html-directory)))
    (with-current-buffer buffer
      (message "Writing redirects to %s" redirects-file-name)
      (write-file redirects-file-name)
      (kill-current-buffer))))

(add-to-list 'org-publish-project-alist
             `("rossabaker.com"
               :base-directory ,(expand-file-name "org/" ross-www/src-directory)
               :base-extension "org"
               :recursive t
               :exclude ,(rx (or "rossabaker.org" "config/" "talks/"))
               :preparation-function ross-www/publish-redirects-prepare
               :publishing-function (org-html-publish-to-html ross-www/publish-redirects)
               :publishing-directory ,(expand-file-name "public_html" ross-www/tmp-directory)
               :completion-function ross-www/publish-redirects-complete
               :section-numbers nil
               :with-toc nil))

(defun ross-www/publish ()
  "Publish the website."
  (interactive)
  (if current-prefix-arg
      (ross-www/legacy-publish))
  (dolist (project '("org/projects/www/index.org" "org/projects/wkd/index.org"))
    (let ((project-file-name (expand-file-name project ross-www/src-directory)))
      (with-temp-buffer
        (insert-file-contents project-file-name)
        (set-visited-file-name project-file-name t)
        (org-babel-tangle)
        (not-modified))))
  (let ((org-confirm-babel-evaluate nil))
    (org-publish "rossabaker.com" current-prefix-arg)))
