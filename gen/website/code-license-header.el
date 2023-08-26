(defun rab/org-hugo-src-block (src-block _contents info)
  (let* ((result (org-hugo-src-block src-block _contents info))
	 (block-info
	  (org-with-point-at (org-element-property :begin src-block)
	    (org-babel-get-src-block-info)))
	 (license (assoc-default :code-license (elt block-info 2))))
    (if (member license '("%code-license" ""))
	result
      (format "<div class=\"code-snippet\"><p class=\"license\">%s</p>\n\n%s\n\n</div>" license result))))

(with-eval-after-load 'ox-hugo
  (map-put!
   (org-export-backend-transcoders (org-export-get-backend 'hugo))
   'src-block 'rab/org-hugo-src-block))
