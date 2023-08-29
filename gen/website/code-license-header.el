(defun rab/org-get-filled-template-argument (key alist)
  "Looks up KEY in ALIST, which is assumed to be from a template
filled by `org-fill-template'.

  If the result is \"%key\", we assume the value was not present
 and return nil."
  (let* ((val (alist-get key alist))
         (fallthrough (concat "%" (s-chop-left 1 (symbol-name key)))))
    (unless (string= fallthrough val) val)))

(defun rab/org-hugo-src-block (src-block _contents info)
  "Invokes `org-hugo-src-block' on its arguments, and then wraps the
result in a license div if `:code-license' is found in the block
arguments of SRC-BLOCK."
  (if-let* ((result (org-hugo-src-block src-block _contents info))
            (block-info
             (org-with-point-at (org-element-property :begin src-block)
               (org-babel-get-src-block-info)))
            (block-arguments (elt block-info 2))
            (license (rab/org-get-filled-template-argument
                      :code-license block-arguments)))
      (format "<div class=\"code-snippet\"><p class=\"license\">%s</p>\n\n%s\n\n</div>"
              license result)
    result))

(with-eval-after-load 'ox-hugo
  (map-put!
   (org-export-backend-transcoders (org-export-get-backend 'hugo))
   'src-block 'rab/org-hugo-src-block))
