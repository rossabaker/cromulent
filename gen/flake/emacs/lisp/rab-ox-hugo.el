;;; rab-ox-hugo.el --- Extensions to Ox-Hugo for Ross A. Baker's Emacs configuration.

;;; Commentary:
;;
;; Extensions to Ox-Hugo for Ross A. Baker's Emacs configuration.

;;; Code:

(require 'cl-macs)
(require 'ox-hugo)
(require 's)

(defgroup rab/ox-hugo nil
  "Ross A. Baker's Ox-Hugo extensions."
  :group 'org-export-hugo)

(defun rab/ox-hugo--get-filled-template-argument (key alist)
  "Helper for `org-fill-template` to handle missing properties.

Looks up KEY in ALIST.  If the result is If the result is \"%key\",
we assume the value was not present and return nil."
  (let* ((val (alist-get key alist))
         (fallthrough (concat "%" (s-chop-left 1 (symbol-name key)))))
    (unless (string= fallthrough val) val)))

(defun rab/ox-hugo-src-block (src-block contents info)
  "Wraps `org-hugo-src-block' with a code license, if found.

Invokes `org-hugo-src-block` on SRC_BLOCK, CONTENTS, and INFO.
If a `:code-license' header is found in the block arguments of
SRC-BLOCK, the result is wrapped in a div that includes the
license."
  (if-let* ((result (org-hugo-src-block src-block contents info))
            (block-info
             (org-with-point-at (org-element-property :begin src-block)
               (org-babel-get-src-block-info)))
            (block-arguments (elt block-info 2))
            (license (rab/ox-hugo--get-filled-template-argument
                      :code-license block-arguments)))
      (format "<div class=\"code-snippet\"><p class=\"license\">%s</p>\n\n%s\n\n</div>"
              license result)
    result))

(defun rab/ox-hugo--put-backend-transcoders (transcoders)
  "Puts TRANSCODERS alist to the ox-hugo backend as `src-block'."
  (cl-loop for (key . value) in transcoders
           do (map-put! (org-export-backend-transcoders
                         (org-export-get-backend 'hugo))
                        key value)))

(define-minor-mode rab/ox-hugo-mode
  "Minor mode to override Ox-Hugo's transcoders."
  :global t
  (if rab/ox-hugo-mode
      (rab/ox-hugo--put-backend-transcoders '((src-block . rab/ox-hugo-src-block)))
    (rab/ox-hugo--put-backend-transcoders '((src-block . org-hugo-src-block)))))

(defun rab/ox-hugo-update-lastmod ()
  "Put the last modified time to the current Hugo subtree.

Finds the nearest `EXPORT_FILE_NAME' property, and puts the
current time to the `EXPORT_HUGO_LAST_MOD' property."
  (interactive)
  (save-excursion
    (when-let* ((elem (car (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))
                (begin (org-element-property :begin elem))
                (time (format-time-string (org-time-stamp-format t t) (current-time))))
      (org-entry-put begin "EXPORT_HUGO_LASTMOD" time))))

(provide 'rab-ox-hugo)
;;; rab-ox-hugo.el ends here
