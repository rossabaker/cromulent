(require 'ox-hugo)
(require 'ob-shell)
(require 's)

(with-current-buffer (find-file-noselect "src/org/cromulent.org")
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)
    (org-hugo-export-wim-to-md t)))
