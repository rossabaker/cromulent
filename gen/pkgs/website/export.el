(require 'ob-dot)
(require 'ob-shell)
(require 'rab-ox-hugo)

(rab/ox-hugo-mode)

(with-current-buffer (find-file-noselect "src/org/cromulent.org")
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)
    (org-hugo-export-wim-to-md t)))
