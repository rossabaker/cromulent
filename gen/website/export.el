(require 'ox-hugo)
(require 'ob-shell)

(put 'org-hugo-external-file-extensions-allowed-for-copying 'safe-local-variable 'listp)
(find-file "src/org/rossabaker.org")
(setq org-confirm-babel-evaluate nil)
(org-babel-tangle)
(mkdir "../../.hugo-out/static")
(org-hugo-export-wim-to-md t)
