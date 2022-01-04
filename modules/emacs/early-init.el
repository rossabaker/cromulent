;;; early-init.el --- Run before init.el

;;; Commentary:

;; Aim for a bit faster speedup.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

;; All our packages come from Nix
(setq package-enable-at-startup nil
      package-archives nil)

(provide 'early-init)
;;; early-init.el ends here
