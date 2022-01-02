;;; early-init.el --- Run before init.el

;;; Commentary:

;; Aim for a bit faster speedup.

;;; Code:

(setq gc-cons-threshold most-positive-fixnum)

(provide 'early-init)
;;; early-init.el ends here
