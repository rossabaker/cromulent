(eval-when-compile
  (require 'use-package))

;;; Core

(use-package files
  :custom
  (confirm-kill-emacs 'yes-or-no-p))

;;; UI

(use-package emacs
  :no-require
  :custom
  (use-dialog-box nil)
  :custom-face
  (default ((t :height 140))))

(use-package frame
  :config
  (blink-cursor-mode -1)
  :bind
  ("C-z" . nil)				; Previously suspend-frame
  )

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package startup
  :no-require
  :custom
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message t)
  (inhibit-startup-screen t))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

;;; Tools

(use-package magit
  :ensure)
