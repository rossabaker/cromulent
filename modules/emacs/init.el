(eval-when-compile
  (require 'use-package))

;;; Core

(use-package startup
  :no-require
  :custom
  (initial-scratch-message nil)
  (inhibit-startup-screen t))

;;; UI

(use-package emacs
  :custom-face
  (default ((t :height 140))))

(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

;;; Tools

(use-package magit
  :ensure)
