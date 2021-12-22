(eval-when-compile
  (require 'use-package))

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
