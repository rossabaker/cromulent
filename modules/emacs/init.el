(eval-when-compile
  (require 'use-package))

(use-package emacs
  :custom-face
  (default ((t :height 140))))

(use-package frame
  :config
  (blink-cursor-mode -1))
