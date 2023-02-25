(use-package on
  :ensure)

(use-package "startup"
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

(use-package emacs
  :custom
  (cursor-type 'bar))
(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package mode-line-bell
  :ensure
  :hook (on-first-input . #'mode-line-bell-mode))
