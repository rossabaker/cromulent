(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package on
  :ensure)

(require 'bind-key)

(use-package "startup"
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

(use-package emacs
  :custom
  (line-spacing 0.25)
  :custom-face
  (default ((t :height 140)))
  :config
  (cond
   ((find-font (font-spec :name "IBM Plex Mono"))
    (set-face-attribute 'default nil :font "IBM Plex Mono"))))

(use-package emacs
  :custom
  (cursor-type 'bar))
(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi :no-confirm))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package mode-line-bell
  :ensure
  :hook (on-first-input . mode-line-bell-mode))

(use-package envrc
  :ensure t
  :hook (on-first-file . envrc-global-mode))

(use-package magit
  :ensure t
  :defer 1
  :bind ("C-c g g" . magit-status))

(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode))

(use-package nix-mode
  :ensure t
  :defer t)
