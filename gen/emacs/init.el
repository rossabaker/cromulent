(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package on
  :ensure)

(use-package gnutls
  :defer t
  :custom
  (gnutls-verify-error t))

(use-package no-littering
  :ensure t
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	no-littering-var-directory "~/.cache/emacs/var/")
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "eln-cache/" no-littering-var-directory)))))

(require 'bind-key)

(use-package diminish :ensure t)

(use-package "startup"
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

(use-package emacs
  :custom
  (frame-inhibit-implied-resize t))

(use-package fontaine
  :ensure t
  :demand t
  :custom
  (fontaine-presets
   `((regular
      :default-height 140
      :line-spacing 0.25)
     (presentation
      :default-height 210
      :line-spacing 0.125)
     (t ;; defaults
      :default-family
      ,(cond
	((find-font (font-spec :name "IBM Plex Mono"))
	 "IBM Plex Mono")
	("Monospace")))))
  :config
  (fontaine-set-preset (or fontaine-current-preset 'regular)))

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

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package mode-line-bell
  :ensure
  :hook (on-first-input . mode-line-bell-mode))

(use-package simple
  :custom
  (set-mark-command-repeat-pop t))

(use-package magit
  :ensure t
  :defer 1
  :bind ("C-c g g" . magit-status))

(use-package git-link
  :ensure t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

(use-package envrc
  :ensure t
  :hook (on-first-file . envrc-global-mode))

(use-package grab-mac-link
  :disabled t
  :ensure t
  :commands (grab-mac-link grab-mac-link-dwim)
  :custom
  (grab-mac-link-dwim-favourite-app 'firefox))

(use-package titlecase
  :ensure t
  :defer t)

(use-package ws-butler
  :ensure t
  :diminish
  :config
  (ws-butler-global-mode))

(use-package ox-hugo
  :defer
  :ensure t
  :config
  (defun ross/ox-hugo-update-lastmod ()
    "Updates the EXPORT_HUGO_LAST_MOD property of the nearest element with EXPORT_FILE_NAME."
    (interactive)
      (save-excursion
	(when-let* ((elem (car (org-hugo--get-elem-with-prop :EXPORT_FILE_NAME)))
		    (begin (org-element-property :begin elem))
		    (time (format-time-string (org-time-stamp-format t) (current-time))))
	  (org-entry-put begin "EXPORT_HUGO_LASTMOD" time)))))

(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

(use-package eglot :defer t)

(use-package corfu
  :ensure t
  :hook (on-first-buffer . global-corfu-mode))

(use-package nix-mode
  :ensure t
  :defer t)

(use-package scala-mode
  :ensure t
  :interpreter ("scala" . scala-mode)
  :hook
  (scala-mode . eglot-ensure)
  (scala-mode . subword-mode))

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command)
