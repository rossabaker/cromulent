(use-package benchmark-init
  :ensure t
  :demand t
  :hook (after-init . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package gcmh
  :ensure t
  :diminish
  :init (setq gc-cons-threshold (* 80 1024 1024))
  :hook (emacs-startup . gcmh-mode))

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

(use-package bind-key
  :demand t)

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

(use-package simple
  :custom
  (save-interprogram-paste-before-kill t))

(use-package recentf
  :hook (on-first-file-hook . recentf-mode))

(use-package saveplace
  :hook (on-first-buffer . save-place-mode))

(use-package magit
  :ensure t
  :defer 1
  :functions ross/magit-clone-read-args-a
  :bind
  (:prefix-map ross/git-map
   :prefix "C-c g"
   ("g" . magit-status))
  :custom
  (magit-clone-default-directory "~/src/")
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  :config
  (defun ross/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'ross/magit-clone-read-args-a))

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

(use-package rfc-mode
  :ensure t
  :defer t)

(use-package titlecase
  :ensure t
  :defer t)

(use-package ws-butler
  :ensure t
  :diminish
  :config
  (ws-butler-global-mode))

(use-package ox-hugo
  :ensure t
  :after org
  :config
  (defun ross/ox-hugo-update-lastmod ()
    "Updates the EXPORT_HUGO_LAST_MOD property of the nearest element
with EXPORT_FILE_NAME."
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

(use-package vertico
  :ensure t
  :hook (on-first-input . vertico-mode))

(use-package vertico-indexed
  :after vertico
  :config (vertico-indexed-mode))

(use-package vertico-repeat
  :after vertico
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("M-R" . vertico-repeat))

(use-package vertico-directory
  :after vertico
  :bind
  (:map vertico-map
	("RET" . vertico-directory-enter)
	("M-DEL" . vertico-directory-delete-word)))

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

(use-package which-key
  :ensure t
  :hook (on-first-input . which-key-mode)
  :diminish
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-9)
  (let ((ross-map (rx "ross/" (group (1+ (or alnum "-"))) "-map")))
    (push `((nil . ,ross-map) . (nil . (rx (backref 1))))
	    which-key-replacement-alist)))
