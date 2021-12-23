(eval-when-compile
  (setq use-package-hook-name-suffix nil)
  (require 'use-package))

(use-package no-littering
  :ensure
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	no-littering-var-directory "~/.cache/emacs/var/"))

;;; Core

(use-package emacs
  :no-require
  :custom
  (echo-keystrokes 0.01))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package files
  :custom
  (confirm-kill-emacs 'yes-or-no-p))

(use-package minibuf-eldef
  :config
  (minibuffer-electric-default-mode))

;;; UI

(use-package emacs
  :no-require
  :custom
  (use-dialog-box nil)
  :custom-face
  (default ((t :height 140))))

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode-hook conf-mode-hook) . display-line-numbers-mode))

(use-package frame
  :config
  (blink-cursor-mode -1)
  :bind
  ("C-z" . nil)				; Previously suspend-frame
  )

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package modus-themes
  :ensure
  :init
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi)
  :bind
  ("C-c T t" . modus-themes-toggle))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package simple
  :no-require
  :config
  (column-number-mode)
  (size-indication-mode))

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

(use-package helpful
  :ensure
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap Info-goto-emacs-command-node] . helpful-function)
  ("C-h (" . helpful-macro))

(use-package magit
  :ensure)

;;; Languages

;;;; Nix

(use-package nix-mode
  :ensure
  :hook
  (nix-mode-hook . subword-mode))

;;;; Scala

(use-package hocon-mode
  :mode
  ("/application\\.conf\\'" . hocon-mode)
  ("/reference\\.conf\\'" . hocon-mode))

(use-package sbt-mode
  :ensure
  :hook
  (sbt-mode-hook . subword-mode))

(use-package scala-mode
  :ensure
  :hook
  (scala-mode-hook . subword-mode))
