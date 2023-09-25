;; SPDX-License-Identifier: CC-BY-SA-4.0

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
  :demand t
  :bind
  (:prefix-map rab/files-map
   :prefix "C-c f")
  :bind
  (:prefix-map rab/toggles-map
   :prefix "C-c t"))

(use-package diminish :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(setopt read-quoted-char-radix 16)

(setopt set-mark-command-repeat-pop t)

(use-package simple
  :config
  (setq-default indent-tabs-mode nil))

(use-package simple
  :custom
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t))

(use-package bookmark
  :custom
  (bookmark-save-flag 1))

(use-package selected
  :ensure t
  :diminish
  :config (selected-global-mode)
  :bind (:map selected-keymap
         ("q" . selected-off)
         ("u" . upcase-region)
         ("d" . downcase-region)
         ("w" . count-words-region)
         ("m" . apply-macro-to-region-lines)))

(use-package copilot
  :disabled t
  :ensure t
  :custom
  (copilot-disable-predicates '(always))
  :hook
  (prog-mode . copilot-mode)
  :bind
  ("M-`" . copilot-complete)
  :bind
  (:map rab/toggles-map
   ("`" . copilot-mode))
  :bind
  (:map copilot-completion-map
   ("C-g" .  'copilot-clear-overlay)
   ("M-p" . 'copilot-previous-completion)
   ("M-n" . 'copilot-next-completion)
   ("<tab>" . 'copilot-accept-completion)
   ("M-f" . 'copilot-accept-completion-by-word)
   ("M-<return>" . 'copilot-accept-completion-by-line)))

(use-package corfu
  :ensure t
  :hook (on-first-buffer . global-corfu-mode))

(use-package docker
  :ensure t
  :defer t)

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(setopt confirm-kill-emacs 'yes-or-no-p)

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package hl-line
  :hook (on-first-buffer . global-hl-line-mode))

(use-package ffap
  :hook (on-first-input . ffap-bindings))

(use-package persist-state
  :ensure t
  :hook
  (on-first-input . persist-state-mode))

(use-package suggest
  :ensure t)

(use-package ws-butler
  :ensure t
  :hook (on-first-buffer . ws-butler-global-mode)
  :diminish)

(setopt create-lockfiles nil)

(use-package autorevert
  :diminish auto-revert-mode
  :hook (on-first-buffer . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

(use-package recentf
  :hook (on-first-file-hook . recentf-mode)
  :bind
  (:map rab/files-map
   ("r" . recentf-open)))

(use-package emacs
  :bind
  ([remap capitalize-word] . capitalize-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap upcase-word] . upcase-dwim))

(use-package titlecase
  :ensure t
  :defer t)

(use-package jinx
  :ensure t
  :hook (on-first-buffer . global-jinx-mode)
  :bind
  ([remap ispell-word] . jinx-correct)
  :bind
  (:map rab/toggles-map
   ("$" . jinx-mode)))

(use-package markdown-mode
  :ensure t)

(use-package org
  :custom
  (org-time-stamp-formats '("%Y-%m-%d %a" . "%Y-%m-%d %a %H:%M %Z")))

(use-package ox-hugo
  :ensure t
  :after org)

(use-package rab-ox-hugo
  :after ox-hugo
  :config
  (rab/ox-hugo-mode))

(use-package ox-slack
  :ensure t
  :after org
  :bind
  (:map org-mode-map
   :prefix-map rab/org-mode-map
   :prefix "C-c m"
   ("w" . org-slack-export-to-clipboard-as-slack)))

(use-package subword
  :defer t
  :diminish)

(bind-key [remap count-words-region] 'count-words)

(use-package saveplace
  :hook (on-first-buffer . save-place-mode))

(use-package rfc-mode
  :ensure t
  :defer t)

(use-package envrc
  :ensure t
  :hook (on-first-file . envrc-global-mode))

(use-package csharp-mode
  :mode ((rx ".cs" eos) . 'csharp-ts-mode)
  :hook (csharp-ts-mode . subword-mode))

(use-package rab-lisp
  :after lisp-mode
  :config
  (advice-add #'calculate-lisp-indent :override #'rab/lisp-calculate-indent))

(use-package comp
  :custom
  (native-comp-async-report-warnings-errors 'silent))

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

(use-package esxml
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg))

(use-package git-modes
  :defer t
  :ensure t)

(use-package magit
  :ensure t
  :defer 1
  :functions rab/magit-clone-read-args-a
  :bind
  (:prefix-map rab/git-map
   :prefix "C-c g"
   ("g" . magit-status)
   ("c" . magit-clone))
  :custom
  (magit-clone-default-directory "~/src/")
  (magit-no-message (list "Turning on magit-auto-revert-mode..."))
  (magit-save-repository-buffers 'dontask)
  :config
  (defun rab/magit-clone-read-args-a (orig-fun &rest args)
    "Sets `vertico-preselect' to `prompt' when cloning repos, so we
clone to the default prompted directory, and not some random
existing directory under `magit-clone-default-directory'."
    (let ((vertico-preselect 'prompt))
      (apply orig-fun args)))
  (advice-add 'magit-clone-read-args :around #'rab/magit-clone-read-args-a))

(defun rab/git-add-to-blame-ignore-revs (revision)
  "Adds COMMIT to .git-blame-ignore-revs.  Runs `git rev parse' on COMMIT to resolve it first."
  (interactive (list (magit-read-other-branch-or-commit "Blame-ignored revision")))
  (when-let ((file (expand-file-name ".git-blame-ignore-revs" (project-root (project-current))))
             (parsed (magit-rev-parse revision)))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (end-of-buffer)
        (insert parsed "\n")
        (save-buffer)))))

(use-package git-link
  :ensure t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage))

(use-package git-related
  :bind
  (:map rab/files-map
   ("g" . git-related-find-file)))

(use-package restclient
  :ensure t
  :defer t)

(use-package treesit-auto
  :ensure t
  :demand t
  :config
  (global-treesit-auto-mode))

(use-package uuidgen
  :ensure t
  :defer t)

(use-package xref
  :defer
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package verb
  :after org
  :ensure t
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package dictionary
  :bind
  ("M-#" . dictionary-lookup-definition))

(use-package dictionary
  :if (memq window-system '(mac ns x))
  :custom
  (dictionary-server "dict.org"))

(use-package eglot :defer t)

(use-package htmlize
  :ensure t
  :after ox-html)

(use-package dired
  :defer
  :custom
  (dired-auto-revert-buffer t))

(setopt frame-inhibit-implied-resize t)

(setopt cursor-type 'bar)
(use-package frame
  :config
  (blink-cursor-mode -1))

(use-package simple
  :hook
  (on-first-buffer . column-number-mode))

(defun rab/mode-line-binary-size-indication ()
  "Replaces the size indication in the mode line with base 1024 units."
  (require 'cl-seq)
  (setopt mode-line-position
	  (cl-subst-if
	   '(size-indication-mode
	     (8 " of " (:eval (file-size-human-readable (buffer-size) 'iec "" "B"))))
	   (lambda (x) (and (listp x) (eq 'size-indication-mode (car x))))
	   mode-line-position)))
(add-hook 'on-first-buffer-hook #'rab/mode-line-binary-size-indication)
(add-hook 'on-first-buffer-hook #'size-indication-mode)

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package frame
  :bind
  ("C-z" . nil))

(use-package mode-line-bell
  :ensure
  :hook (on-first-input . mode-line-bell-mode))

(use-package fontaine
  :ensure t
  :demand t
  :bind
  (:map rab/toggles-map
   ("p" . rab/presentation-mode))
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
  (fontaine-set-preset (or fontaine-current-preset 'regular))
  (define-minor-mode rab/presentation-mode
    "Toggles global rab/presentation-mode."
    nil
    :global t
    (if rab/presentation-mode
        (fontaine-set-preset 'presentation)
      (fontaine-set-preset 'regular))))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi :no-confirm))

(use-package "startup"
  :custom
  (inhibit-startup-screen t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message nil))

(use-package marginalia
  :ensure t
  :after vertico
  :bind
  (:map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap recentf-open] . consult-recent-file)
  ([remap yank] . nil)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ("M-g m" . consult-mark)
  ("M-g M" . consult-global-mark)
  ("M-g o" . consult-outline)
  ("M-g i" . consult-imenu)
  ("M-g I" . consult-imenu-multi)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)
  ("M-s r" . consult-ripgrep)
  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ([remap repeat-complex-command] . consult-complex-command)
  ("M-s e" . consult-isearch-history)
  ([remap isearch-edit-string] . consult-isearch-history)
  ([remap next-matching-history-element] . consult-history)
  ([remap previous-matching-history-element] . consult-history)
  ([remap Info-search] . consult-info)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref))

(setopt use-dialog-box nil)

(use-package pixel-scroll
  :hook
  (on-first-buffer . pixel-scroll-precision-mode))

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
   ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-commands '((git-related-find-file (vertico-sort-function . nil))))
  :config
  (vertico-multiform-mode))

(use-package zoom
  :ensure t
  :custom
  `(zoom-size ,(let ((phi (- (/ (+ 1 (sqrt 5)) 2) 1)))
                (cons phi phi))))

(use-package which-key
  :ensure t
  :hook (on-first-input . which-key-mode)
  :diminish
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-9)
  :config
  (push `((nil . ,(rx bos "rab/" (group (1+ any)) "-map" eos)) .
          (nil . ,(rx (backref 1))))
        which-key-replacement-alist))

(use-package help
  :config
  (dolist (key (where-is-internal 'help-for-help))
    (unbind-key key)))

(put 'narrow-to-region 'disabled nil)
