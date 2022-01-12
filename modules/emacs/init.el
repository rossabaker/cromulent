;;; init.el -- Ross A. Baker's Emacs Configuration

;;; Commentary:

;; Tastes great with ./default.nix.

;;; Code:

(eval-when-compile
  (defun ross/use-package-ensure-already-installed
      (name _ensure state &optional _context)
    "Value for `use-package-ensure-function` that assumes the package
is already installed.  This is true in our Nix environment."
    (let ((autoloads-file-name (format "%s-autoloads" name)))
      (with-demoted-errors "Error loading autoloads: %s"
	(load autoloads-file-name t t))))
  (setq use-package-hook-name-suffix nil
	use-package-ensure-function #'ross/use-package-ensure-already-installed
	)
  (require 'use-package))

;;; Benchmark init

(use-package benchmark-init
  :ensure
  :demand
  :config
  (require 'benchmark-init-modes)	; explicitly required
  :hook
  (after-init-hook . benchmark-init/deactivate))

;;; State

(defconst ross/is-mac (eq system-type 'darwin))

;;; Things to run early

(use-package emacs
  :no-require
  :custom
  ;; These are duplicated with the git config in Nix...
  (user-full-name "Ross A. Baker")
  (user-mail-address "ross@rossabaker.com"))

(use-package no-littering
  :ensure
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
	no-littering-var-directory "~/.cache/emacs/var/"))

(use-package exec-path-from-shell
  :ensure
  :if ross/is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package delight
  :ensure)

;;; Core

(use-package emacs
  :no-require
  :custom
  (create-lockfiles nil)
  (echo-keystrokes 0.01)
  (load-prefer-newer t)
  :config
  (defun ross/show-trailing-whitespace ()
    (setq show-trailing-whitespace t))
  (defun ross/scratch ()
    "Pop to the scratch buffer, receating it if necessary."
    (interactive)
    (pop-to-buffer (get-buffer-create "*scratch*")))
  :hook
  ((prog-mode-hook text-mode-hook conf-mode-hook) . ross/show-trailing-whitespace)
  :bind
  ("C-c b x" . ross/scratch))

(use-package advice
  :custom
  (ad-redefinition-action 'accept))

(use-package auth-source
  :custom
  ;; .gpg suffix encrypts it.  The default uses plaintext ~/.authinfo.  Yuck!
  (auth-sources (list (expand-file-name "authinfo.gpg" no-littering-etc-directory))))

(use-package autorevert
  :delight auto-revert-mode)

(use-package comint
  :custom
  (comint-prompt-read-only t))

(use-package compile
  :custom
  (compilation-always-kill t))

(use-package cus-edit
  :custom
  (custom-file null-device))

(use-package delsel
  :config
  (delete-selection-mode))

(use-package editorconfig
  :ensure
  :delight
  :custom
  (editorconfig-trim-whitespaces-mode 'ws-butler-mode)
  :config
  (require 'editorconfig-core)		;unclear why this isn't happening for free
  (editorconfig-mode))

(use-package envrc
  :delight
  :ensure
  :config
  (envrc-global-mode))

(use-package fill-sentences-correctly
  ;; I'm a double spacer.  It works better with abbreviations.  It
  ;; looks better in monospace.  It collapses by default in most
  ;; proportional contexts.  And the rest of the world just doesn't
  ;; get it.  Ergo...
  :ensure
  :delight
  :config
  (fill-sentences-correctly-mode))

(use-package ffap
  :init
  (setq ffap-machine-p-known 'reject)	;default slows load and sounds terrifying
  )

(use-package files
  :custom
  (confirm-kill-emacs 'yes-or-no-p))

(use-package gcmh
  :ensure
  :delight
  :hook
  (after-init-hook . gcmh-mode))

(use-package hydra
  :ensure)

(use-package minibuf-eldef
  :config
  (minibuffer-electric-default-mode))

(use-package mouse
  :custom
  (mouse-yank-at-point t))

(use-package recentf
  :config
  (recentf-mode))

(use-package savehist
  :config
  (savehist-mode))

(use-package simple
  :requires cl-lib
  :config
  (defun ross/ad-keyboard-escape-quit (fun &rest args)
    (cl-letf (((symbol-function 'one-window-p) (lambda (&rest _) t)))
      (apply fun args)))
  (advice-add 'keyboard-escape-quit :around #'ross/ad-keyboard-escape-quit)
  :bind
  ("<escape>" . keyboard-escape-quit))

(use-package string-inflection
  ;; This deserves some keybindings
  :ensure)

(use-package subword
  :delight)

(use-package unmodified-buffer
  :ensure
  :config
  (unmodified-buffer-mode))

(use-package wgrep
  :ensure)

(use-package whole-line-or-region
  :ensure
  :delight whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode))

(use-package ws-butler
  :ensure
  :delight
  :hook
  ((prog-mode-hook text-mode-hook conf-mode-hook) . ws-butler-mode))

;;; UI

(use-package emacs
  :no-require
  :custom
  (cursor-type 'bar)
  (fast-but-imprecise-scrolling t)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (scroll-conservatively 101)
  (scroll-margin 2)
  (scroll-preserve-screen-position t)
  (use-dialog-box nil)
  (visible-bell nil)
  (window-combination-resize t)
  :custom-face
  (default ((t :height 140))))

(use-package ansi-color
  :custom
  (ansi-color-for-comint-mode t))

(use-package default-text-scale
  :ensure
  :config
  (default-text-scale-mode))

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode-hook conf-mode-hook) . display-line-numbers-mode))

(use-package faces
  :custom
  (ring-bell-function 'ross/flash-mode-line)
  :config
  (defun ross/flash-mode-line ()
    (let ((old-fg (face-foreground 'mode-line)))
      (set-face-foreground 'mode-line "red")
      (run-with-idle-timer 0.1 nil
			   (lambda (fg) (set-face-foreground 'mode-line fg))
			   old-fg))))

(use-package frame
  :config
  (blink-cursor-mode -1)
  :bind
  ("C-z" . nil)				; Previously suspend-frame
  )

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package hl-todo
  :ensure
  :hook
  ((prog-mode-hook conf-mode-hook) . hl-todo-mode))

(use-package minibuf
  :no-require
  :custom
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(use-package modus-themes
  :ensure
  :demand t
  :init
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi)
  :bind
  ("C-c T t" . modus-themes-toggle))

(use-package paren
  :custom
  (show-paren-delay 0))

(use-package projectile
  :ensure
  :custom
  (projectile-project-search-path '("~/src"))
  :config
  (projectile-mode)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :hook
  (after-init-hook . projectile-discover-projects-in-search-path))

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

(use-package tooltip
  :config
  (tooltip-mode -1))

;;; Completion

(use-package consult
  :ensure
  :custom
  (consult-narrow-key (kbd "C-+"))
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :bind
  ([remap switch-to-buffer] . consult-buffer)
  ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
  ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
  ([remap yank-pop] . consult-yank-pop)
  ([remap goto-line] . consult-goto-line)
  ([remap repeat-complex-command] . consult-history)
  ([remap apropros] . consult-apropos)
  ([remap man] . consult-man)
  ([remap isearch-edit-string] . consult-isearch-history))

(use-package embark
  :ensure
  :custom
  (prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ([remap describe-bindings] . embark-bindings))

(use-package embark-consult
  :ensure
  :after (embark consult)
  :demand t)

(use-package marginalia
  :ensure
  :config
  (marginalia-mode))

(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless)))

(use-package vertico
  :ensure
  :config
  (vertico-mode)
  (use-package vertico-directory
    :ensure
    :bind (:map vertico-map
		("RET" . vertico-directory-enter)
		;; I don't like vertico-directory-delete-char
		("M-DEL" . vertico-directory-delete-word))
    ;; I don't know what this does, but it's recommended
    :hook
    (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))
  (use-package vertico-indexed
    :ensure
    :config
    (vertico-indexed-mode)))

(use-package which-key
  :ensure
  :delight
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode))

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

(use-package flycheck
  :ensure
  :custom
  (flycheck-emacs-lisp-initialize-packages nil)
  :config
  (global-flycheck-mode))

(use-package magit
  :ensure)

(use-package restclient
  :ensure)

(use-package smerge-mode
  :config
  (defhydra ross/hydra-smerge
    ;; Credit: https://github.com/kaushalmodi/.emacs.d
    (:color pink
     :hint nil
     :pre (smerge-mode 1)
     ;; Disable `smerge-mode' when quitting hydra if no merge
     ;; conflicts remain.
     :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))
  :bind
  ("C-c g m" . ross/hydra-smerge/body))

;;; Languages

;;;; Emacs Lisp

(use-package eldoc
  :delight)

;;;; Git

(use-package git-modes
  :ensure)

;;;; Groovy

(use-package groovy-mode
  :ensure
  :hook
  (groovy-mode-hook . subword-mode))

(use-package jenkinsfile-mode
  :ensure)

;;;; JSON

(use-package json-mode
  :ensure
  :mode
  ("/flake\\.lock\\'" . json-mode))

;;;; Markdown

(use-package markdown-mode
  :ensure
  :config
  ;; This function is dedicated to Rob Norris.
  (defun ross/migrate-tut ()
    "Migrate the *.md files in `default-directory` from tut to mdoc."
    (interactive)
    (let ((files (directory-files default-directory nil "\\.md$"))
	  (mstart (make-hash-table :test 'eq)))
      (fileloop-initialize
       files
       (lambda ()
	 (save-excursion
	   (when (re-search-forward "```tut" nil t)
             (puthash (current-buffer) (match-beginning 0) mstart))))
       (lambda ()
	 (save-excursion
	   (goto-char (gethash (current-buffer) mstart (point-min)))
	   (while (re-search-forward "```tut\\(?::book\\)?" nil t)
	     (replace-match "```scala mdoc" nil nil))
	   t)))
      (fileloop-continue))))

;;;; Nix

(use-package nix-mode
  :ensure
  :hook
  (nix-mode-hook . subword-mode))

;;;; Scala

(use-package hocon-mode
  :ensure
  :config
  (flycheck-define-checker ross/hocon-pyhocon
    "A HOCON checker using the pyhocon tool."
    :command ("pyhocon" "-i" source "-o" null-device)
    :error-patterns
    ((error line-start
            "pyparsing.ParseSyntaxException: "
            (message (one-or-more anychar))
            "(line:" line ", col:" column ")"
            line-end))
    :modes (hocon-mode))
  (add-to-list 'flycheck-checkers 'ross/hocon-pyhocon)
  :mode
  ("/application\\.conf\\'" . hocon-mode)
  ("/reference\\.conf\\'" . hocon-mode)
  ("/\\.scala-steward\\.conf\\'" . hocon-mode)
  ("/\\.scalafmt\\.conf\\'" . hocon-mode))

(use-package sbt-mode
  :ensure
  :hook
  (sbt-mode-hook . subword-mode))

(use-package scala-mode
  :ensure
  :hook
  (scala-mode-hook . subword-mode))

;;;; YAML

(use-package yaml-mode
  :ensure)

(provide 'init)
;;; init.el ends here
