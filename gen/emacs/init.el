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
  (:prefix-map ross/files-map
   :prefix "C-c f")
  :bind
  (:prefix-map ross/toggles-map
   :prefix "C-c t")
  :config
  (defun ross/unbind-all (fn)
    "Unbinds a function everywhere."
    (dolist (key (where-is-internal fn nil))
      (unbind-key key))))

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
  :custom
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t))

(use-package copilot
  :ensure t
  :custom
  (copilot-disable-predicates '(always))
  :hook
  (prog-mode . copilot-mode)
  :bind
  ("M-`" . copilot-complete)
  :bind
  (:map ross/toggles-map
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

(setopt confirm-kill-emacs 'yes-or-no-p)

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

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
  (:map ross/files-map
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
  (:map ross/toggles-map
   ("$" . jinx-mode)))

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

(use-package ox-slack
  :ensure t
  :after org)

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

(use-package lisp-mode
  :defer
  :config
  (defun ross/calculate-lisp-indent (&optional parse-start)
    "Add better indentation for quoted and backquoted lists."
    ;; This line because `calculate-lisp-indent-last-sexp` was defined with `defvar`
    ;; with it's value ommited, marking it special and only defining it locally. So
    ;; if you don't have this, you'll get a void variable error.
    (defvar calculate-lisp-indent-last-sexp)
    (save-excursion
      (beginning-of-line)
      (let ((indent-point (point))
            state
            ;; setting this to a number inhibits calling hook
            (desired-indent nil)
            (retry t)
            calculate-lisp-indent-last-sexp containing-sexp)
        (cond ((or (markerp parse-start) (integerp parse-start))
               (goto-char parse-start))
              ((null parse-start) (beginning-of-defun))
              (t (setq state parse-start)))
        (unless state
          ;; Find outermost containing sexp
          (while (< (point) indent-point)
            (setq state (parse-partial-sexp (point) indent-point 0))))
        ;; Find innermost containing sexp
        (while (and retry
                    state
                    (> (elt state 0) 0))
          (setq retry nil)
          (setq calculate-lisp-indent-last-sexp (elt state 2))
          (setq containing-sexp (elt state 1))
          ;; Position following last unclosed open.
          (goto-char (1+ containing-sexp))
          ;; Is there a complete sexp since then?
          (if (and calculate-lisp-indent-last-sexp
                   (> calculate-lisp-indent-last-sexp (point)))
              ;; Yes, but is there a containing sexp after that?
              (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                              indent-point 0)))
                (if (setq retry (car (cdr peek))) (setq state peek)))))
        (if retry
            nil
          ;; Innermost containing sexp found
          (goto-char (1+ containing-sexp))
          (if (not calculate-lisp-indent-last-sexp)
              ;; indent-point immediately follows open paren.
              ;; Don't call hook.
              (setq desired-indent (current-column))
            ;; Find the start of first element of containing sexp.
            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
            (cond ((looking-at "\\s(")
                   ;; First element of containing sexp is a list.
                   ;; Indent under that list.
                   )
                  ((> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp)
                   ;; This is the first line to start within the containing sexp.
                   ;; It's almost certainly a function call.
                   (if (or
                        ;; Containing sexp has nothing before this line
                        ;; except the first element. Indent under that element.
                        (= (point) calculate-lisp-indent-last-sexp)

                        ;; First sexp after `containing-sexp' is a keyword. This
                        ;; condition is more debatable. It's so that I can have
                        ;; unquoted plists in macros. It assumes that you won't
                        ;; make a function whose name is a keyword.
                        (when-let (char-after (char-after (1+ containing-sexp)))
                          (char-equal char-after ?:))

                        ;; Check for quotes or backquotes around.
                        (let* ((positions (elt state 9))
                               (last (car (last positions)))
                               (rest (reverse (butlast positions)))
                               (any-quoted-p nil)
                               (point nil))
                          (or
                           (when-let (char (char-before last))
                             (or (char-equal char ?')
                                 (char-equal char ?`)))
                           (progn
                             (while (and rest (not any-quoted-p))
                               (setq point (pop rest))
                               (setq any-quoted-p
                                     (or
                                      (when-let (char (char-before point))
                                        (or (char-equal char ?')
                                            (char-equal char ?`)))
                                      (save-excursion
                                        (goto-char (1+ point))
                                        (looking-at-p
                                         "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                             any-quoted-p))))
                       ;; Containing sexp has nothing before this line
                       ;; except the first element.  Indent under that element.
                       nil
                     ;; Skip the first element, find start of second (the first
                     ;; argument of the function call) and indent under.
                     (progn (forward-sexp 1)
                            (parse-partial-sexp (point)
                                                calculate-lisp-indent-last-sexp
                                                0 t)))
                   (backward-prefix-chars))
                  (t
                   ;; Indent beneath first sexp on same line as
                   ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                   ;; almost certainly a function call.
                   (goto-char calculate-lisp-indent-last-sexp)
                   (beginning-of-line)
                   (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                       0 t)
                   (backward-prefix-chars)))))
        ;; Point is at the point to indent under unless we are inside a string.
        ;; Call indentation hook except when overridden by lisp-indent-offset
        ;; or if the desired indentation has already been computed.
        (let ((normal-indent (current-column)))
          (cond ((elt state 3)
                 ;; Inside a string, don't change indentation.
                 nil)
                ((and (integerp lisp-indent-offset) containing-sexp)
                 ;; Indent by constant offset
                 (goto-char containing-sexp)
                 (+ (current-column) lisp-indent-offset))
                ;; in this case calculate-lisp-indent-last-sexp is not nil
                (calculate-lisp-indent-last-sexp
                 (or
                  ;; try to align the parameters of a known function
                  (and lisp-indent-function
                       (not retry)
                       (funcall lisp-indent-function indent-point state))
                  ;; If the function has no special alignment
                  ;; or it does not apply to this argument,
                  ;; try to align a constant-symbol under the last
                  ;; preceding constant symbol, if there is such one of
                  ;; the last 2 preceding symbols, in the previous
                  ;; uncommented line.
                  (and (save-excursion
                         (goto-char indent-point)
                         (skip-chars-forward " \t")
                         (looking-at ":"))
                       ;; The last sexp may not be at the indentation
                       ;; where it begins, so find that one, instead.
                       (save-excursion
                         (goto-char calculate-lisp-indent-last-sexp)
                         ;; Handle prefix characters and whitespace
                         ;; following an open paren.  (Bug#1012)
                         (backward-prefix-chars)
                         (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                       (line-beginning-position))
                                         (and containing-sexp
                                              (>= (1+ containing-sexp) (point)))))
                           (forward-sexp -1)
                           (backward-prefix-chars))
                         (setq calculate-lisp-indent-last-sexp (point)))
                       (> calculate-lisp-indent-last-sexp
                          (save-excursion
                            (goto-char (1+ containing-sexp))
                            (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                            (point)))
                       (let ((parse-sexp-ignore-comments t)
                             indent)
                         (goto-char calculate-lisp-indent-last-sexp)
                         (or (and (looking-at ":")
                                  (setq indent (current-column)))
                             (and (< (line-beginning-position)
                                     (prog2 (backward-sexp) (point)))
                                  (looking-at ":")
                                  (setq indent (current-column))))
                         indent))
                  ;; another symbols or constants not preceded by a constant
                  ;; as defined above.
                  normal-indent))
                ;; in this case calculate-lisp-indent-last-sexp is nil
                (desired-indent)
                (t
                 normal-indent))))))
  (advice-add #'calculate-lisp-indent :override #'ross/calculate-lisp-indent))

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

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-force-searcher 'rg))

(use-package magit
  :ensure t
  :defer 1
  :functions ross/magit-clone-read-args-a
  :bind
  (:prefix-map ross/git-map
   :prefix "C-c g"
   ("g" . magit-status)
   ("c" . magit-clone))
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

(use-package eglot :defer t)

(setopt frame-inhibit-implied-resize t)

(setopt cursor-type 'bar)
(use-package frame
  :config
  (blink-cursor-mode -1))

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
  (:map ross/toggles-map
   ("p" . ross/presentation-mode))
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
  (define-minor-mode ross/presentation-mode
    "Toggles global ross/presentation-mode."
    nil
    :global t
    (if ross/presentation-mode
        (fontaine-set-preset 'presentation)
      (fontaine-set-preset 'regular))))

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi :no-confirm))

(use-package "startup"
  :custom
  (inhibit-splash-screen t)
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

(setopt use-dialog-box nil)

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

(use-package which-key
  :ensure t
  :hook (on-first-input . which-key-mode)
  :diminish
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay most-positive-fixnum)
  (which-key-idle-secondary-delay 1e-9)
  :config
  (push `((nil . ,(rx bos "ross/" (group (1+ any)) "-map" eos)) .
          (nil . ,(rx (backref 1))))
        which-key-replacement-alist))

(use-package help
  :config
  (ross/unbind-all 'help-for-help))

(defun ross/refresh-load-path ()
  "Refresh the load path written by home-manager to pick up new
 packages without restarting Emacs."
  (interactive)
  (load-file "~/.config/emacs/load-path.el"))

(use-package markdown-mode
  :ensure t)
