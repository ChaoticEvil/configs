;;; init.el --- Peter's Emacs config file
;;
;; Copyright (C) 2015-2019 by Peter Brovchenko <p.brovchenko@protonmail.com>
;;
;; Author: Peter Brovchenko <p.brovchenko@protonmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d/init.el
;; Version: 0.9.0
;;
;;; Commentary:
;;
;; Yet another Emacs customisation :)
;;
;;; Code:

;;;
;;; Common settings
;;;

;; Increase GC threshold to speed up startup.
;; Reset the GC threshold after initialization, and GC whenever we tab out.
(setq gc-cons-threshold (* 64 1000 1000))
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold (* 2 1000 1000))))
(add-hook 'focus-out-hook 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

;; Encoding
(set-language-environment            'UTF-8)
(setq buffer-file-coding-system      'utf-8)
(setq-default coding-system-for-read 'utf-8)
(setq file-name-coding-system        'utf-8)
(set-selection-coding-system         'utf-8)
(set-keyboard-coding-system          'utf-8)
(set-terminal-coding-system          'utf-8)
(prefer-coding-system                'utf-8)

;; Disable autosave
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Create variable for check terminal mode
(defun is-in-terminal()
  (not (display-graphic-p)))

;; Fringes
(if (not (is-in-terminal))
    (fringe-mode '(8 . 0))
  (setq-default indicate-empty-lines t) ;; Gliph indicate for empty line
  (setq-default indicate-buffer-boundaries 'left)) ;; Only left-indication

;; Disable scrollbar for X version
(if (not (is-in-terminal))
    (scroll-bar-mode   -1))

;; Print supplement info in modeline
(display-time-mode             t) ;; Time
(setq display-time-24hr-format t) ;; Time format
(size-indication-mode          t) ;; Show filesize in % (percent)

;; Words wrap
(setq word-wrap          t) ;; Wrap by word
(global-visual-line-mode t)

;; Syntax highlight
(require 'font-lock)
(setq font-lock-maximum-decoration t)

;; Indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80
              c-basic-offset 4        ;; Indentation level for C
              c-default-style "k&r"   ;; C-specific style
              indicate-empty-lines t) ;; Highlight end of buffer?
(global-set-key (kbd "RET") 'newline-and-indent) ;; Autoindent for newline
(setq lisp-indent-function  'common-lisp-indent-function) ;; Enable indent for lisp code

;; Scroll
(setq scroll-step               1) ;; Scroll by one line
(setq scroll-margin             5) ;; Set top and bottom scroll margin as a five lines
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq select-enable-clipboard t)

;; Search results highlight
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Enable for delete selected text region
(delete-selection-mode t)

;; Short answers (yes->y, no->n)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove trailing whitespace before save buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;
;;; Look and Feel settings
;;;

(electric-pair-mode 1) ;; Autocompare scopes
(show-paren-mode t)    ;; Enable highlight for scopes
(setq show-paren-style 'expression)
(tooltip-mode      -1) ;; Disable tooltip
(menu-bar-mode     -1) ;; Disable menubar
(tool-bar-mode     -1) ;; Disable toolbar

(setq use-dialog-box nil                  ;; No gui dialogs. Only minibuffer.
      ring-bell-function 'ignore          ;; Disable bell sound
      inhibit-startup-message t           ;; Disable startup message
      inhibit-splash-screen   t           ;; Disable splash-screen
      initial-scratch-message ""          ;; Empty string into *scratch* buffer
      confirm-kill-emacs nil              ;; Always confirm before closing Emacs?
      show-trailing-whitespace t          ;; Display trailing whitespace.
      frame-title-format "GNU Emacs: %b") ;; Set window title as 'GNU Emacs: <filename>'

;; Set font only if we're not in the terminal.
(when (display-graphic-p)
  ;; Function for checking font existence.
  (defun font-exists-p (font)
    "Check if FONT exists."
    (if (null (x-list-fonts font)) nil t))
  (declare-function font-exists-p "init.el")

  ;; Set font.
  (cond
    ((font-exists-p "Iosevka")
     (set-face-attribute
      'default nil :font "Iosevka:weight=Regular" :height 190)
     (setq-default line-spacing 0))))

;; Highlight current line
(global-hl-line-mode nil)

;;;
;;; Shortcusts settings
;;;

;; Move cursor up
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)

;; Move cursor down
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'next-line)

;; Move cursor left
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'backward-char)

;; Move cursor right
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'forward-char)

;; Move page up
(global-unset-key (kbd "M-I"))
(global-set-key (kbd "M-I") 'scroll-down-command)

;; Move page down
(global-unset-key (kbd "M-K"))
(global-set-key (kbd "M-K") 'scroll-up-command)

;; Forward word
(global-unset-key (kbd "M-L"))
(global-set-key (kbd "M-L") 'forward-word)

;; Backward word
(global-unset-key (kbd "M-J"))
(global-set-key (kbd "M-J") 'backward-word)

;; Beginnning of line
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'move-beginning-of-line)

;; End of line
(global-unset-key (kbd "M-H"))
(global-set-key (kbd "M-H") 'move-end-of-line)

;; Beginning of buffer
(global-unset-key (kbd "M-n"))
(global-set-key (kbd "M-n") 'beginning-of-buffer)

;; End of buffer
(global-unset-key (kbd "M-N"))
(global-set-key (kbd "M-N") 'end-of-buffer)

;; Move cursor in next window
(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") 'other-window)

;;
;; Editing
;;

;; Delete
(global-unset-key (kbd "M-f"))
(global-set-key (kbd "M-f") 'delete-forward-char)

;; Backspace
(global-unset-key (kbd "M-d"))
(global-set-key (kbd "M-d") 'delete-backward-char)

;; Delete word
(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'kill-word)

;; Delete word backward
(global-unset-key (kbd "M-e"))
(global-set-key (kbd "M-e") 'backward-kill-word)

;; Enter
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'reindent-then-newline-and-indent)

;; Select region
(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Copy selected region
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'kill-ring-save)

;; Cut selected region
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'kill-region)

;; Past selected region
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v") 'yank)

;; Undo
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'undo)

;; Redo
(global-unset-key (kbd "M-Z"))
(global-set-key (kbd "M-Z") 'undo-only)

;; Open file
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)

;; Comment or uncomment selected region
(global-unset-key (kbd "M-'"))
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)

;; Revert buffer
(global-unset-key (kbd "C-M-r"))
(global-set-key (kbd "C-M-r") 'revert-buffer)

;; Goto line
(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<f6>") 'goto-line)

;; Change Meta for OS X
(cond
  ((string-equal system-type "darwin") ; Mac OS X
   (progn
     (setq mac-option-key-is-meta nil)
     (setq mac-command-key-is-meta t)
     (setq mac-command-modifier 'meta)
     (setq mac-option-modifier nil))))

;;;
;;; Biltin packages settings
;;;

;; IDO
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;; Linum
(require 'linum)
(line-number-mode   t)    ;; Show current line number in modeline
(global-linum-mode  t)    ;; Show lines numbers in all buffers
(column-number-mode t)    ;; Show column number in modeline
;; Set format for line numbers
(if (not (is-in-terminal))
    (setq linum-format " %d")
  (setq linum-format " %d "))

;; Org-mode
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED")))
(setq org-src-fontify-natively 't)
;;(define-key org-mode-map (kbd "M-e") nil)
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (C . t)
                             (shell . t)
                             (perl . t)
                             (python . t)))

;; Execute Emacs command
(global-unset-key (kbd "<f5>"))
(global-set-key (kbd "<f5>") 'execute-extended-command)

;; List of buffers (with *scratch)
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'bs-show)

;; Folding
(require 'hideshow)
(defvar hs-special-modes-alist
  (mapcar 'purecopy
          '((c-mode "{" "}" "/[*/]" nil nil)
            (c++-mode "{" "}" "/[*/]" nil nil)
            (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
            (java-mode "{" "}" "/[*/]" nil nil)
            (scala-mode "{" "}" "/[*/]" nil nil)
            (cperl-mode "{" "}" "/[*/]" nil nil)
            (js-mode "{" "}" "/[*/]" nil)
            (js2-mode "{" "}" "/[*/]" nil nil)
            (json-mode "{" "}" "/[*/]" nil nil))))
(global-set-key (kbd "M-<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)

;; Bookmarks
(global-set-key (kbd "C-b") 'bookmark-set)
(global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

;;
;; C
;;

(setq-default c-basic-offset 4) ;; Set indention for C-mode

;;
;; Perl
;;

;; Auto associate with cperl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\|t\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(setq cperl-invalid-face nil)    ;; disable errors
(setq cperl-electric-keywords t) ;; expands for keywords such as foreach, while, etc...

;; Autocomplete pairs
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; Indentation
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

(add-hook 'cperl-mode-hook
          (lambda()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; Finding perl modules paths
(defun find-perl-module (module-name)
  (interactive "sPerl module name: ")
  (let ((path (perl-module-path module-name)))
    (if path
        (find-file path)
      (error "Module '%s' not found" module-name))))

;;;
;;; Third-party packages settings
;;;

(require 'package)
;; Explicitly enable packages.
(setq package-enable-at-startup nil)
;; Add package sources.
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; Run auto-load functions specified by package authors.
(package-initialize)

;; Require use-package.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Set theme (Nimbus)
(use-package nimbus-theme
    :ensure t)

;; Snippets
(use-package yasnippet
    :ensure t
    :init
    (yas-global-mode t))

;; Company mode for total auto-completion.
(use-package company
    :diminish company-mode
    :bind ("M-/" . company-complete)
    :hook (after-init . global-company-mode)
    ;; :config (setq company-backends (remove 'company-ropemacs company-backends)
    ;;               company-tooltip-limit 20 company-tooltip-align-annotations t)
    :init (setq company-idle-delay nil
                ;; Align tooltips to right border.
                company-tooltip-align-annotations t)
    (global-company-mode 1))

(use-package company
    :ensure t
    :diminish
    :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay t)

    (use-package company-irony
        :ensure t
        :config
        (add-to-list 'company-backends 'company-irony))

    (use-package company-anaconda
        :ensure t
        :config
        (add-to-list 'company-backends 'company-anaconda)))

;; Python anaconda
(use-package anaconda-mode
    :ensure t
    :config
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; C
(use-package irony
    :ensure t
    :hook (c-mode . irony-mode))

(use-package flycheck-irony
    :ensure t
    :hook (flycheck-mode . flycheck-irony-setup))

(use-package rainbow-delimiters
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; brew install the_silver_searcher
(use-package dumb-jump
    :ensure t
    :diminish dumb-jump-mode
    :bind (("C-M-g" . dumb-jump-go)
           ("C-M-p" . dumb-jump-back)
           ("C-M-q" . dumb-jump-quick-look))
    :config (setq dumb-jump-force-searcher 'rg))

;; Magit
(use-package magit
    :diminish auto-revert-mode
    :bind (("<f6>" . magit-status))
    :init (setq
           ;; Show fine differences for all displayed diff hunks.
           magit-diff-refine-hunk `all
           magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
           ;; Don't ask before saving repository buffers.
           magit-save-repository-buffers 'dontask))

;; Web-mode
(use-package web-mode
    :mode (("\\.html$"        . web-mode)
           ("\\.phtml\\'"     . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[agj]sp\\'"   . web-mode)
           ("\\.as[cp]x\\'"   . web-mode)
           ("\\.erb\\'"       . web-mode)
           ("\\.mustache\\'"  . web-mode)
           ("\\.djhtml\\'"    . web-mode)
           ("\\.ejs\\'"       . web-mode)
           ("\\.html?\\'"     . web-mode)
           ("\\.js?\\'"       . web-mode)
           ("\\.jsx?\\'"      . web-mode)
           ("\\.css?\\'"      . web-mode)
           ("\\.scss?\\'"     . web-mode)
           ("\\.ep?\\'"       . web-mode)
           ("\\.vbhtml?\\'"   . web-mode)
           ("\\.jinja\\'"     . web-mode))
    :config (setq web-mode-markup-indent-offset 4
                  web-mode-code-indent-offset 4
                  web-mode-css-indent-offset 4
                  js-indent-level 4
                  web-mode-enable-auto-pairing t
                  web-mode-enable-auto-expanding t
                  web-mode-enable-css-colorization t
                  web-mode-engines-alist '(("underscore" . "\\.html\\'")))
    (add-hook 'web-mode-hook 'electric-pair-mode))

;; Lua
(use-package lua-mode
    :mode "\\.lua\\'"
    :config (setq lua-indent-level 4))

;; Markdowm
(use-package markdown-mode
    :mode "\\.md\\'")

;; Pomidor
(use-package pomidor
    :config (setq pomidor-sound-tick nil
                  pomidor-sound-tack nil))
;; Crux
(use-package crux
    :bind (("M-h" . crux-move-beginning-of-line)
           ("C-k" . crux-kill-whole-line)
           ("C-j" . crux-top-join-line)))

;; Smart region selection
(use-package expand-region
    :ensure t
    :bind ("M--". 'er/expand-region))

;; Hightlight word under cursor
(use-package highlight-symbol
    :ensure t
    :bind (("M-8" . 'highlight-symbol-next)
           ("M-7" . 'highlight-symbol-prev)
           ("M-9" . 'highlight-symbol-query-replace)
           ("M-0" . 'highlight-symbol-mode))
    :config (setq highlight-symbol-on-navigation-p t))

;; restclient - http client
(use-package restclient
    :ensure t)

;; Syntax check
(use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook 'global-flycheck-mode)
    ;;(add-hook 'flycheck-mode-hook 'jc/use-eslint-from-node-modules)
    ;;(add-to-list 'flycheck-checkers 'proselint)
    (setq-default flycheck-highlighting-mode 'lines)
    ;; Define fringe indicator / warning levels
    (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00011100
                #b00111110
                #b00111110
                #b00111110
                #b00011100
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-ball
      :fringe-face 'flycheck-fringe-info))

;; Org-mode billets
(use-package org-bullets
    :ensure t
    :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;
;; Scala
;;

(use-package ensime
    :ensure t
    :config
    (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package scala-mode
    :ensure t
    :interpreter
    ("scala" . scala-mode)
    :config
    (setq scala-indent:use-javadoc-style t))

(use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map))

(setq exec-path (append exec-path (list "/usr/share/scala/bin" ))) ;; Set location of scala bin

;; Mutt support.
(setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

;;; perlcritic.el --- minor mode for Perl::Critic integration

;;; Customization and variables.
(defgroup perlcritic nil "Perl::Critic"
          :prefix "perlcritic-"
          :group 'tools)

(defcustom perlcritic-bin "perlcritic"
  "The perlcritic program used by `perlcritic'."
  :type 'string
  :group 'perlcritic)

(defcustom perlcritic-pass-required nil
  "When \\[perlcritic-mode] is enabled then this boolean controls
whether your file can be saved when there are perlcritic warnings.

This variable is automatically buffer-local and may be overridden on a
per-file basis with File Variables."
  :type '(radio
	  (const :tag "Require no warnings from perlcritic to save" t)
	  (const :tag "Allow warnings from perlcritic when saving" nil))
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-pass-required)

(defcustom perlcritic-profile nil
  "Specify an alternate .perlcriticrc file. This is only used if
non-nil."
  :type '(string)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-profile)

(defcustom perlcritic-noprofile nil
  "Disables the use of any .perlcriticrc file."
  :type '(boolean)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-noprofile)

(defcustom perlcritic-severity nil
  "Directs perlcritic to only report violations of Policies with a
severity greater than N. Severity values are integers ranging from
1 (least severe) to 5 (most severe). The default is 5. For a given
-profile, decreasing the -severity will usually produce more
violations.  Users can redefine the severity for any Policy in their
.perlcriticrc file.

This variable is automatically buffer-local and may be overridden on a
per-file basis with File Variables."
  :type '(radio
	  (const :tag "Show only the most severe: 5" 5)
	  (const :tag "4" 4)
	  (const :tag "3" 3)
	  (const :tag "2" 2)
	  (const :tag "Show everything including the least severe: 1" 1)
	  (const :tag "Default from .perlcriticrc" nil))
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-severity)

(defcustom perlcritic-top nil
  "Directs \"perlcritic\" to report only the top N Policy violations in
each file, ranked by their severity. If the -severity option is not
explicitly given, the -top option implies that the minimum severity
level is 1. Users can redefine the severity for any Policy in their
.perlcriticrc file.

This variable is automatically buffer-local and may be overridden on a
per-file basis with File Variables."
  :type '(integer)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-top)

(defcustom perlcritic-include nil
  "Directs \"perlcritic\" to apply additional Policies that match the regex \"/PATTERN/imx\".
Use this option to override your profile and/or the severity settings.

For example:

  layout

This would cause \"perlcritic\" to apply all the \"CodeLayout::*\" policies
even if they have a severity level that is less than the default level of 5,
or have been disabled in your .perlcriticrc file.  You can specify multiple
`perlcritic-include' options and you can use it in conjunction with the
`perlcritic-exclude' option.  Note that `perlcritic-exclude' takes precedence
over `perlcritic-include' when a Policy matches both patterns.  You can set
the default value for this option in your .perlcriticrc file."
  :type '(string)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-include)

(defcustom perlcritic-exclude nil
  "Directs \"perlcritic\" to not apply any Policy that matches the regex
\"/PATTERN/imx\".  Use this option to temporarily override your profile and/or
the severity settings at the command-line.  For example:

  strict

This would cause \"perlcritic\" to not apply the \"RequireUseStrict\" and
\"ProhibitNoStrict\" Policies even though they have the highest severity
level.  You can specify multiple `perlcritic-exclude' options and you can use
it in conjunction with the `perlcritic-include' option.  Note that
`perlcritic-exclude' takes precedence over `perlcritic-include' when a Policy
matches both patterns.  You can set the default value for this option in your
.perlcriticrc file."
  :type '(string)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-exclude)


(defcustom perlcritic-force nil
  "Directs \"perlcritic\" to ignore the magical \"## no critic\"
pseudo-pragmas in the source code. You can set the default value for this
option in your .perlcriticrc file."
  :type '(boolean)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-force)

(defcustom perlcritic-verbose nil
  "Sets the numeric verbosity level or format for reporting violations. If
given a number (\"N\"), \"perlcritic\" reports violations using one of the
predefined formats described below. If the `perlcritic-verbose' option is not
specified, it defaults to either 4 or 5, depending on whether multiple files
were given as arguments to \"perlcritic\".  You can set the default value for
this option in your .perlcriticrc file.

Verbosity     Format Specification
-----------   -------------------------------------------------------------
 1            \"%f:%l:%c:%m\n\",
 2            \"%f: (%l:%c) %m\n\",
 3            \"%m at %f line %l\n\",
 4            \"%m at line %l, column %c.  %e.  (Severity: %s)\n\",
 5            \"%f: %m at line %l, column %c.  %e.  (Severity: %s)\n\",
 6            \"%m at line %l, near â€™%râ€™.  (Severity: %s)\n\",
 7            \"%f: %m at line %l near â€™%râ€™.  (Severity: %s)\n\",
 8            \"[%p] %m at line %l, column %c.  (Severity: %s)\n\",
 9            \"[%p] %m at line %l, near â€™%râ€™.  (Severity: %s)\n\",
10            \"%m at line %l, column %c.\n  %p (Severity: %s)\n%d\n\",
11            \"%m at line %l, near â€™%râ€™.\n  %p (Severity: %s)\n%d\n\"

Formats are a combination of literal and escape characters similar to the way
\"sprintf\" works.  See String::Format for a full explanation of the
formatting capabilities.  Valid escape characters are:

Escape    Meaning
-------   ----------------------------------------------------------------
%c        Column number where the violation occurred
%d        Full diagnostic discussion of the violation
%e        Explanation of violation or page numbers in PBP
%F        Just the name of the file where the violation occurred.
%f        Path to the file where the violation occurred.
%l        Line number where the violation occurred
%m        Brief description of the violation
%P        Full name of the Policy module that created the violation
%p        Name of the Policy without the Perl::Critic::Policy:: prefix
%r        The string of source code that caused the violation
%s        The severity level of the violation

The purpose of these formats is to provide some compatibility with text
editors that have an interface for parsing certain kinds of input.


This variable is automatically buffer-local and may be overridden on a
per-file basis with File Variables."
  :type '(integer)
  :group 'perlcritic)
(make-variable-buffer-local 'perlcritic-verbose)

;; TODO: Enable strings in perlcritic-verbose.
;; (defcustom perlcritic-verbose-regexp nil
;;   "An optional  regexp to match the warning output.
;;
;; This is used when `perlcritic-verbose' has a regexp instead of one of
;; the standard verbose levels.")
;; (make-local-variable 'perlcritic-verbose-regexp)


;; compile.el requires that something be the "filename." I've tagged
;; the severity with that. It happens to make it get highlighted in
;; red. The following advice on COMPILATION-FIND-FILE makes sure that
;; the "filename" is getting ignored when perlcritic is using it.

;; These patterns are defined in Perl::Critic::Utils

(defvar perlcritic-error-error-regexp-alist nil
  "Alist that specified how to match errors in perlcritic output.")
(setq perlcritic-error-error-regexp-alist
      '(;; Verbose level 1
        ;;  "%f:%l:%c:%m\n"
        ("^\\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3 1)

        ;; Verbose level 2
        ;;  "%f: (%l:%c) %m\n"
        ("^\\([^\n]+\\): (\\([0-9]+\\):\\([0-9]+\\))" 1 2 3 1)

        ;; Verbose level 3
        ;;   "%m at %f line %l\n"
        ("^[^\n]+ at \\([^\n]+\\) line \\([0-9]+\\)" 1 2 nil 1)
        ;;   "%m at line %l, column %c.  %e.  (Severity: %s)\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\), column \\([0-9]+\\)." nil 2 3 1)

        ;; Verbose level 4
        ;;   "%m at line %l, column %c.  %e.  (Severity: %s)\n"
        ("^[^\n]+\\( \\)at line \\([0-9]+\\), column \\([0-9]+\\)" nil 2 3)
        ;;   "%f: %m at line %l, column %c.  %e.  (Severity: %s)\n"
        ("^\\([^\n]+\\): [^\n]+ at line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)

        ;; Verbose level 5
        ;;    "%m at line %l, near '%r'.  (Severity: %s)\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\)," nil 2)
        ;;    "%f: %m at line %l, column %c.  %e.  (Severity: %s)\n"
        ("^\\([^\n]+\\): [^\n]+ at line \\([0-9]+\\), column \\([0-9]+\\)" 1 2 3)

        ;; Verbose level 6
        ;;    "%m at line %l, near '%r'.  (Severity: %s)\\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\)" nil 2)
        ;;    "%f: %m at line %l near '%r'.  (Severity: %s)\n"
        ("^\\([^\n]+\\): [^\n]+ at line \\([0-9]+\\)" 1 2)

        ;; Verbose level 7
        ;;    "%f: %m at line %l near '%r'.  (Severity: %s)\n"
        ("^\\([^\n]+\\): [^\n]+ at line \\([0-9]+\\)" 1 2)
        ;;    "[%p] %m at line %l, column %c.  (Severity: %s)\n"
        ("^\\[[^\n]+\\] [^\n]+ at line\\( \\)\\([0-9]+\\), column \\([0-9]+\\)" nil 2 3)

        ;; Verbose level 8
        ;;    "[%p] %m at line %l, column %c.  (Severity: %s)\n"
        ("^\\[[^\n]+\\] [^\n]+ at line\\( \\)\\([0-9]+\\), column \\([0-9]+\\)" nil 2 3)
        ;;    "[%p] %m at line %l, near '%r'.  (Severity: %s)\n"
        ("^\\[[^\n]+\\] [^\n]+ at line\\( \\)\\([0-9]+\\)" nil 2)

        ;; Verbose level 9
        ;;    "%m at line %l, column %c.\n  %p (Severity: %s)\n%d\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\), column \\([0-9]+\\)" nil 2 3)
        ;;    "[%p] %m at line %l, near '%r'.  (Severity: %s)\n"
        ("^\\[[^\n]+\\] [^\n]+ at line\\( \\)\\([0-9]+\\)" nil 2)

        ;; Verbose level 10
        ;;    "%m at line %l, near '%r'.\n  %p (Severity: %s)\n%d\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\)" nil 2)
        ;;    "%m at line %l, column %c.\n  %p (Severity: %s)\n%d\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\), column \\([0-9]+\\)" nil 2 3)

        ;; Verbose level 11
        ;;    "%m at line %l, near '%r'.\n  %p (Severity: %s)\n%d\n"
        ("^[^\n]+ at line\\( \\)\\([0-9]+\\)" nil 2)
        ))

;; The Emacs Lisp manual says to do this with the cl library.
(eval-when-compile (require 'cl))

(define-compilation-mode perlcritic-error-mode "perlcritic-error"
  "..."
  (set (make-local-variable 'perlcritic-buffer) src-buf)
  (ad-activate #'compilation-find-file))

;;;###autoload
(defun perlcritic ()
  "\\[perlcritic]] returns a either nil or t depending on whether the
current buffer passes perlcritic's check. If there are any warnings
those are displayed in a separate buffer."
  (interactive)
  (save-restriction
    (widen)
    (perlcritic-region (point-min) (point-max))))

;;;###autoload
(defun perlcritic-region (start end)
  "\\[perlcritic-region] returns a either nil or t depending on
whether the region passes perlcritic's check. If there are any
warnings those are displayed in a separate buffer."

  (interactive "r")

  ;; Kill the perlcritic buffer so I can make a new one.
  (if (get-buffer "*perlcritic*")
      (kill-buffer "*perlcritic*"))

  ;; In the following lines I'll be switching between buffers
  ;; freely. This upper save-excursion will keep things sane.
  (save-excursion
    (let ((src-buf (current-buffer))
          (err-buf (get-buffer-create "*perlcritic*")))

      (set-buffer src-buf)
      (let ((perlcritic-args (loop for p in (list
                                             ;; Add new bin/perlcritic
                                             ;; parameters here!
					     (perlcritic--param-profile)
					     (perlcritic--param-noprofile)
                                             (perlcritic--param-severity)
                                             (perlcritic--param-top)
					     (perlcritic--param-include)
					     (perlcritic--param-exclude)
					     (perlcritic--param-force)
                                             (perlcritic--param-verbose))
                                unless (null p)
                                append p)))
                                        ;
        (message "Perl critic...running")
        ;; Seriously. Is this the nicest way to call
        ;; CALL-PROCESS-REGION with variadic arguments? This blows!
        ;; (apply FUNCTION (append STATIC-PART DYNAMIC-PART))
        (let ((rc (apply 'call-process-region
                         (nconc (list start end
                                      perlcritic-bin nil
                                      (list err-buf t)
                                      nil)
                                perlcritic-args))))

          ;; Figure out whether we're ok or not. perlcritic has to
          ;; return zero and the output buffer has to be empty except
          ;; for that "... source OK" line. Different versions of the
          ;; perlcritic script will print different things when
          ;; they're ok. I expect to see things like "some-file source
          ;; OK", "SCALAR=(0x123457) source OK", "STDIN source OK",
          ;; and "source OK".
          (let ((perlcritic-ok (and (numberp rc)
                                    (zerop rc)
                                    (progn
				      (set-buffer err-buf)
				      (goto-char (point-min))
				      (delete-matching-lines "source OK$")
				      (zerop (buffer-size))))))
            ;; Either clean up or finish setting up my output.
            (if perlcritic-ok
		;; Ok!
                (progn
                  (kill-buffer err-buf)
                  (message "Perl critic...ok"))


	      ;; Not ok!
	      (message "Perl critic...not ok")

              ;; Set up the output buffer now I know it'll be used.  I
              ;; scooped the guts out of compile-internal. It is
              ;; CRITICAL that the errors start at least two lines
              ;; from the top. compile.el normally assumes the first
              ;; line is an informational `cd somedirectory' command
              ;; and the second line shows the program's invocation.
	      ;;
	      ;; Since I have the space available I've put the
	      ;; program's invocation here. Maybe it'd make sense to
	      ;; put the buffer's directory here somewhere too.
              (set-buffer err-buf)
              (goto-char (point-min))
              (insert (reduce (lambda (a b) (concat a " " b))
                              (nconc (list perlcritic-bin)
                                     perlcritic-args))
                      "\n"
		      ;; TODO: instead of a blank line, print the
		      ;; buffer's directory+file.
		      "\n")
              (goto-char (point-min))
	      ;; TODO: get `recompile' to work.

	      ;; just an fyi. compilation-mode will delete my local
	      ;; variables so be sure to call it *first*.
              (perlcritic-error-mode)
              ;; (ad-deactivate #'compilation-find-file)
              (display-buffer err-buf))

	    ;; Return our success or failure.
            perlcritic-ok))))))

;;; Parameters for use by perlcritic-region.
(defun perlcritic--param-profile ()
  "A private method that supplies the -profile FILENAME parameter for
\\[perlcritic-region]"
  (if perlcritic-profile (list "-profile" perlcritic-profile)))

(defun perlcritic--param-noprofile ()
  "A private method that supplies the -noprofile parameter for
\\[perlcritic-region]"
  (if perlcritic-noprofile (list "-noprofile")))

(defun perlcritic--param-force ()
  "A private method that supplies the -force parameter for
\\[perlcritic-region]"
  (if perlcritic-force (list "-force")))

(defun perlcritic--param-severity ()
  "A private method that supplies the -severity NUMBER parameter for
\\[perlcritic-region]"
  (cond ((stringp perlcritic-severity)
	 (list "-severity" perlcritic-severity))
        ((numberp perlcritic-severity)
	 (list "-severity" (number-to-string perlcritic-severity)))
        (t nil)))

(defun perlcritic--param-top ()
  "A private method that supplies the -top NUMBER parameter for
\\[perlcritic-region]"
  (cond ((stringp perlcritic-top)
	 (list "-top" perlcritic-top))
        ((numberp perlcritic-top)
	 (list "-top" (number-to-string perlcritic-top)))
        (t nil)))

(defun perlcritic--param-include ()
  "A private method that supplies the -include REGEXP parameter for
\\[perlcritic-region]"
  (if perlcritic-include
      (list "-include" perlcritic-include)
    nil))

(defun perlcritic--param-exclude ()
  "A private method that supplies the -exclude REGEXP parameter for
\\[perlcritic-region]"
  (if perlcritic-exclude
      (list "-exclude" perlcritic-exclude)
    nil))

(defun perlcritic--param-verbose ()
  "A private method that supplies the -verbose NUMBER parameter for
\\[perlcritic-region]"
  (cond ((stringp perlcritic-verbose)
	 (list "-verbose" perlcritic-verbose))
        ((numberp perlcritic-verbose)
	 (list "-verbose" (number-to-string perlcritic-verbose)))
        (t nil)))


;; Interactive functions for use by the user to modify parameters on
;; an adhoc basis. I'm sure there's room for significant niceness
;; here. Suggest something. Please.
(defun perlcritic-profile (profile)
  "Sets perlcritic's -profile FILENAME parameter."
  (interactive "sperlcritic -profile: ")
  (setq perlcritic-profile (if (string= profile "") nil profile)))

(defun perlcritic-noprofile (noprofile)
  "Toggles perlcritic's -noprofile parameter."
  (interactive (list (yes-or-no-p "Enable perlcritic -noprofile? ")))
  (setq perlcritic-noprofile noprofile))

(defun perlcritic-force (force)
  "Toggles perlcritic's -force parameter."
  (interactive (list (yes-or-no-p "Enable perlcritic -force? ")))
  (setq perlcritic-force force))

(defun perlcritic-severity (severity)
  "Sets perlcritic's -severity NUMBER parameter."
  (interactive "nperlcritic -severity: ")
  (setq perlcritic-severity severity))

(defun perlcritic-top (top)
  "Sets perlcritic's -top NUMBER parameter."
  (interactive "nperlcritic -top: ")
  (setq perlcritic-top top))

(defun perlcritic-include (include)
  "Sets perlcritic's -include REGEXP parameter."
  (interactive "sperlcritic -include: ")
  (setq perlcritic-include include))

(defun perlcritic-exclude (exclude)
  "Sets perlcritic's -exclude REGEXP parameter."
  (interactive "sperlcritic -exclude: ")
  (setq perlcritic-exclude exclude))

(defun perlcritic-verbose (verbose)
  "Sets perlcritic's -verbose NUMBER parameter."
  (interactive "nperlcritic -verbose: ")
  (setq perlcritic-verbose verbose))

;; Hooks compile.el's compilation-find-file to enable our file-less
;; operation. We feed `perlcritic-bin' from STDIN, not from a file.
(defadvice compilation-find-file (around perlcritic-find-file)
  "Lets perlcritic lookup into the buffer we just came from and don't
require that the perl document exist in a file anywhere."
  (let ((debug-buffer (marker-buffer marker)))
    (if (local-variable-p 'perlcritic-buffer debug-buffer)
        (setq ad-return-value perlcritic-buffer)
      ad-do-it)))

;; All the scaffolding of having a minor mode.
(defvar perlcritic-mode nil
  "Toggle `perlcritic-mode'")
(make-variable-buffer-local 'perlcritic-mode)

(defun perlcritic-write-hook ()
  "Check perlcritic during `write-file-hooks' for `perlcritic-mode'"
  (if perlcritic-mode
      (save-excursion
        (widen)
        (mark-whole-buffer)
        (let ((perlcritic-ok (perlcritic)))
          (if perlcritic-pass-required
	      ;; Impede saving if we're not ok.
              (not perlcritic-ok)
	    ;; Don't impede saving. We might not be ok but that
	    ;; doesn't matter now.
            nil)))
    ;; Don't impede saving. We're not in perlcritic-mode.
    nil))

;;;###autoload
(defun perlcritic-mode (&optional arg)
  "Perl::Critic checking minor mode."
  (interactive "P")

  ;; Enable/disable perlcritic-mode
  (setq perlcritic-mode (if (null arg)
			    ;; Nothing! Just toggle it.
			    (not perlcritic-mode)
			  ;; Set it.
			  (> (prefix-numeric-value arg) 0)))

  (if perlcritic-mode
      (add-hook 'write-file-hooks 'perlcritic-write-hook nil "local")
    (remove-hook 'write-file-hooks 'perlcritic-write-hook)))

;; Make a nice name for perl critic mode. This string will appear at
;; the bottom of the screen.
(if (not (assq 'perlcritic-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(perlcritic-mode " Critic")
                minor-mode-alist)))

(provide 'perlcritic)
