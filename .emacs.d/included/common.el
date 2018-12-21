;;; common.el --- Basic Emacs settings
;;
;; Copyright (C) 2015-2018 by Peter Brovchenko <p.brovchenko@protonmail.com>
;;
;; Author: Peter Brovchenko <p.brovchenko@protonmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d/included/common.el
;; Version: 0.7.6
;;
;;; Commentary:
;;
;; Settings for encoding and loock and feel
;;
;;; Code:

;; Set themes directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Encoding
(set-language-environment            'UTF-8)
(setq buffer-file-coding-system      'utf-8)
(setq-default coding-system-for-read 'utf-8)
(setq file-name-coding-system        'utf-8)
(set-selection-coding-system         'utf-8)
(set-keyboard-coding-system          'utf-8-unix)
(set-terminal-coding-system          'utf-8)
(prefer-coding-system                'utf-8)

;; Disable autosave
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Fringes
(if (not (is-in-terminal))
	(fringe-mode '(8 . 0))
  (setq-default indicate-empty-lines t) ;; Gliph indicate for empty line
  (setq-default indicate-buffer-boundaries 'left)) ;; Only left-indication

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
(setq-default indent-tabs-mode t) ;; Indention as a tab symbol
(setq-default tab-width 4) ;; Set tad width in spaces
(setq tab-width 4) ;; Set tad width in spaces
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

;; Enable for delete selected text regionk
(delete-selection-mode t)

(defalias 'yes-or-no-p 'y-or-n-p) ;; Short answers (yes->y, no->n)

;; Look and Feel settings
(electric-pair-mode 1) ;; Autocompare scopes
(show-paren-mode t) ;; Enable highlight for scopes
(setq show-paren-style 'expression)
(tooltip-mode      -1) ;; Disable tooltip
(menu-bar-mode     -1) ;; Disable menubar
(tool-bar-mode     -1) ;; Disable toolbar

(if (not (is-in-terminal))
	(scroll-bar-mode   -1)) ;; Disable scrollbar

(setq use-dialog-box nil) ;; No gui dialogs. Only minibuffer
(setq ring-bell-function 'ignore) ;; Disable bell sound
(setq ingibit-startup-message t) ;; Disable startup message
(setq inhibit-splash-screen   t) ;; Disable splash-screen
(setq frame-title-format "GNU Emacs: %b") ;; Set window title as 'GNU Emacs: <filename>'

;; Set color theme only for windowed mode
(load-theme 'nimbus t)

(set-frame-font "Iosevka Medium 19") ;; Set font
(global-hl-line-mode nil) ;; Highlight current line

;; Change Meta for OS X
(cond
  ((string-equal system-type "darwin") ; Mac OS X
   (progn
	 (setq mac-option-key-is-meta nil)
	 (setq mac-command-key-is-meta t)
	 (setq mac-command-modifier 'meta)
	 (setq mac-option-modifier nil))))

;;; common.el ends here
