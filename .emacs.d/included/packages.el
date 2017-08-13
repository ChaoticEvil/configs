;;; packages.el --- Emacs packages defined
;;
;; Copyright (C) 2015-2017 by Peter Brovchenko <peter.brovchenko@gmail.com>
;;
;; Author: Peter Brovchenko <peter.brovchenko@gmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d
;; Version: 0.5.3
;;
;;; Commentary:
;;
;; Set and install some Emacs plugins (packages)
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; require packages list
(defvar packagesList
  '(elpy ;; Python autocomplete
	smex ;; Smex is a M-x enhancement for Emacs.
	magit ;; Work with git scm
	ensime ;; Scala runtime server
	pomidor ;; Pomidor is a simple and cool pomodoro technique timer
	lua-mode ;; Better work with lua
	web-mode ;; Better work with html,css,template engines
	js2-mode ;; Better work with javascript
	json-mode ;; Better work with json
	scala-mode ;; Work with scala
	flycheck ;; Backend for several linters
	restclient ;; Simple http-client
	sr-speedbar ;; Filesystem tree
	py-autopep8 ;; Enable python pep8
	expand-region ;; For expandong text regions
	markdown-mode ;; Better work with markdown (.md)
	zenburn-theme ;; Best Emacs theme
	auto-complete ;; Autocomplete
	highlight-symbol)) ;; Highlight word under cursosr

;; install package in packagesList
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packagesList)

;;; packages.el ends here
