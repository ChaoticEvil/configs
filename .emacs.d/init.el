;;; init.el --- Peter's Emacs config file
;;
;; Copyright (C) 2015-2018 by Peter Brovchenko <p.brovchenko@protonmail.com>
;;
;; Author: Peter Brovchenko <p.brovchenko@protonmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d/init.el
;; Version: 0.7.6
;;
;;; Commentary:
;;
;; Yet another Emacs customisation :)
;;
;;; Code:

(package-initialize)

(defun is-in-terminal()
  (not (display-graphic-p)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/included/themes")

(load "~/.emacs.d/included/packages.el"           ) ;; Set and install requirements plugins
(load "~/.emacs.d/included/common.el"             ) ;; Basic emacs settings (encoding, look and feel, etc)
(load "~/.emacs.d/included/shortcuts.el"          ) ;; Custom key bindings
(load "~/.emacs.d/included/plugins.el"            ) ;; Enable and settings builtin plugins
(load "~/.emacs.d/included/third_party_plugins.el") ;; Enable and settings third party plugins

;;; init.el ends here
