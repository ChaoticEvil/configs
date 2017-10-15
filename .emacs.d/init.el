;;; init.el --- Peter's Emacs config file
;;
;; Copyright (C) 2015-2017 by Peter Brovchenko <peter.brovchenko@gmail.com>
;;
;; Author: Peter Brovchenko <peter.brovchenko@gmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d
;; Version: 0.6.2
;;
;;; Commentary:
;;
;; Yet another Emacs customisation :)
;;
;;; Code:

(package-initialize)

(defun is-in-terminal()
    (not (display-graphic-p)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/included")
(load "~/.emacs.d/included/packages.el"           ) ;; Set and install requirements plugins
(load "~/.emacs.d/included/common.el"             ) ;; Basic emacs settings (encoding, look and feel, etc)
(load "~/.emacs.d/included/shortcuts.el"          ) ;; Custom key bindings
(load "~/.emacs.d/included/plugins.el"            ) ;; Enable and settings builtin plugins
(load "~/.emacs.d/included/third_party_plugins.el") ;; Enable and settings third party plugins

;;; init.el ends here
