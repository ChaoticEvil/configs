;;; Peter's Emacs config file


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/included/packages.el") ;; enable melpa repository and install requirements plugins
(load "~/.emacs.d/included/common.el") ;; base emacs settings (encoding, look and feel, etc)
(load "~/.emacs.d/included/shortcuts.el") ;; set new key bindings
(load "~/.emacs.d/included/plugins.el") ;; enable and settings integrated plugins
(load "~/.emacs.d/included/third_party_plugins.el") ;; enable and settings third party plugins (installed from melpa)

;;; EOF
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(powerline ace-jump-mode auto-complete zenburn-theme markdown-mode py-autopep8 wanderlust flycheck lua-mode js2-mode web-mode magit smex elpy ein))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
