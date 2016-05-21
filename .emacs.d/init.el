;;; Peter's Emacs config file

(load "~/.emacs.d/included/packages.el") ;; enable melpa repository and install requirements plugins
(load "~/.emacs.d/included/common.el") ;; base emacs settings (encoding, look and feel, etc)
(load "~/.emacs.d/included/shortcuts.el") ;; set new key bindings
(load "~/.emacs.d/included/plugins.el") ;; enable and settings integrated plugins
(load "~/.emacs.d/included/third_party_plugins.el") ;; enable and settings third party plugins (installed from melpa)

;;; EOF
