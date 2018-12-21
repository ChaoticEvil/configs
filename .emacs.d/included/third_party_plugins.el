;;; third_party_plugins.el --- Settings for third-party plugins
;;
;; Copyright (C) 2015-2018 by Peter Brovchenko <p.brovchenko@protonmail.com>
;;
;; Author: Peter Brovchenko <p.brovchenko@protonmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d/included/third_party_plugins.el
;; Version: 0.7.6
;;
;;; Commentary:
;;
;; Settings for third-party plugins
;;
;;; Code:

;; Total autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Magit
(global-set-key (kbd "C-<f6>") 'magit-status)

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ep?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vbhtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vbhtml?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("underscore"    . "\\.html\\'"))
)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
  )
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Markdowm Mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 4)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode) ;; turn on flychecking globally

;; Disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode) ;; use eslint with web-mode for jsx files

(setq-default flycheck-temp-prefix ".flycheck") ;; customize flycheck temp file prefix

;; Disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; Check html and css
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

;; For better jsx syntax-highlighting in web-mode
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(add-hook 'js2-mode (lambda ()
	((global-unset-key (kbd "M-j"))
	 (global-set-key (kbd "M-j") 'backward-char)
	 )))

;; Python
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Smart region selection
(require 'expand-region)
(global-unset-key (kbd "M-="))
(global-set-key (kbd "M-=") 'er/expand-region)

;; Hightlight word under cursor
(require 'highlight-symbol)
(setq highlight-symbol-on-navigation-p t)
(global-unset-key (kbd "M-8"))
(global-set-key (kbd "M-8") 'highlight-symbol-next)
(global-unset-key (kbd "M-7"))
(global-set-key (kbd "M-7") 'highlight-symbol-prev)
(global-unset-key (kbd "M-9"))
(global-set-key (kbd "M-9") 'highlight-symbol-query-replace)
(global-unset-key (kbd "M-0"))
(global-set-key (kbd "M-0") 'highlight-symbol-mode)

;; Sr-speedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
(setq speedbar-show-unknown-files t) ; show all files
(setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side

;; Ensime
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Pomidor
(setq pomidor-sound-tick nil
      pomidor-sound-tack nil)

;;; third_party_plugins.el ends here
