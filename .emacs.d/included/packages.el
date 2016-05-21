;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Устанавливаем необходимые плагины из melpa, если таковые еще не установлены
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; require packages list
(defvar packagesList
  '(ein
	elpy
    smex
	magit
	web-mode
	js2-mode
	lua-mode
	flycheck
	yasnippet
	wanderlust
	py-autopep8
	markdown-mode
	zenburn-theme
    auto-complete))

;; install package in packagesList
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packagesList)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
