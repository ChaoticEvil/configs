;;; packages.el --- определение и установка необходмых плагинов (если таковые еще не установлены)

(require 'package)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; require packages list
(defvar packagesList
  '(ein ;; Emacs IPython Notebook
	elpy ;; 
    smex ;; Smex is a M-x enhancement for Emacs.
	magit ;; мод для работы с git
	lua-mode ;; мод для работы с Lua
	web-mode ;; мод для работы с веб-подноготНОЙ
	js2-mode ;; улучшенный мод для работы с javascript
	json-mode ;; мод для работы с json-файлами
	flycheck ;; интерфейс для различных линтеров
	yasnippet ;; снипеты для различных ЯП
	py-autopep8 ;; поддержка питнячих пепов
	expand-region ;; для схлопывания региона
	markdown-mode ;; подсветка markdown
	zenburn-theme ;; лучшая тема, форевер энд евер
	auto-complete ;; автодополнение всего и вся
	highlight-symbol)) ;; подсветка слова, стоящего под курсором

;; install package in packagesList
(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      packagesList)

