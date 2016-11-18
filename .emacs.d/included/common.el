;;; common.el --- general settings

;; Настройки кодировки
(set-language-environment 'UTF-8)
(setq buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)

;; Отключаем автоматическо создание резервных копиц и файлов autosave
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil)

;; Настройка границ
(fringe-mode '(8 . 0)) ;; строка с символами состояний (слева)
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Выводим доп. инфу в modeline
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

;; Перенос строк
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Подсветка синтаксиса
(require 'font-lock)
(setq font-lock-maximum-decoration t)

;; Настройка отступов
(setq-default indent-tabs-mode t) ;; в качестве отступов использовать символ табуляции
(setq-default tab-width 4) ;; ширина табуляции - 4 пробельных символа
(setq tab-width 4) ;; ширина табуляции - 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function) ;; отступы для lisp-кода

;; Настройка прокрутки
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin             5) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

(setq select-enable-clipboard t) ;; Clipboard settings

;; Подсветка результатов поиска
(setq search-highlight        t)
(setq query-replace-highlight t)

(delete-selection-mode t) ;; Возможность переопределять выделенный фрагмент текста

;; Look and Feel settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(show-paren-mode t) ;; включить выделение выражений между {},[],()
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()
(tooltip-mode      -1) ;; отключаем подсказки
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(setq use-dialog-box nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал
(load-theme 'zenburn t) ;; устанавливаем тему оформления
(set-frame-font "Iosevka Medium 14") ;; устанавливаем шрифт
(setq inhibit-splash-screen   t) ;; Прячем splash-screen
(setq ingibit-startup-message t) ;; Прячем начальное сообщение
(setq frame-title-format "GNU Emacs: %b") ;; Устанавливаем title окна 'GNU Emacs: filename'
(defalias 'yes-or-no-p 'y-or-n-p) ;; Укорачиваем ответы на вопросы в минибуфере
(global-hl-line-mode nil) ;; Потсвечиваем текущую строку

