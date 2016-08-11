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
(fringe-mode '(8 . 0)) ;; органичиталь текста только слева
(setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
(setq-default indicate-buffer-boundaries 'left) ;; индикация только слева

;; Отображаем в mode-line название редактируемого файла, текущее время и размер файла
(setq display-time-24hr-format t) ;; 24-часовой временной формат в mode-line
(display-time-mode             t) ;; показывать часы в mode-line
(size-indication-mode          t) ;; размер файла в %-ах

;; Перенос строк
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Подсветка синтаксиса
(require 'font-lock)
(global-font-lock-mode             t) ;; включено с версии Emacs-22. На всякий...
(setq font-lock-maximum-decoration t)

;; Настройка отступов
(setq-default indent-tabs-mode t) ;; в качестве отступов использовать отступы TAB'ы
(setq-default tab-width 4) ;; ширина табуляции - 4 пробельных символа
(setq tab-width 4) ;; ширина табуляции - 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function)

;; Настройка прокрутки
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

(setq select-enable-clipboard t) ;; Clipboard settings

;; Подсветка результатов поиска
(setq search-highlight        t)
(setq query-replace-highlight t)

(delete-selection-mode t) ;; Возможность переопределять выделенный фрагмент текста

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and Feel settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию electric-indent-mod'ом (default in Emacs-24.4)

(show-paren-mode t) ;; включить выделение выражений между {},[],()
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()

(tooltip-mode      -1) ;; отключаем подсказки
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(setq use-dialog-box nil) ;; никаких графических диалогов и окон - все через минибуфер
;;(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

(load-theme 'zenburn t) ;; устанавливаем тему оформления
(set-frame-font "Iosevka Medium 14") ;; устанавливаем шрифт

;; Прячем splash-screen и начальное сообщение
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Устанавливаем title окна, соответствующий шаблону 'GNU Emacs: имя_редактируемого_файла'
(setq frame-title-format "GNU Emacs: %b")

(defalias 'yes-or-no-p 'y-or-n-p) ;; Укорачиваем ответы на вопросы в минибуфере

;; Потсвечиваем текущую строку
(global-hl-line-mode nil)

