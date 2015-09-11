;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Добаляем необходимые пути
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/plugins/")

;; Avialable themes
(set 'darcula-theme 'darcula)
(set 'tango-theme 'tango)
(set 'light-soap-theme 'light-soap)
(set 'zen-and-art-theme 'zen-and-art)
(set 'zenburn-theme 'zenburn)
(set 'jazz-theme 'jazz)
(set 'ample-zen-theme 'ample-zen)
(set 'ample-theme 'ample)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
;; choose one to enable
;;(enable-theme 'ample)

(set 'current-theme 'ample) ;; Set current theme
(set 'current-font "Consolas 14") ;;  Set default font
(if (equal system-type 'darwin)
    (set 'current-font "Monaco 13"))

;; Настройки кодировки
(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8)
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
;; (setq-default indicate-empty-lines t) ;; отсутствие строки выделить глифами рядом с полосой с номером строки
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
(setq-default indent-tabs-mode t) ;; отключить возможность ставить отступы TAB'ом
(setq-default tab-width 4)
(setq tab-width 4)
;;(setq-default tab-width          4) ;; ширина табуляции - 4 пробельных символа
;;(setq-default c-basic-offset     4)
;;(setq-default standart-indent    4) ;; стандартная ширина отступа - 4 пробельных символа
;;(setq-default lisp-body-indent   4) ;; сдвигать Lisp-выражения на 4 пробельных символа
(global-set-key (kbd "RET") 'newline-and-indent) ;; при нажатии Enter перевести каретку и сделать отступ
(setq lisp-indent-function  'common-lisp-indent-function)

;; Настройка прокрутки
(setq scroll-step               1) ;; вверх-вниз по 1 строке
(setq scroll-margin            10) ;; сдвигать буфер верх/вниз когда курсор в 10 шагах от верхней/нижней границы
(setq scroll-conservatively 10000)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Подсветка результатов поиска
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Bookmarks map
;;(global-set-key (kbd "C-b") 'bookmark-set)
;;(global-set-key (kbd "M-b") 'bookmark-jump)
;;(global-set-key (kbd "<f5>") 'bookmark-bmenu-list)

;; Фолдинг
;;(defvar hs-special-modes-alist
;;  (mapcar 'purecopy
;;		  '((c-mode "{" "}" "/[*/]" nil nil)
;;			(c++-mode "{" "}" "/[*/]" nil nil)
;;			(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
;;			(java-mode "{" "}" "/[*/]" nil nil)
;;			(js-mode "{" "}" "/[*/]" nil)))

;;(require 'hideshow)
;; (global-set-key (kbd "M-<f9>") 'hs-toggle-hiding)
;; (global-set-key (kbd "C-<f9>") 'hs-hide-all)
;; (global-set-key (kbd "C-S-<f9>") 'hs-show-all)

;; Удаляем пробелы в конце строк при сохранении файла
;;(defun format-current-buffer()
;;    (indent-region (point-min) (point-max)))
;;(defun untabify-current-buffer()
;;    (if (not indent-tabs-mode)
;;        (untabify (point-min) (point-max)))
;;    nil)
;;;(add-to-list 'write-file-functions 'format-current-buffer)
;;;(add-to-list 'write-file-functions 'untabify-current-buffer)
;;(add-to-list 'write-file-functions 'delete-trailing-whitespace)

;; Если используется современная версия emcas
;; Используем melpa-репозиторий плагинов
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End | Global settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look and Feel settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Electric-modes settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию  electric-indent-mod'ом (default in Emacs-24.4)

;; Show-paren-mode settings
(show-paren-mode t) ;; включить выделение выражений между {},[],()
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()

;; Отключаем компоненты GUI
(tooltip-mode      -1) ;; отключаем подсказки
(menu-bar-mode     -1) ;; отключаем графическое меню
(tool-bar-mode     -1) ;; отключаем tool-bar
(scroll-bar-mode   -1) ;; отключаем полосу прокрутки
(setq use-dialog-box nil) ;; никаких графических диалогов и окон - все через минибуфер
(setq redisplay-dont-pause t)  ;; лучшая отрисовка буфера
(setq ring-bell-function 'ignore) ;; отключить звуковой сигнал

(load-theme current-theme t) ;; Устанавливаем активную тему
(set-default-font current-font) ;; Устанавливаем шрифт

;; Прячем splash-screen и начальное сообщение
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Устанавливаем title окан
(setq frame-title-format "GNU Emacs: %b")

;; Возможность переопределять выделенный фрагмент текста
(delete-selection-mode t)

;; Укорачиваем ответы на вопросы в минибуфере
(defalias 'yes-or-no-p 'y-or-n-p)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End | Look and Feel settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugins settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Активируем ErgoEmacs
(require 'ergoemacs-mode)
(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)

;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Активируем sr-speedbar и биндим на C-f12
(require 'sr-speedbar)
(global-set-key (kbd "C-<f12>") 'sr-speedbar-toggle)

;; Magit
(global-set-key (kbd "C-<f6>") 'magit-status)

;; Плагин IDO
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;; Плагин Dired
(require 'dired)
(setq dired-recursive-deletes 'top) ;; чтобы можно было непустые директории удалять...

;; Плагин Linum
(require 'linum)
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d ") ;; задаем формат нумерации строк

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ep?\\'" . web-mode))
;; Количество пробелов в отступах для html, css и javascript
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;;(setq web-mode-markup-indent-offset 4)
  ;;(setq web-mode-css-indent-offset 4)
  ;;(setq web-mode-code-indent-offset 4)
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

;; CPerl-mode
(mapc
     (lambda (pair)
       (if (eq (cdr pair) 'perl-mode)
           (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist))
(setq cperl-indent-level 4)
;; (setq cperl-hairy nil) ;; Turns on most of the CPerlMode options
(defun find-perl-module (module-name)
      (interactive "sPerl module name: ")
      (let ((path (perl-module-path module-name)))
        (if path
            (find-file path)
          (error "Module '%s' not found" module-name))))
(defun global-trim ()
    "Trim all trailing whitespace in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (replace-match "" t t))))

;; Ya-snippets
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<M-s>") 'yas-expand)

;; Org-mode
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED")))
(setq org-src-fontify-natively 't)

;; Skype
;; (require 'skype)
;; (setq skype--my-user-handle "dr.brduch")


;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 4)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Flycheck
;; (require 'flycheck)
;; (add-hook 'js2-mode-hook
;;           (lambda () (flycheck-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End | Plugins settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(org-CUA-compatible nil)
 '(org-replace-disputed-keys nil)
 '(recentf-mode t)
 '(shift-select-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
