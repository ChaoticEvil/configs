;;; plugins.el --- настройка встроенных плагинов

;; IDO
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;; Linum
(require 'linum)
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

;; Org-mode
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED")))
(setq org-src-fontify-natively 't)

;; Запуск команды
(global-unset-key (kbd "<f5>"))
(global-set-key (kbd "<f5>") 'execute-extended-command)

;; Выбор буфера
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'bs-show)

;; Фолдинг
(defvar hs-special-modes-alist
 (mapcar 'purecopy
		 '((c-mode "{" "}" "/[*/]" nil nil)
			(c++-mode "{" "}" "/[*/]" nil nil)
			(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
			(java-mode "{" "}" "/[*/]" nil nil)
			(cperl-mode "{" "}" "/[*/]" nil nil)
			(js-mode "{" "}" "/[*/]" nil)
			(js2-mode "{" "}" "/[*/]" nil nil)
			(json-mode "{" "}" "/[*/]" nil nil))))

(require 'hideshow)
(global-set-key (kbd "M-<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)

;; Закладки
(global-set-key (kbd "C-b") 'bookmark-set)
(global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

(setq-default c-basic-offset 4)

;; CPerl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))
(setq cperl-invalid-face nil)
(setq cperl-electric-keywords t) ;; expands for keywords such as foreach, while, etc...
(mapc
     (lambda (pair)
       (if (eq (cdr pair) 'perl-mode)
           (setcdr pair 'cperl-mode)))
     (append auto-mode-alist interpreter-mode-alist))
(setq cperl-indent-level 4)

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

