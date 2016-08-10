;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Third party plugins settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Autocomplete
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
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ep?\\'" . web-mode))

;; Количество пробелов в отступах для html, css и javascript
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

;; Ya-snippets
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<M-s>") 'yas-expand)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 4)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; Flycheck
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; Check html and css
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'html-tidy 'web-mode)
  (flycheck-add-mode 'css-csslint 'web-mode))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; Python
(elpy-enable)
;;(elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Выделение региона текста
(require 'expand-region)
(global-unset-key (kbd "M-="))
(global-set-key (kbd "M-=") 'er/expand-region)

;; Подсветка слова, находящегося под курсором
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
