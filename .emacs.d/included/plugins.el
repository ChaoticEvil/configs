;;; plugins.el --- Plugins settings
;;
;; Copyright (C) 2015-2017 by Peter Brovchenko <peter.brovchenko@gmail.com>
;;
;; Author: Peter Brovchenko <peter.brovchenko@gmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d
;; Version: 0.5.0
;;
;;; Commentary:
;;
;; Settings for used plugins
;;
;;; Code:

;; IDO
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-vitrual-buffers      t)
(setq ido-enable-flex-matching t)

;; Linum
(require 'linum)
(line-number-mode   t) ;; Show current line number in modeline
(global-linum-mode  t) ;; Show lines numbers in all buffers
(column-number-mode t) ;; Show column number in modeline
(setq linum-format " %d") ;; Set format for line numbers

;; Org-mode
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "|" "DONE" "DELEGATED")))
(setq org-src-fontify-natively 't)

;; Execute Emacs command
(global-unset-key (kbd "<f5>"))
(global-set-key (kbd "<f5>") 'execute-extended-command)

;; List of buffers
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'bs-show)

;; Folding
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

;; Bookmarks
(global-set-key (kbd "C-b") 'bookmark-set)
(global-set-key (kbd "M-b") 'bookmark-jump)
(global-set-key (kbd "<f4>") 'bookmark-bmenu-list)

(setq-default c-basic-offset 4) ;; Set indention for C-mode

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
;; Finding perl modules paths
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

;; Scala-mode settings
(setq exec-path (append exec-path (list "/usr/share/scala/bin" ))) ;; Set location of scala bin
(require 'scala-mode)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(eval-after-load "scala-mode" 
  '(progn
    (define-key scala-mode-map (kbd "<f9>") 'ensime-builder-build)
    (define-key scala-mode-map (kbd "<f10>") 'ensime-inf-switch)))

(eval-after-load "scala-mode" 
  '(progn
	(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
	(define-key scala-mode-map (kbd "<f9>") 'scala-run)
	(define-key scala-mode-map (kbd "RET") 'newline-and-indent)
	))
(defun scala-run () 
  (interactive)   
  (ensime-sbt-action "run")
  (ensime-sbt-action "~compile")
  (let ((c (current-buffer)))
    (switch-to-buffer-other-window
	 (get-buffer-create (ensime-sbt-build-buffer-name)))
	(switch-to-buffer-other-window c))) 
(setq exec-path
	  (append exec-path (list "/usr/share/scala/bin"))) ;; Set path for scalac bin

;;; plugins.el ends here
