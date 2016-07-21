;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Переопределение клавиш
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Вверх
(global-unset-key (kbd "M-i"))
(global-set-key (kbd "M-i") 'previous-line)

;; Вниз
(global-unset-key (kbd "M-k"))
(global-set-key (kbd "M-k") 'next-line)

;; Влево
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "M-j") 'backward-char)

;; Вправо
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-l") 'forward-char)

;; Page Up
(global-unset-key (kbd "M-I"))
(global-set-key (kbd "M-I") 'scroll-down-command)

;; Page Down
(global-unset-key (kbd "M-K"))
(global-set-key (kbd "M-K") 'scroll-up-command)

;; Forward word
(global-unset-key (kbd "M-L"))
(global-set-key (kbd "M-L") 'forward-word)

;; Backward word
(global-unset-key (kbd "M-J"))
(global-set-key (kbd "M-J") 'backward-word)

;; Beginnning of line
(global-unset-key (kbd "M-H"))
(global-set-key (kbd "M-H") 'move-end-of-line)

;; End of line
(global-unset-key (kbd "M-h"))
(global-set-key (kbd "M-h") 'move-beginning-of-line)

;; End of buffer
(global-unset-key (kbd "M-N"))
(global-set-key (kbd "M-N") 'end-of-buffer)

;; Beginning of buffer
(global-unset-key (kbd "M-n"))
(global-set-key (kbd "M-n") 'beginning-of-buffer)

;; Переместить курсор в другое (следующее) окно
(global-unset-key (kbd "M-s"))
(global-set-key (kbd "M-s") 'other-window)

;; Редактирование
;;

;; Delete
(global-unset-key (kbd "M-f"))
(global-set-key (kbd "M-f") 'delete-forward-char)

;; Backspace
(global-unset-key (kbd "M-d"))
(global-set-key (kbd "M-d") 'delete-backward-char)

;; Delete word
(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-r") 'kill-word)

;; Delete word backward
(global-unset-key (kbd "M-e"))
(global-set-key (kbd "M-e") 'backward-kill-word)

;; Enter
(global-unset-key (kbd "M-m"))
(global-set-key (kbd "M-m") 'reindent-then-newline-and-indent)

;; Выделение
(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Копирование
(global-unset-key (kbd "M-c"))
(global-set-key (kbd "M-c") 'kill-ring-save)

;; Вырезание
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'kill-region)

;; Вставка
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v") 'yank)

;; Отмена
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'undo)

;; Открытие
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'find-file)

;; Comment or uncomment selected region
(global-unset-key (kbd "M-'"))
(global-set-key (kbd "M-'") 'comment-or-uncomment-region)

;; Revert buffer
(global-unset-key (kbd "C-M-r"))
(global-set-key (kbd "C-M-r") 'revert-buffer)

;; Goto line
(global-unset-key (kbd "<f6>"))
(global-set-key (kbd "<f6>") 'goto-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;