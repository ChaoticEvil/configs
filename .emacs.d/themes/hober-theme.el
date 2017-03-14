;;; hober-theme.el --- hober theme
;;
;; Copyright (C) 1999, 2000  Jonadab the Unsightly One <jonadab@bright.net>
;; Copyright (C) 2000, 2001, 2002, 2003  Alex Schroeder <alex@gnu.org>
;; Copyright (C) 2003, 2004, 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2013 by Syohei YOSHIDA
;; Copyright (C) 2017 by Peter Brovchenko
;;
;; Author: Peter Brovchenko <peter.brovchenko@gmail.com>
;; URL: https://github.com/ChaoticEvil/configs/tree/master/.emacs.d
;; Version: 0.5.0
;;
;;; Commentary:
;;
;; Modified of hober-theme from `color-themes-modern' package
;;
;;; Code:

(deftheme hober
  "hober theme")

(custom-theme-set-faces
 'hober
 '(default ((t (:background "black" :foreground "#c0c0c0"))))
 '(bold ((t (:bold t :weight bold))))
 '(bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
 '(border ((t (:background "black"))))
 '(buffer-menu-buffer ((t (:bold t :weight bold))))
 '(button ((t (:underline t :foreground "cyan1"))))
 '(calendar-today-face ((t (:underline t))))
 '(completions-annotations ((t (:italic t :slant italic))))
 '(completions-common-part ((t (:foreground "#c0c0c0" :background "black"))))
 '(completions-first-difference ((t (:bold t :weight bold))))
 '(cperl-array-face ((t (:bold t :background "lightyellow2"
                               :foreground "Blue" :weight bold))))
 '(cperl-hash-face ((t (:italic t :bold t :background "lightyellow2"
                                :foreground "Red" :slant italic :weight bold))))
 '(cperl-nonoverridable-face ((t (:foreground "chartreuse3"))))
 '(cursor ((t (:background "greenyellow"))))
 '(diary-face ((t (:foreground "red"))))

 '(dired-directory ((t (:foreground "#4186be"))))
 '(dired-flagged ((t (:bold t :weight bold :foreground "Pink"))))
 '(dired-header ((t (:foreground "Coral"))))
 '(dired-ignored ((t (:foreground "grey70"))))
 '(dired-mark ((t (:foreground "#00ff00"))))
 '(dired-marked ((t (:bold t :weight bold :foreground "DarkOrange"))))
 '(dired-perm-write ((t (:foreground "Red"))))
 '(dired-symlink ((t (:foreground "#00ffff"))))
 '(dired-warning ((t (:bold t :weight bold :foreground "Red"))))

 '(error ((t (:bold t :foreground "Pink" :weight bold))))
 '(escape-glyph ((t (:foreground "cyan"))))
 '(excerpt ((t (:italic t :slant italic))))
 '(file-name-shadow ((t (:foreground "grey70"))))
 '(fixed ((t (:bold t :weight bold))))

 '(flyspell-duplicate-face ((t (:bold t :foreground "Gold3" :underline t :weight bold))))
 '(flyspell-incorrect-face ((t (:bold t :foreground "OrangeRed" :underline t :weight bold))))

 '(font-lock-builtin-face ((t (:foreground "#ffaa00"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "Red"))))
 '(font-lock-comment-face ((t (:foreground "Red"))))
 '(font-lock-constant-face ((t (:foreground "#00ff00"))))
 '(font-lock-doc-face ((t (:foreground "#ffff00"))))
 '(font-lock-function-name-face ((t (:foreground "#4186be"))))
 '(font-lock-keyword-face ((t (:foreground "#00ffff"))))
 '(font-lock-negation-char-face ((t (nil))))
 '(font-lock-preprocessor-face ((t (:foreground "#ffaa00"))))
 '(font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
 '(font-lock-string-face ((t (:foreground "#ffff00"))))
 '(font-lock-type-face ((t (:foreground "Coral"))))
 '(font-lock-variable-name-face ((t (:bold t :foreground "white" :weight bold))))
 '(font-lock-warning-face ((t (:bold t :foreground "Red" :weight bold))))

 '(fringe ((t (:background "grey10"))))
 '(glyphless-char ((t (:height 0.6))))

 '(header-line ((t (:box (:line-width -1 :style released-button)
                         :background "grey20" :foreground "grey90" :box nil))))
 '(help-argument-name ((t (nil))))
 '(highlight ((t (:background "#353535" :foreground "grey90"))))
 '(highlight-changes-delete-face ((t (:foreground "red" :underline t))))
 '(highlight-changes-face ((t (:foreground "red"))))
 '(holiday-face ((t (:background "pink"))))
 '(isearch ((t (:background "palevioletred2" :foreground "brown4"))))
 '(isearch-fail ((t (:background "red4"))))
 '(italic ((t (:italic t :slant italic))))
 '(lazy-highlight ((t (:background "paleturquoise4"))))
 '(link ((t (:foreground "cyan1" :underline t))))
 '(link-visited ((t (:underline t :foreground "violet"))))
 '(makefile-space-face ((t (:background "hotpink"))))
 '(match ((t (:background "RoyalBlue3"))))
 '(menu ((t (nil))))

 '(message-cited-text-face ((t (:foreground "white"))))
 '(message-header-cc-face ((t (:foreground "#4186be"))))
 '(message-header-name-face ((t (:foreground "white"))))
 '(message-header-newsgroups-face
   ((t (:bold t :foreground "Coral" :weight bold))))
 '(message-header-other-face ((t (:foreground "steel blue"))))
 '(message-header-subject-face
   ((t (:bold t :foreground "#4186be" :weight bold))))
 '(message-header-to-face ((t (:bold t :foreground "#4186be" :weight bold))))
 '(message-header-xheader-face ((t (:foreground "blue"))))
 '(message-separator-face ((t (:foreground "brown"))))

 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(mode-line
   ((t (:background "darkslateblue" :foreground "white"
                    :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:bold t :background "darkslateblue"
                                  :foreground "white" :weight bold))))
 '(mode-line-emphasis ((t (:bold t :weight bold))))
 '(mode-line-highlight
   ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive
   ((t (:background "grey30"
                    :foreground "grey80"
                    :box (:line-width -1 :color "grey40" :style nil)
                    :weight light))))
 '(modeline-mousable ((t (:background "darkslateblue" :foreground "white"))))
 '(modeline-mousable-minor-mode
   ((t (:background "darkslateblue" :foreground "white"))))

 '(mouse ((t (:background "black"))))
 '(next-error ((t (:foreground "white" :background "darkslateblue"))))
 '(nobreak-space ((t (:foreground "cyan" :underline t))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2"))))
 '(region ((t (:background "darkslateblue" :foreground "white"))))
 '(scroll-bar ((t (nil))))
 '(secondary-selection ((t (:background "paleturquoise"))))

 '(sgml-doctype-face ((t (:foreground "orange"))))
 '(sgml-end-tag-face ((t (:foreground "greenyellow"))))
 '(sgml-entity-face ((t (:foreground "gold"))))
 '(sgml-ignored-face ((t (:background "gray60" :foreground "gray20"))))
 '(sgml-sgml-face ((t (:foreground "yellow"))))
 '(sgml-start-tag-face ((t (:foreground "mediumspringgreen"))))

 '(shadow ((t (:foreground "grey70"))))
 '(show-paren-match-face ((t (:background "purple" :foreground "white"))))
 '(show-paren-mismatch-face ((t (:background "red" :foreground "white"))))
 '(success ((t (:bold t :foreground "Green1" :weight bold))))

 '(term-black ((t (:foreground "black"))))
 '(term-blackbg ((t (:background "black"))))
 '(term-blue ((t (:foreground "#4186be"))))
 '(term-bluebg ((t (:background "#4186be"))))
 '(term-bold ((t (:bold t :weight bold))))
 '(term-cyan ((t (:foreground "#71bebe"))))
 '(term-cyanbg ((t (:background "#71bebe"))))
 '(term-default-bg ((t (nil))))
 '(term-default-bg-inv ((t (nil))))
 '(term-default-fg ((t (nil))))
 '(term-default-fg-inv ((t (nil))))
 '(term-green ((t (:foreground "#e5f779"))))
 '(term-greenbg ((t (:background "#e5f779"))))
 '(term-invisible ((t (nil))))
 '(term-invisible-inv ((t (nil))))
 '(term-magenta ((t (:foreground "#ef9ebe"))))
 '(term-magentabg ((t (:background "#ef9ebe"))))
 '(term-red ((t (:foreground "#ef8171"))))
 '(term-redbg ((t (:background "#ef8171"))))
 '(term-underline ((t (:underline t))))
 '(term-white ((t (:foreground "#c0c0c0"))))
 '(term-whitebg ((t (:background "#c0c0c0"))))
 '(term-yellow ((t (:foreground "#fff796"))))
 '(term-yellowbg ((t (:background "#fff796"))))

 '(tool-bar
   ((t (:background "grey75" :foreground "black"
                    :box (:line-width 1 :style released-button)))))
 '(tooltip
   ((t (:background "lightyellow" :foreground "black"))))
 '(trailing-whitespace ((t (:background "red1"))))
 '(underline ((t (:underline t))))

 '(vertical-border ((t (nil))))
 '(warning ((t (:bold t :foreground "DarkOrange" :weight bold))))

 '(widget-button ((t (:bold t :weight bold))))
 '(widget-button-pressed ((t (:background "black" :foreground "red"))))
 '(widget-documentation ((t (:background "white" :foreground "dark green"))))
 '(widget-field ((t (:background "gray85" :foreground "black"))))
 '(widget-inactive ((t (:background "red" :foreground "dim gray"))))
 '(widget-single-line-field ((t (:background "gray85" :foreground "black"))))

 '(zmacs-region ((t (:background "darkslateblue" :foreground "white")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hober)

;;; hober-theme.el ends here
