;; gattaca-theme.el -- custom base16 themes for emacs

;; Author: Kaleb Elwert <belak@coded.io>
;;         Neil Bhakta
;; Maintainer: Kaleb Elwert <belak@coded.io>
;; Version: 1.1
;; Homepage: https://github.com/belak/base16-emacs

;;; Commentary:
;; base16-theme is a collection of themes built around the base16
;; concept (https://github.com/chriskempson/base16).  All themes are
;; generated from the official set of color schemes and the templates
;; which are included in this repo.

;;; Code:

(defcustom base16-theme-256-color-source "terminal"
  "Where to get the colors in a 256-color terminal.

In a 256-color terminal, it's not clear where the colors should come from.
There are 3 possible values: terminal (which was taken from the xresources
theme), base16-shell (which was taken from a combination of base16-shell and
the xresources theme) and colors (which will be converted from the actual
html color codes to the closest color).

Note that this needs to be set before themes are loaded or it will not work."
  :type '(string)
  :group 'base16
  :options '("terminal" "base16-shell" "colors"))

(defcustom base16-distinct-fringe-background t
  "Make the fringe background different from the normal background color.
Also affects `linum-mode' background."
  :type 'boolean
  :group 'base16)

(defcustom base16-highlight-mode-line nil
  "Make the active mode line stand out more.

There are two choices for applying the emphasis:
  box:      Draws a thin border around the active 
            mode line.
  contrast: Use the \"default\" face's foreground
            which should result in more contrast."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Draw box around" box)
                 (const :tag "Contrast" contrast))
  :group 'base16)

(defvar base16-shell-colors
  '(:base00 "black"
    :base01 "brightgreen"
    :base02 "brightyellow"
    :base03 "brightblack"
    :base04 "brightblue"
    :base05 "white"
    :base06 "brightmagenta"
    :base07 "brightwhite"
    :base08 "red"
    :base09 "brightred"
    :base0A "yellow"
    :base0B "green"
    :base0C "cyan"
    :base0D "blue"
    :base0E "magenta"
    :base0F "brightcyan")
  "Base16 colors used when in a terminal and not using base16-shell.

These mappings are based on the xresources themes.  If you're
using a different terminal color scheme, you may want to look for
an alternate theme for use in the terminal.")

(defvar base16-shell-colors-256
  '(:base00 "black"
    :base01 "color-18"
    :base02 "color-19"
    :base03 "brightblack"
    :base04 "color-20"
    :base05 "white"
    :base06 "color-21"
    :base07 "brightwhite"
    :base08 "red"
    :base09 "color-16"
    :base0A "yellow"
    :base0B "green"
    :base0C "cyan"
    :base0D "blue"
    :base0E "magenta"
    :base0F "color-17")
  "Base16 colors used when in a terminal and using base16-shell.

These mappings are based on the xresources themes combined with
the base16-shell code.  If you're using a different terminal
color scheme, you may want to look for an alternate theme for use
in the terminal.")

(defun base16-transform-color-key (key colors)
  "Transform a given color `KEY' into a theme color.

This function is meant for transforming symbols to valid colors.
If the value refers to a setting then return whatever is appropriate.
If not a setting but is found in the valid list of colors then
return the actual color value. Otherwise return the value unchanged."
  (if (symbolp key)
      (cond

       ((string= (symbol-name key) "base16-settings-fringe-bg")
        (if base16-distinct-fringe-background
            (plist-get colors :base00)
		  (plist-get colors :base00)))

	   ((string= (symbol-name key) "base16-settings-mode-line-box")
		(if (eq base16-highlight-mode-line 'box)
			(list :line-width 1 :color (plist-get colors :base04))
		  nil))

	   ((string= (symbol-name key) "base16-settings-mode-line-fg")
		(if (eq base16-highlight-mode-line 'contrast)
			(plist-get colors :base05)
		  (plist-get colors :base04)))

	   (t
		(let ((maybe-color (plist-get colors (intern (concat ":" (symbol-name key))))))
		  (if maybe-color
			  maybe-color
			key))))
    key))

  
(defun base16-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key (car spec))
             (value (base16-transform-color-key (cadr spec) colors)))

        ;; Append the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (setq output (append output (list key (base16-transform-spec value colors)))))
         (t
          (setq output (append output (list key value))))))

      ;; Go to the next element in the list
      (setq spec (cddr spec)))

    ;; Return the transformed spec
    output))

(defun base16-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
         (definition       (cdr spec))
         (shell-colors-256 (pcase base16-theme-256-color-source
                             ("terminal"     base16-shell-colors)
                             ("base16-shell" base16-shell-colors-256)
                             ("colors"       colors)
                             (_              base16-shell-colors))))

    ;; This is a list of fallbacks to make us select the sanest option possible.
    ;; If there's a graphical terminal, we use the actual colors. If it's not
    ;; graphical, the terminal supports 256 colors, and the user enables it, we
    ;; use the base16-shell colors. Otherwise, we fall back to the basic
    ;; xresources colors.
    (list face `((((type graphic))   ,(base16-transform-spec definition colors))
                 (((min-colors 256)) ,(base16-transform-spec definition shell-colors-256))
                 (t                  ,(base16-transform-spec definition base16-shell-colors))))))

(defun base16-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (base16-transform-face face colors))
                 faces)))

(defun base16-theme-define (theme-name theme-colors)
  "Define the faces for a base16 colorscheme given a `THEME-NAME' and a plist of `THEME-COLORS'."
  (base16-set-faces
   theme-name
   theme-colors

   '(
;;; Built-in
;;;; basic colors
     (border                                       :background base03)
     (cursor                                       :background base08)
     (default                                      :foreground base05 :background base00)
     (fringe                                       :background base16-settings-fringe-bg)
     (gui-element                                  :background base01)
     (header-line                                  :foreground base0E :background nil :inherit mode-line)
     (highlight                                    :background base01)
     (link                                         :foreground base0D :underline t)
     (link-visited                                 :foreground base0E :underline t)
     (minibuffer-prompt                            :foreground base0D)
     (region                                       :background base02)
     (secondary-selection                          :background base03)
     (trailing-whitespace                          :foreground base0A :background base0C)
     (widget-button                                :underline t)
     (widget-field                                 :background base03 :box (:line-width 1 :color base06))

     (error                                        :foreground base08 :weight bold)
     (warning                                      :foreground base09 :weight bold)
     (success                                      :foreground base0B :weight bold)
     (shadow                                       :foreground base03)

;;;; compilation
     (compilation-column-number                    :foreground base0A)
     (compilation-line-number                      :foreground base0A)
     (compilation-message-face                     :foreground base0D)
     (compilation-mode-line-exit                   :foreground base0B)
     (compilation-mode-line-fail                   :foreground base08)
     (compilation-mode-line-run                    :foreground base0D)

;;;; custom
     (custom-variable-tag                          :foreground base0D)
     (custom-group-tag                             :foreground base0D)
     (custom-state                                 :foreground base0B)

;;;; font-lock
     (font-lock-builtin-face                       :foreground base0C)
     (font-lock-comment-delimiter-face             :foreground base02)
     (font-lock-comment-face                       :foreground base03)
     (font-lock-constant-face                      :foreground base09)
     (font-lock-doc-face                           :foreground base04)
     (font-lock-doc-string-face                    :foreground base03)
     (font-lock-function-name-face                 :foreground base0D)
     (font-lock-keyword-face                       :foreground base0E)
     (font-lock-negation-char-face                 :foreground base0B)
     (font-lock-preprocessor-face                  :foreground base0D)
     (font-lock-regexp-grouping-backslash          :foreground base0A)
     (font-lock-regexp-grouping-construct          :foreground base0E)
     (font-lock-string-face                        :foreground base0B)
     (font-lock-type-face                          :foreground base0A)
     (font-lock-variable-name-face                 :foreground base08)
     (font-lock-warning-face                       :foreground base08)

;;;; isearch
     (match                                        :foreground base0D :background base01 :inverse-video t)
     (isearch                                      :foreground base0A :background base01 :inverse-video t)
     (isearch-lazy-highlight-face                  :foreground base0C :background base01 :inverse-video t)
     (isearch-fail                                 :background base01 :inverse-video t :inherit font-lock-warning-face)

;;;; line-numbers
     (line-number                                  :foreground base03 :background base16-settings-fringe-bg)
     (line-number-current-line                     :inverse-video t)

;;; Third-party

;;;; cperl-mode
     (cperl-array-face                             :weight bold :inherit font-lock-variable-name-face)
     (cperl-hash-face                              :weight bold :slant italic :inherit font-lock-variable-name-face)
     (cperl-nonoverridable-face                    :inherit font-lock-builtin-face)

;;;; cscope-minor-mode
     (cscope-file-face                             :foreground base0B)
     (cscope-function-face                         :foreground base0D)
     (cscope-line-number-face                      :foreground base0A)
     (cscope-mouse-face                            :foreground base04 :background base01)
     (cscope-separator-face                        :foreground base08 :overline t :underline t :weight bold)

;;;; diff-hl-mode
     (diff-hl-change                               :foreground base0E)
     (diff-hl-delete                               :foreground base08)
     (diff-hl-insert                               :foreground base0B)

;;;; diff-mode
     (diff-added                                   :foreground base0B)
     (diff-changed                                 :foreground base0E)
     (diff-removed                                 :foreground base08)
     (diff-header                                  :background base01)
     (diff-file-header                             :background base02)
     (diff-hunk-header                             :foreground base0E :background base01)

;;;; dired+
     (diredp-compressed-file-suffix                :foreground base0D)
     (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
     (diredp-dir-priv                              :foreground base0C :background nil)
     (diredp-exec-priv                             :foreground base0D :background nil)
     (diredp-executable-tag                        :foreground base08 :background nil)
     (diredp-file-name                             :foreground base0A)
     (diredp-file-suffix                           :foreground base0B)
     (diredp-flag-mark-line                        :background nil :inherit highlight)
     (diredp-ignored-file-name                     :foreground base04)
     (diredp-link-priv                             :foreground base0E :background nil)
     (diredp-mode-line-flagged                     :foreground base08)
     (diredp-mode-line-marked                      :foreground base0B)
     (diredp-no-priv                               :background nil)
     (diredp-number                                :foreground base0A)
     (diredp-other-priv                            :foreground base0E :background nil)
     (diredp-rare-priv                             :foreground base08 :background nil)
     (diredp-read-priv                             :foreground base0B :background nil)
     (diredp-symlink                               :foreground base0E)
     (diredp-write-priv                            :foreground base0A :background nil)

;;;; ediff-mode
     (ediff-even-diff-A                            :foreground nil :background nil :inverse-video t)
     (ediff-even-diff-B                            :foreground nil :background nil :inverse-video t)
     (ediff-odd-diff-A                             :foreground base04 :background nil :inverse-video t)
     (ediff-odd-diff-B                             :foreground base04 :background nil :inverse-video t)

;;;; eshell
     (eshell-ls-archive                            :foreground base08)
     (eshell-ls-backup                             :foreground base0F)
     (eshell-ls-clutter                            :foreground base09)
     (eshell-ls-directory                          :foreground base0D)
     (eshell-ls-executable                         :foreground base0B)
     (eshell-ls-missing                            :foreground base08)
     (eshell-ls-product                            :foreground base0F)
     (eshell-ls-readonly                           :foreground base06)
     (eshell-ls-special                            :foreground base0E)
     (eshell-ls-symlink                            :foreground base0C)
     (eshell-ls-unreadable                         :foreground base04)
     (eshell-prompt                                :foreground base05)

;;;; flycheck-mode
     (flycheck-error                               :underline (:style wave :color base08))
     (flycheck-info                                :underline (:style wave :color base0B))
     (flycheck-warning                             :underline (:style wave :color base09))

;;;; grep
     (grep-context-face                            :foreground base04)
     (grep-error-face                              :foreground base08 :weight bold :underline t)
     (grep-hit-face                                :foreground base0D)
     (grep-match-face                              :foreground nil :background nil :inherit match)

;;;; highlight-indentation minor mode
     (highlight-indentation-face                   :background base01)

;;;; hl-line-mode
     (hl-line                                      :background base01)
     (col-highlight                                :background base01)

;;;; hl-sexp-mode
     (hl-sexp-face                                 :background base03)

;;;; ido-mode
     (ido-subdir                                   :foreground base04)
     (ido-first-match                              :foreground base09 :weight bold)
     (ido-only-match                               :foreground base08 :weight bold)
     (ido-indicator                                :foreground base08 :background base01)
     (ido-virtual                                  :foreground base04)

;;;; js2-mode
     (js2-warning-face                             :underline base09)
     (js2-error-face                               :foreground nil :underline base08)
     (js2-external-variable-face                   :foreground base0E)
     (js2-function-param-face                      :foreground base0D)
     (js2-instance-member-face                     :foreground base0D)
     (js2-private-function-call-face               :foreground base08)

;;;; js3-mode
     (js3-warning-face                             :underline base09)
     (js3-error-face                               :foreground nil :underline base08)
     (js3-external-variable-face                   :foreground base0E)
     (js3-function-param-face                      :foreground base0D)
     (js3-jsdoc-tag-face                           :foreground base09)
     (js3-jsdoc-type-face                          :foreground base0C)
     (js3-jsdoc-value-face                         :foreground base0A)
     (js3-jsdoc-html-tag-name-face                 :foreground base0D)
     (js3-jsdoc-html-tag-delimiter-face            :foreground base0B)
     (js3-instance-member-face                     :foreground base0D)
     (js3-private-function-call-face               :foreground base08)

;;;; linum-mode
     (linum                                        :foreground base03 :background base16-settings-fringe-bg)

;;;; magit
     (magit-blame-culprit                          :background base01)
     (magit-blame-heading                          :background base01 :foreground base05)
     (magit-branch                                 :foreground base04 :weight bold)
     (magit-branch-current                         :foreground base0C :weight bold :box t)
     (magit-branch-local                           :foreground base0C :weight bold)
     (magit-branch-remote                          :foreground base0B :weight bold)
     (magit-diff-context-highlight                 :background base01 :foreground base05)
     (magit-diff-file-header                       :background base01 :foreground base05)
     (magit-hash                                   :foreground base0D)
     (magit-header-line                            :background base02 :foreground base05 :weight bold)
     (magit-hunk-heading                           :background base03)
     (magit-hunk-heading-highlight                 :background base03)
     (magit-diff-hunk-heading                      :background base01)
     (magit-diff-hunk-heading-highlight            :background base01)
     (magit-item-highlight                         :background base01)
     (magit-log-author                             :foreground base0D)
     (magit-section-highlight                      :background base01)
     (magit-tag                                    :foreground base05)
;;;; mark-multiple
     (mm/master-face                               :foreground nil :background nil :inherit region)
     (mm/mirror-face                               :foreground nil :background nil :inherit region)

;;;; markdown-mode
     (markdown-url-face                            :inherit link)
     (markdown-link-face                           :foreground base0D :underline t)

;;;; message-mode
     (message-header-other                         :foreground nil :background nil :weight normal)
     (message-header-subject                       :foreground base0A :weight bold :inherit message-header-other)
     (message-header-to                            :foreground base09 :weight bold :inherit message-header-other)
     (message-header-cc                            :foreground nil :inherit message-header-to)
     (message-header-name                          :foreground base0D :background nil)
     (message-header-newsgroups                    :foreground base0C :background nil :slant normal)
     (message-separator                            :foreground base0E)

;;;; org-mode
     (org-agenda-structure                         :foreground base0E)
     (org-agenda-date                              :foreground base0D :underline nil)
     (org-agenda-done                              :foreground base0B)
     (org-agenda-dimmed-todo-face                  :foreground base04)
     (org-block                                    :foreground base09)
     (org-code                                     :foreground base0A)
     (org-column                                   :background base01)
     (org-column-title                             :weight bold :underline t :inherit org-column)
     (org-date                                     :foreground base0E :underline t)
     (org-document-info                            :foreground base0C)
     (org-document-info-keyword                    :foreground base0B)
     (org-document-title                           :foreground base09 :weight bold :height 1.44)
     (org-done                                     :foreground base0B)
     (org-ellipsis                                 :foreground base04)
     (org-footnote                                 :foreground base0C)
     (org-formula                                  :foreground base08)
     (org-hide                                     :foreground base03)
     (org-link                                     :foreground base0D)
     (org-scheduled                                :foreground base0B)
     (org-scheduled-previously                     :foreground base09)
     (org-scheduled-today                          :foreground base0B)
     (org-special-keyword                          :foreground base09)
     (org-table                                    :foreground base0E)
     (org-todo                                     :foreground base08)
     (org-upcoming-deadline                        :foreground base09)
     (org-warning                                  :foreground base08 :weight bold)

;;;; paren-face-mode
     (paren-face                                   :foreground base04 :background nil)

;;;; popup
     (popup-face                                   :foreground base05 :background base02)
     (popup-isearch-match                          :foreground base00 :background base0B)
     (popup-scroll-bar-background-face             :background base03)
     (popup-scroll-bar-foreground-face             :background base05)
     (popup-summary-face                           :foreground base04)
     (popup-tip-face                               :foreground base00 :background base0A)
     (popup-menu-mouse-face                        :foreground base00 :background base0D)
     (popup-menu-selection-face                    :foreground base00 :background base0C)

;;;; python-mode
     (py-builtins-face                             :foreground base09 :weight normal)

;;;; regex-tool
     (regex-tool-matched-face                      :foreground nil :background nil :inherit match)

;;;; sh-mode
     (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
     (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

;;;; show-paren-mode
     (show-paren-match                             :foreground base01 :background base0D)
     (show-paren-mismatch                          :foreground base01 :background base09)

;;;; term and ansi-term
     (term                                         :foreground base05 :background base00)
     (term-color-black                             :foreground base02 :background base00)
     (term-color-white                             :foreground base05 :background base07)
     (term-color-red                               :foreground base08 :background base08)
     (term-color-yellow                            :foreground base0A :background base0A)
     (term-color-green                             :foreground base0B :background base0B)
     (term-color-cyan                              :foreground base0C :background base0C)
     (term-color-blue                              :foreground base0D :background base0D)
     (term-color-magenta                           :foreground base0E :background base0E)

;;;; tuareg-mode
     (tuareg-font-lock-governing-face              :weight bold :inherit font-lock-keyword-face)

;;;; undo-tree-mode
     (undo-tree-visualizer-default-face            :foreground base06)
     (undo-tree-visualizer-current-face            :foreground base0B :weight bold)
     (undo-tree-visualizer-active-branch-face      :foreground base08)
     (undo-tree-visualizer-register-face           :foreground base0A)

;;;; utop-mode
     (utop-prompt                                  :foreground base0E)
     (utop-error                                   :underline (:style wave :color base08) :inherit error)

;;;; w3m-mode
     (w3m-anchor                                   :underline nil :inherit link)
     (w3m-anchor-visited                           :underline nil :inherit link-visited)
     (w3m-form                                     :foreground base09 :underline t)
     (w3m-image                                    :foreground base05 :background base03)
     (w3m-image-anchor                             :foreground base05 :background base03 :underline t)
     (w3m-header-line-location-content             :foreground base0D :background base00)
     (w3m-header-line-location-title               :foreground base0D :background base00)
     (w3m-tab-background                           :foreground base05 :background base01)
     (w3m-tab-selected                             :foreground base05 :background base00)
     (w3m-tab-selected-retrieving                  :foreground base05 :background base00)
     (w3m-tab-unselected                           :foreground base03 :background base01)
     (w3m-tab-unselected-unseen                    :foreground base03 :background base01)
     (w3m-tab-unselected-retrieving                :foreground base03 :background base01)

;;;; which-func-mode
     (which-func                                   :foreground base0D :background nil :weight bold)

;;;; whitespace-mode
     (whitespace-empty                             :foreground base08 :background base0A)
     (whitespace-hspace                            :foreground base04 :background base04)
     (whitespace-indentation                       :foreground base08 :background base0A)
     (whitespace-line                              :foreground base0F :background base01)
     (whitespace-newline                           :foreground base04)
     (whitespace-space                             :foreground base03 :background base01)
     (whitespace-space-after-tab                   :foreground base08 :background base0A)
     (whitespace-space-before-tab                  :foreground base08 :background base09)
     (whitespace-tab                               :foreground base03 :background base01)
     (whitespace-trailing                          :foreground base0A :background base08)))

  ;; Anything leftover that doesn't fall neatly into a face goes here.
  (let ((base00 (plist-get theme-colors :base00))
        (base01 (plist-get theme-colors :base01))
        (base02 (plist-get theme-colors :base02))
        (base03 (plist-get theme-colors :base03))
        (base04 (plist-get theme-colors :base04))
        (base05 (plist-get theme-colors :base05))
        (base06 (plist-get theme-colors :base06))
        (base07 (plist-get theme-colors :base07))
        (base08 (plist-get theme-colors :base08))
        (base09 (plist-get theme-colors :base09))
        (base0A (plist-get theme-colors :base0A))
        (base0B (plist-get theme-colors :base0B))
        (base0C (plist-get theme-colors :base0C))
        (base0D (plist-get theme-colors :base0D))
        (base0E (plist-get theme-colors :base0E))
        (base0F (plist-get theme-colors :base0F)))
    (custom-theme-set-variables
     theme-name
     `(ansi-color-names-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05])
     `(ansi-term-color-vector
       ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
       [unspecified ,base00 ,base08 ,base0B ,base0A ,base0D ,base0E ,base0D ,base05]))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(defvar gattaca-colors
  '(:base00 "#000000"
    :base01 "#282a2b"
    :base02 "#3b758c"
    :base03 "#41535b"
    :base04 "#43a5d5"
    :base05 "#d6d6d6"
    :base06 "#eeeeee"
    :base07 "#ffffff"
    :base08 "#cd3f45"
    :base09 "#db7b55"
    :base0A "#e6cd69"
    :base0B "#9fca56"
    :base0C "#55dbbe"
    :base0D "#55b5db"
    :base0E "#a074c4"
    :base0F "#8a553f")
  "All colors for Base16 Seti UI are defined here.")

;; Define the theme
(deftheme gattaca)

;; Add all the faces to the theme
(base16-theme-define 'gattaca gattaca-colors)

;; Mark the theme as provided
(provide-theme 'gattaca)

(provide 'gattaca-theme)

;;; gattaca-theme.el ends here
