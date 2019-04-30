;;; init+settings.el --- Emacs settings
;;; Commentary:
;; Main file for initial settings.

;;; ChangeLog:
;; 2016 12 16 init SeJ
;; 2016 12 21 add kill-this-buffer
;; 2017 01 06 cleanup by move of packages to init-misc-pkgs.el
;; 2017 01 06 change from req-package to use-package
;; 2017 01 11 add more pragmatic Emacs tips
;; 2017 01 12 add steve drunken tips
;; 2017 01 30 add sudo-edit function (C-x C-r) to edit file as sudo
;; 2017 03 29 add truncate lines setting
;; 2017 05 09 add copy-line C-c C-k
;;        add some neat keybindings from emacs-starter-kit
;;        rename file to init-bindings-settings.el
;; 2017 05 12 adds from purcell/emacs.d
;; 2017 05 21 add delete to trash can
;; 2017 05 25 add imenu binding
;; 2017 05 30 hippie-expand remap M-/ C-M-/
;; 2017 06 05 add sej-map keyboard mapping
;; 2017 08 07 add Hyper & Super keys for osx and win remove sej-map
;; 2017 08 21 add some Lisp stuff from Getting Started with Emacs Lisp
;; 2017 08 29 take another run at sej-map
;; 2017 09 08 add code to unset C- M- digit keys
;; 2017 09 18 add goto-line with temp line numbers
;; 2017 09 19 add transpose keybindings & others from magnar
;; 2017 09 20 make more pure settings & move others stuff out
;; 2017 09 24 some ohai tips

;;; Code:

;; indentation & CodeStyle
(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 80)
;; Javascript
(setq-default js2-basic-offset 2)
;; JSON
(setq-default js-indent-level 2)
;; Coffeescript
(setq coffee-tab-width 2)
;; Typescript
(setq typescript-indent-level 2
      typescript-expr-indent-offset 2)
;; Python
(setq-default py-indent-offset 2)
;; XML
(setq-default nxml-child-indent 2)
;; C
(setq-default c-basic-offset 2)
;; HTML etc with web-mode
(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-style-padding 2
              web-mode-script-padding 2)

;; Set the default formatting styles for various C based modes
(setq c-default-style
      '((awk-mode . "awk")
        (other . "java")))

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; do/don't indicate empty or end of a buffer
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries t)
(setq-default show-trailing-whitespace nil)
(setq-default mode-require-final-newline nil)
(setq-default require-final-newline nil)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 3)

;; each line of text gets one line on the screen
(setq-default truncate-lines 1)
(setq font-lock-maximum-decoration t
      truncate-partial-width-windows 1)

;; ignore case when searching
(setq-default case-fold-search 1)

;; add a new line when going to the next line
(setq next-line-add-newlines t)

;; marking text and clipboard settings
(delete-selection-mode nil)
;;(transient-mark-mode t)
(setq select-enable-clipboard t)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; electric-pair-mode
(electric-pair-mode t)
(electric-layout-mode t)
(electric-indent-mode t)
;; Ignore electric indentation for python and yaml
(defun electric-indent-ignore-mode (char)
  "Ignore electric indentation for 'python-mode'.  CHAR is input character."
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'yaml-mode))
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-mode)

;; Add proper word wrapping
(global-visual-line-mode t)
(setq line-move-visual t)

(setq-default backup-directory-alist
        '(("." . ".saves")))    ; don't litter my fs tree

(setq vc-make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . ".saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; delete to trash can
(setq delete-by-moving-to-trash t)

;; remove kill buffer with live process prompt
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
      kill-buffer-query-functions))

(setq-default kill-read-only-ok t)

;; hide mouse while typing
(setq make-pointer-invisible t)


;; color codes
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
;; https://github.com/dakrone/eos/blob/master/eos.org
(setq save-interprogram-paste-before-kill t)

;; uniquify settings
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; scratch buffer
(setq initial-scratch-message "")

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; this makes forward-word & backward-word understand snake & camel case
(setq c-subword-mode t)
(global-subword-mode)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(setq set-mark-command-repeat-pop t)


(provide 'init+settings)
;;; init+settings.el ends here
