;; init-edit.el --- Initialize editing configurations.  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Changelog:
;;
;; 2019 04 28 merge from init+settings.el

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Set the default formatting styles for various C based modes
(setq c-default-style
      '((awk-mode . "awk")
        (other . "java")))

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

;;(transient-mark-mode t)
(setq select-enable-clipboard t)

;; Automatically update unmodified buffers whose files have changed.
(global-auto-revert-mode 1)

;; Make compilation buffers scroll to follow the output, but stop scrolling
;; at the first error.
(setq compilation-scroll-output 'first-error)

;; echo keystrokes ; no dialog boxes ; visable bell ; highlight parens
(setq echo-keystrokes 0.1)
(setq use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

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

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Miscs

;; scratch buffer
(setq initial-scratch-message "")
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  "Bury the *scratch* buffer, but never kill it."
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;; uniquify settings
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save

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


(setq-default major-mode 'text-mode)

;; Sentences do not need double spaces to end. Period.
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;; indentation & CodeStyle
(setq-default tab-width 2
              indent-tabs-mode nil
              fill-column 80)
;; Line and Column
(setq column-number-mode t)
(setq line-number-mode t)

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

;; Do not delete selection if you insert
(use-package delsel
  :ensure nil
  :config (setq-default delete-selection-mode nil))

;; set built in regex helper to string format
(use-package re-builder
  :ensure nil
  :config (setq reb-re-syntax 'string))

;; Rectangle
(use-package rect
  :ensure nil)

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Quickly follow links
(use-package ace-link
  :bind (:map sej-mode-map
              ("H-o" . ace-link-addr))
  :hook (after-init . ace-link-setup-default))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (:map sej-mode-map
              ("C-c C-z ." . browse-url-at-point)
              ("C-c C-z b" . browse-url-of-buffer)
              ("C-c C-z r" . browse-url-of-region)
              ("C-c C-z u" . browse-url)
              ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map)))

;; Click to browse URL or to send to e-mail address
(use-package goto-addr
  :ensure nil
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

;; for selecting a window to switch to
(use-package ace-window
  :bind (:map sej-mode-map
              ("M-o" . ace-window)
              ("C-x M-o" . ace-swap-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; crux - smart moving to beginning of line or to beginning of text on line
(use-package crux
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c o" . crux-open-with)
              ("C-k" . crux-smart-kill-line)
              ("C-S-RET" . crux-smart-open-line-above)
              ([(shift return)] . crux-smart-open-line)
              ("C-c n" . crux-cleanup-buffer-or-region)
              ("C-c u" . crux-view-url)
              ("C-c C-d" . crux-delete-file-and-buffer)
              ("s-d" . crux-duplicate-current-line-or-region)
              ("C-c C-k" . crux-duplicate-current-line-or-region)
              ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
              ([remap kill-whole-line] . crux-kill-whole-line)
              ("C-<backspace>" . crux-kill-line-backwards))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (crux-reopen-as-root-mode))

(use-package mwim
  :bind (:map sej-mode-map
              ("C-a" . mwim-beginning)
              ("C-e" . mwim-end))) ; better than crux

;; underscore -> upcase -> camelcase conversion
(use-package string-inflection
  :bind (:map sej-mode-map
              ("M-u" . my-string-inflection-all-cycle)))

;; buffer-move to swap buffers between windows
(use-package buffer-move)

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (:map sej-mode-map
              ("C-:" . avy-goto-char)
              ("C-'" . avy-goto-char-2)
              ("M-g f" . avy-goto-line)
              ("M-g w" . avy-goto-word-1)
              ;; ("C-<return>" . avy-goto-word-1)
              ("s-." . avy-goto-word-0)
              ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-background t))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (:map sej-mode-map
              ("M-z" . avy-zap-to-char-dwim)
              ("M-Z" . avy-zap-up-to-char-dwim)))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; FIXME: Disable in big files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (if (> (buffer-size) (* 3000 80))
                            (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(asm-mode web-mode html-mode css-mode robot-mode go-mode))
    (push mode aggressive-indent-excluded-modes))

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (or (derived-mode-p 'c-mode)
             (derived-mode-p 'c++-mode)
             (derived-mode-p 'csharp-mode)
             (derived-mode-p 'java-mode)
             (derived-mode-p 'go-mode)
             (derived-mode-p 'swift-mode))
         (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                             (thing-at-point 'line))))))

;; Display incremental search stats in the modeline.
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)) ; C-; and  M-;

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

;; A saner ediff
(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-diff-options "-w")
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-layout-mode t)
  (electric-indent-mode t)
  ;; Ignore electric indentation for python and yaml
  (defun electric-indent-ignore-mode (char)
    "Ignore electric indentation for 'python-mode'.  CHAR is input character."
    (if (or (equal major-mode 'python-mode)
            (equal major-mode 'yaml-mode))
        'no-indent
      nil))
  (add-hook 'electric-indent-functions 'electric-indent-ignore-mode))


;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind ((:map sej-mode-map
               ("A-;" . iedit-mode)
               ("C-x r RET" . iedit-rectangle-mode))
         (:map isearch-mode-map ("A-;" . iedit-mode-from-isearch))
         (:map esc-map ("A-;" . iedit-execute-last-modification))
         (:map help-map ("A-;" . iedit-mode-toggle-on-function)))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))

;; Increase selected region by semantic units
(use-package expand-region
  :bind (:map sej-mode-map
              ("C-=" . er/expand-region)))

;; Multiple cursors
(use-package multiple-cursors
  :bind ((:map sej-mode-map
               ("C-S-c C-S-c"   . mc/edit-lines)
               ("C->"           . mc/mark-next-like-this)
               ("C-<"           . mc/mark-previous-like-this)
               ("C-c C-<"       . mc/mark-all-like-this)
               ("C-M->"         . mc/skip-to-next-like-this)
               ("C-M-<"         . mc/skip-to-previous-like-this)
               ("s-<mouse-1>"   . mc/add-cursor-on-click)
               ("C-S-<mouse-1>" . mc/add-cursor-on-click))
         (:map mc/keymap
               ("C-|" . mc/vertical-align-with-space))))

;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))

;; Make bindings that stick around
(use-package hydra)

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (:map sej-mode-map
              ("C-." . imenu)))

;; Treat undo history as a tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :bind (:map sej-mode-map
              ("C-/" . undo-tree-undo)
              ("C-?" . undo-tree-redo)
              ("C-x u" . undo-tree-visualize)
              ("C-x r u" . undo-tree-save-state-to-register)
              ("C-x r U" . undo-tree-save-state-from-register))
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil
              undo-tree-history-directory-alist
              `(("." . ,(locate-user-emacs-file "undo-tree-hist/"))))
  :config
  ;; ;; FIXME:  `undo-tree-visualizer-diff' is a local variable in *undo-tree* buffer.
  ;; (defun undo-tree-visualizer-show-diff (&optional node)
  ;;   ;; show visualizer diff display
  ;;   (setq-local undo-tree-visualizer-diff t)
  ;;   (let ((buff (with-current-buffer undo-tree-visualizer-parent-buffer
  ;;                 (undo-tree-diff node)))
  ;;         (display-buffer-mark-dedicated 'soft)
  ;;         win)
  ;;     (setq win (split-window))
  ;;     (set-window-buffer win buff)
  ;;     (shrink-window-if-larger-than-buffer win)))

  ;; (defun undo-tree-visualizer-hide-diff ()
  ;;   ;; hide visualizer diff display
  ;;   (setq-local undo-tree-visualizer-diff nil)
  ;;   (let ((win (get-buffer-window undo-tree-diff-buffer-name)))
  ;; (when win (with-selected-window win (kill-buffer-and-window)))))
  )

(use-package goto-chg
  :ensure t
  :defines sej-mode-map
  :bind ("C-," . goto-last-change))

;; redefine M-< and M-> for some modes
(use-package beginend               ; smart M-< & M->
  :ensure t
  :defer 2
  :config
  (beginend-global-mode)
  )

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode))
  :config
  ;; this makes forward-word & backward-word understand snake & camel case
  (setq c-subword-mode t)
  (global-subword-mode t))

;; Flexible text folding
(use-package origami
  :hook (prog-mode . origami-mode)
  :init (setq origami-show-fold-header t)
  :bind (:map origami-mode-map
              ("A-`" . hydra-origami/body))
  ;; DONE conflict with sej/push-mark-no-activate
  :config
  (face-spec-reset-face 'origami-fold-header-face)

  (when sej-lsp
    ;; Support LSP
    (use-package lsp-origami
      :hook (origami-mode . (lambda ()
                              (if lsp-mode
                                  (lsp-origami-mode))))))

  (defhydra hydra-origami (:color blue :hint none)
    "
^Node^                     ^Other^
^^─────────────────────────^^────────────
_:_: toggle recursively    _u_: undo
_a_: toggle all            _r_: redo
_t_: toggle current        _R_: reset
_o_: only show current
"
    (":" origami-recursively-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("t" origami-toggle-node)
    ("o" origami-show-only-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("R" origami-reset)))

;; Narrow/Widen
;; C-x n prefix
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

(use-package ethan-wspace
  :commands global-ethan-wspace-mode
  :diminish ethan-wspace-mode
  :hook (after-init . global-ethan-wspace-mode)
  :bind ("A-w" . ethan-wspace-clean-all))
;; DONE conflict with hydra-frame-window-body

;; dtrt-indent to automatically set the right indent for other people's files
(use-package dtrt-indent
  :defer 2
  :diminish
  :config
  ;; (setq dtrt-indent-active-mode-line-info "")
  )

(use-package aggressive-indent
  :hook (after-init . global-aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; ;; simplified access to the system clipboard in Emacs
;; (use-package simpleclip
;;   :ensure t
;;   :hook (after-init . simpleclip-mode)
;;   :bind (:map sej-mode-map
;;               ("s-x" . simpleclip-cut)
;;               ("s-c" . simpleclip-copy)
;;               ("s-v" . simpleclip-paste)
;;               ("C-S-v" . scroll-down-command)
;;               ("H-v" . scroll-down-command)
;;               ("M-v" . scroll-down-command)
;;               )) ;; this last one will help integration with Flycut


(provide 'init-edit)
;;; init-edit.el ends here
