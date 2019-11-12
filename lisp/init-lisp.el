;;; init-lisp.el --- elisp setups.	-*- lexical-binding: t; -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d
;; Version: 1.0
;; Keywords: .emacs.d sejgit

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
;; settings for elisp

;;; ChangeLog
;;
;; 2017 08 25 init SeJ
;; 2017 08 28 added some paredit, eldoc, & ielm settings from EOS
;; 2017 08 30 clean up some comments & defers
;; 2017 09 20 move autocomplete from init-autocomplete.el & delete file
;;            move paredit defun from init-bindings-settings.el
;; 2017 11 18 swap paredit for smartparens
;; 2017 12 01 some mods for use-package & removal of autocomplete
;; 2018 04 04 adding abo-abo's lispy package for specifically better reading of LISP
;; 2018 09 24 some additions from ahai
;; 2019 10 05 add lispy remove par-edit

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(define-key emacs-lisp-mode-map (kbd "C-c D") 'toggle-debug-on-error)

(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode))

;; we don't want this minor mode to be shown in the minibuffer, however
;; we use eldoc to show the signature of the function at point in the minibuffer
(use-package eldoc
  :diminish eldoc-mode
  :hook
  ((emacs-lisp-mode . eldoc-mode)
   (ielm-mode . eldoc-mode)
   (lisp-interaction-mode . eldoc-mode)
   (eval-expression-minibuffer-setup . eldoc-mode))
  :config
  (setq eldoc-idle-delay 0.1))

;; add a nice popup for ielm
(defun ielm-other-window ()
  "Run ielm on other window."
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "H-i") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "H-i") 'ielm-other-window)

;; use flycheck in elisp
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; enable dash for Emacs lisp highlighting
(eval-after-load "dash" '(dash-enable-font-lock))


;; turn on elisp-slime-nav if available so M-. works to jump to function definitions
(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :hook (emacs-lisp-mode . elisp-slime-nav-mode))

;; Use C-M-. to jump to the definition of the symbol under the cursor.
;; (define-key emacs-lisp-mode-map (kbd "C-M-.") 'find-function-at-point)

;; When saving an elisp file, remove its compiled version if
;; there is one, as you'll want to recompile it.
(defun sej/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))
(add-hook 'emacs-lisp-mode-hook 'sej/remove-elc-on-save)

;; Enable eldoc mode, which provides context based documentation
;; in the minibuffer.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;; eros-mode will show you the result of evaluating an elisp command
;; as an overlay in your elisp buffer. Try it out with C-x C-e now!
(use-package eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode))

(provide 'init-lisp)
;;; init-lisp.el ends here
