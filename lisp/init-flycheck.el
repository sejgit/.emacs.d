;; init-flycheck.el --- Initialize flycheck.	-*- lexical-binding: t -*-

;; Copyright (C) 2019

;; Author: Stephen Jenkins <stephenearljenkins@gmail.com>
;; URL: https://github.com/sejgit/.emacs.d

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
;; Flycheck configurations.
;;

;;; ChangeLog:
;;
;; 2016 12 16
;; 2017 01 06 change from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 24 add flymake to this file
;; 2017 08 28 add some flycheck settings & helm-flycheck
;; 2017 08 29 map to sej-mode-hook
;; 2017 80 30 update some binds
;; 2017 12 01 update for new use-package
;; 2018 08 06 helm-flycheck keymap changes
;; 2019 05 02 Initialize & Merge


;;; Code:

(eval-when-compile
  (require 'init-const))

(use-package flymake
  :ensure t
  :defines sej-mode-map
  :hook (post-command . flymake-error-at-point)
  :bind (:map sej-mode-map
              ("H-[" . flymake-goto-prev-error)
              ("H-]" . flymake-goto-next-error))
  :init
  (defun flymake-error-at-point ()
    "Show the flymake error in the minibuffer when point is on an invalid line."
    (when (get-char-property (point) 'flymake-overlay)
      (let ((help (get-char-property (point) 'help-echo)))
        (if help (message "%s" help))))))

(use-package flycheck
  :diminish flycheck-mode
  :defines sej-mode-map
  :hook (after-init . global-flycheck-mode)
  :bind
  (:map sej-mode-map
        ("s-[" . flycheck-previous-error)
        ("s-]" . flycheck-next-error)
        ("C-c f" . flycheck-list-errors)
        ("s-f" . flycheck-list-errors)        )
  :config
  (global-flycheck-mode 1)
  (defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
    (push-mark))

  (setq flycheck-indication-mode 'right-fringe
        flycheck-check-syntax-automatically '(save mode-enabled))
  (custom-set-faces
   '(flycheck-error ((((class color)) (:underline "Red"))))
   '(flycheck-warning ((((class color)) (:underline "Orange")))))

  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8-maximum-line-length 79)
  (setq flycheck-highlighting-mode 'lines)
  (progn    (set-face-attribute 'flycheck-warning nil
                                :inherit 'warning
                                :underline nil)
            (set-face-attribute 'flycheck-error nil
                                :inherit 'error
                                :underline nil)))
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Display Flycheck errors in GUI tooltips
(if (display-graphic-p)
    (if emacs/>=26p
        (use-package flycheck-posframe
          :hook (flycheck-mode . flycheck-posframe-mode)
          :config (add-to-list 'flycheck-posframe-inhibit-functions
                               #'(lambda () (bound-and-true-p company-backend))))

      (use-package flycheck-pos-tip
        :defines (flycheck-pos-tip-timeout flycheck-pos-tip-error-messages)
        :hook (global-flycheck-mode . flycheck-pos-tip-mode)
        :config
        (setq flycheck-pos-tip-timeout 10
              flycheck-display-errors-delay 0.5)
        (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

  (use-package flycheck-popup-tip
    :hook (flycheck-mode . flycheck-popup-tip-mode)))

(use-package flycheck-color-mode-line
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package helm-flycheck
  :bind (:map sej-mode-map
              ("C-c s h" . helm-flycheck)
              ("H-f" . helm-flycheck)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
