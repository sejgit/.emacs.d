;;; init-package.el --- Initialize package configurations.  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

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
;; Emacs Package management configurations.
;;

;;; Changelog
;;
;; 2019 04 28 Init & merge


;;; Code:

(eval-when-compile
  (require 'init-custom))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq load-prefer-newer t)

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Extensions
(use-package paradox
  :init
  (setq paradox-execute-asynchronously nil)
  (setq paradox-github-token t)
  (setq paradox-display-star-count nil)

  (defalias #'upgrade-packages #'paradox-upgrade-packages)

  ;; Replace default `list-packages'
  (defadvice list-packages (before my-list-packages activate)
    (paradox-enable))
  )

;; The EMACS environment variable being set to the binary path of emacs.
(setenv "EMACS"
        (file-truename (expand-file-name invocation-name invocation-directory)))

;; check OS type
(when
    sys/win32p
  (progn
    (message "Microsoft Windows")
    ;;see if we can get some speed improvements
    (use-package auto-compile
      :demand t
      :config
      (progn
        (auto-compile-on-load-mode)
        (auto-compile-on-save-mode)))

    ;; set exec-path for latex installation
    (setq exec-path (append exec-path sej-latex-directory))

    ;; load AutoHotkey mode
    (load-library "xahk-mode")))

(provide 'init-package)
;;; init-package.el ends here
