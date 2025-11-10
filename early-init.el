;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Stephen Jenkins

;; Author: Stephen Jenkins
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
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;;;;; Package-enable prevent early
;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
;; (setq package-enable-at-startup nil)

;;;;; Garbage collection
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))
(add-hook 'after-init-hook #'garbage-collect t)

;;;;; gccemacs
;; Native Compilation Vars
(setq-default native-comp-speed 2
              native-comp-deferred-compilation t
              native-comp-async-report-warnings-errors 'silent)

;;;;; frame settings early to prevent clutter
(modify-all-frames-parameters '((width . 80)
                                (height . 50)
                                (left . 0)
                                (right . 0)
                                (left-fringe . 4)
                                (right-fringe . 4)
                                (internal-border-width . 1)
                                (child-frame-border-width . 2)
                                (vertical-scroll-bars . nil)
                                (horizontal-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (ns-appearance . dark)
                                (undecorated . t)
                                (undecorated-round . t)
                                ))
(unless (display-graphic-p)
  (add-to-list 'default-frame-alist '(menu-bar-mode . 0) ))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-x-resources t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

(tool-bar-mode -1)

(message "early-init done")
(provide 'early-init)
;;; early-init.el ends here
