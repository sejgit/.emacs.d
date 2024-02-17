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
(setq package-enable-at-startup nil)

;;;;; Garbage collection
;; Defer garbage collection further back in the startup process
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar extended-gc-cons-threshold most-positive-fixnum)
(defvar default-gc-cons-threshold (* 100 1024 1024))

(setq file-name-handler-alist nil
      gc-cons-threshold extended-gc-cons-threshold)

(defun sej/return-gc-to-default ()
  "Take garbage collection back to default."
  (setq-default gc-cons-threshold default-gc-cons-threshold
                load-prefer-newer nil))

(defun sej/reset-file-handler-alist-h ()
  "Take file name handler back to default."
  (dolist (handler file-name-handler-alist)
    (add-to-list 'default-file-name-handler-alist handler))
  (setq file-name-handler-alist default-file-name-handler-alist))

(add-hook 'after-init-hook #'sej/reset-file-handler-alist-h)
(add-hook 'after-init-hook #'sej/return-gc-to-default)
(advice-add #'package--ensure-init-file :override #'ignore)

;;;;; gccemacs
;; Native Compilation Vars
(setq-default native-comp-speed 2
              native-comp-deferred-compilation t
              native-comp-async-report-warnings-errors 'silent)

;;;;; Prevents libgccjit error (may not still be needed)
;; Solution found at: https://github.com/d12frosted/homebrew-emacs-plus/issues/323
;; (if (eq system-type 'darwin)
;; (setenv "LIBRARY_PATH" "/usr/local/opt/gcc@12/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc@12/lib/gcc/12/gcc/x86_64-apple-darwin21/12"))

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
  (add-to-list 'default-frame-alist '(menu-bar-mode . 0)))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-buffer-menu t)

(provide 'early-init)
;;; early-init.el ends here
