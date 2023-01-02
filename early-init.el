;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

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
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(defvar default-file-name-handler-alist file-name-handler-alist)
(defvar extended-gc-cons-threshold most-positive-fixnum)
(defvar default-gc-cons-threshold (* 100 1024 1024))


;; for gccemacs
;; Native Compilation Vars
(setq-default native-comp-speed 2
              native-comp-deferred-compilation t)

;; Prevents libgccjit error
;; Solution found at: https://github.com/d12frosted/homebrew-emacs-plus/issues/323
(if (eq system-type 'darwin)
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc@12/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/12:/usr/local/opt/gcc@12/lib/gcc/12/gcc/x86_64-apple-darwin21/12"))

(setq auto-window-vscroll nil
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              frame-inhibit-implied-resize t
              inhibit-default-init t
              site-run-file nil
              load-prefer-newer t
              read-process-output-max (* 1024 1024 3))

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


;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (setq menu-bar-mode nil))
(modify-all-frames-parameters '((width . 80)
                                (height . 50)
                                (left . 0)
                                (right . 0)
                                (internal-border-width . 1)
                                (vertical-scroll-bars . nil)
                                (tool-bar-lines . 0)
                                (ns-appearance . dark)
                                ;; (font . "Iosevka-14")
                                ))

;;; early-init.el ends here
