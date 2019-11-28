;;; init.el --- SeJ Emacs configurations. -*- lexical-binding: t no-byte-compile: t; -*-

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
;; SeJ Emacs configurations.
;;

;;; Changelog
;;
;; 2019 04 28 Merge from old .emacs.d
;; 2019 10 20 remove helm stuff; remove most messages
;; 2019 10 22 start to tangle in init-org.org
;; 2019 11 27 remove untangled files

;;; Code:
(message "Emacs start")

;; debugger
;; (setq debug-on-error nil)
;; (setq debug-on-quit nil)

;; org-tangle load-file
(org-babel-load-file (concat user-emacs-directory "init-org.org"))

(message "init.el ends here")
;;; init.el ends here
