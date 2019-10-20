;;; init-registers.el --- Set up registers.	-*- lexical-binding: t no-byte-compile: t; -*-

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
;; Registers allow you to jump to a file or other location
;; quickly.  Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

;;; ChangeLog:
;;
;; 2017 05 09 init copied from Part of the Emacs Starter Kit
;; 2017 08 30 add s-r to sej-mode-map
;; 2018 03 21 added comments refering to register save and helm
;; 2018 08 02 removed commas from list
;; 2018 10 16 mod registers

;;; Code:

;; this is defined in init+bindings.el (kbd "s-r") 'jump-to-register
(set-register ?b '(file . "~/.ssh/custom-post.el"))
(set-register ?s '(file . "~/.emacs.d/lisp/init-bindings.el"))
(set-register ?a '(file . "~/.emacs.d/lisp/init-appearance.el"))
(set-register ?m '(file . "~/.emacs.d/lisp/init-misc-pkgs.el"))
(set-register ?r '(file . "~/.emacs.d/lisp/init-registers.el"))
(set-register ?d '(file . "~/.emacs.d/lisp/"))
(set-register ?i '(file . "~/.emacs.d/init.el"))

(provide 'init-registers)

;;; init-registers.el ends here
