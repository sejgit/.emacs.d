;;; init-misc-filetypes.el --- settings for miscellaneous filetypes.	-*- lexical-binding: t -*-

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
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;;
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file
;; 2018 06 06 added JSON & web-mode etc from dotemacs/emacs.org at master · vidjuheffex/dotemacs
;; 2018 08 06 deleted init-js and added here js2-mode
;; 2018 08 07 fix rainbow-mode
;; 2018 09 28 move rainbow-mode to init-appearance & add language server protocall
;; 2018 10 10 move out LSP to init-languages.el will move more out later
;; 2018 10 10 move out arduino, html to init-languages
;; 2018 10 15 move out c & java script to init-languages
;; 2019 05 07 merge with centaur


;;; Table of contents
;;
;; conf-mode
;; csv-mode
;; textile-mode

;;; Code:

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :diminish conf-mode
  :mode "\\.gitconfig$")

;; major mode for csv
(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;; textile markup editing major mode
(use-package textile-mode
  :mode "\\.textile\\'")

(provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here
