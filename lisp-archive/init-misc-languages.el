;;; init-misc-languages.el ---  Miscellaneous language setup.	-*- lexical-binding: t; -*-

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
;; Language settings for Emacs
;; from lots of different sources

;; ChangeLog:
;;
;; 2017 03 29 SeJ init
;; 2017 04 04 set python-interpreter
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 14 adds from purcell/.emacs.d
;; 2017 05 19 add mastering Emacs python debugging with compile
;; 2017 08 25 add from EOS insert-doc-string
;; 2017 08 30 map to sej-mode-map, ensure/defer, cleanup documentation
;; 2018 06 06 add company-jedi (not sure of interactions)
;; 2018 10 09 some changes to work with language-server-protocall
;; 2018 10 10 changed to init-languages.el to hold all lsp type stuff
;; 2018 10 15 lsp added for c modes, html modes, css, python, bash/sh, java, js
;; 2018 10 17 add all-format
;; 2018 12 28 fix lsp servers
;; 2019 05 03 merge

;;; Code:

;;
;; Formatting
;;

(use-package format-all
  :bind (:map sej-mode-map
              ("C-c s f" . format-all-buffer)
              ("A-f" . format-all-buffer)))

;; arduino-mode
(use-package arduino-mode
  :mode "\\.ino$"
  :config
  (setq arduino-mode-home "/Users/stephenjenkins/Projects/sej/Arduino")
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))

(provide 'init-misc-languages)
;;; init-misc-languages.el ends here
