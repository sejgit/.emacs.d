;; init-kill-ring.el --- Initialize kill-ring configurations.	-*- lexical-binding: t -*-

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
;; Kill ring configurations.
;;

;;; Changelog:
;;
;; 2019 05 12 Merge


;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(setq kill-ring-max 200)

;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)

;; Kill & Mark things easily
(use-package easy-kill-extras
  :bind (([remap kill-ring-save] . easy-kill) ; M-w
         ([remap mark-sexp] . easy-mark-sexp) ; C-M-spc
         ([remap mark-word] . easy-mark-word) ; M-@

         ;; Integrate `zap-to-char'
         ([remap zap-to-char] . easy-mark-to-char) ; M-z
         ([remap zap-up-to-char] . easy-mark-up-to-char) ; none

         ;; Integrate `expand-region'
         :map easy-kill-base-map
         ("o" . easy-kill-er-expand)
         ("i" . easy-kill-er-unexpand))
  :init
  (setq easy-kill-alist '((?w word           " ")
                          (?s sexp           "\n")
                          (?l list           "\n")
                          (?f filename       "\n")
                          (?d defun          "\n\n")
                          (?D defun-name     " ")
                          (?e line           "\n")
                          (?b buffer-file-name)

                          (?^ backward-line-edge "")
                          (?$ forward-line-edge "")
                          (?h buffer "")
                          (?< buffer-before-point "")
                          (?> buffer-after-point "")
                          (?f string-to-char-forward "")
                          (?F string-up-to-char-forward "")
                          (?t string-to-char-backward "")
                          (?T string-up-to-char-backward ""))))

(provide 'init-kill-ring)
;;; init-kill-ring.el ends here
