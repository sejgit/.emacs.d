;;; init-templates.el --- templates for auto-insertion. -*- lexical-binding: t; -*-

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
;;to be auto inserted in new files

;;; ChangeLog
;;
;; 2017 05 17 init SeJ
;; 2017 09 01 update ensure / defines
;; 2019 05 02 Merge into new


;;; Code:

(use-package autoinsert
  :hook (find-file . auto-insert)
  :defines
  auto-insert-query
  auto-insert-directory
  :init
  (setq auto-insert-directory "~/.emacs.d/templates/")
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  :config
  (define-auto-insert ".*\\.py[3]?$" "template.py")
  (define-auto-insert ".*\\.el" "template.el")
  )

(provide 'init-templates)
;;; init-templates.el ends here
