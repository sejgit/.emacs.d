;;; init-calendar.el --- Initialize calendar configurations.  -*- lexical-binding: t -*-

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
;; Calendar configuration.
;;

;;; Changelog
;;
;; 2019 04 30 Integrated

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Better views of calendar
(use-package calfw
  :commands cfw:open-calendar-buffer
  :bind ("<C-f12>" . open-calendar)
  :init
  (use-package calfw-org
    :commands (cfw:open-org-calendar cfw:org-create-source))

  (use-package calfw-ical
    :commands (cfw:open-ical-calendar cfw:ical-create-source))

  (defun open-calendar ()
    "Open calendar."
    (interactive)
    (unless (ignore-errors
              (cfw:open-calendar-buffer
               :contents-sources
               (list
                (when org-agenda-files
                  (cfw:org-create-source "YellowGreen"))
                (when (bound-and-true-p sej-ical)
                  (cfw:ical-create-source "gcal" sej-ical "IndianRed")))))
      (cfw:open-calendar-buffer)))
  (defalias 'sej-open-calendar 'open-calendar))

(provide 'init-calendar)
;;; init-calendar.el ends here
