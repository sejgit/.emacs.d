;;; init-tramp.el --- Emacs tramp initialize.	-*- lexical-binding: t -*-

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
;; tramp setup.

;;; ChangeLog:
;;
;; 2017 03 14 init SeJ
;; 2017 03 28 fix settings
;; 2017 04 04 get tramp into use-package
;; 2017 05 08 possible changes for darwin
;; 2017 09 01 minor use-package tweaks
;; 2017 09 06 add pass

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(use-package tramp
  :commands
  tramp-default-method
  tramp-default-user
  tramp-default-host
  :init
  (if (eq system-type 'darwin)
      (setq
       tramp-default-method "ssh"
       password-cache-expiry nil)
    (setq
     tramp-default-method "ssh"
     tramp-default-user "pi"
     tramp-default-host "home"
     password-cache-expiry nil)
    )
  (setq tramp-use-ssh-controlmaster-options nil)

  (defadvice tramp-handle-write-region
      (after tramp-write-beep-advice activate)
    "Make tramp beep after writing a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-do-copy-or-rename-file
      (after tramp-copy-beep-advice activate)
    "Make tramp beep after copying a file."
    (interactive)
    (beep))

  (defadvice tramp-handle-insert-file-contents
      (after tramp-insert-beep-advice activate)
    "Make tramp beep after inserting a file."
    (interactive)
    (beep))
  )

(use-package pass
  :ensure t
  :defer 7)

(provide 'init-tramp)
;;; init-tramp.el ends here
