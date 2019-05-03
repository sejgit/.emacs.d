;;; init-projectile.el --- Projectile configurations.	-*- lexical-binding: t -*-

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
;; projectile settings

;;; ChangeLog
;;
;; 2017 05 14 SeJ init from purcell/.emacs.d
;; 2017 06 01 simplified & added helm-projectile
;; 2017 08 25 add settings from EOS
;; 2017 08 30 cleanup
;; 2018 03 19 move helm-projectile to helm init file
;; 2018 08 28 updates for projectile
;; 2018 09 28 add redundant bind for helm-projectile
;; 2018 10 04 helm-projectile only in init-ido-ivy-helm
;; 2019 05 02 Initialize & Merge


;;; Code:

(eval-when-compile
  (require 'init-const))

;; Manage and navigate projects
(use-package projectile
  :diminish
  :defines sej-mode-map
  ;;  :diminish projectile-mode
  :bind (:map sej-mode-map
	            ("s-P" . projectile-command-map)
	            ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  ;; global ignores
  (add-to-list 'projectile-globally-ignored-files ".tern-port")
  (add-to-list 'projectile-globally-ignored-files "GTAGS")
  (add-to-list 'projectile-globally-ignored-files "GPATH")
  (add-to-list 'projectile-globally-ignored-files "GRTAGS")
  (add-to-list 'projectile-globally-ignored-files "GSYMS")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  ;; always ignore .class files
  (add-to-list 'projectile-globally-ignored-file-suffixes ".class")
  (setq projectile-project-search-path '("~/Projects/" "~/" "~/Documents/"))

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (executable-find "rg")
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  )

(use-package helm-ag
  :ensure t)

(use-package grep
  :ensure t)

(use-package emr
  :ensure t
  ;; Just hit M-RET to access your refactoring tools in any supported mode.
  :bind (:map sej-mode-map
              ("M-RET" . emr-show-refactor-menu))
  :config
  (add-hook 'prog-mode-hook 'emr-initialize))

(provide 'init-projectile)
;;; init-projectile.el ends here
