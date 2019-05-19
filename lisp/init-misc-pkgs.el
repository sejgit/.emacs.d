;;; init-misc-pkgs.el --- miscellaneous small packages.	-*- lexical-binding: t -*-

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
;;Lots of small packages not deserving of their own file so far.

;;; ChangeLog:
;;
;; 2017 01 06 init SeJ moved from init-look-and-feel.el the package setups
;;            add google-this ::search google with C-/ return
;;            add volatile-highlights  ::temporarily highlight pasting changes
;;            add rainbow-delimiters ::dired mode for colours
;;            add saveplace ::return to the same place in saved file
;;            add conf-mode :: for editing conf/ini files
;;            remove zenburn-theme ::used from pragmatic Emacs
;;            change from req-package to use-package
;; 2017 01 10 add swiper to M-s from pragmatic Emacs
;;            add crux to move to biginning of line intelligently
;;            add avy for efficient movement through search
;;            move swiper to own file & add ivy-dired-recent-dirs()
;; 2017 01 16 add drag-stuff to move highlighted region around
;;            add beacon mode to highlight cursor when moved
;; 2017 03 30 move magit & pyenv-mode-auto to init-python.el
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 10 add bookmark+
;;            add rpn-calc
;; 2017 05 12 mods from purcell/emacs.d
;; 2017 05 17 add help-fns+.el
;; 2017 05 28 add whole-line-or-region
;; 2017 06 12 add mode-icons
;; 2017 06 19 add no-littering
;; 2017 08 02 add beginend mode
;; 2017 08 22 add midnight mode
;; 2017 08 23 add comments to packages
;;            add expand-region, vlf
;; 2017 08 25 add undo-tree
;; 2017 08 28 add smartscan & dtrt-indent & highlight-numbers
;; 2017 08 30 clean-up, defer, map to sej-mode-map
;; 2017 09 01 turn off for now as not using features
;; 2017 09 04 move indent-guide, page-break-lines,
;;            whitespace-cleanup-mode to init-writing.el
;; 2017 09 06 removed no-littering as was messing with backups
;; 2017 09 07 move modes for editing filetypes to init-misc-filetypes.el
;; 2017 09 20 move some packages to init-appearance.el or init-movement.el
;;            move which-key in from init-which-key.el and delete file
;; 2017 11 17 modified which-key to add binds and side-window-right-bottom
;; 2018 06 22 move from fic-mode to hl-todo-mode delete file init-fic-ext.el
;; 2018 07 02 add osx-dictionary to get definitions using built in osx dictionary
;; 2018 08 06 add paredit for use in js2 & json modes
;; 2018 08 07 add try to allow 'trying' a package
;; 2018 09 27 update of which-key ohai tip
;; 2018 10 15 add comment-dwim2
;; 2018 10 17 move osx definition to init-spelling
;;            document packages a bit better
;; 2019 04 30 merged with init-utils

;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; for TRYing out a package in current Emacs instance
(use-package try)

;; moving around paredit style
(use-package paredit)

;; highlighting and moving around todos and similar keywords
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :bind (:map hl-todo-mode-map
              ("H-p" . hl-todo-previous)
              ("H-n" . hl-todo-next)
              ("H-o" . hl-todo-occur)))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :commands which-key-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-h C-m" . which-key-show-major-mode)
              ("C-h C-k" . which-key-show-top-level))
  :config
  (which-key-setup-minibuffer))

;; expand selection region larger & smaller
(use-package expand-region
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("s-=" . er/expand-region)
              ("s--" . er/contract-region)))

;; vlf lets you handle very large files for viewing
(use-package vlf-setup
  :ensure vlf
  :commands (vlf vlf-occur-load vlf-ediff-files))

;; midnight mode to clear buffers at midnight
(use-package midnight
  :defer 60
  :config
  (customize-set-variable 'midnight-mode t))

;; google-this
(use-package google-this
  :diminish google-this-mode
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ("C-c g" . google-this)
              ("s-g" . google-this))
  :config
  (google-this-mode 1))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

;; writable grep buffer and apply the changes to files
(use-package wgrep
  :defer 5
  :init
  (setq-default grep-highlight-matches t
                grep-scroll-output t
                wgrep-auto-save-buffer t
                wgrep-change-readonly-file t)
  :config
  (when (eq system-type 'darwin)
    (setq-default locate-command "which")
    (setq exec-path (append exec-path '("/usr/local/bin"))))

  (when (executable-find "ag")
    (use-package ag
      :config
      (setq ag-executable (executable-find "ag")))
    (use-package wgrep-ag)
    (setq-default ag-highlight-search t)
    (define-key sej-mode-map (kbd "M-?") 'ag-project)))

;; `ripgrep'
(when (executable-find "rg")
  (use-package rg
    :defines projectile-command-map
    :hook (after-init . rg-enable-default-bindings)
    :config
    (setq rg-group-result t)
    (setq rg-show-columns t)

    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)

    (with-eval-after-load 'projectile
      (defalias 'projectile-ripgrep 'rg-project)
      (bind-key "s R" #'rg-project projectile-command-map))

    (with-eval-after-load 'counsel
      (bind-keys :map rg-global-map
                 ("c r" . counsel-rg)
                 ("c s" . counsel-ag)
                 ("c p" . counsel-pt)
                 ("c f" . counsel-fzf)))))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :init (setq atomic-chrome-buffer-open-style 'frame)
  :config
  (if (fboundp 'gfm-mode)
      (setq atomic-chrome-url-major-mode-alist
            '(("github\\.com" . gfm-mode)))))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Docker
(use-package docker
  :bind ("C-c D" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")))

;; Tramp
(use-package docker-tramp)

;; Discover key bindings and their meaning for the current Emacs major mode
(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))


;; helpful is an improved help-fns & help-fns+
(use-package helpful
  :defines sej-mode-map
  :bind (:map sej-mode-map
              ;;("C-h f" . helpful-function)
              ("C-h c" . helpful-command)
              ("C-h M" . helpful-macro)
              ("C-h v" . helpful-variable)))

;; Misc
(use-package copyit)                    ; copy path, url, etc.
(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package esup)                      ; Emacs startup profiler
(use-package focus)                     ; Focus on the current region
(use-package htmlize)                   ; covert to html
(use-package list-environment)
(use-package memory-usage)
(use-package tldr)
(use-package ztree)                     ; text mode directory tree. Similar with beyond compare

(provide 'init-misc-pkgs)
;;; init-misc-pkgs.el ends here
