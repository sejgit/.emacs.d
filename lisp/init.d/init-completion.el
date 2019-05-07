;;; init-completion.el --- Completion configuration.	-*- lexical-binding: t -*-

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
;; settings for company-mode in my Emacs

;; ChangeLog:
;;
;; 2016 12 16 SeJ
;; 2017 01 07 switch from req-package to use-package
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 08 29 remove yasnippet
;; 2017 08 30 change binding to sej-map
;; 2017 09 03 rename to init-completion.el update company settings with EOS
;; 2017 09 20 move hippie-expand settings from init-bindings-settings.el
;; 2018 09 24 some improvements from ohai
;; 2019 05 07 merge into new .emacs.d


;;; Code:

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-line-all-buffers
        try-complete-lisp-symbol-partially
        try-compelete-lisp-symbol))

(use-package company
  :diminish company-mode
  :demand
  :defines
  sej-mode-map
  company-dabbrev-ignore-case
  company-dabbrev-downcase
  company-dabbrev-code-modes
  company-dabbrev-code-ignore-case
  :hook (after-init . global-company-mode)
  :bind (:map sej-mode-map
              (("C-<tab>" . company-complete)
               ("M-<tab>" . company-complete))
              (:map company-active-map
                    (("C-n" . company-select-next)
                     ("C-p" . company-select-previous)
                     ("C-d" . company-show-doc-buffer)
                     ("C-l" . company-show-location)
                     ("<tab>" . company-complete))))
  :init
  (setq company-dabbrev-ignore-case nil
        ;; don't downcase dabbrev suggestions
        company-dabbrev-downcase nil)
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case nil)

  :config (setq global-company-mode t
                ;; do or don't automatically start completion after <idle time>
                company-idle-delay 0.3
                company-show-numbers t
                ;; at least 3 letters need to be there though
                company-minimum-prefix-length 3
                company-auto-complete nil
                company-selection-wrap-around t
                ;; show completion numbers for hotkeys
                company-show-numbers t
                ;; align annotations to the right
                company-tooltip-align-annotations t
                company-search-regexp-function #'company-search-flex-regexp
                company-selection-wrap-around t
                company-show-numbers t
                company-tooltip-align-annotations t
                company-require-match nil)
  ;; Sort completion candidates that already occur in the current
  ;; buffer at the top of the candidate list.
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-quickhelp
  :after (company)
  :hook (company-mode . company-quickhelp-mode)
  :config (setq company-quickhelp-delay 1))

;; Set up statistics for company completions
(use-package company-statistics
  :hook (after-init . company-statistics-mode))

(use-package company-try-hard
  :commands company-try-hard
  :bind ("C-\\" . company-try-hard)
  :config
  (bind-keys :map company-active-map
             ("C-\\" . company-try-hard)))

(use-package smart-tab
  :diminish ""
  :defines
  smart-tab-using-hippie-expand
  :init
  (setq smart-tab-using-hippie-expand t)
  :config
  (global-smart-tab-mode 1)
  (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
  (add-to-list 'smart-tab-disabled-major-modes 'shell-mode))

(provide 'init-completion)
;;; init-completion.el ends here
