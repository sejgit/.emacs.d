;; init-python.el --- Initialize python configurations.	-*- lexical-binding: t -*-

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
;; Python configurations.
;;

;;; ChangeLog:
;;
;; 2019 05 03 initialize & merge


;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Python Mode
;; Install:
;;   pip install pyflakes
;;   pip install autopep8
;; you also need python-language-server installed and on your PATH
;; pip3 install -U setuptools
;; pip3 install python-language-server[all] --isolated
;; [all] should give you: jedi, rope, pyflakes, pycodestyle, pydocstyle, autopep8, YAPF

(use-package python
  :ensure nil
  :defines gud-pdb-command-name pdb-path flycheck-disabled-checkers
  :interpreter "python"
  :bind (:map python-mode-map
              ("<backtab>" . python-back-indent)
			        ("<f9>" . py-insert-debug))
  :hook ((python-mode . flycheck-mode)
         (python-mode . (lambda ()
		                      (add-to-list 'flycheck-disabled-checkers 'python-pylint))))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :init
  ;;(setq python-shell-interpreter "ipython"
	;;	    python-shell-interpreter-args "--simple-prompt -i")
  :config
  (define-skeleton python-insert-docstring
    "Insert a Python docstring."
    "This string is ignored!"
    "\"\"\"" - "\n\n    \"\"\"")

  (define-key python-mode-map (kbd "s-\\") 'python-insert-docstring)

  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 2)
  
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)

  (add-hook 'inferior-python-mode-hook
            (lambda ()
              ;; (bind-key "C-c C-z" #'kill-buffer-and-window inferior-python-mode-map)
              (process-query-on-exit-flag (get-process "Python"))))

  ;; Live Coding in Python
  (use-package live-py-mode)

  ;; Format using YAPF
  ;; Install: pip install yapf
  (use-package yapfify
    :diminish yapf-mode
    :hook (python-mode . yapf-mode)))

;; Emacs IPython Notebook
(use-package ein
  :diminish ein:notebook-mode
  :defines ein:completion-backend
  :init (setq ein:completion-backend 'ein:use-company-backend))

;; major mode for editing pip requirement files
(use-package pip-requirements
  :ensure t
  :defer t)

;; virtualenv api in Emacs
(use-package python-environment
  :ensure t
  :defer t)

;; file comparison
(use-package ediff
  :ensure t
  :defer t
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		            (quote split-window-vertically)))

(use-package pyvenv
  :ensure t
  :hook (pyvenv-post-activate . pyvenv-restart-python))

(use-package company-jedi
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-jedi))

(provide 'init-python)
;;; init-python.el ends here
