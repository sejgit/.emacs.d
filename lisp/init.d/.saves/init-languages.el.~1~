;;; init-python.el ---  Stephen's emacs init-python.el

;;; Commentary:
;; Python settings for Emacs
;; from lots of different sources

;; ChangeLog:
;; 2017 03 29 SeJ init
;; 2017 04 04 set python-interpreter
;; 2017 04 04 remove ensure went global ; defer not required for mode,bind,int
;; 2017 05 14 adds from purcell/.emacs.d
;; 2017 05 19 add mastering Emacs python debugging with compile
;; 2017 08 25 add from EOS insert-doc-string
;; 2017 08 30 map to sej-mode-map, ensure/defer, cleanup documentation
;; 2018 06 06 add company-jedi (not sure of interactions)
;; 2018 10 09 some changes to work with language-server-protocall

;;; Code:

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

(use-package lsp-python
  :ensure t
  :after lsp-mode
  :init
  (add-hook 'python-mode-hook  #'lsp-python-enable)
  (lsp-define-stdio-client lsp-python "python"
			                     (lsp-make-traverser #'(lambda (dir)
						                                       (not (directory-files
						                                             dir
						                                             nil
						                                             "__init__.py"))))
			                     '("pyls"))
  )

(use-package python
  :ensure t
  :after lsp-mode
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
  (setq tab-width 2))

(use-package pyvenv
  :ensure t
  :hook (pyvenv-post-activate . pyvenv-restart-python))

;; (use-package jedi
;;   :ensure t
;;   :defer t
;;   :hook ((python-mode . jedi:setup)
;; 	       (lsp-mode . jedi:setup)
;;          )
;;   :preface
;;   (declare-function jedi:goto-definition jedi nil)
;;   (declare-function jedi:related-names jedi nil)
;;   (declare-function jedi:show-doc jedi nil)
;;   :bind (:map python-mode-map
;; 	            ("C-." . jedi:goto-definition)
;; 	            ("C-c r" . jedi:related-names)
;; 	            ("C-?" . jedi:show-doc))
;;   :config
;;   (autoload 'jedi:setup "jedi" nil t)
;;   (setq jedi:complete-on-dot t)
;;   )

;; (use-package company-jedi
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook (add-to-list 'company-backends 'company-jedi)))

;; (use-package py-autopep8
;;   :ensure t
;;   :hook (python-mode . py-autopep8-enable-on-save))

(provide 'init-python)
;;; init-python.el ends here
