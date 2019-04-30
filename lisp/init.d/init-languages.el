;;; init-languages.el ---  Stephen's emacs init-languages.el

;;; Commentary:
;; Language settings for Emacs
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
;; 2018 10 10 changed to init-languages.el to hold all lsp type stuff
;; 2018 10 15 lsp added for c modes, html modes, css, python, bash/sh, java, js
;; 2018 10 17 add all-format
;; 2018 12 28 fix lsp servers

;;; Code:

;;
;; Formatting
;;

(use-package format-all
  :ensure t
  :bind (:map sej-mode-map
              ("C-c s f" . format-all-buffer)
              ("A-f" . format-all-buffer)))


;;
;; LSP SETUP
;;

;; Basic lsp-mode config.
;; Language modules will add their own lsp setup if this is loaded.
(use-package lsp-mode
  :ensure t
  :config
  (require 'lsp-clients)
  (add-hook 'python-mode-hook 'lsp))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :quelpa (lsp-ui :fetcher github :repo "emacs-lsp/lsp-ui")
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-after-open . lsp-enable-imenu))
  :bind (:map lsp-ui-mode-map
              ("C-." . lsp-ui-peek-find-definitions)
              ("C-?" . lsp-ui-peek-find-references)
              ("C-c C-j" . lsp-ui-imenu)
              ("C-\'" . lsp-ui-imenu)
              ))

;;
;; C, C++
;;

(require 'cc-mode)

;; Flycheck supports C, so we switch it on
(add-hook 'c-mode-common-hook #'flycheck-mode)

;; always indent with 4 spaces ; in the Linuxx kernel style
(setq-default c-default-style "linux"
	            c-basic-offset 4)

;; hungry delete is useful in C ; remove up to the next non-whitespace
(setq-default c-hungry-delete-key t)


;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$"
  :config
  (setq arduino-mode-home "/Users/stephenjenkins/Projects/sej/Arduino")
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))


;;
;; html
;;

(use-package web-mode
  :ensure t
  :defines web-mode-enable-comment-keywords
  :mode (("\\.phtml\\'" . web-mode)
	       ("\\.tpl\\.php\\'" . web-mode)
	       ("\\.blade\\.php\\'" . web-mode)
	       ("\\.jsp\\'" . web-mode)
	       ("\\.as[cp]x\\'" . web-mode)
	       ("\\.erb\\'" . web-mode)
	       ("\\.html?\\'" . web-mode)
	       ("\\.ejs\\'" . web-mode)
	       ("\\.php\\'" . web-mode)
	       ("\\.mustache\\'" . web-mode)
	       ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-attr-value-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t))

(use-package company-web
  :ensure t
  :hook (web-mode . (lambda ()
		                  (add-to-list 'company-backends 'company-web-html)
		                  (add-to-list 'company-backends 'company-web-jade)
		                  (add-to-list 'company-backends 'company-web-slim))))

(use-package emmet-mode
  :ensure t
  :hook (web-mode sgml-mode html-mode css-mode))

;; you also need vscode-html-languageserver-bin installed and on your PATH
;; npm install -g vscode-html-languageserver-bin

;; you also need vscode-css-languageserver-bin installed and on your PATH
;; npm install -g vscode-css-languageserver-bin

;;
;; BASH (SH-MODE)
;;

;; you also need bash-language-server installed and on your PATH
;; npm install -g bash-language-server


(use-package company-shell
  :ensure t
  :after company
  :config
  (push 'company-shell company-backends))

;;
;; PYTHON
;;

;; you also need python-language-server installed and on your PATH
;; pip3 install -U setuptools
;; pip3 install python-language-server[all] --isolated
;; [all] should give you: jedi, rope, pyflakes, pycodestyle, pydocstyle, autopep8, YAPF

(use-package python
  :ensure t
  :after lsp-mode
  :interpreter "python"
  :defines flycheck-disabled-checkers
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


;; should get all of below functionality with language-server setup
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


;;
;; javascript
;;

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
	       ("\\.json\\'" . js2-mode)
	       ("\\.js$\\'" . js2-mode )
	       ("\\.es6\\'" . js2-mode )
	       ("\\.ejs\\'" . js2-mode )
	       ("\\manifest.webapp\\'" . js2-mode )
	       ("\\.tern-project\\'" . js2-mode))
  :interpreter "node"
  :commands js2-mode
  :config
  ;; Leverage js2-mode to get some refactoring support through js2-refactor.
  (use-package js2-refactor
    :ensure t
    :commands (js2r-add-keybindings-with-prefix)
    :init
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    (js2r-add-keybindings-with-prefix "C-c C-m"))
  ;; Configure js2-mode good.
  (setq-default
   js2-mode-indent-ignore-first-tab t
   js2-strict-inconsistent-return-warning nil
   js2-global-externs
   '("module" "require" "__dirname" "process" "console" "JSON" "$" "_"))
  ;; js2-show-parse-errors nil
  ;; js2-strict-var-hides-function-arg-warning nil
  ;; js2-strict-missing-semi-warning nil
  ;; js2-strict-trailing-comma-warning nil
  ;; js2-strict-cond-assign-warning nil
  ;; js2-strict-var-redeclaration-warning nil
  )

;; Use Tern for smarter JS.
(use-package tern
  :ensure t
  :commands tern-mode
  :config
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  ;; Locate the Tern binary by querying the system search path, which
  ;; should now include the local npm prefix.
  (setq tern-command (list (or (ohai/resolve-exec "tern") "tern")))
  ;; Setup Tern as an autocomplete source.
  (with-eval-after-load "company"
    (use-package company-tern
      :config
      (add-to-list 'company-backends 'company-tern))))

;; ;; you also need lsp-javascript-typescript installed and on your PATH
;; npm -i -g javascript-typescript-langserver

(provide 'init-languages)
;;; init-languages.el ends here
