;;; init-misc-filetypes.el --- settings for miscellaneous filetypes

;;; Commentary:
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file
;; 2018 06 06 added JSON & web-mode etc from dotemacs/emacs.org at master · vidjuheffex/dotemacs
;; 2018 08 06 deleted init-js and added here js2-mode
;; 2018 08 07 fix rainbow-mode
;; 2018 09 28 move rainbow-mode to init-appearance & add language server protocall

;;; Table of contents
;; arduino-mode
;; batch-mode
;; conf-mode
;; crontab-mode
;; csv-mode
;; csv-nav
;; nov-mode ;; nov.el for epub
;; php-mode
;; textile-mode
;; yaml-mode
;; JSON-mode
;; js2-mode for javascript
;; web-mode with company-web emmet-mode rainbow-mode
;; language server protocall


;;; Code:

;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$"
  :config
  (setq arduino-mode-home "/Users/stephenjenkins/Projects/sej/Arduino")
  (setq arduino-executable "/Applications/Arduino.app/Contents/MacOS/Arduino"))

(use-package batch-mode
  :load-path "lisp/batch-mode"
  :mode "\\.bat\\'")

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :ensure t
  :diminish conf-mode
  :mode "\\.gitconfig$")

;; major mode for csv
(use-package csv-mode
  :load-path "lisp/csv-mode"
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;; navigate and edit CSV files
(use-package csv-nav
  :load-path "lisp/csv-nav"
  :after csv-mode)

;; nov-mode ;; nov.el for epub
(use-package nov-mode
  :ensure nov
  :mode "\\.epub\\'")

;; major mode for editing PHP code
(use-package php-mode
  :ensure t
  :mode (("\\.module$" . php-mode)
	       ("\\.inc$" . php-mode)
	       ("\\.install$" . php-mode)
	       ("\\.engine$" . php-mode)))

;; textile markup editing major mode
(use-package textile-mode
  :ensure t
  :mode "\\.textile\\'")

;; YAML support
(use-package yaml-mode
  :ensure t
  :mode
  (("\\.yml$" . yaml-mode)
   ("\\.yaml$" . yaml-mode)))

;; JSON
(use-package json-mode
  :ensure t
  :commands json-mode)

;; javascript
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

;; Basic lsp-mode config.
;; Language modules will add their own lsp setup if this is loaded.
(use-package lsp-mode
  :ensure t
  )

(use-package company-lsp
  :ensure t
  :after company
  :after lsp-mode
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :quelpa (lsp-ui :fetcher github :repo "emacs-lsp/lsp-ui")
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-mode-map
              ("C-." . lsp-ui-peek-find-definitions)
              ("C-?" . lsp-ui-peek-find-references)
              ("C-c C-j" . lsp-ui-imenu)
              ("C-\'" . lsp-ui-imenu)
              ))


  (provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here
