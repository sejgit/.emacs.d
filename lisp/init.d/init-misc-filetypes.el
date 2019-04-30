;;; init-misc-filetypes.el --- settings for miscellaneous filetypes

;;; Commentary:
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file
;; 2018 06 06 added JSON & web-mode etc from dotemacs/emacs.org at master · vidjuheffex/dotemacs
;; 2018 08 06 deleted init-js and added here js2-mode
;; 2018 08 07 fix rainbow-mode
;; 2018 09 28 move rainbow-mode to init-appearance & add language server protocall
;; 2018 10 10 move out LSP to init-languages.el will move more out later
;; 2018 10 10 move out arduino, html to init-languages
;; 2018 10 15 move out c & java script to init-languages

;;; Table of contents
;; conf-mode
;; crontab-mode
;; csv-mode
;; csv-nav
;; nov-mode ;; nov.el for epub
;; php-mode
;; textile-mode
;; yaml-mode
;; JSON-mode


;;; Code:

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


(provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here
