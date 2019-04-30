;;; init-misc-filetypes.el --- settings for miscellaneous filetypes

;;; Commentary:
;; Lots of filetype modes not deserving of their own file so far.

;;; ChangeLog:
;; 2017 09 07 init SeJ moved simple modes from init-misc.pkgs & others to its own file

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

;;; Code:

;; arduino-mode
(use-package arduino-mode
  :ensure t
  :mode "\\.ino$")

(use-package batch-mode
  :ensure t
  :mode "\\.bat\\'")

;; major mode for editing conf/ini/properties files
(use-package conf-mode
  :ensure t
  :diminish conf-mode
  :mode "\\.gitconfig$")


;; ;; editing of crontab scheduling files (removed as seems not on melpa)
;; (use-package crontab-mode
;;   :ensure t
;;   :defer t
;;   :mode "\\.?cron\\(tab\\)?\\'")

;; major mode for csv
(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config
  (setq csv-separators '("," ";" "|" " ")))

;; navigate and edit CSV files
(use-package csv-nav
  :ensure t
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

(provide 'init-misc-filetypes)
;;; init-misc-filetypes.el ends here
