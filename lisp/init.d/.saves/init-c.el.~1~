;;; init-c.el --- c/c++ programming settings for Emacs

;;; Commentary:
;; settings from EOS

;;; Log
;; 2017 08 25 init SeJ

;;; Code:

;; Flycheck supports C, so we switch it on
(add-hook 'c-mode-common-hook #'flycheck-mode)

;; always indent with 4 spaces ; in the Linuxx kernel style
(setq-default c-default-style "linux"
	      c-basic-offset 4)

;; hungry delete is useful in C ; remove up to the next non-whitespace
(setq-default c-hungry-delete-key t)

(provide 'init-c)
;;; init-c.el ends here
