;;; company-try-hard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "company-try-hard" "company-try-hard.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from company-try-hard.el

(autoload 'company-try-hard "company-try-hard" "\
Offer completions from the first backend in `company-backends' that
offers candidates. If called again, use the next backend, and so on." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-try-hard" '("company-try-hard--last-index")))

;;;***

(provide 'company-try-hard-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-try-hard-autoloads.el ends here
