;;; sej-os-specific-bsd.el --- sej Emacs OS Specific BSD -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides OS specific constants for BSD
;; as well sets up the keyboard for modifiers.

;;; Code:
;;;;; FreeBSD System specific environment setting
(when sys/freebsdp
  (message "FreeBSD")

;;;;; FreeBSD keyboard
  ;; - nothing set at this moment
  ;; load-dir init.d
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq insert-directory-program "/usr/local/bin/gls"))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-os-specific-bsd)
;;; sej-os-specific-bsd.el ends here
