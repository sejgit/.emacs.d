;;; esup-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "esup" "esup.el" (0 0 0 0))
;;; Generated autoloads from esup.el

(autoload 'esup "esup" "\
Profile the startup time of Emacs in the background.
If INIT-FILE is non-nil, profile that instead of USER-INIT-FILE.
ARGS is a list of extra command line arguments to pass to Emacs.

\(fn &optional INIT-FILE &rest ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esup" '("esup-")))

;;;***

;;;### (autoloads nil "esup-child" "esup-child.el" (0 0 0 0))
;;; Generated autoloads from esup-child.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "esup-child" '("esup-" "with-esup-child-increasing-depth")))

;;;***

(provide 'esup-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; esup-autoloads.el ends here
