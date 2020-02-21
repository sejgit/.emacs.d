;;; mocha-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "mocha" "mocha.el" (0 0 0 0))
;;; Generated autoloads from mocha.el

(autoload 'mocha-test-project "mocha" "\
Test the current project." t nil)

(autoload 'mocha-debug-project "mocha" "\
Debug the current project." t nil)

(autoload 'mocha-test-file "mocha" "\
Test the current file." t nil)

(autoload 'mocha-debug-file "mocha" "\
Debug the current file." t nil)

(autoload 'mocha-test-at-point "mocha" "\
Test the current innermost 'it' or 'describe' or the file if none is found." t nil)

(autoload 'mocha-debug-at-point "mocha" "\
Debug the current innermost 'it' or 'describe' or the file if none is found." t nil)

(autoload 'mocha-toggle-imenu-function "mocha" "\
Toggle the use mocha-specific imenu function." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mocha" '("mocha-" "node-error-regexp")))

;;;***

(provide 'mocha-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mocha-autoloads.el ends here
