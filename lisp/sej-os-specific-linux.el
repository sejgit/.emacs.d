;;; sej-os-specific-linux.el --- sej Emacs OS Specific Linux -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:
;; Provides OS specific constants for Linux
;; as well sets up the keyboard for modifiers.

;;; Code:
(when sys/linuxp
  (message "Linux")

;;;;; Linux keyboard
  ;; nothing set at this moment
  ;; load-dir init.d
  (setq exec-path (append exec-path '("/usr/local/bin")))  )

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

(provide 'sej-os-specific-linux)
;;; sej-os-specific-linux.el ends here
