;;; omni-quotes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "omni-quotes" "omni-quotes.el" (0 0 0 0))
;;; Generated autoloads from omni-quotes.el

(autoload 'omni-quotes-display-random-quote "omni-quotes" "\
Display a random quote obtained from `omni-quotes-random-quote'.
The quote will be prefixed by the current `omni-quotes-prompt'" t nil)

(defvar omni-quotes-mode nil "\
Non-nil if Omni-Quotes mode is enabled.
See the `omni-quotes-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `omni-quotes-mode'.")

(custom-autoload 'omni-quotes-mode "omni-quotes" nil)

(autoload 'omni-quotes-mode "omni-quotes" "\
Display random quotes when idle.

If called interactively, enable Omni-Quotes mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "omni-quotes" '("omni-quotes-")))

;;;***

;;;### (autoloads nil "omni-quotes-reader" "omni-quotes-reader.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omni-quotes-reader.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "omni-quotes-reader" '("omni-quotes-")))

;;;***

;;;### (autoloads nil "omni-quotes-ring" "omni-quotes-ring.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from omni-quotes-ring.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "omni-quotes-ring" '("omni-quote")))

;;;***

;;;### (autoloads nil "omni-quotes-timer" "omni-quotes-timer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from omni-quotes-timer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "omni-quotes-timer" '("omni-quotes-idle-")))

;;;***

(provide 'omni-quotes-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; omni-quotes-autoloads.el ends here
