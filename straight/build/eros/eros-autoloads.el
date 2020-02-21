;;; eros-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "eros" "eros.el" (0 0 0 0))
;;; Generated autoloads from eros.el

(defvar eros-mode nil "\
Non-nil if Eros mode is enabled.
See the `eros-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `eros-mode'.")

(custom-autoload 'eros-mode "eros" nil)

(autoload 'eros-mode "eros" "\
Display Emacs Lisp evaluation results overlays.

If called interactively, enable Eros mode if ARG is positive, and
disable it if ARG is zero or negative.  If called from Lisp, also
enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eros" '("eros-")))

;;;***

(provide 'eros-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eros-autoloads.el ends here
