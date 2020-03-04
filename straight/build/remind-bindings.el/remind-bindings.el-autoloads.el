;;; remind-bindings.el-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "remind-bindings" "remind-bindings.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from remind-bindings.el

(autoload 'remind-bindings-initialise "remind-bindings" "\
Collect all ‘use-package’ and global key bindings and set the omni-quotes list." nil nil)

(autoload 'remind-bindings-togglebuffer "remind-bindings" "\
Toggle the sidebar with static window rules.  Initialise and recurse to a max LEVEL of 2.

\(fn &optional LEVEL)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "remind-bindings" '("remind-bindings-")))

;;;***

(provide 'remind-bindings.el-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; remind-bindings.el-autoloads.el ends here
