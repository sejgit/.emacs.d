;;; ivy-xref-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ivy-xref" "ivy-xref.el" (0 0 0 0))
;;; Generated autoloads from ivy-xref.el

(autoload 'ivy-xref-show-xrefs "ivy-xref" "\
Show the list of xrefs returned by FETCHER and ALIST via ivy.

\(fn FETCHER ALIST)" nil nil)

(autoload 'ivy-xref-show-defs "ivy-xref" "\
Show the list of definitions returned by FETCHER and ALIST via ivy.
Will jump to the definition if only one is found.

\(fn FETCHER ALIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-xref" '("ivy-xref-")))

;;;***

(provide 'ivy-xref-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-xref-autoloads.el ends here
