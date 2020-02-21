;;; smart-region-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "smart-region" "smart-region.el" (0 0 0 0))
;;; Generated autoloads from smart-region.el

(autoload 'smart-region "smart-region" "\
Smart region guess what you want to select by one command.
If you call this command multiple times at the same position, it expands
selected region (it calls `er/expand-region').
Else, if you move from the mark and call this command, it select the
region rectangular (it call `rectangle-mark-mode').
Else, if you move from the mark and call this command at same column as
mark, it add cursor to each line (it call `mc/edit-lines').

\(fn ARG)" t nil)

(autoload 'smart-region-on "smart-region" "\
Set C-SPC to smart-region." t nil)

(autoload 'smart-region-off "smart-region" "\
Reset C-SPC to original command." t nil)

;;;***

(provide 'smart-region-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smart-region-autoloads.el ends here
