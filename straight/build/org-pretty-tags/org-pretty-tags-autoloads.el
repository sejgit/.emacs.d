;;; org-pretty-tags-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "org-pretty-tags" "org-pretty-tags.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-pretty-tags.el

(defvar org-pretty-tags-surrogate-strings '(("imp" . "☆") ("idea" . "💡") ("money" . "$$$") ("easy" . "₰") ("music" . "♬")) "\
List of pairs of tag and replacement e.g. (\"money\" . \"$$$\") of
  surrogates for tags.")

(custom-autoload 'org-pretty-tags-surrogate-strings "org-pretty-tags" t)

(defvar org-pretty-tags-surrogate-images 'nil "\
List of pairs of tag and file-path to an image e.g. (\"@alice\" . \"/images/alice.png\") of
  image surrogates for tags.")

(custom-autoload 'org-pretty-tags-surrogate-images "org-pretty-tags" t)

(defvar org-pretty-tags-mode-lighter " pretty-tags" "\
Text in the mode line to indicate that the mode is on.")

(custom-autoload 'org-pretty-tags-mode-lighter "org-pretty-tags" t)

(defvar org-pretty-tags-agenda-unpretty-habits nil "\
If non-nil don't prettify agenda habit lines.  This feature helps
to keep the alignment of the habit table.")

(custom-autoload 'org-pretty-tags-agenda-unpretty-habits "org-pretty-tags" t)

(autoload 'org-pretty-tags-mode "org-pretty-tags" "\
Display surrogates for tags in buffer.
This mode is local to Org mode buffers.

If called interactively, enable Org-Pretty-Tags mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Special: when invoked from an Org agenda buffer the mode gets
applied to every Org mode buffer.

\(fn &optional ARG)" t nil)

(put 'org-pretty-tags-global-mode 'globalized-minor-mode t)

(defvar org-pretty-tags-global-mode nil "\
Non-nil if Org-Pretty-Tags-Global mode is enabled.
See the `org-pretty-tags-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `org-pretty-tags-global-mode'.")

(custom-autoload 'org-pretty-tags-global-mode "org-pretty-tags" nil)

(autoload 'org-pretty-tags-global-mode "org-pretty-tags" "\
Toggle Org-Pretty-Tags mode in all buffers.
With prefix ARG, enable Org-Pretty-Tags-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Org-Pretty-Tags mode is enabled in all buffers where
`(lambda nil (when (derived-mode-p 'org-mode) (org-pretty-tags-mode 1)))' would do it.
See `org-pretty-tags-mode' for more information on Org-Pretty-Tags mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pretty-tags" '("org-pretty-tags-")))

;;;***

(provide 'org-pretty-tags-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-pretty-tags-autoloads.el ends here
