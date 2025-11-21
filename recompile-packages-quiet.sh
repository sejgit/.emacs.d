#!/bin/bash
# Recompile Emacs packages silently to suppress obsolete warnings

echo "Recompiling packages... this will take a few minutes"

emacs --batch --eval '
(progn
  (setq byte-compile-warnings (quote (not obsolete make-local suspicious lexical free-vars cl-functions)))
  (setq byte-compile-verbose nil)
  (setq native-comp-async-report-warnings-errors (quote silent))
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0 t))
' 2>&1 | grep -v "Warning:" | grep -v "obsolete"

echo "Done! Restart Emacs to see clean startup."
