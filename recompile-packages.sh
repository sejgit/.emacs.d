#!/bin/bash
# Recompile Emacs packages to suppress obsolete warnings

emacs --batch --eval '
(progn
  (setq byte-compile-warnings (quote (not obsolete make-local suspicious lexical free-vars)))
  (setq byte-compile-verbose nil)
  (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0 t))
'
