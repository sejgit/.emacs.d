;;; org-plus-contrib-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ob-C" "ob-C.el" (0 0 0 0))
;;; Generated autoloads from ob-C.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-C" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-J" "ob-J.el" (0 0 0 0))
;;; Generated autoloads from ob-J.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-J" '("obj-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-R" "ob-R.el" (0 0 0 0))
;;; Generated autoloads from ob-R.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-R" '("ob-R-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-abc" "ob-abc.el" (0 0 0 0))
;;; Generated autoloads from ob-abc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-abc" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-arduino" "ob-arduino.el" (0 0 0 0))
;;; Generated autoloads from ob-arduino.el

(autoload 'org-babel-execute:arduino "ob-arduino" "\
org-babel arduino hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load 'org '(add-to-list 'org-src-lang-modes '("arduino" . arduino)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-arduino" '("ob-arduino:" "org-babel-default-header-args:sclang")))

;;;***

;;;### (autoloads nil "ob-asymptote" "ob-asymptote.el" (0 0 0 0))
;;; Generated autoloads from ob-asymptote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-asymptote" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-awk" "ob-awk.el" (0 0 0 0))
;;; Generated autoloads from ob-awk.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-awk" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-calc" "ob-calc.el" (0 0 0 0))
;;; Generated autoloads from ob-calc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-calc" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-clojure" "ob-clojure.el" (0 0 0 0))
;;; Generated autoloads from ob-clojure.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure" '("ob-clojure-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-clojure-literate" "ob-clojure-literate.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-clojure-literate.el

(defvar ob-clojure-literate-auto-jackin-p nil "\
Auto jack in ob-clojure project.
Don't auto jack in by default for not rude.")

(custom-autoload 'ob-clojure-literate-auto-jackin-p "ob-clojure-literate" t)

(autoload 'ob-clojure-literate-specify-session "ob-clojure-literate" "\
Specify ob-clojure header argument :session with value selected from a list of available sessions." t nil)

(autoload 'ob-clojure-literate-auto-jackin "ob-clojure-literate" "\
Auto setup ob-clojure-literate scaffold and jack-in Clojure project." t nil)

(autoload 'ob-clojure-literate-enable "ob-clojure-literate" "\
Enable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(autoload 'ob-clojure-literate-disable "ob-clojure-literate" "\
Disable Org-mode buffer locally for `ob-clojure-literate'." nil nil)

(if ob-clojure-literate-auto-jackin-p (ob-clojure-literate-auto-jackin))

(autoload 'ob-clojure-literate-mode "ob-clojure-literate" "\
A minor mode to toggle `ob-clojure-literate'.

If called interactively, enable Ob-Clojure-Literate mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-clojure-literate" '("ob-clojure-literate-")))

;;;***

;;;### (autoloads nil "ob-comint" "ob-comint.el" (0 0 0 0))
;;; Generated autoloads from ob-comint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-comint" '("org-babel-comint-")))

;;;***

;;;### (autoloads nil "ob-coq" "ob-coq.el" (0 0 0 0))
;;; Generated autoloads from ob-coq.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-coq" '("coq-program-name" "org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-core" "ob-core.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-core" '("org-")))

;;;***

;;;### (autoloads nil "ob-csharp" "ob-csharp.el" (0 0 0 0))
;;; Generated autoloads from ob-csharp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-csharp" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-css" "ob-css.el" (0 0 0 0))
;;; Generated autoloads from ob-css.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-css" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-ditaa" "ob-ditaa.el" (0 0 0 0))
;;; Generated autoloads from ob-ditaa.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ditaa" '("org-")))

;;;***

;;;### (autoloads nil "ob-dot" "ob-dot.el" (0 0 0 0))
;;; Generated autoloads from ob-dot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-dot" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-ebnf" "ob-ebnf.el" (0 0 0 0))
;;; Generated autoloads from ob-ebnf.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ebnf" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-emacs-lisp" "ob-emacs-lisp.el" (0 0 0 0))
;;; Generated autoloads from ob-emacs-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-emacs-lisp" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-eshell" "ob-eshell.el" (0 0 0 0))
;;; Generated autoloads from ob-eshell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eshell" '("ob-eshell-session-live-p" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-eukleides" "ob-eukleides.el" (0 0 0 0))
;;; Generated autoloads from ob-eukleides.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eukleides" '("org-")))

;;;***

;;;### (autoloads nil "ob-eval" "ob-eval.el" (0 0 0 0))
;;; Generated autoloads from ob-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-eval" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-exp" "ob-exp.el" (0 0 0 0))
;;; Generated autoloads from ob-exp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-exp" '("org-")))

;;;***

;;;### (autoloads nil "ob-fomus" "ob-fomus.el" (0 0 0 0))
;;; Generated autoloads from ob-fomus.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fomus" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-forth" "ob-forth.el" (0 0 0 0))
;;; Generated autoloads from ob-forth.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-forth" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-fortran" "ob-fortran.el" (0 0 0 0))
;;; Generated autoloads from ob-fortran.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-fortran" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-gnuplot" "ob-gnuplot.el" (0 0 0 0))
;;; Generated autoloads from ob-gnuplot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-gnuplot" '("*org-babel-gnuplot-" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-groovy" "ob-groovy.el" (0 0 0 0))
;;; Generated autoloads from ob-groovy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-groovy" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-haskell" "ob-haskell.el" (0 0 0 0))
;;; Generated autoloads from ob-haskell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-haskell" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-hledger" "ob-hledger.el" (0 0 0 0))
;;; Generated autoloads from ob-hledger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-hledger" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-io" "ob-io.el" (0 0 0 0))
;;; Generated autoloads from ob-io.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-io" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-java" "ob-java.el" (0 0 0 0))
;;; Generated autoloads from ob-java.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-java" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-js" "ob-js.el" (0 0 0 0))
;;; Generated autoloads from ob-js.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-js" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-julia" "ob-julia.el" (0 0 0 0))
;;; Generated autoloads from ob-julia.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-julia" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-latex" "ob-latex.el" (0 0 0 0))
;;; Generated autoloads from ob-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-latex" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-ledger" "ob-ledger.el" (0 0 0 0))
;;; Generated autoloads from ob-ledger.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ledger" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-lilypond" "ob-lilypond.el" (0 0 0 0))
;;; Generated autoloads from ob-lilypond.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lilypond" '("lilypond-mode" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-lisp" "ob-lisp.el" (0 0 0 0))
;;; Generated autoloads from ob-lisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lisp" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-lob" "ob-lob.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-lob.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lob" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-lua" "ob-lua.el" (0 0 0 0))
;;; Generated autoloads from ob-lua.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-lua" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-makefile" "ob-makefile.el" (0 0 0 0))
;;; Generated autoloads from ob-makefile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-makefile" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-mathematica" "ob-mathematica.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ob-mathematica.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mathematica" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-mathomatic" "ob-mathomatic.el" (0 0 0 0))
;;; Generated autoloads from ob-mathomatic.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mathomatic" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-maxima" "ob-maxima.el" (0 0 0 0))
;;; Generated autoloads from ob-maxima.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-maxima" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-mscgen" "ob-mscgen.el" (0 0 0 0))
;;; Generated autoloads from ob-mscgen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-mscgen" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-ocaml" "ob-ocaml.el" (0 0 0 0))
;;; Generated autoloads from ob-ocaml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ocaml" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-octave" "ob-octave.el" (0 0 0 0))
;;; Generated autoloads from ob-octave.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-octave" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-org" "ob-org.el" (0 0 0 0))
;;; Generated autoloads from ob-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-org" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-oz" "ob-oz.el" (0 0 0 0))
;;; Generated autoloads from ob-oz.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-oz" '("org-babel-" "oz-send-string-expression")))

;;;***

;;;### (autoloads nil "ob-perl" "ob-perl.el" (0 0 0 0))
;;; Generated autoloads from ob-perl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-perl" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-php" "ob-php.el" (0 0 0 0))
;;; Generated autoloads from ob-php.el

(autoload 'org-babel-execute:php "ob-php" "\
Orgmode Babel PHP evaluate function for `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(eval-after-load "org" '(add-to-list 'org-src-lang-modes '("php" . php)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-php" '("ob-php:inf-php-buffer" "org-babel-")))

;;;***

;;;### (autoloads nil "ob-picolisp" "ob-picolisp.el" (0 0 0 0))
;;; Generated autoloads from ob-picolisp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-picolisp" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-plantuml" "ob-plantuml.el" (0 0 0 0))
;;; Generated autoloads from ob-plantuml.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-plantuml" '("org-")))

;;;***

;;;### (autoloads nil "ob-processing" "ob-processing.el" (0 0 0 0))
;;; Generated autoloads from ob-processing.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-processing" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-python" "ob-python.el" (0 0 0 0))
;;; Generated autoloads from ob-python.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-python" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-redis" "ob-redis.el" (0 0 0 0))
;;; Generated autoloads from ob-redis.el

(autoload 'org-babel-execute:redis "ob-redis" "\
org-babel redis hook.

\(fn BODY PARAMS)" nil nil)

(eval-after-load "org" '(add-to-list 'org-src-lang-modes '("redis" . redis)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-redis" '("ob-redis:default-db")))

;;;***

;;;### (autoloads nil "ob-ref" "ob-ref.el" (0 0 0 0))
;;; Generated autoloads from ob-ref.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ref" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-ruby" "ob-ruby.el" (0 0 0 0))
;;; Generated autoloads from ob-ruby.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-ruby" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-sass" "ob-sass.el" (0 0 0 0))
;;; Generated autoloads from ob-sass.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sass" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-scheme" "ob-scheme.el" (0 0 0 0))
;;; Generated autoloads from ob-scheme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-scheme" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-sclang" "ob-sclang.el" (0 0 0 0))
;;; Generated autoloads from ob-sclang.el

(autoload 'org-babel-execute:sclang "ob-sclang" "\
Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sclang" '("org-babel-default-header-args:sclang")))

;;;***

;;;### (autoloads nil "ob-screen" "ob-screen.el" (0 0 0 0))
;;; Generated autoloads from ob-screen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-screen" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-sed" "ob-sed.el" (0 0 0 0))
;;; Generated autoloads from ob-sed.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sed" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-shell" "ob-shell.el" (0 0 0 0))
;;; Generated autoloads from ob-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shell" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-shen" "ob-shen.el" (0 0 0 0))
;;; Generated autoloads from ob-shen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-shen" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-smiles" "ob-smiles.el" (0 0 0 0))
;;; Generated autoloads from ob-smiles.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-smiles" '("molecule-" "org-babel-execute:smiles")))

;;;***

;;;### (autoloads nil "ob-spice" "ob-spice.el" (0 0 0 0))
;;; Generated autoloads from ob-spice.el

(autoload 'org-babel-execute:spice "ob-spice" "\
Execute a block of Spice code `BODY' with org-babel and `PARAMS'.

\(fn BODY PARAMS)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-spice" '("ob-spice-concat" "org-babel-expand-body:spice")))

;;;***

;;;### (autoloads nil "ob-sql" "ob-sql.el" (0 0 0 0))
;;; Generated autoloads from ob-sql.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sql" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-sqlite" "ob-sqlite.el" (0 0 0 0))
;;; Generated autoloads from ob-sqlite.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-sqlite" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-stan" "ob-stan.el" (0 0 0 0))
;;; Generated autoloads from ob-stan.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stan" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-stata" "ob-stata.el" (0 0 0 0))
;;; Generated autoloads from ob-stata.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-stata" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-table" "ob-table.el" (0 0 0 0))
;;; Generated autoloads from ob-table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-table" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ob-tangle" "ob-tangle.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ob-tangle.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-tangle" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-tcl" "ob-tcl.el" (0 0 0 0))
;;; Generated autoloads from ob-tcl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-tcl" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-vala" "ob-vala.el" (0 0 0 0))
;;; Generated autoloads from ob-vala.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vala" '("org-babel-")))

;;;***

;;;### (autoloads nil "ob-vbnet" "ob-vbnet.el" (0 0 0 0))
;;; Generated autoloads from ob-vbnet.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ob-vbnet" '("org-babel-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ol" "ol.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ol-bbdb" "ol-bbdb.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ol-bbdb.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bbdb" '("org-bbdb-")))

;;;***

;;;### (autoloads nil "ol-bibtex" "ol-bibtex.el" (0 0 0 0))
;;; Generated autoloads from ol-bibtex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bibtex" '("org-")))

;;;***

;;;### (autoloads nil "ol-bookmark" "ol-bookmark.el" (0 0 0 0))
;;; Generated autoloads from ol-bookmark.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-bookmark" '("org-bookmark-")))

;;;***

;;;### (autoloads nil "ol-docview" "ol-docview.el" (0 0 0 0))
;;; Generated autoloads from ol-docview.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-docview" '("org-docview-")))

;;;***

;;;### (autoloads nil "ol-elisp-symbol" "ol-elisp-symbol.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from ol-elisp-symbol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-elisp-symbol" '("org-elisp-symbol-")))

;;;***

;;;### (autoloads nil "ol-eshell" "ol-eshell.el" (0 0 0 0))
;;; Generated autoloads from ol-eshell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eshell" '("org-eshell-")))

;;;***

;;;### (autoloads nil "ol-eww" "ol-eww.el" (0 0 0 0))
;;; Generated autoloads from ol-eww.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-eww" '("org-eww-")))

;;;***

;;;### (autoloads nil "ol-git-link" "ol-git-link.el" (0 0 0 0))
;;; Generated autoloads from ol-git-link.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-git-link" '("org-git")))

;;;***

;;;### (autoloads nil "ol-gnus" "ol-gnus.el" (0 0 0 0))
;;; Generated autoloads from ol-gnus.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-gnus" '("org-gnus-")))

;;;***

;;;### (autoloads nil "ol-info" "ol-info.el" (0 0 0 0))
;;; Generated autoloads from ol-info.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-info" '("org-info-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ol-irc" "ol-irc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ol-irc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-irc" '("org-irc-")))

;;;***

;;;### (autoloads nil "ol-man" "ol-man.el" (0 0 0 0))
;;; Generated autoloads from ol-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-man" '("org-man-")))

;;;***

;;;### (autoloads nil "ol-mew" "ol-mew.el" (0 0 0 0))
;;; Generated autoloads from ol-mew.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-mew" '("org-mew-")))

;;;***

;;;### (autoloads nil "ol-mhe" "ol-mhe.el" (0 0 0 0))
;;; Generated autoloads from ol-mhe.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-mhe" '("org-mhe-")))

;;;***

;;;### (autoloads nil "ol-notmuch" "ol-notmuch.el" (0 0 0 0))
;;; Generated autoloads from ol-notmuch.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-notmuch" '("org-notmuch-")))

;;;***

;;;### (autoloads nil "ol-rmail" "ol-rmail.el" (0 0 0 0))
;;; Generated autoloads from ol-rmail.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-rmail" '("org-rmail-")))

;;;***

;;;### (autoloads nil "ol-vm" "ol-vm.el" (0 0 0 0))
;;; Generated autoloads from ol-vm.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-vm" '("org-vm-")))

;;;***

;;;### (autoloads nil "ol-w3m" "ol-w3m.el" (0 0 0 0))
;;; Generated autoloads from ol-w3m.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-w3m" '("org-w3m-")))

;;;***

;;;### (autoloads nil "ol-wl" "ol-wl.el" (0 0 0 0))
;;; Generated autoloads from ol-wl.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ol-wl" '("org-wl-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org" "org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org" '("org-" "turn-on-org-cdlatex")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-agenda" "org-agenda.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-agenda.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-agenda" '("org-")))

;;;***

;;;### (autoloads nil "org-annotate-file" "org-annotate-file.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-annotate-file.el

(autoload 'org-annotate-file "org-annotate-file" "\
Visit `org-annotate-file-storage-file` and add a new annotation section.
The annotation is opened at the new section which will be referencing
the point in the current file." t nil)

(autoload 'org-annotate-file-show-section "org-annotate-file" "\
Add or show annotation entry in STORAGE-FILE and return the buffer.
The annotation will link to ANNOTATED-BUFFER if specified,
  otherwise the current buffer is used.

\(fn STORAGE-FILE &optional ANNOTATED-BUFFER)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-annotate-file" '("org-annotate-file-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-archive"
;;;;;;  "org-archive.el" (0 0 0 0))
;;; Generated autoloads from org-archive.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-archive" '("org-a")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-attach" "org-attach.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-attach.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach" '("org-attach-")))

;;;***

;;;### (autoloads nil "org-attach-embedded-images" "org-attach-embedded-images.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-attach-embedded-images.el

(autoload 'org-attach-embedded-images-in-subtree "org-attach-embedded-images" "\
Save the displayed images as attachments and insert links to them." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach-embedded-images" '("org-attach-embedded-images--")))

;;;***

;;;### (autoloads nil "org-attach-git" "org-attach-git.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-attach-git.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-attach-git" '("org-attach-git-")))

;;;***

;;;### (autoloads nil "org-bibtex-extras" "org-bibtex-extras.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-bibtex-extras.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-bibtex-extras" '("obe-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-capture"
;;;;;;  "org-capture.el" (0 0 0 0))
;;; Generated autoloads from org-capture.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture" '("org-capture-")))

;;;***

;;;### (autoloads nil "org-checklist" "org-checklist.el" (0 0 0 0))
;;; Generated autoloads from org-checklist.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-checklist" '("org-")))

;;;***

;;;### (autoloads nil "org-choose" "org-choose.el" (0 0 0 0))
;;; Generated autoloads from org-choose.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-choose" '("org-choose-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-clock" "org-clock.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-clock.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-clock" '("org-")))

;;;***

;;;### (autoloads nil "org-collector" "org-collector.el" (0 0 0 0))
;;; Generated autoloads from org-collector.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-collector" '("and-rest" "org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-colview"
;;;;;;  "org-colview.el" (0 0 0 0))
;;; Generated autoloads from org-colview.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-colview" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-compat" "org-compat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-compat.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-compat" '("org-")))

;;;***

;;;### (autoloads nil "org-contacts" "org-contacts.el" (0 0 0 0))
;;; Generated autoloads from org-contacts.el

(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-contacts" '("erc-nicknames-list" "org-co")))

;;;***

;;;### (autoloads nil "org-crypt" "org-crypt.el" (0 0 0 0))
;;; Generated autoloads from org-crypt.el

(autoload 'org-encrypt-entry "org-crypt" "\
Encrypt the content of the current headline." t nil)

(autoload 'org-decrypt-entry "org-crypt" "\
Decrypt the content of the current headline." t nil)

(autoload 'org-encrypt-entries "org-crypt" "\
Encrypt all top-level entries in the current buffer." t nil)

(autoload 'org-decrypt-entries "org-crypt" "\
Decrypt all entries in the current buffer." t nil)

(autoload 'org-crypt-use-before-save-magic "org-crypt" "\
Add a hook to automatically encrypt entries before a file is saved to disk." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-crypt" '("org-")))

;;;***

;;;### (autoloads nil "org-ctags" "org-ctags.el" (0 0 0 0))
;;; Generated autoloads from org-ctags.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-ctags" '("org-ctags-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-datetree"
;;;;;;  "org-datetree.el" (0 0 0 0))
;;; Generated autoloads from org-datetree.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-datetree" '("org-datetree-")))

;;;***

;;;### (autoloads nil "org-depend" "org-depend.el" (0 0 0 0))
;;; Generated autoloads from org-depend.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-depend" '("org-depend-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-duration"
;;;;;;  "org-duration.el" (0 0 0 0))
;;; Generated autoloads from org-duration.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-duration" '("org-duration-")))

;;;***

;;;### (autoloads nil "org-effectiveness" "org-effectiveness.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-effectiveness.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-effectiveness" '("org-effectiveness-")))

;;;***

;;;### (autoloads nil "org-eldoc" "org-eldoc.el" (0 0 0 0))
;;; Generated autoloads from org-eldoc.el

(autoload 'org-eldoc-load "org-eldoc" "\
Set up org-eldoc documentation function." t nil)

(add-hook 'org-mode-hook #'org-eldoc-load)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eldoc" '("org-eldoc-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-element"
;;;;;;  "org-element.el" (0 0 0 0))
;;; Generated autoloads from org-element.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-element" '("org-element-")))

;;;***

;;;### (autoloads nil "org-entities" "org-entities.el" (0 0 0 0))
;;; Generated autoloads from org-entities.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-entities" '("org-entit")))

;;;***

;;;### (autoloads nil "org-eval" "org-eval.el" (0 0 0 0))
;;; Generated autoloads from org-eval.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eval" '("org-eval-")))

;;;***

;;;### (autoloads nil "org-eval-light" "org-eval-light.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-eval-light.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-eval-light" '("org-eval-light-")))

;;;***

;;;### (autoloads nil "org-expiry" "org-expiry.el" (0 0 0 0))
;;; Generated autoloads from org-expiry.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-expiry" '("org-expiry-")))

;;;***

;;;### (autoloads nil "org-faces" "org-faces.el" (0 0 0 0))
;;; Generated autoloads from org-faces.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-faces" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-feed" "org-feed.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-feed.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-feed" '("org-feed-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-footnote"
;;;;;;  "org-footnote.el" (0 0 0 0))
;;; Generated autoloads from org-footnote.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-footnote" '("org-footnote-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-goto" "org-goto.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-goto.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-goto" '("org-goto-")))

;;;***

;;;### (autoloads nil "org-habit" "org-habit.el" (0 0 0 0))
;;; Generated autoloads from org-habit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-habit" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-id" "org-id.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-id.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-id" '("org-id-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-indent" "org-indent.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-indent.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-indent" '("org-indent-")))

;;;***

;;;### (autoloads nil "org-inlinetask" "org-inlinetask.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-inlinetask.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-inlinetask" '("org-inlinetask-")))

;;;***

;;;### (autoloads nil "org-interactive-query" "org-interactive-query.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-interactive-query.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-interactive-query" '("org-agenda-query-")))

;;;***

;;;### (autoloads nil "org-invoice" "org-invoice.el" (0 0 0 0))
;;; Generated autoloads from org-invoice.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-invoice" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-keys" "org-keys.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-keys.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-keys" '("org-")))

;;;***

;;;### (autoloads nil "org-learn" "org-learn.el" (0 0 0 0))
;;; Generated autoloads from org-learn.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-learn" '("calculate-new-optimal-factor" "determine-next-interval" "get-optimal-factor" "initial-" "inter-repetition-interval" "modify-" "org-" "set-optimal-factor")))

;;;***

;;;### (autoloads nil "org-license" "org-license.el" (0 0 0 0))
;;; Generated autoloads from org-license.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-license" '("org-license-")))

;;;***

;;;### (autoloads nil "org-link-edit" "org-link-edit.el" (0 0 0 0))
;;; Generated autoloads from org-link-edit.el

(autoload 'org-link-edit-forward-slurp "org-link-edit" "\
Slurp N trailing blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org mode site]]

A blob is a block of non-whitespace characters.  When slurping
forward, trailing punctuation characters are not considered part
of a blob.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-slurp "org-link-edit" "\
Slurp N leading blobs into link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  [[https://orgmode.org/][The Org mode]] site

A blob is a block of non-whitespace characters.

After slurping, return the slurped text and move point to the
beginning of the link.

If N is negative, slurp trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-forward-barf "org-link-edit" "\
Barf N trailing blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The [[https://orgmode.org/][Org]] mode site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf leading blobs instead of trailing blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-backward-barf "org-link-edit" "\
Barf N leading blobs from link's description.

  The [[https://orgmode.org/][Org mode]] site

                        |
                        v

  The Org [[https://orgmode.org/][mode]] site

A blob is a block of non-whitespace characters.

After barfing, return the barfed text and move point to the
beginning of the link.

If N is negative, barf trailing blobs instead of leading blobs.

\(fn &optional N)" t nil)

(autoload 'org-link-edit-transport-next-link "org-link-edit" "\
Move the next link to point.

If the region is active, use the selected text as the link's
description.  Otherwise, use the word at point.

With prefix argument PREVIOUS, move the previous link instead of
the next link.

Non-interactively, use the text between BEG and END as the
description, moving the next (or previous) link relative to BEG
and END.  By default, refuse to overwrite an existing
description.  If OVERWRITE is `ask', prompt for confirmation
before overwriting; for any other non-nil value, overwrite
without asking.

\(fn &optional PREVIOUS BEG END OVERWRITE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-link-edit" '("org-link-edit--")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-lint" "org-lint.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-lint.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-lint" '("org-lint-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-list" "org-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-list.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-list" '("org-")))

;;;***

;;;### (autoloads nil "org-mac-iCal" "org-mac-iCal.el" (0 0 0 0))
;;; Generated autoloads from org-mac-iCal.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mac-iCal" '("omi-" "org-mac-iCal")))

;;;***

;;;### (autoloads nil "org-mac-link" "org-mac-link.el" (0 0 0 0))
;;; Generated autoloads from org-mac-link.el

(autoload 'org-mac-grab-link "org-mac-link" "\
Prompt for an application to grab a link from.
When done, go grab the link, and insert it at point." t nil)

(autoload 'org-mac-firefox-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-firefox-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-vimperator-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-vimperator-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-chrome-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-chrome-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-brave-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-brave-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-safari-get-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-safari-insert-frontmost-url "org-mac-link" nil t nil)

(autoload 'org-mac-together-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-together-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-finder-item-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-finder-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-addressbook-item-get-selected "org-mac-link" nil t nil)

(autoload 'org-mac-addressbook-insert-selected "org-mac-link" nil t nil)

(autoload 'org-mac-skim-get-page "org-mac-link" nil t nil)

(autoload 'org-mac-skim-insert-page "org-mac-link" nil t nil)

(autoload 'org-mac-acrobat-get-page "org-mac-link" nil t nil)

(autoload 'org-mac-acrobat-insert-page "org-mac-link" nil t nil)

(autoload 'org-mac-outlook-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject of the
messages in Microsoft Outlook.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-outlook-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Microsoft Outlook.app.
This will use AppleScript to get the message-id and the subject
of the active mail in Microsoft Outlook.app and make a link out
of it." t nil)

(autoload 'org-mac-outlook-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all mac-outlook:// links within
heading's first level.  If heading doesn't exist, create it at
point-max.  Insert list of mac-outlook:// links to flagged mail
after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(autoload 'org-mac-evernote-note-insert-selected "org-mac-link" "\
Insert a link to the notes currently selected in Evernote.app.
This will use AppleScript to get the note id and the title of the
note(s) in Evernote.app and make a link out of it/them." t nil)

(autoload 'org-mac-devonthink-item-insert-selected "org-mac-link" "\
Insert a link to the item(s) currently selected in DEVONthink Pro Office.
This will use AppleScript to get the `uuid'(s) and the name(s) of the
selected items in DEVONthink Pro Office and make link(s) out of it/them." t nil)

(autoload 'org-mac-message-get-links "org-mac-link" "\
Create links to the messages currently selected or flagged in Mail.app.
This will use AppleScript to get the message-id and the subject of the
messages in Mail.app and make a link out of it.
When SELECT-OR-FLAG is \"s\", get the selected messages (this is also
the default).  When SELECT-OR-FLAG is \"f\", get the flagged messages.
The Org-syntax text will be pushed to the kill ring, and also returned.

\(fn &optional SELECT-OR-FLAG)" t nil)

(autoload 'org-mac-message-insert-selected "org-mac-link" "\
Insert a link to the messages currently selected in Mail.app.
This will use AppleScript to get the message-id and the subject of the
active mail in Mail.app and make a link out of it." t nil)

(autoload 'org-mac-message-insert-flagged "org-mac-link" "\
Asks for an org buffer and a heading within it, and replace message links.
If heading exists, delete all message:// links within heading's first
level.  If heading doesn't exist, create it at point-max.  Insert
list of message:// links to flagged mail after heading.

\(fn ORG-BUFFER ORG-HEADING)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mac-link" '("as-get-s" "org-")))

;;;***

;;;### (autoloads nil "org-macro" "org-macro.el" (0 0 0 0))
;;; Generated autoloads from org-macro.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macro" '("org-macro-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-macs" "org-macs.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-macs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-macs" '("org-")))

;;;***

;;;### (autoloads nil "org-mairix" "org-mairix.el" (0 0 0 0))
;;; Generated autoloads from org-mairix.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mairix" '("org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-mobile" "org-mobile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-mobile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mobile" '("org-mobile-")))

;;;***

;;;### (autoloads nil "org-mouse" "org-mouse.el" (0 0 0 0))
;;; Generated autoloads from org-mouse.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-mouse" '("org-mouse-")))

;;;***

;;;### (autoloads nil "org-notify" "org-notify.el" (0 0 0 0))
;;; Generated autoloads from org-notify.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-notify" '("org-notify-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-num" "org-num.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-num.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-num" '("org-num-")))

;;;***

;;;### (autoloads nil "org-panel" "org-panel.el" (0 0 0 0))
;;; Generated autoloads from org-panel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-panel" '("orgpan-")))

;;;***

;;;### (autoloads nil "org-passwords" "org-passwords.el" (0 0 0 0))
;;; Generated autoloads from org-passwords.el

(autoload 'org-passwords-mode "org-passwords" "\
Mode for storing passwords

\(fn)" t nil)

(autoload 'org-passwords "org-passwords" "\
Open the password file. Open the password file defined by the
variable `org-password-file' in read-only mode and kill that
buffer later according to the value of the variable
`org-passwords-time-opened'. It also adds the `org-password-file'
to the auto-mode-alist so that it is opened with its mode being
`org-passwords-mode'.

With prefix arg ARG, the command does not set up a timer to kill the buffer.

With a double prefix arg \\[universal-argument] \\[universal-argument], open the file for editing.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-passwords" '("org-passwords-")))

;;;***

;;;### (autoloads nil "org-pcomplete" "org-pcomplete.el" (0 0 0 0))
;;; Generated autoloads from org-pcomplete.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-pcomplete" '("org-" "pcomplete/org-mode/")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-plot" "org-plot.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-plot.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-plot" '("org-plot")))

;;;***

;;;### (autoloads nil "org-protocol" "org-protocol.el" (0 0 0 0))
;;; Generated autoloads from org-protocol.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-protocol" '("org-protocol-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-refile" "org-refile.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-refile.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-refile" '("org-")))

;;;***

;;;### (autoloads nil "org-registry" "org-registry.el" (0 0 0 0))
;;; Generated autoloads from org-registry.el

(autoload 'org-registry-show "org-registry" "\
Show Org files where there are links pointing to the current
buffer.

\(fn &optional VISIT)" t nil)

(autoload 'org-registry-visit "org-registry" "\
If an Org file contains a link to the current location, visit
this file." t nil)

(autoload 'org-registry-initialize "org-registry" "\
Initialize `org-registry-alist'.
If FROM-SCRATCH is non-nil or the registry does not exist yet,
create a new registry from scratch and eval it. If the registry
exists, eval `org-registry-file' and make it the new value for
`org-registry-alist'.

\(fn &optional FROM-SCRATCH)" t nil)

(autoload 'org-registry-insinuate "org-registry" "\
Call `org-registry-update' after saving in Org-mode.
Use with caution.  This could slow down things a bit." t nil)

(autoload 'org-registry-update "org-registry" "\
Update the registry for the current Org file." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-registry" '("org-registry-")))

;;;***

;;;### (autoloads nil "org-screen" "org-screen.el" (0 0 0 0))
;;; Generated autoloads from org-screen.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-screen" '("org-screen")))

;;;***

;;;### (autoloads nil "org-screenshot" "org-screenshot.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-screenshot.el

(autoload 'org-screenshot-take "org-screenshot" "\
Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds

\(fn &optional DELAY)" t nil)

(autoload 'org-screenshot-rotate-prev "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-rotate-next "org-screenshot" "\
Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction

\(fn DIR)" t nil)

(autoload 'org-screenshot-show-unused "org-screenshot" "\
Open A Dired buffer with unused screenshots marked" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-screenshot" '("org-screenshot-")))

;;;***

;;;### (autoloads nil "org-secretary" "org-secretary.el" (0 0 0 0))
;;; Generated autoloads from org-secretary.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-secretary" '("join" "org-sec-")))

;;;***

;;;### (autoloads nil "org-src" "org-src.el" (0 0 0 0))
;;; Generated autoloads from org-src.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-src" '("org-")))

;;;***

;;;### (autoloads nil "org-static-mathjax" "org-static-mathjax.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-static-mathjax.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-static-mathjax" '("org-static-mathjax-")))

;;;***

;;;### (autoloads nil "org-sudoku" "org-sudoku.el" (0 0 0 0))
;;; Generated autoloads from org-sudoku.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-sudoku" '("org-sudoku-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-table" "org-table.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-table.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-table" '("org")))

;;;***

;;;### (autoloads nil "org-tempo" "org-tempo.el" (0 0 0 0))
;;; Generated autoloads from org-tempo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-tempo" '("org-tempo-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "org-timer" "org-timer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-timer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-timer" '("org-timer-")))

;;;***

;;;### (autoloads nil "org-toc" "org-toc.el" (0 0 0 0))
;;; Generated autoloads from org-toc.el

(autoload 'org-toc-show "org-toc" "\
Show the table of contents of the current Org-mode buffer.

\(fn &optional DEPTH POSITION)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-toc" '("org-")))

;;;***

;;;### (autoloads nil "org-track" "org-track.el" (0 0 0 0))
;;; Generated autoloads from org-track.el

(autoload 'org-track-fetch-package "org-track" "\
Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY.

\(fn &optional DIRECTORY)" t nil)

(autoload 'org-track-compile-org "org-track" "\
Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo.

\(fn &optional DIRECTORY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-track" '("org-track-")))

;;;***

;;;### (autoloads nil "org-velocity" "org-velocity.el" (0 0 0 0))
;;; Generated autoloads from org-velocity.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-velocity" '("org-velocity")))

;;;***

;;;### (autoloads nil "org-wikinodes" "org-wikinodes.el" (0 0 0 0))
;;; Generated autoloads from org-wikinodes.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-wikinodes" '("org-wikinodes-")))

;;;***

;;;### (autoloads nil "orgtbl-sqlinsert" "orgtbl-sqlinsert.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from orgtbl-sqlinsert.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "orgtbl-sqlinsert" '("orgtbl-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox" "ox.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from ox.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox" '("org-export-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-ascii" "ox-ascii.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-ascii.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-ascii" '("org-ascii-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-beamer" "ox-beamer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-beamer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-beamer" '("org-beamer-")))

;;;***

;;;### (autoloads nil "ox-bibtex" "ox-bibtex.el" (0 0 0 0))
;;; Generated autoloads from ox-bibtex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-bibtex" '("org-bibtex-")))

;;;***

;;;### (autoloads nil "ox-confluence" "ox-confluence.el" (0 0 0 0))
;;; Generated autoloads from ox-confluence.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-confluence" '("org-confluence-")))

;;;***

;;;### (autoloads nil "ox-deck" "ox-deck.el" (0 0 0 0))
;;; Generated autoloads from ox-deck.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-deck" '("org-deck-")))

;;;***

;;;### (autoloads nil "ox-extra" "ox-extra.el" (0 0 0 0))
;;; Generated autoloads from ox-extra.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-extra" '("org-" "ox-extras")))

;;;***

;;;### (autoloads nil "ox-freemind" "ox-freemind.el" (0 0 0 0))
;;; Generated autoloads from ox-freemind.el

(autoload 'org-freemind-export-to-freemind "ox-freemind" "\
Export current buffer to a Freemind Mindmap file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-freemind" '("org-freemind-")))

;;;***

;;;### (autoloads nil "ox-groff" "ox-groff.el" (0 0 0 0))
;;; Generated autoloads from ox-groff.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-groff" '("org-groff-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-html" "ox-html.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-html.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-html" '("org-html-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-icalendar"
;;;;;;  "ox-icalendar.el" (0 0 0 0))
;;; Generated autoloads from ox-icalendar.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-icalendar" '("org-icalendar-")))

;;;***

;;;### (autoloads nil "ox-koma-letter" "ox-koma-letter.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ox-koma-letter.el

(autoload 'org-koma-letter-export-as-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org KOMA-LETTER Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-latex "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(autoload 'org-koma-letter-export-to-pdf "ox-koma-letter" "\
Export current buffer as a KOMA Scrlttr2 letter (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY BODY-ONLY EXT-PLIST)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-koma-letter" '("org-koma-letter-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-latex" "ox-latex.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-latex.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-latex" '("org-latex-")))

;;;***

;;;### (autoloads nil "ox-man" "ox-man.el" (0 0 0 0))
;;; Generated autoloads from ox-man.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-man" '("org-man-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-md" "ox-md.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-md.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-md" '("org-md-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-odt" "ox-odt.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-odt.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-odt" '("org-odt-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-org" "ox-org.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-org" '("org-org-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-publish" "ox-publish.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-publish.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-publish" '("org-publish-")))

;;;***

;;;### (autoloads nil "ox-rss" "ox-rss.el" (0 0 0 0))
;;; Generated autoloads from ox-rss.el

(autoload 'org-rss-export-as-rss "ox-rss" "\
Export current buffer to an RSS buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org RSS Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-export-to-rss "ox-rss" "\
Export current buffer to an RSS file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-rss-publish-to-rss "ox-rss" "\
Publish an org file to RSS.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name.

\(fn PLIST FILENAME PUB-DIR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-rss" '("org-rss-")))

;;;***

;;;### (autoloads nil "ox-s5" "ox-s5.el" (0 0 0 0))
;;; Generated autoloads from ox-s5.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-s5" '("org-s5-")))

;;;***

;;;### (autoloads nil "ox-taskjuggler" "ox-taskjuggler.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from ox-taskjuggler.el

(autoload 'org-taskjuggler-export "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name.

\(fn &optional ASYNC SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-and-process "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-taskjuggler-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-taskjuggler-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-taskjuggler-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(autoload 'org-taskjuggler-export-process-and-open "ox-taskjuggler" "\
Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-taskjuggler-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-taskjuggler-target-version') the processing and display of
the reports is done using the TaskJuggler GUI.

\(fn &optional SUBTREEP VISIBLE-ONLY)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-taskjuggler" '("org-taskjuggler-")))

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "ox-texinfo" "ox-texinfo.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ox-texinfo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ox-texinfo" '("org-texinfo-")))

;;;***

;;;### (autoloads nil nil ("ob-core.el" "ob-lob.el" "ob-matlab.el"
;;;;;;  "ob-tangle.el" "ob.el" "ol-bbdb.el" "ol-irc.el" "ol.el" "org-agenda.el"
;;;;;;  "org-archive.el" "org-attach.el" "org-capture.el" "org-clock.el"
;;;;;;  "org-colview.el" "org-compat.el" "org-contribdir.el" "org-datetree.el"
;;;;;;  "org-duration.el" "org-element.el" "org-feed.el" "org-footnote.el"
;;;;;;  "org-goto.el" "org-id.el" "org-indent.el" "org-install.el"
;;;;;;  "org-keys.el" "org-lint.el" "org-list.el" "org-macs.el" "org-mobile.el"
;;;;;;  "org-num.el" "org-plot.el" "org-refile.el" "org-table.el"
;;;;;;  "org-timer.el" "org.el" "ox-ascii.el" "ox-beamer.el" "ox-html.el"
;;;;;;  "ox-icalendar.el" "ox-latex.el" "ox-md.el" "ox-odt.el" "ox-org.el"
;;;;;;  "ox-publish.el" "ox-texinfo.el" "ox.el") (0 0 0 0))

;;;***

(provide 'org-plus-contrib-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-plus-contrib-autoloads.el ends here
