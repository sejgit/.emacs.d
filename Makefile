# * makem.sh/Makefile --- Script to aid building and testing Emacs Lisp packages
# URL: https://github.com/alphapapa/makem.sh
# Version: 0.5
# sej adds

.PHONY: lint

# * Arguments

# For consistency, we use only var=val options, not hyphen-prefixed options.

# NOTE: I don't like duplicating the arguments here and in makem.sh,
# but I haven't been able to find a way to pass arguments which
# conflict with Make's own arguments through Make to the script.
# Using -- doesn't seem to do it.

ifdef sandbox
	ifeq ($(sandbox), t)
		SANDBOX = --sandbox
	else
		SANDBOX = --sandbox=$(sandbox)
	endif
endif

# * Rules

.DEFAULT: lint

lint:
	pre-commit run --all-files
