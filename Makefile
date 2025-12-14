# * makem.sh/Makefile --- Script to aid building and testing Emacs Lisp packages
# URL: https://github.com/alphapapa/makem.sh
# Version: 0.5
# sej adds

.PHONY: lint check

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

.DEFAULT: check

# Run elisp linting directly
lint:
	@echo "Linting Emacs Lisp files in lisp/ directory..."
	@for file in lisp/*.el; do \
		echo "Linting $$file..."; \
		emacs --batch \
			--eval "(progn \
				(setq package-archives '((\"melpa\" . \"https://melpa.org/packages/\") \
										 (\"gnu\" . \"https://elpa.gnu.org/packages/\") \
										 (\"nongnu\" . \"https://elpa.nongnu.org/nongnu/\"))) \
				(package-initialize) \
				(unless (package-installed-p 'package-lint) \
					(package-refresh-contents) \
					(package-install 'package-lint)) \
				(require 'package-lint))" \
			-f package-lint-batch-and-exit "$$file" || exit 1; \
	done
	@echo "Linting complete!"

# Run pre-commit hooks (for git commits)
check:
	pre-commit run --all-files
