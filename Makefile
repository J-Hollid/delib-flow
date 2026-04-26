.PHONY: test coverage complexity validate

EMACS ?= emacs
EMACS_BATCH = $(EMACS) -Q --batch -L . -L test

test:
	$(EMACS_BATCH) -l test/delib-flow-test.el -f ert-run-tests-batch-and-exit

coverage:
	$(EMACS_BATCH) -l scripts/validate-coverage.el

complexity:
	$(EMACS_BATCH) -l scripts/validate-complexity.el

validate: test coverage complexity
