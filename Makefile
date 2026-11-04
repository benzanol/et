.PHONY: all ci check compile unit-test check-test clean generate-docs

EMACS ?= emacs

ELISP_FILES := et-annotate.el et.el et-check.el et-cache.el et-flymake.el
ELC_FILES := $(ELISP_FILES:.el=.elc)

all: check
ci: check
check: compile unit-test check-test

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(ELISP_FILES)

unit-test:
	$(EMACS) -Q --batch -L . -L test \
		-l test/unit-tests.el \
		-f ert-run-tests-batch-and-exit

check-test:
	$(EMACS) -Q --batch -L . -L test \
		-l test/check-tests.el \
		-f ert-run-tests-batch-and-exit

clean:
	$(RM) $(ELC_FILES)

generate-docs:
	$(EMACS) --script ./docs/generate.el ./docs
