emacs ?= emacs
elmake = $(emacs) -batch -l makefi.el -f

LOAD = -l elpa.el -l colir.el -l ivy-overlay.el -l ivy.el -l swiper.el -l counsel.el
RM ?= rm -f

all: test

deps:
	$(emacs) -batch -l targets/install-deps.el

test: lazy-load-test other-tests

other-tests:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ivy-test-run-other-tests
lazy-load-test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ivy-test-run-lazy-load-test

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -L . -f batch-byte-compile colir.el ivy-overlay.el ivy.el swiper.el counsel.el

plain:
	$(emacs) --version
	$(emacs) -Q $(LOAD) -l targets/plain.el

obsolete:
	$(emacs) -batch -l targets/obsolete-config.el

update-issues:
	$(elmake) update-issues

clean:
	$(RM) *.elc

.PHONY: all test checkdoc compile plain obsolete update-issues clean
