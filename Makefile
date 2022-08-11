emacs ?= emacs

LOAD = -l elpa.el -l colir.el -l ivy-overlay.el -l ivy.el -l swiper.el -l counsel.el
RM ?= rm -f

all: test

deps:
	$(emacs) -batch -l targets/install-deps.el

test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ivy-test-run-tests

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch -l elpa.el -L . -f batch-byte-compile colir.el ivy-faces.el ivy-overlay.el ivy.el ivy-avy.el ivy-hydra.el swiper.el counsel.el

plain:
	$(emacs) --version
	$(emacs) -Q -l elpa.el -l targets/plain.el

obsolete:
	$(emacs) -batch -l targets/obsolete-config.el

clean:
	$(RM) *.elc

.PHONY: all test checkdoc compile plain obsolete update-issues clean
