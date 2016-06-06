emacs ?= emacs

LOAD = -l colir.el -l ivy.el -l swiper.el -l counsel.el

all: test

test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch --eval "(progn (add-to-list 'load-path default-directory) (mapc #'byte-compile-file '(\"ivy.el\" \"swiper.el\" \"counsel.el\")))"

plain:
	$(emacs) -Q $(LOAD) --eval "(progn (package-initialize) (ivy-mode))" -l ivy-hydra.el

clean:
	rm -f *.elc

.PHONY: all compile clean test
