emacs ?= emacs

LOAD = -l colir.el -l ivy.el -l swiper.el -l counsel.el

.PHONY: all compile clean

all: test

test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch --eval "(progn (add-to-list 'load-path default-directory) (mapc #'byte-compile-file '(\"ivy.el\" \"swiper.el\" \"counsel.el\")))"

clean:
	rm -f *.elc
