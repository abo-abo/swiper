emacs ?= emacs

LOAD = -l ivy.el -l swiper.el

.PHONY: all compile clean

all: test

test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ert-run-tests-batch-and-exit

compile:
	$(emacs) -batch $(LOAD) --eval "(mapc #'byte-compile-file '(\"ivy.el\" \"swiper.el\" \"counsel.el\"))"

clean:
	rm -f *.elc
