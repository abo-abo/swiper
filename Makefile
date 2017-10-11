emacs ?= emacs
elmake = $(emacs) -batch -l makefi.el -f

LOAD = -l colir.el -l ivy-overlay.el -l ivy.el -l swiper.el -l counsel.el

all: test

test:
	$(emacs) -batch $(LOAD) -l ivy-test.el -f ert-run-tests-batch-and-exit

checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

compile:
	$(emacs) -batch --eval "(progn (add-to-list 'load-path default-directory) (mapc #'byte-compile-file '(\"ivy.el\" \"swiper.el\" \"counsel.el\" \"colir.el\" \"ivy-overlay.el\")))"

plain:
	$(emacs) --version
	$(emacs) -Q $(LOAD) -l targets/plain.el

obsolete:
	$(emacs) -batch -l targets/obsolete-config.el

update-issues:
	$(elmake) update-issues

clean:
	rm -f *.elc

.PHONY: all compile clean test update-issues checkdoc
