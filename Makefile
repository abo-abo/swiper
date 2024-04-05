emacs ?= emacs
RM ?= rm -f

src-elcs = \
  colir.elc \
  ivy-faces.elc \
  ivy-overlay.elc \
  ivy.elc \
  ivy-avy.elc \
  ivy-hydra.elc \
  swiper.elc \
  counsel.elc

test-elcs = ivy-test.elc

.PHONY: all
all: compile

.PHONY: deps
deps:
	$(emacs) -Q -batch -l targets/install-deps.el

.PHONY: test
test: compile $(test-elcs)
	$(emacs) -Q -batch -l elpa.el -L . -l ivy-test -f ivy-test-run-tests

.PHONY: checkdoc
checkdoc:
	$(emacs) -batch -l targets/checkdoc.el

.PHONY: compile
compile: $(src-elcs)

.PHONY: plain
plain: compile
	$(emacs) -version
	$(emacs) -Q -l elpa.el -L . -l targets/plain.el

.PHONY: obsolete
obsolete:
	$(emacs) -batch -l targets/obsolete-config.el

.PHONY: clean
clean:
	$(RM) $(src-elcs) $(test-elcs)

%.elc: %.el
	$(emacs) -Q -batch -L . -f batch-byte-compile $<

ivy-avy.elc: ivy-avy.el
ivy-hydra.elc: ivy-hydra.el
ivy-avy.elc ivy-hydra.elc:
	$(emacs) -Q -batch -l elpa.el -L . -f batch-byte-compile $<
