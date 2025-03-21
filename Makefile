# Build and test Ivy from a source checkout.

# Copyright (C) 2015-2025 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

EMACS ?= emacs
RM ?= rm -f

opt-elcs := \
  ivy-avy.elc \
  ivy-hydra.elc

src-elcs := \
  colir.elc \
  ivy-faces.elc \
  ivy-overlay.elc \
  ivy.elc \
  swiper.elc \
  counsel.elc \
  $(opt-elcs)

test-elcs := ivy-test.elc

.PHONY: all
all: compile

.PHONY: deps
deps:
	$(EMACS) -Q -batch -l targets/elpa.el -f ivy--elpa-install

.PHONY: compile
compile: $(src-elcs)

.PHONY: test
test: compile $(test-elcs)
	$(EMACS) -Q -batch -L . $(test-elcs:%.elc=-l %) -f ivy-test-run-tests

.PHONY: clean
clean:
	$(RM) $(src-elcs) $(test-elcs)

.PHONY: checkdoc
checkdoc:
	$(EMACS) -Q -batch -L . -l targets/checkdoc.el $(src-elcs:c=)

.PHONY: check-declare
check-declare:
	$(EMACS) -Q -batch -eval '(check-declare-directory "$(CURDIR)")'

.PHONY: plain
plain: compile
	$(EMACS) -Q -L . -l targets/plain.el

%.elc: %.el
	$(EMACS) -Q -batch -L . $(ELCFLAGS) -f batch-byte-compile $<

$(opt-elcs): ELCFLAGS += -l targets/elpa.el -f ivy--elpa-activate
