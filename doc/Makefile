# Build and install Ivy Info manual from Org source.

# Copyright (C) 2018-2025 Free Software Foundation, Inc.
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
INSTALL ?= install
INSTALL_DATA ?= $(INSTALL) -m 644
MAKEINFO_FLAGS += --no-split
RM ?= rm -f
infodir ?= $${HOME}/git/gnu-elpa/packages/ivy

info-src := ivy.info
info-dest = $(infodir)/$(info-src)

.PHONY: info
info: $(info-src)

.PHONY: install
install: info
	$(INSTALL_DATA) $(info-src) "$(infodir)"
	$(POST_INSTALL)
	install-info --info-dir="$(infodir)" "$(info-dest)"

.PHONY: uninstall
uninstall:
	$(PRE_UNINSTALL)
	install-info --delete --info-dir="$(infodir)" "$(info-dest)"
	$(NORMAL_UNINSTALL)
	$(RM) "$(info-dest)"

.PHONY: clean
clean:
	$(RM) $(info-src)

ivy.texi: ivy.org ivy-ox.el
	$(EMACS) -Q -batch -l ivy-ox.el \
	  -eval '(setq org-confirm-babel-evaluate nil)' \
	  -f org-texinfo-export-to-texinfo-batch $< $@
