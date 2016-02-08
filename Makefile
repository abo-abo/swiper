index.html: ../doc/ivy.org Makefile init.el
	emacs -Q -l init.el $< -f doexport
	mv ../doc/ivy.html $@
