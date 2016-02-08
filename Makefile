index.html: ../doc/ivy.org
	emacs -batch -l init.el $^ -f org-html-export-to-html
	mv ../doc/ivy.html $@


