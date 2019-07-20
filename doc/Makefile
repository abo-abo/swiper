INFO_INSTALL_DIR="${HOME}/git/gnu-elpa/packages/ivy"

ivy.texi: ivy.org
	emacs -batch -l ivy-ox.el -l scripts.el --eval "(org-to-texi \"ivy.org\")"

ivy.info: ivy.texi
	makeinfo --no-split $^

install: ivy.info
	cp $^ $(INFO_INSTALL_DIR)
	install-info --info-dir=$(INFO_INSTALL_DIR) --info-file=$^
