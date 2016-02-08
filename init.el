(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/org-mode/lisp")
(require 'org)
(load-file "~/Dropbox/source/site-lisp/git/eclipse-theme/eclipse-theme.el")
(load-file "~/Dropbox/source/site-lisp/git/htmlize/htmlize.el")

(defun doexport ()
  (interactive)
  (org-html-export-to-html)
  (kill-emacs))
