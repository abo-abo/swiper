;;* Includes
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/org-mode/lisp")
(require 'org)
(load-file "~/Dropbox/source/site-lisp/git/eclipse-theme/eclipse-theme.el")
(load-file "~/Dropbox/source/site-lisp/git/htmlize/htmlize.el")

;;* ox-html
(require 'ox-html)
(setq org-html-validation-link nil)
(setq org-html-postamble nil)
(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<kbd>%s</kbd>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<code>%s</code>")))
(setq org-html-style-default nil)

(defun doexport ()
  (interactive)
  (org-html-export-to-html)
  (kill-emacs))
