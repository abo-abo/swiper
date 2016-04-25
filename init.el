(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/org-mode/lisp")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/swiper/doc")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/eclipse-theme")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/htmlize")
(require 'ivy-ox)
(require 'org)
(require 'eclipse-theme)
(require 'htmlize)

(setq org-confirm-babel-evaluate nil)

(defun org-export-get-reference (_datum _info)
  "orgheadline")

(defun doexport ()
  (interactive)
  (org-html-export-to-html)
  (kill-emacs))

(defun add-hlines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\{3,\\}" nil t)
    (cond ((save-excursion
             (beginning-of-line 0)
             (looking-at "\\*\\{2,\\}")))
          ((looking-back "-----\n\\*+"))
          (t
           (end-of-line 0)
           (insert "\n-----")
           (forward-line 2)))))
