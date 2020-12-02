;; Copyright (C) 2020  Free Software Foundation, Inc.
(setq org-confirm-babel-evaluate nil)
(defun org-to-texi (fname)
  (find-file fname)
  (org-texinfo-export-to-texinfo))
