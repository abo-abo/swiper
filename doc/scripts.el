;; Copyright (C) 2020-2021 Free Software Foundation, Inc.
(setq org-confirm-babel-evaluate nil)
(defun org-to-texi (fname)
  (find-file fname)
  (setq-default indent-tabs-mode nil)
  (org-texinfo-export-to-texinfo))
