;; Copyright (C) 2019-2024 Free Software Foundation, Inc.
(setq package-user-dir
      (expand-file-name
       (format "~/.elpa/%s/elpa"
               (concat emacs-version (when (getenv "MELPA_STABLE") "-stable")))))
(package-initialize)
