;; Copyright (C) 2016-2024 Free Software Foundation, Inc.
(add-to-list 'load-path (directory-file-name default-directory))
(require 'counsel)
(ivy-set-display-transformer 'ivy-switch-buffer #'ivy-switch-buffer-transformer)
(byte-compile-file (expand-file-name "targets/obsolete-config.el"))
