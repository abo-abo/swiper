(add-to-list 'load-path default-directory)
(require 'counsel)
(ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer-transformer)
(byte-compile-file (expand-file-name "targets/obsolete-config.el"))
