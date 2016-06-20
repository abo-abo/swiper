(add-to-list 'load-path default-directory)
(require 'counsel)
(setq counsel-prompt-function 'counsel-prompt-function-default)
(byte-compile-file (expand-file-name "targets/obsolete-config.el"))
