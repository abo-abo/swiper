;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (bug-reference-bug-regexp . "#\\(?2:[[:digit:]]+\\)")
  (bug-reference-url-format . "https://github.com/abo-abo/swiper/issues/%s")
  (copyright-names-regexp . "Free Software Foundation, Inc\\.")
  (sentence-end-double-space . t))
 (emacs-lisp-mode
  (indent-tabs-mode . nil)
  (outline-regexp . ";;\\([;*]+ [^\s\t\n]\\|###autoload\\)\\|(")
  ;; extra config here: https://github.com/abo-abo/oremacs/blob/github/modes/ora-elisp-style-guide.el
  ;; (lisp-indent-function . common-lisp-indent-function)
  ))
