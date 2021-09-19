;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  ;; Emacs 28+ automatically sets up these `bug-reference-mode' variables
  ;; in a more general way, so setting them here is not future-proof.  If
  ;; you still need these settings in older Emacs versions, you can add
  ;; them to your personal `.dir-locals-2.el' file in the meantime.
  ;; (bug-reference-bug-regexp . "\\(#\\([[:digit:]]+\\)\\)")
  ;; (bug-reference-url-format . "https://github.com/abo-abo/swiper/issues/%s")
  (copyright-names-regexp . "Free Software Foundation, Inc\\.")
  (sentence-end-double-space . t))
 (emacs-lisp-mode
  (indent-tabs-mode . nil)
  (outline-regexp . ";;\\([;*]+ [^\s\t\n]\\|###autoload\\)\\|(")
  ;; extra config here: https://github.com/abo-abo/oremacs/blob/github/modes/ora-elisp-style-guide.el
  ;; (lisp-indent-function . common-lisp-indent-function)
  ))
