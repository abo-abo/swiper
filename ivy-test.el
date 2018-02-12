;;; ivy-test.el --- tests for ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This packages provides the tests for `ert'.  They can be executed
;; from the command line as well by calling "make test".

;;; Code:
(require 'ert)
(require 'colir)

;; useful for #'ivy-read-remap. It must arrive before (require 'ivy)
(define-key global-map (kbd "<S-right>") #'end-of-buffer)

(require 'ivy)
(require 'counsel)

(defvar ivy-expr nil
  "Holds a test expression to evaluate with `ivy-eval'.")

(defvar ivy-result nil
  "Holds the eval result of `ivy-expr' by `ivy-eval'.")

(defun ivy-eval ()
  "Evaluate `ivy-expr'."
  (interactive)
  (setq ivy-result (eval ivy-expr)))

(global-set-key (kbd "C-c e") 'ivy-eval)

(defun ivy-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((ivy-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c e")
              (kbd keys)))
    ivy-result))

(defun command-execute-setting-this-command (cmd &rest args)
  "Like `command-execute' but sets `this-command' first."
  (setq this-command cmd)
  (apply #'command-execute cmd args))

(defadvice symbol-function (around no-void-function activate)
  "Suppress void-function errors.

This advice makes `symbol-function' return nil when called on a
symbol with no function rather than throwing a void-function
error. On Emacs 24.4 and above, this has no effect, because
`symbol-function' already does this, but on 24.3 and earlier, it
will bring the behavior in line with the newer Emacsen."
  (condition-case nil
      ad-do-it
    (void-function nil)))

(ert-deftest ivy-partial-1 ()
  (should (equal
           (ivy-with '(ivy-read "test: " '("case" "Case"))
                     "ca TAB C-m")
           "case"))
  (should (equal
           (ivy-with '(ivy-read "test: " '("case" "Case"))
                     "Ca TAB C-m")
           "Case")))

(ert-deftest ivy-read ()
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow"))
                     "C-m")
           "blue"))
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow"))
                     "y C-m")
           "yellow"))
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow"))
                     "y DEL b C-m")
           "blue"))
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow"))
                     "z C-m")
           "z"))
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow"))
                     "y <backspace> C-m")
           "blue"))
  (should (equal
           (ivy-with '(let ((ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
                       (ivy-read "pattern: " '("package-list-packages" "something-else")))
                     "plp C-m")
           "package-list-packages"))
  (should (equal
           (ivy-with '(ivy-read "test" '("aaab" "aaac"))
                     "a C-n <tab> C-m")
           "aaac"))
  (should (equal
           (ivy-with '(ivy-read "test" '(("foo" . "bar")))
                     "asdf C-m")
           "asdf"))
  (should (equal
           (ivy-with
            '(with-output-to-string
              (ivy-read "test" '(("foo" . "bar"))
               :action (lambda (x) (prin1 x))))
            "f C-m")
           "(#(\"foo\" 0 1 (idx 0)) . \"bar\")"))
  (should (equal
           (ivy-with
            '(with-output-to-string
              (ivy-read "test" '(("foo" . "bar"))
               :action (lambda (x) (prin1 x))))
            "asdf C-m")
           "\"asdf\""))
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("can do" "can" "can't do"))
                     "can C-m")
           "can")))

(ert-deftest ivy-read-remap ()
  (should (equal
           (ivy-with '(ivy-read "pattern: " '("blue" "yellow" "red"))
                     "<S-right> C-m")
           "red")))

(ert-deftest swiper--re-builder ()
  (setq swiper--width 4)
  (should (string= (swiper--re-builder "^")
                   "."))
  (should (string= (swiper--re-builder "^a")
                   "^ ?\\(a\\)"))
  (should (string= (swiper--re-builder "^a b")
                   "^ \\(a\\).*?\\(b\\)")))

(ert-deftest ivy--split ()
  (should (equal (ivy--split "King of the who?")
                 '("King" "of" "the" "who?")))
  (should (equal (ivy--split "The  Britons.")
                 '("The Britons.")))
  (should (equal (ivy--split "Who  are the  Britons?")
                 '("Who are" "the Britons?")))
  (should (equal (ivy--split "We're  all  Britons and   I   am your   king.")
                 '("We're all Britons"
                   "and  I  am"
                   "your  king.")))
  (should (equal (ivy--split "^[^ ]") '("^[^ ]")))
  (should (equal (ivy--split "^[^ ] bar") '("^[^ ]" "bar"))))

(ert-deftest ivy--regex ()
  (should (equal (ivy--regex
                  "\\(?:interactive\\|swiper\\) \\(?:list\\|symbol\\)")
                 "\\(\\(?:interactive\\|swiper\\)\\).*?\\(\\(?:list\\|symbol\\)\\)")))

(ert-deftest ivy--split-negation ()
  (should (equal (ivy--split-negation "") ()))
  (should (equal (ivy--split-negation "not") '("not")))
  (should (equal (ivy--split-negation "!not") '("" "not")))
  (should (equal (ivy--split-negation "not!") '("not")))
  (should (equal (ivy--split-negation "!not!") '("" "not")))
  (should (equal (ivy--split-negation "not!not!not") '("not" "not")))
  (should (equal (ivy--split-negation "not!not\\!not") '("not" "not!not")))
  (should (equal (ivy--split-negation "\\!not!not\\!not") '("!not" "not!not")))
  (should (equal (ivy--split-negation "\\!not!notnot\\!") '("!not" "notnot!"))))

(ert-deftest ivy--split-spaces ()
  (should (equal (ivy--split-spaces "") ()))
  (should (equal (ivy--split-spaces " ") ()))
  (should (equal (ivy--split-spaces "  ") ()))

  (should (equal (ivy--split-spaces "a ") '("a")))
  (should (equal (ivy--split-spaces " a") '("a")))
  (should (equal (ivy--split-spaces " a ") '("a")))
  (should (equal (ivy--split-spaces "a  ") '("a")))
  (should (equal (ivy--split-spaces "  a") '("a")))
  (should (equal (ivy--split-spaces "  a  ") '("a")))

  (should (equal (ivy--split-spaces "\\ ") '(" ")))
  (should (equal (ivy--split-spaces "\\  ") '(" ")))
  (should (equal (ivy--split-spaces " \\ ") '(" ")))
  (should (equal (ivy--split-spaces "\\ \\ ") '("  ")))
  (should (equal (ivy--split-spaces "a\\ ") '("a ")))
  (should (equal (ivy--split-spaces "\\ a") '(" a")))
  (should (equal (ivy--split-spaces "\\ a\\ ") '(" a ")))

  (should (equal (ivy--split-spaces "a b") '("a" "b")))
  (should (equal (ivy--split-spaces "a\\ b") '("a b")))
  (should (equal (ivy--split-spaces " a b\\ ") '("a" "b ")))
  (should (equal (ivy--split-spaces "\\  a b ") '(" " "a" "b")))
  (should (equal (ivy--split-spaces " a\\  \\ b ") '("a " " b"))))

(ert-deftest ivy--regex-plus ()
  (should (equal (ivy--regex-plus "add path\\!") "\\(add\\).*?\\(path!\\)")))

(ert-deftest ivy-partial-2 ()
  (when (fboundp 'read--expression)
    (should
     (equal
      (ivy-with '(read--expression "Eval: "
                  "'s-c-t-st")
                "<tab> C-m")
      '(quote shell-command-to-string)))))

(ert-deftest ivy--regex-fuzzy ()
  (should (string= (ivy--regex-fuzzy "tmux")
                   "\\(t\\).*?\\(m\\).*?\\(u\\).*?\\(x\\)"))
  (should (string= (ivy--regex-fuzzy ".tmux")
                   "\\(\\.\\).*?\\(t\\).*?\\(m\\).*?\\(u\\).*?\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux")
                   "^\\(t\\).*?\\(m\\).*?\\(u\\).*?\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux$")
                   "^\\(t\\).*?\\(m\\).*?\\(u\\).*?\\(x\\)$"))
  (should (string= (ivy--regex-fuzzy "")
                   ""))
  (should (string= (ivy--regex-fuzzy "^")
                   "^"))
  (should (string= (ivy--regex-fuzzy "$")
                   "$")))

(ert-deftest ivy--regex-ignore-order ()
  (should (equal (ivy--regex-ignore-order "tmux")
                 '(("tmux" . t))))
  (should (equal (ivy--regex-ignore-order "^tmux")
                 '(("^tmux" . t))))
  (should (equal (ivy--regex-ignore-order "^tmux$")
                 '(("^tmux$" . t))))
  (should (equal (ivy--regex-ignore-order "")
                 ""))
  (should (equal (ivy--regex-ignore-order "^")
                 '(("^" . t))))
  (should (equal (ivy--regex-ignore-order "$")
                 '(("$" . t))))
  (should (equal (ivy--regex-ignore-order "one two")
                 '(("one" . t) ("two" . t))))
  (should (equal (ivy--regex-ignore-order "one two !three")
                 '(("one" . t) ("two" . t) ("three"))))
  (should (equal (ivy--regex-ignore-order "one two !three four")
                 '(("one" . t) ("two" . t) ("three") ("four"))))
  (should (equal (ivy--regex-ignore-order "!three four")
                 '(("three") ("four"))))
  ;; Support escaping ! and spaces.
  (should (equal (ivy--regex-ignore-order "one\\ two")
                 '(("one two" . t))))
  (should (equal (ivy--regex-ignore-order "one\\!two")
                 '(("one!two" . t))))
  ;; Don't crash on multiple !.
  (ivy--regex-ignore-order "! ! !")
  ;; Escape invalid regexps.
  (should (equal (ivy--regex-ignore-order "foo[ bar[xy]")
                 '(("foo\\[" . t) ("bar[xy]" . t)))))

(ert-deftest ivy--format ()
  (should (string= (let ((ivy--index 10)
                         (ivy-format-function (lambda (x) (mapconcat #'identity x "\n")))
                         (cands '("NAME"
                                  "SYNOPSIS"
                                  "DESCRIPTION"
                                  "FUNCTION LETTERS"
                                  "SWITCHES"
                                  "DIAGNOSTICS"
                                  "EXAMPLE 1"
                                  "EXAMPLE 2"
                                  "EXAMPLE 3"
                                  "SEE ALSO"
                                  "AUTHOR")))
                     (ivy--format cands))
                   #("\nDESCRIPTION\nFUNCTION LETTERS\nSWITCHES\nDIAGNOSTICS\nEXAMPLE 1\nEXAMPLE 2\nEXAMPLE 3\nSEE ALSO\nAUTHOR"
                     0 90 (read-only nil)
                     90 96 (face ivy-current-match read-only nil)))))

(ert-deftest ivy--filter ()
  (setq ivy-last (make-ivy-state))
  (should (equal (ivy--filter "the" '("foo" "the" "The"))
                 '("the" "The")))
  (should (equal (ivy--filter "The" '("foo" "the" "The"))
                 '("The"))))

(ert-deftest counsel-unquote-regex-parens ()
  (should (equal (counsel-unquote-regex-parens
                  (ivy--regex "foo bar"))
                 "(foo).*?(bar)"))
  (should (equal (counsel-unquote-regex-parens
                  (ivy--regex "(foo bar"))
                 "(\\(foo).*?(bar)")))

(defmacro ivy--string-buffer (text &rest body)
  "Test helper that wraps TEXT in a temp buffer while running BODY."
  `(with-temp-buffer
     (insert ,text)
     ,@body))

(ert-deftest counsel-url-expand ()
  "Test ffap expansion using `counsel-url-expansions-alist'."
  ;; no expansions defined
  (let (counsel-url-expansions-alist)
    (should (eq (counsel-url-expand) nil)))
  (let ((counsel-url-expansions-alist
         '(("^foo$" . "https://foo.com/%s")
           ("^issue\\([0-9]+\\)" . (lambda (word)
                                     (concat "https://foo.com/issues/"
                                             (match-string 1 word)))))))
    ;; no match
    (should (equal (ivy--string-buffer
                    "foobar"
                    (counsel-url-expand)) nil))
    ;; string expansion
    (should (equal (ivy--string-buffer
                    "foo"
                    (counsel-url-expand)) "https://foo.com/foo"))
    ;; function expansion
    (should (equal (ivy--string-buffer
                    "issue123"
                    (counsel-url-expand)) "https://foo.com/issues/123"))))

(ert-deftest colir-color-parse ()
  (should (equal (colir-color-parse "#ab1234")
                 ;; (color-name-to-rgb "#ab1234")
                 '(0.6705882352941176
                   0.07058823529411765
                   0.20392156862745098))))

(ert-deftest colir-blend-face-background ()
  ;; Note: should be `equal-including-properties', but it doesn't work as I like
  ;; `equal' doesn't test text properties
  (should (equal
           (let ((str #("One" 0 3 (face (:foreground "#badfad")))))
             (ivy--add-face str 'ivy-current-match)
             str)
           #("One" 0 3 (face (ivy-current-match :foreground "#badfad")))))
  (should (equal
           (let ((str #("Desktop" 0 7 (face ((foreground-color . "#badfad") bold)))))
             (colir-blend-face-background 0 (length str) 'ivy-current-match str)
             str)
           #("Desktop" 0 7 (face (ivy-current-match (foreground-color . "#8ac6f2") bold))))))


;;* prefix arg tests
;;** tests with no prefix
(ert-deftest ivy-no-prefix-arg ()
  "Tests with no prefix arg."
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-m")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-j")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-M-j")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-M-m")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-M-n")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-M-p")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "M-o o")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "TAB TAB")
           nil)))

;;** tests with one prefix
(ert-deftest ivy-one-prefix-arg ()
  "Tests with no prefix arg."
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-m")
           '(4)))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-j")
           '(4)))
  ;; C-M-j does not pass a prefix on.
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-M-j")
           nil))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-M-m")
           '(4)))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-M-n")
           '(4)))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u C-M-p")
           '(4)))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u M-o o")
           '(4)))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action
               '(1 ("o" (lambda (x)
                          (setq res ivy-current-prefix-arg)))
                 ("p" (lambda (x)
                        (setq res ivy-current-prefix-arg)))))
              res)
            "C-u M-o p")
           '(4)))
  ;; TAB TAB does not pass prefix arg
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "TAB TAB")
           nil)))


(ert-deftest ivy-numeric-prefix-arg ()
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "M-1 M-2 M-3 C-m")
           123))
  (should (equal
           (ivy-with
            '(let (res)
              (ivy-read "pattern: " '("blue" "yellow")
               :action (lambda (x)
                         (setq res ivy-current-prefix-arg)))
              res)
            "C-u 123 C-m")
           123)))

(ert-deftest ivy-re-match ()
  (should (ivy-re-match '(("counsel" . t)) "(defun counsel"))
  (should (ivy-re-match '(("defun" . t) ("counsel" . t)) "(defun counsel"))
  (should (ivy-re-match '(("counsel" . t) ("defun" . t)) "(defun counsel"))
  (should (not (ivy-re-match '(("counsel" . nil) ("defun" . t)) "(defun counsel")))
  (should (not (ivy-re-match '(("defun" . t) ("counsel" . nil)) "(defun counsel"))))

(ert-deftest ivy-read-preselect ()
  (should (equal
           (ivy-with
            '(ivy-read "test: "
              (list "abc" "default" "def")
              :preselect 1)
            "RET")
           "default"))
  (should (equal
           (ivy-with
            '(ivy-read "test: "
              (list "abc" "default" "def")
              :preselect "defa")
            "RET")
           "default")))

(ert-deftest ivy-read-prompt ()
  (setq prompt "pattern: ")
  (setq collection '("blue" "yellow"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-m")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-p C-m")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-j")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-p C-j")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-M-j")
           "bl"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt nil))
              (ivy-read prompt collection))
            "bl C-p C-M-j")
           "bl"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-m")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-p C-m")
           "bl"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-j")
           "blue"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-p C-j")
           "bl"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-M-j")
           "bl"))
  (should (equal
           (ivy-with
            '(let ((ivy-use-selectable-prompt t))
              (ivy-read prompt collection))
            "bl C-p C-M-j")
           "bl")))

(defmacro ivy-with-r (expr &rest keys)
  `(with-output-to-string
     (save-window-excursion
       (switch-to-buffer standard-output t)
       ,expr
       (ivy-mode)
       (execute-kbd-macro
        ,(apply #'vconcat (mapcar #'kbd keys))))))

(ert-deftest ivy-completion-in-region ()
  (should (string=
           (ivy-with-r
            (progn
              (emacs-lisp-mode)
              (insert " emacs-lisp-mode-h"))
            "C-M-i")
           " emacs-lisp-mode-hook"))
  (should (string=
           (ivy-with-r
            (progn
              (emacs-lisp-mode)
              (insert "(nconc"))
            "C-M-i")
           "(nconc")))

(ert-deftest ivy-completing-read-def-handling ()
  ;; DEF in COLLECTION
  (should
   (equal "b"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil "b")
                    "RET")))
  ;; Also make sure that choosing a non-default item works
  (should
   (equal "c"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil "b")
                    "c RET")))
  ;; DEF not in COLLECTION
  (should
   (equal "d"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil "d")
                    "RET")))
  (should
   (equal "c"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil "d")
                    "c RET")))
  ;; DEF list, some in COLLECTION
  (should
   (equal "e"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil '("e" "b"))
                    "RET")))
  (should
   (equal "c"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil '("e" "b"))
                    "c RET")))
  ;; DEF nil
  (should
   (equal "a"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil nil)
                    "RET")))
  (should
   (equal "c"
          (ivy-with '(ivy-completing-read "Pick: " '("a" "b" "c") nil t nil nil nil)
                    "c RET")))
  ;; DEF nil, and called via `ivy-completing-read-with-empty-string-def'
  (should
   (equal ""
          (ivy-with '(ivy-completing-read-with-empty-string-def
                      "Pick: " '("a" "b" "c") nil t nil nil nil)
                    "RET")))
  (should
   (equal "c"
          (ivy-with '(ivy-completing-read-with-empty-string-def
                      "Pick: " '("a" "b" "c") nil t nil nil nil)
                    "c RET"))))

(ert-deftest ivy-completing-read-handlers ()
  (cl-letf* ((ivy-mode-reset-arg (if ivy-mode 1 0))
             ;; Let-bind this so changes are reset after test
             (ivy-completing-read-handlers-alist
              '((test-command-default-handler . completing-read-default)
                (test-command-recursive-handler . ivy-completing-read-with-empty-string-def)))
             ;; Temporarily define several identical commands
             ((symbol-function 'test-command-no-handler)
              (lambda (arg)
                "Read and arg and return it"
                (interactive
                 (list
                  (completing-read "Pick: " '("a" "b" "c") nil t nil nil nil)))
                arg))
             ((symbol-function 'test-command-default-handler)
              (symbol-function 'test-command-no-handler))
             ((symbol-function 'test-command-recursive-handler)
              (symbol-function 'test-command-no-handler)))
    (unwind-protect
         (progn
           ;; Activate ivy-mode
           (ivy-mode 1)
           ;; No handler
           (should
            (equal "a"
                   (ivy-with
                    '(command-execute-setting-this-command
                      'test-command-no-handler)
                    "RET")))
           (should
            (equal "c"
                   (ivy-with
                    '(command-execute-setting-this-command
                      'test-command-no-handler)
                    "c RET")))
           ;; Handler = `completing-read-default'; make sure ivy-read
           ;; is never called
           (cl-letf (((symbol-function 'ivy-read)
                      (lambda (&rest args) (error "`ivy-read' should not be called"))))

             (should
              (equal ""
                     (ivy-with
                      '(command-execute-setting-this-command
                        'test-command-default-handler)
                      "RET")))
             (should
              (equal "c"
                     (ivy-with
                      '(command-execute-setting-this-command
                        'test-command-default-handler)
                      "c RET"))))
           ;; Handler = `ivy-completing-read-with-empty-string-def';
           ;; make sure infinite recursion does not occur
           (should
            (equal ""
                   (ivy-with
                    '(command-execute-setting-this-command
                      'test-command-recursive-handler)
                    "RET")))
           (should
            (equal "c"
                   (ivy-with
                    '(command-execute-setting-this-command
                      'test-command-recursive-handler)
                    "c RET"))))
      (ivy-mode ivy-mode-reset-arg))))

(ert-deftest ivy-completion-common-length ()
  (should (= 2
             (ivy-completion-common-length
              #("test/"
                0 2 (face completions-common-part)
                2 3 (face (completions-first-difference))))))
  (should (= 5
             (ivy-completion-common-length
              #("Math/E"
                0 5 (face (completions-common-part))
                5 6 (face (completions-first-difference))))))
  (should (= 3
             (ivy-completion-common-length
              #("vec"
                0 3 (face (completions-common-part)))))))

(provide 'ivy-test)

;;; ivy-test.el ends here
