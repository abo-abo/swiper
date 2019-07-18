;;; ivy-test.el --- tests for ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Free Software Foundation, Inc.

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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages provides the tests for `ert'.  They can be executed
;; from the command line as well by calling "make test".

;;; Code:

(require 'ert)
(require 'colir)

;; Useful for #'ivy-read-remap.  It must arrive before (require 'ivy).
(define-key global-map (kbd "<S-right>") #'end-of-buffer)

(require 'ivy)
(require 'counsel)

(message "%s" (emacs-version))

(defvar ivy-expr nil
  "Holds a test expression to evaluate with `ivy-eval'.")

(defvar ivy-result nil
  "Holds the eval result of `ivy-expr' by `ivy-eval'.")

(defun ivy-eval ()
  "Evaluate `ivy-expr'."
  (interactive)
  (setq ivy-result (eval ivy-expr)))

(global-set-key (kbd "C-c e") 'ivy-eval)

(cl-defun ivy-with (expr keys &key dir)
  "Evaluate EXPR followed by KEYS."
  (let ((ivy-expr expr)
        (inhibit-message t)
        (buf (current-buffer)))
    (save-window-excursion
      (unwind-protect
           (progn
             ;; `execute-kbd-macro' doesn't pick up `default-directory'
             (when dir
               (dired (expand-file-name dir (counsel-locate-git-root))))
             (execute-kbd-macro
              (vconcat (kbd "C-c e")
                       (kbd keys))))
        (switch-to-buffer buf)))
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
  (should (equal-including-properties
           (ivy-with '(ivy-read "test" '(("foo" . "bar")))
                     "C-m")
           "foo"))
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
           "(\"foo\" . \"bar\")"))
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
           "can"))
  (should (equal
           (ivy-with '(ivy-read "pattern: "
                       '("ignore" "build" "build-1" "build-2") :preselect "build")
                     "b C-m")
           "build"))
  (should (equal (ivy-with
                  '(ivy-read "x: " '("one" "two" ("three" . "four")))
                  "th C-m")
                 "three")))

(ert-deftest ivy-read-sort-alist ()
  (should (equal (ivy-with '(let ((coll '(("b" . "1") ("a" . "2"))))
                             (ivy-read "test:" coll
                              :sort t)
                             coll)
                           "C-m")
                 '(("b" . "1") ("a" . "2")))))

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

(ert-deftest swiper--re-builder-char-fold ()
  :expected-result (if (>= emacs-major-version 25)
                       :passed
                     :failed)
  (let ((search-default-mode 'char-fold-to-regexp))
    (should (string= (swiper--re-builder "f b")
                     "\\(\\(?:ḟ\\|[fᶠḟⓕｆ𝐟𝑓𝒇𝒻𝓯𝔣𝕗𝖋𝖿𝗳𝘧𝙛𝚏]\\)\\).*?\\(\\(?:b[̣̱̇]\\|[bᵇḃḅḇⓑｂ𝐛𝑏𝒃𝒷𝓫𝔟𝕓𝖇𝖻𝗯𝘣𝙗𝚋]\\)\\)"))
    (should (= ivy--subexps 2))))

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
  (should (equal (ivy--split "^[^ ] bar") '("^[^ ]" "bar")))
  (should (equal (ivy--split "defun [^ ]+") '("defun" "[^ ]+"))))

(ert-deftest ivy--regex ()
  (should (equal (ivy--regex
                  "defun [^ ]+")
                 "\\(defun\\).*?\\([^ ]+\\)"))
  (should (equal (ivy--regex
                  "\\(?:interactive\\|swiper\\) \\(?:list\\|symbol\\)")
                 "\\(\\(?:interactive\\|swiper\\)\\).*?\\(\\(?:list\\|symbol\\)\\)"))
  (should (equal (ivy--regex
                  "foo[")
                 "foo\\["))
  (should (equal (ivy--regex
                  ".org")
                 "\\.org")))

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
  (should (equal (ivy--split-spaces " a\\  \\ b ") '("a " " b")))

  (should (equal (ivy--split-spaces "foo[") '("foo\\[")))
  (should (equal (ivy--split-spaces "foo[a]") '("foo[a]")))
  (should (equal (ivy--split-spaces "foo[ ]") '("foo\\[" "]"))))

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
                   "\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (string= (ivy--regex-fuzzy ".tmux")
                   "\\(\\.\\)[^t\n]*\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux")
                   "^\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux$")
                   "^\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)$"))
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
                         (ivy-format-functions-alist
                          '((t . (lambda (x) (mapconcat #'identity x "\n")))))
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

(ert-deftest counsel--elisp-to-pcre ()
  (should (equal (counsel--elisp-to-pcre
                  (ivy--regex "foo bar"))
                 "(foo).*?(bar)"))
  (should (equal (counsel--elisp-to-pcre
                  (ivy--regex "(foo bar)"))
                 "(\\(foo).*?(bar\\))"))
  (should (equal (counsel--elisp-to-pcre
                  (ivy--regex "{foo bar}"))
                 "({foo).*?(bar})"))
  (should (equal (counsel--elisp-to-pcre "\\{foo bar\\}")
                 "{foo bar}"))
  (should (equal (counsel--elisp-to-pcre "\\(foo\\|bar\\)\\|baz")
                 "(foo|bar)|baz"))
  (should (equal (counsel--elisp-to-pcre
                  '(("foo") ("bar" . t) ("baz" . t)))
                 "bar.*baz"))
  (should (equal (counsel--elisp-to-pcre
                  '(("foo\\|bar" . t) ("blah\\|bloop") ("blick" . t) ("\\(baz\\)\\|quux" . t)))
                 "(?:foo|bar).*blick.*(?:(baz)|quux)"))
  (should (equal (counsel--elisp-to-pcre
                  '(("ivy" . t) ("-")) t)
                 "^(?=.*ivy)(?!.*-)"))
  (should (equal (counsel--elisp-to-pcre
                  '(("foo" . t)) t)
                 "foo"))
  (should (equal (counsel--elisp-to-pcre
                  '(("foo")) t)
                 "^(?!.*foo)")))

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
       (let ((inhibit-message t))
         (execute-kbd-macro
          ,(apply #'vconcat (mapcar #'kbd keys)))))))

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
  ;; DEF list, empty input (no text collection), non-text default, same object
  (let ((def '([a b])))
    (should
     (eq (car def)
         (ivy-with
          (eval `'(ivy-completing-read "Pick: " nil nil 'require-match nil nil ',def))
          "RET"))))
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

(ert-deftest ivy--sort-function ()
  "Test `ivy--sort-function' behavior."
  ;; No enabled collections
  (dolist (alist '(() ((t)) ((t nil)) ((a)) ((a nil))))
    (let ((ivy-sort-functions-alist alist))
      (dolist (coll '(a b))
        (should (not (ivy--sort-function coll))))))
  (dolist (fn (list #'identity (lambda ()) '(lambda ())))
    ;; No fallback
    (dolist (alist `(((a . ,fn))
                     ((a ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (should (eq (ivy--sort-function 'a) fn))
        (should (not (ivy--sort-function 'b)))))
    ;; Only fallback
    (dolist (alist `(((t . ,fn))
                     ((t ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (dolist (coll '(a b))
          (should (eq (ivy--sort-function coll) fn)))))
    ;; Fallback with disabled collection
    (dolist (alist `(((a) (t . ,fn))
                     ((a) (t ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (should (not (ivy--sort-function 'a)))
        (should (eq (ivy--sort-function 'b) fn)))))
  ;; Fallback with enabled collection
  (let* ((fn0 #'identity)
         (fn1 (lambda ()))
         (ivy-sort-functions-alist `((a ,fn0) (b) (t ,fn1))))
    (should (eq (ivy--sort-function 'a) fn0))
    (should (not (ivy--sort-function 'b)))
    (should (eq (ivy--sort-function 'c) fn1))))

(ert-deftest ivy-read-directory-name ()
  (should
   (equal "/tmp/"
          (ivy-with
           '(read-directory-name "cd: " "/tmp")
           "RET"))))

(ert-deftest ivy-partial-files ()
  (when (file-exists-p "/tmp/ivy-partial-test")
    (delete-directory "/tmp/ivy-partial-test" t))
  (mkdir "/tmp/ivy-partial-test/test1" t)
  (mkdir "/tmp/ivy-partial-test/test2")
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)
  (should
   (equal
    (save-window-excursion
      (condition-case nil
          (ivy-with
           '(let ((default-directory "/tmp/ivy-partial-test/"))
             (counsel-find-file))
           "t TAB TAB TAB C-g")
        (quit ivy--old-cands)))
    '("test1/" "test2/")))
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)
  (delete-directory "/tmp/ivy-partial-test" t))

(defun ivy-with-temp-buffer (expr keys)
  (let ((temp-buffer (generate-new-buffer " *temp*")))
    (save-window-excursion
      (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (ivy-with expr keys)
             (list (point)
                   (buffer-string)))
        (and (buffer-name temp-buffer)
             (kill-buffer temp-buffer))))))

(ert-deftest counsel-yank-pop ()
  (let ((kill-ring '("foo")))
    (should (equal
             (ivy-with-temp-buffer '(counsel-yank-pop) "C-m")
             '(4 "foo")))
    (let ((counsel-yank-pop-after-point t))
      (should (equal
               (ivy-with-temp-buffer '(counsel-yank-pop) "C-m")
               '(1 "foo"))))))

(ert-deftest ivy-read-file-name-in-buffer-visiting-file ()
  "Test `ivy-immediate-done' command in `read-file-name' without any editing in
a buffer visiting a file."
  (let ((ivy-mode-reset-arg (if ivy-mode 1 0)))
    (ivy-mode 1)
    ;; `ivy-read' returns "~/dummy-dir/dummy-file" (same object, not a copy).
    ;;
    ;; `read-file-name-default' will then return "" in order for
    ;; `set-visited-file-name' to detect that the user typed RET with
    ;; the minibuffer empty.
    (should
     (equal (ivy-with
             '(let ((insert-default-directory t))
               (with-temp-buffer
                 (set-visited-file-name "~/dummy-dir/dummy-file")
                 (read-file-name "Load file: " nil nil 'lambda)))
             ;; No editing, just command ivy-immediate-done
             "C-M-j")
            ""))
    (should
     (equal (ivy-state-current ivy-last) "~/dummy-dir/dummy-file"))
    (ivy-mode ivy-mode-reset-arg)))

(ert-deftest ivy-starts-with-dotslash ()
  (should (ivy--starts-with-dotslash "./test1"))
  (should (ivy--starts-with-dotslash ".\\test2"))
  (should (not (ivy--starts-with-dotslash "test3")))
  (should (not (ivy--starts-with-dotslash "t/est4")))
  (should (not (ivy--starts-with-dotslash "t\\est5")))
  (should (not (ivy--starts-with-dotslash "tes./t6"))))

(ert-deftest counsel--normalize-grep-match ()
  (with-temp-buffer
    (let ((match-data-orig
           (progn
             (insert "abcd\nefgh")
             (goto-char (point-min))
             (re-search-forward "\\(ab\\)\\(cd\\)")
             (match-data)))
          input expected out)
      (dolist (test '(("./FILENAME:1234:32:  TEXT   MORETEXT" .
                       "./FILENAME:1234:  TEXT   MORETEXT")
                      ("FILENAME:1234:  TEXT   MORETEXT" .
                       "./FILENAME:1234:  TEXT   MORETEXT")
                      ))
        (setq input (car test))
        (setq expected (cdr test))
        (setq out (counsel--normalize-grep-match input))
        (should (equal out expected))
        (should (equal match-data-orig (match-data)))
        (setq out (counsel--normalize-grep-match out))
        (should (equal out expected))
        (should (equal match-data-orig (match-data)))))))

(ert-deftest counsel--grep-regex ()
  ;; negative lookahead: lines with "ivy", without "-"
  (should
   (string=
    (let ((counsel--regex-look-around t)
          (ivy--regex-function 'ivy--regex-plus))
      (counsel--grep-regex "ivy ! -"))
    "^(?=.*ivy)(?!.*-)"))
  (should
   (string=
    (let ((counsel--regex-look-around t)
          (ivy--regex-function 'ivy--regex-fuzzy))
      (counsel--grep-regex "ivy"))
    "(i)[^v\n]*(v)[^y\n]*(y)")))

(defmacro ivy-with-text (text &rest body)
  (let ((old-bindings
         (delq nil (mapcar
                    (lambda (x)
                      (when (and (listp x)
                                 (eq (car x) 'global-set-key))
                        (let ((key (eval (cadr x))))
                          (list key (lookup-key global-map key)))))
                    body))))
    `(let ((temp-buffer (generate-new-buffer " *temp*")))
       (save-window-excursion
         (unwind-protect
              (progn
                (switch-to-buffer temp-buffer)
                (insert ,text)
                (search-backward "|")
                (delete-char 1)
                (setq current-prefix-arg nil)
                (let ((inhibit-message t))
                  ,@(mapcar (lambda (x)
                              (if (and (listp x)
                                       (stringp (car x)))
                                  `(execute-kbd-macro
                                    (vconcat ,@(mapcar #'kbd x)))
                                x))
                            body))
                (insert "|")
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))
           (dolist (old-binding ',old-bindings)
             (apply #'global-set-key old-binding))
           (and (buffer-name temp-buffer)
                (kill-buffer temp-buffer)))))))

(ert-deftest swiper-isearch ()
  (should
   (string=
    (ivy-with-text
     "abc\na|sdf123 def\ndem"
     (global-set-key (kbd "C-s") #'isearch-forward-regexp)
     ("C-s" "de" "" "RET"))
    "abc\nasd|f123 def\ndem"))
  (should
   (string=
    (ivy-with-text
     "abc\na|sdf123 def\ndem"
     (global-set-key (kbd "C-s") #'swiper-isearch)
     ("C-s" "de" "" "RET"))
    "abc\nasd|f123 def\ndem"))
  (should
   (string=
    (ivy-with-text
     "|(defun foo)\nasdf\n(defvar bar)"
     (global-set-key (kbd "C-s") #'isearch-forward-regexp)
     ("C-s" "defun\\|defvar" "RET"))
    "(defun| foo)\nasdf\n(defvar bar)"))
  (should
   (string=
    (ivy-with-text
     "|(defun foo)\nasdf\n(defvar bar)"
     (global-set-key (kbd "C-s") #'swiper-isearch)
     ("C-s" "defun\\|defvar" "RET"))
    "(defun| foo)\nasdf\n(defvar bar)"))
  (should
   (string=
    (ivy-with-text
     "|(defun foo)\nasdf\n(defvar bar)"
     (global-set-key (kbd "C-s") #'swiper-isearch)
     ("C-s" "defun\\|defvar" "C-n RET"))
    "(defun foo)\nasdf\n(defvar| bar)")))

(ert-deftest swiper-isearch-backward ()
  (should
   (string=
    (ivy-with-text
     "abc\nasdf123 def\ndem|"
     (global-set-key (kbd "C-r") #'isearch-backward-regexp)
     ("C-r" "de" "" "RET"))
    "abc\nasdf123 def\n|dem"))
  (should
   (string=
    (ivy-with-text
     "abc\nasdf123 def\ndem|"
     (global-set-key (kbd "C-r") #'swiper-isearch-backward)
     ("C-r" "de" "" "RET"))
    "abc\nasdf123 def\n|dem"))
  (should
   (string=
    (ivy-with-text
     "(defun foo)\nasdf\n(defvar bar)|"
     (global-set-key (kbd "C-r") #'isearch-backward-regexp)
     ("C-r" "defun\\|defvar" "RET"))
    "(defun foo)\nasdf\n(|defvar bar)"))
  ;; NOTE: The following two behaviors do not match
  ;; `isearch-backward-regexp', but they match that of
  ;; `swiper-isearch-forward', as `swiper-isearch' does not reset the
  ;; point when the regexp becomes invalid, meaning the point is left
  ;; at the initial match of the first part of the regexp.
  (should
   (string=
    (ivy-with-text
     "(defun foo)\nasdf\n(defvar bar)|"
     (global-set-key (kbd "C-r") #'swiper-isearch-backward)
     ("C-r" "defun\\|defvar" "RET"))
    "(|defun foo)\nasdf\n(defvar bar)"))
  (should
   (string=
    (ivy-with-text
     "(defun foo)\nasdf\n(defvar bar)|"
     (global-set-key (kbd "C-r") #'swiper-isearch-backward)
     ("C-r" "defun\\|defvar" "C-n RET"))
    "(defun foo)\nasdf\n(|defvar bar)"))
  (should
   (string=
    (ivy-with-text
     "(defun foo)\nasdf\n(|defun bar)"
     (global-set-key (kbd "C-r") #'isearch-backward)
     ("C-r" "defun" "RET"))
    "(|defun foo)\nasdf\n(defun bar)"))
  (should
   (string=
    (ivy-with-text
     "(defun foo)\nasdf\n(|defun bar)"
     (global-set-key (kbd "C-r") #'swiper-isearch-backward)
     ("C-r" "defun" "RET"))
    "(|defun foo)\nasdf\n(defun bar)")))

(ert-deftest swiper-isearch-backward-backspace ()
  (should
   (string=
    (ivy-with-text
     "(while (when |))"
     (global-set-key (kbd "C-r") #'swiper-isearch-backward)
     ("C-r" "whi" "" "RET"))
    "(while (|when ))"))
  (should
   (string=
    (ivy-with-text
     "(while (when |))"
     (global-set-key (kbd "C-r") #'isearch-backward-regexp)
     ("C-r" "whi" "" "RET"))
    "(while (|when ))")))

(ert-deftest swiper-isearch-case-fold ()
  (should
   (string=
    (ivy-with-text
     "|Foo\nfoo\nFOO\n"
     (global-set-key (kbd "C-s") #'swiper-isearch)
     ("C-s" "foo" "C-n RET"))
    "Foo\nfoo|\nFOO\n"))
  (should
   (string=
    (let ((ivy-case-fold-search-default 'auto))
      (ivy-with-text
       "|Foo\nfoo\nFOO\n"
       (global-set-key (kbd "C-s") #'swiper-isearch)
       ("C-s" "Foo" "C-n RET")))
    "Foo|\nfoo\nFOO\n"))
  (should
   (string=
    (let ((ivy-case-fold-search-default t))
      (ivy-with-text
       "|Foo\nfoo\nFOO\n"
       (global-set-key (kbd "C-s") #'swiper-isearch)
       ("C-s" "Foo" "C-n RET")))
    "Foo\nfoo|\nFOO\n")))

(ert-deftest swiper--isearch-format ()
  (setq swiper--isearch-point-history
        (list
         (cons "" 1)))
  (with-temp-buffer
    (insert
     "line0\nline1\nline line\nline line\nline5")
    (let* ((input "li")
           (cands (swiper--isearch-function input))
           (len (length cands)))
      (should (equal cands '(3 9 15 20 25 30 35)))
      (dotimes (index len)
        (should (string= (substring-no-properties
                          (swiper--isearch-format
                           index len
                           cands
                           input
                           (nth index cands)
                           (current-buffer)))
                         "line0\nline1\nline line\nline line\nline5"))))))

(ert-deftest ivy-use-selectable-prompt ()
  (let ((ivy-use-selectable-prompt t)
        (completing-read-function #'ivy-completing-read))
    (should (string= (ivy-with '(ivy-read "prompt: " '("foo" "bar")
                                 :require-match t)
                               "C-p C-m")
                     "foo"))
    (should (string= (ivy-with '(ivy-read "prompt: " '("" "foo" "bar")
                                 :require-match t)
                               "C-p C-m")
                     ""))
    (should (string= (ivy-with '(completing-read "Position: " '(("") ("t") ("b")) nil t)
                               "C-p C-m")
                     ""))))

(ert-deftest ivy--minibuffer-index-bounds ()
  (should (equal (ivy--minibuffer-index-bounds 0 1 10) '(0 1 0)))
  (should (equal (ivy--minibuffer-index-bounds 0 10 10) '(0 9 0)))
  (should (equal (ivy--minibuffer-index-bounds 0 11 10) '(0 9 0)))
  (should (equal (ivy--minibuffer-index-bounds 1 11 10) '(0 9 1)))
  (should (equal (ivy--minibuffer-index-bounds 5 11 10) '(0 9 5)))
  (should (equal (ivy--minibuffer-index-bounds 6 11 10) '(1 10 5)))
  (should (equal (ivy--minibuffer-index-bounds 7 11 10) '(2 11 5)))
  (should (equal (ivy--minibuffer-index-bounds 8 11 10) '(2 11 6)))
  (should (equal (ivy--minibuffer-index-bounds 10 11 10) '(2 11 8)))
  (should (equal (ivy--minibuffer-index-bounds 1 3 10) '(0 3 1))))

(defun counsel--setup-test-files ()
  (unless (file-exists-p "tests/")
    (shell-command
     "git clone -b test --single-branch https://github.com/abo-abo/swiper/ tests"))
  (let ((default-directory (expand-file-name "tests/"))
        (version "066ec1d"))
    (shell-command
     (format "git checkout %s || git fetch && git checkout %s" version version))))

(ert-deftest counsel-find-file-with-dollars ()
  (counsel--setup-test-files)
  (should (string=
           (file-relative-name
            (ivy-with '(counsel-find-file) "fo C-m"
                      :dir "tests/find-file/files-with-dollar/"))
           "tests/find-file/files-with-dollar/foo$")))

(ert-deftest counsel-find-file-with-dotfiles ()
  (counsel--setup-test-files)
  (should (string=
           (file-relative-name
            (ivy-with '(counsel-find-file) "f C-m"
                      :dir "tests/find-file/dotfiles/"))
           "tests/find-file/dotfiles/foo/"))
  (should (string=
           (file-relative-name
            (ivy-with '(counsel-find-file) "foob C-m"
                      :dir "tests/find-file/dotfiles/"))
           "tests/find-file/dotfiles/.foobar1")))

(ert-deftest counsel-find-file-with-spaces ()
  (counsel--setup-test-files)
  (let ((ivy-extra-directories nil))
    (should (string=
             (file-relative-name
              (ivy-with '(counsel-find-file) "TAB TAB TAB TAB"
                        :dir "tests/find-file/directories-with-spaces/"))
             "tests/find-file/directories-with-spaces/bar baz i/file1"))
    (should (string=
             (file-relative-name
              (ivy-with '(counsel-find-file) "C-n TAB TAB TAB TAB"
                        :dir "tests/find-file/directories-with-spaces/"))
             "tests/find-file/directories-with-spaces/bar baz ii/file2"))
    (should (string=
             (file-relative-name
              (ivy-with '(counsel-find-file) "TAB C-n TAB TAB TAB TAB"
                        :dir "tests/find-file/directories-with-spaces/"))
             "tests/find-file/directories-with-spaces/bar baz ii/file2"))))

(ert-deftest ivy-avy ()
  (when (require 'avy nil t)
    (let ((enable-recursive-minibuffers t)
          (read-numbers '(ivy-read "test: " (mapcar #'number-to-string (number-sequence 1 100)))))
      (should (string= (ivy-with read-numbers "C-' a") "1"))
      (should (string= (ivy-with read-numbers "C-v C-' d") "7")))))

(provide 'ivy-test)

;;; ivy-test.el ends here
