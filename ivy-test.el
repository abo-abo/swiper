;;; ivy-test.el --- tests for ivy

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

(require 'ert)

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
  (should (equal (ivy--split "The  Brittons.")
                 '("The Brittons.")))
  (should (equal (ivy--split "Who  are the  Brittons?")
                 '("Who are" "the Brittons?")))
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

(ert-deftest ivy--regex-fuzzy ()
  (should (string= (ivy--regex-fuzzy "tmux")
                   "\\(t\\).*?\\(m\\).*?\\(u\\).*?\\(x\\)"))
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
                 '(("" . t) (("three") ("four")))))
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

(ert-deftest colir-color-parse ()
  (should (equal (colir-color-parse "#ab1234")
                 ;; (color-name-to-rgb "#ab1234")
                 '(0.6705882352941176
                   0.07058823529411765
                   0.20392156862745098))))


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

(provide 'ivy-test)
