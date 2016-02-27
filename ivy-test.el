;;; ivy-test.el --- tests for ivy

;; Copyright (C) 2015  Free Software Foundation, Inc.

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
           (ivy-with '(ivy-read "pattern: " '("can do" "can" "can't do"))
                     "can C-m")
           "can")))

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
                   "your  king."))))

(ert-deftest ivy--regex ()
  (should (equal (ivy--regex
                  "\\(?:interactive\\|swiper\\) \\(?:list\\|symbol\\)")
                 "\\(\\(?:interactive\\|swiper\\)\\).*?\\(\\(?:list\\|symbol\\)\\)")))

(ert-deftest ivy--regex-fuzzy ()
  (should (string= (ivy--regex-fuzzy "tmux")
                   "\\(t\\).*\\(m\\).*\\(u\\).*\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux")
                   "^\\(t\\).*\\(m\\).*\\(u\\).*\\(x\\)"))
  (should (string= (ivy--regex-fuzzy "^tmux$")
                   "^\\(t\\).*\\(m\\).*\\(u\\).*\\(x\\)$"))
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
                 '(("" . t) (("three") ("four"))))))

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
                 "(foo).*?(bar)")))
