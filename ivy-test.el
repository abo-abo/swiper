;;; ivy-test.el --- tests for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>

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

;; This package provides ERT tests for Ivy.  They can be executed
;; from the command line as well as by calling "make test".

;;; Code:

;; Optional dependencies.
(and (require 'targets/elpa nil t)
     (fboundp 'ivy--elpa-activate)
     (ivy--elpa-activate))

(require 'colir)
(require 'counsel)
(require 'ivy)
(require 'swiper)

(require 'cl-lib)
(require 'ert)
(eval-when-compile
  (require 'ert-x)
  (require 'subr-x))

(defvar ivy-test-history ()
  "Custom minibuffer history list for Ivy tests.")

(defvar ivy-test-inhibit-message t
  "Value of `inhibit-message' during Ivy tests.
Set to nil for more verbose logging.")

(defconst ivy-test-exec-key (vector (make-symbol "ivy-test-exec-event"))
  "Key sequence to be bound to command under test.")

(defun ivy-test-exec (keymap fn input)
  "Apply KEYMAP; call FN; INPUT key sequence; return FN's result.
KEYMAP is a list of (KEY DEFINITION), as in `keymap-set', that override
corresponding global bindings while processing FN and INPUT.
There are two ways for FN to incorporate keyboard input: either by
executing keyboard macros directly, or by receiving a key sequence as
INPUT.  Use the latter when FN reads input, e.g., via `ivy-read'."
  (with-temp-buffer
    ;; Seemingly needed for commands to act on the right buffer.
    (set-window-buffer nil (current-buffer))
    (let (res)
      ;; Binding this to a custom event which executes synchronously with
      ;; INPUT seems a simple solution for both non/interactive runs.
      (cl-flet ((cmd () (interactive) (setq res (funcall fn))))
        (let* ((exec ivy-test-exec-key)
               (omap (current-global-map))
               (map (let ((map (make-sparse-keymap)))
                      ;; Avoid modifying global map directly.
                      (set-keymap-parent map omap)
                      (dolist (k keymap)
                        (define-key map (kbd (nth 0 k)) (nth 1 k)))
                      (define-key map exec #'cmd)
                      map)))
          ;; New in Emacs 25.
          (defvar inhibit-message)
          (unwind-protect
              (let ((inhibit-message ivy-test-inhibit-message))
                (use-global-map map)
                (execute-kbd-macro (vconcat exec (kbd input))))
            (use-global-map omap))))
      res)))

(cl-defmacro ivy-test-with (keymap (&rest body) input)
  "Like `ivy-test-exec', but with a nested BODY in place of a function."
  (declare (debug (form (def-body) form)) (indent defun))
  `(ivy-test-exec ,keymap (lambda () ,@body) ,input))

(cl-defmacro ivy-test-with-text (keymap text (&rest body) &optional input)
  "Like `ivy-test-with', but for buffer manipulation.
TEXT is a string to insert before evaluating BODY.
It must contain a \"|\" character indicating where to place point.
Return the resulting buffer contents as a string, again
with \"|\" indicating the final placement of point."
  (declare (debug (form form (body) &optional form)) (indent defun))
  `(ivy-test-with ,keymap
     ((insert ,text)
      (skip-chars-backward "^|")
      (delete-char -1)
      ,@body
      (insert ?|)
      (buffer-string))
     ,(or input "")))

(eval-and-compile
  (defalias 'ivy-test-with-tmpdir
    (if (fboundp 'ert-with-temp-directory)
        #'ert-with-temp-directory
      `(macro . ,(lambda (name &rest body)
                   `(let (,name)
                      (unwind-protect
                          (progn
                            (setq ,name (file-name-as-directory
                                         (make-temp-file "ivy-test-" t)))
                            ,@body)
                        (when ,name (delete-directory ,name t)))))))
    "Compatibility shim for Emacs 29 macro `ert-with-temp-directory'.
\n(fn NAME &rest BODY)")
  (def-edebug-spec ivy-test-with-tmpdir (symbolp body))
  (function-put #'ivy-test-with-tmpdir 'lisp-indent-function 1))

(defalias 'ivy-test-empty-file
  (if (fboundp 'make-empty-file)
      #'make-empty-file
    (lambda (filename &optional parents)
      (let ((parent (and parents (file-name-directory filename))))
        (when parent (make-directory parent parents)))
      (write-region "" nil filename nil 0 nil 'excl)))
  "Compatibility shim for Emacs 27 function `make-empty-file'.
\n(fn FILENAME &optional PARENTS)")

(defun ivy-test-command-execute (cmd &rest args)
  "Like `command-execute' but sets `this-command' first."
  (setq this-command cmd)
  (apply #'command-execute cmd args))

(defun ivy-test= (x y)
  "Like `=', but with relative tolerance between floating-point numbers.
From info node `(elisp) Comparison of Numbers'."
  (or (= x y)
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         1e-6)))

;;; Colir

(ert-deftest colir-color-parse ()
  (dolist (test '(("#000000" 0.0 0.0 0.0)
                  ("#ffffff" 1.0 1.0 1.0)
                  ("#ab1234"
                   ;; (color-name-to-rgb "#ab1234")
                   0.6705882352941176
                   0.07058823529411765
                   0.20392156862745098)))
    (let ((out (colir-color-parse (car test)))
          (expect (cdr test)))
      (should (consp out))
      (should (= (length out) (length expect)))
      (cl-mapc (lambda (x y) (should (ivy-test= x y)))
               out expect))))

(ert-deftest colir-blend-face-background ()
  ;; FIXME: Why does `face' differ in interactive sessions?
  (skip-unless noninteractive)
  (let ((pred (if (equal-including-properties
                   (propertize "a" 'face (list 'default))
                   (propertize "a" 'face (list 'default)))
                  ;; Since Emacs 29.
                  #'equal-including-properties
                #'equal)))
    (let ((old (propertize "One" 'face (list :foreground "#badfad")))
          (new #("One" 0 3 (face (ivy-current-match :foreground "#badfad")))))
      (ivy--add-face old 'ivy-current-match)
      (should (funcall pred old new)))
    (let* ((face (list (cons 'foreground-color "#badfad") 'bold))
           (old (propertize "Desktop" 'face face))
           (new #("Desktop" 0 7 (face (ivy-current-match
                                       (foreground-color . "#badfad")
                                       bold)))))
      (colir-blend-face-background 0 (length old) 'ivy-current-match old)
      (should (funcall pred old new)))))

;;; Ivy

(ert-deftest ivy-partial-1 ()
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " '("case" "Case")))
                   "ca TAB C-m")
                 "case"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " '("case" "Case")))
                   "Ca TAB C-m")
                 "Case")))

(ert-deftest ivy-read ()
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow")))
                   "C-m")
                 "blue"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow")))
                   "y C-m")
                 "yellow"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow")))
                   "y DEL b C-m")
                 "blue"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow")))
                   "z C-m")
                 "z"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow")))
                   "y <backspace> C-m")
                 "blue"))
  (should (equal (ivy-test-with ()
                   ((let ((ivy-re-builders-alist `((t . ,#'ivy--regex-fuzzy))))
                      (ivy-read "pattern: " '("package-list-packages"
                                              "something-else"))))
                   "plp C-m")
                 "package-list-packages"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "test" '("aaab" "aaac")))
                   "a C-n <tab> C-m")
                 "aaac"))
  (should (equal-including-properties
           (ivy-test-with () ((ivy-read "test" '(("foo" . "bar")))) "C-m")
           "foo"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "test" '(("foo" . "bar"))))
                   "asdf C-m")
                 "asdf"))
  (should (equal (ivy-test-with ()
                   ((with-output-to-string
                      (ivy-read "test" '(("foo" . "bar"))
                                :action (lambda (x) (prin1 x)))))
                   "f C-m")
                 "(\"foo\" . \"bar\")"))
  (should (equal (ivy-test-with ()
                   ((with-output-to-string
                      (ivy-read "test" '(("foo" . "bar"))
                                :action (lambda (x) (prin1 x)))))
                   "asdf C-m")
                 "\"asdf\""))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("can do" "can" "can't do")))
                   "can C-m")
                 "can"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: "
                              '("ignore" "build" "build-1" "build-2")
                              :preselect "build"))
                   "b C-m")
                 "build"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "x: " '("one" "two" ("three" . "four"))))
                   "th C-m")
                 "three"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "x: " (lambda (_) '(("one" . 1) ("two" . 2)))
                              :dynamic-collection t
                              :action (lambda (_))))
                   "C-m")
                 "one")))

(ert-deftest ivy-read-history ()
  (should (equal (let ((ivy-test-history (list "c" "b" "a")))
                   (ivy-test-with ()
                     ((ivy-read "test: " '("c" "d")
                                :history 'ivy-test-history))
                     "RET")
                   ivy-test-history)
                 '("c" "b" "a")))
  (should (equal (let ((ivy-test-history (list "cdef" "b" "a")))
                   (ivy-test-with ()
                     ((ivy-read "test: " '("cdef" "g")
                                :history 'ivy-test-history))
                     "cd RET")
                   ivy-test-history)
                 '("cd" "cdef" "b" "a"))))

(ert-deftest ivy-read-sort-alist ()
  (should (equal (ivy-test-with ()
                   ((let ((coll (list '("b" . "1") '("a" . "2"))))
                      (ivy-read "test: " coll :sort t)
                      coll))
                   "C-m")
                 '(("b" . "1") ("a" . "2")))))

(ert-deftest ivy-read-alist-multi-cands ()
  (should (equal (ivy-test-with ()
                   ((let (acc)
                      (ivy-read "test: "
                                '(("Key 1" . "Data 1") ("Key 2" . "Data 2"))
                                :action (lambda (x) (push x acc)))
                      acc))
                   "M-a RET")
                 '(("Key 2" . "Data 2") ("Key 1" . "Data 1"))))
  (should (equal (ivy-test-with ()
                   ((let (res)
                      (ivy-read "test: "
                                '(("Key 1" . "Data 1") ("Key 2" . "Data 2"))
                                :action (lambda (x) (push x res))
                                :multi-action (lambda (xs) (setq res xs)))
                      res))
                   "M-a RET")
                 '(("Key 1" . "Data 1") ("Key 2" . "Data 2")))))

(ert-deftest ivy-read-multi-action-1 ()
  (should (equal (let (res
                       (ivy--actions-list (copy-sequence ivy--actions-list)))
                   (ivy-add-actions 'ivy-test
                                    `(("a" ,(lambda (x) (push x res)) "desc")))
                   (ivy-test-with ()
                     ((ivy-read "test: " '("x" "y")
                                :action (lambda (_))
                                :multi-action (lambda (_))
                                :caller 'ivy-test))
                     "M-a M-o a")
                   res)
                 '("y" "x"))))

(ert-deftest ivy-read-multi-action-2 ()
  (should (equal (let (res
                       (ivy--actions-list (copy-sequence ivy--actions-list)))
                   (ivy-add-actions 'ivy-test
                                    `(("a"
                                       ,(lambda (x) (push x res))
                                       "desc"
                                       ,(lambda (xs) (push xs res)))))
                   (ivy-test-with ()
                     ((ivy-read "test: " '("x" "y")
                                :action (lambda (_))
                                :multi-action (lambda (_))
                                :caller 'ivy-test))
                     "M-a M-o a")
                   res)
                 '(("x" "y")))))

(ert-deftest ivy-read-sort-def ()
  (should (equal (ivy-test-with ()
                   ((ivy-read "Test: " '("1" "2") :def '("a" "b" "c")))
                   "C-m")
                 "a")))

(ert-deftest ivy-read-remap ()
  (should (equal (ivy-test-with `(("S-<right>" ,#'end-of-buffer))
                   ((ivy-read "pattern: " '("blue" "yellow" "red")))
                   "S-<right> C-m")
                 "red"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "pattern: " '("blue" "yellow" "red")))
                   "M-> C-m")
                 "red")))

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
  (should (equal (ivy--split "defun [^ ]+") '("defun" "[^ ]+")))
  (should (equal (ivy--split "[^ ]+ -> .*")
                 '("[^ ]+" "->" ".*")))
  (should (equal (ivy--split "[^ \n]+ \\( ->\\)")
                 '("[^ \n]+" "\\( ->\\)")))
  (should (equal (ivy--split "abc[^ \n]+\\( ->\\)")
                 '("abc[^ \n]+" "\\( ->\\)")))
  (should (equal (ivy--split "abc[^ \n]+\\( -> \\)def")
                 '("abc[^ \n]+" "\\( -> \\)" "def")))
  (should (equal (ivy--split "\\(?:interactive\\|swiper\\) \\(?:list\\|symbol\\)")
                 '("\\(?:interactive\\|swiper\\)" "\\(?:list\\|symbol\\)")))
  (should (equal (ivy--split "\\([^ \n]+\\)\\( -> \\).*")
                 '("\\([^ \n]+\\)"
                   "\\( -> \\)"
                   ".*")))
  (should (equal (ivy--split "[^ ]\\( -> \\).*")
                 '("[^ ]" "\\( -> \\)" ".*")))
  (should (equal (ivy--split "[ab][cd]") '("[ab][cd]")))
  (should (equal (ivy--split "[a b][c d]") '("[a b][c d]")))
  (should (equal (ivy--split "[ab] [cd]") '("[ab]" "[cd]")))
  (should (equal (ivy--split "[a b] [c d]") '("[a b]" "[c d]"))))

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
                 "\\.org"))
  (should (equal (ivy--regex "foo\\") "foo"))
  (should (equal (ivy--regex "foo\\|") "foo"))
  (should (equal (ivy--regex "[^ \n]+\\( -> \\).*")
                 "\\([^ \n]+\\)\\( -> \\).*?\\(.*\\)"))
  (should (equal (ivy--regex "\\([^ \\n]+\\)\\( -> \\).*")
                 "\\([^ \\n]+\\)\\( -> \\).*?\\(.*\\)"))
  (should (equal (ivy--regex "\\\\") "\\\\")))

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
  (should (equal (ivy-test-with ()
                   ((read--expression "Eval: " "'s-c-t-st"))
                   "<tab> C-m")
                 ''shell-command-to-string)))

(ert-deftest ivy--regex-fuzzy ()
  (should (equal (ivy--regex-fuzzy "tmux")
                 "\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (equal (ivy--regex-fuzzy ".tmux")
                 "\\(\\.\\)[^t\n]*\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (equal (ivy--regex-fuzzy "^tmux")
                 "^\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)"))
  (should (equal (ivy--regex-fuzzy "^tmux$")
                 "^\\(t\\)[^m\n]*\\(m\\)[^u\n]*\\(u\\)[^x\n]*\\(x\\)$"))
  (should (equal (ivy--regex-fuzzy "")
                 ""))
  (should (equal (ivy--regex-fuzzy "^")
                 "^"))
  (should (equal (ivy--regex-fuzzy "$")
                 "$"))
  (should (equal (ivy--regex-fuzzy "abc\\|")
                 "\\(a\\)[^b\n]*\\(b\\)[^c\n]*\\(c\\)")))

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
                 '(("foo\\[" . t) ("bar[xy]" . t))))
  (should (equal (ivy--regex-ignore-order "foo\\|")
                 '(("foo" . t)))))

(ert-deftest ivy--format ()
  (should (equal (let ((ivy-last (make-ivy-state))
                       (ivy--index 10)
                       (ivy-format-functions-alist
                        `((t . ,(lambda (x) (string-join x "\n")))))
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
                 "
DESCRIPTION
FUNCTION LETTERS
SWITCHES
DIAGNOSTICS
EXAMPLE 1
EXAMPLE 2
EXAMPLE 3
SEE ALSO
AUTHOR")))

(ert-deftest ivy--filter ()
  (should (equal (ivy--filter "the" '("foo" "the" "The"))
                 '("the" "The")))
  (should (equal (ivy--filter "The" '("foo" "the" "The"))
                 '("The"))))

(ert-deftest ivy-backward-kill-word ()
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " () :initial-input "one two three"))
                   "M-DEL M-DEL C-M-j")
                 "one "))
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " () :initial-input "one two three"))
                   "M-DEL M-DEL M-DEL C-y C-M-j")
                 "one two three")))

(ert-deftest ivy-current-prefix-arg ()
  "Check that `ivy-current-prefix-arg' is set."
  (let ((fn (lambda ()
              (let* (arg
                     (action (lambda (_) (setq arg ivy-current-prefix-arg))))
                (ivy-read "" () :action `(1 ("o" ,action) ("p" ,action)))
                arg))))

    ;; No prefix arg.
    (should-not (ivy-test-exec () fn "C-m"))
    (should-not (ivy-test-exec () fn "C-j"))
    (should-not (ivy-test-exec () fn "C-M-j"))
    (should-not (ivy-test-exec () fn "C-M-m"))
    (should-not (ivy-test-exec () fn "C-M-n"))
    (should-not (ivy-test-exec () fn "C-M-p"))
    (should-not (ivy-test-exec () fn "M-o o"))
    (should-not (ivy-test-exec () fn "TAB TAB"))

    ;; With prefix arg.
    (should (equal (ivy-test-exec () fn "C-u C-m") '(4)))
    (should (equal (ivy-test-exec () fn "C-u C-j") '(4)))
    ;; C-M-j does not pass on a prefix arg.
    (should (equal (ivy-test-exec () fn "C-u C-M-j") nil))
    (should (equal (ivy-test-exec () fn "C-u C-M-m") '(4)))
    (should (equal (ivy-test-exec () fn "C-u C-M-n") '(4)))
    (should (equal (ivy-test-exec () fn "C-u C-M-p") '(4)))
    (should (equal (ivy-test-exec () fn "C-u M-o o") '(4)))
    (should (equal (ivy-test-exec () fn "C-u M-o p") '(4)))
    ;; TAB TAB does not pass on a prefix arg.
    (should (equal (ivy-test-exec () fn "TAB TAB") nil))
    (should (equal (ivy-test-exec () fn "M-1 M-2 M-3 C-m") 123))
    (should (equal (ivy-test-exec () fn "C-u 123 C-m") 123))))

(ert-deftest ivy-re-match ()
  (should (ivy-re-match '(("counsel" . t)) "(defun counsel"))
  (should (ivy-re-match '(("defun" . t) ("counsel" . t)) "(defun counsel"))
  (should (ivy-re-match '(("counsel" . t) ("defun" . t)) "(defun counsel"))
  (should-not (ivy-re-match '(("counsel") ("defun" . t)) "(defun counsel"))
  (should-not (ivy-re-match '(("defun" . t) ("counsel")) "(defun counsel")))

(ert-deftest ivy-read-preselect ()
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " (list "abc" "default" "def")
                              :preselect 1))
                   "RET")
                 "default"))
  (should (equal (ivy-test-with ()
                   ((ivy-read "test: " (list "abc" "default" "def")
                              :preselect "defa"))
                   "RET")
                 "default")))

(ert-deftest ivy-completion-in-region ()
  "Test `ivy-completion-in-region' behavior."
  (let ((completion-in-region-function #'ivy-completion-in-region)
        (minibuffer-message-timeout 0))
    (should (equal (ivy-test-with-text () " emacs-lisp-mode-h|"
                     ((emacs-lisp-mode) (execute-kbd-macro (kbd "C-M-i"))))
                   " emacs-lisp-mode-hook|"))
    (should (equal (ivy-test-with-text () "(nconc|"
                     ((emacs-lisp-mode) (execute-kbd-macro (kbd "C-M-i"))))
                   "(nconc|"))))

(ert-deftest ivy-completing-read ()
  (should (equal (ivy-test-with ()
                   ((ivy-completing-read "Test: " '(("1" . "a") ("2" . "b"))))
                   "RET")
                 "1"))
  (should (equal (let ((completing-read-function #'ivy-completing-read)
                       (ivy-test-history (list "foo")))
                   (ivy-test-with ()
                     ((completing-read "Prompt: " '("foo" "bar" "baz")
                                       nil t nil 'ivy-test-history))
                     "fo RET")
                   ivy-test-history)
                 '("foo"))))

(ert-deftest ivy-completing-read-def-handling ()
  ;; DEF in COLLECTION.
  (should (equal "b" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil "b"))
                       "RET")))
  ;; Also make sure that choosing a non-default item works.
  (should (equal "c" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil "b"))
                       "c RET")))
  ;; DEF not in COLLECTION.
  (should (equal "d" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil "d"))
                       "RET")))
  (should (equal "c" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil "d"))
                       "c RET")))
  ;; DEF list, some in COLLECTION.
  (should (equal "e" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil '("e" "b")))
                       "RET")))
  (should (equal "c" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c")
                                             nil t nil nil '("e" "b")))
                       "c RET")))
  ;; DEF nil.
  (should (equal "a" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c") nil t))
                       "RET")))
  (should (equal "c" (ivy-test-with ()
                       ((ivy-completing-read "Pick: " '("a" "b" "c") nil t))
                       "c RET")))
  ;; DEF list, empty input (no text collection), non-text default, same object.
  (let ((def '([a b])))
    (should (eq (ivy-test-with ()
                  ((ivy-completing-read "Pick: " ()
                                        nil 'require-match nil nil def))
                  "RET")
                (car def))))
  ;; DEF nil, and called via `ivy-completing-read-with-empty-string-def'.
  (should (equal "" (ivy-test-with ()
                      ((ivy-completing-read-with-empty-string-def
                        "Pick: " '("a" "b" "c") nil t))
                      "RET")))
  (should (equal "c" (ivy-test-with ()
                       ((ivy-completing-read-with-empty-string-def
                         "Pick: " '("a" "b" "c") nil t))
                       "c RET"))))

(ert-deftest ivy-completing-read-handlers ()
  ;; Temporarily define several identical command symbols.
  (cl-letf* (((symbol-function 'ivy-test-no-handler)
              (lambda (arg)
                "Return ARG, reading it interactively."
                (interactive
                 (list (completing-read "Pick: " '("a" "b" "c") nil t)))
                arg))
             ((symbol-function 'ivy-test-default-handler)
              (symbol-function 'ivy-test-no-handler))
             ((symbol-function 'ivy-test-recursive-handler)
              (symbol-function 'ivy-test-no-handler))
             (ivy-completing-read-handlers-alist
              `((ivy-test-default-handler . ,#'completing-read-default)
                (ivy-test-recursive-handler
                 . ,#'ivy-completing-read-with-empty-string-def)))
             ;; Simulate `ivy-mode'.
             (completing-read-function #'ivy-completing-read))
    ;; No handler.
    (should (equal (ivy-test-with ()
                     ((ivy-test-command-execute 'ivy-test-no-handler))
                     "RET")
                   "a"))
    (should (equal (ivy-test-with ()
                     ((ivy-test-command-execute 'ivy-test-no-handler))
                     "c RET")
                   "c"))
    ;; Handler = `completing-read-default'; ensure `ivy-read' is not called.
    (cl-letf (((symbol-function 'ivy-read)
               (lambda (&rest _)
                 (error "`ivy-read' should not be called"))))
      (should (equal (ivy-test-with ()
                       ((ivy-test-command-execute 'ivy-test-default-handler))
                       "RET")
                     ""))
      (should (equal (ivy-test-with ()
                       ((ivy-test-command-execute 'ivy-test-default-handler))
                       "c RET")
                     "c")))
    ;; Handler = `ivy-completing-read-with-empty-string-def';
    ;; make sure infinite recursion does not occur.
    (should (equal (ivy-test-with ()
                     ((ivy-test-command-execute 'ivy-test-recursive-handler))
                     "RET")
                   ""))
    (should (equal (ivy-test-with ()
                     ((ivy-test-command-execute 'ivy-test-recursive-handler))
                     "c RET")
                   "c"))))

(ert-deftest ivy-completion-common-length ()
  (mapc (lambda (pair)
          (dolist (str (cdr pair))
            (ert-info ((format "%S" str) :prefix "String: ")
              (should (= (with-no-warnings (ivy-completion-common-length str))
                         (car pair))))))
        '((0 ""
             #("a"   0 1 (face completions-first-difference))
             #("ab"  0 1 (face completions-first-difference))
             #("abc" 0 1 (face completions-first-difference))
             #("a"   0 1 (face (completions-first-difference)))
             #("ab"  0 1 (face (completions-first-difference)))
             #("abc" 0 1 (face (completions-first-difference)))
             #("a"   0 1 (font-lock-face (completions-first-difference)))
             #("ab"  0 1 (font-lock-face (completions-first-difference)))
             #("abc" 0 1 (font-lock-face (completions-first-difference)))
             #("abc"
               0 1 (face completions-first-difference)
               1 2 (face default))
             #("abc"
               0 1 (face completions-first-difference)
               2 3 (face default))
             #("abc"
               0 1 (face (completions-first-difference))
               1 2 (face default))
             #("abc"
               0 1 (face (completions-first-difference))
               2 3 (face default)))
          (1 "a"
             #("a"   0 1 (face default))
             #("ab"  1 2 (face completions-first-difference))
             #("ab"  0 2 (face completions-first-difference))
             #("abc" 1 2 (face completions-first-difference))
             #("abc" 0 2 (face completions-first-difference))
             #("ab"  1 2 (face (completions-first-difference)))
             #("ab"  0 2 (face (completions-first-difference)))
             #("abc" 1 2 (face (completions-first-difference)))
             #("abc" 0 2 (face (completions-first-difference)))
             #("ab"  1 2 (font-lock-face (completions-first-difference)))
             #("ab"  0 2 (font-lock-face (completions-first-difference)))
             #("abc" 1 2 (font-lock-face (completions-first-difference)))
             #("abc" 0 2 (font-lock-face (completions-first-difference)))
             #("abc"
               0 1 (face default)
               1 2 (face completions-first-difference))
             #("abc"
               1 2 (face completions-first-difference)
               2 3 (face default))
             #("abc"
               0 1 (face default)
               1 2 (face (completions-first-difference)))
             #("abc"
               1 2 (face (completions-first-difference))
               2 3 (face default)))
          (2 "ab"
             #("ab"  0 1 (face default))
             #("ab"  1 2 (face default))
             #("ab"  0 2 (face default))
             #("abc" 2 3 (face completions-first-difference))
             #("abc" 1 3 (face completions-first-difference))
             #("abc" 0 3 (face completions-first-difference))
             #("abc" 2 3 (face (completions-first-difference)))
             #("abc" 1 3 (face (completions-first-difference)))
             #("abc" 0 3 (face (completions-first-difference)))
             #("abc" 2 3 (font-lock-face (completions-first-difference)))
             #("abc" 1 3 (font-lock-face (completions-first-difference)))
             #("abc" 0 3 (font-lock-face (completions-first-difference)))
             #("abc"
               0 1 (face default)
               2 3 (face completions-first-difference))
             #("abc"
               1 2 (face default)
               2 3 (face completions-first-difference))
             #("abc"
               0 1 (face default)
               2 3 (face (completions-first-difference)))
             #("abc"
               1 2 (face default)
               2 3 (face (completions-first-difference)))
             #("test/"
               0 2 (face completions-common-part)
               2 3 (face completions-first-difference))
             #("test/"
               0 2 (face completions-common-part)
               2 3 (face (completions-first-difference))))
          (3 "abc"
             #("abc" 0 1 (face default))
             #("abc" 1 2 (face default))
             #("abc" 2 3 (face default))
             #("abc" 0 2 (face default))
             #("abc" 1 3 (face default))
             #("abc" 0 3 (face default)))
          (5 #("Math/E"
               0 5 (face completions-common-part)
               5 6 (face completions-first-difference))
             #("Math/E"
               0 5 (face completions-common-part)
               5 6 (face (completions-first-difference)))))))

(ert-deftest ivy--sort-function ()
  "Test `ivy--sort-function' behavior."
  ;; No enabled collections.
  (dolist (alist '(() ((t)) ((t nil)) ((a)) ((a nil))))
    (let ((ivy-sort-functions-alist alist))
      (dolist (coll '(a b))
        (should-not (ivy--sort-function coll)))))
  (dolist (fn (list #'identity (lambda ()) '(lambda ())))
    ;; No fallback.
    (dolist (alist `(((a . ,fn))
                     ((a ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (should (eq (ivy--sort-function 'a) fn))
        (should-not (ivy--sort-function 'b))))
    ;; Only fallback.
    (dolist (alist `(((t . ,fn))
                     ((t ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (dolist (coll '(a b))
          (should (eq (ivy--sort-function coll) fn)))))
    ;; Fallback with disabled collection.
    (dolist (alist `(((a) (t . ,fn))
                     ((a) (t ,fn))))
      (let ((ivy-sort-functions-alist alist))
        (should-not (ivy--sort-function 'a))
        (should (eq (ivy--sort-function 'b) fn)))))
  ;; Fallback with enabled collection.
  (let* ((fn0 #'identity)
         (fn1 (lambda ()))
         (ivy-sort-functions-alist `((a ,fn0) (b) (t ,fn1))))
    (should (eq (ivy--sort-function 'a) fn0))
    (should-not (ivy--sort-function 'b))
    (should (eq (ivy--sort-function 'c) fn1))))

(ert-deftest ivy-read-directory-name ()
  (let ((completing-read-function #'ivy-completing-read))
    (ivy-test-with-tmpdir dir
      (should (equal dir (expand-file-name
                          (ivy-test-with ()
                            ((read-directory-name "cd: " dir nil t))
                            "RET")))))
    (should (equal (ivy-test-with ()
                     ((read-directory-name "cd: " "/tmp"))
                     "RET")
                   (expand-file-name "/tmp/")))
    (let ((default-directory "/tmp/"))
      (should (equal (ivy-test-with () ((read-directory-name "cd: ")) "C-M-j")
                     (expand-file-name "/tmp/")))
      (should (equal (ivy-test-with ()
                       ((read-directory-name "cd: "))
                       "DEL C-M-j")
                     (expand-file-name "/"))))
    (let ((default-directory "/"))
      (should (equal (ivy-test-with ()
                       ((read-directory-name "cd: "))
                       "tmp C-j C-M-j")
                     (expand-file-name "/tmp/"))))))

(ert-deftest ivy-read-file-name-initial-input ()
  (let* ((relname "ivy.el")
         (absname (expand-file-name relname)))
    (should (equal (ivy-test-with ()
                     ((ivy-read "Find file: " #'read-file-name-internal
                                :predicate #'file-exists-p
                                :require-match 'confirm-after-completion
                                :initial-input absname
                                :preselect absname
                                :def absname
                                :caller #'read-file-name-internal
                                :action #'ignore))
                     "RET")
                   absname))
    (should (equal (ivy-state-initial-input ivy-last) relname))))

(ert-deftest ivy-partial-files ()
  (ivy-test-with-tmpdir dir
    (let ((ivy-minibuffer-map
           (let ((map (make-sparse-keymap)))
             ;; Avoid modifying global `ivy-minibuffer-map'.
             (set-keymap-parent map ivy-minibuffer-map)
             (define-key map "\t" #'ivy-partial)
             ;; Allow quitting during `execute-kbd-macro'.
             ;; See issue #2906 and URL `https://bugs.gnu.org/48603'.
             (define-key map "\C-g" #'abort-recursive-edit)
             map))
          (subdirs '("test1/" "test2/")))
      (dolist (subdir subdirs)
        (make-directory (expand-file-name subdir dir)))
      (should (equal (condition-case nil
                         (ivy-test-with ()
                           ((counsel-find-file nil dir))
                           "t TAB TAB TAB C-g")
                       (quit ivy--old-cands))
                     subdirs)))))

(ert-deftest ivy-read-file-name-in-buffer-visiting-file ()
  "Test `ivy-immediate-done' during `read-file-name' in file-visiting buffer."
  (let ((completing-read-function #'ivy-completing-read))
    ;; Abbreviated form of visited file name.
    (should (equal (ivy-test-with ()
                     ((let ((insert-default-directory t))
                        (set-visited-file-name "~/dummy-dir/dummy-file")
                        ;; Don't ask to save in interactive session.
                        (set-buffer-modified-p nil)
                        ;; As per `load-file'.
                        (read-file-name "Load file: " nil nil 'lambda)))
                     ;; No editing, just command `ivy-immediate-done'.
                     "C-M-j")
                   "~/dummy-dir/dummy-file")))
  (should (equal (ivy-state-current ivy-last) "~/dummy-dir/dummy-file")))

(ert-deftest ivy-read-file-name-make-directory ()
  (let ((completing-read-function #'ivy-completing-read)
        (default-directory "/nonexistent/"))
    (should (equal (ivy-test-with ()
                     ((read-file-name
                       "Make directory: " default-directory default-directory))
                     "C-M-j")
                   "/nonexistent/"))))

(ert-deftest ivy-starts-with-dotslash ()
  (should (ivy--starts-with-dotslash "./test1"))
  (should (ivy--starts-with-dotslash ".\\test2"))
  (should-not (ivy--starts-with-dotslash "test3"))
  (should-not (ivy--starts-with-dotslash "t/est4"))
  (should-not (ivy--starts-with-dotslash "t\\est5"))
  (should-not (ivy--starts-with-dotslash "tes./t6")))

(ert-deftest ivy-use-selectable-prompt ()
  "Test `ivy-use-selectable-prompt' effect."
  (dolist (test '((nil "bl C-m"       "blue")
                  (nil "bl C-j"       "blue")
                  (t   "bl C-m"       "blue")
                  (t   "bl C-j"       "blue")
                  (nil "bl C-p C-m"   "blue")
                  (nil "bl C-p C-j"   "blue")
                  (t   "bl C-p C-m"   "bl")
                  (t   "bl C-p C-j"   "bl")
                  (nil "bl C-M-j"     "bl")
                  (t   "bl C-M-j"     "bl")
                  (nil "bl C-p C-M-j" "bl")
                  (t   "bl C-p C-M-j" "bl")))
    (should (equal (ivy-test-with ()
                     ((let ((ivy-use-selectable-prompt (nth 0 test)))
                        (ivy-read "Prompt: " '("blue"))))
                     (nth 1 test))
                   (nth 2 test)))))

(ert-deftest ivy-use-selectable-prompt-mustmatch ()
  "Test `ivy-use-selectable-prompt' with `:require-match'."
  (let ((ivy-use-selectable-prompt t))
    (should (equal (ivy-test-with ()
                     ((ivy-read "Prompt: " '("a" "b") :require-match t))
                     "C-p C-m")
                   "a"))
    (should (equal (ivy-test-with ()
                     ((ivy-read "Prompt: " '("" "a" "b") :require-match t))
                     "C-p C-m")
                   ""))
    (let ((completing-read-function #'ivy-completing-read))
      (should (equal (ivy-test-with ()
                       ((completing-read "Position: " '("" "a" "b") nil t))
                       "C-p C-m")
                     "")))))

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

(ert-deftest ivy--yank-handle-case-fold ()
  (should (equal (let ((ivy-text ""))
                   (ivy--yank-handle-case-fold "FirstName"))
                 "FirstName"))
  (should (equal (let ((ivy-text "f"))
                   (ivy--yank-handle-case-fold "irstName"))
                 "irstname")))

(ert-deftest ivy--handle-directory ()
  (should (equal (ivy--handle-directory "/") "/"))
  (should (equal (let ((ivy--directory "/tmp/"))
                   (ivy--handle-directory "/sudo::"))
                 "/sudo::/tmp/")))

(ert-deftest ivy--handle-full-path-yank-on-remote ()
  (should (equal (let ((ivy--directory "/ssh:dev:/bin/"))
                   (ivy--expand-file-name "/etc/hosts"))
                 "/ssh:dev:/etc/hosts")))

(ert-deftest ivy-inhibit-action ()
  (should (equal (ivy-test-with ()
                   ((let ((ivy-inhibit-action #'identity))
                      (ivy-read "pattern: " '(("a" . 1) ("b" . 2)))))
                   "C-m")
                 '("a" . 1)))
  (should (equal (ivy-test-with ()
                   ((let ((ivy-inhibit-action #'cdr))
                      (ivy-read "pattern: " '(("a" . 1) ("b" . 2)))))
                   "C-n C-m")
                 2)))

(ert-deftest ivy-empty-directory-open ()
  (ivy-test-with-tmpdir default-directory
    (let ((subdir "subdir/"))
      (make-directory subdir)
      (should (equal (file-relative-name
                      (let ((default-directory (expand-file-name subdir)))
                        (ivy-test-exec () #'counsel-find-file "RET")))
                     subdir)))))

(ert-deftest ivy--preselect-index ()
  "Test `ivy--preselect-index' behavior."
  (should (eql (ivy--preselect-index nil ()) 0))
  (should (eql (ivy--preselect-index nil '(nil)) 0))
  (should (eql (ivy--preselect-index nil '(t)) 0))
  (should (eql (ivy--preselect-index nil '(t nil)) 1))
  (should (eql (ivy--preselect-index 0 ()) 0))
  (should (eql (ivy--preselect-index 0 '(0)) 0))
  (should (eql (ivy--preselect-index 0 '(1)) 0))
  (should (eql (ivy--preselect-index 0 '(1 0)) 1))
  (should (eql (ivy--preselect-index 0 '(a)) 0))
  (should (eql (ivy--preselect-index 1 '(a)) 1))
  (should (eql (ivy--preselect-index "" ()) 0))
  (should (eql (ivy--preselect-index "" '("")) 0))
  (should (eql (ivy--preselect-index "" '("a")) 0))
  (should (eql (ivy--preselect-index "a+" '("a")) 0))
  (should (eql (ivy--preselect-index "a+" '("b" "a")) 1)))

(ert-deftest ivy-multi-resume ()
  (with-temp-buffer
    (let ((ivy-last (copy-ivy-state ivy-last))
          (ivy-text nil)
          (ivy--all-candidates ())
          (ivy--sessions ())
          (buf (current-buffer)))
      (ivy-test-with ()
        ;; Use buffer that survives until `ivy-resume'
        ;; (the `ivy-test-with' one is too short-lived).
        ((with-current-buffer buf
           (ivy-read "A: " '("a123" "b456" "c789")
                     :caller 'ivy-test-a
                     :action #'ignore)
           (ivy-read "A: " '("d123" "e456" "f789")
                     :caller 'ivy-test-b)
           (ivy-read "A: " '("g123" "h456" "k789")
                     :action #'ignore
                     :extra-props '(:session ivy-test-c))))
        "b4 RET d1 RET k7 RET")
      (should (equal ivy-text "k7"))
      (should (equal (mapcar #'car ivy--sessions) '(ivy-test-c ivy-test-a)))
      (should (equal (ivy-test-with ()
                       ((let ((current-prefix-arg '(4)))
                          (ivy-resume)))
                       "test-a RET RET")
                     "b456"))
      (should (equal ivy-text "b4"))
      (should (equal (ivy-test-with () ((ivy-resume 'ivy-test-c)) "RET")
                     "k789"))
      (should (equal ivy-text "k7")))))

(ert-deftest ivy--break-lines ()
  "Test `ivy--break-lines' behavior."
  (dolist (width '(-1 0))
    (dolist (str '("" "\n" "a" "a\nb"))
      (should (equal (ivy--break-lines str width) str))))
  (should (equal (ivy--break-lines "" 1) ""))
  (should (equal (ivy--break-lines "a" 1) "a"))
  (should (equal (ivy--break-lines "a" 2) "a"))
  (should (equal (ivy--break-lines "ab" 1) "a\nb"))
  (should (equal (ivy--break-lines "ab" 2) "ab"))
  (should (equal (ivy--break-lines "abc" 1) "a\nb\nc"))
  (should (equal (ivy--break-lines "abc" 2) "ab\nc"))
  (should (equal (ivy--break-lines "abc" 3) "abc"))
  (should (equal (ivy--break-lines "\^X" 1) "\^X"))
  (should (equal (ivy--break-lines "\^X" 2) "\^X"))
  (should (equal (ivy--break-lines "a\^X" 1) "a\n\^X"))
  (should (equal (ivy--break-lines "a\^X" 2) "a\n\^X"))
  (should (equal (ivy--break-lines "a\^X" 3) "a\^X"))
  (should (equal (ivy--break-lines "\^X\^X" 1) "\^X\n\^X"))
  (should (equal (ivy--break-lines "\^X\^X" 2) "\^X\n\^X"))
  (should (equal (ivy--break-lines "\^X\^X" 3) "\^X\n\^X"))
  (should (equal (ivy--break-lines "\^X\^X" 4) "\^X\^X"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 1)
                 "\nf\no\no\n\^X\n\^X\n\^X\nb\na\nr\n"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 2)
                 "\nfo\no\n\^X\n\^X\n\^X\nba\nr\n"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 3)
                 "\nfoo\n\^X\n\^X\n\^X\nbar\n"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 4)
                 "\nfoo\n\^X\^X\n\^X\nbar\n"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 5)
                 "\nfoo\n\^X\^X\n\^X\nbar\n"))
  (should (equal (ivy--break-lines "\nfoo\n\^X\^X\^X\nbar\n" 6)
                 "\nfoo\n\^X\^X\^X\nbar\n")))

(ert-deftest ivy-avy ()
  (skip-unless (require 'avy nil t))
  (require 'ivy-avy)
  (let ((nums (lambda ()
                (ivy-read "test: " (mapcar #'number-to-string
                                           (number-sequence 1 100))))))
    (should (equal (ivy-test-exec () nums "C-' a") "1"))
    (should (equal (ivy-test-exec () nums "C-v C-' d") "7"))))

;;; Swiper

(ert-deftest swiper--re-builder ()
  (cl-letf ((swiper--width 4)
            ((ivy-state-caller ivy-last) 'swiper))
    (should (equal (swiper--re-builder "^") "^ "))
    (should (equal (swiper--re-builder "^a") "^ a"))
    (should (equal (swiper--re-builder "^a b") "\\(^ a\\).*?\\(b\\)"))
    (should (string-match-p "\\`\\\\_<.*\\\\_>\\'"
                            (swiper--re-builder "\\_<iv\\_>")))))

(ert-deftest swiper--re-builder-char-fold ()
  :expected-result (if (>= emacs-major-version 25)
                       :passed
                     :failed)
  ;; New in Emacs 25.
  (defvar search-default-mode)
  (let ((search-default-mode 'char-fold-to-regexp))
    (should (equal (swiper--re-builder "f b")
                   "\
\\(\\(?:ḟ\\|[fᶠḟⓕｆ𝐟𝑓𝒇𝒻𝓯𝔣𝕗𝖋𝖿𝗳𝘧𝙛𝚏]\\)\\).*?\
\\(\\(?:b[̣̱̇]\\|[bᵇḃḅḇⓑｂ𝐛𝑏𝒃𝒷𝓫𝔟𝕓𝖇𝖻𝗯𝘣𝙗𝚋]\\)\\)"))
    (should (= ivy--subexps 2))))

(ert-deftest swiper-query-replace ()
  (dolist (re-builder (list #'regexp-quote
                            #'ivy--regex
                            #'ivy--regex-plus
                            #'ivy--regex-fuzzy
                            #'ivy--regex-ignore-order))
    (let ((ivy-re-builders-alist `((t . ,re-builder))))
      (dolist (swiper-cmd (list #'swiper #'swiper-isearch))
        (should (equal (ivy-test-with-text `(("C-s" ,swiper-cmd)) "|foo bar"
                         ((execute-kbd-macro (kbd "C-s foo M-q asdf C-j y"))))
                       "asdf| bar"))))))

(ert-deftest swiper-thing-at-point ()
  (should (equal (ivy-test-with-text `(("C-s" ,#'swiper-thing-at-point))
                   "let\n|let\nlet"
                   ((execute-kbd-macro (kbd "C-s RET"))))
                 "let\nlet|\nlet"))
  (should (equal (ivy-test-with-text `(("C-s" ,#'swiper-thing-at-point))
                   "foo\nlet\nbar\n|let\nlet"
                   ((execute-kbd-macro (kbd "C-s RET"))))
                 "foo\nlet\nbar\nlet|\nlet")))

(ert-deftest swiper-isearch ()
  (should (equal (ivy-test-with-text `(("C-s" ,#'isearch-forward-regexp))
                   "abc\na|sdf123 def\ndem"
                   ((execute-kbd-macro (kbd "C-s de  RET"))))
                 "abc\nasd|f123 def\ndem"))
  (should (equal (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                   "abc\na|sdf123 def\ndem"
                   ((execute-kbd-macro (kbd "C-s de  RET"))))
                 "abc\nasd|f123 def\ndem"))
  (should (equal (ivy-test-with-text `(("C-s" ,#'isearch-forward-regexp))
                   "|(defun foo)\nasdf\n(defvar bar)"
                   ((execute-kbd-macro (kbd "C-s defun\\|defvar RET"))))
                 "(defun| foo)\nasdf\n(defvar bar)"))
  (should (equal (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                   "|(defun foo)\nasdf\n(defvar bar)"
                   ((execute-kbd-macro (kbd "C-s defun\\|defvar RET"))))
                 "(defun| foo)\nasdf\n(defvar bar)"))
  (should (equal (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                   "|(defun foo)\nasdf\n(defvar bar)"
                   ((execute-kbd-macro (kbd "C-s defun\\|defvar C-n RET"))))
                 "(defun foo)\nasdf\n(defvar| bar)")))

(ert-deftest swiper-isearch-backward ()
  (should (equal (ivy-test-with-text `(("C-r" ,#'isearch-backward-regexp))
                   "abc\nasdf123 def\ndem|"
                   ((execute-kbd-macro (kbd "C-r de  RET"))))
                 "abc\nasdf123 def\n|dem"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "abc\nasdf123 def\ndem|"
                   ((execute-kbd-macro (kbd "C-r de  RET"))))
                 "abc\nasdf123 def\n|dem"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'isearch-backward-regexp))
                   "(defun foo)\nasdf\n(defvar bar)|"
                   ((execute-kbd-macro (kbd "C-r defun\\|defvar RET"))))
                 "(defun foo)\nasdf\n(|defvar bar)"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "(defun foo)\nasdf\n(defvar bar)|"
                   ((execute-kbd-macro (kbd "C-r defun\\|defvar RET"))))
                 "(defun foo)\nasdf\n(|defvar bar)"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'isearch-backward))
                   "(defun foo)\nasdf\n(|defun bar)"
                   ((execute-kbd-macro (kbd "C-r defun RET"))))
                 "(|defun foo)\nasdf\n(defun bar)"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "(defun foo)\nasdf\n(|defun bar)"
                   ((execute-kbd-macro (kbd "C-r defun RET"))))
                 "(|defun foo)\nasdf\n(defun bar)"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "(defun foo)\nasdf\n(de|fun bar)"
                   ((execute-kbd-macro (kbd "C-r def RET"))))
                 "(|defun foo)\nasdf\n(defun bar)"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "(defun foo)\nasdf\n(de|fun bar)"
                   ((execute-kbd-macro (kbd "C-r def? RET"))))
                 "(defun foo)\nasdf\n(|defun bar)")))

(ert-deftest swiper-isearch-backward-backspace ()
  (should (equal (ivy-test-with-text `(("C-r" ,#'swiper-isearch-backward))
                   "(while (when |))"
                   ((execute-kbd-macro (kbd "C-r whi  RET"))))
                 "(while (|when ))"))
  (should (equal (ivy-test-with-text `(("C-r" ,#'isearch-backward-regexp))
                   "(while (when |))"
                   ((execute-kbd-macro (kbd "C-r whi  RET"))))
                 "(while (|when ))")))

(ert-deftest swiper-isearch-case-fold ()
  (should (equal (let ((ivy-case-fold-search-default nil))
                   (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                     "|Foo\nfoo\nFOO\n"
                     ((execute-kbd-macro (kbd "C-s foo C-n RET")))))
                 "Foo\nfoo|\nFOO\n"))
  (should (equal (let ((ivy-case-fold-search-default 'auto))
                   (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                     "|Foo\nfoo\nFOO\n"
                     ((execute-kbd-macro (kbd "C-s Foo C-n RET")))))
                 "Foo|\nfoo\nFOO\n"))
  (should (equal (let ((ivy-case-fold-search-default t))
                   (ivy-test-with-text `(("C-s" ,#'swiper-isearch))
                     "|Foo\nfoo\nFOO\n"
                     ((execute-kbd-macro (kbd "C-s Foo C-n RET")))))
                 "Foo\nfoo|\nFOO\n")))

(ert-deftest swiper-wgrep ()
  ;; `wgrep' requires Emacs 25 or later.
  (skip-unless (and (>= emacs-major-version 25)
                    (require 'wgrep nil t)))
  (let ((default-directory "/tmp/")
        bufname)
    (dolist (search-cmd '(swiper swiper-isearch))
      (should (equal (ivy-test-with-text `(("C-s" ,search-cmd))
                       "|a one\na two\na three"
                       ((setq bufname (buffer-name))
                        (execute-kbd-macro
                         (kbd "C-s a C-c C-o C-x C-q C-e 1 C-n 2 C-n C-e 3"))))
                     (format "-*- mode:grep; default-directory: \"/tmp/\" -*-


3 candidates:
./%s:1:a one1
./%s:2:a two2
./%s:3:a three3|
"
                             ;; Explicit field numbers only added in Emacs 26.
                             bufname bufname bufname))))))

(ert-deftest swiper--isearch-format ()
  (with-temp-buffer
    (let ((swiper--opoint (point-min))
          (lines '("line0" "line1" "line line" "line line" "line5")))
      (insert (string-join lines "\n"))
      (let* ((input "li")
             (cands (progn
                      (ivy-set-text input)
                      (swiper--isearch-function input)))
             (len (length cands)))
        (should (equal cands '(3 9 15 20 25 30 35)))
        (dotimes (index len)
          (should (equal (swiper--isearch-format
                          index len cands input
                          (nth index cands)
                          (current-buffer))
                         lines)))))))

;;; Counsel

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
                  '(("foo\\|bar" . t)
                    ("blah\\|bloop")
                    ("blick" . t)
                    ("\\(baz\\)\\|quux" . t)))
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

(ert-deftest counsel--M-x-prompt ()
  "Test `counsel--M-x-prompt' behavior."
  (should (equal (counsel--M-x-prompt ()) "M-x "))
  (should (equal (counsel--M-x-prompt t) "M-x "))
  (should (equal (counsel--M-x-prompt '(())) "M-x "))
  (should (equal (counsel--M-x-prompt '(t)) "M-x "))
  (should (equal (counsel--M-x-prompt -1) "-1 M-x "))
  (should (equal (counsel--M-x-prompt '(-1)) "-1 M-x "))
  (should (equal (counsel--M-x-prompt 0) "0 M-x "))
  (should (equal (counsel--M-x-prompt '(0)) "0 M-x "))
  (should (equal (counsel--M-x-prompt 1) "1 M-x "))
  (should (equal (counsel--M-x-prompt '(1)) "1 M-x "))
  (should (equal (counsel--M-x-prompt 4) "4 M-x "))
  (should (equal (counsel--M-x-prompt '(4)) "C-u M-x "))
  (should (equal (counsel--M-x-prompt 16) "16 M-x "))
  (should (equal (counsel--M-x-prompt '(16)) "16 M-x "))
  (should (equal (counsel--M-x-prompt '-) "- M-x ")))

(ert-deftest counsel-url-expand ()
  "Test ffap expansion using `counsel-url-expansions-alist'."
  ;; No expansions defined.
  (let ((counsel-url-expansions-alist ()))
    (with-temp-buffer
      (should-not (counsel-url-expand))
      (insert "foo")
      (should-not (counsel-url-expand))))
  (let ((counsel-url-expansions-alist
         `(("\\`foo\\'" . "https://foo.com/%s")
           ("\\`issue\\([0-9]+\\)"
            . ,(lambda (word)
                 (concat "https://foo.com/issues/"
                         (match-string 1 word)))))))
    ;; No match.
    (should-not (with-temp-buffer (insert "foobar") (counsel-url-expand)))
    ;; String expansion.
    (should (equal (with-temp-buffer (insert "foo") (counsel-url-expand))
                   "https://foo.com/foo"))
    ;; Function expansion.
    (should (equal (with-temp-buffer (insert "issue123") (counsel-url-expand))
                   "https://foo.com/issues/123"))))

(ert-deftest counsel-read-directory-name ()
  (should (equal (ivy-test-with ()
                   ((let ((default-directory "/tmp/"))
                      (counsel-read-directory-name "cd: ")))
                   "RET")
                 "/tmp/")))

(ert-deftest counsel-yank-pop ()
  "Test `counsel-yank-pop' behavior."
  (dolist (after '(nil t))
    (let ((kill-ring (list "foo"))
          (counsel-yank-pop-after-point after))
      (should (equal (ivy-test-with-text () "|" ((counsel-yank-pop)) "C-m")
                     (if after "|foo" "foo|"))))))

(ert-deftest counsel-string-non-blank-p ()
  "Test `counsel-string-non-blank-p'."
  (should-not (counsel-string-non-blank-p ""))
  (should-not (counsel-string-non-blank-p " "))
  (should-not (counsel-string-non-blank-p "  "))
  (should (counsel-string-non-blank-p "a"))
  (should (counsel-string-non-blank-p " a"))
  (should (counsel-string-non-blank-p "a "))
  (should (counsel-string-non-blank-p "aa")))

(ert-deftest counsel--equal-w-props ()
  "Sanity check for `sxhash-equal-including-properties'."
  (let ((name 'counsel--equal-w-props)
        (test (counsel--equal-w-props)))
    (should (eq test (and (>= emacs-major-version 28) name)))
    (if test
        (should (make-hash-table :test test :size 0))
      (should-not (get name 'hash-table-test)))))

(ert-deftest counsel--yank-pop-filter ()
  "Test `counsel--yank-pop-filter'."
  (should-not (counsel--yank-pop-filter ()))
  (dolist (len '(1 2 3 120))
    (let (kills)
      (dotimes (_ len)
        (push (propertize "a" t nil) kills))
      (should (equal (counsel--yank-pop-filter kills) '("a")))))
  (dolist (len '(1 2 3 60))
    (let (kills)
      (dotimes (_ len)
        (push (propertize "a" t nil) kills)
        (push (propertize "a" t t) kills))
      (should (equal (counsel--yank-pop-filter kills) '("a" "a"))))))

(ert-deftest counsel--normalize-grep-match ()
  (with-temp-buffer
    (insert "abcd\nefgh")
    (goto-char (point-min))
    (re-search-forward "\\(ab\\)\\(cd\\)")
    (let ((match-data-orig (match-data)))
      (dolist (test '(("./FILENAME:1234:32:  TEXT   MORETEXT" .
                       "./FILENAME:1234:  TEXT   MORETEXT")
                      ("FILENAME:1234:  TEXT   MORETEXT" .
                       "./FILENAME:1234:  TEXT   MORETEXT")))
        (let* ((input (car test))
               (expected (cdr test))
               (out (counsel--normalize-grep-match input)))
          (should (equal out expected))
          (should (equal match-data-orig (match-data)))
          (setq out (counsel--normalize-grep-match out))
          (should (equal out expected))
          (should (equal match-data-orig (match-data))))))))

(ert-deftest counsel--grep-regex ()
  ;; Negative lookahead: lines with "ivy", without "-".
  (should (equal (cl-letf ((counsel--regex-look-around t)
                           ((ivy-state-re-builder ivy-last) #'ivy--regex-plus))
                   (counsel--grep-regex "ivy ! -"))
                 "^(?=.*ivy)(?!.*-)"))
  (should (equal (cl-letf ((counsel--regex-look-around t)
                           ((ivy-state-re-builder ivy-last) #'ivy--regex-fuzzy))
                   (counsel--grep-regex "ivy"))
                 "(i)[^v\n]*(v)[^y\n]*(y)")))

(ert-deftest counsel-find-file-with-dollars ()
  (ivy-test-with-tmpdir default-directory
    (mapc #'ivy-test-empty-file '("foo$" "one" "two" "$"))
    (should (equal (ivy-test-exec () #'counsel-find-file "fo C-m")
                   (expand-file-name "foo$")))))

(ert-deftest counsel-find-file-with-dotfiles ()
  (ivy-test-with-tmpdir default-directory
    (dolist (file '("foo/placeholder" ".foobar1" ".foobar2"))
      (ivy-test-empty-file file t))
    (should (equal (ivy-test-exec () #'counsel-find-file "f C-m")
                   (expand-file-name "foo/")))
    (should (equal (ivy-test-exec () #'counsel-find-file "foob C-m")
                   (expand-file-name ".foobar1")))))

(ert-deftest counsel-find-file-with-spaces ()
  (ivy-test-with-tmpdir default-directory
    (dolist (file '("bar baz i/file1" "bar baz ii/file2"))
      (ivy-test-empty-file file t))
    (let ((ivy-extra-directories nil))
      (should (equal (ivy-test-exec
                      () #'counsel-find-file "TAB TAB TAB TAB")
                     (expand-file-name "bar baz i/file1")))
      (should (equal (ivy-test-exec
                      () #'counsel-find-file "C-n TAB TAB TAB TAB")
                     (expand-file-name "bar baz ii/file2")))
      (should (equal (ivy-test-exec
                      () #'counsel-find-file "TAB C-n TAB TAB TAB TAB")
                     (expand-file-name "bar baz ii/file2"))))))

(ert-deftest counsel-find-file-single-match-directories ()
  (ivy-test-with-tmpdir default-directory
    (dolist (file '("a/file_in_a.txt" "ba/file_in_ba.txt"))
      (ivy-test-empty-file file t))
    (let ((ivy-extra-directories nil))
      (dolist (ivy-re-builders-alist `(((t . ,#'ivy--regex-plus))
                                       ((t . ,#'ivy--regex-ignore-order))))
        (should (equal (ivy-test-exec () #'counsel-find-file "a TAB TAB TAB")
                       (expand-file-name "a/file_in_a.txt")))
        (should (equal (ivy-test-exec () #'counsel-find-file "b TAB TAB TAB")
                       (expand-file-name "ba/file_in_ba.txt")))))))

(ert-deftest counsel--split-string ()
  "Test `counsel--split-string' behavior."
  (should (equal (counsel--split-string "one\rtwo") '("one" "two")))
  (should (equal (counsel--split-string "one\ntwo") '("one" "two")))
  (should (equal (counsel--split-string "one\r\ntwo") '("one" "two"))))

(ert-deftest counsel--split-command-args ()
  (should (equal (counsel--split-command-args "require -- -g*.el")
                 '("-g*.el" . "require")))
  (should (equal (counsel--split-command-args "-g*.el -- require")
                 '("-g*.el" . "require")))
  (should (equal (counsel--split-command-args "counsel--format")
                 '("" . "counsel--format"))))

(provide 'ivy-test)

;;; ivy-test.el ends here
