;;; counsel-ag-popup.el --- Interactive search with ag -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  Eder Elorriaga

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; Keywords: convenience, matching, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Just call the interactive function `counsel-ag-popup' and use the
;; popup to configure the search.

;;; Code:

(require 'counsel)
(require 'transient)

(eval-when-compile
  (require 'subr-x))

(defun counsel-ag-popup-search-here ()
  "Ag search in current directory."
  (interactive)
  (counsel-ag-popup-search default-directory))

(defun counsel-ag-popup-search (directory)
  "Ag search in DIRECTORY."
  (interactive "DDirectory: ")
  (let ((ag-args (counsel-ag-popup--map-args (transient-args 'counsel-ag-popup))))
    (counsel-ag "" directory ag-args)))

(defun counsel-ag-popup--map-args (transient-args)
  "Convert TRANSIENT-ARGS to a string of args."
  (mapconcat
   (lambda (arg)
     (if (listp arg)
         (let ((args (cdr arg)))
           (mapconcat (lambda (x) (concat "--" x)) args " "))
       arg))
   transient-args
   " "))

(defvar counsel-ag-popup-file-types
  '("actionscript" "ada" "asciidoc" "asm" "batch" "bitbake" "bro" "cc" "cfmx"
    "chpl" "clojure" "coffee" "cpp" "crystal" "csharp" "css" "cython" "delphi"
    "dot" "ebuild" "elisp" "elixir" "elm" "erlang" "factor" "fortran" "fsharp"
    "gettext" "glsl" "go" "groovy" "haml" "handlebars" "haskell" "haxe" "hh"
    "html" "ini" "ipython" "jade" "java" "js" "json" "jsp" "julia" "kotlin"
    "less" "liquid" "lisp" "log" "lua" "m4" "make" "mako" "markdown" "mason"
    "matlab" "mathematica" "md" "mercury" "nim" "nix" "objc" "objcpp" "ocaml"
    "octave" "org" "parrot" "perl" "php" "pike" "plist" "plone" "proto" "puppet"
    "python" "qml" "racket" "rake" "restructuredtext" "rs" "r" "rdoc" "ruby"
    "rust" "salt" "sass" "scala" "scheme" "shell" "smalltalk" "sml" "sql"
    "stylus" "swift" "tcl" "tex" "tt" "toml" "ts" "twig" "vala" "vb" "velocity"
    "verilog" "vhdl" "vim" "wix" "wsdl" "wadl" "xml" "yaml")
  "List of supported file types.")

(defun counsel-ag-popup-read-file-types (prompt initial-input history)
  "Prompt for Ag file type with PROMPT INITIAL-INPUT HISTORY."
  (completing-read-multiple
   prompt
   counsel-ag-popup-file-types
   nil nil
   initial-input
   history))

(defclass counsel-ag-popup-file-types (transient-infix) ()
  "Class used for the \"--\" argument.
All remaining arguments are treated as file types.
They become the value of this argument.")

(cl-defmethod transient-format-value ((obj counsel-ag-popup-file-types))
  "Format OBJ's value for display and return the result."
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize (mapconcat (lambda (f) (concat argument f))
                               (oref obj value) " ")
                    'face 'transient-argument)
      (propertize argument 'face 'transient-inactive-argument))))

(cl-defmethod transient-init-value ((obj counsel-ag-popup-file-types))
  "Set the initial value of the object OBJ."
  (oset obj value
        (cdr (assoc "--" (oref transient--prefix value)))))

(cl-defmethod transient-infix-value ((obj counsel-ag-popup-file-types))
  "Return (concat ARGUMENT VALUE) or nil.

ARGUMENT and VALUE are the values of the respective slots of OBJ.
If VALUE is nil, then return nil.  VALUE may be the empty string,
which is not the same as nil."
  (when-let ((value (oref obj value)))
    (cons (oref obj argument) value)))

(transient-define-argument counsel-ag-popup:-- ()
  "Restrict the search to certain types of files."
  :description "Limit to file types"
  :class 'counsel-ag-popup-file-types
  :key "--"
  :argument "--"
  :prompt "Limit to file type(s): "
  :multi-value t
  :reader #'counsel-ag-popup-read-file-types)

(transient-define-prefix counsel-ag-popup ()
  "Recursive search with Ag."
  ["Output options"
   ("-A" "After" "--after=" transient-read-number-N+)
   ("-B" "Before" "--before=" transient-read-number-N+)
   ("-C" "Context" "--context=" transient-read-number-N+)]
  ["Search options"
   ("-i" "Ignore case" "--ignore-case")
   ("-m" "Max count" "--max-count " transient-read-number-N+)
   ("-Q" "Literal" "--literal")
   ("-s" "Case sensitive" "--case-sensitive")
   ("-S" "Smart case" "--smart-case")
   ("-v" "Invert match" "--invert-match")
   ("-w" "Word regexp" "--word-regexp")
   (counsel-ag-popup:--)]
  ["Search"
   ("s" "in current directory" counsel-ag-popup-search-here)
   ("o" "in other directory" counsel-ag-popup-search)])

(provide 'counsel-ag-popup)
;;; counsel-ag-popup.el ends here
