;;; counsel.el --- Various completion functions using Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2024 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Basil L. Contovounesios <basil@contovou.net>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.14.2
;; Package-Requires: ((emacs "24.5") (ivy "0.14.2") (swiper "0.14.2"))
;; Keywords: convenience, matching, tools

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

;; Just call one of the interactive functions in this file to complete
;; the corresponding thing using `ivy'.
;;
;; Currently available:
;; - Symbol completion for Elisp, Common Lisp, Python, Clojure, C, C++.
;; - Describe functions for Elisp: function, variable, library, command,
;;   bindings, theme.
;; - Navigation functions: imenu, ace-line, semantic, outline.
;; - Git utilities: git-files, git-grep, git-log, git-stash, git-checkout.
;; - Grep utilities: grep, ag, pt, recoll, ack, rg.
;; - System utilities: process list, rhythmbox, linux-app.
;; - Many more.

;;; Code:

(require 'ivy)
(require 'swiper)

(require 'compile)
(require 'dired)

(eval-when-compile
  (require 'subr-x))

(defgroup counsel nil
  "Completion functions using Ivy."
  :group 'matching
  :prefix "counsel-")

;;* Utility
(defun counsel--elisp-to-pcre (regex &optional look-around)
  "Convert REGEX from Elisp format to PCRE format, on best-effort basis.
REGEX may be of any format returned by an Ivy regex function,
namely a string or a list.  The return value is always a string.

Note that incorrect results may be returned for sufficiently
complex regexes."
  (if (consp regex)
      (if (and look-around
               (or (cdr regex)
                   (not (cdar regex))))
          (concat
           "^"
           (mapconcat
            (lambda (pair)
              (let ((subexp (counsel--elisp-to-pcre (car pair))))
                (format "(?%c.*%s)"
                        (if (cdr pair) ?= ?!)
                        subexp)))
            regex
            ""))
        (mapconcat
         (lambda (pair)
           (let ((subexp (counsel--elisp-to-pcre (car pair))))
             (if (ivy--string-search "|" subexp)
                 (format "(?:%s)" subexp)
               subexp)))
         (cl-remove-if-not #'cdr regex)
         ".*"))
    (replace-regexp-in-string
     "\\\\[(){}|`']\\|[()]"
     (lambda (s)
       (or (cdr (assoc s '(("\\(" . "(")
                           ("\\)" . ")")
                           ("(" . "\\(")
                           (")" . "\\)")
                           ("\\{" . "{")
                           ("\\}" . "}")
                           ("\\|" . "|")
                           ("\\`" . "^")
                           ("\\'" . "$"))))
           (error
            "Unexpected error in `counsel--elisp-to-pcre' (got match %S)" s)))
     regex t t)))

(defun counsel-directory-name (dir)
  "Return the name of directory DIR with a slash."
  (file-name-as-directory
   (file-name-nondirectory
    (directory-file-name dir))))

(defun counsel-string-compose (prefix str)
  "Make PREFIX the display prefix of STR through text properties."
  (let ((str (copy-sequence str)))
    (put-text-property
     0 1 'display
     (concat prefix (substring str 0 1))
     str)
    str))

(defalias 'counsel--executable-find
  ;; Gained optional argument in 27.1.
  (if (>= emacs-major-version 27)
      #'executable-find
    (lambda (command &optional _remote)
      (executable-find command)))
  "Compatibility shim for `executable-find'.")

(defun counsel-require-program (cmd &optional noerror)
  "Check system for program used in CMD, printing error if not found.
CMD is either a string or a list of strings.
To skip the `executable-find' check, start the string with a space.
When NOERROR is non-nil, return nil instead of raising an error."
  (unless (and (stringp cmd) (string-prefix-p " " cmd))
    (let ((program (if (listp cmd)
                       (car cmd)
                     (car (split-string cmd)))))
      (or (and (stringp program)
               (not (string= program ""))
               (counsel--executable-find program t))
          (unless noerror
            (user-error "Required program \"%s\" not found in your path" program))))))

(declare-function eshell-split-path "esh-util")

(defun counsel-prompt-function-dir ()
  "Return prompt appended with the parent directory."
  (require 'esh-util)
  (let* ((dir (ivy-state-directory ivy-last))
         (parts (nthcdr 3 (eshell-split-path dir)))
         (dir (format " [%s]: " (if parts (apply #'concat "..." parts) dir))))
    (ivy-add-prompt-count
     (replace-regexp-in-string          ; Insert dir before any trailing colon.
      "\\(?:: ?\\)?\\'" dir (ivy-state-prompt ivy-last) t t))))

(defalias 'counsel--flatten
  ;; Added in Emacs 27.1
  (if (fboundp 'flatten-tree)
      #'flatten-tree
    (lambda (tree)
      (let (elems)
        (while (consp tree)
          (let ((elem (pop tree)))
            (while (consp elem)
              (push (cdr elem) tree)
              (setq elem (car elem)))
            (if elem (push elem elems))))
        (if tree (push tree elems))
        (nreverse elems))))
  "Compatibility shim for `flatten-tree'.")

(defun counsel--format (formatter &rest args)
  "Like `format' but FORMATTER can be a list.
When FORMATTER is a list, only `%s' is replaced with ARGS.

Return a list or string depending on input."
  (cond
   ((listp formatter)
    (counsel--flatten (mapcar
                       (lambda (it) (if (equal it "%s") (pop args) it))
                       formatter)))
   (t (apply #'format formatter args))))

(defalias 'counsel--null-device
  (if (fboundp 'null-device) #'null-device (lambda () null-device))
  "Compatibility shim for Emacs 28 function `null-device'.")

;;* Async Utility
(defvar counsel--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar counsel--async-start nil
  "Store the time when a new process was started.")

(defvar counsel--async-timer nil
  "Timer used to dispose `counsel--async-command.")

(defvar counsel--async-duration nil
  "Store the time a process takes to gather all its candidates.
The time is measured in seconds.")

(defvar counsel--async-exit-code-plist ()
  "Associate commands with their exit code descriptions.
This plist maps commands to a plist mapping their exit codes to
descriptions.")

(defvar counsel--async-last-error-string nil
  "When the process returned non-0, store the output here.")

(defun counsel-set-async-exit-code (cmd number str)
  "For CMD, associate NUMBER exit code with STR."
  (let ((plist (plist-get counsel--async-exit-code-plist cmd)))
    (setq counsel--async-exit-code-plist
          (plist-put counsel--async-exit-code-plist
                     cmd
                     (plist-put plist number str)))))

(defvar counsel-async-split-string-re-alist '((t . "[\r\n]"))
  "Store the regexp for splitting shell command output.")

(defvar counsel-async-ignore-re-alist nil
  "An alist of regexp matching candidates to ignore in `counsel--async-filter'.")

(defvar counsel--async-last-command nil
  "Store the last command ran by `counsel--async-command-1'.")

(defun counsel--async-command-1 (cmd &optional sentinel filter name)
  "Start and return new counsel process by calling CMD.
CMD can be either a shell command as a string, or a list of the
program name to be called directly, followed by its arguments.
If the default counsel process or one with NAME already exists,
kill it and its associated buffer before starting a new one.
Give the process the functions SENTINEL and FILTER, which default
to `counsel--async-sentinel' and `counsel--async-filter',
respectively."
  (counsel-delete-process name)
  (setq name (or name " *counsel*"))
  (when (get-buffer name)
    (kill-buffer name))
  (setq counsel--async-last-command cmd)
  (let* ((buf (get-buffer-create name))
         (proc (if (listp cmd)
                   (apply #'start-file-process name buf cmd)
                 (start-file-process-shell-command name buf cmd))))
    (setq counsel--async-time (current-time))
    (setq counsel--async-start counsel--async-time)
    (set-process-sentinel proc (or sentinel #'counsel--async-sentinel))
    (set-process-filter proc (or filter #'counsel--async-filter))
    proc))

(defcustom counsel-async-command-delay 0
  "Number of seconds to wait before spawning another async command."
  :type 'number)

(defun counsel--async-command (&rest args)
  "Like `counsel--async-command-1', with same ARGS, but debounced.
Calls to `counsel--async-command-1' are separated by at least
`counsel-async-command-delay' seconds, so as to avoid issues
caused by spawning too many subprocesses too quickly."
  (if (zerop counsel-async-command-delay)
      (apply #'counsel--async-command-1 args)
    (when counsel--async-timer
      (cancel-timer counsel--async-timer))
    (setq counsel--async-timer
          (apply #'run-with-timer
                 counsel-async-command-delay
                 nil
                 #'counsel--async-command-1
                 args))))

(defun counsel--split-string (&optional str)
  (split-string
   (or str (buffer-string))
   (ivy-alist-setting counsel-async-split-string-re-alist)
   t))

(defun counsel--sync-sentinel-on-exit (process)
  (if (zerop (process-exit-status process))
      (let ((cur (ivy-state-current ivy-last)))
        (ivy--set-candidates
         (ivy--sort-maybe
          (with-current-buffer (process-buffer process)
            (counsel--split-string))))
        (when counsel--async-start
          (setq counsel--async-duration
                (time-to-seconds (time-since counsel--async-start))))
        (let ((re (ivy-re-to-str ivy-regex)))
          (if ivy--old-cands
              (if (eq (ivy-alist-setting ivy-index-functions-alist) 'ivy-recompute-index-zero)
                  (ivy-set-index 0)
                (ivy--recompute-index re ivy--all-candidates))
            ;; index was changed before a long-running query exited
            (unless (string= cur (nth ivy--index ivy--all-candidates))
              (let ((func (ivy-alist-setting ivy-index-functions-alist)))
                (if func
                    (funcall func re ivy--all-candidates)
                  (ivy--preselect-index
                   (if (> (length re) 0)
                       cur
                     (ivy-state-preselect ivy-last))
                   ivy--all-candidates))))))
        (setq ivy--old-cands ivy--all-candidates)
        (if ivy--all-candidates
            (ivy--exhibit)
          (ivy--insert-minibuffer "")))
    (setq counsel--async-last-error-string
          (with-current-buffer (process-buffer process) (buffer-string)))
    (setq ivy--all-candidates
          (let ((status (process-exit-status process))
                (plist (plist-get counsel--async-exit-code-plist
                                  (ivy-state-caller ivy-last))))
            (list (or (plist-get plist status)
                      (format "error code %d" status)))))
    (setq ivy--old-cands ivy--all-candidates)
    (ivy--exhibit)))

(defun counsel--async-sentinel (process _msg)
  "Sentinel function for an asynchronous counsel PROCESS."
  (when (eq (process-status process) 'exit)
    (counsel--sync-sentinel-on-exit process)))

(defcustom counsel-async-filter-update-time 500000
  "The amount of microseconds to wait until updating `counsel--async-filter'."
  :type 'integer)

(defalias 'counsel--async-filter-update-time
  (if (fboundp 'time-convert)
      ;; Preferred (TICKS . HZ) format since Emacs 27.1.
      (lambda () (cons counsel-async-filter-update-time 1000000))
    (lambda () (list 0 0 counsel-async-filter-update-time)))
  "Return `counsel-async-filter-update-time' as a time value.")

(defun counsel--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (counsel--async-filter-update-time)
                     (time-since counsel--async-time))
    (let (numlines)
      (with-current-buffer (process-buffer process)
        (setq numlines (count-lines (point-min) (point-max)))
        (ivy--set-candidates
         (let ((lines (counsel--split-string))
               (ignore-re (ivy-alist-setting counsel-async-ignore-re-alist)))
           (if (stringp ignore-re)
               (cl-delete-if (lambda (line)
                               (string-match-p ignore-re line))
                             lines)
             lines))))
      (let ((ivy--prompt (format "%d++ %s" numlines (ivy-state-prompt ivy-last))))
        (ivy--insert-minibuffer (ivy--format ivy--all-candidates)))
      (setq counsel--async-time (current-time)))))

(defun counsel-delete-process (&optional name)
  "Delete current counsel process or that with NAME."
  (let ((process (get-process (or name " *counsel*"))))
    (when process
      (delete-process process))))

;;* Completion at point
(define-obsolete-function-alias 'counsel-el
  #'complete-symbol "0.13.2 (2020-05-20)")
(define-obsolete-function-alias 'counsel-cl
  #'complete-symbol "0.13.2 (2020-05-20)")
(define-obsolete-function-alias 'counsel-jedi
  #'complete-symbol "0.13.2 (2020-05-20)")
(define-obsolete-function-alias 'counsel-clj
  #'complete-symbol "0.13.2 (2020-05-20)")

;;** `counsel-company'
(defvar company-candidates)
(declare-function company-abort "ext:company")
(declare-function company-complete "ext:company")
(declare-function company-mode "ext:company")
(declare-function company-call-backend "ext:company")
(declare-function company--clean-string "ext:company")
(declare-function company--continue "ext:company")

;;;###autoload
(defun counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (when company-candidates
    (company--continue)
    (ivy-read "Candidate: " company-candidates
              :action 'company-finish
              :caller 'counsel-company)))

(ivy-configure 'counsel-company
  :display-transformer-fn #'counsel--company-display-transformer
  :unwind-fn (lambda() (unless ivy-exit (company-abort))))

(defun counsel--company-display-transformer (s)
  (concat s (let ((annot (company-call-backend 'annotation s)))
              (when annot
                (company--clean-string annot)))))

;;** `counsel-irony'
(declare-function irony-completion-candidates-async "ext:irony-completion")
(declare-function irony-completion-symbol-bounds "ext:irony-completion")
(declare-function irony-completion-annotation "ext:irony-completion")

;;;###autoload
(defun counsel-irony ()
  "Inline C/C++ completion using Irony."
  (interactive)
  (irony-completion-candidates-async 'counsel-irony-callback))

(defun counsel-irony-callback (candidates)
  "Callback function for Irony to search among CANDIDATES."
  (interactive)
  (let* ((symbol-bounds (irony-completion-symbol-bounds))
         (beg (car symbol-bounds))
         (end (cdr symbol-bounds))
         (prefix (buffer-substring-no-properties beg end)))
    (setq ivy-completion-beg beg
          ivy-completion-end end)
    (ivy-read "code: " (mapcar #'counsel-irony-annotate candidates)
              :predicate (lambda (candidate)
                           (string-prefix-p prefix (car candidate)))
              :caller 'counsel-irony
              :action #'ivy-completion-in-region-action)))

(defun counsel-irony-annotate (x)
  "Make Ivy candidate from Irony candidate X."
  (cons (concat (car x) (irony-completion-annotation x))
        (car x)))

(ivy-configure #'counsel-irony
  :display-fn #'ivy-display-function-overlay)

;;* Elisp symbols
;;** `counsel-describe-variable'
(defvar counsel-describe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'counsel-find-symbol)
    (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
    map))

(ivy-set-actions
 'counsel-describe-variable
 '(("I" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(defvar counsel-describe-symbol-history ()
  "History list for variable and function names.
Used by commands `counsel-describe-symbol',
`counsel-describe-variable', and `counsel-describe-function'.")

(defun counsel-find-symbol ()
  "Jump to the definition of the current symbol."
  (interactive)
  (ivy-exit-with-action #'counsel--find-symbol))
(put 'counsel-find-symbol 'no-counsel-M-x t)

(defun counsel--info-lookup-symbol ()
  "Lookup the current symbol in the info docs."
  (interactive)
  (ivy-exit-with-action #'counsel-info-lookup-symbol))

(defvar find-tag-marker-ring)
(declare-function xref-push-marker-stack "xref")

(defalias 'counsel--push-xref-marker
  ;; Added in Emacs 25.1.
  (if (require 'xref nil t)
      #'xref-push-marker-stack
    (require 'etags)
    (lambda (&optional m)
      (ring-insert (with-no-warnings find-tag-marker-ring) (or m (point-marker)))))
  "Compatibility shim for `xref-push-marker-stack'.")

(defun counsel--find-symbol (x)
  "Find symbol definition that corresponds to string X."
  (with-ivy-window
    (counsel--push-xref-marker)
    (let ((full-name (get-text-property 0 'full-name x)))
      (if full-name
          (find-library full-name)
        (let ((sym (read x)))
          (cond ((and (eq (ivy-state-caller ivy-last)
                          'counsel-describe-variable)
                      (boundp sym))
                 (find-variable sym))
                ((fboundp sym)
                 (find-function sym))
                ((boundp sym)
                 (find-variable sym))
                ((or (featurep sym)
                     (locate-library
                      (prin1-to-string sym)))
                 (find-library
                  (prin1-to-string sym)))
                (t
                 (error "Couldn't find definition of %s"
                        sym))))))))

(defun counsel--variable-p (symbol)
  "Return non-nil if SYMBOL is a bound or documented variable."
  (or (and (boundp symbol)
           (not (keywordp symbol)))
      (get symbol 'variable-documentation)))

(defcustom counsel-describe-variable-function #'describe-variable
  "Function to call to describe a variable passed as parameter."
  :type 'function)

(defun counsel-describe-variable-transformer (var)
  "Propertize VAR if it's a custom variable."
  (if (custom-variable-p (intern var))
      (ivy-append-face var 'ivy-highlight-face)
    var))

;;;###autoload
(defun counsel-describe-variable ()
  "Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe variable: " obarray
              :predicate #'counsel--variable-p
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :action (lambda (x)
                        (funcall counsel-describe-variable-function (intern x)))
              :caller 'counsel-describe-variable)))

(ivy-configure 'counsel-describe-variable
  :parent 'counsel-describe-symbol
  :display-transformer-fn #'counsel-describe-variable-transformer)

;;** `counsel-describe-function'
(ivy-set-actions
 'counsel-describe-function
 '(("I" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(defcustom counsel-describe-function-function #'describe-function
  "Function to call to describe a function passed as parameter."
  :type 'function)

(defun counsel-describe-function-transformer (function-name)
  "Propertize FUNCTION-NAME if it's an interactive function."
  (if (commandp (intern function-name))
      (ivy-append-face function-name 'ivy-highlight-face)
    function-name))

(defun ivy-function-called-at-point ()
  (let ((f (function-called-at-point)))
    (and f (symbol-name f))))

(defcustom counsel-describe-function-preselect #'ivy-thing-at-point
  "Determine what `counsel-describe-function' should preselect."
  :type '(radio
          (function-item ivy-thing-at-point)
          (function-item ivy-function-called-at-point)))

;;;###autoload
(defun counsel-describe-function ()
  "Forward to `describe-function'.

Interactive functions (i.e., commands) are highlighted according
to `ivy-highlight-face'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe function: " obarray
              :predicate (lambda (sym)
                           (or (fboundp sym)
                               (get sym 'function-documentation)))
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (funcall counsel-describe-function-preselect)
              :action (lambda (x)
                        (funcall counsel-describe-function-function (intern x)))
              :caller 'counsel-describe-function)))

(ivy-configure 'counsel-describe-function
  :parent 'counsel-describe-symbol
  :display-transformer-fn #'counsel-describe-function-transformer)

;;** `counsel-describe-symbol'
(defcustom counsel-describe-symbol-function #'describe-symbol
  "Function to call to describe a symbol passed as parameter."
  :type 'function)

;;;###autoload
(defun counsel-describe-symbol ()
  "Forward to `describe-symbol'."
  (interactive)
  (unless (functionp 'describe-symbol)
    (user-error "This command requires Emacs 25.1 or later"))
  (require 'help-mode)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe symbol: " obarray
              :predicate (lambda (sym)
                           (cl-some (lambda (backend)
                                      (funcall (cadr backend) sym))
                                    describe-symbol-backends))
              :require-match t
              :history 'counsel-describe-symbol-history
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :action (lambda (x)
                        (funcall counsel-describe-symbol-function (intern x)))
              :caller 'counsel-describe-symbol)))

(ivy-configure 'counsel-describe-symbol
  :initial-input "^"
  :sort-fn #'ivy-string<)

(ivy-set-actions
 'counsel-describe-symbol
 `(("I" ,#'counsel-info-lookup-symbol "info")
   ("d" ,#'counsel--find-symbol "definition")))

;;** `counsel-set-variable'
(defvar counsel-set-variable-history nil
  "Store history for `counsel-set-variable'.")

(defun counsel-read-setq-expression (sym)
  "Read and eval a setq expression for SYM."
  (setq this-command 'eval-expression)
  (let* ((sym-value (symbol-value sym))
         (init (format "(setq %s%S)"
                       (if (or (consp sym-value)
                               (and sym-value (symbolp sym-value)))
                           "'"
                         "")
                       sym-value)))
    ;; Most of this duplicates `read--expression'.
    (minibuffer-with-setup-hook
        (lambda ()
          (set-syntax-table emacs-lisp-mode-syntax-table)
          ;; Added in Emacs 25.1.
          (when (fboundp 'elisp-completion-at-point)
            (add-hook 'completion-at-point-functions
                      #'elisp-completion-at-point nil t))
          ;; Emacs 27+ already sets up ElDoc in this hook.  Emacs 25 added
          ;; `elisp-eldoc-documentation-function' and Emacs 28 obsoletes it.
          (when (< emacs-major-version 27)
            (when (fboundp 'elisp-eldoc-documentation-function)
              (add-function :before-until (local 'eldoc-documentation-function)
                            #'elisp-eldoc-documentation-function))
            (eldoc-mode))
          (run-hooks 'eval-expression-minibuffer-setup-hook)
          ;; The following diverges from `read--expression'.
          (goto-char (minibuffer-prompt-end))
          (forward-char 6)
          (insert (format "%S " sym)))
      (read-from-minibuffer "Eval: " init read-expression-map t
                            'read-expression-history))))

(defun counsel--setq-doconst (x)
  "Return a cons of description and value for X.
X is an item of a radio- or choice-type defcustom."
  (when (listp x)
    (let ((v (car-safe (last x)))
          (tag (and (eq (car x) 'const)
                    (plist-get (cdr x) :tag))))
      (when (and (or v tag) (not (eq v 'function)))
        (cons
         (concat
          (when tag
            (concat tag ": "))
          (if (stringp v) v (prin1-to-string v)))
         (if (symbolp v)
             (list 'quote v)
           v))))))

(declare-function lv-message "ext:lv")
(declare-function lv-delete-window "ext:lv")
(declare-function custom-variable-documentation "cus-edit")

(defface counsel-variable-documentation
  '((t :inherit font-lock-comment-face))
  "Face for displaying Lisp documentation."
  :group 'ivy-faces)

;;;###autoload
(defun counsel-set-variable (sym)
  "Set a variable SYM, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable.

With a prefix arg, restrict list to variables defined using
`defcustom'."
  (interactive (list (intern
                      (ivy-read "Set variable: " obarray
                                :predicate (if current-prefix-arg
                                               #'custom-variable-p
                                             #'counsel--variable-p)
                                :history 'counsel-set-variable-history
                                :preselect (ivy-thing-at-point)))))
  (let ((doc (and (require 'cus-edit)
                  (require 'lv nil t)
                  (not (string= "nil" (custom-variable-documentation sym)))
                  (propertize (custom-variable-documentation sym)
                              'face 'counsel-variable-documentation)))
        sym-type
        cands)
    (unwind-protect
         (progn
           (when doc
             (lv-message (ivy--quote-format-string doc)))
           (if (and (boundp sym)
                    (setq sym-type (get sym 'custom-type))
                    (cond
                      ((and (consp sym-type)
                            (memq (car sym-type) '(choice radio)))
                       (setq cands (delq nil (mapcar #'counsel--setq-doconst
                                                     (cdr sym-type)))))
                      ((eq sym-type 'boolean)
                       (setq cands '(("nil" . nil) ("t" . t))))
                      (t nil)))
               (let* ((sym-val (symbol-value sym))
                      (res (ivy-read (format "Set (%S <%s>): " sym sym-val)
                                     cands
                                     :preselect (prin1-to-string sym-val))))
                 (when res
                   (setq res
                         (if (assoc res cands)
                             (cdr (assoc res cands))
                           (read res)))
                   (kill-new (format "(setq %S %S)" sym res))
                   (set sym (if (and (listp res) (eq (car res) 'quote))
                                (cadr res)
                              res))))
             (unless (boundp sym)
               (set sym nil))
             (let ((expr (counsel-read-setq-expression sym)))
               (kill-new (format "%S" expr))
               (eval-expression expr))))
      (when doc
        (lv-delete-window)))))

;;** `counsel-apropos'
;;;###autoload
(defun counsel-apropos ()
  "Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them."
  (interactive)
  (ivy-read "Search for symbol (word list or regexp): " obarray
            :predicate (lambda (sym)
                         (or (fboundp sym)
                             (boundp sym)
                             (facep sym)
                             (symbol-plist sym)))
            :history 'counsel-apropos-history
            :preselect (ivy-thing-at-point)
            :action (lambda (pattern)
                      (when (string= pattern "")
                        (user-error "Please specify a pattern"))
                      ;; If the user selected a candidate form the list, we use
                      ;; a pattern which matches only the selected symbol.
                      (if (memq this-command '(ivy-immediate-done ivy-alt-done))
                          ;; Regexp pattern are passed verbatim, other input is
                          ;; split into words.
                          (if (string= (regexp-quote pattern) pattern)
                              (apropos (split-string pattern "[ \t]+" t))
                            (apropos pattern))
                        (apropos (concat "\\`" pattern "\\'"))))
            :caller 'counsel-apropos))

(ivy-configure 'counsel-apropos
  :sort-fn #'ivy-string<)

;;** `counsel-info-lookup-symbol'
(defvar info-lookup-mode)
(declare-function info-lookup-guess-default "info-look")
(declare-function info-lookup->completions "info-look")
(declare-function info-lookup->mode-value "info-look")
(declare-function info-lookup-select-mode "info-look")
(declare-function info-lookup-change-mode "info-look")
(declare-function info-lookup "info-look")

;;;###autoload
(defun counsel-info-lookup-symbol (symbol &optional mode)
  "Forward SYMBOL to `info-lookup-symbol' with ivy completion.
With prefix arg MODE a query for the symbol help mode is offered."
  (interactive
   (progn
     (require 'info-look)
     ;; Courtesy of `info-lookup-interactive-arguments'
     (let* ((topic 'symbol)
            (mode (cond (current-prefix-arg
                         (info-lookup-change-mode topic))
                        ((info-lookup->mode-value
                          topic (info-lookup-select-mode))
                         info-lookup-mode)
                        ((info-lookup-change-mode topic))))
            (enable-recursive-minibuffers t))
       (list (ivy-read "Describe symbol: " (info-lookup->completions topic mode)
                       :history 'info-lookup-history
                       :preselect (info-lookup-guess-default topic mode)
                       :caller 'counsel-info-lookup-symbol)
             mode))))
  (info-lookup-symbol symbol mode))

(ivy-configure 'counsel-info-lookup-symbol
  :sort-fn #'ivy-string<)

;;** `counsel-M-x'
(defface counsel-key-binding
  '((t :inherit font-lock-keyword-face))
  "Face used by `counsel-M-x' for key bindings."
  :group 'ivy-faces)

(defface counsel-active-mode
  '((t :inherit font-lock-builtin-face))
  "Face used by `counsel-M-x' for activated modes."
  :group 'ivy-faces)

(defcustom counsel-alias-expand t
  "When non-nil, show the expansion of aliases in `counsel-M-x'."
  :type 'boolean
  :group 'ivy)

(defun counsel-M-x-transformer (cmd)
  "Return CMD annotated with its active key binding, if any."
  (let* ((sym (intern cmd))
         (alias (symbol-function sym))
         (key (where-is-internal sym nil t)))
    (when (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)
               (buffer-local-value sym (ivy-state-buffer ivy-last))))
      (setq cmd (propertize cmd 'face 'counsel-active-mode)))
    (concat cmd
            (when (and (symbolp alias) counsel-alias-expand)
              (format " (%s)" alias))
            (when key
              ;; Prefer `<f2>' over `C-x 6' where applicable
              (let ((i (cl-search [?\C-x ?6] key)))
                (when i
                  (let ((dup (vconcat (substring key 0 i) [f2] (substring key (+ i 2))))
                        (map (current-global-map)))
                    (when (equal (lookup-key map key)
                                 (lookup-key map dup))
                      (setq key dup)))))
              (setq key (key-description key))
              (put-text-property 0 (length key) 'face 'counsel-key-binding key)
              (format " (%s)" key)))))

(defvar amx-initialized)
(defvar amx-cache)
(declare-function amx-initialize "ext:amx")
(declare-function amx-detect-new-commands "ext:amx")
(declare-function amx-update "ext:amx")
(declare-function amx-rank "ext:amx")
(defvar smex-initialized-p)
(defvar smex-ido-cache)
(declare-function smex-initialize "ext:smex")
(declare-function smex-detect-new-commands "ext:smex")
(declare-function smex-update "ext:smex")
(declare-function smex-rank "ext:smex")

(defun counsel--M-x-externs ()
  "Return `counsel-M-x' candidates from external packages.
The return value is a list of strings.  The currently supported
packages are, in order of precedence, `amx' and `smex'."
  (cond ((require 'amx nil t)
         (unless amx-initialized
           (amx-initialize))
         (when (amx-detect-new-commands)
           (amx-update))
         (mapcar (lambda (entry)
                   (symbol-name (car entry)))
                 amx-cache))
        ((require 'smex nil t)
         (unless smex-initialized-p
           (smex-initialize))
         (when (smex-detect-new-commands)
           (smex-update))
         smex-ido-cache)))

(defun counsel--M-x-externs-predicate (cand)
  "Return non-nil if `counsel-M-x' should complete CAND.
CAND is a string returned by `counsel--M-x-externs'."
  (not (get (intern cand) 'no-counsel-M-x)))

(defun counsel--M-x-make-predicate ()
  "Return a predicate for `counsel-M-x' in the current buffer."
  (defvar read-extended-command-predicate)
  (let ((buf (current-buffer)))
    (lambda (sym)
      (and (commandp sym)
           (not (get sym 'byte-obsolete-info))
           (not (get sym 'no-counsel-M-x))
           (cond ((not (bound-and-true-p read-extended-command-predicate)))
                 ((functionp read-extended-command-predicate)
                  (condition-case-unless-debug err
                      (funcall read-extended-command-predicate sym buf)
                    (error (message "read-extended-command-predicate: %s: %s"
                                    sym (error-message-string err))))))))))

(defun counsel--M-x-prompt ()
  "String for `M-x' plus the string representation of `current-prefix-arg'."
  (concat (cond ((null current-prefix-arg)
                 nil)
                ((eq current-prefix-arg '-)
                 "- ")
                ((integerp current-prefix-arg)
                 (format "%d " current-prefix-arg))
                ((= (car current-prefix-arg) 4)
                 "C-u ")
                (t
                 (format "%d " (car current-prefix-arg))))
          "M-x "))

(defvar counsel-M-x-history nil
  "History for `counsel-M-x'.")

(defun counsel-M-x-action (cmd)
  "Execute CMD."
  (setq cmd (intern
             (subst-char-in-string ?\s ?- (string-remove-prefix "^" cmd))))
  (cond ((bound-and-true-p amx-initialized)
         (amx-rank cmd))
        ((bound-and-true-p smex-initialized-p)
         (smex-rank cmd)))
  (setq prefix-arg current-prefix-arg)
  (setq this-command cmd)
  (setq real-this-command cmd)
  (command-execute cmd 'record))

;;;###autoload
(defun counsel-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence."
  (interactive)
  ;; When `counsel-M-x' returns, `last-command' would be set to
  ;; `counsel-M-x' because :action hasn't been invoked yet.
  ;; Instead, preserve the old value of `this-command'.
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (let ((externs (counsel--M-x-externs)))
    (ivy-read (counsel--M-x-prompt) (or externs obarray)
              :predicate (if externs
                             #'counsel--M-x-externs-predicate
                           (counsel--M-x-make-predicate))
              :require-match t
              :history 'counsel-M-x-history
              :action #'counsel-M-x-action
              :keymap counsel-describe-map
              :initial-input initial-input
              :caller 'counsel-M-x)))

(ivy-configure 'counsel-M-x
  :initial-input "^"
  :display-transformer-fn #'counsel-M-x-transformer)

(ivy-set-actions
 'counsel-M-x
 `(("d" counsel--find-symbol "definition")
   ("h" ,(lambda (x) (funcall counsel-describe-function-function (intern x))) "help")))

;;** `counsel-command-history'
(defun counsel-command-history-action-eval (cmd)
  "Eval the command CMD."
  (eval (read cmd) t))

(defun counsel-command-history-action-edit-and-eval (cmd)
  "Edit and eval the command CMD."
  (edit-and-eval-command "Eval: " (read cmd)))

(ivy-set-actions
 'counsel-command-history
 '(("r" counsel-command-history-action-eval           "eval command")
   ("e" counsel-command-history-action-edit-and-eval  "edit and eval command")))

;;;###autoload
(defun counsel-command-history ()
  "Show the history of commands."
  (interactive)
  (ivy-read "Command: " (mapcar #'prin1-to-string command-history)
            :require-match t
            :action #'counsel-command-history-action-eval
            :caller 'counsel-command-history))

;;** `counsel-load-library'
(defun counsel-library-candidates ()
  "Return a list of completion candidates for `counsel-load-library'."
  (let ((suffix (concat (regexp-opt '(".el" ".el.gz") t) "\\'"))
        (cands (make-hash-table :test #'equal))
        short-name
        old-val
        dir-parent
        res)
    (dolist (dir load-path)
      (setq dir (or dir default-directory)) ;; interpret nil in load-path as default-directory
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions "" dir))
          (when (string-match suffix file)
            (unless (string-match "pkg.elc?$" file)
              (setq short-name (substring file 0 (match-beginning 0)))
              (if (setq old-val (gethash short-name cands))
                  (progn
                    ;; assume going up directory once will resolve name clash
                    (setq dir-parent (counsel-directory-name (cdr old-val)))
                    (puthash short-name
                             (cons
                              (counsel-string-compose dir-parent (car old-val))
                              (cdr old-val))
                             cands)
                    (setq dir-parent (counsel-directory-name dir))
                    (puthash (concat dir-parent short-name)
                             (cons
                              (propertize
                               (counsel-string-compose
                                dir-parent short-name)
                               'full-name (expand-file-name file dir))
                              dir)
                             cands))
                (puthash short-name
                         (cons (propertize
                                short-name
                                'full-name (expand-file-name file dir))
                               dir)
                         cands)))))))
    (maphash (lambda (_k v) (push (car v) res)) cands)
    (nreverse res)))

;;;###autoload
(defun counsel-load-library ()
  "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((cands (counsel-library-candidates)))
    (ivy-read "Load library: " cands
              :action (lambda (x)
                        (load-library
                         (get-text-property 0 'full-name x)))
              :keymap counsel-describe-map)))

(ivy-set-actions
 'counsel-load-library
 '(("d" counsel--find-symbol "definition")))

;;** `counsel-find-library'
(declare-function find-library-name "find-func")
(defun counsel-find-library-other-window (library)
  (let ((buf (find-file-noselect (find-library-name library))))
    (pop-to-buffer buf 'other-window)))

(defun counsel-find-library-other-frame (library)
  (let ((buf (find-file-noselect (find-library-name library))))
    (condition-case nil
        (switch-to-buffer-other-frame buf)
      (error (pop-to-buffer buf)))))

(ivy-set-actions
 'counsel-find-library
 '(("j" counsel-find-library-other-window "other window")
   ("f" counsel-find-library-other-frame "other frame")))

;;;###autoload
(defun counsel-find-library ()
  "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((cands (counsel-library-candidates)))
    (ivy-read "Find library: " cands
              :action #'counsel--find-symbol
              :keymap counsel-describe-map
              :caller 'counsel-find-library)))

;;** `counsel-load-theme'
(declare-function powerline-reset "ext:powerline")

(defun counsel-load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t)
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x)))

;;;###autoload
(defun counsel-load-theme ()
  "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar #'symbol-name
                    (custom-available-themes))
            :action #'counsel-load-theme-action
            :caller 'counsel-load-theme))

;;** `counsel-descbinds'
(ivy-set-actions
 'counsel-descbinds
 '(("d" counsel-descbinds-action-find "definition")
   ("I" counsel-descbinds-action-info "info")
   ("x" counsel-descbinds-action-exec "execute")))

(defvar counsel-descbinds-history nil
  "History for `counsel-descbinds'.")

(defun counsel--descbinds-cands (&optional prefix buffer)
  "Get key bindings starting with PREFIX in BUFFER.
See `describe-buffer-bindings' for further information."
  (let ((buffer (or buffer (current-buffer)))
        (re-exclude (regexp-opt
                     '("<vertical-line>" "<bottom-divider>" "<right-divider>"
                       "<mode-line>" "<C-down-mouse-2>" "<left-fringe>"
                       "<right-fringe>" "<header-line>"
                       "<vertical-scroll-bar>" "<horizontal-scroll-bar>")))
        res)
    (with-temp-buffer
      (let ((indent-tabs-mode t))
        (describe-buffer-bindings buffer prefix))
      (goto-char (point-min))
      ;; Skip the "Key translations" section
      (skip-chars-forward "^\C-l")
      (forward-char 2)
      (while (not (eobp))
        (when (looking-at "^\\([^\t\n]+\\)[\t ]*\\(.*\\)$")
          (let ((key (match-string 1))
                (fun (match-string 2))
                cmd)
            (unless (or (member fun '("??" "self-insert-command"))
                        (string-match-p re-exclude key)
                        (not (or (commandp (setq cmd (intern-soft fun)))
                                 (equal fun "Prefix Command"))))
              (push
               (cons (format
                      "%-15s %s"
                      (propertize key 'face 'counsel-key-binding)
                      fun)
                     (cons key cmd))
               res))))
        (forward-line)))
    (nreverse res)))

(defcustom counsel-descbinds-function #'describe-function
  "Function to call to describe a function passed as parameter."
  :type 'function)

(defun counsel-descbinds-action-describe (x)
  "Describe function of candidate X.
See `describe-function' for further information."
  (let ((cmd (cddr x)))
    (funcall counsel-descbinds-function cmd)))

(defun counsel-descbinds-action-exec (x)
  "Run candidate X.
See `execute-extended-command' for further information."
  (let ((cmd (cddr x)))
    (command-execute cmd 'record)))

(defun counsel-descbinds-action-find (x)
  "Find symbol definition of candidate X.
See `counsel--find-symbol' for further information."
  (let ((cmd (cddr x)))
    (counsel--find-symbol (symbol-name cmd))))

(defun counsel-descbinds-action-info (x)
  "Display symbol definition of candidate X, as found in the relevant manual.
See `info-lookup-symbol' for further information."
  (let ((cmd (cddr x)))
    (counsel-info-lookup-symbol (symbol-name cmd))))

;;;###autoload
(defun counsel-descbinds (&optional prefix buffer)
  "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one."
  (interactive)
  (ivy-read "Bindings: " (counsel--descbinds-cands prefix buffer)
            :action #'counsel-descbinds-action-describe
            :history 'counsel-descbinds-history
            :caller 'counsel-descbinds))

;;** `counsel-describe-face'
(defcustom counsel-describe-face-function #'describe-face
  "Function to call to describe a face or face name argument."
  :type 'function)

(defun counsel--face-at-point ()
  "Return name of face around point.
Try detecting a face name in the text around point before falling
back to the face of the character after point, and finally the
`default' face."
  (symbol-name (or (face-at-point t) 'default)))

;;;###autoload
(defun counsel-describe-face ()
  "Completion for `describe-face'."
  (interactive)
  (ivy-read "Face: " (face-list)
            :require-match t
            :history 'face-name-history
            :preselect (counsel--face-at-point)
            :action counsel-describe-face-function
            :caller 'counsel-describe-face))

(ivy-configure 'counsel-describe-face
  :sort-fn #'ivy-string<)

(defun counsel-customize-face (name)
  "Customize face with NAME."
  (customize-face (intern name)))

(defun counsel-customize-face-other-window (name)
  "Customize face with NAME in another window."
  (customize-face-other-window (intern name)))

(declare-function hi-lock-set-pattern "hi-lock")
(defun counsel-highlight-with-face (face)
  "Highlight thing-at-point with FACE."
  (hi-lock-mode 1)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (hi-lock-set-pattern (regexp-quote thing) (intern face))))

(ivy-set-actions
 'counsel-describe-face
 '(("c" counsel-customize-face "customize")
   ("C" counsel-customize-face-other-window "customize other window")))

;;** `counsel-faces'
(defvar counsel--faces-format "%-40s %s")

(defun counsel--faces-format-function (names)
  "Format NAMES according to `counsel--faces-format'."
  (let ((formatter
         (lambda (name)
           (format counsel--faces-format name
                   (propertize list-faces-sample-text
                               'face (intern name))))))
    (ivy--format-function-generic
     (lambda (name)
       (funcall formatter (ivy--add-face name 'ivy-current-match)))
     formatter names "\n")))

;;;###autoload
(defun counsel-faces ()
  "Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face."
  (interactive)
  (let* ((names (mapcar #'symbol-name (face-list)))
         (counsel--faces-format
          (format "%%-%ds %%s"
                  (apply #'max 0 (mapcar #'string-width names)))))
    (ivy-read "Face: " names
              :require-match t
              :history 'face-name-history
              :preselect (counsel--face-at-point)
              :action counsel-describe-face-function
              :caller 'counsel-faces)))

(ivy-configure 'counsel-faces
  :parent 'counsel-describe-face
  :format-fn #'counsel--faces-format-function)

(ivy-set-actions
 'counsel-faces
 '(("c" counsel-customize-face "customize")
   ("C" counsel-customize-face-other-window "customize other window")
   ("h" counsel-highlight-with-face "highlight")))

;;* Git
;;** `counsel-git'
(defvar counsel-git-cmd "git ls-files -z --full-name --"
  "Command for `counsel-git'.")

(ivy-set-actions
 'counsel-git
 '(("j" find-file-other-window "other window")
   ("x" counsel-find-file-extern "open externally")))

(defun counsel--dominating-file (file &optional dir)
  "Look up directory hierarchy for FILE, starting in DIR.
Like `locate-dominating-file', but DIR defaults to
`default-directory' and the return value is expanded."
  (and (setq dir (locate-dominating-file (or dir default-directory) file))
       (expand-file-name dir)))

(defun counsel-locate-git-root ()
  "Return the root of the Git repository containing the current buffer."
  (or (counsel--git-root)
      (error "Not in a Git repository")))

(defun counsel-git-cands (dir)
  (let ((default-directory dir))
    (split-string
     (shell-command-to-string counsel-git-cmd)
     "\0"
     t)))

(defvar counsel-git-history nil
  "History for `counsel-git'.")

;;;###autoload
(defun counsel-git (&optional initial-input)
  "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program counsel-git-cmd)
  (let ((default-directory (counsel-locate-git-root)))
    (ivy-read "Find file: " (counsel-git-cands default-directory)
              :initial-input initial-input
              :action #'counsel-git-action
              :history 'counsel-git-history
              :caller 'counsel-git)))

(ivy-configure 'counsel-git
  :occur #'counsel-git-occur)

(defun counsel-git-action (x)
  "Find file X in current Git repository."
  (with-ivy-window
    (let ((default-directory (ivy-state-directory ivy-last)))
      (find-file x))))

(defun counsel-git-occur (&optional _cands)
  "Occur function for `counsel-git' using `counsel-cmd-to-dired'."
  (cd (ivy-state-directory ivy-last))
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (format "%s | %s | xargs ls"
            (replace-regexp-in-string
             "\\(-0\\)\\|\\(-z\\)" "" counsel-git-cmd t t)
            (counsel--file-name-filter)))))

(defvar counsel-dired-listing-switches "-alh"
  "Switches passed to `ls' for `counsel-cmd-to-dired'.")

(defun counsel-cmd-to-dired (full-cmd &optional filter)
  "Adapted from `find-dired'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dired-mode default-directory counsel-dired-listing-switches)
    (insert "  " default-directory ":\n")
    (let ((point (point)))
      (insert "  " full-cmd "\n")
      (dired-insert-set-properties point (point)))
    (setq-local dired-sort-inhibit t)
    (setq-local revert-buffer-function
                (lambda (_1 _2) (counsel-cmd-to-dired full-cmd)))
    (setq-local dired-subdir-alist
                (list (cons default-directory (point-min-marker))))
    (let ((proc (start-process-shell-command
                 "counsel-cmd" (current-buffer) full-cmd)))
      (set-process-filter proc filter)
      (set-process-sentinel
       proc
       (lambda (process _msg)
         (when (and (eq (process-status process) 'exit)
                    (zerop (process-exit-status process)))
           (goto-char (point-min))
           (forward-line 2)
           (dired-move-to-filename)))))))

;;** `counsel-git-grep'
(defvar counsel-git-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'ivy-call-and-recenter)
    (define-key map (kbd "M-q") #'counsel-git-grep-query-replace)
    (define-key map (kbd "C-c C-m") #'counsel-git-grep-switch-cmd)
    (define-key map (kbd "C-x C-d") #'counsel-cd)
    map))

(defvar counsel-git-grep-cmd-default "git --no-pager grep -n --no-color -I -e \"%s\""
  "Initial command for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd nil
  "Store the command for `counsel-git-grep'.")

(defvar counsel-git-grep-history nil
  "History for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd-history
  (list counsel-git-grep-cmd-default)
  "History for `counsel-git-grep' shell commands.")

(defcustom counsel-grep-post-action-hook nil
  "Hook that runs after the point moves to the next candidate.
A typical example of what to add to this hook is the function
`recenter'."
  :type 'hook
  :options '(recenter))

(defcustom counsel-git-grep-cmd-function #'counsel-git-grep-cmd-function-default
  "How a git-grep shell call is built from the input.
This function should set `ivy--old-re'."
  :type '(radio
          (function-item counsel-git-grep-cmd-function-default)
          (function-item counsel-git-grep-cmd-function-ignore-order)
          (function :tag "Other")))

(defun counsel-git-grep-cmd-function-default (str)
  (format counsel-git-grep-cmd
          (setq ivy--old-re
                (if (eq ivy--regex-function #'ivy--regex-fuzzy)
                    (ivy--string-replace "\n" "" (ivy--regex-fuzzy str))
                  (ivy--regex str t)))))

(defun counsel-git-grep-cmd-function-ignore-order (str)
  (setq ivy--old-re (ivy--regex str t))
  (let ((parts (split-string str " " t)))
    (concat
     "git --no-pager grep --full-name -n --no-color -i -e "
     (mapconcat #'shell-quote-argument parts " --and -e "))))

(defun counsel-git-grep-function (string)
  "Grep in the current Git repository for STRING."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (concat
       (funcall counsel-git-grep-cmd-function string)
       (if (ivy--case-fold-p string) " -i" "")))
     nil)))

(defun counsel-git-grep-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (let ((file-name (match-string-no-properties 1 x))
          (line-number (match-string-no-properties 2 x)))
      (find-file (expand-file-name
                  file-name
                  (ivy-state-directory ivy-last)))
      (goto-char (point-min))
      (forward-line (1- (string-to-number line-number)))
      (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (when swiper-goto-start-of-match
          (goto-char (match-beginning 0))))
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defun counsel-git-grep-transformer (str)
  "Highlight file and line number in STR."
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" str)
    (add-face-text-property (match-beginning 1) (match-end 1)
                            'ivy-grep-info nil str)
    (add-face-text-property (match-beginning 2) (match-end 2)
                            'ivy-grep-line-number nil str))
  str)

(defvar counsel-git-grep-projects-alist nil
  "An alist of project directory to \"git-grep\" command.
Allows to automatically use a custom \"git-grep\" command for all
files in a project.")

(defun counsel--git-grep-cmd-and-proj (cmd)
  (let ((dd (expand-file-name default-directory))
        proj)
    (cond
      ((stringp cmd))
      (current-prefix-arg
       (if (setq proj
                 (cl-find-if
                  (lambda (x)
                    (string-match-p (car x) dd))
                  counsel-git-grep-projects-alist))
           (setq cmd (cdr proj))
         (setq cmd
               (ivy-read "cmd: " counsel-git-grep-cmd-history
                         :history 'counsel-git-grep-cmd-history
                         :re-builder #'ivy--regex))
         (setq counsel-git-grep-cmd-history
               (delete-dups counsel-git-grep-cmd-history))))
      (t
       (setq cmd counsel-git-grep-cmd-default)))
    (cons proj cmd)))

(defun counsel--call (command &optional result-fn)
  "Synchronously call COMMAND and return its output as a string.
COMMAND comprises the program name followed by its arguments, as
in `make-process'.  Signal `file-error' and emit a warning if
COMMAND fails.  Obey file handlers based on `default-directory'.
On success, RESULT-FN is called in output buffer with no arguments."
  (let ((stderr (make-temp-file "counsel-call-stderr-"))
        status)
    (unwind-protect
         (with-temp-buffer
           (setq status (apply #'process-file (car command) nil
                               (list t stderr) nil (cdr command)))
           (if (eq status 0)
               (if result-fn
                   (funcall result-fn)
                 ;; Return all output except trailing newline.
                 (buffer-substring (point-min)
                                   (- (point)
                                      (if (eq (bobp) (bolp))
                                          0
                                        1))))
             ;; Convert process status into error list.
             (setq status (list 'file-error
                                (mapconcat #'identity `(,@command "failed") " ")
                                status))
             ;; Print stderr contents, if any, to *Warnings* buffer.
             (let ((msg (condition-case err
                            (unless (zerop (cadr (insert-file-contents
                                                  stderr nil nil nil t)))
                              (buffer-string))
                          (error (error-message-string err)))))
               (lwarn 'ivy :warning "%s" (apply #'concat
                                                (error-message-string status)
                                                (and msg (list "\n" msg)))))
             ;; Signal `file-error' with process status.
             (signal (car status) (cdr status))))
      (delete-file stderr))))

(defun counsel--command (&rest command)
  "Forward COMMAND to `counsel--call'."
  (counsel--call command))

(defun counsel--grep-unwind ()
  (counsel-delete-process)
  (swiper--cleanup))

;;;###autoload
(defun counsel-git-grep (&optional initial-input initial-directory cmd)
  "Grep for a string in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command."
  (interactive)
  (let ((proj-and-cmd (counsel--git-grep-cmd-and-proj cmd))
        proj)
    (setq proj (car proj-and-cmd))
    (setq counsel-git-grep-cmd (cdr proj-and-cmd))
    (counsel-require-program counsel-git-grep-cmd)
    (let ((collection-function
           (if proj
               #'counsel-git-grep-proj-function
             #'counsel-git-grep-function))
          (default-directory (or initial-directory
                                 (if proj
                                     (car proj)
                                   (counsel-locate-git-root)))))
      (ivy-read "git grep: " collection-function
                :initial-input initial-input
                :dynamic-collection t
                :keymap counsel-git-grep-map
                :action #'counsel-git-grep-action
                :history 'counsel-git-grep-history
                :require-match t
                :caller 'counsel-git-grep))))

(defun counsel--git-grep-index (_re-str cands)
  (let (name ln)
    (cond
      (ivy--old-cands
       (ivy-recompute-index-swiper-async nil cands))
      ((unless (with-ivy-window
                 (when buffer-file-name
                   (setq ln (line-number-at-pos))
                   (setq name (file-name-nondirectory buffer-file-name))))
         0))
      ;; Closest to current line going forwards.
      ((let ((beg (1+ (length name))))
         (cl-position-if (lambda (x)
                           (and (string-prefix-p name x)
                                (>= (string-to-number (substring x beg)) ln)))
                         cands)))
      ;; Closest to current line going backwards.
      ((cl-position-if (lambda (x)
                         (string-prefix-p name x))
                       cands
                       :from-end t))
      (t 0))))

(ivy-configure 'counsel-git-grep
  :occur #'counsel-git-grep-occur
  :unwind-fn #'counsel--grep-unwind
  :index-fn #'counsel--git-grep-index
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

(defun counsel-git-grep-proj-function (str)
  "Grep for STR in the current Git repository."
  (or
   (ivy-more-chars)
   (let ((regex (setq ivy--old-re
                      (ivy--regex str t))))
     (counsel--async-command
      (concat
       (format counsel-git-grep-cmd regex)
       (if (ivy--case-fold-p str) " -i" "")))
     nil)))

(defun counsel-git-grep-switch-cmd ()
  "Set `counsel-git-grep-cmd' to a different value."
  (interactive)
  (setq counsel-git-grep-cmd
        (ivy-read "cmd: " counsel-git-grep-cmd-history
                  :history 'counsel-git-grep-cmd-history))
  (setq counsel-git-grep-cmd-history
        (delete-dups counsel-git-grep-cmd-history))
  (unless (ivy-state-dynamic-collection ivy-last)
    (setq ivy--all-candidates
          (all-completions "" #'counsel-git-grep-function))))

(defun counsel--normalize-grep-match (str)
  ;; Prepend ./ if necessary:
  (unless (ivy--starts-with-dotslash str)
    (setq str (concat "./" str)))
  ;; Remove column info if any:
  (save-match-data
    (when (string-match
           "[^\n:]+?[^\n/:]:[\t ]*[1-9][0-9]*[\t ]*:\\([1-9][0-9]*:\\)"
           str)
      (setq str (replace-match "" t t str 1))))
  str)

(defun counsel--git-grep-occur-cmd (input)
  (let* ((regex ivy--old-re)
         (positive-pattern ;; git-grep can't handle .*?
          (ivy--string-replace ".*?" ".*" (ivy-re-to-str regex)))
         (negative-patterns
          (if (stringp regex) ""
            (mapconcat (lambda (x)
                         (and (null (cdr x))
                              (format "| grep -v %s" (car x))))
                       regex
                       " "))))
    (concat
     (format counsel-git-grep-cmd positive-pattern)
     negative-patterns
     (if (ivy--case-fold-p input) " -i" ""))))

(defun counsel-git-grep-occur (&optional _cands)
  "Generate a custom occur buffer for `counsel-git-grep'."
  (counsel-grep-like-occur #'counsel--git-grep-occur-cmd))

(defun counsel-git-grep-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error
     "Should only be called in the minibuffer through `counsel-git-grep-map'"))
  (let* ((enable-recursive-minibuffers t)
         (from (ivy--regex ivy-text))
         (to (query-replace-read-to from "Query replace" t)))
    (ivy-exit-with-action
     (lambda (_)
       (let (done-buffers)
         (dolist (cand ivy--old-cands)
           (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
             (with-ivy-window
               (let ((file-name (match-string-no-properties 1 cand)))
                 (setq file-name (expand-file-name
                                  file-name
                                  (ivy-state-directory ivy-last)))
                 (unless (member file-name done-buffers)
                   (push file-name done-buffers)
                   (find-file file-name)
                   (goto-char (point-min)))
                 (perform-replace from to t t nil))))))))))

;;** `counsel-git-stash'
(defun counsel-git-stash-kill-action (x)
  "Add git stash command to kill ring.
The git command applies the stash entry where candidate X was found in."
  (when (string-match "\\([^:]+\\):" x)
    (kill-new (message (format "git stash apply %s" (match-string 1 x))))))

;;;###autoload
(defun counsel-git-stash ()
  "Search through all available git stashes."
  (interactive)
  (let* ((default-directory (counsel-locate-git-root))
         (cands (split-string (shell-command-to-string
                               "IFS=$'\n'
for i in `git stash list --format=\"%gd\"`; do
    git stash show -p $i | grep -H --label=\"$i\" \"$1\"
done") "\n" t)))
    (ivy-read "git stash: " cands
              :action #'counsel-git-stash-kill-action
              :caller 'counsel-git-stash)))

;;** `counsel-git-log'
(defvar counsel-git-log-cmd "GIT_PAGER=cat git log --no-color --grep '%s'"
  "Command used for \"git log\".")

(defun counsel-git-log-function (_)
  "Search for `ivy-regex' in git log."
  (or
   (ivy-more-chars)
   (progn
     ;; `counsel--yank-pop-format-function' uses this
     (setq ivy--old-re ivy-regex)
     (counsel--async-command
      ;; "git log --grep" likes to have groups quoted e.g. \(foo\).
      ;; But it doesn't like the non-greedy ".*?".
      (format counsel-git-log-cmd
              (ivy--string-replace ".*?" ".*" (ivy-re-to-str ivy--old-re))))
     nil)))

(defun counsel-git-log-action (x)
  "Add candidate X to kill ring."
  (message "%S" (kill-new x)))

(declare-function magit-show-commit "ext:magit-diff")

(defun counsel-git-log-show-commit-action (log-entry)
  "Visit the commit corresponding to LOG-ENTRY."
  (require 'magit-diff)
  (let ((commit (substring-no-properties log-entry 0 (string-match-p "\\W" log-entry))))
    (magit-show-commit commit)))

(ivy-set-actions
 'counsel-git-log
 '(("v" counsel-git-log-show-commit-action "visit commit")))

;;** `counsel-git-change-worktree'
(defun counsel-git-change-worktree-action (git-root-dir tree)
  "Find the corresponding file in the worktree located at tree.
The current buffer is assumed to be in a subdirectory of GIT-ROOT-DIR.
TREE is the selected candidate."
  (let* ((new-root-dir (counsel-git-worktree-parse-root tree))
         (tree-filename (file-relative-name buffer-file-name git-root-dir))
         (file-name (expand-file-name tree-filename new-root-dir)))
    (find-file file-name)))

(defun counsel-git-worktree-list ()
  "List worktrees in the Git repository containing the current buffer."
  (let ((default-directory (counsel-locate-git-root)))
    (split-string (shell-command-to-string "git worktree list") "\n" t)))

(defun counsel-git-worktree-parse-root (tree)
  "Return worktree from candidate TREE."
  (substring tree 0 (ivy--string-search " " tree)))

(defun counsel-git-close-worktree-files-action (root-dir)
  "Close all buffers from the worktree located at ROOT-DIR."
  (setq root-dir (counsel-git-worktree-parse-root root-dir))
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (and buffer-file-name
           (string= "." (file-relative-name root-dir (counsel-locate-git-root)))
           (kill-buffer buf)))))

(ivy-set-actions
 'counsel-git-change-worktree
 '(("k" counsel-git-close-worktree-files-action "kill all")))

;;;###autoload
(defun counsel-git-change-worktree ()
  "Find the file corresponding to the current buffer on a different worktree."
  (interactive)
  (let ((default-directory (counsel-locate-git-root)))
    (ivy-read "Select worktree: "
              (or (cl-delete default-directory (counsel-git-worktree-list)
                             :key #'counsel-git-worktree-parse-root :test #'string=)
                  (error "No other worktrees"))
              :action (lambda (tree)
                        (counsel-git-change-worktree-action
                         (ivy-state-directory ivy-last) tree))
              :require-match t
              :caller 'counsel-git-change-worktree)))

;;** `counsel-git-checkout'
(defun counsel-git-checkout-action (branch)
  "Switch branch by invoking git-checkout(1).
The command is passed a single argument comprising all characters
in BRANCH up to, but not including, the first space
character (#x20), or the string's end if it lacks a space."
  (shell-command
   (format "git checkout %s"
           (shell-quote-argument
            (substring branch 0 (ivy--string-search " " branch))))))

(defun counsel-git-branch-list ()
  "Return list of branches in the current Git repository.
Value comprises all local and remote branches bar the one
currently checked out."
  (cl-mapcan (lambda (line)
               (and (string-match "\\`[[:blank:]]+" line)
                    (list (substring line (match-end 0)))))
             (let ((default-directory (counsel-locate-git-root)))
               (split-string (shell-command-to-string
                              "git branch -vv --all --no-color")
                             "\n" t))))

;;;###autoload
(defun counsel-git-checkout ()
  "Call the \"git checkout\" command."
  (interactive)
  (ivy-read "Checkout branch: " (counsel-git-branch-list)
            :action #'counsel-git-checkout-action
            :caller 'counsel-git-checkout))

(defvar counsel-yank-pop-truncate-radius)

(defun counsel--git-log-format-function (str)
  (let ((counsel-yank-pop-truncate-radius 5))
    (counsel--yank-pop-format-function str)))

;;;###autoload
(defun counsel-git-log ()
  "Call the \"git log --grep\" shell command."
  (interactive)
  (ivy-read "Grep log: " #'counsel-git-log-function
            :dynamic-collection t
            :action #'counsel-git-log-action
            :caller 'counsel-git-log))

(ivy-configure 'counsel-git-log
  :height 4
  :unwind-fn #'counsel-delete-process
  :format-fn #'counsel--git-log-format-function)

(add-to-list 'counsel-async-split-string-re-alist '(counsel-git-log . "^commit "))
(add-to-list 'counsel-async-ignore-re-alist '(counsel-git-log . "^[ \n]*$"))

;;* File
;;** `counsel-find-file'
(defvar counsel-find-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-DEL") #'counsel-up-directory)
    (define-key map (kbd "C-<backspace>") #'counsel-up-directory)
    (define-key map (kbd "`") #'counsel-file-jump-from-find)
    (define-key map (kbd "C-`") (ivy-make-magic-action #'counsel-find-file "b"))
    (define-key map [remap undo] #'counsel-find-file-undo)
    map))

(defun counsel-file-jump-from-find ()
  "Switch to `counsel-file-jump' from `counsel-find-file'."
  (interactive)
  (ivy-quit-and-run
    (counsel-file-jump ivy-text (ivy-state-directory ivy-last))))

(when (executable-find "git")
  (add-to-list 'ivy-ffap-url-functions 'counsel-github-url-p)
  (add-to-list 'ivy-ffap-url-functions 'counsel-emacs-url-p))
(add-to-list 'ivy-ffap-url-functions 'counsel-url-expand)
(defun counsel-find-file-cd-bookmark-action (_)
  "Reset `counsel-find-file' from selected directory."
  (ivy-read "cd: "
            (progn
              (ivy--virtual-buffers)
              (delete-dups
               (mapcar (lambda (x) (file-name-directory (cdr x)))
                       ivy--virtual-buffers)))
            :action (lambda (x)
                      (let ((default-directory (file-name-directory x)))
                        (counsel-find-file)))))

(defcustom counsel-root-command "sudo"
  "Command to gain root privileges."
  :type 'string)

(defun counsel-find-file-as-root (x)
  "Find file X with root privileges."
  (counsel-require-program counsel-root-command)
  (let* ((host (file-remote-p x 'host))
         (file-name (format "/%s:%s:%s"
                            counsel-root-command
                            (or host "")
                            (expand-file-name
                             (if host
                                 (file-remote-p x 'localname)
                               x)))))
    ;; If the current buffer visits the same file we are about to open,
    ;; replace the current buffer with the new one.
    (if (eq (current-buffer) (get-file-buffer x))
        (find-alternate-file file-name)
      (find-file file-name))))

(defun counsel--yes-or-no-p (fmt &rest args)
  "Ask user a yes or no question created using FMT and ARGS.
If Emacs 26 user option `read-answer-short' is bound, use it to
choose between `yes-or-no-p' and `y-or-n-p'; otherwise default to
`yes-or-no-p'."
  (funcall (if (and (boundp 'read-answer-short)
                    (cond ((eq read-answer-short t))
                          ((eq read-answer-short 'auto)
                           (eq (symbol-function 'yes-or-no-p) 'y-or-n-p))))
               #'y-or-n-p
             #'yes-or-no-p)
           (apply #'format fmt args)))

(defun counsel-find-file-copy (x)
  "Copy file X."
  (require 'dired-aux)
  (counsel--find-file-1 "Copy file to: "
                        ivy--directory
                        (lambda (new-name)
                          (dired-copy-file x new-name 1))
                        'counsel-find-file-copy))

(defun counsel-find-file-delete (x)
  "Delete file X."
  (when (or delete-by-moving-to-trash
            ;; `dired-delete-file', which see, already prompts for directories
            (eq t (car (file-attributes x)))
            (counsel--yes-or-no-p "Delete %s? " x))
    (dired-delete-file x dired-recursive-deletes delete-by-moving-to-trash)
    (dired-clean-up-after-deletion x)
    (let ((win (and (not (eq ivy-exit 'done))
                    (active-minibuffer-window))))
      (when win (with-selected-window win (ivy--cd ivy--directory))))))

(defun counsel-find-file-move (x)
  "Move or rename file X."
  (require 'dired-aux)
  (counsel--find-file-1 "Rename file to: "
                        ivy--directory
                        (lambda (new-name)
                          (dired-rename-file x new-name 1))
                        'counsel-find-file-move))

(defun counsel-find-file-mkdir-action (_x)
  "Create a directory and any nonexistent parent dirs from `ivy-text'."
  (let ((dir (file-name-as-directory
              (expand-file-name ivy-text ivy--directory)))
        (win (and (not (eq ivy-exit 'done))
                  (active-minibuffer-window))))
    (make-directory dir t)
    (when win (with-selected-window win (ivy--cd dir)))))

(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-window "other window")
   ("f" find-file-other-frame "other frame")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("r" counsel-find-file-as-root "open as root")
   ("R" find-file-read-only "read only")
   ("l" find-file-literally "open literally")
   ("k" counsel-find-file-delete "delete")
   ("c" counsel-find-file-copy "copy file")
   ("m" counsel-find-file-move "move or rename")
   ("d" counsel-find-file-mkdir-action "mkdir")))

(defcustom counsel-find-file-at-point nil
  "When non-nil, add file-at-point to the list of candidates."
  :type 'boolean)

(defcustom counsel-preselect-current-file nil
  "When non-nil, preselect current file in list of candidates."
  :type 'boolean)

(defcustom counsel-find-file-ignore-regexp nil
  "A regexp of files to ignore while in `counsel-find-file'.
These files are un-ignored if `ivy-text' matches them.  The
common way to show all files is to start `ivy-text' with a dot.

Example value: \"\\\\=`[#.]\\|[#~]\\\\='\".
This will hide temporary and lock files.
\\<ivy-minibuffer-map>
Choosing the dotfiles option, \"\\\\=`\\.\", might be convenient,
since you can still access the dotfiles if your input starts with
a dot.  The generic way to toggle ignored files is \\[ivy-toggle-ignore],
but the leading dot is a lot faster."
  :type `(choice
          (const :tag "None" nil)
          (const :tag "Dotfiles and Lockfiles" "\\(?:\\`\\|[/\\]\\)\\(?:[#.]\\)")
          (const :tag "Ignored Extensions"
                 ,(concat (regexp-opt completion-ignored-extensions) "\\'"))
          (regexp :tag "Regex")))

(defvar counsel--find-file-predicate nil
  "When non-nil, `counsel--find-file-matcher' will use this predicate.")

(defun counsel--find-file-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip some dotfiles unless `ivy-text' requires them."
  (let ((res
         (ivy--re-filter
          regexp candidates
          (lambda (re-str)
            (lambda (x)
              (string-match re-str (directory-file-name x)))))))
    (when counsel--find-file-predicate
      (let ((default-directory ivy--directory))
        (setq res (cl-remove-if-not counsel--find-file-predicate res))))
    (if (or (null ivy-use-ignore)
            (null counsel-find-file-ignore-regexp)
            (string-match-p counsel-find-file-ignore-regexp ivy-text))
        res
      (or (cl-remove-if
           (lambda (x)
             (and
              (string-match-p counsel-find-file-ignore-regexp x)
              (not (member x ivy-extra-directories))))
           res)
          res))))

(declare-function ffap-guesser "ffap")

(defvar counsel-find-file-speedup-remote t
  "Speed up opening remote files by disabling `find-file-hook' for them.")

(defcustom counsel-find-file-extern-extensions '("mp4" "mkv" "xlsx")
  "List of extensions that make `counsel-find-file' use `counsel-find-file-extern'."
  :type '(repeat string))

(defun counsel-find-file-action (x)
  "Find file X."
  (cond ((and counsel-find-file-speedup-remote
              (file-remote-p ivy--directory))
         (let ((find-file-hook nil))
           (find-file (expand-file-name x ivy--directory))))
        ((member (file-name-extension x) counsel-find-file-extern-extensions)
         (counsel-find-file-extern x))
        (t
         (find-file (expand-file-name x ivy--directory)))))

(defun counsel--preselect-file ()
  "Return candidate to preselect during filename completion.
The preselect behavior can be customized via user options
`counsel-find-file-at-point' and
`counsel-preselect-current-file', which see."
  (or
   (when counsel-find-file-at-point
     (require 'ffap)
     (let ((f (ffap-guesser)))
       (when (and f (not (ivy-ffap-url-p f)))
         (expand-file-name f))))
   (and counsel-preselect-current-file
        buffer-file-name
        (file-name-nondirectory buffer-file-name))))

(defun counsel--find-file-1 (prompt initial-input action caller)
  (let ((default-directory
         (if (eq major-mode 'dired-mode)
             (dired-current-directory)
           default-directory)))
    (ivy-read prompt #'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action action
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller caller)))

;;;###autoload
(defun counsel-find-file (&optional initial-input initial-directory)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (defvar tramp-archive-enabled)
  (let ((tramp-archive-enabled nil)
        (default-directory (or initial-directory default-directory)))
    (counsel--find-file-1 "Find file: " initial-input
                          #'counsel-find-file-action
                          'counsel-find-file)))

(ivy-configure 'counsel-find-file
  :parent 'read-file-name-internal
  :occur #'counsel-find-file-occur)

(defvar counsel-find-file-occur-cmd "ls -a | %s | xargs -d '\\n' ls -d --group-directories-first"
  "Format string for `counsel-find-file-occur'.")

(defvar counsel-find-file-occur-use-find (not (eq system-type 'gnu/linux))
  "When non-nil, `counsel-find-file-occur' will use \"find\" as the base cmd.")

(defun counsel--expand-ls (cmd)
  "Expand CMD that ends in \"ls\" with switches."
  (concat cmd " " counsel-dired-listing-switches " | sed -e \"s/^/  /\""))

(defvar counsel-file-name-filter-alist
  '(("ag -i '%s'" . t)
    ("ack -i '%s'" . t)
    ("perl -ne '/(.*%s.*)/i && print \"$1\\n\";'" . t)
    ("grep -i -E '%s'"))
  "Alist of file name filtering commands.
The car is a shell command and the cdr is t when the shell
command supports look-arounds.  The executable for the commands
will be checked for existence via `executable-find'.  The first
one that exists will be used.")

(defun counsel--file-name-filter (&optional use-ignore)
  "Return a command that filters a file list to match ivy candidates.
If USE-IGNORE is non-nil, try to generate a command that respects
`counsel-find-file-ignore-regexp'."
  (let ((regex ivy--old-re))
    (if (= 0 (length regex))
        "cat"
      (let ((filter-cmd (cl-find-if
                         (lambda (x)
                           (executable-find
                            (car (split-string (car x)))))
                         counsel-file-name-filter-alist))
            cmd)
        (when (and use-ignore ivy-use-ignore
                   counsel-find-file-ignore-regexp
                   (cdr filter-cmd)
                   (not (string-match-p counsel-find-file-ignore-regexp ivy-text))
                   (not (string-match-p counsel-find-file-ignore-regexp
                                        (or (car ivy--old-cands) ""))))
          (let ((ignore-re (list (counsel--elisp-to-pcre
                                  counsel-find-file-ignore-regexp))))
            (setq regex (if (stringp regex)
                            (list ignore-re (cons regex t))
                          (cons ignore-re regex)))))
        (setq cmd (format (car filter-cmd)
                          (counsel--elisp-to-pcre regex (cdr filter-cmd))))
        (if (string-suffix-p "csh" shell-file-name)
            (ivy--string-replace "?!" "?\\!" cmd)
          cmd)))))

(defun counsel--occur-cmd-find ()
  (let ((cmd (format
              "find . -maxdepth 1 | %s | xargs -I {} find {} -maxdepth 0 -ls"
              (counsel--file-name-filter t))))
    (concat
     (counsel--cmd-to-dired-by-type "d" cmd)
     " && "
     (counsel--cmd-to-dired-by-type "f" cmd))))

(defun counsel--cmd-to-dired-by-type (type cmd)
  (let ((exclude-dots
         (unless (string-prefix-p "." ivy-text)
           " | grep -v '/\\.'")))
    (ivy--string-replace
     " | grep"
     (concat " -type " type exclude-dots " | grep") cmd)))

(defun counsel-find-file-occur (&optional _cands)
  (require 'find-dired)
  (cd ivy--directory)
  (if counsel-find-file-occur-use-find
      (counsel-cmd-to-dired
       (counsel--occur-cmd-find)
       'find-dired-filter)
    (counsel-cmd-to-dired
     (counsel--expand-ls
      (format counsel-find-file-occur-cmd
              (if (ivy--string-search "grep" counsel-find-file-occur-cmd)
                  ;; for backwards compatibility
                  (counsel--elisp-to-pcre ivy--old-re)
                (counsel--file-name-filter t)))))))

(defvar counsel-up-directory-level t
  "Control whether `counsel-up-directory' goes up a level or always a directory.

If non-nil, then `counsel-up-directory' will remove the final level of the path.
For example: /a/long/path/file.jpg => /a/long/path/
             /a/long/path/     =>     /a/long/

If nil, then `counsel-up-directory' will go up a directory.
For example: /a/long/path/file.jpg => /a/long/
             /a/long/path/     =>     /a/long/")

(defun counsel-up-directory ()
  "Go to the parent directory preselecting the current one.

If the current directory is remote and it's not possible to go up any
further, make the remote prefix editable.

See variable `counsel-up-directory-level'."
  (interactive)
  (let* ((cur-dir (directory-file-name (expand-file-name ivy--directory)))
         (up-dir (file-name-directory cur-dir)))
    (if (and (file-remote-p cur-dir) (string-equal cur-dir up-dir))
        (progn
          ;; make the remote prefix editable
          (setq ivy--old-cands nil)
          (setq ivy--old-re nil)
          (ivy-set-index 0)
          (setq ivy--directory "")
          (setq ivy--all-candidates nil)
          (ivy-set-text "")
          (delete-minibuffer-contents)
          (insert up-dir))
      (if (and counsel-up-directory-level (not (string= ivy-text "")))
          (delete-region (line-beginning-position) (line-end-position))
        (ivy--cd up-dir)
        (setf (ivy-state-preselect ivy-last)
              (file-name-as-directory (file-name-nondirectory cur-dir)))))))

(defun counsel-down-directory ()
  "Descend into the current directory."
  (interactive)
  (ivy--directory-enter))

(defun counsel-find-file-undo ()
  (interactive)
  (if (string= ivy-text "")
      (let ((dir (progn
                   (pop ivy--directory-hist)
                   (pop ivy--directory-hist))))
        (when dir
          (ivy--cd dir)))
    (undo)))

(defun counsel-at-git-issue-p ()
  "When point is at an issue in a Git-versioned file, return the issue string."
  (and (looking-at "#[0-9]+")
       (save-match-data
         (or (eq (vc-backend buffer-file-name) 'Git)
             (memq major-mode '(magit-commit-mode vc-git-log-view-mode))
             (bound-and-true-p magit-commit-mode)))
       (match-string-no-properties 0)))

(defun counsel-github-url-p ()
  "Return a Github issue URL at point."
  (when (counsel-require-program "git" t)
    (let ((url (counsel-at-git-issue-p)))
      (when url
        (let ((origin (shell-command-to-string
                       "git remote get-url origin"))
              user repo)
          (cond ((string-match "\\`git@github.com:\\([^/]+\\)/\\(.*\\)\\.git$"
                               origin)
                 (setq user (match-string 1 origin))
                 (setq repo (match-string 2 origin)))
                ((string-match "\\`https://github.com/\\([^/]+\\)/\\(.*\\)$"
                               origin)
                 (setq user (match-string 1 origin))
                 (setq repo (match-string 2 origin))))
          (when user
            (setq url (format "https://github.com/%s/%s/issues/%s"
                              user repo (substring url 1)))))))))

(defun counsel-emacs-url-p ()
  "Return a Debbugs issue URL at point."
  (let ((url (and (counsel-require-program "git" t)
                  (counsel-at-git-issue-p))))
    (when url
      (let ((origin (shell-command-to-string "git remote get-url origin")))
        (when (string-match-p "git.sv.gnu.org:/srv/git/emacs.git" origin)
          (format "https://bugs.gnu.org/%s" (substring url 1)))))))

(defvar counsel-url-expansions-alist nil
  "Map of regular expressions to expansions.

The value of this variable is a list of pairs (REGEXP . FORMAT).

`counsel-url-expand' expands the word at point according to
FORMAT for the first matching REGEXP.  FORMAT can be either a
string or a function.  If it is a string, it is used as the
format string for the function `format', with the word at point
as the next argument.  If it is a function, it is called with the
word at point as the sole argument.

For example, a pair of the form:
  \\='(\"\\\\\\=`BSERV-[[:digit:]]+\\\\\\='\" .
    \"https://jira.atlassian.com/browse/%s\")
expands to the URL `https://jira.atlassian.com/browse/BSERV-100'
when the word at point is \"BSERV-100\".

If FORMAT is a function, more powerful transformations are
possible.  As an example,
  \\='(\"\\\\\\=`issue\\\\([[:digit:]]+\\\\)\\\\\\='\" .
    (lambda (word)
      (concat \"https://bugs.gnu.org/\" (match-string 1 word))))
trims the \"issue\" prefix from the word at point before creating
the URL.")

(defun counsel-url-expand ()
  "Expand word at point using `counsel-url-expansions-alist'.
The first pair in the list whose regexp matches the word at point
will be expanded according to its format.  This function is
intended to be used in `ivy-ffap-url-functions' to browse the
result as a URL."
  (let ((word-at-point (current-word)))
    (when word-at-point
      (cl-some
       (lambda (pair)
         (let ((regexp (car pair))
               (formatter (cdr pair)))
           (when (string-match regexp word-at-point)
             (if (functionp formatter)
                 (funcall formatter word-at-point)
               (format formatter word-at-point)))))
       counsel-url-expansions-alist))))

;;** `counsel-dired'
(declare-function dired "dired")

;;;###autoload
(defun counsel-dired (&optional initial-input)
  "Forward to `dired'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (let ((counsel--find-file-predicate #'file-directory-p))
    (counsel--find-file-1
     "Dired (directory): " initial-input
     (lambda (d) (dired (expand-file-name d)))
     'counsel-dired)))

(ivy-configure 'counsel-dired
  :parent 'read-file-name-internal)

;;** `counsel-recentf'
(defvar recentf-list)
(declare-function recentf-mode "recentf")

(defcustom counsel-recentf-include-xdg-list nil
  "Include recently used files listed by XDG-compliant environments.
Examples of such environments are GNOME and KDE.  See the URL
`https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec'."
  :type 'boolean
  :link '(url-link "\
https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec"))

;;;###autoload
(defun counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: " (counsel-recentf-candidates)
            :action (lambda (f)
                      (with-ivy-window
                        (find-file f)))
            :require-match t
            :caller 'counsel-recentf))

(ivy-set-actions
 'counsel-recentf
 `(("j" find-file-other-window "other window")
   ("f" find-file-other-frame "other frame")
   ("x" counsel-find-file-extern "open externally")
   ("d" ,(lambda (file) (setq recentf-list (delete file recentf-list)))
    "delete from recentf")))

(defun counsel-recentf-candidates ()
  "Return candidates for `counsel-recentf'.

When `counsel-recentf-include-xdg-list' is non-nil, also include
the files in said list, sorting the combined list by file access
time."
  (if (and counsel-recentf-include-xdg-list
           (>= emacs-major-version 26))
      (delete-dups
       (sort (nconc (mapcar #'substring-no-properties recentf-list)
                    (counsel--recentf-get-xdg-recent-files))
             (lambda (file1 file2)
               (cond ((file-remote-p file1)
                      nil)
                     ((file-remote-p file2))
                     (t
                      ;; Added in Emacs 26.1.
                      (declare-function file-attribute-access-time "files"
                                        (attributes))
                      (time-less-p (file-attribute-access-time
                                    (file-attributes file2))
                                   (file-attribute-access-time
                                    (file-attributes file1))))))))
    (mapcar #'substring-no-properties recentf-list)))

(defalias 'counsel--xml-parse-region
  (if (cond ((fboundp 'libxml-available-p)
             ;; Added in Emacs 27.1.
             (libxml-available-p))
            ((fboundp 'libxml-parse-xml-region)
             ;; Checking for `fboundp' is not enough on Windows, where it
             ;; will return non-nil even if the library is not installed.
             (with-temp-buffer
               (insert "<xml/>")
               (libxml-parse-xml-region (point-min) (point-max)))))
      (lambda (&optional beg end)
        (libxml-parse-xml-region (or beg (point-min)) (or end (point-max))))
    #'xml-parse-region)
  "Compatibility shim for `libxml-parse-xml-region'.
For convenience, BEG and END default to `point-min' and
`point-max', respectively.

\(fn &optional BEG END)")

(defun counsel--recentf-get-xdg-recent-files ()
  "Return list of XDG recent files.

This information is parsed from the file \"recently-used.xbel\",
which lists both files and directories, under `xdg-data-home'.
This function uses the `dom' library from Emacs 25.1 or later."
  (unless (require 'dom nil t)
    (user-error "This function requires Emacs 25.1 or later"))
  (declare-function dom-attr "dom" (node attr))
  (declare-function dom-by-tag "dom" (dom tag))
  (let ((file-of-recent-files
         (expand-file-name "recently-used.xbel" (counsel--xdg-data-home))))
    (unless (file-readable-p file-of-recent-files)
      (user-error "List of XDG recent files not found: %s"
                  file-of-recent-files))
    (cl-mapcan (lambda (bookmark-node)
                 (let* ((file (dom-attr bookmark-node 'href))
                        (file (string-remove-prefix "file://" file))
                        (file (url-unhex-string file t))
                        (file (decode-coding-string file 'utf-8 t)))
                   (and (file-exists-p file)
                        (list file))))
               (let ((dom (with-temp-buffer
                            (insert-file-contents file-of-recent-files)
                            (counsel--xml-parse-region))))
                 (nreverse (dom-by-tag dom 'bookmark))))))

(defun counsel-buffer-or-recentf-candidates ()
  "Return candidates for `counsel-buffer-or-recentf'."
  (require 'recentf)
  (recentf-mode)
  (let ((buffers (delq nil (mapcar #'buffer-file-name (buffer-list)))))
    (nconc
     buffers
     (cl-remove-if (lambda (f) (member f buffers))
                   (counsel-recentf-candidates)))))

;;;###autoload
(defun counsel-buffer-or-recentf ()
  "Find a buffer visiting a file or file on `recentf-list'."
  (interactive)
  (ivy-read "Buffer File or Recentf: " (counsel-buffer-or-recentf-candidates)
            :action (lambda (s)
                      (with-ivy-window
                        (if (bufferp s)
                            (switch-to-buffer s)
                          (find-file s))))
            :require-match t
            :caller 'counsel-buffer-or-recentf))

(ivy-configure 'counsel-buffer-or-recentf
  :display-transformer-fn #'counsel-buffer-or-recentf-transformer)

(ivy-set-actions
 'counsel-buffer-or-recentf
 '(("j" find-file-other-window "other window")
   ("f" find-file-other-frame "other frame")
   ("x" counsel-find-file-extern "open externally")))

(defun counsel-buffer-or-recentf-transformer (var)
  "Propertize VAR if it's a buffer visiting a file."
  (if (member var (mapcar #'buffer-file-name (buffer-list)))
      (ivy-append-face var 'ivy-highlight-face)
    var))

;;** `counsel-bookmark'
(defcustom counsel-bookmark-avoid-dired nil
  "If non-nil, open directory bookmarks with `counsel-find-file'.
By default `counsel-bookmark' opens a dired buffer for directories."
  :type 'boolean)

(defvar bookmark-alist)
(declare-function bookmark-location "bookmark")
(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-get-filename "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")

;;;###autoload
(defun counsel-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (bookmark-all-names)
            :history 'bookmark-history
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let ((default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

(defun counsel--apply-bookmark-fn (fn)
  "Return a function applying FN to a bookmark's location."
  (lambda (bookmark)
    (funcall fn (bookmark-location bookmark))))

(ivy-set-actions
 'counsel-bookmark
 `(("j" bookmark-jump-other-window "other window")
   ("d" bookmark-delete "delete")
   ("e" bookmark-rename "edit")
   ("s" bookmark-set "overwrite")
   ("x" ,(counsel--apply-bookmark-fn #'counsel-find-file-extern)
        "open externally")
   ("r" ,(counsel--apply-bookmark-fn #'counsel-find-file-as-root)
        "open as root")))

;;** `counsel-bookmarked-directory'
(defun counsel-bookmarked-directory--candidates ()
  "Get a list of bookmarked directories sorted by file path."
  (bookmark-maybe-load-default-file)
  (sort (cl-delete-if-not
         #'ivy--dirname-p
         (delq nil (mapcar #'bookmark-get-filename bookmark-alist)))
        #'string<))

;;;###autoload
(defun counsel-bookmarked-directory ()
  "Ivy interface for bookmarked directories.

With a prefix argument, this command creates a new bookmark which points to the
current value of `default-directory'."
  (interactive)
  (require 'bookmark)
  (ivy-read "Bookmarked directory: "
            (counsel-bookmarked-directory--candidates)
            :caller 'counsel-bookmarked-directory
            :action #'dired))

(ivy-set-actions 'counsel-bookmarked-directory
                 `(("j" dired-other-window "other window")
                   ("x" counsel-find-file-extern "open externally")
                   ("r" counsel-find-file-as-root "open as root")
                   ("f" ,(lambda (dir)
                           (let ((default-directory dir))
                             (call-interactively #'find-file)))
                    "find-file")))

;;** `counsel-file-register'
;;;###autoload
(defun counsel-file-register ()
  "Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file."
  (interactive)
  (ivy-read "File Register: "
            ;; Use the `register-alist' variable to filter out file
            ;; registers.  Each entry for a file register will have the
            ;; following layout:
            ;;
            ;;     (NUMBER 'file . "string/path/to/file")
            ;;
            ;; So we go through each entry and see if the `cadr' is
            ;; `eq' to the symbol `file'.  If so then add the filename
            ;; (`cddr') which `ivy-read' will use for its choices.
            (mapcar (lambda (register-alist-entry)
                      (if (eq 'file (cadr register-alist-entry))
                          (cddr register-alist-entry)))
                    register-alist)
            :require-match t
            :history 'counsel-file-register
            :caller 'counsel-file-register
            :action (lambda (register-file)
                      (with-ivy-window (find-file register-file)))))

(ivy-configure 'counsel-file-register
  :sort-fn #'ivy-string<)

(ivy-set-actions
 'counsel-file-register
 '(("j" find-file-other-window "other window")))

;;** `counsel-locate'
(defcustom counsel-locate-cmd (cond ((memq system-type '(darwin berkeley-unix))
                                     #'counsel-locate-cmd-noregex)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "es.exe"))
                                     #'counsel-locate-cmd-es)
                                    (t
                                     #'counsel-locate-cmd-default))
  "The function for producing a `locate' command string from the input.

The function takes a string - the current input, and returns a
string - the full shell command to run."
  :type '(choice
          (const :tag "Default" counsel-locate-cmd-default)
          (const :tag "No regex" counsel-locate-cmd-noregex)
          (const :tag "mdfind" counsel-locate-cmd-mdfind)
          (const :tag "everything" counsel-locate-cmd-es)
          (function :tag "Custom")))

(ivy-set-actions
 'counsel-locate
 '(("x" counsel-locate-action-extern "xdg-open")
   ("r" counsel-find-file-as-root "open as root")
   ("d" counsel-locate-action-dired "dired")))

(defvar counsel-locate-history nil
  "History for `counsel-locate'.")

;;;###autoload
(defun counsel-locate-action-extern (x)
  "Pass X to `xdg-open' or equivalent command via the shell."
  (interactive "FFile: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" x)
    (call-process-shell-command (format "%s %s"
                                        (cl-case system-type
                                          (darwin "open")
                                          (cygwin "cygstart")
                                          (t "xdg-open"))
                                        (shell-quote-argument x))
                                nil 0)))

(defalias 'counsel-find-file-extern #'counsel-locate-action-extern)

(declare-function dired-jump "dired-x")

(defun counsel-locate-action-dired (x)
  "Use `dired-jump' on X."
  (dired-jump nil x))

(defvar locate-command)

(defun counsel-locate-cmd-default (input)
  "Return a `locate' shell command based on regexp INPUT.
This uses the user option `locate-command' from the `locate'
library, which see."
  (counsel-require-program locate-command)
  (format "%s -i --regex %s"
          locate-command
          (shell-quote-argument
           (counsel--elisp-to-pcre
            (ivy--regex input)))))

(defun counsel-locate-cmd-noregex (input)
  "Return a `locate' shell command based on INPUT.
This uses the user option `locate-command' from the `locate'
library, which see."
  (counsel-require-program locate-command)
  (format "%s -i %s"
          locate-command
          (shell-quote-argument input)))

(defun counsel-locate-cmd-mdfind (input)
  "Return a `mdfind' shell command based on INPUT."
  (counsel-require-program "mdfind")
  (format "mdfind -name %s 2>%s"
          (shell-quote-argument input)
          (shell-quote-argument (counsel--null-device))))

(defun counsel-locate-cmd-es (input)
  "Return a `es' shell command based on INPUT."
  (defvar w32-ansi-code-page)
  (counsel-require-program "es.exe")
  (let ((raw-string (format "es.exe -i -p -r %s"
                            (counsel--elisp-to-pcre
                             (ivy--regex input t)))))
    ;; W32 doesn't use Unicode by default, so we encode search command
    ;; to local codepage to support searching file names containing
    ;; non-ASCII characters.
    (if (and (eq system-type 'windows-nt)
             (boundp 'w32-ansi-code-page))
        (encode-coding-string raw-string
                              (intern (format "cp%d" w32-ansi-code-page)))
      raw-string)))

(defun counsel-locate-function (input)
  "Call a \"locate\" style shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (funcall counsel-locate-cmd input))
     '("" "working..."))))

(defcustom counsel-locate-db-path "~/.local/mlocate.db"
  "Location where to put the locatedb in case your home folder is encrypted."
  :type 'file)

(defun counsel-file-stale-p (fname seconds)
  "Return non-nil if FNAME was modified more than SECONDS ago."
  (> (float-time (time-since (nth 5 (file-attributes fname))))
     seconds))

(defun counsel--locate-updatedb ()
  (when (file-exists-p "~/.Private")
    (let ((db-fname (expand-file-name counsel-locate-db-path)))
      (setenv "LOCATE_PATH" db-fname)
      (when (or (not (file-exists-p db-fname))
                (counsel-file-stale-p db-fname 60))
        (message "Updating %s..." db-fname)
        (counsel--command
         "updatedb" "-l" "0" "-o" db-fname "-U" (expand-file-name "~"))))))

;;;###autoload
(defun counsel-locate (&optional initial-input)
  "Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  ;; For `locate-command', which is honored in some options of `counsel-locate-cmd'.
  (require 'locate)
  (counsel--locate-updatedb)
  (ivy-read "Locate: " #'counsel-locate-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (when file
                        (with-ivy-window
                          (find-file
                           (concat (file-remote-p default-directory) file)))))
            :caller 'counsel-locate))

(ivy-configure 'counsel-locate
  :unwind-fn #'counsel-delete-process
  :exit-codes '(1 "Nothing found"))

;;** `counsel-tracker'
(defun counsel-tracker-function (input)
  "Call the \"tracker\" shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (format
       "tracker sparql -q \"SELECT ?url WHERE { ?s a nfo:FileDataObject ; nie:url ?url . FILTER (STRSTARTS (?url, 'file://$HOME/')) . FILTER regex(?url, '%s') }\" | tail -n +2 | head -n -1"
       (counsel--elisp-to-pcre (funcall ivy--regex-function input))))
     '("" "working..."))))

(defun counsel-tracker-transformer (str)
  (if (string-match "file:///" str)
      (decode-coding-string (url-unhex-string (substring str 9)) 'utf-8)
    str))

;;;###autoload
(defun counsel-tracker ()
  (interactive)
  (ivy-read "Tracker: " 'counsel-tracker-function
            :dynamic-collection t
            :action (lambda (s) (find-file (counsel-tracker-transformer s)))
            :caller 'counsel-tracker))

(ivy-configure 'counsel-tracker
  :display-transformer-fn #'counsel-tracker-transformer
  :unwind-fn #'counsel-delete-process)

;;** `counsel-fzf'
(defvar counsel-fzf-cmd "fzf -f \"%s\""
  "Command for `counsel-fzf'.")

(defvar counsel--fzf-dir nil
  "Store the base fzf directory.")

(defvar counsel-fzf-dir-function 'counsel-fzf-dir-function-projectile
  "Function that returns a directory for fzf to use.")

(defun counsel-fzf-dir-function-projectile ()
  (if (and
       (fboundp 'projectile-project-p)
       (fboundp 'projectile-project-root)
       (projectile-project-p))
      (projectile-project-root)
    default-directory))

(defun counsel-fzf-function (str)
  (let ((default-directory counsel--fzf-dir))
    (setq ivy--old-re (ivy--regex-fuzzy str))
    (counsel--async-command
     (format counsel-fzf-cmd str)))
  nil)

;;;###autoload
(defun counsel-fzf (&optional initial-input initial-directory fzf-prompt)
  "Open a file using the fzf shell command.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
FZF-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive
   (let ((fzf-basename (car (split-string counsel-fzf-cmd))))
     (list nil
           (when current-prefix-arg
             (counsel-read-directory-name (concat
                                           fzf-basename
                                           " in directory: "))))))
  (counsel-require-program counsel-fzf-cmd)
  (setq counsel--fzf-dir
        (or initial-directory
            (funcall counsel-fzf-dir-function)))
  (ivy-read (or fzf-prompt "fzf: ")
            #'counsel-fzf-function
            :initial-input initial-input
            :re-builder #'ivy--regex-fuzzy
            :dynamic-collection t
            :action #'counsel-fzf-action
            :caller 'counsel-fzf))

(ivy-configure 'counsel-fzf
  :occur #'counsel-fzf-occur
  :unwind-fn #'counsel-delete-process
  :exit-codes '(1 "Nothing found"))

(defun counsel-fzf-action (x)
  "Find file X in current fzf directory."
  (with-ivy-window
    (let ((default-directory counsel--fzf-dir))
      (find-file x))))

(defun counsel-fzf-occur (&optional _cands)
  "Occur function for `counsel-fzf' using `counsel-cmd-to-dired'."
  (cd counsel--fzf-dir)
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (format
     "%s --print0 | xargs -0 ls"
     (format counsel-fzf-cmd ivy-text)))))

(ivy-set-actions
 'counsel-fzf
 '(("x" counsel-locate-action-extern "xdg-open")
   ("d" counsel-locate-action-dired "dired")))

;;** `counsel-dpkg'
;;;###autoload
(defun counsel-dpkg ()
  "Call the \"dpkg\" shell command."
  (interactive)
  (counsel-require-program "dpkg")
  (let ((cands (mapcar
                (lambda (x)
                  (let ((y (split-string x "  +")))
                    (cons (format "%-40s   %s"
                                  (ivy--truncate-string
                                   (nth 1 y) 40)
                                  (nth 4 y))
                          (mapconcat #'identity y " "))))
                (split-string
                 (shell-command-to-string "dpkg -l | tail -n+6") "\n" t))))
    (ivy-read "dpkg: " cands
              :action (lambda (x)
                        (message (cdr x)))
              :caller 'counsel-dpkg)))

;;** `counsel-rpm'
;;;###autoload
(defun counsel-rpm ()
  "Call the \"rpm\" shell command."
  (interactive)
  (counsel-require-program "rpm")
  (let ((cands (mapcar
                (lambda (x)
                  (let ((y (split-string x "|")))
                    (cons (format "%-40s   %s"
                                  (ivy--truncate-string
                                   (nth 0 y) 40)
                                  (nth 1 y))
                          (mapconcat #'identity y " "))))
                (split-string
                 (shell-command-to-string "rpm -qa --qf \"%{NAME}|%{SUMMARY}\\n\"") "\n" t))))
    (ivy-read "rpm: " cands
              :action (lambda (x)
                        (message (cdr x)))
              :caller 'counsel-rpm)))

(defun counsel--find-return-list (args)
  (unless (listp args)
    (user-error
     "`counsel-file-jump-args' is a list now; please customize accordingly"))
  (counsel--call
   (cons find-program args)
   (lambda ()
     (let (files)
       (goto-char (point-min))
       (while (< (point) (point-max))
         (when (looking-at "\\./")
           (goto-char (match-end 0)))
         (push (buffer-substring (point) (line-end-position)) files)
         (beginning-of-line 2))
       (nreverse files)))))

(defcustom counsel-file-jump-args (split-string ". -name .git -prune -o -type f -print")
  "Arguments for the `find-command' when using `counsel-file-jump'."
  :type '(repeat string))

;;** `counsel-file-jump'
(defvar counsel-file-jump-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "`") #'counsel-find-file-from-jump)
    map)
  "Key bindings to be used when in a file-jump minibuffer.")

(defun counsel-find-file-from-jump ()
  "Switch to `counsel-find-file' from `counsel-file-jump'."
  (interactive)
  (ivy-quit-and-run
    (counsel-find-file ivy-text (ivy-state-directory ivy-last))))

;;;###autoload
(defun counsel-file-jump (&optional initial-input initial-directory)
  "Jump to a file below the current directory.
List all files within the current directory or any of its sub-directories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  (counsel-require-program find-program)
  (let ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find file: "
              (counsel--find-return-list counsel-file-jump-args)
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action #'find-file
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-file-jump-map
              :caller 'counsel-file-jump)))

(ivy-set-actions
 'counsel-file-jump
 `(("d" ,(lambda (x)
           (dired (or (file-name-directory x) default-directory)))
    "open in dired")))

(defcustom counsel-dired-jump-args (split-string ". -name .git -prune -o -type d -print")
  "Arguments for the `find-command' when using `counsel-dired-jump'."
  :type '(repeat string))

;;** `counsel-dired-jump'
;;;###autoload
(defun counsel-dired-jump (&optional initial-input initial-directory)
  "Jump to a directory (see `dired-jump') below the current directory.
List all sub-directories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (counsel-read-directory-name "From directory: "))))
  (counsel-require-program find-program)
  (let ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find directory: "
              (cdr
               (counsel--find-return-list counsel-dired-jump-args))
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action (lambda (d) (dired-jump nil (expand-file-name d)))
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-dired-jump)))

;;* Grep
;;** `counsel-ag'
(defvar counsel-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'ivy-call-and-recenter)
    (define-key map (kbd "M-q") #'counsel-git-grep-query-replace)
    (define-key map (kbd "C-'") #'swiper-avy)
    (define-key map (kbd "C-x C-d") #'counsel-cd)
    map))

(defcustom counsel-ag-base-command (list "ag" "--vimgrep" "%s")
  "Template for default `counsel-ag' command.
The value should be either a list of strings, starting with the
`ag' executable file name and followed by its arguments, or a
single string describing a full `ag' shell command.

If the command is specified as a list, `ag' is called directly
using `process-file'; otherwise, it is called as a shell command.
Calling `ag' directly avoids various shell quoting pitfalls, so
it is generally recommended.

If the string \"%s\" appears as an element of the list, or as a
substring of the command string, it is replaced by any optional
`ag' arguments followed by the search regexp specified during the
`counsel-ag' session."
  :package-version '(counsel . "0.14.0")
  :type '(choice (repeat :tag "Command list to call directly" string)
                 (string :tag "Shell command")))

(defvar counsel-ag-command nil)

(defvar counsel--grep-tool-look-around t)

(defvar counsel--regex-look-around nil)

(defconst counsel--command-args-separator " -- ")

(defun counsel--split-command-args (arguments)
  "Split ARGUMENTS into its switches and search-term parts.
Return pair of corresponding strings (SWITCHES . SEARCH-TERM)."
  (if (string-match counsel--command-args-separator arguments)
      (let ((args (substring arguments (match-end 0)))
            (search-term (substring arguments 0 (match-beginning 0))))
        (if (string-prefix-p "-" arguments)
            (cons search-term args)
          (cons args search-term)))
    (cons "" arguments)))

(defun counsel--format-ag-command (extra-args needle)
  "Construct a complete `counsel-ag-command' as a string.
EXTRA-ARGS is a string of the additional arguments.
NEEDLE is the search string."
  (counsel--format counsel-ag-command
                   (if (listp counsel-ag-command)
                       (if (string-match " \\(--\\) " extra-args)
                           (counsel--format
                            (split-string (replace-match "%s" t t extra-args 1))
                            needle)
                         (nconc (split-string extra-args) needle))
                     (if (string-match " \\(--\\) " extra-args)
                         (replace-match needle t t extra-args 1)
                       (concat extra-args " " needle)))))

(defun counsel--grep-regex (str)
  (counsel--elisp-to-pcre
   (setq ivy--old-re
         (funcall (ivy-state-re-builder ivy-last) str))
   counsel--regex-look-around))

(defun counsel--ag-extra-switches (regex)
  "Get additional switches needed for look-arounds."
  (and (stringp counsel--regex-look-around)
       ;; using look-arounds
       (string-match-p "\\`\\^(\\?[=!]" regex)
       (concat " " counsel--regex-look-around " ")))

(defun counsel-ag-function (string)
  "Grep in the current directory for STRING."
  (let* ((command-args (counsel--split-command-args string))
         (search-term (cdr command-args)))
    (or
     (let ((ivy-text search-term))
       (ivy-more-chars))
     (let* ((default-directory (ivy-state-directory ivy-last))
            (regex (counsel--grep-regex search-term))
            (switches (concat (if (ivy--case-fold-p string)
                                  " -i "
                                " -s ")
                              (counsel--ag-extra-switches regex)
                              (car command-args))))
       (counsel--async-command (counsel--format-ag-command
                                switches
                                (funcall (if (listp counsel-ag-command) #'identity
                                           #'shell-quote-argument)
                                         regex)))
       nil))))

;;;###autoload
(cl-defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt
                      &key caller)
  "Grep for a string in a root directory using `ag'.

By default, the root directory is the first directory containing
a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
prompt additionally for EXTRA-AG-ARGS."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (let ((prog-name (car (if (listp counsel-ag-command) counsel-ag-command
                          (split-string counsel-ag-command))))
        (arg (prefix-numeric-value current-prefix-arg)))
    (when (>= arg 4)
      (setq initial-directory
            (or initial-directory
                (counsel-read-directory-name (concat
                                              prog-name
                                              " in directory: ")))))
    (when (>= arg 16)
      (setq extra-ag-args
            (or extra-ag-args
                (read-from-minibuffer (format "%s args: " prog-name)))))
    (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
    (let ((default-directory (or initial-directory
                                 (counsel--git-root)
                                 default-directory)))
      (ivy-read (or ag-prompt
                    (concat prog-name ": "))
                #'counsel-ag-function
                :initial-input initial-input
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :require-match t
                :caller (or caller 'counsel-ag)))))

(ivy-configure 'counsel-ag
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

(defun counsel-read-directory-name (prompt &optional default)
  "Read a directory name.
This is intended as a (partial) replacement for
`read-directory-name'."
  (let ((counsel--find-file-predicate #'file-directory-p))
    (ivy-read prompt
              #'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :def default
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-read-directory-name)))

(ivy-configure 'counsel-read-directory-name
  :parent 'read-file-name-internal)

(defun counsel-cd ()
  "Change the directory for the currently running Ivy grep-like command.
Works for `counsel-git-grep', `counsel-ag', etc."
  (interactive)
  (counsel-delete-process)
  (let* ((input ivy-text)
         (enable-recursive-minibuffers t)
         (def-dir (buffer-file-name (ivy-state-buffer ivy-last)))
         (def-dir (and def-dir (file-name-directory def-dir)))
         (new-dir (counsel-read-directory-name "cd: " def-dir)))
    (ivy-quit-and-run
      (funcall (ivy-state-caller ivy-last) input new-dir))))

(defun counsel--grep-smart-case-flag ()
  (if (ivy--case-fold-p ivy-text)
      "-i"
    (if (and (stringp counsel-ag-base-command)
             (string-prefix-p "pt" counsel-ag-base-command))
        "-S"
      "-s")))

(defun counsel-grep-like-occur (cmd-template)
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (ivy-set-text
   (let ((name (buffer-name)))
     (if (string-match "\"\\(.*\\)\"" name)
         (match-string 1 name)
       (ivy-state-text ivy-occur-last))))
  (let* ((cmd
          (if (functionp cmd-template)
              (funcall cmd-template ivy-text)
            (let* ((command-args (counsel--split-command-args ivy-text))
                   (regex (counsel--grep-regex (cdr command-args)))
                   (extra-switches (counsel--ag-extra-switches regex))
                   (all-args (append
                              (when (car command-args)
                                (split-string (car command-args)))
                              (when extra-switches
                                (split-string extra-switches))
                              (list
                               (counsel--grep-smart-case-flag)
                               regex))))
              (if (stringp cmd-template)
                  (counsel--format
                   cmd-template
                   (mapconcat #'shell-quote-argument all-args " "))
                (cl-mapcan
                 (lambda (x) (if (string= x "%s") (copy-sequence all-args) (list x)))
                 cmd-template)))))
         (cands (counsel--split-string
                 (if (stringp cmd)
                     (shell-command-to-string cmd)
                   (counsel--call cmd)))))
    (swiper--occur-insert-lines (mapcar #'counsel--normalize-grep-match cands))))

(defun counsel-ag-occur (&optional _cands)
  "Generate a custom occur buffer for `counsel-ag'."
  (counsel-grep-like-occur
   counsel-ag-command))

;;** `counsel-pt'
(defcustom counsel-pt-base-command "pt --nocolor --nogroup -e %s"
  "Alternative to `counsel-ag-base-command' using pt."
  :type 'string)

;;;###autoload
(defun counsel-pt (&optional initial-input)
  "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'."
  (interactive)
  (let ((counsel-ag-base-command counsel-pt-base-command)
        (counsel--grep-tool-look-around nil))
    (counsel-ag initial-input nil nil nil :caller 'counsel-pt)))

(ivy-configure 'counsel-pt
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t)

;;** `counsel-ack'
(defcustom counsel-ack-base-command
  (concat
   (file-name-nondirectory
    (or (executable-find "ack-grep") "ack"))
   " --nocolor --nogroup %s")
  "Alternative to `counsel-ag-base-command' using ack."
  :type 'string)

;;;###autoload
(defun counsel-ack (&optional initial-input)
  "Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'."
  (interactive)
  (let ((counsel-ag-base-command counsel-ack-base-command)
        (counsel--grep-tool-look-around t))
    (counsel-ag
     initial-input nil nil nil
     :caller 'counsel-ack)))

;;** `counsel-rg'
(defcustom counsel-rg-base-command
  `("rg"
    "--max-columns" "240"
    "--with-filename"
    "--no-heading"
    "--line-number"
    "--color" "never"
    "%s"
    ,@(and (memq system-type '(ms-dos windows-nt))
           (list "--path-separator" "/" ".")))
  "Like `counsel-ag-base-command', but for `counsel-rg'.

Note: don't use single quotes for the regexp."
  :package-version '(counsel . "0.14.0")
  :type '(choice (repeat :tag "Command list to call directly" string)
                 (string :tag "Shell command")))

(defun counsel--rg-targets ()
  "Return a list of files to operate on, based on `dired-mode' marks."
  (when (eq major-mode 'dired-mode)
    (let ((files
           (dired-get-marked-files 'no-dir nil nil t)))
      (when (or (cdr files)
                (when (ivy--string-search "*ivy-occur" (buffer-name))
                  (dired-toggle-marks)
                  (setq files (dired-get-marked-files 'no-dir))
                  (dired-toggle-marks)
                  t))
        (delq t files)))))

;;;###autoload
(defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Grep for a string in the current directory using `rg'.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el"
  (interactive)
  (let ((counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                :caller 'counsel-rg)))

(ivy-configure 'counsel-rg
  :occur #'counsel-ag-occur
  :unwind-fn #'counsel--grep-unwind
  :display-transformer-fn #'counsel-git-grep-transformer
  :grep-p t
  :exit-codes '(1 "No matches found"))

;;** `counsel-grep'
(defvar counsel-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'ivy-call-and-recenter)
    (define-key map (kbd "M-q") #'swiper-query-replace)
    (define-key map (kbd "C-'") #'swiper-avy)
    map))

(defcustom counsel-grep-base-command "grep -E -n -e %s %s"
  "Format string used by `counsel-grep' to build a shell command.
It should contain two %-sequences (see function `format') to be
substituted by the search regexp and file, respectively.  Neither
%-sequence should be contained in single quotes."
  :type 'string)

(defvar counsel-grep-command nil)

(defun counsel-grep-function (string)
  "Grep in the current directory for STRING."
  (or
   (ivy-more-chars)
   (let* ((regex (counsel--grep-regex string))
          (cmd (counsel--format
                counsel-grep-command
                (funcall (if (listp counsel-grep-command) #'identity
                           #'shell-quote-argument)
                         regex))))
     (counsel--async-command
      (if (ivy--case-fold-p regex)
          (if (listp cmd) (nconc (list (car cmd) "-i") (cdr cmd))
            (string-match " " cmd)
            (replace-match " -i " nil nil cmd))
        cmd))
     nil)))

(defvar counsel--grep-last-pos nil
  "Store the last point and line that `counsel-grep-action' scrolled to.
This speeds up scrolling: instead of going to `point-min' and
`forward-line' with a huge arg (e.g. to scroll 50K lines), scroll
relative to the last position stored here.")

(defun counsel-grep-action (x)
  "Go to candidate X."
  (with-ivy-window
    (swiper--cleanup)
    (let ((default-directory
           (file-name-directory
            (ivy-state-directory ivy-last)))
          file-name line-number)
      (when (cond ((string-match "\\`\\([0-9]+\\):\\(.*\\)\\'" x)
                   (setq file-name (buffer-file-name (ivy-state-buffer ivy-last)))
                   (setq line-number (match-string-no-properties 1 x)))
                  ((string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\(.*\\)\\'" x)
                   (setq file-name (match-string-no-properties 1 x))
                   (setq line-number (match-string-no-properties 2 x))))
        ;; If the file buffer is already open, just get it. Prevent doing
        ;; `find-file', as that file could have already been opened using
        ;; `find-file-literally'.
        (with-current-buffer (or (get-file-buffer file-name)
                                 (find-file file-name))
          (setq line-number (string-to-number line-number))
          (if (and counsel--grep-last-pos (= (point) (car counsel--grep-last-pos)))
              (forward-line (- line-number (cdr counsel--grep-last-pos)))
            (goto-char (point-min))
            (forward-line (1- line-number)))
          (setq counsel--grep-last-pos (cons (point) line-number))
          (when (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
            (when swiper-goto-start-of-match
              (goto-char (match-beginning 0))))
          (run-hooks 'counsel-grep-post-action-hook)
          (if (eq ivy-exit 'done)
              (swiper--ensure-visible)
            (isearch-range-invisible (line-beginning-position)
                                     (line-end-position))
            (swiper--add-overlays (ivy--regex ivy-text))))))))

(defun counsel-grep-occur (&optional _cands)
  "Generate a custom Occur buffer for `counsel-grep'."
  (let ((file (buffer-file-name (ivy-state-buffer ivy-last))))
    (counsel-grep-like-occur
     (format "grep -niE %%s %s %s"
             (if file (shell-quote-argument (file-name-nondirectory file)) "")
             (shell-quote-argument (counsel--null-device))))))

(defvar counsel-grep-history nil
  "History for `counsel-grep'.")

;;;###autoload
(defun counsel-grep (&optional initial-input)
  "Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (counsel-require-program counsel-grep-base-command)
  (setq counsel-grep-command
        (counsel--format counsel-grep-base-command "%s"
                         (funcall (if (listp counsel-grep-base-command) #'identity
                                    #'shell-quote-argument)
                                  (file-name-nondirectory
                                   buffer-file-name))))
  (let ((default-directory (file-name-directory buffer-file-name))
        (init-point (point))
        res)
    (unwind-protect
         (setq res (ivy-read "grep: " #'counsel-grep-function
                             :initial-input initial-input
                             :dynamic-collection t
                             :require-match t
                             :preselect
                             (when (< (- (line-end-position) (line-beginning-position)) 300)
                               (format "%d:%s"
                                       (line-number-at-pos)
                                       (regexp-quote
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)))))
                             :keymap counsel-grep-map
                             :history 'counsel-grep-history
                             :re-builder #'ivy--regex
                             :action #'counsel-grep-action
                             :caller 'counsel-grep))
      (unless res
        (goto-char init-point)))))

(ivy-configure 'counsel-grep
  :update-fn 'auto
  :unwind-fn #'counsel--grep-unwind
  :index-fn #'ivy-recompute-index-swiper-async
  :occur #'counsel-grep-occur
  :more-chars 2
  :grep-p t
  :exit-codes '(1 ""))

;;;###autoload
(defun counsel-grep-backward (&optional initial-input)
  "Grep for a string in the file visited by the current buffer going
backward similar to `swiper-backward'. When non-nil, INITIAL-INPUT is
the initial search pattern."
  (interactive)
  (let ((ivy-index-functions-alist
         '((counsel-grep . ivy-recompute-index-swiper-async-backward))))
    (counsel-grep initial-input)))

;;** `counsel-grep-or-swiper'
(defcustom counsel-grep-swiper-limit 300000
  "Buffer size threshold for `counsel-grep-or-swiper'.
When the number of characters in a buffer exceeds this threshold,
`counsel-grep' will be used instead of `swiper'."
  :type 'integer)

(defcustom counsel-grep-use-swiper-p #'counsel-grep-use-swiper-p-default
  "When this function returns non-nil, call `swiper', else `counsel-grep'."
  :type '(choice
          (const :tag "Rely on `counsel-grep-swiper-limit'."
           counsel-grep-use-swiper-p-default)
          (const :tag "Always use `counsel-grep'." ignore)
          (function :tag "Custom")))

(defun counsel-grep-use-swiper-p-default ()
  (<= (buffer-size)
      (/ counsel-grep-swiper-limit
         (if (eq major-mode 'org-mode) 4 1))))

;;;###autoload
(defun counsel-grep-or-swiper (&optional initial-input)
  "Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (if (or (not buffer-file-name)
          (buffer-narrowed-p)
          (ignore-errors
            (file-remote-p buffer-file-name))
          (jka-compr-get-compression-info buffer-file-name)
          (funcall counsel-grep-use-swiper-p))
      (swiper initial-input)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (counsel-grep initial-input)))

;;** `counsel-grep-or-swiper-backward'
;;;###autoload
(defun counsel-grep-or-swiper-backward (&optional initial-input)
  "Call `swiper-backward' for small buffers and `counsel-grep-backward' for
large ones.  When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . ivy-recompute-index-swiper-backward)
           (counsel-grep . ivy-recompute-index-swiper-async-backward))))
    (counsel-grep-or-swiper initial-input)))

;;** `counsel-recoll'
(defun counsel-recoll-function (str)
  "Run recoll for STR."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (format "recoll -t -b %s"
              (shell-quote-argument str)))
     nil)))

;; This command uses the recollq command line tool that comes together
;; with the recoll (the document indexing database) source:
;;     https://www.lesbonscomptes.com/recoll/download.html
;; You need to build it yourself (together with recoll):
;;     cd ./query && make && sudo cp recollq /usr/local/bin
;; You can try the GUI version of recoll with:
;;     sudo apt-get install recoll
;; Unfortunately, that does not install recollq.
;;;###autoload
(defun counsel-recoll (&optional initial-input)
  "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program "recoll")
  (ivy-read "recoll: " 'counsel-recoll-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match "file://\\(.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))
            :caller 'counsel-recoll))

(ivy-configure 'counsel-recoll
  :unwind-fn #'counsel-delete-process)

;;* Org
;;** `counsel-org-tag'
(defvar counsel-org-tags nil
  "Store the current list of tags.")

(defvar org-outline-regexp)
(defvar org-indent-mode)
(defvar org-indent-indentation-per-level)
(defvar org-tags-column)
(declare-function org-get-tags-string "org")
(declare-function org-get-tags "org")
(declare-function org-make-tag-string "org")
(declare-function org-move-to-column "org-compat")

(defun counsel--org-make-tag-string ()
  (if (fboundp #'org-make-tag-string)
      ;; >= Org 9.2
      (org-make-tag-string (counsel--org-get-tags))
    (with-no-warnings
      (org-get-tags-string))))

(defun counsel-org-change-tags (tags)
  "Change tags of current org headline to TAGS."
  (let ((current (counsel--org-make-tag-string))
        (col (current-column))
        level)
    ;; Insert new tags at the correct column
    (beginning-of-line 1)
    (setq level (or (and (looking-at org-outline-regexp)
                         (- (match-end 0) (point) 1))
                    1))
    (cond
      ((and (equal current "") (equal tags "")))
      ((re-search-forward
        (concat "\\([ \t]*" (regexp-quote current) "\\)[ \t]*$")
        (line-end-position) t)
       (if (equal tags "")
           (delete-region
            (match-beginning 0)
            (match-end 0))
         (goto-char (match-beginning 0))
         (let* ((c0 (current-column))
                ;; compute offset for the case of org-indent-mode active
                (di (if (bound-and-true-p org-indent-mode)
                        (* (1- org-indent-indentation-per-level) (1- level))
                      0))
                (p0 (if (equal (char-before) ?*) (1+ (point)) (point)))
                (tc (+ org-tags-column (if (> org-tags-column 0) (- di) di)))
                (c1 (max (1+ c0) (if (> tc 0) tc (- (- tc) (string-width tags)))))
                (rpl (concat (make-string (max 0 (- c1 c0)) ?\ ) tags)))
           (replace-match rpl t t)
           (and c0 indent-tabs-mode (tabify p0 (point)))
           tags)))
      (t (error "Tags alignment failed")))
    (org-move-to-column col)))

(defun counsel-org--set-tags ()
  "Set tags of current org headline to `counsel-org-tags'."
  (counsel-org-change-tags
   (if counsel-org-tags
       (format ":%s:"
               (mapconcat #'identity counsel-org-tags ":"))
     "")))

(defvar org-agenda-bulk-marked-entries)

(declare-function org-get-at-bol "org")
(declare-function org-agenda-error "org-agenda")

(defun counsel-org-tag-action (x)
  "Add tag X to `counsel-org-tags'.
If X is already part of the list, remove it instead.  Quit the selection if
X is selected by either `ivy-done', `ivy-alt-done' or `ivy-immediate-done',
otherwise continue prompting for tags."
  (if (member x counsel-org-tags)
      (progn
        (setq counsel-org-tags (delete x counsel-org-tags)))
    (unless (equal x "")
      (setq counsel-org-tags (append counsel-org-tags (list x)))
      (unless (member x ivy--all-candidates)
        (setq ivy--all-candidates (append ivy--all-candidates (list x))))))
  (let ((prompt (counsel-org-tag-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "%-4d " prompt)))
  (cond ((memq this-command '(ivy-done
                              ivy-alt-done
                              ivy-immediate-done))
         (if (eq major-mode 'org-agenda-mode)
             (if (null org-agenda-bulk-marked-entries)
                 (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                                     (org-agenda-error))))
                   (with-current-buffer (marker-buffer hdmarker)
                     (goto-char hdmarker)
                     (counsel-org--set-tags)))
               (let ((add-tags (copy-sequence counsel-org-tags)))
                 (dolist (m org-agenda-bulk-marked-entries)
                   (with-current-buffer (marker-buffer m)
                     (save-excursion
                       (goto-char m)
                       (setq counsel-org-tags
                             (delete-dups
                              (append (counsel--org-get-tags) add-tags)))
                       (counsel-org--set-tags))))))
           (counsel-org--set-tags)
           (unless (member x counsel-org-tags)
             (message "Tag %S has been removed." x))))
        ((eq this-command 'ivy-call)
         (with-selected-window (active-minibuffer-window)
           (delete-minibuffer-contents)))))

(defun counsel-org-tag-prompt ()
  "Return prompt for `counsel-org-tag'."
  (format "Tags (%s): "
          (mapconcat #'identity counsel-org-tags ", ")))

(defvar org-setting-tags)
(defvar org-last-tags-completion-table)
(defvar org-tag-persistent-alist)
(defvar org-tag-alist)
(defvar org-complete-tags-always-offer-all-agenda-tags)

(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-get-buffer-tags "org")
(declare-function org-global-tags-completion-table "org")
(declare-function org-agenda-files "org")
(declare-function org-agenda-set-tags "org-agenda")
(declare-function org-tags-completion-function "org")

;;;###autoload
(defun counsel--org-get-tags ()
  (delete "" (condition-case nil
                 (org-get-tags nil t)
               (error (org-get-tags)))))

;;;###autoload
(defun counsel-org-tag ()
  "Add or remove tags in `org-mode'."
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags (counsel--org-get-tags)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (counsel--org-get-tags)))
    (let ((org-last-tags-completion-table
           (append (and (or org-complete-tags-always-offer-all-agenda-tags
                            (eq major-mode 'org-agenda-mode))
                        (org-global-tags-completion-table
                         (org-agenda-files)))
                   (unless (boundp 'org-current-tag-alist)
                     org-tag-persistent-alist)
                   (or (if (boundp 'org-current-tag-alist)
                           org-current-tag-alist
                         org-tag-alist)
                       (org-get-buffer-tags)))))
      (ivy-read (counsel-org-tag-prompt)
                (lambda (str _pred _action)
                  (delete-dups
                   (all-completions str #'org-tags-completion-function)))
                :history 'org-tags-history
                :action #'counsel-org-tag-action
                :caller 'counsel-org-tag))))

(defvar org-version)

;;;###autoload
(defun counsel-org-tag-agenda ()
  "Set tags for the current agenda item."
  (interactive)
  (cl-letf (((symbol-function (if (version< org-version "9.2")
                                  'org-set-tags
                                'org-set-tags-command))
             #'counsel-org-tag))
    (org-agenda-set-tags)))

(defcustom counsel-org-headline-display-tags nil
  "If non-nil, display tags in matched `org-mode' headlines."
  :type 'boolean)

(defcustom counsel-org-headline-display-todo nil
  "If non-nil, display todo keywords in matched `org-mode' headlines."
  :type 'boolean)

(defcustom counsel-org-headline-display-priority nil
  "If non-nil, display priorities in matched `org-mode' headlines."
  :type 'boolean)

(defcustom counsel-org-headline-display-comment nil
  "If non-nil, display COMMENT string in matched `org-mode' headlines."
  :type 'boolean)

(defcustom counsel-org-headline-display-statistics nil
  "If non-nil, display statistics cookie in matched `org-mode' headlines."
  :type 'boolean)

(declare-function org-get-heading "org")
(declare-function org-goto-marker-or-bmk "org")
(declare-function outline-next-heading "outline")

;;;###autoload
(defalias 'counsel-org-goto #'counsel-outline)

(defcustom counsel-org-goto-all-outline-path-prefix nil
  "Prefix for outline candidates in `counsel-org-goto-all'."
  :type '(choice
          (const :tag "None" nil)
          (const :tag "File name" file-name)
          (const :tag "File name (nondirectory part)" file-name-nondirectory)
          (const :tag "Buffer name" buffer-name)))

(defun counsel-org-goto-all--outline-path-prefix ()
  (cl-case counsel-org-goto-all-outline-path-prefix
    (file-name buffer-file-name)
    (file-name-nondirectory (file-name-nondirectory buffer-file-name))
    (buffer-name (buffer-name))))

(defvar counsel-outline-settings
  '((emacs-lisp-mode
     :outline-regexp ";;[;*]+[\s\t]+"
     :outline-level counsel-outline-level-emacs-lisp)
    (org-mode
     :outline-title counsel-outline-title-org
     :action counsel-org-goto-action
     :history counsel-org-goto-history
     :caller counsel-org-goto)
    ;; markdown-mode package
    (markdown-mode
     :outline-title counsel-outline-title-markdown)
    ;; Built-in mode or AUCTeX package
    (latex-mode
     :outline-title counsel-outline-title-latex))
  "Alist mapping major modes to their `counsel-outline' settings.

Each entry is a pair (MAJOR-MODE . PLIST).  `counsel-outline'
checks whether an entry exists for the current buffer's
MAJOR-MODE and, if so, loads the settings specified by PLIST
instead of the default settings.  The following settings are
recognized:

- `:outline-regexp' is a regexp to match the beginning of an
  outline heading.  It is only checked at the start of a line and
  so need not start with \"^\".
  Defaults to the value of the variable `outline-regexp'.

- `:outline-level' is a function of no arguments which computes
  the level of an outline heading.  It is called with point at
  the beginning of `outline-regexp' and with the match data
  corresponding to `outline-regexp'.
  Defaults to the value of the variable `outline-level'.

- `:outline-title' is a function of no arguments which returns
  the title of an outline heading.  It is called with point at
  the end of `outline-regexp' and with the match data
  corresponding to `outline-regexp'.
  Defaults to the function `counsel-outline-title'.

- `:action' is a function of one argument, the selected outline
  heading to jump to.  This setting corresponds directly to its
  eponymous `ivy-read' keyword, as used by `counsel-outline', so
  the type of the function's argument depends on the value
  returned by `counsel-outline-candidates'.
  Defaults to the function `counsel-outline-action'.

- `:history' is a history list, usually a symbol representing a
  history list variable.  It corresponds directly to its
  eponymous `ivy-read' keyword, as used by `counsel-outline'.
  Defaults to the symbol `counsel-outline-history'.

- `:caller' is a symbol to uniquely identify the caller to
  `ivy-read'.  It corresponds directly to its eponymous
  `ivy-read' keyword, as used by `counsel-outline'.
  Defaults to the symbol `counsel-outline'.

- `:display-style' overrides the variable
  `counsel-outline-display-style'.

- `:path-separator' overrides the variable
  `counsel-outline-path-separator'.

- `:face-style' overrides the variable
  `counsel-outline-face-style'.

- `:custom-faces' overrides the variable
  `counsel-outline-custom-faces'.")

;;;###autoload
(defun counsel-org-goto-all ()
  "Go to a different location in any org file."
  (interactive)
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries
                (nconc entries
                       (counsel-outline-candidates
                        (cdr (assq 'org-mode counsel-outline-settings))
                        (counsel-org-goto-all--outline-path-prefix)))))))
    (ivy-read "Goto: " entries
              :history 'counsel-org-goto-history
              :action #'counsel-org-goto-action
              :caller 'counsel-org-goto-all)))

(defun counsel-org-goto-action (x)
  "Go to headline in candidate X."
  (org-goto-marker-or-bmk (cdr x)))

(defun counsel--org-get-heading-args ()
  "Return list of arguments for `org-get-heading'.
Try to return the right number of arguments for the current Org
version.  Argument values are based on the
`counsel-org-headline-display-*' user options."
  (nbutlast (mapcar #'not (list counsel-org-headline-display-tags
                                counsel-org-headline-display-todo
                                counsel-org-headline-display-priority
                                counsel-org-headline-display-comment))
            ;; Added in Emacs 26.1.
            (if (if (fboundp 'func-arity)
                    (< (cdr (func-arity #'org-get-heading)) 3)
                  (version< org-version "9.1.1"))
                2 0)))

;;** `counsel-org-file'
(declare-function org-attach-dir "org-attach")
(declare-function org-attach-file-list "org-attach")
(defvar org-attach-directory)

(defun counsel-org-files ()
  "Return list of all files under current Org attachment directories.
Filenames returned are relative to `default-directory'.  For each
attachment directory associated with the current buffer, all
contained files are listed, so the return value could conceivably
include attachments of other Org buffers."
  (require 'org-attach)
  (let (dirs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ":\\(?:ATTACH_DIR\\|ID\\):[\t ]+.*$" nil t)
        (let ((dir (org-attach-dir)))
          (when dir
            (push dir dirs)))))
    (cl-mapcan
     (lambda (dir)
       (mapcar (lambda (file)
                 (file-relative-name (expand-file-name file dir)))
               (org-attach-file-list dir)))
     (nreverse dirs))))

;;;###autoload
(defun counsel-org-file ()
  "Browse all attachments for current Org file."
  (interactive)
  (ivy-read "file: " (counsel-org-files)
            :action #'counsel-locate-action-dired
            :caller 'counsel-org-file))

;;** `counsel-org-entity'
(defvar org-entities)
(defvar org-entities-user)

;;;###autoload
(defun counsel-org-entity ()
  "Complete Org entities using Ivy."
  (interactive)
  (require 'org)
  (ivy-read "Entity: " (cl-loop for element in (append org-entities org-entities-user)
                          unless (stringp element)
                          collect (cons
                                   (format "%20s | %20s | %20s | %s"
                                           (cl-first element)    ; name
                                           (cl-second element)   ; latex
                                           (cl-fourth element)   ; html
                                           (cl-seventh element)) ; utf-8
                                   element))
            :require-match t
            :action '(1
                      ("u" (lambda (candidate)
                             (insert (cl-seventh (cdr candidate)))) "utf-8")
                      ("o" (lambda (candidate)
                             (insert "\\" (cl-first (cdr candidate)))) "org-entity")
                      ("l" (lambda (candidate)
                             (insert (cl-second (cdr candidate)))) "latex")
                      ("h" (lambda (candidate)
                             (insert (cl-fourth (cdr candidate)))) "html")
                      ("a" (lambda (candidate)
                             (insert (cl-fifth (cdr candidate)))) "ascii")
                      ("L" (lambda (candidate)
                             (insert (cl-sixth (cdr candidate))) "Latin-1")))))

;;** `counsel-org-capture'
(defvar org-capture-templates)
(defvar org-capture-templates-contexts)
(declare-function org-contextualize-keys "org")
(declare-function org-capture-goto-last-stored "org-capture")
(declare-function org-capture-goto-target "org-capture")
(declare-function org-capture-upgrade-templates "org-capture")

;;;###autoload
(defun counsel-org-capture ()
  "Capture something."
  (interactive)
  (require 'org-capture)
  (ivy-read "Capture template: "
            ;; We build the list of capture templates as in `org-capture-select-template':
            (let (prefixes)
              (cl-mapcan
               (lambda (x)
                 (let ((x-keys (car x)))
                   ;; Remove prefixed keys until we get one that matches the current item.
                   (while (and prefixes
                               (let ((p1-keys (caar prefixes)))
                                 (or
                                  (<= (length x-keys) (length p1-keys))
                                  (not (string-prefix-p p1-keys x-keys)))))
                     (pop prefixes))
                   (if (> (length x) 2)
                       (let ((desc (mapconcat #'cadr (reverse (cons x prefixes)) " | ")))
                         (list (format "%-5s %s" x-keys desc)))
                     (push x prefixes)
                     nil)))
               (or (org-contextualize-keys
                    (org-capture-upgrade-templates org-capture-templates)
                    org-capture-templates-contexts)
                   '(("t" "Task" entry (file+headline "" "Tasks")
                      "* TODO %?\n  %u\n  %a")))))
            :require-match t
            :action (lambda (x)
                      (org-capture nil (car (split-string x))))
            :caller 'counsel-org-capture))

(ivy-configure 'counsel-org-capture
  :initial-input "^")

(ivy-set-actions
 'counsel-org-capture
 `(("t" ,(lambda (x)
           (org-capture-goto-target (car (split-string x))))
        "go to target")
   ("l" ,(lambda (_x)
           (org-capture-goto-last-stored))
        "go to last stored")
   ("p" ,(lambda (x)
           (org-capture 0 (car (split-string x))))
        "insert template at point")
   ("c" ,(lambda (_x)
           (customize-variable 'org-capture-templates))
        "customize org-capture-templates")))

;;** `counsel-org-agenda-headlines'
(defvar org-odd-levels-only)
(declare-function org-set-startup-visibility "org")
(declare-function org-show-entry "org")
(declare-function org-map-entries "org")
(declare-function org-heading-components "org")

(defun counsel-org-agenda-headlines-action-goto (headline)
  "Go to the `org-mode' agenda HEADLINE."
  (find-file (nth 1 headline))
  (org-set-startup-visibility)
  (goto-char (nth 2 headline))
  (org-show-entry))

(ivy-set-actions
 'counsel-org-agenda-headlines
 '(("g" counsel-org-agenda-headlines-action-goto "goto headline")))

(defvar counsel-org-agenda-headlines-history nil
  "History for `counsel-org-agenda-headlines'.")

(defcustom counsel-outline-display-style 'path
  "The style used when displaying matched outline headings.

If `headline', the title is displayed with leading stars
indicating the outline level.

If `path', the path hierarchy is displayed.  For each entry the
title is shown.  Entries are separated with
`counsel-outline-path-separator'.

If `title' or any other value, only the title of the heading is
displayed.

For displaying tags and TODO keywords in `org-mode' buffers, see
`counsel-org-headline-display-tags' and
`counsel-org-headline-display-todo', respectively."
  :type '(choice
          (const :tag "Title only" title)
          (const :tag "Headline" headline)
          (const :tag "Path" path)))

(defcustom counsel-outline-path-separator "/"
  "String separating path entries in matched outline headings.
This variable has no effect unless
`counsel-outline-display-style' is set to `path'."
  :type 'string)

(declare-function org-get-outline-path "org")

(defun counsel-org-agenda-headlines--candidates ()
  "Return a list of completion candidates for `counsel-org-agenda-headlines'."
  (org-map-entries
   (lambda ()
     (let* ((components (org-heading-components))
            (level (and (eq counsel-outline-display-style 'headline)
                        (make-string
                         (if org-odd-levels-only
                             (nth 1 components)
                           (nth 0 components))
                         ?*)))
            (todo (and counsel-org-headline-display-todo
                       (nth 2 components)))
            (path (and (eq counsel-outline-display-style 'path)
                       (org-get-outline-path)))
            (priority (and counsel-org-headline-display-priority
                           (nth 3 components)))
            (text (nth 4 components))
            (tags (and counsel-org-headline-display-tags
                       (nth 5 components))))
       (list (string-join
              (delq nil (list level
                              todo
                              (and priority (format "[#%c]" priority))
                              (string-join (append path (list text))
                                           counsel-outline-path-separator)
                              tags))
              " ")
             buffer-file-name
             (point))))
   nil
   'agenda))

;;;###autoload
(defun counsel-org-agenda-headlines ()
  "Choose from headers of `org-mode' files in the agenda."
  (interactive)
  (require 'org)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "Org headline: "
              (counsel-org-agenda-headlines--candidates)
              :action #'counsel-org-agenda-headlines-action-goto
              :history 'counsel-org-agenda-headlines-history
              :caller 'counsel-org-agenda-headlines)))

;;** `counsel-org-link'
(declare-function org-insert-link "ol")
(declare-function org-id-get-create "org-id")

(defun counsel-org-link-action (x)
  "Insert a link to X."
  (let ((id (save-excursion
              (goto-char (cdr x))
              (org-id-get-create))))
    (org-insert-link nil (concat "id:" id) (car x))))

;;;###autoload
(defun counsel-org-link ()
  "Insert a link to an headline with completion."
  (interactive)
  (ivy-read "Link: " (counsel-outline-candidates
                      '(:outline-title counsel-outline-title-org ))
            :action #'counsel-org-link-action
            :history 'counsel-org-link-history
            :caller 'counsel-org-link))

;; Misc. Emacs
;;** `counsel-mark-ring'
(defface counsel--mark-ring-highlight
  '((t :inherit highlight))
  "Face for current `counsel-mark-ring' line."
  :group 'ivy-faces)

(defvar counsel--mark-ring-overlay nil
  "Internal overlay to highlight line by candidate of `counsel-mark-ring'.")

(defun counsel--mark-ring-add-highlight ()
  "Add highlight to current line."
  (setq counsel--mark-ring-overlay
        (make-overlay (line-beginning-position) (1+ (line-end-position))))
  (with-ivy-window
    (overlay-put counsel--mark-ring-overlay 'face
                 'counsel--mark-ring-highlight)))

(defun counsel--mark-ring-delete-highlight ()
  "If `counsel-mark-ring' have highlight, delete highlight."
  (if counsel--mark-ring-overlay (delete-overlay counsel--mark-ring-overlay)))

(defvar counsel--mark-ring-calling-point 0
  "Internal variable to remember calling position.")

(defun counsel--mark-ring-unwind ()
  "Return back to calling position of `counsel-mark-ring'."
  (goto-char counsel--mark-ring-calling-point)
  (counsel--mark-ring-delete-highlight))

(defun counsel--mark-ring-update-fn ()
  "Show preview by candidate."
  (let ((pos (get-text-property 0 'point (ivy-state-current ivy-last))))
    (counsel--mark-ring-delete-highlight)
    (with-ivy-window
      (goto-char pos)
      (counsel--mark-ring-add-highlight))))

;;;###autoload
(defun counsel-mark-ring ()
  "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let* ((counsel--mark-ring-calling-point (point))
         (marks (copy-sequence mark-ring))
         (marks (delete-dups marks))
         (marks
          ;; mark-marker is empty?
          (if (equal (mark-marker) (make-marker))
              marks
            (cons (copy-marker (mark-marker)) marks)))
         (candidates (counsel-mark--get-candidates marks)))
    (if candidates
        (counsel-mark--ivy-read "Mark: " candidates 'counsel-mark-ring)
      (message "Mark ring is empty"))))

(defun counsel-mark--get-candidates (marks)
  "Convert a list of MARKS into mark candidates.
candidates are simply strings formatted to have the line number of the
associated mark prepended to them and having an extra text property of
point to indicarte where the candidate mark is."
  (when marks
    (save-excursion
      (save-restriction
        ;; Widen, both to save `line-number-at-pos' the trouble
        ;; and for `buffer-substring' to work.
        (widen)
        (let* ((width (length (number-to-string (line-number-at-pos (point-max)))))
               (fmt (format "%%%dd %%s" width)))
          (mapcar (lambda (mark)
                    (goto-char (marker-position mark))
                    (let ((linum (line-number-at-pos))
                          (line  (buffer-substring
                                  (line-beginning-position) (line-end-position))))
                      (propertize (format fmt linum line) 'point (point))))
                  marks))))))

(defun counsel-mark--ivy-read (prompt candidates caller)
  "Call `ivy-read' with sane defaults for traversing marks.
CANDIDATES should be an alist with the `car' of the list being
the completion candidate string and the `cdr' being the point that
mark should take you to.

This subroutine is intended to be used by both `counsel-mark-ring' and
`counsel-evil-marks'."
  (ivy-read prompt candidates
            :require-match t
            :update-fn #'counsel--mark-ring-update-fn
            :action (lambda (cand)
                      (let ((pos (get-text-property 0 'point cand)))
                        (when pos
                          (unless (<= (point-min) pos (point-max))
                            (if widen-automatically
                                (widen)
                              (error "\
Position of selected mark outside accessible part of buffer")))
                          (goto-char pos))))
            :unwind #'counsel--mark-ring-unwind
            :caller caller))

(ivy-configure 'counsel-mark-ring
  :update-fn #'counsel--mark-ring-update-fn
  :unwind-fn #'counsel--mark-ring-unwind
  :sort-fn #'ivy-string<)

;;** `counsel-evil-marks'
(defvar counsel-evil-marks-exclude-registers nil
  "List of evil registers to not display in `counsel-evil-marks' by default.
Each member of the list should be a character (stored as an integer).")

(defvar evil-markers-alist)
(declare-function evil-global-marker-p "ext:evil-common")

(defun counsel-mark--get-evil-candidates (all-markers-p)
  "Convert all evil MARKS in the current buffer to mark candidates.
Works like `counsel-mark--get-candidates' but also prepends the
register tied to a mark in the message string."
  ;; evil doesn't provide a standalone method to access the list of
  ;; marks in the current buffer, as it does with registers.
  (let* ((all-markers
          (append
           (cl-remove-if (lambda (m)
                           (or (evil-global-marker-p (car m))
                               (not (markerp (cdr m)))))
                         evil-markers-alist)
           (cl-remove-if (lambda (m)
                           (or (not (evil-global-marker-p (car m)))
                               (not (markerp (cdr m)))))
                         (default-value 'evil-markers-alist))))

         (all-markers
          ;; with prefix, ignore register exclusion list.
          (if all-markers-p
              all-markers
            (cl-remove-if
             (lambda (x) (member (car x) counsel-evil-marks-exclude-registers))
             all-markers)))
         ;; separate the markers from the evil registers
         ;; for call to `counsel-mark--get-candidates'
         (registers (mapcar #'car all-markers))
         (markers (mapcar #'cdr all-markers))
         (candidates (counsel-mark--get-candidates markers)))
    (when candidates
      (let (register candidate result)
        (while (and (setq register (pop registers))
                    (setq candidate (pop candidates)))
          (let ((point (get-text-property 0 'point candidate))
                (evil-candidate
                 (format "[%s]: %s"
                         (propertize (char-to-string register)
                                     'face 'counsel-evil-register-face)
                         candidate)))
            (push (propertize evil-candidate 'point point) result)))
        result))))

;;;###autoload
(defun counsel-evil-marks (&optional arg)
  "Ivy replacement for `evil-show-marks'.
By default, this function respects `counsel-evil-marks-exclude-registers'.
When ARG is non-nil, display all active evil registers."
  (interactive "P")
  (if (and (boundp 'evil-markers-alist)
           (fboundp 'evil-global-marker-p))
      (let* ((counsel--mark-ring-calling-point (point))
             (candidates (counsel-mark--get-evil-candidates arg)))
        (if candidates
            (counsel-mark--ivy-read "Evil mark: " candidates 'counsel-evil-marks)
          (message "No evil marks are active")))
    (user-error "Required feature `evil' not installed or loaded")))

;;** `counsel-package'
(defvar package--initialized)
(defvar package-alist)
(defvar package-archive-contents)
(defvar package-archives)
(defvar package-user-dir)
(declare-function package-installed-p "package")
(declare-function package-delete "package")
(declare-function package-desc-extras "package")

(defvar counsel-package-history nil
  "History for `counsel-package'.")

(defun counsel--package-candidates ()
  "Return completion alist for `counsel-package'."
  (unless package--initialized
    (package-initialize t))
  (if (or (not package-archive-contents)
          (cl-find-if (lambda (package-archive)
                        (let ((fname
                               (format
                                "%s/archives/%s/archive-contents"
                                package-user-dir (car package-archive))))
                          (or (not (file-exists-p fname))
                              (counsel-file-stale-p fname (* 4 60 60)))))
                      package-archives))
      (package-refresh-contents))
  (sort (mapcar (lambda (entry)
                  (cons (let ((pkg (car entry)))
                          (concat (if (package-installed-p pkg) "-" "+")
                                  (symbol-name pkg)))
                        entry))
                package-archive-contents)
        #'counsel--package-sort))

;;;###autoload
(defun counsel-package ()
  "Install or delete packages.

Packages not currently installed are prefixed with \"+\", and
selecting one of these will try to install it.
Packages currently installed are prefixed with \"-\", and
selecting one of these will try to delete it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Describe package
  \\[ivy-dispatching-done] h: Visit package's homepage"
  (interactive)
  (require 'package)
  (ivy-read "Packages (install +pkg or delete -pkg): "
            (counsel--package-candidates)
            :action #'counsel-package-action
            :require-match t
            :history 'counsel-package-history
            :caller 'counsel-package))

(ivy-configure 'counsel-package
  :initial-input "^+")

(defun counsel-package-action (package)
  "Delete or install PACKAGE."
  (setq package (cadr package))
  (if (package-installed-p package)
      (package-delete (cadr (assq package package-alist)))
    (package-install package)))

(defun counsel-package-action-describe (package)
  "Call `describe-package' on PACKAGE."
  (describe-package (cadr package)))

(defun counsel-package-action-homepage (package)
  "Open homepage for PACKAGE in a WWW browser."
  (let ((url (cdr (assq :url (package-desc-extras (nth 2 package))))))
    (if url
        (browse-url url)
      (message "No homepage specified for package `%s'" (nth 1 package)))))

(defun counsel--package-sort (a b)
  "Sort function for `counsel-package' candidates."
  (let* ((a (car a))
         (b (car b))
         (a-inst (= (string-to-char a) ?+))
         (b-inst (= (string-to-char b) ?+)))
    (or (and a-inst (not b-inst))
        (and (eq a-inst b-inst) (string-lessp a b)))))

(ivy-set-actions
 'counsel-package
 '(("d" counsel-package-action-describe "describe package")
   ("h" counsel-package-action-homepage "open package homepage")))

;;** `counsel-tmm'
(declare-function tmm-get-keymap "tmm" (elt &optional in-x-menu))
(declare-function tmm--completion-table "tmm" (items))

(defalias 'counsel--menu-keymap
  ;; Added in Emacs 28.1.
  (if (fboundp 'menu-bar-keymap)
      #'menu-bar-keymap
    (autoload 'tmm-get-keybind "tmm")
    (declare-function tmm-get-keybind "tmm" (keyseq))
    (lambda () (tmm-get-keybind [menu-bar])))
  "Compatibility shim for `menu-bar-keymap'.")

(defun counsel-tmm-prompt (menu)
  "Select and call an item from the MENU keymap."
  (defvar tmm-km-list)
  (let (out
        choice
        chosen-string)
    (setq tmm-km-list nil)
    (map-keymap (lambda (k v) (tmm-get-keymap (cons k v))) menu)
    (setq tmm-km-list (nreverse tmm-km-list))
    (setq out (ivy-read "Menu bar: " (tmm--completion-table tmm-km-list)
                        :require-match t))
    (setq choice (cdr (assoc out tmm-km-list)))
    (setq chosen-string (car choice))
    (setq choice (cdr choice))
    (cond ((keymapp choice)
           (counsel-tmm-prompt choice))
          ((and choice chosen-string)
           (setq last-command-event chosen-string)
           (call-interactively choice)))))

;;;###autoload
(defun counsel-tmm ()
  "Text-mode emulation of looking and choosing from a menu bar."
  (interactive)
  (require 'tmm)
  (defvar tmm-table-undef)
  (run-hooks 'menu-bar-update-hook)
  (setq tmm-table-undef nil)
  (counsel-tmm-prompt (counsel--menu-keymap)))

;;** `counsel-yank-pop'
(defcustom counsel-yank-pop-truncate-radius 2
  "Number of context lines around `counsel-yank-pop' candidates."
  :type 'integer)

(defun counsel--yank-pop-truncate (str)
  "Truncate STR for use in `counsel-yank-pop'."
  (condition-case nil
      (let* ((lines (split-string str "\n" t))
             (n (length lines))
             (re (ivy-re-to-str ivy--old-re))
             (first-match (cl-position-if
                           (lambda (s) (string-match re s))
                           lines))
             (beg (max 0 (- first-match
                            counsel-yank-pop-truncate-radius)))
             (end (min n (+ first-match
                            counsel-yank-pop-truncate-radius
                            1)))
             (seq (cl-subseq lines beg end)))
        (if (null first-match)
            (error "Could not match %s" str)
          (when (> beg 0)
            (setcar seq (concat "[...] " (car seq))))
          (when (< end n)
            (setcar (last seq)
                    (concat (car (last seq)) " [...]")))
          (mapconcat #'identity seq "\n")))
    (error str)))

(defcustom counsel-yank-pop-separator "\n"
  "Separator for the kill ring strings in `counsel-yank-pop'."
  :type '(choice
          (const :tag "Plain" "\n")
          (const :tag "Dashes" "\n----\n")
          string))

(defun counsel--yank-pop-format-function (cand-pairs)
  "Transform CAND-PAIRS into a string for `counsel-yank-pop'."
  (ivy--format-function-generic
   (lambda (str)
     (mapconcat
      (lambda (s)
        (ivy--add-face s 'ivy-current-match))
      (split-string
       (counsel--yank-pop-truncate str) "\n" t)
      "\n"))
   (lambda (str)
     (counsel--yank-pop-truncate str))
   cand-pairs
   (propertize counsel-yank-pop-separator 'face 'ivy-separator)))

;; Macro to leverage `compiler-macro' of `cl-member' in Emacs >= 24.
(defmacro counsel--idx-of (elt list test)
  "Return index of ELT in LIST, comparing with TEST.
Typically faster than `cl-position' using `equal' on large LIST."
  ;; No `macroexp-let2*' before Emacs 25.
  (macroexp-let2 nil elt elt
    (macroexp-let2 nil list list
      (macroexp-let2 nil tail `(cl-member ,elt ,list :test ,test)
        `(and ,tail (- (length ,list) (length ,tail)))))))

(defun counsel--yank-pop-position (s)
  "Return position of S in `kill-ring' relative to last yank."
  (or (counsel--idx-of s kill-ring-yank-pointer #'equal-including-properties)
      (counsel--idx-of s kill-ring-yank-pointer #'equal)
      (+ (or (counsel--idx-of s kill-ring #'equal-including-properties)
             (counsel--idx-of s kill-ring #'equal))
         (- (length kill-ring-yank-pointer)
            (length kill-ring)))))

(defun counsel-string-non-blank-p (s)
  "Return non-nil if S includes non-blank characters.
Newlines and carriage returns are considered blank."
  (string-match-p "[^\n\r[:blank:]]" s))

(defcustom counsel-yank-pop-filter #'counsel-string-non-blank-p
  "Unary filter function applied to `counsel-yank-pop' candidates.
All elements of `kill-ring' for which this function returns nil
will be destructively removed from `kill-ring' before completion.
All blank strings are deleted from `kill-ring' by default."
  :type '(radio
          (function-item counsel-string-non-blank-p)
          (function-item identity) ;; Faster than the newer `always'.
          (function :tag "Other")))

(defun counsel--equal-w-props ()
  "Return a `hash-table-test' using `equal-including-properties'.
If not available, return nil."
  ;; Added in Emacs 28.
  (when (fboundp 'sxhash-equal-including-properties)
    (let ((name 'counsel--equal-w-props))
      ;; Define the test only once.
      (unless (get name 'hash-table-test)
        (define-hash-table-test name #'equal-including-properties
                                #'sxhash-equal-including-properties))
      name)))

(defun counsel--yank-pop-filter (kills)
  "Apply `counsel-yank-pop-filter' to and deduplicate KILLS.
Equality is defined by `equal-including-properties' for some consistency
with `kill-do-not-save-duplicates' (which is otherwise ignored).  This
function tries to be faster than `cl-delete-duplicates' when possible."
  (let* ((pred counsel-yank-pop-filter)
         (len (length kills))
         ;; Same threshold as `delete-dups'.
         (test (and (> len 100) (counsel--equal-w-props))))
    (if (not test) ;; Slow fallback.
        (cl-delete-duplicates (cl-delete-if-not pred kills)
                              :test #'equal-including-properties
                              :from-end t)
      ;; The rest is `delete-dups' combined with `delete' in a single pass.
      ;; Find first (or no) element that passes through filter.
      (while (unless (funcall pred (car kills))
               (cl-decf len)
               (setq kills (cdr kills))))
      (let ((ht (make-hash-table :test test :size len))
            (tail kills)
            retail)
        ;; Mark it and continue with the rest.
        (puthash (car tail) t ht)
        (while (setq retail (cdr tail))
          (let ((elt (car retail)))
            (if (or (gethash elt ht)
                    (not (funcall pred elt)))
                (setcdr tail (cdr retail))
              (puthash elt t ht)
              (setq tail retail)))))
      kills)))

(defun counsel--yank-pop-kills ()
  "Return filtered `kill-ring' for `counsel-yank-pop' completion.
Both `kill-ring' and `kill-ring-yank-pointer' may be
destructively modified to eliminate duplicates under
`equal-including-properties', satisfy `counsel-yank-pop-filter',
and incorporate `interprogram-paste-function'."
  ;; Protect against `kill-ring' and result of
  ;; `interprogram-paste-function' both being nil
  (ignore-errors (current-kill 0))
  ;; Keep things consistent with the rest of Emacs
  (prog1 (setq kill-ring (counsel--yank-pop-filter kill-ring))
    (setq kill-ring-yank-pointer
          (counsel--yank-pop-filter kill-ring-yank-pointer))))

(defcustom counsel-yank-pop-after-point nil
  "Whether `counsel-yank-pop' yanks after point.
Nil means `counsel-yank-pop' puts point at the end of the yanked
text and mark at its beginning, as per the default \\[yank].
Non-nil means `counsel-yank-pop' swaps the resulting point and
mark, as per \\[universal-argument] \\[yank]."
  :type 'boolean)

(defun counsel-yank-pop-action (s)
  "Like `yank-pop', but insert the kill corresponding to S.
Signal a `buffer-read-only' error if called from a read-only
buffer position."
  (when (and (eq major-mode 'vterm-mode)
             (fboundp 'vterm-insert))
    (let ((inhibit-read-only t))
      (vterm-insert s)))
  (barf-if-buffer-read-only)
  (setq yank-window-start (window-start))
  (unless (eq last-command 'yank)
    ;; Avoid unexpected deletions with `yank-handler' properties.
    (setq yank-undo-function nil))
  (condition-case nil
      (let (;; Deceive `yank-pop'.
            (last-command 'yank)
            ;; Avoid unexpected additions to `kill-ring'.
            interprogram-paste-function)
        (yank-pop (counsel--yank-pop-position s)))
    (error
     ;; Support strings not present in the kill ring.
     (insert s)))
  (when (funcall (if counsel-yank-pop-after-point #'> #'<)
                 (point) (mark t))
    (exchange-point-and-mark t)))

(defun counsel-yank-pop-action-remove (s)
  "Remove all occurrences of S from the kill ring."
  (setq kill-ring
        (cl-delete s kill-ring :test #'equal-including-properties))
  (setq kill-ring-yank-pointer
        (cl-delete s kill-ring-yank-pointer :test #'equal-including-properties))
  ;; Update collection and preselect for next `ivy-call'
  (setf (ivy-state-collection ivy-last) kill-ring)
  (setf (ivy-state-preselect ivy-last)
        (nth (min ivy--index (1- (length kill-ring)))
             kill-ring))
  (ivy--reset-state ivy-last))

(defun counsel-yank-pop-action-rotate (s)
  "Rotate the yanking point to S in the kill ring.
See `current-kill' for how this interacts with the window system
selection."
  (let ((i (counsel--yank-pop-position s)))
    ;; Avoid unexpected additions to `kill-ring'
    (let (interprogram-paste-function)
      (setf (ivy-state-preselect ivy-last) (current-kill i)))
    ;; Manually change window system selection because `current-kill' won't
    (when (and (zerop i)
               yank-pop-change-selection
               interprogram-cut-function)
      (funcall interprogram-cut-function (car kill-ring-yank-pointer))))
  (ivy--reset-state ivy-last))

(defcustom counsel-yank-pop-preselect-last nil
  "Whether `counsel-yank-pop' preselects the last kill by default.

The command `counsel-yank-pop' always preselects the same kill
that `yank-pop' would have inserted, given the same prefix
argument.

When `counsel-yank-pop-preselect-last' is nil (the default), the
prefix argument of `counsel-yank-pop' defaults to 1 (as per
`yank-pop'), which causes the next-to-last kill to be
preselected.  Otherwise, the prefix argument defaults to 0, which
results in the most recent kill being preselected."
  :type 'boolean)

;;;###autoload
(defun counsel-yank-pop (&optional arg)
  "Ivy replacement for `yank-pop'.
With a plain prefix argument (\\[universal-argument]),
temporarily toggle the value of `counsel-yank-pop-after-point'.
Any other value of ARG has the same meaning as in `yank-pop', but
`counsel-yank-pop-preselect-last' determines its default value.
See also `counsel-yank-pop-filter' for how to filter candidates.

Note: Duplicate elements of `kill-ring' are always deleted."
  ;; Do not specify `*' to allow browsing `kill-ring' in read-only buffers
  (interactive "P")
  (let ((kills (or (counsel--yank-pop-kills)
                   (error "Kill ring is empty or blank")))
        (preselect (let (interprogram-paste-function)
                     (current-kill (cond ((nlistp arg)
                                          (prefix-numeric-value arg))
                                         (counsel-yank-pop-preselect-last 0)
                                         (t 1))
                                   t)))
        (counsel-yank-pop-after-point
         (xor (consp arg) counsel-yank-pop-after-point)))
    (unless (eq last-command 'yank)
      (push-mark))
    (ivy-read "kill-ring: " kills
              :require-match t
              :preselect preselect
              :action #'counsel-yank-pop-action
              :caller 'counsel-yank-pop)))

(put #'counsel-yank-pop 'delete-selection 'yank)

(ivy-configure 'counsel-yank-pop
  :height 5
  :format-fn #'counsel--yank-pop-format-function)

(ivy-set-actions
 'counsel-yank-pop
 '(("d" counsel-yank-pop-action-remove "delete")
   ("r" counsel-yank-pop-action-rotate "rotate")))

;;** `counsel-register'
(defvar counsel-register-actions
  '(("\\`buffer" . jump-to-register)
    ("\\`text" . insert-register)
    ("\\`rectangle" . insert-register)
    ("\\`window" . jump-to-register)
    ("\\`frame" . jump-to-register)
    ("\\`[-+]?[0-9]+\\(?:\\.[0-9]\\)?\\'" . insert-register)
    ("\\`\\(?:the \\)?file " . jump-to-register)
    ("\\`keyboard" . jump-to-register)
    ("\\`file-query" . jump-to-register))
  "Alist of (REGEXP . FUNCTION) pairs for `counsel-register'.
Selecting a register whose description matches REGEXP specifies
FUNCTION as the action to take on the register.")

(defvar counsel-register-history nil
  "History for `counsel-register'.")

(defun counsel-register-action (register)
  "Default action for `counsel-register'.

Call a function on REGISTER.  The function is determined by
matching the register's value description against a regexp in
`counsel-register-actions'."
  (let* ((val (get-text-property 0 'register register))
         (desc (register-describe-oneline val))
         (action (cdr (cl-assoc-if (lambda (re) (string-match-p re desc))
                                   counsel-register-actions))))
    (if action
        (funcall action val)
      (error "No action was found for register %s"
             (single-key-description val)))))

;;;###autoload
(defun counsel-register ()
  "Interactively choose a register."
  (interactive)
  (ivy-read "Register: "
            (cl-mapcan
             (lambda (reg)
               (let ((s (funcall register-preview-function reg)))
                 (setq s (substring s 0 (string-match-p "[ \t\n\r]+\\'" s)))
                 (unless (string= s "")
                   (put-text-property 0 1 'register (car reg) s)
                   (list s))))
             register-alist)
            :require-match t
            :history 'counsel-register-history
            :action #'counsel-register-action
            :caller 'counsel-register))

(ivy-configure 'counsel-register
  :sort-fn #'ivy-string<)

;;** `counsel-evil-registers'
(defface counsel-evil-register-face
  '((t :inherit counsel-outline-1))
  "Face for highlighting `evil' registers in ivy."
  :group 'ivy-faces)

;;;###autoload
(defun counsel-evil-registers ()
  "Ivy replacement for `evil-show-registers'."
  (interactive)
  (if (fboundp 'evil-register-list)
      (ivy-read "evil-registers: "
                (cl-loop for (key . val) in (evil-register-list)
                   collect (format "[%s]: %s"
                                   (propertize (char-to-string key)
                                               'face 'counsel-evil-register-face)
                                   (if (stringp val) val "")))
                :require-match t
                :action #'counsel-evil-registers-action
                :caller 'counsel-evil-registers)
    (user-error "Required feature `evil' not installed")))

(ivy-configure 'counsel-evil-registers
  :height 5
  :format-fn #'counsel--yank-pop-format-function)

(defun counsel-evil-registers-action (s)
  "Paste contents of S, trimming the register part.

S will be of the form \"[register]: content\"."
  (with-ivy-window
    (insert
     (replace-regexp-in-string "\\`\\[.*?]: " "" s t t))))

;;** `counsel-imenu'
(defvar imenu-auto-rescan)
(defvar imenu-auto-rescan-maxout)
(declare-function imenu--subalist-p "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun counsel--imenu-candidates ()
  (require 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items))
         (items (cond ((eq major-mode 'emacs-lisp-mode)
                       (counsel-imenu-categorize-functions items))
                      ((and (derived-mode-p 'python-mode)
                            (fboundp 'python-imenu-create-flat-index))
                       (python-imenu-create-flat-index))
                      (t
                       items))))
    (counsel-imenu-get-candidates-from items)))

(defun counsel-imenu-get-candidates-from (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (cl-mapcan
   (lambda (elm)
     (if (imenu--subalist-p elm)
         (counsel-imenu-get-candidates-from
          (cl-loop for (e . v) in (cdr elm) collect
               (cons e (if (integerp v) (copy-marker v) v)))
          ;; pass the prefix to next recursive call
          (concat prefix (if prefix ".") (car elm)))
       (let ((key (concat
                   (when prefix
                     (concat
                      (propertize prefix 'face 'ivy-grep-info)
                      ": "))
                   (car elm))))
         (list (cons key
                     (cons key (if (overlayp (cdr elm))
                                   (overlay-start (cdr elm))
                                 (cdr elm))))))))
   alist))

(defvar counsel-imenu-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'ivy-call-and-recenter)
    map))

(defun counsel-imenu-categorize-functions (items)
  "Categorize all the functions of imenu."
  (let ((fns (cl-remove-if #'listp items :key #'cdr)))
    (if fns
        (append (cl-remove-if #'nlistp items :key #'cdr)
                `(("Functions" ,@fns)))
      items)))

(defun counsel-imenu-action (x)
  (imenu (cdr x)))

(defvar counsel-imenu-history nil
  "History for `counsel-imenu'.")

;;;###autoload
(defun counsel-imenu ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (ivy-read "imenu items: " (counsel--imenu-candidates)
            :preselect (thing-at-point 'symbol)
            :require-match t
            :action #'counsel-imenu-action
            :keymap counsel-imenu-map
            :history 'counsel-imenu-history
            :caller 'counsel-imenu))

;;** `counsel-list-processes'
(defun counsel-list-processes-action-delete (x)
  "Delete process X."
  (delete-process x)
  (setf (ivy-state-collection ivy-last)
        (setq ivy--all-candidates
              (delete x ivy--all-candidates))))

(defun counsel-list-processes-action-switch (x)
  "Switch to buffer of process X."
  (let* ((proc (get-process x))
         (buf (and proc (process-buffer proc))))
    (if buf
        (switch-to-buffer buf)
      (message "Process %s doesn't have a buffer" x))))

;;;###autoload
(defun counsel-list-processes ()
  "Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer."
  (interactive)
  (with-temp-buffer
    (list-processes--refresh))
  (ivy-read "Process: " (mapcar #'process-name (process-list))
            :require-match t
            :action
            '(1
              ("o" counsel-list-processes-action-delete "kill")
              ("s" counsel-list-processes-action-switch "switch"))
            :caller 'counsel-list-processes))

;;** `counsel-ace-link'
(defun counsel-ace-link ()
  "Use Ivy completion for `ace-link'."
  (interactive)
  (let (collection action)
    (cond ((eq major-mode 'Info-mode)
           (setq collection 'ace-link--info-collect)
           (setq action 'ace-link--info-action))
          ((eq major-mode 'help-mode)
           (setq collection 'ace-link--help-collect)
           (setq action 'ace-link--help-action))
          ((eq major-mode 'woman-mode)
           (setq collection 'ace-link--woman-collect)
           (setq action 'ace-link--woman-action))
          ((eq major-mode 'eww-mode)
           (setq collection 'ace-link--eww-collect)
           (setq action 'ace-link--eww-action))
          ((eq major-mode 'compilation-mode)
           (setq collection 'ace-link--eww-collect)
           (setq action 'ace-link--compilation-action))
          ((eq major-mode 'org-mode)
           (setq collection 'ace-link--org-collect)
           (setq action 'ace-link--org-action)))
    (if (null collection)
        (error "%S is not supported" major-mode)
      (ivy-read "Ace-Link: " (funcall collection)
                :action (lambda (x) (funcall action (cdr x)))
                :require-match t
                :caller 'counsel-ace-link))))

;;** `counsel-minibuffer-history'
;;;###autoload
(defun counsel-minibuffer-history ()
  "Browse minibuffer history."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "History: " (ivy-history-contents minibuffer-history-variable)
              :keymap ivy-reverse-i-search-map
              :action (lambda (x)
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties (car x))))
              :caller 'counsel-minibuffer-history)))

;;** `counsel-esh-history'
(defvar comint-input-ring-index)
(defvar eshell-history-index)
(defvar slime-repl-input-history-position)

(defvar counsel-esh--index-last nil
  "Index corresponding to last selection with `counsel-esh-history'.")

(defvar counsel-shell-history--index-last nil
  "Index corresponding to last selection with `counsel-shell-history'.")

(defun counsel--browse-history-action (pair)
  (let ((snd (cdr pair)))
    (cl-case (ivy-state-caller ivy-last)
      (counsel-esh-history
       (setq eshell-history-index snd
             counsel-esh--index-last snd))
      (counsel-shell-history
       (setq comint-input-ring-index snd
             counsel-shell-history--index-last snd))
      ;; Leave this as a no-op. If someone decides to patch
      ;; `slime-repl-previous-input' or one of its utility functions,
      ;; or to add history-replay to Slime, then this section can be
      ;; updated to add the relevant support for those commands.
      (counsel-slime-repl-history
       nil))
    (ivy-completion-in-region-action (car pair))))

(cl-defun counsel--browse-history (ring &key caller)
  "Use Ivy to navigate through RING."
  (let* ((proc (get-buffer-process (current-buffer)))
         (end (point))
         (beg (if proc
                  (min (process-mark proc) end)
                end))
         (input (when (< beg end)
                  (concat "^" (buffer-substring beg end)))))
    (setq ivy-completion-beg beg)
    (setq ivy-completion-end end)
    (ivy-read "History: " (ivy-history-contents ring)
              :keymap ivy-reverse-i-search-map
              :initial-input input
              :action #'counsel--browse-history-action
              :caller caller)))

(defvar eshell-history-ring)
(defvar eshell-matching-input-from-input-string)

;;;###autoload
(defun counsel-esh-history ()
  "Browse Eshell history."
  (interactive)
  (require 'em-hist)
  (counsel--browse-history eshell-history-ring
                           :caller #'counsel-esh-history))

(advice-add 'eshell-previous-matching-input
            :before #'counsel--set-eshell-history-index)
(defun counsel--set-eshell-history-index (&rest _)
  "Reassign `eshell-history-index'."
  (when (and (memq last-command '(ivy-alt-done ivy-done))
             (equal (ivy-state-caller ivy-last) 'counsel-esh-history))
    (setq eshell-history-index counsel-esh--index-last)))

(defvar comint-input-ring)
(defvar comint-matching-input-from-input-string)

;;;###autoload
(defun counsel-shell-history ()
  "Browse shell history."
  (interactive)
  (require 'comint)
  (counsel--browse-history comint-input-ring
                           :caller #'counsel-shell-history))

(advice-add 'comint-previous-matching-input
            :before #'counsel--set-comint-history-index)
(defun counsel--set-comint-history-index (&rest _)
  "Reassign `comint-input-ring-index'."
  (when (and (memq last-command '(ivy-alt-done ivy-done))
             (equal (ivy-state-caller ivy-last) 'counsel-shell-history))
    (setq comint-input-ring-index counsel-shell-history--index-last)))

(defvar slime-repl-input-history)

;;;###autoload
(defun counsel-slime-repl-history ()
  "Browse Slime REPL history."
  (interactive)
  (require 'slime-repl)
  (counsel--browse-history slime-repl-input-history
                           :caller #'counsel-slime-repl-history))

;; TODO: add advice for slime-repl-input-previous/next to properly
;; reassign the ring index and match string.  This requires a case for
;; `counsel-slime-repl-history' within
;; `counsel--browse-history-action'.

;;** `counsel-hydra-heads'
(defvar hydra-curr-body-fn)
(declare-function hydra-keyboard-quit "ext:hydra")

;;;###autoload
(defun counsel-hydra-heads ()
  "Call a head of the current/last hydra."
  (interactive)
  (let* ((base (substring
                (prin1-to-string hydra-curr-body-fn)
                0 -4))
         (heads (symbol-value (intern (concat base "heads"))))
         (keymap (symbol-value (intern (concat base "keymap"))))
         (head-names
          (mapcar (lambda (x)
                    (cons
                     (if (nth 2 x)
                         (format "[%s] %S (%s)" (nth 0 x) (nth 1 x) (nth 2 x))
                       (format "[%s] %S" (nth 0 x) (nth 1 x)))
                     (lookup-key keymap (kbd (nth 0 x)))))
                  heads)))
    (ivy-read "head: " head-names
              :action (lambda (x) (call-interactively (cdr x))))
    (hydra-keyboard-quit)))
;;** `counsel-semantic'
(declare-function semantic-tag-start "semantic/tag")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-name "semantic/tag")
(declare-function semantic-tag-put-attribute "semantic/tag")
(declare-function semantic-tag-get-attribute "semantic/tag")
(declare-function semantic-fetch-tags "semantic")
(declare-function semantic-format-tag-summarize "semantic/format")
(declare-function semantic-active-p "semantic/fw")

(defun counsel-semantic-action (x)
  "Got to semantic TAG."
  (goto-char (semantic-tag-start (cdr x))))

(defvar counsel-semantic-history nil
  "History for `counsel-semantic'.")

(defun counsel-semantic-format-tag (tag)
  "Return a pretty string representation of TAG."
  (let ((depth (or (semantic-tag-get-attribute tag :depth) 0))
        (parent (semantic-tag-get-attribute tag :parent)))
    (concat (make-string (* depth 2) ?\ )
            (if parent
                (concat "(" parent ") ")
              "")
            (semantic-format-tag-summarize tag nil t))))

(defun counsel-flatten-forest (func treep forest)
  "Use FUNC and TREEP to flatten FOREST.
FUNC is applied to each node.
TREEP is used to expand internal nodes."
  (cl-labels ((reducer (forest out depth)
                (dolist (tree forest)
                  (let ((this (cons (funcall func tree depth) out))
                        (leafs (funcall treep tree)))
                    (setq out
                          (if leafs
                              (reducer leafs this (1+ depth))
                            this))))
                out))
    (nreverse (reducer forest nil 0))))

(defun counsel-semantic-tags ()
  "Fetch semantic tags."
  (counsel-flatten-forest
   (lambda (tree depth)
     (semantic-tag-put-attribute tree :depth depth))
   (lambda (tag)
     (when (eq (semantic-tag-class tag) 'type)
       (let ((name (semantic-tag-name tag)))
         (mapcar
          (lambda (x) (semantic-tag-put-attribute x :parent name))
          (semantic-tag-get-attribute tag :members)))))
   (semantic-fetch-tags)))

;;;###autoload
(defun counsel-semantic ()
  "Jump to a semantic tag in the current buffer."
  (interactive)
  (let ((tags (mapcar
               (lambda (x)
                 (cons
                  (counsel-semantic-format-tag x)
                  x))
               (counsel-semantic-tags))))
    (ivy-read "tag: " tags
              :action #'counsel-semantic-action
              :history 'counsel-semantic-history
              :caller 'counsel-semantic)))

;;;###autoload
(defun counsel-semantic-or-imenu ()
  (interactive)
  (require 'semantic/fw)
  (if (semantic-active-p)
      (counsel-semantic)
    (counsel-imenu)))

;;** `counsel-outline'
(declare-function org-trim "org-macs")

(defcustom counsel-outline-face-style nil
  "Determines how to style outline headings during completion.

If `org', the faces `counsel-outline-1' through
`counsel-outline-8' are applied in a similar way to Org.
Note that no cycling is performed, so headings on levels 9 and
higher are not styled.

If `verbatim', the faces used in the buffer are applied.  For
simple headlines in `org-mode' buffers, this is usually the same
as the `org' setting, except that it depends on how much of the
buffer has been completely fontified.  If your buffer exceeds a
certain size, headlines are styled lazily depending on which
parts of the tree are visible.  Headlines which are not yet
styled in the buffer will appear unstyled in the minibuffer as
well.  If your headlines contain parts which are fontified
differently than the headline itself (e.g. TODO keywords, tags,
links) and you want these parts to be styled properly, verbatim
is the way to go; otherwise you are probably better off using the
`org' setting instead.

If `custom', the faces defined in `counsel-outline-custom-faces'
are applied.  Note that no cycling is performed, so if there is
no face defined for a certain level, headlines on that level will
not be styled.

If `nil', all headlines are highlighted using
`counsel-outline-default'.

For displaying tags and TODO keywords in `org-mode' buffers, see
`counsel-org-headline-display-tags' and
`counsel-org-headline-display-todo', respectively."
  :type '(choice
          (const :tag "Same as org-mode" org)
          (const :tag "Verbatim" verbatim)
          (const :tag "Custom" custom)
          (const :tag "No style" nil)))

(defcustom counsel-outline-custom-faces nil
  "List of faces for custom display of outline headings.

Headlines on level N are fontified with the Nth entry of this
list, starting with N = 1.  Headline levels with no corresponding
entry in this list will not be styled.

This variable has no effect unless `counsel-outline-face-style'
is set to `custom'."
  :type '(repeat face))

(defun counsel-outline-title ()
  "Return title of current outline heading.
Intended as a value for the `:outline-title' setting in
`counsel-outline-settings', which see."
  (buffer-substring (point) (line-end-position)))

(defun counsel-outline-title-org ()
  "Return title of current outline heading.
Like `counsel-outline-title' (which see), but for `org-mode'
buffers."
  (let ((statistics-re "\\[[0-9]*\\(?:%\\|/[0-9]*\\)]")
        (heading (apply #'org-get-heading (counsel--org-get-heading-args))))
    (cond (counsel-org-headline-display-statistics
           heading)
          (heading
           (org-trim (replace-regexp-in-string
                      statistics-re " " heading t t))))))

(defun counsel-outline-title-markdown ()
  "Return title of current outline heading.
Like `counsel-outline-title' (which see), but for
`markdown-mode' (from the eponymous package) buffers."
  ;; `outline-regexp' is set by `markdown-mode' to match both setext
  ;; (underline) and atx (hash) headings (see
  ;; `markdown-regex-header').
  (or (match-string 1)                  ; setext heading title
      (match-string 5)))                ; atx heading title

(defun counsel-outline-title-latex ()
  "Return title of current outline heading.
Like `counsel-outline-title' (which see), but for `latex-mode'
buffers."
  ;; `outline-regexp' is set by `latex-mode' (see variable
  ;; `latex-section-alist' for the built-in mode or function
  ;; `LaTeX-outline-regexp' for the AUCTeX package) to match section
  ;; macros, in which case we get the section name, as well as
  ;; `\appendix', `\documentclass', `\begin{document}', and
  ;; `\end{document}', in which case we simply return that.
  (if (and (assoc (match-string 1)                             ; Macro name
                  (or (bound-and-true-p LaTeX-section-list)    ; AUCTeX
                      (bound-and-true-p latex-section-alist))) ; Built-in
           (progn
             ;; Point is at end of macro name, skip stars and optional args
             (skip-chars-forward "*")
             (while (looking-at-p "\\[")
               (forward-list))
             ;; First mandatory arg should be section title
             (looking-at-p "{")))
      (buffer-substring (1+ (point)) (1- (progn (forward-list) (point))))
    (buffer-substring (line-beginning-position) (point))))

(defun counsel-outline-level-emacs-lisp ()
  "Return level of current outline heading.
Like `lisp-outline-level', but adapted for the `:outline-level'
setting in `counsel-outline-settings', which see."
  (if (looking-at ";;\\([;*]+\\)")
      (- (match-end 1) (match-beginning 1))
    (funcall outline-level)))

(defvar counsel-outline--preselect 0
  "Index of the preselected candidate in `counsel-outline'.")

(defun counsel-outline-candidates (&optional settings prefix)
  "Return an alist of outline heading completion candidates.
Each element is a pair (HEADING . MARKER), where the string
HEADING is located at the position of MARKER.  SETTINGS is a
plist entry from `counsel-outline-settings', which see.
PREFIX is a string prepended to all candidates."
  (let* ((bol-regex (concat "^\\(?:"
                            (or (plist-get settings :outline-regexp)
                                outline-regexp)
                            "\\)"))
         (outline-title-fn (or (plist-get settings :outline-title)
                               #'counsel-outline-title))
         (outline-level-fn (or (plist-get settings :outline-level)
                               outline-level))
         (display-style (or (plist-get settings :display-style)
                            counsel-outline-display-style))
         (path-separator (or (plist-get settings :path-separator)
                             counsel-outline-path-separator))
         (face-style (or (plist-get settings :face-style)
                         counsel-outline-face-style))
         (custom-faces (or (plist-get settings :custom-faces)
                           counsel-outline-custom-faces))
         (stack-level 0)
         (orig-point (point))
         (stack (and prefix (list (counsel-outline--add-face
                                   prefix 0 face-style custom-faces))))
         cands name level marker)
    (save-excursion
      (setq counsel-outline--preselect 0)
      (goto-char (point-min))
      (while (re-search-forward bol-regex nil t)
        (save-excursion
          (setq name (or (save-match-data
                           (funcall outline-title-fn))
                         ""))
          (goto-char (match-beginning 0))
          (setq marker (point-marker))
          (setq level (funcall outline-level-fn))
          (cond ((eq display-style 'path)
                 ;; Update stack.  The empty entry guards against incorrect
                 ;; headline hierarchies, e.g. a level 3 headline
                 ;; immediately following a level 1 entry.
                 (while (<= level stack-level)
                   (pop stack)
                   (cl-decf stack-level))
                 (while (> level stack-level)
                   (push "" stack)
                   (cl-incf stack-level))
                 (setf (car stack)
                       (counsel-outline--add-face
                        name level face-style custom-faces))
                 (setq name (mapconcat #'identity
                                       (reverse stack)
                                       path-separator)))
                (t
                 (when (eq display-style 'headline)
                   (setq name (concat (make-string level ?*) " " name)))
                 (setq name (counsel-outline--add-face
                             name level face-style custom-faces))))
          (push (cons name marker) cands))
        (unless (or (string= name "")
                    (< orig-point marker))
          (cl-incf counsel-outline--preselect))))
    (nreverse cands)))

(defun counsel-outline--add-face (name level &optional face-style custom-faces)
  "Set the `face' property on headline NAME according to LEVEL.
FACE-STYLE and CUSTOM-FACES override `counsel-outline-face-style'
and `counsel-outline-custom-faces', respectively, which determine
the face to apply."
  (let ((face (cl-case (or face-style counsel-outline-face-style)
                (verbatim)
                (custom (nth (1- level)
                             (or custom-faces counsel-outline-custom-faces)))
                (org (format "counsel-outline-%d" level))
                (t 'counsel-outline-default))))
    (when face
      (put-text-property 0 (length name) 'face face name)))
  name)

(defun counsel-outline-action (x)
  "Go to outline X."
  (goto-char (cdr x)))

;;;###autoload
(defun counsel-outline ()
  "Jump to an outline heading with completion."
  (interactive)
  (let ((settings (cdr (assq major-mode counsel-outline-settings))))
    (ivy-read "Outline: " (counsel-outline-candidates settings)
              :action (or (plist-get settings :action)
                          #'counsel-outline-action)
              :history (or (plist-get settings :history)
                           'counsel-outline-history)
              :preselect (max (1- counsel-outline--preselect) 0)
              :caller (or (plist-get settings :caller)
                          'counsel-outline))))

;;** `counsel-ibuffer'
(defvar counsel-ibuffer--buffer-name nil
  "Name of the buffer to use for `counsel-ibuffer'.")

;;;###autoload
(defun counsel-ibuffer (&optional name)
  "Use ibuffer to switch to another buffer.
NAME specifies the name of the buffer (defaults to \"*Ibuffer*\")."
  (interactive)
  (setq counsel-ibuffer--buffer-name (or name "*Ibuffer*"))
  (ivy-read "Switch to buffer: " (counsel--ibuffer-get-buffers)
            :history 'counsel-ibuffer-history
            :action #'counsel-ibuffer-visit-buffer
            :caller 'counsel-ibuffer))

(declare-function ibuffer-update "ibuffer")
(declare-function ibuffer-current-buffer "ibuffer")
(declare-function ibuffer-forward-line "ibuffer")
(defvar ibuffer-movement-cycle)

(defun counsel--ibuffer-get-buffers ()
  "Return an alist with buffer completion candidates from Ibuffer.
The keys are buffer-related lines from Ibuffer as strings, and
the values are the corresponding buffer objects."
  (let ((oldbuf (get-buffer counsel-ibuffer--buffer-name)))
    (unless oldbuf
      ;; Avoid messing with the user's precious window/frame configuration.
      (save-window-excursion
        (let ((display-buffer-overriding-action
               '(display-buffer-same-window (inhibit-same-window . nil))))
          (ibuffer nil counsel-ibuffer--buffer-name nil t))))
    (with-current-buffer counsel-ibuffer--buffer-name
      (when oldbuf
        ;; Forcibly update possibly stale existing buffer.
        (ibuffer-update nil t))
      (goto-char (point-min))
      (let ((ibuffer-movement-cycle nil)
            entries)
        (while (not (eobp))
          (ibuffer-forward-line 1 t)
          (let ((buf (ibuffer-current-buffer)))
            ;; We are only interested in buffers we can actually visit.
            ;; This filters out headings and other unusable entries.
            (when (buffer-live-p buf)
              (push (cons (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))
                          buf)
                    entries))))
        (nreverse entries)))))

(defun counsel-ibuffer-visit-buffer (x)
  "Switch to buffer of candidate X."
  (switch-to-buffer (or (cdr-safe x) x)))

(defun counsel-ibuffer-visit-buffer-other-window (x)
  "Switch to buffer of candidate X in another window."
  (switch-to-buffer-other-window (or (cdr-safe x) x)))

(defun counsel-ibuffer-visit-ibuffer (_)
  "Switch to Ibuffer buffer."
  (switch-to-buffer counsel-ibuffer--buffer-name))

(ivy-set-actions
 'counsel-ibuffer
 '(("j" counsel-ibuffer-visit-buffer-other-window "other window")
   ("v" counsel-ibuffer-visit-ibuffer "switch to Ibuffer")))

;;** `counsel-switch-to-shell-buffer'
(defun counsel--buffers-with-mode (mode)
  "Return names of buffers with MODE as their `major-mode'."
  (let (bufs)
    (dolist (buf (buffer-list))
      (when (eq (buffer-local-value 'major-mode buf) mode)
        (push (buffer-name buf) bufs)))
    (nreverse bufs)))

(declare-function shell-mode "shell")

;;;###autoload
(defun counsel-switch-to-shell-buffer ()
  "Switch to a shell buffer, or create one."
  (interactive)
  (ivy-read "Shell buffer: " (counsel--buffers-with-mode #'shell-mode)
            :action #'counsel--switch-to-shell
            :caller 'counsel-switch-to-shell-buffer))

(defun counsel--switch-to-shell (name)
  "Display shell buffer with NAME and select its window.
Reuse any existing window already displaying the named buffer.
If there is no such buffer, start a new `shell' with NAME."
  (if (get-buffer name)
      (pop-to-buffer name '((display-buffer-reuse-window
                             display-buffer-same-window)
                            (inhibit-same-window . nil)
                            (reusable-frames . visible)))
    (shell name)))

;;** `counsel-unicode-char'
(defvar counsel-unicode-char-history nil
  "History for `counsel-unicode-char'.")

(defun counsel--unicode-names ()
  "Return formatted and sorted list of `ucs-names'.
The result of `ucs-names' is mostly, but not completely, sorted,
so this function ensures lexicographic order."
  (let* (cands
         (table (ucs-names))            ; Either hash map or alist
         (fmt (lambda (name code)       ; Common format function
                (let ((cand (format "%06X %-58s %c" code name code)))
                  (put-text-property 0 1 'code code cand)
                  (push cand cands)))))
    (if (not (hash-table-p table))
        ;; Support `ucs-names' returning an alist in Emacs < 26.
        ;; The result of `ucs-names' comes pre-reversed so no need to repeat.
        (dolist (entry table)
          (funcall fmt (car entry) (cdr entry)))
      (maphash fmt table)
      ;; Reverse to speed up sorting
      (setq cands (nreverse cands)))
    (sort cands #'string-lessp)))

(defvar counsel--unicode-table
  (lazy-completion-table counsel--unicode-table counsel--unicode-names)
  "Lazy completion table for `counsel-unicode-char'.
Candidates comprise `counsel--unicode-names', which see.")

;;;###autoload
(defun counsel-unicode-char (&optional count)
  "Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1."
  (interactive "p")
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (ivy-read "Unicode name: " counsel--unicode-table
            :history 'counsel-unicode-char-history
            :action (lambda (name)
                      (with-ivy-window
                        (delete-region ivy-completion-beg ivy-completion-end)
                        (setq ivy-completion-beg (point))
                        (insert-char (get-text-property 0 'code name) count)
                        (setq ivy-completion-end (point))))
            :caller 'counsel-unicode-char))

(ivy-configure 'counsel-unicode-char
  :sort-fn #'ivy-string<)

(defun counsel-unicode-copy (name)
  "Ivy action to copy the unicode from NAME to the kill ring."
  (kill-new (char-to-string (get-text-property 0 'code name))))

(ivy-set-actions
 'counsel-unicode-char
 '(("w" counsel-unicode-copy "copy")))

;;** `counsel-colors'
(defun counsel-colors-action-insert-hex (color)
  "Insert the hexadecimal RGB value of COLOR."
  (insert (get-text-property 0 'hex color)))

(defun counsel-colors-action-kill-hex (color)
  "Kill the hexadecimal RGB value of COLOR."
  (kill-new (get-text-property 0 'hex color)))

;;** `counsel-colors-emacs'
(defvar counsel-colors-emacs-history ()
  "History for `counsel-colors-emacs'.")

(defun counsel-colors--name-to-hex (name)
  "Return hexadecimal RGB value of color with NAME.

Return nil if NAME does not designate a valid color."
  (let ((rgb (color-name-to-rgb name)))
    (when rgb
      (apply #'color-rgb-to-hex rgb))))

(defvar shr-color-visible-luminance-min)
(declare-function shr-color-visible "shr-color")
(defvar counsel--colors-format "%-20s %s %s%s")

(defun counsel--colors-emacs-format-function (colors)
  "Format function for `counsel-colors-emacs'."
  (require 'shr-color)
  (let* ((blank (make-string 10 ?\s))
         (formatter
          (lambda (color)
            (let ((fg (list :foreground color)))
              (format counsel--colors-format color
                      (propertize (get-text-property 0 'hex color) 'face fg)
                      (propertize blank 'face (list :background color))
                      (propertize (mapconcat (lambda (dup)
                                               (concat " " dup))
                                             (get-text-property 0 'dups color)
                                             ",")
                                  'face fg))))))
    (ivy--format-function-generic
     (lambda (color)
       (let* ((hex (get-text-property 0 'hex color))
              (shr-color-visible-luminance-min 100)
              (fg (cadr (shr-color-visible hex "black" t))))
         (propertize (funcall formatter color)
                     'face (list :foreground fg :background hex))))
     formatter colors "\n")))

(defun counsel--colors-web-format-function (colors)
  "Format function for `counsel-colors-web'."
  (require 'shr-color)
  (let* ((blank (make-string 10 ?\s))
         (formatter (lambda (color)
                      (let ((hex (get-text-property 0 'hex color)))
                        (format counsel--colors-format color
                                (propertize hex 'face (list :foreground hex))
                                (propertize blank 'face (list :background hex)))))))
    (ivy--format-function-generic
     (lambda (color)
       (let* ((hex (get-text-property 0 'hex color))
              (shr-color-visible-luminance-min 100)
              (fg (cadr (shr-color-visible hex "black" t))))
         (propertize (funcall formatter color)
                     'face (list :foreground fg :background hex))))
     formatter colors "\n")))

;; No longer preloaded in Emacs 28.
(autoload 'list-colors-duplicates "facemenu")

;;;###autoload
(defun counsel-colors-emacs ()
  "Show a list of all supported colors for a particular frame.

You can insert or kill the name or hexadecimal RGB value of the
selected color."
  (interactive)
  (let* ((colors
          (delete nil
                  (mapcar (lambda (cell)
                            (let* ((name (car cell))
                                   (dups (cdr cell))
                                   (hex (counsel-colors--name-to-hex name)))
                              (when hex
                                (propertize name 'hex hex 'dups dups))))
                          (list-colors-duplicates))))
         (counsel--colors-format
          (format "%%-%ds %%s %%s%%s"
                  (apply #'max 0 (mapcar #'string-width colors)))))
    (ivy-read "Emacs color: " colors
              :require-match t
              :history 'counsel-colors-emacs-history
              :action #'insert
              :caller 'counsel-colors-emacs)))
(ivy-configure 'counsel-colors-emacs
  :format-fn #'counsel--colors-emacs-format-function)

(ivy-set-actions
 'counsel-colors-emacs
 '(("h" counsel-colors-action-insert-hex "insert hexadecimal value")
   ("H" counsel-colors-action-kill-hex "kill hexadecimal value")))

;;** `counsel-colors-web'
(defvar shr-color-html-colors-alist)

(defun counsel-colors--web-alist ()
  "Return list of CSS colors for `counsel-colors-web'."
  (require 'shr-color)
  (let* ((alist (copy-alist shr-color-html-colors-alist))
         (mp  (assoc "MediumPurple"  alist))
         (pvr (assoc "PaleVioletRed" alist))
         (rp  (assoc "RebeccaPurple" alist)))
    ;; Backport GNU Emacs bug#30377
    (when mp (setcdr mp "#9370db"))
    (when pvr (setcdr pvr "#db7093"))
    (unless rp (push (cons "rebeccapurple" "#663399") alist))
    (sort (mapcar (lambda (cell)
                    (propertize (downcase (car cell))
                                'hex (downcase (cdr cell))))
                  alist)
          #'string-lessp)))

(defvar counsel-colors-web-history ()
  "History for `counsel-colors-web'.")

;;;###autoload
(defun counsel-colors-web ()
  "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or hexadecimal RGB value of the
selected color."
  (interactive)
  (let* ((colors (counsel-colors--web-alist))
         (counsel--colors-format
          (format "%%-%ds %%s %%s"
                  (apply #'max 0 (mapcar #'string-width colors)))))
    (ivy-read "Web color: " colors
              :require-match t
              :history 'counsel-colors-web-history
              :action #'insert
              :caller 'counsel-colors-web)))

(ivy-configure 'counsel-colors-web
  :sort-fn #'ivy-string<
  :format-fn #'counsel--colors-web-format-function)

(ivy-set-actions
 'counsel-colors-web
 '(("h" counsel-colors-action-insert-hex "insert hexadecimal value")
   ("H" counsel-colors-action-kill-hex "kill hexadecimal value")))

;;** `counsel-fonts'
(defvar counsel-fonts-history ()
  "History for `counsel-fonts'.")

;;;###autoload
(defun counsel-fonts ()
  "Show a list of all supported font families for a particular frame.

You can insert or kill the name of the selected font."
  (interactive)
  (let ((current-font
         (symbol-name (font-get (face-attribute 'default :font) :family))))
    (ivy-read "Font: " (delete-dups (font-family-list))
              :preselect current-font
              :require-match t
              :history 'counsel-fonts-history
              :action #'insert
              :caller 'counsel-fonts)))

(ivy-configure 'counsel-fonts
  :display-transformer-fn #'counsel--font-with-sample)

(defun counsel--font-with-sample (font-name)
  "Format function for `counsel-fonts'."
  (format "%-75s%s" font-name
          (propertize "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      'face (list :family font-name))))

;;** `counsel-kmacro'
(defvar counsel-kmacro-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-k") #'counsel-kmacro-kill)
    map))

(defun counsel-kmacro-kill ()
  "Kill the line, or delete the keyboard macro."
  (interactive)
  (if (not (eolp))
      (ivy-kill-line)
    (counsel-kmacro-action-delete-kmacro
     (assoc
      (ivy-state-current ivy-last)
      (ivy-state-collection ivy-last)))
    (ivy--kill-current-candidate)))

(defvar kmacro-ring)
(defvar kmacro-initial-counter-value)
(defvar kmacro-counter)
(defvar kmacro-counter-value-start)
(defvar kmacro-counter-format-start)

;;;###autoload
(defun counsel-kmacro ()
  "Interactively choose and run a keyboard macro.

With prefix argument, run macro that many times.

Macros are run using the current value of `kmacro-counter-value'
and their respective counter format. Displayed next to each macro is
the counter's format and initial value.

One can use actions to copy the counter format or initial counter
value of a macro, using them for a new macro."
  (interactive)
  (if (or last-kbd-macro kmacro-ring)
      (ivy-read
       (concat "Execute macro (counter at "
               (number-to-string (or kmacro-initial-counter-value kmacro-counter))
               "): ")
       (counsel--kmacro-candidates)
       :keymap counsel-kmacro-map
       :require-match t
       :action #'counsel-kmacro-action-run
       :caller 'counsel-kmacro)
    (user-error "No keyboard macros defined")))

(ivy-configure 'counsel-kmacro
  :format-fn #'counsel--kmacro-format-function)

(defcustom counsel-kmacro-separator "\n------------------------\n"
  "Separator displayed between keyboard macros in `counsel-kmacro'."
  :type 'string)

(defun counsel--kmacro-format-function (formatted-kmacro)
  "Transform FORMATTED-KMACRO into a string for `counsel-kmacro'."
  (ivy--format-function-generic
   (lambda (str) (ivy--add-face str 'ivy-current-match))
   (lambda (str) str)
   formatted-kmacro
   (propertize counsel-kmacro-separator 'face 'ivy-separator)))

(defun counsel--kmacro-candidates ()
  "Create the list of keyboard macros used by `counsel-kmacro'.
This is a combination of `kmacro-ring' and, together in a list,
`last-kbd-macro', `kmacro-counter-format-start', and
`kmacro-counter-value-start'."
  (mapcar
   (lambda (kmacro)
     (cons
      (concat "(" (nth 2 kmacro) "," (number-to-string (nth 1 kmacro)) "): "
              (condition-case nil
                  (format-kbd-macro (if (listp kmacro) (car kmacro) kmacro) 1)
                ;; Recover from error from `edmacro-fix-menu-commands'.
                (error "Warning: Cannot display macros containing mouse clicks")))
      kmacro))
   (cons
    (if (listp last-kbd-macro)
        last-kbd-macro
      (list
       last-kbd-macro
       kmacro-counter-value-start
       kmacro-counter-format-start))
    kmacro-ring)))

(defun counsel-kmacro-action-run (x)
  "Run keyboard macro."
  (let* ((actual-kmacro (cdr x))
         (kmacro-keys (nth 0 actual-kmacro))
         (kmacro-counter-format-start (nth 2 actual-kmacro)))
    ;; With prefix argument, call the macro that many times.
    (kmacro-call-macro (or current-prefix-arg 1) t nil kmacro-keys)))

(defun counsel-kmacro-action-delete-kmacro (x)
  "Delete a keyboard macro from within `counsel-kmacro'.

Either delete a macro from `kmacro-ring', or set `last-kbd-macro'
to the popped head of the ring."
  (let ((actual-macro (cdr x)))
    (if (eq (nth 0 actual-macro) last-kbd-macro)
        (setq last-kbd-macro
              (if (eq kmacro-ring nil)
                  nil
                (let ((prev-macro (pop kmacro-ring)))
                  (if (listp prev-macro)
                      (nth 0 prev-macro)
                    prev-macro))))
      (setq kmacro-ring (delq actual-macro kmacro-ring)))))

(defun counsel-kmacro-action-copy-initial-counter-value (x)
  "Pass an existing keyboard macro's original value to `kmacro-set-counter'.
This value will be used by the next executed macro, or as an
initial value by the next macro defined.

Note that calling an existing macro that itself uses a counter
effectively resets the initial counter value for the next defined macro
to 0."
  ;; NOTE:
  ;; Calling `kmacro-start-macro' without an argument sets `kmacro-counter'
  ;; to 0 if `kmacro-initial-counter'is nil, and sets `kmacro-initial-counter'
  ;; to nil regardless.
  ;; Using `kmacro-insert-counter' sets `kmacro-initial-counter' to nil.
  (let* ((actual-kmacro (cdr x))
         (number (nth 1 actual-kmacro)))
    (kmacro-set-counter number)))

(defun counsel-kmacro-action-copy-counter-format-for-new-macro (x)
  "Set the default keyboard macro counter format.
This sets `kmacro-default-counter-format' to the counter format
of an existing keyboard macro.

This will apply to the next macro a user defines."
  (let* ((actual-kmacro (cdr x))
         (format (nth 2 actual-kmacro)))
    (kmacro-set-format format)))

(declare-function kmacro-cycle-ring-previous "kmacro" (&optional arg))
(declare-function kmacro-set-format "kmacro" (format))
(declare-function kmacro-set-counter "kmacro" (arg))

(defun counsel-kmacro-action-cycle-ring-to-macro (x)
  "Cycle `kmacro-ring' until `last-kbd-macro' is the selected macro.
This is convenient when using \\[kmacro-end-or-call-macro] to call macros.
Note that cycling the ring changes the starting value of the current macro
to changes the current macro counter."
  (let ((actual-kmacro (cdr x)))
    (unless (equal last-kbd-macro
                   (if (listp last-kbd-macro)
                       last-kbd-macro
                     (car actual-kmacro)))
      (while (not (equal actual-kmacro
                         (car kmacro-ring)))
        (kmacro-cycle-ring-previous))
      ;; Once selected macro is at the head of the ring,
      ;; cycle one last time.
      (kmacro-cycle-ring-previous))))

(defun counsel-kmacro-action-set-saved-starting-counter (x)
  "Set the starting counter value of the chosen macro.

By default, sets to current value of the counter. It has no
effect when selecting the current macro.

Normally, when cycling keyboard macro ring with \\[kmacro-cycle-ring-previous]
or \\[kmacro-cycle-ring-next], the current value of the macro counter is
included with the current macro definition. Then, when cycling
back, that counter value is restored.  This function is meant to
achieve something similar when cycling macros in the context of
using `counsel-kmacro', which does not use different counter
values when running different macros."
  (let ((actual-kmacro (cdr x))
        (default-kmacro-counter-string (number-to-string kmacro-counter)))
    (setq kmacro-ring (mapcar (lambda (this-macro-in-ring)
                                (if (equal this-macro-in-ring actual-kmacro)
                                    (list (car this-macro-in-ring)
                                          (read-from-minibuffer (concat "Set initial counter for macro (default: "
                                                                        default-kmacro-counter-string
                                                                        "): ")
                                                                nil nil t nil
                                                                default-kmacro-counter-string)
                                          (cl-caddr this-macro-in-ring))
                                  this-macro-in-ring))
                              kmacro-ring))))

(defun counsel-kmacro-action-execute-after-prompt (x)
  "Execute an existing keyboard macro, prompting for a starting counter value, a
counter format, and the number of times to execute the macro.

If called with a prefix, will suggest that value for both the
counter value and iteration amount."
  (let* ((default-string (if current-prefix-arg
                             (number-to-string current-prefix-arg)
                           nil))
         (actual-kmacro (cdr x))
         (kmacro-keys (nth 0 actual-kmacro))
         (kmacro-starting-counter (number-to-string (nth 1 actual-kmacro)))
         (kmacro-starting-format (nth 2 actual-kmacro))
         (number-of-iterations
          (read-from-minibuffer
           (concat "Enter number of iterations for macro (default: "
                   (or default-string (number-to-string 2))
                   "): ")
           nil nil t nil
           (or default-string (number-to-string 2))))
         (kmacro-initial-counter-value
          (read-from-minibuffer
           (concat "Enter a starting counter for macro (default: "
                   (or default-string kmacro-starting-counter)
                   "): ")
           nil nil t nil
           (or default-string kmacro-starting-counter)))
         (kmacro-counter-format-start
          (symbol-name (read-from-minibuffer
                        (concat "Enter format for macro counter (default: "
                                kmacro-starting-format
                                "): ")
                        nil nil t nil
                        kmacro-starting-format))))
    (kmacro-call-macro number-of-iterations t nil kmacro-keys)))

(ivy-set-actions
 'counsel-kmacro
 '(("c" counsel-kmacro-action-cycle-ring-to-macro "cycle to")
   ("d" counsel-kmacro-action-delete-kmacro "delete")
   ("e" counsel-kmacro-action-execute-after-prompt "execute after prompt")
   ("f" counsel-kmacro-action-copy-counter-format-for-new-macro "copy counter format for new macro")
   ("s" counsel-kmacro-action-set-saved-starting-counter "set this counter value")
   ("v" counsel-kmacro-action-copy-initial-counter-value "copy initial counter value")))

;;** `counsel-geiser-doc-look-up-manual'
(declare-function geiser-doc-manual-for-symbol "ext:geiser-doc")
(defvar geiser-completion-symbol-list-func)

(defvar counsel-geiser-doc-look-up-manual-history ()
  "History for `counsel-geiser-doc-look-up-manual'.")

;;;###autoload
(defun counsel-geiser-doc-look-up-manual ()
  "Search Scheme documentation."
  (interactive)
  (ivy-read "Symbol: " geiser-completion-symbol-list-func
            :require-match t
            :history 'counsel-geiser-doc-look-up-manual-history
            :action (lambda (cand)
                      (geiser-doc-manual-for-symbol (intern cand)))
            :caller 'counsel-geiser-doc-look-up-manual))

;;* Misc. OS
;;** `counsel-rhythmbox'
(declare-function dbus-call-method "dbus")
(declare-function dbus-get-property "dbus")

(defun counsel--run (&rest program-and-args)
  (let ((name (mapconcat #'identity program-and-args " ")))
    (apply #'start-process name nil program-and-args)
    name))

(defun counsel--sl (cmd)
  "Shell command to list."
  (split-string (shell-command-to-string cmd) "\n" t))

(defun counsel-rhythmbox-play-song (song)
  "Let Rhythmbox play SONG."
  (let ((first (string= (shell-command-to-string "pidof rhythmbox") ""))
        (service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (when first
      (counsel--run "nohup" "rhythmbox")
      (sit-for 1.5))
    (dbus-call-method :session service path interface
                      "OpenUri" (cdr song))
    (let ((id (and first
                   (cdr (counsel--wmctrl-parse
                         (shell-command-to-string
                          "wmctrl -l -p | grep $(pidof rhythmbox)"))))))
      (when id
        (sit-for 0.2)
        (counsel--run "wmctrl" "-ic" id)))))

(defun counsel-rhythmbox-enqueue-song (song)
  "Let Rhythmbox enqueue SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dbus-call-method :session service path interface
                      "AddToQueue" (cdr song))))

(defun counsel-rhythmbox-playpause-current-song ()
  "Play/pause the current song."
  (interactive)
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (dbus-call-method :session service path interface
                      "PlayPause")))

(defun counsel-rhythmbox-toggle-shuffle (_song)
  "Toggle Rhythmbox shuffle setting."
  (let* ((old-order (counsel--command "dconf" "read" "/org/gnome/rhythmbox/player/play-order"))
         (new-order (if (string= old-order "'shuffle'")
                        "'linear'"
                      "'shuffle'")))
    (counsel--command
     "dconf"
     "write"
     "/org/gnome/rhythmbox/player/play-order"
     new-order)
    (message (if (string= new-order "'shuffle'")
                 "shuffle on"
               "shuffle off"))))

(defvar counsel-rhythmbox-history nil
  "History for `counsel-rhythmbox'.")

(defvar counsel-rhythmbox-songs nil)

(defun counsel-rhythmbox-current-song ()
  "Return the currently playing song title."
  (ignore-errors
    (let* ((entry (dbus-get-property
                   :session
                   "org.mpris.MediaPlayer2.rhythmbox"
                   "/org/mpris/MediaPlayer2"
                   "org.mpris.MediaPlayer2.Player"
                   "Metadata"))
           (artist (caar (cadr (assoc "xesam:artist" entry))))
           (album (cl-caadr (assoc "xesam:album" entry)))
           (title (cl-caadr (assoc "xesam:title" entry))))
      (format "%s - %s - %s" artist album title))))

;;;###autoload
(defun counsel-rhythmbox (&optional arg)
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive "P")
  (require 'dbus)
  (when (or arg (null counsel-rhythmbox-songs))
    (let* ((service "org.gnome.Rhythmbox3")
           (path "/org/gnome/UPnP/MediaServer2/Library/all")
           (interface "org.gnome.UPnP.MediaContainer2")
           (nb-songs (dbus-get-property
                      :session service path interface "ChildCount")))
      (if (not nb-songs)
          (error "Couldn't connect to Rhythmbox")
        (setq counsel-rhythmbox-songs
              (mapcar (lambda (x)
                        (cons
                         (format
                          "%s - %s - %s"
                          (cl-caadr (assoc "Artist" x))
                          (cl-caadr (assoc "Album" x))
                          (cl-caadr (assoc "DisplayName" x)))
                         (cl-caaadr (assoc "URLs" x))))
                      (dbus-call-method
                       :session service path interface "ListChildren"
                       0 nb-songs '("*")))))))
  (ivy-read "Rhythmbox: " counsel-rhythmbox-songs
            :require-match t
            :history 'counsel-rhythmbox-history
            :preselect (counsel-rhythmbox-current-song)
            :action
            '(1
              ("p" counsel-rhythmbox-play-song "Play song")
              ("e" counsel-rhythmbox-enqueue-song "Enqueue song")
              ("s" counsel-rhythmbox-toggle-shuffle "Shuffle on/off"))
            :caller 'counsel-rhythmbox))

;;** `counsel-linux-app'

;; Added in Emacs 26.1.
(require 'xdg nil t)

(defalias 'counsel--xdg-data-home
  (if (fboundp 'xdg-data-home)
      #'xdg-data-home
    (lambda ()
      (let ((directory (getenv "XDG_DATA_HOME")))
        (if (or (null directory) (string= directory ""))
            "~/.local/share"
          directory))))
  "Compatibility shim for `xdg-data-home'.")

(defalias 'counsel--xdg-data-dirs
  (if (fboundp 'xdg-data-dirs)
      #'xdg-data-dirs
    (lambda ()
      (let ((path (getenv "XDG_DATA_DIRS")))
        (if (or (null path) (string= path ""))
            '("/usr/local/share" "/usr/share")
          (parse-colon-path path)))))
  "Compatibility shim for `xdg-data-dirs'.")

(defcustom counsel-linux-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
          (cons (counsel--xdg-data-home)
                (counsel--xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom counsel-linux-app-format-function #'counsel-linux-app-format-function-default
  "Function to format Linux application names the `counsel-linux-app' menu.
The format function will be passed the application's name, comment, and command
as arguments."
  :type '(choice
          (const :tag "Command : Name - Comment" counsel-linux-app-format-function-default)
          (const :tag "Name - Comment (Command)" counsel-linux-app-format-function-name-first)
          (const :tag "Name - Comment" counsel-linux-app-format-function-name-only)
          (const :tag "Name - Comment (Pretty)" counsel-linux-app-format-function-name-pretty)
          (const :tag "Command" counsel-linux-app-format-function-command-only)
          (function :tag "Custom")))

(defface counsel-application-name
  '((t :inherit font-lock-builtin-face))
  "Face for displaying executable names."
  :group 'ivy-faces)

(defface counsel-outline-1
  '((t :inherit org-level-1))
  "Face for displaying level 1 headings."
  :group 'ivy-faces)

(defface counsel-outline-2
  '((t :inherit org-level-2))
  "Face for displaying level 2 headings."
  :group 'ivy-faces)

(defface counsel-outline-3
  '((t :inherit org-level-3))
  "Face for displaying level 3 headings."
  :group 'ivy-faces)

(defface counsel-outline-4
  '((t :inherit org-level-4))
  "Face for displaying level 4 headings."
  :group 'ivy-faces)

(defface counsel-outline-5
  '((t :inherit org-level-5))
  "Face for displaying level 5 headings."
  :group 'ivy-faces)

(defface counsel-outline-6
  '((t :inherit org-level-6))
  "Face for displaying level 6 headings."
  :group 'ivy-faces)

(defface counsel-outline-7
  '((t :inherit org-level-7))
  "Face for displaying level 7 headings."
  :group 'ivy-faces)

(defface counsel-outline-8
  '((t :inherit org-level-8))
  "Face for displaying level 8 headings."
  :group 'ivy-faces)

(defface counsel-outline-default
  '((t :inherit minibuffer-prompt))
  "Face for displaying headings."
  :group 'ivy-faces)

(defvar counsel-linux-apps-faulty nil
  "List of faulty desktop files.")

(defvar counsel--linux-apps-cache nil
  "Cache of desktop files data.")

(defvar counsel--linux-apps-cached-files nil
  "List of cached desktop files.")

(defvar counsel--linux-apps-cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar counsel--linux-apps-cache-format-function nil
  "The function used to format the cached Linux application menu.")

(defun counsel-linux-app-format-function-default (name comment exec)
  "Default Linux application name formatter.
NAME is the name of the application, COMMENT its comment and EXEC
the command to launch it."
  (format "% -45s: %s%s"
          (propertize
           (ivy--truncate-string
            (replace-regexp-in-string "env +[^ ]+ +" "" exec t t)
            45)
           'face 'counsel-application-name)
          name
          (if comment
              (concat " - " comment)
            "")))

(defun counsel-linux-app-format-function-name-first (name comment exec)
  "Format Linux application names with the NAME (and COMMENT) first.
EXEC is the command to launch the application."
  (format "%s%s (%s)"
          name
          (if comment
              (concat " - " comment)
            "")
          (propertize exec 'face 'counsel-application-name)))

(defun counsel-linux-app-format-function-name-only (name comment _exec)
  "Format Linux application names with the NAME (and COMMENT) only."
  (format "%s%s"
          name
          (if comment
              (concat " - " comment)
            "")))

(defun counsel-linux-app-format-function-command-only (_name _comment exec)
  "Display only the command EXEC when formatting Linux application names."
  exec)

(defun counsel-linux-app-format-function-name-pretty (name comment _exec)
  "Format Linux application names with the NAME (and COMMENT) only, but pretty."
  (format "% -45s%s"
          (propertize
           (ivy--truncate-string name 45)
           'face 'counsel-application-name)
          (if comment
              (concat ": " comment)
            "")))

(defun counsel-linux-apps-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir counsel-linux-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          ;; Function `directory-files-recursively' added in Emacs 25.1.
          (dolist (file (directory-files-recursively dir "\\.desktop\\'"))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
              (when (and (not (gethash id hash)) (file-readable-p file))
                (push (cons id file) result)
                (puthash id file hash)))))))
    result))

(defun counsel-linux-app--parse-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
          (end (re-search-forward "^\\[" nil t))
          (visible t)
          name comment exec)
      (catch 'break
        (unless start
          (push file counsel-linux-apps-faulty)
          (message "Warning: File %s has no [Desktop Entry] group" file)
          (throw 'break nil))

        (goto-char start)
        (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
          (setq visible nil))
        (setq name (match-string 1))

        (goto-char start)
        (unless (re-search-forward "^Type *= *Application *$" end t)
          (throw 'break nil))
        (setq name (match-string 1))

        (goto-char start)
        (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
          (push file counsel-linux-apps-faulty)
          (message "Warning: File %s has no Name" file)
          (throw 'break nil))
        (setq name (match-string 1))

        (goto-char start)
        (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
          (setq comment (match-string 1)))

        (goto-char start)
        (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
          ;; Don't warn because this can technically be a valid desktop file.
          (throw 'break nil))
        (setq exec (match-string 1))

        (goto-char start)
        (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
          (let ((try-exec (match-string 1)))
            (unless (locate-file try-exec exec-path nil #'file-executable-p)
              (throw 'break nil))))
        (propertize
         (funcall counsel-linux-app-format-function name comment exec)
         'visible visible)))))

(defun counsel-linux-apps-parse (desktop-entries-alist)
  "Parse the given alist of Linux desktop entries.
Each entry in DESKTOP-ENTRIES-ALIST is a pair of ((id . file-name)).
Any desktop entries that fail to parse are recorded in
`counsel-linux-apps-faulty'."
  (let (result)
    (setq counsel-linux-apps-faulty nil)
    (dolist (entry desktop-entries-alist result)
      (let* ((id (car entry))
             (file (cdr entry))
             (r (counsel-linux-app--parse-file file)))
        (when r
          (push (cons r id) result))))))

(defun counsel-linux-apps-list ()
  "Return list of all Linux desktop applications."
  (let* ((new-desktop-alist (counsel-linux-apps-list-desktop-files))
         (new-files (mapcar #'cdr new-desktop-alist)))
    (unless (and
             (eq counsel-linux-app-format-function
                 counsel--linux-apps-cache-format-function)
             (equal new-files counsel--linux-apps-cached-files)
             (null (cl-find-if
                    (lambda (file)
                      (time-less-p
                       counsel--linux-apps-cache-timestamp
                       (nth 5 (file-attributes file))))
                    new-files)))
      (setq counsel--linux-apps-cache (counsel-linux-apps-parse new-desktop-alist))
      (setq counsel--linux-apps-cache-format-function counsel-linux-app-format-function)
      (setq counsel--linux-apps-cache-timestamp (current-time))
      (setq counsel--linux-apps-cached-files new-files)))
  counsel--linux-apps-cache)


(defun counsel-linux-app-action-default (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT."
  (call-process "gtk-launch" nil 0 nil (cdr desktop-shortcut)))

(defun counsel-linux-app-action-file (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT with a selected file."
  (call-process "gtk-launch" nil 0 nil
                (cdr desktop-shortcut)
                (read-file-name "File: ")))

(defun counsel-linux-app-action-open-desktop (desktop-shortcut)
  "Open DESKTOP-SHORTCUT."
  (let* ((app (cdr desktop-shortcut))
         (file (cdr (assoc app (counsel-linux-apps-list-desktop-files)))))
    (if file
        (find-file file)
      (error "Could not find location of file %s" app))))

(ivy-set-actions
 'counsel-linux-app
 '(("f" counsel-linux-app-action-file "run on a file")
   ("d" counsel-linux-app-action-open-desktop "open desktop file")))

;;;###autoload
(defun counsel-linux-app (&optional arg)
  "Launch a Linux desktop application, similar to Alt-<F2>.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive "P")
  (ivy-read "Run application: " (counsel-linux-apps-list)
            :predicate (unless arg (lambda (x) (get-text-property 0 'visible (car x))))
            :action #'counsel-linux-app-action-default
            :caller 'counsel-linux-app))

;;** `counsel-wmctrl'
(defun counsel-wmctrl-action (x)
  "Select the desktop window that corresponds to X."
  (counsel--run "wmctrl" "-i" "-a" (cdr x)))

(defvar counsel-wmctrl-ignore '("XdndCollectionWindowImp"
                                "unity-launcher" "unity-panel" "unity-dash"
                                "Hud" "Desktop")
  "List of window titles to ignore for `counsel-wmctrl'.")

(defun counsel--wmctrl-parse (s)
  (when (string-match "\\`\\([0-9a-fx]+\\) +\\([-0-9]+\\) +\\(?:[0-9]+\\) +\\([^ ]+\\) \\(.+\\)$" s)
    (let ((title (match-string 4 s))
          (id (match-string 1 s)))
      (unless (member title counsel-wmctrl-ignore)
        (cons title id)))))

;;;###autoload
(defun counsel-wmctrl ()
  "Select a desktop window using wmctrl."
  (interactive)
  (let* ((cands1 (counsel--sl "wmctrl -l -p"))
         (cands2 (delq nil (mapcar #'counsel--wmctrl-parse cands1))))
    (ivy-read "window: " cands2
              :action #'counsel-wmctrl-action
              :caller 'counsel-wmctrl)))

(defvar counsel--switch-buffer-temporary-buffers nil
  "Internal.")

(defvar counsel--switch-buffer-previous-buffers nil
  "Internal.")

(defun counsel--switch-buffer-unwind ()
  "Clear temporary file buffers and restore `buffer-list'.
The buffers are those opened during a session of `counsel-switch-buffer'."
  (mapc #'kill-buffer counsel--switch-buffer-temporary-buffers)
  (dolist (buf counsel--switch-buffer-previous-buffers)
    (when (buffer-live-p buf) (bury-buffer buf)))
  (setq counsel--switch-buffer-temporary-buffers ())
  (setq counsel--switch-buffer-previous-buffers ()))

(defcustom counsel-switch-buffer-preview-virtual-buffers t
  "When non-nil, `counsel-switch-buffer' will preview virtual buffers."
  :type 'boolean)

(defun counsel--switch-buffer-update-fn ()
  (unless counsel--switch-buffer-previous-buffers
    (setq counsel--switch-buffer-previous-buffers (buffer-list)))
  (let* ((virtual (assoc (ivy-state-current ivy-last) ivy--virtual-buffers)))
    (when (member (ivy-state-current ivy-last) ivy-marked-candidates)
      (setf (ivy-state-current ivy-last)
            (substring (ivy-state-current ivy-last) (length ivy-mark-prefix))))
    (cond
      ((get-buffer (ivy-state-current ivy-last))
       (let ((ivy-marked-candidates nil))
         (ivy-call)))
      ((and counsel-switch-buffer-preview-virtual-buffers virtual (file-exists-p (cdr virtual)))
       (let ((buf (ignore-errors
                    ;; may not open due to `large-file-warning-threshold' etc.
                    (find-file-noselect (cdr virtual)))))
         (if buf
             (progn
               (push buf counsel--switch-buffer-temporary-buffers)
               (ivy-call))
           ;; clean up the minibuffer so that there's no delay before
           ;; the Ivy candidates are displayed once again
           (message ""))))
      (t
       (with-ivy-window
         (switch-to-buffer (ivy-state-buffer ivy-last)))))))

;;;###autoload
(defun counsel-switch-buffer ()
  "Switch to another buffer.
Display a preview of the selected ivy completion candidate buffer
in the current window."
  (interactive)
  (let ((ivy-update-fns-alist
         '((ivy-switch-buffer . counsel--switch-buffer-update-fn)))
        (ivy-unwind-fns-alist
         '((ivy-switch-buffer . counsel--switch-buffer-unwind))))
    (ivy-switch-buffer)))

;;;###autoload
(defun counsel-switch-buffer-other-window ()
  "Switch to another buffer in another window.
Display a preview of the selected ivy completion candidate buffer
in the current window."
  (interactive)
  (let ((ivy-update-fns-alist
         '((ivy-switch-buffer-other-window . counsel--switch-buffer-update-fn)))
        (ivy-unwind-fns-alist
         '((ivy-switch-buffer-other-window . counsel--switch-buffer-unwind))))
    (ivy-switch-buffer-other-window)))

(defun counsel-open-buffer-file-externally (buffer)
  "Open the file associated with BUFFER with an external program."
  (when (zerop (length buffer))
    (user-error "Can't open that"))
  (let* ((virtual (assoc buffer ivy--virtual-buffers))
         (filename (if virtual
                       (cdr virtual)
                     (buffer-file-name (get-buffer buffer)))))
    (unless filename
      (user-error "Can't open `%s' externally" buffer))
    (counsel-locate-action-extern (expand-file-name filename))))

(ivy-add-actions
 'ivy-switch-buffer
 '(("x" counsel-open-buffer-file-externally "open externally")))

(ivy-set-actions
 'counsel-switch-buffer
 '(("x" counsel-open-buffer-file-externally "open externally")
   ("j" ivy--switch-buffer-other-window-action "other window")))

;;** `counsel-compile'
(defvar counsel-compile-history nil
  "History for `counsel-compile'.

This is a list of strings with additional properties which allow
the history to be filtered depending on the context of the call.
The properties include:

`srcdir'
    the root directory of the source code
`blddir'
    the root directory of the build (in or outside the `srcdir')
`bldenv'
    the build environment as passed to `compilation-environment'
`recursive'
    the completion should be run again in `blddir' of this result
`cmd'
    if set, pass only the substring with this property to `compile'

This variable is suitable for addition to
`savehist-additional-variables'.")

(defvar counsel-compile-root-functions
  '(counsel--projectile-root
    counsel--project-current
    counsel--configure-root
    counsel--git-root
    counsel--dir-locals-root)
  "Special hook to find the project root for compile commands.
Each function on this hook is called in turn with no arguments
and should return either a directory, or nil if no root was
found.")

(defun counsel--compile-root ()
  "Return root of current project or signal an error on failure.
The root is determined by `counsel-compile-root-functions'."
  (or (run-hook-with-args-until-success 'counsel-compile-root-functions)
      (error "Couldn't find project root")))

(defun counsel--projectile-root ()
  "Return root of current projectile project or nil on failure.
Use `projectile-project-root' to determine the root."
  (and (fboundp 'projectile-project-root)
       (projectile-project-root)))

(defun counsel--project-current ()
  "Return root of current project or nil on failure.
Use `project-current' to determine the root."
  (let ((proj (and (fboundp 'project-current)
                   (project-current))))
    (cond ((not proj) nil)
          ((fboundp 'project-root)
           (project-root proj))
          ((fboundp 'project-roots)
           (car (project-roots proj))))))

(defun counsel--configure-root ()
  "Return root of current project or nil on failure.
Use the presence of a \"configure\" file to determine the root."
  (counsel--dominating-file "configure"))

(defun counsel--git-root ()
  "Return root of current project or nil on failure.
Use the presence of a \".git\" file to determine the root."
  (counsel--dominating-file ".git"))

(defun counsel--dir-locals-root ()
  "Return root of current project or nil on failure.
Use the presence of a `dir-locals-file' to determine the root."
  (counsel--dominating-file dir-locals-file))

(defvar counsel-compile-local-builds
  '(counsel-compile-get-filtered-history
    counsel-compile-get-build-directories
    counsel-compile-get-make-invocation
    counsel-compile-get-make-help-invocations)
  "Additional compile invocations to feed into `counsel-compile'.

This can either be a list of compile invocation strings or
functions that will provide such a list.  You should customize
this if you want to provide specific non-standard build types to
`counsel-compile'.  The default helpers are set up to handle
common build environments.")

(defcustom counsel-compile-make-args "-k"
  "Additional arguments for make.
You may, for example, want to add \"-jN\" for the number of cores
N in your system."
  :type 'string)

(defcustom counsel-compile-env nil
  "List of environment variables for compilation to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.  This
list is passed to `compilation-environment'."
  :type '(repeat (string :tag "ENVVARNAME=VALUE")))

(defvar counsel-compile-env-history nil
  "History for `counsel-compile-env'.")

(defvar counsel-compile-env-pattern
  "[_[:digit:][:upper:]]+=[/[:alnum:]]*"
  "Pattern to match valid environment variables.")

(defcustom counsel-compile-make-pattern "\\`\\(?:GNUm\\|[Mm]\\)akefile\\'"
  "Regexp for matching the names of Makefiles."
  :type 'regexp)

(defcustom counsel-compile-build-directories
  '("build" "builds" "bld" ".build")
  "List of potential build subdirectory names to check for."
  :type '(repeat directory))

(defvar counsel-compile-phony-pattern "^\\.PHONY:[\t ]+\\(.+\\)$"
  "Regexp for extracting phony targets from Makefiles.")

(defvar counsel-compile-help-pattern
  "\\(?:^\\(\\*\\)?[[:space:]]+\\([^[:space:]]+\\)[[:space:]]+-\\)"
  "Regexp for extracting help targets from a make help call.")

;; This is loosely based on the Bash Make completion code which
;; relies on GNUMake having the following return codes:
;;   0 = no-rebuild, -q & 1 needs rebuild, 2 error
(defun counsel-compile--probe-make-targets (dir)
  "Return a list of Make targets for DIR.

Return a single blank target (so we invoke the default target)
if Make exits with an error.  This might happen because some sort
of configuration needs to be done first or the source tree is
pristine and being used for multiple build trees."
  (with-temp-buffer
    (let* ((default-directory dir)
           (res (call-process "make" nil t nil "-nqp"))
           targets)
      (if (or (not (numberp res)) (> res 1))
          (list "")
        (goto-char (point-min))
        (while (re-search-forward counsel-compile-phony-pattern nil t)
          (push (split-string (match-string-no-properties 1)) targets))
        (sort (apply #'nconc targets) #'string-lessp)))))

(defun counsel-compile--pretty-propertize (leader text face)
  "Return a pretty string of the form \" LEADER TEXT\".
LEADER is propertized with a warning face and the remaining
text with FACE."
  (concat (propertize (concat " " leader " ")
                      'face
                      'font-lock-warning-face)
          (propertize text 'face face)))

(defun counsel--compile-get-make-targets (probe-fn srcdir &optional blddir)
  "Return propertized make targets returned by PROBE-FN in SRCDIR.

The optional BLDDIR allows for handling build directories.  We
search the Makefile for a list of phony targets which are
generally the top level targets a Make system provides.  The
resulting strings are tagged with properties that
`counsel-compile-history' can use for filtering results."
  (let ((fmt (format (propertize "make %s %%s" 'cmd t)
                     counsel-compile-make-args))
        (suffix (and blddir
                     (counsel-compile--pretty-propertize "in" blddir
                                                         'dired-directory)))
        (build-env (and counsel-compile-env
                        (counsel-compile--pretty-propertize
                         "with"
                         (mapconcat #'identity counsel-compile-env " ")
                         'font-lock-variable-name-face)))
        (props `(srcdir ,srcdir blddir ,blddir bldenv ,counsel-compile-env)))
    (mapcar (lambda (target)
              (setq target (concat (format fmt target) suffix build-env))
              (add-text-properties 0 (length target) props target)
              target)
            (funcall probe-fn (or blddir srcdir)))))

(defun counsel-compile-get-make-invocation (&optional blddir)
  "Have a look in the root directory for any build control files.

The optional BLDDIR is useful for other helpers that have found
sub-directories that builds may be invoked in."
  (let ((srcdir (counsel--compile-root)))
    (when (directory-files (or blddir srcdir) nil
                           counsel-compile-make-pattern t)
      (counsel--compile-get-make-targets
       #'counsel-compile--probe-make-targets srcdir blddir))))

(defun counsel-compile--probe-make-help (dir)
  "Return a list of Make targets based on help for DIR.

It is quite common for a \"make help\" invocation to return a
human readable list of targets.  Often common targets are marked
with a leading asterisk.  The exact search pattern is controlled
by `counsel-compile-help-pattern'."
  (let ((default-directory dir)
        primary-targets targets)
    ;; Only proceed if the help target exists.
    (when (eql 1 (apply #'call-process "make" nil nil nil "-q" "help"
                        counsel-compile-env))
      (with-temp-buffer
        (when (eql 0 (apply #'call-process "make" nil t nil "help"
                            counsel-compile-env))
          (goto-char (point-min))
          (while (re-search-forward counsel-compile-help-pattern nil t)
            (push (match-string 2)
                  (if (match-beginning 1) primary-targets targets)))
          (nconc (sort primary-targets #'string-lessp)
                 (sort targets #'string-lessp)))))))

(defun counsel-compile-get-make-help-invocations (&optional blddir)
  "Query the root directory for makefiles with help output.

The optional BLDDIR is useful for other helpers that have found
sub-directories that builds may be invoked in."
  (let ((srcdir (counsel--compile-root)))
    (when (directory-files (or blddir srcdir) nil
                           counsel-compile-make-pattern t)
      (counsel--compile-get-make-targets
       #'counsel-compile--probe-make-help srcdir blddir))))

(defun counsel--find-build-subdir (srcdir)
  "Return builds subdirectory of SRCDIR, if one exists."
  (cl-some (lambda (dir)
             (setq dir (expand-file-name dir srcdir))
             (and (file-directory-p dir) dir))
           counsel-compile-build-directories))

(defun counsel--get-build-subdirs (blddir)
  "Return all subdirs under BLDDIR sorted by modification time.
If there are non-directory files in BLDDIR, include BLDDIR in the
list as it may also be a build directory."
  (let* ((files (directory-files-and-attributes
                 blddir t directory-files-no-dot-files-regexp t))
         (total (length files))
         (dirs (cl-delete-if-not
                (lambda (entry)
                  (let ((dir (nth 1 entry)))
                    (and dir (or (eq dir t)
                                 ;; Symlink.
                                 (file-directory-p (nth 0 entry))))))
                files)))
    ;; Any non-dir files?
    (when (< (length dirs) total)
      (push (cons blddir (file-attributes blddir)) dirs))
    (mapcar #'car (sort dirs (lambda (x y)
                               (time-less-p (nth 6 y) (nth 6 x)))))))

(defun counsel-compile-get-build-directories (&optional dir)
  "Return a list of potential build directories."
  (let* ((srcdir (or dir (counsel--compile-root)))
         (blddir (counsel--find-build-subdir srcdir))
         (props `(srcdir ,srcdir recursive t))
         (fmt (concat (propertize "Select build in "
                                  'face 'font-lock-warning-face)
                      (propertize "%s" 'face 'dired-directory))))
    (mapcar (lambda (subdir)
              (let ((s (format fmt subdir)))
                (add-text-properties 0 (length s) `(blddir ,subdir ,@props) s)
                s))
            (and blddir (counsel--get-build-subdirs blddir)))))

;; This is a workaround for the fact there is no concept of "project"
;; local variables (as opposed to for example buffer-local).  So we
;; store all our history in a global list filter out the results we
;; don't want.
(defun counsel-compile-get-filtered-history (&optional dir)
  "Return a compile history relevant to current project."
  (let ((root (or dir (counsel--compile-root)))
        history)
    (dolist (item counsel-compile-history)
      (let ((srcdir (get-text-property 0 'srcdir item))
            (blddir (get-text-property 0 'blddir item)))
        (when (or (and srcdir (file-in-directory-p srcdir root))
                  (and blddir (file-in-directory-p blddir root)))
          (push item history))))
    (nreverse history)))

(defun counsel--get-compile-candidates (&optional dir)
  "Return the list of compile commands.
This is determined by `counsel-compile-local-builds', which see."
  (let (cands)
    (dolist (cmds counsel-compile-local-builds)
      (when (functionp cmds)
        (setq cmds (funcall cmds dir)))
      (when cmds
        (push (if (listp cmds) cmds (list cmds)) cands)))
    (apply #'append (nreverse cands))))

;; This is a workaround to ensure we tag all the relevant metadata in
;; our compile history.  This also allows M-x compile to do fancy
;; things like infer `default-directory' from 'cd's in the string.
(defun counsel-compile--update-history (_proc)
  "Update `counsel-compile-history' from the compilation state."
  (let* ((srcdir (counsel--compile-root))
         (blddir default-directory)
         (bldenv compilation-environment)
         (cmd (concat
               (propertize (car compilation-arguments) 'cmd t)
               (unless (file-equal-p blddir srcdir)
                 (counsel-compile--pretty-propertize "in" blddir
                                                     'dired-directory))
               (when bldenv
                 (counsel-compile--pretty-propertize "with"
                                                     (mapconcat #'identity bldenv " ")
                                                     'font-lock-variable-name-face)))))
    (add-text-properties 0 (length cmd)
                         `(srcdir ,srcdir blddir ,blddir bldenv ,bldenv) cmd)
    (add-to-history 'counsel-compile-history cmd)))

(defvar counsel-compile--current-build-dir nil
  "Tracks the last directory `counsel-compile' was called with.

This state allows us to set it correctly if the user has manually
edited the command, thus losing our embedded state.")

(defun counsel-compile--action (cmd)
  "Process CMD to call `compile'.

If CMD has the `recursive' property set we call `counsel-compile'
again to further refine the compile options in the directory
specified by the `blddir' property."
  (let ((blddir (get-text-property 0 'blddir cmd))
        (bldenv (get-text-property 0 'bldenv cmd)))
    (if (get-text-property 0 'recursive cmd)
        (counsel-compile blddir)
      (when (get-char-property 0 'cmd cmd)
        (setq cmd (substring-no-properties
                   cmd 0 (next-single-property-change 0 'cmd cmd))))
      (let ((default-directory (or blddir
                                   counsel-compile--current-build-dir
                                   default-directory))
            (compilation-environment bldenv))
        ;; No need to specify `:history' because of this hook.
        (add-hook 'compilation-start-hook #'counsel-compile--update-history)
        (unwind-protect
             (compile cmd)
          (remove-hook 'compilation-start-hook #'counsel-compile--update-history))))))

(defun counsel-compile-edit-command ()
  "Insert current compile command into the minibuffer for editing.

This mirrors the behavior of `ivy-insert-current' but with specific
handling for the `counsel-compile' metadata."
  (interactive)
  (delete-minibuffer-contents)
  (let* ((cmd (ivy-state-current ivy-last))
         (blddir (get-text-property 0 'blddir cmd)))
    (when blddir
      (setq counsel-compile--current-build-dir blddir))
    (insert (substring-no-properties
             cmd 0 (and (get-text-property 0 'cmd cmd)
                        (next-single-property-change 0 'cmd cmd))))))

;; Currently the only thing we do is override ivy's default insert
;; operation which doesn't include the metadata we want.
(defvar counsel-compile-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ivy-insert-current] #'counsel-compile-edit-command)
    map)
  "Additional ivy keybindings during command selection.")

;;;###autoload
(defun counsel-compile (&optional dir)
  "Call `compile' completing with smart suggestions, optionally for DIR.

Additional actions:

\\{counsel-compile-map}"
  (interactive)
  (setq counsel-compile--current-build-dir (or dir
                                               (counsel--compile-root)
                                               default-directory))
  (ivy-read "Compile command: "
            (delete-dups (counsel--get-compile-candidates dir))
            :action #'counsel-compile--action
            :keymap counsel-compile-map
            :caller 'counsel-compile))

(ivy-add-actions
 'counsel-compile
 '(("d" counsel-compile-forget-command "delete")))

(defun counsel-compile-forget-command (cmd)
  "Delete CMD from `counsel-compile-history'."
  (setq counsel-compile-history
        (delete cmd counsel-compile-history)))

(defun counsel-compile-env--format-hint (cands)
  "Return a formatter for compile-env CANDS."
  (let ((rmstr
         (propertize "remove" 'face 'font-lock-warning-face))
        (addstr
         (propertize "add" 'face 'font-lock-variable-name-face)))
    (ivy--format-function-generic
     (lambda (selected)
       (format "%s %s"
               (if (member selected counsel-compile-env) rmstr addstr)
               selected))
     #'identity
     cands
     "\n")))

(defun counsel-compile-env--update (var)
  "Update `counsel-compile-env' either adding or removing VAR."
  (cond ((member var counsel-compile-env)
         (setq counsel-compile-env (delete var counsel-compile-env)))
        ((string-match-p counsel-compile-env-pattern var)
         (push var counsel-compile-env))
        (t (user-error "Ignoring malformed variable: '%s'" var))))

;;;###autoload
(defun counsel-compile-env ()
  "Update `counsel-compile-env' interactively."
  (interactive)
  (ivy-read "Compile environment variable: "
            (delete-dups (append
                          counsel-compile-env counsel-compile-env-history))
            :action #'counsel-compile-env--update
            :predicate (lambda (cand)
                         (string-match-p counsel-compile-env-pattern
                                         cand))
            :history 'counsel-compile-env-history
            :caller 'counsel-compile-env))

(ivy-configure 'counsel-compile-env
  :format-fn #'counsel-compile-env--format-hint)

;;** `counsel-minor'
(defvar counsel-minor-history nil
  "History for `counsel-minor'.")

(defun counsel--minor-candidates ()
  "Return completion alist for `counsel-minor'.

The alist element is cons of minor mode string with its lighter
and minor mode symbol."
  (delq nil
        (mapcar
         (lambda (mode)
           (when (and (boundp mode) (commandp mode))
             (let ((lighter (cdr (assq mode minor-mode-alist))))
               (cons (concat
                      (if (symbol-value mode) "-" "+")
                      (symbol-name mode)
                      (propertize
                       (if lighter
                           (format " \"%s\""
                                   (format-mode-line (cons t lighter)))
                         "")
                       'face font-lock-string-face))
                     mode))))
         minor-mode-list)))

;;;###autoload
(defun counsel-minor ()
  "Enable or disable minor mode.

Disabled minor modes are prefixed with \"+\", and
selecting one of these will enable it.
Enabled minor modes are prefixed with \"-\", and
selecting one of these will enable it.

Additional actions:\\<ivy-minibuffer-map>

  \\[ivy-dispatching-done] d: Go to minor mode definition
  \\[ivy-dispatching-done] h: Describe minor mode"

  (interactive)
  (ivy-read "Minor modes (enable +mode or disable -mode): "
            (counsel--minor-candidates)
            :require-match t
            :history 'counsel-minor-history
            :action (lambda (x)
                      (call-interactively (cdr x)))))

(ivy-configure 'counsel-minor
  :initial-input "^+"
  :sort-fn #'ivy-string<)

(ivy-set-actions
 'counsel-minor
 `(("d" ,(lambda (x) (find-function (cdr x))) "definition")
   ("h" ,(lambda (x) (describe-function (cdr x))) "help")))

;;;###autoload
(defun counsel-major ()
  (interactive)
  (ivy-read "Major modes: " obarray
            :predicate (lambda (f)
                         (and (commandp f)
                              (string-suffix-p "-mode" (symbol-name f))
                              (or (and (autoloadp (symbol-function f))
                                       (let ((doc-split (help-split-fundoc (documentation f) f)))
                                         ;; major mode starters have no arguments
                                         (and doc-split (null (cdr (read (car doc-split)))))))
                                  (null (help-function-arglist f)))))
            :action #'counsel-M-x-action
            :caller 'counsel-major))

;;** `counsel-search'
(declare-function request "ext:request")

(defcustom counsel-search-engine 'ddg
  "The search engine choice in `counsel-search-engines-alist'."
  :type '(choice
          (const ddg)
          (const google)))

(defcustom counsel-search-engines-alist
  '((google
     "http://suggestqueries.google.com/complete/search"
     "https://www.google.com/search?q="
     counsel--search-request-data-google)
    (ddg
     "https://duckduckgo.com/ac/"
     "https://duckduckgo.com/html/?q="
     counsel--search-request-data-ddg))
  "Search engine parameters for `counsel-search'."
  :type '(alist :key-type symbol :value-type (list string string function)))

(defun counsel--search-request-data-google (data)
  (mapcar #'identity (aref data 1)))

(defun counsel--search-request-data-ddg (data)
  (mapcar #'cdar data))

(defun counsel-search-function (input)
  "Create a request to a search engine with INPUT.
Return 0 tells `ivy--exhibit' not to update the minibuffer.
We update it in the callback with `ivy-update-candidates'."
  (or
   (ivy-more-chars)
   (let ((engine (cdr (assoc counsel-search-engine counsel-search-engines-alist))))
     (request
      (nth 0 engine)
      :type "GET"
      :params (list
               (cons "client" "firefox")
               (cons "q" input))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (ivy-update-candidates
                   (funcall (nth 2 engine) data)))))
     0)))

(defun counsel-search-action (x)
  "Search for X."
  (browse-url
   (concat
    (nth 2 (assoc counsel-search-engine counsel-search-engines-alist))
    (url-hexify-string x))))

(defun counsel-search ()
  "Ivy interface for dynamically querying a search engine."
  (interactive)
  (require 'request)
  (require 'json)
  (ivy-read "search: " #'counsel-search-function
            :action #'counsel-search-action
            :dynamic-collection t
            :caller 'counsel-search))

(define-obsolete-function-alias 'counsel-google
    #'counsel-search "0.13.2 (2019-10-17)")

;;** `counsel-compilation-errors'
(defun counsel--compilation-errors-buffer (buf)
  (with-current-buffer buf
    (let ((res nil)
          (pt (point-min)))
      (save-excursion
        (while (setq pt (compilation-next-single-property-change
                         pt 'compilation-message))
          (let ((loc (get-text-property pt 'compilation-message)))
            (when (and loc (setq loc (compilation--message->loc loc)))
              (goto-char pt)
              (push
               (propertize
                (buffer-substring-no-properties pt (line-end-position))
                'pt pt
                'buffer buf)
               res)))))
      (nreverse res))))

(defun counsel-compilation-errors-cands ()
  (cl-loop
     for buf in (buffer-list)
     when (compilation-buffer-p buf)
     nconc (counsel--compilation-errors-buffer buf)))

(defun counsel-compilation-errors-action (x)
  (pop-to-buffer (get-text-property 0 'buffer x))
  (goto-char (get-text-property 0 'pt x))
  (compile-goto-error))

;;;###autoload
(defun counsel-compilation-errors ()
  "Compilation errors."
  (interactive)
  (ivy-read "compilation errors: " (counsel-compilation-errors-cands)
            :require-match t
            :action #'counsel-compilation-errors-action
            :history 'counsel-compilation-errors-history))

;;** `counsel-flycheck'
(defvar flycheck-current-errors)
(declare-function flycheck-error-filename "ext:flycheck")
(declare-function flycheck-error-line "ext:flycheck")
(declare-function flycheck-error-message "ext:flycheck")
(declare-function flycheck-jump-to-error "ext:flycheck")

(defun counsel-flycheck-errors-cands ()
  (mapcar
   (lambda (err)
     (propertize
      (format "%s:%d:%s"
              (file-name-base (flycheck-error-filename err))
              (flycheck-error-line err)
              (flycheck-error-message err)) 'error err))
   flycheck-current-errors))

(defun counsel-flycheck-occur (cands)
  "Generate a custom occur buffer for `counsel-flycheck'."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (swiper--occur-insert-lines
   (mapcar
    (lambda (cand)
      (let ((err (get-text-property 0 'error cand)))
        (propertize
         (format
          "%s:%d:%s"
          (flycheck-error-filename err)
          (flycheck-error-line err)
          cand)
         'error err)))
    cands)))

(defun counsel-flycheck-errors-action (err)
  (flycheck-jump-to-error (get-text-property 0 'error err)))

(ivy-configure 'counsel-flycheck
  :occur #'counsel-flycheck-occur)

;;;###autoload
(defun counsel-flycheck ()
  "Flycheck errors."
  (interactive)
  (require 'flycheck)
  (ivy-read "flycheck errors: " (counsel-flycheck-errors-cands)
            :require-match t
            :action #'counsel-flycheck-errors-action
            :history 'counsel-flycheck-errors-history))


;;* `counsel-mode'
(defvar counsel-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding
              '((execute-extended-command . counsel-M-x)
                (describe-bindings . counsel-descbinds)
                (describe-function . counsel-describe-function)
                (describe-variable . counsel-describe-variable)
                (describe-symbol . counsel-describe-symbol)
                (apropos-command . counsel-apropos)
                (describe-face . counsel-describe-face)
                (list-faces-display . counsel-faces)
                (find-file . counsel-find-file)
                (find-library . counsel-find-library)
                (imenu . counsel-imenu)
                (load-library . counsel-load-library)
                (load-theme . counsel-load-theme)
                (yank-pop . counsel-yank-pop)
                (info-lookup-symbol . counsel-info-lookup-symbol)
                (pop-to-mark-command . counsel-mark-ring)
                (geiser-doc-look-up-manual . counsel-geiser-doc-look-up-manual)
                (bookmark-jump . counsel-bookmark)))
      (define-key map (vector 'remap (car binding)) (cdr binding)))
    map)
  "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements.")

(defcustom counsel-mode-override-describe-bindings nil
  "Whether to override `describe-bindings' when `counsel-mode' is active."
  :type 'boolean)

;;;###autoload
(define-minor-mode counsel-mode
  "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements.

Local bindings (`counsel-mode-map'):
\\{counsel-mode-map}"
  :global t
  :keymap counsel-mode-map
  :lighter " counsel"
  (if counsel-mode
      (progn
        (when counsel-mode-override-describe-bindings
          (advice-add #'describe-bindings :override #'counsel-descbinds))
        (define-key minibuffer-local-map (kbd "C-r")
          #'counsel-minibuffer-history))
    (advice-remove #'describe-bindings #'counsel-descbinds)))

(provide 'counsel)

;;; counsel.el ends here
