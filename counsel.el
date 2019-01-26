;;; counsel.el --- Various completion functions using Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.11.0
;; Package-Requires: ((emacs "24.3") (swiper "0.11.0"))
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
;; - Describe fuctions for Elisp: function, variable, library, command,
;;   bindings, theme.
;; - Navigation functions: imenu, ace-line, semantic, outline.
;; - Git utilities: git-files, git-grep, git-log, git-stash, git-checkout.
;; - Grep utitilies: grep, ag, pt, recoll, ack, rg.
;; - System utilities: process list, rhythmbox, linux-app.
;; - Many more.

;;; Code:

(require 'swiper)
(require 'compile)
(require 'dired)

(defgroup counsel nil
  "Completion functions using Ivy."
  :group 'matching
  :prefix "counsel-")

;;* Utility
(define-obsolete-variable-alias 'counsel-more-chars-alist 'ivy-more-chars-alist "0.10.0")

(define-obsolete-function-alias 'counsel-more-chars 'ivy-more-chars "0.10.0")

(defun counsel--elisp-to-pcre (regex)
  "Convert REGEX from Elisp format to PCRE format, on best-effort basis.
REGEX may be of any format returned by an Ivy regex function,
namely a string or a list.  The return value is always a string.

Note that incorrect results may be returned for sufficiently
complex regexes."
  (if (consp regex)
      (mapconcat
       (lambda (pair)
         (let ((subexp (counsel--elisp-to-pcre (car pair))))
           (if (string-match-p "|" subexp)
               (format "(?:%s)" subexp)
             subexp)))
       (cl-remove-if-not #'cdr regex)
       ".*")
    (replace-regexp-in-string
     "\\\\[(){}|]\\|[()]"
     (lambda (s)
       (or (cdr (assoc s '(("\\(" . "(")
                           ("\\)" . ")")
                           ("(" . "\\(")
                           (")" . "\\)")
                           ("\\{" . "{")
                           ("\\}" . "}")
                           ("\\|" . "|"))))
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

(defun counsel-require-program (program)
  "Check system for PROGRAM, printing error if unfound."
  (or (and (stringp program)
           (not (string= program ""))
           (executable-find program))
      (user-error "Required program \"%s\" not found in your path" program)))

(defun counsel-prompt-function-default ()
  "Return prompt appended with a semicolon."
  (declare (obsolete ivy-set-prompt "0.10.0"))
  (ivy-add-prompt-count (concat (ivy-state-prompt ivy-last) ": ")))

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

;;* Async Utility
(defvar counsel--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar counsel--async-start nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar counsel--async-duration nil
  "Store the time a process takes to gather all its candidates.
The time is measured in seconds.")

(defvar counsel--async-exit-code-plist ()
  "Associate commands with their exit code descriptions.
This plist maps commands to a plist mapping their exit codes to
descriptions.")

(defun counsel-set-async-exit-code (cmd number str)
  "For CMD, associate NUMBER exit code with STR."
  (let ((plist (plist-get counsel--async-exit-code-plist cmd)))
    (setq counsel--async-exit-code-plist
          (plist-put counsel--async-exit-code-plist
                     cmd
                     (plist-put plist number str)))))

(defvar counsel-async-split-string-re "\n"
  "Store the regexp for splitting shell command output.")

(defvar counsel-async-ignore-re nil
  "Regexp matching candidates to ignore in `counsel--async-filter'.")

(defun counsel--async-command (cmd &optional sentinel filter name)
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
  (let* ((buf (get-buffer-create name))
         (proc (if (listp cmd)
                   (apply #'start-file-process name buf cmd)
                 (start-file-process-shell-command name buf cmd))))
    (setq counsel--async-time (current-time))
    (setq counsel--async-start counsel--async-time)
    (set-process-sentinel proc (or sentinel #'counsel--async-sentinel))
    (set-process-filter proc (or filter #'counsel--async-filter))
    proc))

(defvar counsel-grep-last-line nil)

(defun counsel--async-sentinel (process _msg)
  "Sentinel function for an asynchronous counsel PROCESS."
  (when (eq (process-status process) 'exit)
    (if (zerop (process-exit-status process))
        (progn
          (ivy--set-candidates
           (ivy--sort-maybe
            (with-current-buffer (process-buffer process)
              (split-string (buffer-string) counsel-async-split-string-re t))))
          (setq counsel-grep-last-line nil)
          (when counsel--async-start
            (setq counsel--async-duration
                  (time-to-seconds (time-since counsel--async-start))))
          (let ((re (ivy-re-to-str (funcall ivy--regex-function ivy-text))))
            (if ivy--old-cands
                (ivy--recompute-index ivy-text re ivy--all-candidates)
              (unless (ivy-set-index
                       (ivy--preselect-index
                        (ivy-state-preselect ivy-last)
                        ivy--all-candidates))
                (ivy--recompute-index ivy-text re ivy--all-candidates))))
          (setq ivy--old-cands ivy--all-candidates)
          (if ivy--all-candidates
              (ivy--exhibit)
            (ivy--insert-minibuffer "")))
      (setq ivy--all-candidates
            (let ((status (process-exit-status process))
                  (plist (plist-get counsel--async-exit-code-plist
                                    (ivy-state-caller ivy-last))))
              (list (or (plist-get plist status)
                        (format "error code %d" status)))))
      (setq ivy--old-cands ivy--all-candidates)
      (ivy--exhibit))))

(defcustom counsel-async-filter-update-time 500000
  "The amount of microseconds to wait until updating `counsel--async-filter'."
  :type 'integer)

(defun counsel--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (when (time-less-p (list 0 0 counsel-async-filter-update-time)
                     (time-since counsel--async-time))
    (let (numlines)
      (with-current-buffer (process-buffer process)
        (setq numlines (count-lines (point-min) (point-max)))
        (ivy--set-candidates
         (let ((lines (split-string (buffer-string)
                                    counsel-async-split-string-re
                                    t)))
           (if (stringp counsel-async-ignore-re)
               (cl-remove-if (lambda (line)
                               (string-match-p counsel-async-ignore-re line))
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
;;** `counsel-el'
;;;###autoload
(defun counsel-el ()
  "Elisp completion at point."
  (interactive)
  (let* ((bnd (unless (and (looking-at ")")
                           (eq (char-before) ?\())
                (bounds-of-thing-at-point 'symbol)))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd)
                   (cdr bnd))
                ""))
         (pred (and (eq (char-before (car bnd)) ?\()
                    #'fboundp))
         symbol-names)
    (setq ivy-completion-beg (car bnd))
    (setq ivy-completion-end (cdr bnd))
    (if (string= str "")
        (mapatoms
         (lambda (x)
           (when (symbolp x)
             (push (symbol-name x) symbol-names))))
      (setq symbol-names (all-completions str obarray pred)))
    (ivy-read "Symbol name: " symbol-names
              :caller 'counsel-el
              :predicate pred
              :initial-input str
              :action #'ivy-completion-in-region-action)))

(add-to-list 'ivy-height-alist '(counsel-el . 7))

;;** `counsel-cl'
(declare-function slime-symbol-start-pos "ext:slime")
(declare-function slime-symbol-end-pos "ext:slime")
(declare-function slime-contextual-completions "ext:slime-c-p-c")

;;;###autoload
(defun counsel-cl ()
  "Common Lisp completion at point."
  (interactive)
  (setq ivy-completion-beg (slime-symbol-start-pos))
  (setq ivy-completion-end (slime-symbol-end-pos))
  (ivy-read "Symbol name: "
            (car (slime-contextual-completions
                  ivy-completion-beg
                  ivy-completion-end))
            :action #'ivy-completion-in-region-action))

;;** `counsel-jedi'
(declare-function deferred:sync! "ext:deferred")
(declare-function jedi:complete-request "ext:jedi-core")
(declare-function jedi:ac-direct-matches "ext:jedi")

(defun counsel-jedi ()
  "Python completion at point."
  (interactive)
  (let ((bnd (bounds-of-thing-at-point 'symbol)))
    (setq ivy-completion-beg (car bnd))
    (setq ivy-completion-end (cdr bnd)))
  (deferred:sync!
      (jedi:complete-request))
  (ivy-read "Symbol name: " (jedi:ac-direct-matches)
            :action #'counsel--py-action))

(defun counsel--py-action (symbol-name)
  "Insert SYMBOL-NAME, erasing the previous one."
  (when (stringp symbol-name)
    (with-ivy-window
      (when ivy-completion-beg
        (delete-region
         ivy-completion-beg
         ivy-completion-end))
      (setq ivy-completion-beg (point))
      (insert symbol-name)
      (setq ivy-completion-end (point))
      (when (equal (get-text-property 0 'symbol symbol-name) "f")
        (insert "()")
        (setq ivy-completion-end (point))
        (backward-char)))))

;;** `counsel-clj'
(declare-function cider-sync-request:complete "ext:cider-client")
(defun counsel--generic (completion-fn)
  "Complete thing at point with COMPLETION-FN."
  (let* ((bnd (or (bounds-of-thing-at-point 'symbol)
                  (cons (point) (point))))
         (str (buffer-substring-no-properties
               (car bnd) (cdr bnd)))
         (candidates (funcall completion-fn str))
         (res (ivy-read (format "pattern (%s): " str)
                        candidates
                        :caller 'counsel--generic)))
    (when (stringp res)
      (when bnd
        (delete-region (car bnd) (cdr bnd)))
      (insert res))))

(add-to-list 'ivy-height-alist '(counsel--generic . 7))

;;;###autoload
(defun counsel-clj ()
  "Clojure completion at point."
  (interactive)
  (counsel--generic
   (lambda (str)
     (mapcar
      #'cl-caddr
      (cider-sync-request:complete str ":same")))))

;;** `counsel-company'
(defvar company-candidates)
(defvar company-point)
(defvar company-common)
(declare-function company-complete "ext:company")
(declare-function company-mode "ext:company")
(declare-function company-complete-common "ext:company")
(declare-function company-abort "ext:company")

;;;###autoload
(defun counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (when company-point
    (when (looking-back company-common (line-beginning-position))
      (setq ivy-completion-beg (match-beginning 0))
      (setq ivy-completion-end (match-end 0)))
    (ivy-read "company cand: " company-candidates
              :action #'ivy-completion-in-region-action
              :unwind #'company-abort)))

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

(add-to-list 'ivy-display-functions-alist '(counsel-irony . ivy-display-function-overlay))

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
Used by commands `counsel-describe-variable' and
`counsel-describe-function'.")

(defun counsel-find-symbol ()
  "Jump to the definition of the current symbol."
  (interactive)
  (ivy-exit-with-action #'counsel--find-symbol))

(defun counsel--info-lookup-symbol ()
  "Lookup the current symbol in the info docs."
  (interactive)
  (ivy-exit-with-action #'counsel-info-lookup-symbol))

(defvar find-tag-marker-ring)
(declare-function xref-push-marker-stack "xref")

(defalias 'counsel--push-xref-marker
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

(define-obsolete-function-alias 'counsel-symbol-at-point
    'ivy-thing-at-point "0.7.0")

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

(ivy-set-display-transformer
 'counsel-describe-variable 'counsel-describe-variable-transformer)

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
              :sort t
              :action (lambda (x)
                        (funcall counsel-describe-variable-function (intern x)))
              :caller 'counsel-describe-variable)))

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

(ivy-set-display-transformer
 'counsel-describe-function 'counsel-describe-function-transformer)

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
              :sort t
              :action (lambda (x)
                        (funcall counsel-describe-function-function (intern x)))
              :caller 'counsel-describe-function)))

;;** `counsel-set-variable'
(defvar counsel-set-variable-history nil
  "Store history for `counsel-set-variable'.")

(defun counsel-read-setq-expression (sym)
  "Read and eval a setq expression for SYM."
  (setq this-command 'eval-expression)
  (let* ((minibuffer-completing-symbol t)
         (sym-value (symbol-value sym))
         (expr (minibuffer-with-setup-hook
                   (lambda ()
                     (add-function :before-until (local 'eldoc-documentation-function)
                                   #'elisp-eldoc-documentation-function)
                     (add-hook 'completion-at-point-functions #'elisp-completion-at-point nil t)
                     (run-hooks 'eval-expression-minibuffer-setup-hook)
                     (goto-char (minibuffer-prompt-end))
                     (forward-char 6)
                     (insert (format "%S " sym)))
                 (read-from-minibuffer "Eval: "
                                       (format
                                        (if (and sym-value (consp sym-value))
                                            "(setq '%S)"
                                          "(setq %S)")
                                        sym-value)
                                       read-expression-map t
                                       'read-expression-history))))
    (eval-expression expr)))

(defun counsel--setq-doconst (x)
  "Return a cons of description and value for X.
X is an item of a radio- or choice-type defcustom."
  (let (y)
    (when (and (listp x)
               (consp (setq y (last x))))
      (unless (equal y '(function))
        (setq x (car y))
        (cons (prin1-to-string x)
              (if (symbolp x)
                  (list 'quote x)
                x))))))

(declare-function lv-message "ext:lv")
(declare-function lv-delete-window "ext:lv")
(declare-function custom-variable-documentation "cus-edit")

;;;###autoload
(defface counsel-variable-documentation
  '((t :inherit font-lock-comment-face))
  "Face for displaying Lisp documentation."
  :group 'ivy-faces)

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
                   (set sym (if (and (listp res) (eq (car res) 'quote))
                                (cadr res)
                              res))))
             (unless (boundp sym)
               (set sym nil))
             (counsel-read-setq-expression sym)))
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
            :sort t
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
                       :sort t
                       :caller 'counsel-info-lookup-symbol)
             mode))))
  (info-lookup-symbol symbol mode))

;;** `counsel-M-x'
(defface counsel-key-binding
  '((t :inherit font-lock-keyword-face))
  "Face used by `counsel-M-x' for key bindings."
  :group 'ivy-faces)

(defun counsel-M-x-transformer (cmd)
  "Return CMD annotated with its active key binding, if any."
  (let ((key (where-is-internal (intern cmd) nil t)))
    (if (not key)
        cmd
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
      (format "%s (%s)" cmd key))))

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
              :predicate (and (not externs)
                              (lambda (sym)
                                (and (commandp sym)
                                     (not (get sym 'byte-obsolete-info)))))
              :require-match t
              :history 'counsel-M-x-history
              :action (lambda (cmd)
                        (setq cmd (intern cmd))
                        (cond ((bound-and-true-p amx-initialized)
                               (amx-rank cmd))
                              ((bound-and-true-p smex-initialized-p)
                               (smex-rank cmd)))
                        (setq prefix-arg current-prefix-arg)
                        (setq this-command cmd)
                        (setq real-this-command cmd)
                        (command-execute cmd 'record))
              :sort (not externs)
              :keymap counsel-describe-map
              :initial-input initial-input
              :caller 'counsel-M-x)))

(ivy-set-actions
 'counsel-M-x
 `(("d" counsel--find-symbol "definition")
   ("h" ,(lambda (x) (describe-function (intern x))) "help")))

(ivy-set-display-transformer
 'counsel-M-x
 'counsel-M-x-transformer)

;;** `counsel-command-history'
(defun counsel-command-history-action-eval (cmd)
  "Eval the command CMD."
  (eval (read cmd)))

(defun counsel-command-history-action-edit-and-eval (cmd)
  "Edit and eval the command CMD."
  (edit-and-eval-command "Eval: " (read cmd)))

(ivy-set-actions
 'counsel-command-history
 '(("r" counsel-command-history-action-eval           "eval command")
   ("e" counsel-command-history-action-edit-and-eval  "edit and eval command")))

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
            (mapcar 'symbol-name
                    (custom-available-themes))
            :action #'counsel-load-theme-action
            :caller 'counsel-load-theme))

;;** `counsel-descbinds'
(ivy-set-actions
 'counsel-descbinds
 '(("d" counsel-descbinds-action-find "definition")
   ("I" counsel-descbinds-action-info "info")))

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
      (re-search-forward "")
      (forward-char 1)
      (while (not (eobp))
        (when (looking-at "^\\([^\t\n]+\\)[\t ]*\\(.*\\)$")
          (let ((key (match-string 1))
                (fun (match-string 2))
                cmd)
            (unless (or (member fun '("??" "self-insert-command"))
                        (string-match re-exclude key)
                        (not (or (commandp (setq cmd (intern-soft fun)))
                                 (member fun '("Prefix Command")))))
              (push
               (cons (format
                      "%-15s %s"
                      (propertize key 'face 'counsel-key-binding)
                      fun)
                     (cons key cmd))
               res))))
        (forward-line 1)))
    (nreverse res)))

(defun counsel-descbinds-action-describe (x)
  "Describe function of candidate X.
See `describe-function' for further information."
  (let ((cmd (cddr x)))
    (describe-function cmd)))

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

(defun counsel-describe-face ()
  "Completion for `describe-face'."
  (interactive)
  (ivy-read "Face: " (face-list)
            :require-match t
            :history 'face-name-history
            :preselect (counsel--face-at-point)
            :sort t
            :action counsel-describe-face-function
            :caller 'counsel-describe-face))

(defun counsel-customize-face (name)
  "Customize face with NAME."
  (customize-face (intern name)))

(defun counsel-customize-face-other-window (name)
  "Customize face with NAME in another window."
  (customize-face-other-window (intern name)))

(ivy-set-actions
 'counsel-describe-face
 '(("c" counsel-customize-face "customize")
   ("C" counsel-customize-face-other-window "customize other window")))

;;** `counsel-faces'
(defun counsel--faces-format-function (format)
  "Return an `ivy-format-function' for `counsel-faces'.
Each candidate is formatted based on the given FORMAT string."
  (let ((formatter (lambda (name)
                     (format format name (propertize list-faces-sample-text
                                                     'face (intern name))))))
    (lambda (names)
      (ivy--format-function-generic
       (lambda (name)
         (funcall formatter (ivy--add-face name 'ivy-current-match)))
       formatter names "\n"))))

;;;###autoload
(defun counsel-faces ()
  "Complete faces with preview.
Actions are provided by default for describing or customizing the
selected face."
  (interactive)
  (let* ((names (mapcar #'symbol-name (face-list)))
         (ivy-format-function
          (counsel--faces-format-function
           (format "%%-%ds %%s"
                   (apply #'max 0 (mapcar #'string-width names))))))
    (ivy-read "Face: " names
              :require-match t
              :history 'face-name-history
              :preselect (counsel--face-at-point)
              :sort t
              :action counsel-describe-face-function
              :caller 'counsel-faces)))

(ivy-set-actions
 'counsel-faces
 '(("c" counsel-customize-face "customize")
   ("C" counsel-customize-face-other-window "customize other window")))

;;* Git
;;** `counsel-git'
(defvar counsel-git-cmd "git ls-files --full-name --"
  "Command for `counsel-git'.")

(ivy-set-actions
 'counsel-git
 '(("j" find-file-other-window "other window")
   ("x" counsel-find-file-extern "open externally")))

(defun counsel-locate-git-root ()
  "Locate the root of the git repository containing the current buffer."
  (or (locate-dominating-file default-directory ".git")
      (error "Not in a git repository")))

;;;###autoload
(defun counsel-git (&optional initial-input)
  "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (let* ((default-directory (expand-file-name (counsel-locate-git-root)))
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd)
                 "\n"
                 t)))
    (ivy-read "Find file: " cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'counsel-git)))

(defun counsel-git-action (x)
  "Find file X in current Git repository."
  (with-ivy-window
    (let ((default-directory (ivy-state-directory ivy-last)))
      (find-file x))))

(defun counsel-git-occur ()
  "Occur function for `counsel-git' using `counsel-cmd-to-dired'."
  (cd (ivy-state-directory ivy-last))
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (format "%s | grep -i -E '%s' | xargs ls"
            counsel-git-cmd
            (counsel--elisp-to-pcre ivy--old-re)))))

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

(ivy-set-occur 'counsel-git 'counsel-git-occur)

;;** `counsel-git-grep'
(defvar counsel-git-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-q") 'counsel-git-grep-query-replace)
    (define-key map (kbd "C-c C-m") 'counsel-git-grep-switch-cmd)
    map))

(ivy-set-occur 'counsel-git-grep 'counsel-git-grep-occur)
(ivy-set-display-transformer 'counsel-git-grep 'counsel-git-grep-transformer)

(defvar counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -I -e \"%s\""
  "Initial command for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd nil
  "Store the command for `counsel-git-grep'.")

(defvar counsel--git-grep-count nil
  "Store the line count in current repository.")

(defvar counsel--git-grep-count-threshold 20000
  "The maximum threshold beyond which repositories are considered large.")

(defvar counsel-git-grep-history nil
  "History for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd-history
  (list counsel-git-grep-cmd-default)
  "History for `counsel-git-grep' shell commands.")

(defcustom counsel-grep-post-action-hook nil
  "Hook that runs after the point moves to the next candidate.
Typical value: '(recenter)."
  :type 'hook)

(defun counsel-git-grep-function (str &optional _pred &rest _unused)
  "Grep in the current git repository for STRING."
  (or
   (and (> counsel--git-grep-count counsel--git-grep-count-threshold)
        (ivy-more-chars))
   (let* ((default-directory (ivy-state-directory ivy-last))
          (cmd (format counsel-git-grep-cmd
                       (setq ivy--old-re (ivy--regex str t)))))
     (if (<= counsel--git-grep-count counsel--git-grep-count-threshold)
         (split-string (shell-command-to-string cmd) "\n" t)
       (counsel--gg-candidates (ivy--regex str))
       nil))))

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
      (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
      (swiper--ensure-visible)
      (run-hooks 'counsel-grep-post-action-hook)
      (unless (eq ivy-exit 'done)
        (swiper--cleanup)
        (swiper--add-overlays (ivy--regex ivy-text))))))

(defun counsel-git-grep-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES for `counsel-git-grep'."
  (or (and (equal regexp ivy--old-re)
           ivy--old-cands)
      (prog1
          (setq ivy--old-cands
                (cl-remove-if-not
                 (lambda (x)
                   (ignore-errors
                     (when (string-match "^[^:]+:[^:]+:" x)
                       (setq x (substring x (match-end 0)))
                       (if (stringp regexp)
                           (string-match regexp x)
                         (let ((res t))
                           (dolist (re regexp)
                             (setq res
                                   (and res
                                        (ignore-errors
                                          (if (cdr re)
                                              (string-match (car re) x)
                                            (not (string-match (car re) x)))))))
                           res)))))
                 candidates))
        (setq ivy--old-re regexp))))

(defun counsel-git-grep-transformer (str)
  "Higlight file and line number in STR."
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" str)
    (ivy-add-face-text-property (match-beginning 1) (match-end 1)
                                'ivy-grep-info
                                str)
    (ivy-add-face-text-property (match-beginning 2) (match-end 2)
                                'ivy-grep-line-number
                                str))
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
      (cmd
       (if (setq proj
                 (cl-find-if
                  (lambda (x)
                    (string-match (car x) dd))
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

(defun counsel--call (&rest command)
  "Synchronously call COMMAND and return its output as a string.
COMMAND comprises the program name followed by its arguments, as
in `make-process'.  Signal `file-error' and emit a warning if
COMMAND fails.  Obey file handlers based on `default-directory'."
  (let ((stderr (make-temp-file "counsel-call-stderr-"))
        status)
    (unwind-protect
         (with-temp-buffer
           (setq status (apply #'process-file (car command) nil
                               (list t stderr) nil (cdr command)))
           (if (eq status 0)
               ;; Return all output except trailing newline.
               (buffer-substring (point-min)
                                 (- (point)
                                    (if (eq (bobp) (bolp))
                                        0
                                      1)))
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

(defun counsel--git-grep-count-func-default ()
  "Default function to calculate `counsel--git-grep-count'."
  (or (unless (eq system-type 'windows-nt)
        (ignore-errors
          (let ((git-dir (counsel--call "git" "rev-parse" "--git-dir")))
            (read (counsel--call "du" "-s" git-dir)))))
      0))

(defvar counsel--git-grep-count-func #'counsel--git-grep-count-func-default
  "Defun to calculate `counsel--git-grep-count' for `counsel-git-grep'.")

;;;###autoload
(defun counsel-git-grep (&optional cmd initial-input)
  "Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive "P")
  (let ((proj-and-cmd (counsel--git-grep-cmd-and-proj cmd))
        proj)
    (setq proj (car proj-and-cmd))
    (setq counsel-git-grep-cmd (cdr proj-and-cmd))
    (counsel-require-program (car (split-string counsel-git-grep-cmd)))
    (let ((collection-function
           (if proj
               #'counsel-git-grep-proj-function
             #'counsel-git-grep-function))
          (unwind-function
           (if proj
               (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup))
             (lambda ()
               (swiper--cleanup))))
          (default-directory (if proj
                                 (car proj)
                               (counsel-locate-git-root))))
      (setq counsel--git-grep-count (funcall counsel--git-grep-count-func))
      (ivy-read "git grep: " collection-function
                :initial-input initial-input
                :matcher #'counsel-git-grep-matcher
                :dynamic-collection (or proj
                                        (>
                                         counsel--git-grep-count
                                         counsel--git-grep-count-threshold))
                :keymap counsel-git-grep-map
                :action #'counsel-git-grep-action
                :unwind unwind-function
                :history 'counsel-git-grep-history
                :caller 'counsel-git-grep))))
(cl-pushnew 'counsel-git-grep ivy-highlight-grep-commands)

(defun counsel-git-grep-proj-function (str)
  "Grep for STR in the current git repository."
  (or
   (ivy-more-chars)
   (let ((regex (setq ivy--old-re
                      (ivy--regex str t))))
     (counsel--async-command (format counsel-git-grep-cmd regex))
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
          (all-completions "" 'counsel-git-grep-function))))

(defvar counsel-gg-state nil
  "The current state of candidates / count sync.")

(defun counsel--gg-candidates (regex)
  "Return git grep candidates for REGEX."
  (setq counsel-gg-state -2)
  (counsel--gg-count regex)
  (let ((default-directory (ivy-state-directory ivy-last)))
    (set-process-filter
     (counsel--async-command (concat (format counsel-git-grep-cmd regex)
                                     " | head -n 200")
                             #'counsel--gg-sentinel)
     nil)))

(defun counsel--gg-sentinel (process _msg)
  "Sentinel function for a `counsel-git-grep' PROCESS."
  (when (eq (process-status process) 'exit)
    (cl-case (process-exit-status process)
      ((0 141)
       (with-current-buffer (process-buffer process)
         (setq ivy--all-candidates
               (or (split-string (buffer-string) "\n" t)
                   '("")))
         (setq ivy--old-cands ivy--all-candidates))
       (when (zerop (cl-incf counsel-gg-state))
         (ivy--exhibit)))
      (1
       (setq ivy--all-candidates '("Error"))
       (setq ivy--old-cands ivy--all-candidates)
       (ivy--exhibit)))))

(defun counsel--gg-count-sentinel (process _msg)
  "Sentinel function for a `counsel--gg-count' PROCESS."
  (when (and (eq (process-status process) 'exit)
             (zerop (process-exit-status process)))
    (with-current-buffer (process-buffer process)
      (setq ivy--full-length (string-to-number (buffer-string))))
    (when (zerop (cl-incf counsel-gg-state))
      (ivy--exhibit))))

(defun counsel--gg-count (regex &optional no-async)
  "Count the number of results matching REGEX in `counsel-git-grep'.
The command to count the matches is called asynchronously.
If NO-ASYNC is non-nil, do it synchronously instead."
  (let ((default-directory (ivy-state-directory ivy-last))
        (cmd (concat
              (format (replace-regexp-in-string
                       "--full-name" "-c"
                       counsel-git-grep-cmd)
                      ;; "git grep -i -c '%s'"
                      (replace-regexp-in-string
                       "-" "\\\\-"
                       (replace-regexp-in-string "'" "''" regex)))
              " | sed 's/.*:\\(.*\\)/\\1/g' | awk '{s+=$1} END {print s}'")))
    (if no-async
        (string-to-number (shell-command-to-string cmd))
      (set-process-filter
       (counsel--async-command cmd #'counsel--gg-count-sentinel
                               nil " *counsel-gg-count*")
       nil))))

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

(defun counsel-git-grep-occur ()
  "Generate a custom occur buffer for `counsel-git-grep'.
When REVERT is non-nil, regenerate the current *ivy-occur* buffer."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  (let* ((regex (funcall ivy--regex-function ivy-text))
         (positive-pattern (replace-regexp-in-string
                            ;; git-grep can't handle .*?
                            "\\.\\*\\?" ".*"
                            (ivy-re-to-str regex)))
         (negative-patterns
          (if (stringp regex) ""
            (mapconcat (lambda (x)
                         (and (null (cdr x))
                              (format "| grep -v %s" (car x))))
                       regex
                       " ")))
         (cmd (concat (format counsel-git-grep-cmd positive-pattern) negative-patterns))
         cands)
    (setq cands (split-string
                 (shell-command-to-string cmd)
                 counsel-async-split-string-re
                 t))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar #'counsel--normalize-grep-match cands))))

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
(defvar counsel-git-log-cmd "GIT_PAGER=cat git log --grep '%s'"
  "Command used for \"git log\".")

(defvar counsel-git-log-split-string-re "\ncommit "
  "The `split-string' separates when split output of `counsel-git-log-cmd'.")

(defun counsel-git-log-function (str)
  "Search for STR in git log."
  (or
   (ivy-more-chars)
   (progn
     ;; `counsel--yank-pop-format-function' uses this
     (setq ivy--old-re (funcall ivy--regex-function str))
     (counsel--async-command
      ;; "git log --grep" likes to have groups quoted e.g. \(foo\).
      ;; But it doesn't like the non-greedy ".*?".
      (format counsel-git-log-cmd
              (replace-regexp-in-string "\\.\\*\\?" ".*"
                                        (ivy-re-to-str ivy--old-re))))
     nil)))

(defun counsel-git-log-action (x)
  "Add candidate X to kill ring."
  (message "%S" (kill-new x)))

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
  "List worktrees in the git repository containing the current buffer."
  (let ((default-directory (counsel-locate-git-root)))
    (split-string (shell-command-to-string "git worktree list") "\n" t)))

(defun counsel-git-worktree-parse-root (tree)
  "Return worktree from candidate TREE."
  (substring tree 0 (string-match-p " " tree)))

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
                  (error "No other worktrees!"))
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
   (format "git checkout %s" (substring branch 0 (string-match-p " " branch)))))

(defun counsel-git-branch-list ()
  "Return list of branches in the current git repository.
Value comprises all local and remote branches bar the one
currently checked out."
  (cl-mapcan (lambda (line)
               (and (string-match "\\`[[:blank:]]+" line)
                    (list (substring line (match-end 0)))))
             (let ((default-directory (counsel-locate-git-root)))
               (split-string (shell-command-to-string "git branch -vv --all")
                             "\n" t))))

;;;###autoload
(defun counsel-git-checkout ()
  "Call the \"git checkout\" command."
  (interactive)
  (ivy-read "Checkout branch: " (counsel-git-branch-list)
            :action #'counsel-git-checkout-action
            :caller 'counsel-git-checkout))

(defvar counsel-yank-pop-truncate-radius)

;;;###autoload
(defun counsel-git-log ()
  "Call the \"git log --grep\" shell command."
  (interactive)
  (let ((counsel-async-split-string-re counsel-git-log-split-string-re)
        (counsel-async-ignore-re "^[ \n]*$")
        (counsel-yank-pop-truncate-radius 5)
        (ivy-format-function #'counsel--yank-pop-format-function))
    (ivy-read "Grep log: " #'counsel-git-log-function
              :dynamic-collection t
              :action #'counsel-git-log-action
              :unwind #'counsel-delete-process
              :caller 'counsel-git-log)))

(add-to-list 'ivy-height-alist '(counsel-git-log . 4))

;;* File
;;** `counsel-find-file'
(defvar counsel-find-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-DEL") 'counsel-up-directory)
    (define-key map (kbd "C-<backspace>") 'counsel-up-directory)
    (define-key map (kbd "C-M-y") 'counsel-yank-directory)
    map))

(defun counsel-yank-directory ()
  "Yank the current directory into the minibuffer."
  (interactive)
  (insert ivy--directory))

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

(defun counsel-find-file-delete (x)
  "Delete file X."
  (when (or delete-by-moving-to-trash
            ;; `dired-delete-file', which see, already prompts for directories
            (eq t (car (file-attributes x)))
            (counsel--yes-or-no-p "Delete %s? " x))
    (dired-delete-file x dired-recursive-deletes delete-by-moving-to-trash)
    (dired-clean-up-after-deletion x)
    (ivy--reset-state ivy-last)))

(defun counsel-find-file-move (x)
  "Move or rename file X."
  (ivy-read "Rename file to: " #'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action (lambda (new-name)
                      (require 'dired-aux)
                      (dired-rename-file x new-name 1))
            :keymap counsel-find-file-map
            :caller 'counsel-find-file-move))

(defun counsel-find-file-mkdir-action (_x)
  (make-directory (expand-file-name ivy-text ivy--directory)))

(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-window "other window")
   ("f" find-file-other-frame "other frame")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("r" counsel-find-file-as-root "open as root")
   ("k" counsel-find-file-delete "delete")
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

Example value: \"\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\".  This will hide
temporary and lock files.
\\<ivy-minibuffer-map>
Choosing the dotfiles option, \"\\`\\.\", might be convenient,
since you can still access the dotfiles if your input starts with
a dot. The generic way to toggle ignored files is \\[ivy-toggle-ignore],
but the leading dot is a lot faster."
  :type `(choice
          (const :tag "None" nil)
          (const :tag "Dotfiles" "\\`\\.")
          (const :tag "Ignored Extensions"
                 ,(regexp-opt completion-ignored-extensions))
          (regexp :tag "Regex")))

(defun counsel--find-file-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip some dotfiles unless `ivy-text' requires them."
  (let ((res
         (ivy--re-filter
          regexp candidates
          (lambda (re-str)
            (lambda (x)
              (string-match re-str (directory-file-name x)))))))
    (if (or (null ivy-use-ignore)
            (null counsel-find-file-ignore-regexp)
            (string-match "\\`\\." ivy-text))
        res
      (or (cl-remove-if
           (lambda (x)
             (and
              (string-match counsel-find-file-ignore-regexp x)
              (not (member x ivy-extra-directories))))
           res)
          res))))

(declare-function ffap-guesser "ffap")

(defvar counsel-find-file-speedup-remote t
  "Speed up opening remote files by disabling `find-file-hook' for them.")

(defun counsel-find-file-action (x)
  "Find file X."
  (with-ivy-window
    (if (and counsel-find-file-speedup-remote
             (file-remote-p ivy--directory))
        (let ((find-file-hook nil))
          (find-file (expand-file-name x ivy--directory)))
      (find-file (expand-file-name x ivy--directory)))))

(defun counsel--preselect-file ()
  "Return candidate to preselect during filename completion.
The preselect behaviour can be customized via user options
`counsel-find-file-at-point' and
`counsel-preselect-current-file', which see."
  (or
   (when counsel-find-file-at-point
     (require 'ffap)
     (let ((f (ffap-guesser)))
       (when f (expand-file-name f))))
   (and counsel-preselect-current-file
        buffer-file-name
        (file-name-nondirectory buffer-file-name))))

;;;###autoload
(defun counsel-find-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read "Find file: " #'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action #'counsel-find-file-action
            :preselect (counsel--preselect-file)
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file))

(ivy-set-occur 'counsel-find-file 'counsel-find-file-occur)

(defvar counsel-find-file-occur-cmd "ls -a | grep -i -E '%s' | xargs -d '\\n' ls -d --group-directories-first"
  "Format string for `counsel-find-file-occur'.")

(defvar counsel-find-file-occur-use-find (not (eq system-type 'gnu/linux))
  "When non-nil, `counsel-find-file-occur' will use \"find\" as the base cmd.")

(defun counsel--expand-ls (cmd)
  "Expand CMD that ends in \"ls\" with switches."
  (concat cmd " " counsel-dired-listing-switches " | sed -e \"s/^/  /\""))

(defun counsel--occur-cmd-find ()
  (let* ((regex (counsel--elisp-to-pcre ivy--old-re))
         (cmd (format
               "find . -maxdepth 1 | grep -i -E '%s' | xargs -I {} find {} -maxdepth 0 -ls"
               regex)))
    (concat
     (counsel--cmd-to-dired-by-type "d" cmd)
     " && "
     (counsel--cmd-to-dired-by-type "f" cmd))))

(defun counsel--cmd-to-dired-by-type (type cmd)
  (let ((exclude-dots
         (if (string-match "^\\." ivy-text)
             ""
           " | grep -v '/\\\\.'")))
    (replace-regexp-in-string
     " | grep"
     (concat " -type " type exclude-dots " | grep") cmd)))

(defun counsel-find-file-occur ()
  (require 'find-dired)
  (cd ivy--directory)
  (if counsel-find-file-occur-use-find
      (counsel-cmd-to-dired
       (counsel--occur-cmd-find)
       'find-dired-filter)
    (counsel-cmd-to-dired
     (counsel--expand-ls
      (format counsel-find-file-occur-cmd
              (counsel--elisp-to-pcre ivy--old-re))))))

(defun counsel-up-directory ()
  "Go to the parent directory preselecting the current one.

If the current directory is remote and it's not possible to go up any
further, make the remote prefix editable"
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
          (setq ivy-text "")
          (delete-minibuffer-contents)
          (insert up-dir))
      (ivy--cd up-dir)
      (setf (ivy-state-preselect ivy-last)
            (file-name-as-directory (file-name-nondirectory cur-dir))))))

(defun counsel-down-directory ()
  "Descend into the current directory."
  (interactive)
  (ivy--directory-enter))

(defun counsel-at-git-issue-p ()
  "When point is at an issue in a Git-versioned file, return the issue string."
  (and (looking-at "#[0-9]+")
       (or (eq (vc-backend buffer-file-name) 'Git)
           (eq major-mode 'magit-commit-mode)
           (bound-and-true-p magit-commit-mode))
       (match-string-no-properties 0)))

(defun counsel-github-url-p ()
  "Return a Github issue URL at point."
  (counsel-require-program "git")
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
                            user repo (substring url 1))))))))

(defun counsel-emacs-url-p ()
  "Return a Debbugs issue URL at point."
  (counsel-require-program "git")
  (let ((url (counsel-at-git-issue-p)))
    (when url
      (let ((origin (shell-command-to-string
                     "git remote get-url origin")))
        (when (string-match "git.sv.gnu.org:/srv/git/emacs.git" origin)
          (format "https://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                  (substring url 1)))))))

(defvar counsel-url-expansions-alist nil
  "Map of regular expressions to expansions.

This variable should take the form of a list of (REGEXP . FORMAT)
pairs.

`counsel-url-expand' will expand the word at point according to
FORMAT for the first matching REGEXP.  FORMAT can be either a
string or a function.  If it is a string, it will be used as the
format string for the `format' function, with the word at point
as the next argument.  If it is a function, it will be called
with the word at point as the sole argument.

For example, a pair of the form:
  '(\"\\`BSERV-[[:digit:]]+\\'\" . \"https://jira.atlassian.com/browse/%s\")
will expand to URL `https://jira.atlassian.com/browse/BSERV-100'
when the word at point is BSERV-100.

If the format element is a function, more powerful
transformations are possible.  As an example,
  '(\"\\`issue\\([[:digit:]]+\\)\\'\" .
    (lambda (word)
      (concat \"https://debbugs.gnu.org/cgi/bugreport.cgi?bug=\"
              (match-string 1 word))))
trims the \"issue\" prefix from the word at point before creating the URL.")

(defun counsel-url-expand ()
  "Expand word at point using `counsel-url-expansions-alist'.
The first pair in the list whose regexp matches the word at point
will be expanded according to its format.  This function is
intended to be used in `ivy-ffap-url-functions' to browse the
result as a URL."
  (let ((word-at-point (current-word)))
    (cl-some
     (lambda (pair)
       (let ((regexp (car pair))
             (formatter (cdr pair)))
         (when (string-match regexp word-at-point)
           (if (functionp formatter)
               (funcall formatter word-at-point)
             (format formatter word-at-point)))))
     counsel-url-expansions-alist)))

;;** `counsel-recentf'
(defvar recentf-list)
(declare-function recentf-mode "recentf")

;;;###autoload
(defun counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: " (mapcar #'substring-no-properties recentf-list)
            :action (lambda (f)
                      (with-ivy-window
                        (find-file f)))
            :caller 'counsel-recentf))
(ivy-set-actions
 'counsel-recentf
 '(("j" find-file-other-window "other window")
   ("f" find-file-other-frame "other frame")
   ("x" counsel-find-file-extern "open externally")))

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
  "Return a function applyinig FN to a bookmark's location."
  (lambda (bookmark)
    (funcall fn (bookmark-location bookmark))))

(ivy-set-actions
 'counsel-bookmark
 `(("d" bookmark-delete "delete")
   ("e" bookmark-rename "edit")
   ("x" ,(counsel--apply-bookmark-fn #'counsel-find-file-extern)
        "open externally")
   ("r" ,(counsel--apply-bookmark-fn #'counsel-find-file-as-root)
        "open as root")))

;;** `counsel-bookmarked-directory'
(defun counsel-bookmarked-directory--candidates ()
  "Get a list of bookmarked directories sorted by file path."
  (bookmark-maybe-load-default-file)
  (sort (cl-remove-if-not
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
                 '(("j" dired-other-window "other window")
                   ("x" counsel-find-file-extern "open externally")
                   ("r" counsel-find-file-as-root "open as root")
                   ("f" (lambda (dir)
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
            ;; registers.  Each entry for a file registar will have the
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
            :sort t
            :require-match t
            :history 'counsel-file-register
            :caller 'counsel-file-register
            :action (lambda (register-file)
                      (with-ivy-window (find-file register-file)))))

(ivy-set-actions
 'counsel-file-register
 '(("j" find-file-other-window "other window")))

;;** `counsel-locate'
(defcustom counsel-locate-cmd (cond ((eq system-type 'darwin)
                                     'counsel-locate-cmd-noregex)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "es.exe"))
                                     'counsel-locate-cmd-es)
                                    (t
                                     'counsel-locate-cmd-default))
  "The function for producing a locate command string from the input.

The function takes a string - the current input, and returns a
string - the full shell command to run."
  :type '(choice
          (const :tag "Default" counsel-locate-cmd-default)
          (const :tag "No regex" counsel-locate-cmd-noregex)
          (const :tag "mdfind" counsel-locate-cmd-mdfind)
          (const :tag "everything" counsel-locate-cmd-es)))

(ivy-set-actions
 'counsel-locate
 '(("x" counsel-locate-action-extern "xdg-open")
   ("r" counsel-find-file-as-root "open as root")
   ("d" counsel-locate-action-dired "dired")))

(counsel-set-async-exit-code 'counsel-locate 1 "Nothing found")

(defvar counsel-locate-history nil
  "History for `counsel-locate'.")

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

(defun counsel-locate-cmd-default (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i --regex '%s'"
          (counsel--elisp-to-pcre
           (ivy--regex input))))

(defun counsel-locate-cmd-noregex (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i '%s'" input))

(defun counsel-locate-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "mdfind")
  (format "mdfind -name '%s'" input))

(defun counsel-locate-cmd-es (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "es.exe")
  (format "es.exe -i -r -p %s"
          (counsel--elisp-to-pcre
           (ivy--regex input t))))

(defun counsel-locate-function (input)
  "Call the \"locate\" shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (funcall counsel-locate-cmd input))
     '("" "working..."))))

;;;###autoload
(defun counsel-locate (&optional initial-input)
  "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " #'counsel-locate-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (when file
                        (with-ivy-window
                          (find-file
                           (concat (file-remote-p default-directory) file)))))
            :unwind #'counsel-delete-process
            :caller 'counsel-locate))

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
     (list "fzf" "-f" str)))
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
             (read-directory-name (concat
                                   fzf-basename
                                   " in directory: "))))))

  (let ((fzf-basename (car (split-string counsel-fzf-cmd))))
    (counsel-require-program fzf-basename)
    (setq counsel--fzf-dir
          (or initial-directory
              (funcall counsel-fzf-dir-function)))
    (ivy-read (or fzf-prompt (concat fzf-basename ": "))
              #'counsel-fzf-function
              :initial-input initial-input
              :re-builder #'ivy--regex-fuzzy
              :dynamic-collection t
              :action #'counsel-fzf-action
              :unwind #'counsel-delete-process
              :caller 'counsel-fzf)))

(defun counsel-fzf-action (x)
  "Find file X in current fzf directory."
  (with-ivy-window
    (let ((default-directory counsel--fzf-dir))
      (find-file x))))

(defun counsel-fzf-occur ()
  "Occur function for `counsel-fzf' using `counsel-cmd-to-dired'."
  (cd counsel--fzf-dir)
  (counsel-cmd-to-dired
   (counsel--expand-ls
    (format
     "%s --print0 | xargs -0 ls"
     (format counsel-fzf-cmd ivy-text)))))

(ivy-set-occur 'counsel-fzf 'counsel-fzf-occur)

(ivy-set-actions
 'counsel-fzf
 '(("x" counsel-locate-action-extern "xdg-open")
   ("d" counsel-locate-action-dired "dired")))

(counsel-set-async-exit-code 'counsel-fzf 1 "Nothing found")

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

(defcustom counsel-file-jump-args "* -type f -not -path '*\/.git*'"
  "Arguments for the `find-command' when using `counsel-file-jump'."
  :type 'string)

;;** `counsel-file-jump'
;;;###autoload
(defun counsel-file-jump (&optional initial-input initial-directory)
  "Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "find")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find file: "
              (split-string
               (shell-command-to-string
                (concat find-program " " counsel-file-jump-args))
               "\n" t)
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action (lambda (x)
                        (with-ivy-window
                          (find-file (expand-file-name x ivy--directory))))
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-file-jump)))

(defcustom counsel-dired-jump-args "* -type f -not -path '*\/.git*'"
  "Arguments for the `find-command' when using `counsel-dired-jump'."
  :type 'string)

;;** `counsel-dired-jump'
;;;###autoload
(defun counsel-dired-jump (&optional initial-input initial-directory)
  "Jump to a directory (in dired) below the current directory.
List all subdirectories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "find")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Directory: "
              (split-string
               (shell-command-to-string
                (concat find-program " " counsel-dired-jump-args))
               "\n" t)
              :initial-input initial-input
              :action (lambda (d) (dired-jump nil (expand-file-name d)))
              :caller 'counsel-dired-jump)))

;;* Grep
(defun counsel--grep-mode-occur (git-grep-dir-is-file)
  "Generate a custom occur buffer for grep like commands.
If GIT-GREP-DIR-IS-FILE is t, then `ivy-state-directory' is treated as a full
path to a file rather than a directory (e.g. for `counsel-grep-occur').

This function expects that the candidates have already been filtered.
It applies no filtering to ivy--all-candidates."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode))
  (let ((directory
         (if git-grep-dir-is-file
             (file-name-directory (ivy-state-directory ivy-last))
           (ivy-state-directory ivy-last))))
    (setq default-directory directory)
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n" default-directory))
    (insert (format "%d candidates:\n" (length ivy--all-candidates)))
    (ivy--occur-insert-lines
     (mapcar #'counsel--normalize-grep-match ivy--all-candidates))))

;;** `counsel-ag'
(defvar counsel-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-q") 'counsel-git-grep-query-replace)
    (define-key map (kbd "C-'") 'swiper-avy)
    map))

(defcustom counsel-ag-base-command
  (if (memq system-type '(ms-dos windows-nt))
      "ag --vimgrep %s"
    "ag --nocolor --nogroup %s")
  "Format string to use in `counsel-ag-function' to construct the command.
The %s will be replaced by optional extra ag arguments followed by the
regex string."
  :type 'string)

(defvar counsel-ag-command nil)

(counsel-set-async-exit-code 'counsel-ag 1 "No matches found")
(ivy-set-occur 'counsel-ag 'counsel-ag-occur)
(ivy-set-display-transformer 'counsel-ag 'counsel-git-grep-transformer)

(defconst counsel--command-args-separator "-- ")

(defun counsel--split-command-args (arguments)
  "Split ARGUMENTS into its switches and search-term parts.
Return pair of corresponding strings (SWITCHES . SEARCH-TERM)."
  (let ((switches "")
        (search-term arguments))
    (when (string-prefix-p "-" arguments)
      (let ((index (string-match counsel--command-args-separator arguments)))
        (when index
          (setq search-term
                (substring arguments (+ (length counsel--command-args-separator) index)))
          (setq switches (substring arguments 0 index)))))
    (cons switches search-term)))

(defun counsel--format-ag-command (extra-args needle)
  "Construct a complete `counsel-ag-command' as a string.
EXTRA-ARGS is a string of the additional arguments.
NEEDLE is the search string."
  (format counsel-ag-command
          (if (string-match " \\(--\\) " extra-args)
              (replace-match needle t t extra-args 1)
            (concat extra-args " " needle))))

(defun counsel--grep-regex (str)
  (counsel--elisp-to-pcre
   (setq ivy--old-re
         (funcall ivy--regex-function str))))

(defun counsel-ag-function (string)
  "Grep in the current directory for STRING."
  (let ((command-args (counsel--split-command-args string)))
    (let ((switches (car command-args))
          (search-term (cdr command-args)))
      (or
       (let ((ivy-text search-term))
         (ivy-more-chars))
       (let ((default-directory (ivy-state-directory ivy-last))
             (regex (counsel--grep-regex search-term)))
         (counsel--async-command (counsel--format-ag-command
                                  switches
                                  (shell-quote-argument regex)))
         nil)))))

;;;###autoload
(defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (counsel-require-program (car (split-string counsel-ag-command)))
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (read-from-minibuffer (format
                                     "%s args: "
                                     (car (split-string counsel-ag-command)))))))
  (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
  (let ((default-directory (or initial-directory
                               (locate-dominating-file default-directory ".git")
                               default-directory)))
    (ivy-read (or ag-prompt
                  (concat (car (split-string counsel-ag-command)) ": "))
              #'counsel-ag-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :unwind (lambda ()
                        (counsel-delete-process)
                        (swiper--cleanup))
              :caller 'counsel-ag)))

(cl-pushnew 'counsel-ag ivy-highlight-grep-commands)

(defun counsel-grep-like-occur (cmd-template)
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory (ivy-state-directory ivy-last)))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  (let* ((command-args (counsel--split-command-args ivy-text))
         (cmd (format cmd-template
                      (concat
                       (car command-args)
                       (shell-quote-argument
                        (counsel--elisp-to-pcre
                         (ivy--regex (cdr command-args)))))))
         (cands (split-string (shell-command-to-string cmd)
                              counsel-async-split-string-re
                              t)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar #'counsel--normalize-grep-match cands))))

(defun counsel-ag-occur ()
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
  (let ((counsel-ag-base-command counsel-pt-base-command))
    (counsel-ag initial-input)))
(cl-pushnew 'counsel-pt ivy-highlight-grep-commands)

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
  (let ((counsel-ag-base-command counsel-ack-base-command))
    (counsel-ag initial-input)))


;;** `counsel-rg'
(defcustom counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
  "Alternative to `counsel-ag-base-command' using ripgrep.

Note: don't use single quotes for the regex."
  :type 'string)

(counsel-set-async-exit-code 'counsel-rg 1 "No matches found")
(ivy-set-occur 'counsel-rg 'counsel-ag-occur)
(ivy-set-display-transformer 'counsel-rg 'counsel-git-grep-transformer)

;;;###autoload
(defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive)
  (let ((counsel-ag-base-command counsel-rg-base-command))
    (counsel-ag initial-input initial-directory extra-rg-args rg-prompt)))
(cl-pushnew 'counsel-rg ivy-highlight-grep-commands)

;;** `counsel-grep'
(defvar counsel-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-q") 'swiper-query-replace)
    (define-key map (kbd "C-'") 'swiper-avy)
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
   (let ((regex (counsel--elisp-to-pcre
                 (setq ivy--old-re
                       (ivy--regex string)))))
     (counsel--async-command
      (format counsel-grep-command (shell-quote-argument regex)))
     nil)))

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
          (if counsel-grep-last-line
              (forward-line (- line-number counsel-grep-last-line))
            (goto-char (point-min))
            (forward-line (1- line-number)))
          (setq counsel-grep-last-line line-number)
          (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
          (run-hooks 'counsel-grep-post-action-hook)
          (if (eq ivy-exit 'done)
              (swiper--ensure-visible)
            (isearch-range-invisible (line-beginning-position)
                                     (line-end-position))
            (swiper--add-overlays (ivy--regex ivy-text))))))))

(defun counsel-grep-occur ()
  "Generate a custom occur buffer for `counsel-grep'."
  (counsel-grep-like-occur
   (format
    "grep -niE %%s %s /dev/null"
    (shell-quote-argument
     (file-name-nondirectory
      (buffer-file-name
       (ivy-state-buffer ivy-last)))))))

(ivy-set-occur 'counsel-grep 'counsel-grep-occur)
(counsel-set-async-exit-code 'counsel-grep 1 "")

;;;###autoload
(defun counsel-grep (&optional initial-input)
  "Grep for a string in the file visited by the current buffer.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (counsel-require-program (car (split-string counsel-grep-base-command)))
  (setq counsel-grep-last-line nil)
  (setq counsel-grep-command
        (format counsel-grep-base-command
                "%s" (shell-quote-argument buffer-file-name)))
  (let ((init-point (point))
        res)
    (unwind-protect
         (setq res (ivy-read "grep: " 'counsel-grep-function
                             :initial-input initial-input
                             :dynamic-collection t
                             :preselect
                             (when (< (- (line-end-position) (line-beginning-position)) 300)
                               (format "%d:%s"
                                       (line-number-at-pos)
                                       (regexp-quote
                                        (buffer-substring-no-properties
                                         (line-beginning-position)
                                         (line-end-position)))))

                             :keymap counsel-grep-map
                             :history 'counsel-git-grep-history
                             :update-fn (lambda ()
                                          (counsel-grep-action (ivy-state-current ivy-last)))
                             :re-builder #'ivy--regex
                             :action #'counsel-grep-action
                             :unwind (lambda ()
                                       (counsel-delete-process)
                                       (swiper--cleanup))
                             :caller 'counsel-grep))
      (unless res
        (goto-char init-point)))))

;;** `counsel-grep-or-swiper'
(defcustom counsel-grep-swiper-limit 300000
  "Buffer size threshold for `counsel-grep-or-swiper'.
When the number of characters in a buffer exceeds this threshold,
`counsel-grep' will be used instead of `swiper'."
  :type 'integer)

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
          (<= (buffer-size)
              (/ counsel-grep-swiper-limit
                 (if (eq major-mode 'org-mode) 4 1))))
      (swiper initial-input)
    (when (file-writable-p buffer-file-name)
      (save-buffer))
    (counsel-grep initial-input)))

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
            :unwind #'counsel-delete-process
            :caller 'counsel-recoll))

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
           (counsel-org--set-tags)))
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

;;;###autoload
(defun counsel-org-tag-agenda ()
  "Set tags for the current agenda item."
  (interactive)
  (let ((store (symbol-function 'org-set-tags)))
    (unwind-protect
         (progn
           (fset 'org-set-tags
                 (symbol-function 'counsel-org-tag))
           (org-agenda-set-tags nil nil))
      (fset 'org-set-tags store))))

(define-obsolete-variable-alias 'counsel-org-goto-display-tags
    'counsel-org-headline-display-tags "0.10.0")

(defcustom counsel-org-headline-display-tags nil
  "If non-nil, display tags in matched `org-mode' headlines."
  :type 'boolean)

(define-obsolete-variable-alias 'counsel-org-goto-display-todo
    'counsel-org-headline-display-todo "0.10.0")

(defcustom counsel-org-headline-display-todo nil
  "If non-nil, display todo keywords in matched `org-mode' headlines."
  :type 'boolean)

(defcustom counsel-org-headline-display-priority nil
  "If non-nil, display priorities in matched `org-mode' headlines."
  :type 'boolean)

(declare-function org-get-heading "org")
(declare-function org-goto-marker-or-bmk "org")
(declare-function outline-next-heading "outline")

;;;###autoload
(defalias 'counsel-org-goto #'counsel-outline)

;;;###autoload
(defun counsel-org-goto-all ()
  "Go to a different location in any org file."
  (interactive)
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (setq entries (nconc entries (counsel-outline-candidates))))))
    (ivy-read "Goto: " entries
              :history 'counsel-org-goto-history
              :action #'counsel-org-goto-action
              :caller 'counsel-org-goto-all)))

(defun counsel-org-goto-action (x)
  "Go to headline in candidate X."
  (org-goto-marker-or-bmk (cdr x)))

(defvar org-version)

(defun counsel--org-get-heading-args ()
  "Return list of arguments for `org-get-heading'.
Try to return the right number of arguments for the current Org
version.  Argument values are based on the
`counsel-org-headline-display-*' user options."
  (nbutlast (mapcar #'not (list counsel-org-headline-display-tags
                                counsel-org-headline-display-todo
                                counsel-org-headline-display-priority))
            (if (if (fboundp 'func-arity)
                    (< (cdr (func-arity #'org-get-heading)) 3)
                  (version< org-version "9.1.1"))
                1 0)))

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
  (let* ((ids (let (res)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward "^:ID:[\t ]+\\(.*\\)$" nil t)
                    (push (match-string-no-properties 1) res))
                  (nreverse res))))
         (files
          (cl-remove-if-not
           #'file-exists-p
           (mapcar (lambda (id)
                     (expand-file-name
                      (concat (substring id 0 2) "/" (substring id 2))
                      org-attach-directory))
                   ids))))
    (cl-mapcan
     (lambda (dir)
       (mapcar (lambda (file)
                 (file-relative-name (expand-file-name file dir)))
               (org-attach-file-list dir)))
     files)))

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
            (delq nil
                  (mapcar
                   (lambda (x)
                     (when (> (length x) 2)
                       (format "%-5s %s" (nth 0 x) (nth 1 x))))
                   ;; We build the list of capture templates as in
                   ;; `org-capture-select-template':
                   (or (org-contextualize-keys
                        (org-capture-upgrade-templates org-capture-templates)
                        org-capture-templates-contexts)
                       '(("t" "Task" entry (file+headline "" "Tasks")
                          "* TODO %?\n  %u\n  %a")))))
            :require-match t
            :action (lambda (x)
                      (org-capture nil (car (split-string x))))
            :caller 'counsel-org-capture))

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

(define-obsolete-variable-alias 'counsel-org-goto-display-style
    'counsel-outline-display-style "0.10.0")
(define-obsolete-variable-alias 'counsel-org-headline-display-style
    'counsel-outline-display-style "0.10.0")

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

(define-obsolete-variable-alias 'counsel-org-goto-separator
    'counsel-outline-path-separator "0.10.0")
(define-obsolete-variable-alias 'counsel-org-headline-path-separator
    'counsel-outline-path-separator "0.10.0")

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
       (list
        (mapconcat
         'identity
         (cl-remove-if 'null
                       (list
                        level
                        todo
                        (and priority (format "[#%c]" priority))
                        (mapconcat 'identity
                                   (append path (list text))
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

;;* Misc. Emacs
;;** `counsel-mark-ring'
(defun counsel-mark-ring ()
  "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let ((cands
         (save-excursion
           (save-restriction
             ;; Widen, both to save `line-number-at-pos' the trouble
             ;; and for `buffer-substring' to work.
             (widen)
             (let ((fmt (format "%%%dd %%s"
                                (length (number-to-string
                                         (line-number-at-pos (point-max)))))))
               (mapcar (lambda (mark)
                         (goto-char (marker-position mark))
                         (let ((linum (line-number-at-pos))
                               (line  (buffer-substring
                                       (line-beginning-position)
                                       (line-end-position))))
                           (cons (format fmt linum line) (point))))
                       (sort (delete-dups (copy-sequence mark-ring)) #'<)))))))
    (if cands
        (ivy-read "Mark: " cands
                  :require-match t
                  :action (lambda (cand)
                            (let ((pos (cdr-safe cand)))
                              (when pos
                                (unless (<= (point-min) pos (point-max))
                                  (if widen-automatically
                                      (widen)
                                    (error "\
Position of selected mark outside accessible part of buffer")))
                                (goto-char pos))))
                  :caller 'counsel-mark-ring)
      (message "Mark ring is empty"))))

;;** `counsel-package'
(defvar package--initialized)
(defvar package-alist)
(defvar package-archive-contents)
(declare-function package-installed-p "package")
(declare-function package-delete "package")
(declare-function package-desc-extras "package")

(defun counsel--package-candidates ()
  "Return completion alist for `counsel-package'."
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (sort (mapcar (lambda (entry)
                  (cons (let ((pkg (car entry)))
                          (concat (if (package-installed-p pkg) "-" "+")
                                  (symbol-name pkg)))
                        entry))
                package-archive-contents)
        #'counsel--package-sort))

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
            :caller 'counsel-package))

(cl-pushnew '(counsel-package . "^+ ") ivy-initial-inputs-alist :key #'car)

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
(defvar tmm-km-list nil)
(declare-function tmm-get-keymap "tmm")
(declare-function tmm--completion-table "tmm")
(declare-function tmm-get-keybind "tmm")

(defun counsel-tmm-prompt (menu)
  "Select and call an item from the MENU keymap."
  (let (out
        choice
        chosen-string)
    (setq tmm-km-list nil)
    (map-keymap (lambda (k v) (tmm-get-keymap (cons k v))) menu)
    (setq tmm-km-list (nreverse tmm-km-list))
    (setq out (ivy-read "Menu bar: " (tmm--completion-table tmm-km-list)
                        :require-match t
                        :sort nil))
    (setq choice (cdr (assoc out tmm-km-list)))
    (setq chosen-string (car choice))
    (setq choice (cdr choice))
    (cond ((keymapp choice)
           (counsel-tmm-prompt choice))
          ((and choice chosen-string)
           (setq last-command-event chosen-string)
           (call-interactively choice)))))

(defvar tmm-table-undef)

;;;###autoload
(defun counsel-tmm ()
  "Text-mode emulation of looking and choosing from a menubar."
  (interactive)
  (require 'tmm)
  (run-hooks 'menu-bar-update-hook)
  (setq tmm-table-undef nil)
  (counsel-tmm-prompt (tmm-get-keybind [menu-bar])))

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

(make-obsolete-variable
 'counsel-yank-pop-height
 'ivy-height-alist
 "<2018-04-14 Fri>") ;; TODO: Add version tag

(defcustom counsel-yank-pop-height 5
  "The `ivy-height' of `counsel-yank-pop'."
  :type 'integer)

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

(defun counsel--yank-pop-position (s)
  "Return position of S in `kill-ring' relative to last yank."
  (or (cl-position s kill-ring-yank-pointer :test #'equal-including-properties)
      (cl-position s kill-ring-yank-pointer :test #'equal)
      (+ (or (cl-position s kill-ring :test #'equal-including-properties)
             (cl-position s kill-ring :test #'equal))
         (- (length kill-ring-yank-pointer)
            (length kill-ring)))))

(defun counsel-string-non-blank-p (s)
  "Return non-nil if S includes non-blank characters.
Newlines and carriage returns are considered blank."
  (not (string-match-p "\\`[\n\r[:blank:]]*\\'" s)))

(defcustom counsel-yank-pop-filter #'counsel-string-non-blank-p
  "Unary filter function applied to `counsel-yank-pop' candidates.
All elements of `kill-ring' for which this function returns nil
will be destructively removed from `kill-ring' before completion.
All blank strings are deleted from `kill-ring' by default."
  :type '(radio
          (function-item counsel-string-non-blank-p)
          (function-item identity)
          (function :tag "Other")))

(defun counsel--yank-pop-kills ()
  "Return filtered `kill-ring' for `counsel-yank-pop' completion.
Both `kill-ring' and `kill-ring-yank-pointer' may be
destructively modifed to eliminate duplicates under
`equal-including-properties', satisfy `counsel-yank-pop-filter',
and incorporate `interprogram-paste-function'."
  ;; Protect against `kill-ring' and result of
  ;; `interprogram-paste-function' both being nil
  (ignore-errors (current-kill 0))
  ;; Keep things consistent with the rest of Emacs
  (dolist (sym '(kill-ring kill-ring-yank-pointer))
    (set sym (cl-delete-duplicates
              (cl-delete-if-not counsel-yank-pop-filter (symbol-value sym))
              :test #'equal-including-properties :from-end t)))
  kill-ring)

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
  (with-ivy-window
    (barf-if-buffer-read-only)
    (setq last-command 'yank)
    (setq yank-window-start (window-start))
    ;; Avoid unexpected additions to `kill-ring'
    (let (interprogram-paste-function)
      (yank-pop (counsel--yank-pop-position s)))
    (when (funcall (if counsel-yank-pop-after-point #'> #'<)
                   (point) (mark t))
      (exchange-point-and-mark t))))

(defun counsel-yank-pop-action-remove (s)
  "Remove all occurrences of S from the kill ring."
  (dolist (sym '(kill-ring kill-ring-yank-pointer))
    (set sym (cl-delete s (symbol-value sym)
                        :test #'equal-including-properties)))
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

(autoload 'xor "array")

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
         (xor (consp arg) counsel-yank-pop-after-point))
        (ivy-format-function #'counsel--yank-pop-format-function))
    (unless (eq last-command 'yank)
      (push-mark))
    (ivy-read "kill-ring: " kills
              :require-match t
              :preselect preselect
              :action #'counsel-yank-pop-action
              :caller 'counsel-yank-pop)))

(add-to-list 'ivy-height-alist '(counsel-yank-pop . 5))

(ivy-set-actions
 'counsel-yank-pop
 '(("d" counsel-yank-pop-action-remove "delete")
   ("r" counsel-yank-pop-action-rotate "rotate")))

;;** `counsel-evil-registers'
(make-obsolete-variable
 'counsel-evil-registers-height
 'ivy-height-alist
 "<2018-04-14 Fri>") ;; TODO: Add version tag

(defcustom counsel-evil-registers-height 5
  "The `ivy-height' of `counsel-evil-registers'."
  :type 'integer)

(defun counsel-evil-registers ()
  "Ivy replacement for `evil-show-registers'."
  (interactive)
  (if (fboundp 'evil-register-list)
      (let ((ivy-format-function #'counsel--yank-pop-format-function))
        (ivy-read "evil-registers: "
                  (cl-loop for (key . val) in (evil-register-list)
                     collect (format "[%c]: %s" key (if (stringp val) val "")))
                  :require-match t
                  :action #'counsel-evil-registers-action
                  :caller 'counsel-evil-registers))
    (user-error "Required feature `evil' not installed.")))

(add-to-list 'ivy-height-alist '(counsel-evil-registers . 5))

(defun counsel-evil-registers-action (s)
  "Paste contents of S, trimming the register part.

S will be of the form \"[register]: content\"."
  (with-ivy-window
    (insert
     (replace-regexp-in-string "\\`\\[.*?\\]: " "" s))))

;;** `counsel-imenu'
(defvar imenu-auto-rescan)
(defvar imenu-auto-rescan-maxout)
(declare-function imenu--subalist-p "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun counsel-imenu-get-candidates-from (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (cl-mapcan (lambda (elm)
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
                               ;; create a imenu candidate here
                               (cons key (if (overlayp (cdr elm))
                                             (overlay-start (cdr elm))
                                           (cdr elm))))))))
             alist))

(defvar counsel-imenu-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    map))

(defun counsel-imenu-categorize-functions (items)
  "Categorize all the functions of imenu."
  (let ((fns (cl-remove-if #'listp items :key #'cdr)))
    (if fns
        (nconc (cl-remove-if #'nlistp items :key #'cdr)
               `(("Functions" ,@fns)))
      items)))

;;;###autoload
(defun counsel-imenu ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items))
         (items (counsel-imenu-categorize-functions items)))
    (ivy-read "imenu items: " (counsel-imenu-get-candidates-from items)
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action (lambda (candidate)
                        (with-ivy-window
                          ;; In org-mode, (imenu candidate) will expand child node
                          ;; after jump to the candidate position
                          (imenu (cdr candidate))))
              :keymap counsel-imenu-map
              :caller 'counsel-imenu)))

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
(defun counsel-expression-history ()
  "Select an element of `read-expression-history'.
And insert it into the minibuffer.  Useful during `eval-expression'."
  (declare (obsolete counsel-minibuffer-history "0.10.0 <2017-11-13 Mon>"))
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Expression: "
              (delete-dups (copy-sequence read-expression-history))
              :action #'insert
              :caller 'counsel-expression-history)))

;;;###autoload
(defun counsel-shell-command-history ()
  "Browse shell command history."
  (declare (obsolete counsel-minibuffer-history "0.10.0 <2017-11-13 Mon>"))
  (interactive)
  (ivy-read "Command: " shell-command-history
            :action #'insert
            :caller 'counsel-shell-command-history))

;;;###autoload
(defun counsel-minibuffer-history ()
  "Browse minibuffer history."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "History: "
              (delete-dups (copy-sequence
                            (symbol-value minibuffer-history-variable)))
              :action #'insert
              :caller 'counsel-minibuffer-history)))

;;** `counsel-esh-history'
(defun counsel--browse-history (elements)
  "Use Ivy to navigate through ELEMENTS."
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (let ((cands
         (delete-dups
          (when (> (ring-size elements) 0)
            (ring-elements elements)))))
    (ivy-read "Symbol name: " cands
              :action #'ivy-completion-in-region-action
              :caller 'counsel-shell-history)))

(defvar eshell-history-ring)

;;;###autoload
(defun counsel-esh-history ()
  "Browse Eshell history."
  (interactive)
  (require 'em-hist)
  (counsel--browse-history eshell-history-ring))

(defvar comint-input-ring)

;;;###autoload
(defun counsel-shell-history ()
  "Browse shell history."
  (interactive)
  (require 'comint)
  (counsel--browse-history comint-input-ring))

;;** `counsel-hydra-heads'
(defvar hydra-curr-body-fn)
(declare-function hydra-keyboard-quit "ext:hydra")

(defun counsel-hydra-heads ()
  "Call a head of the current/last hydra."
  (interactive)
  (let* ((base (substring
                (prin1-to-string hydra-curr-body-fn)
                0 -4))
         (heads (eval (intern (concat base "heads"))))
         (keymap (eval (intern (concat base "keymap"))))
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

(defun counsel-semantic-or-imenu ()
  (interactive)
  (require 'semantic/fw)
  (if (semantic-active-p)
      (counsel-semantic)
    (counsel-imenu)))

;;** `counsel-outline'
(define-obsolete-variable-alias 'counsel-org-goto-face-style
    'counsel-outline-face-style "0.10.0")

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

(define-obsolete-variable-alias 'counsel-org-goto-custom-faces
    'counsel-outline-custom-faces "0.10.0")

(defcustom counsel-outline-custom-faces nil
  "List of faces for custom display of outline headings.

Headlines on level N are fontified with the Nth entry of this
list, starting with N = 1.  Headline levels with no corresponding
entry in this list will not be styled.

This variable has no effect unless `counsel-outline-face-style'
is set to `custom'."
  :type '(repeat face))

(defvar counsel-outline-settings
  '((emacs-lisp-mode
     :outline-regexp ";;[;*]+[\s\t]+"
     :outline-level counsel-outline-level-emacs-lisp)
    (org-mode
     :outline-title counsel-outline-title-org
     :action counsel-org-goto-action
     :history counsel-org-goto-history
     :caller counsel-org-goto)
    (markdown-mode                      ; markdown-mode package
     :outline-title counsel-outline-title-markdown)
    (latex-mode                         ; Built-in mode or AUCTeX package
     :outline-title counsel-outline-title-latex))
  "Alist mapping major modes to their `counsel-outline' settings.

Each entry is a pair (MAJOR-MODE . PLIST).  `counsel-outline'
checks whether an entry exists for the current buffer's
MAJOR-MODE and, if so, loads the settings specified by PLIST
instead of the default settings.  The following settings are
recognized:

- `:outline-regexp' is a regexp to match the beggining of an
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

- `:caller' is a symbol to uniquely idendify the caller to
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

(defun counsel-outline-title ()
  "Return title of current outline heading.
Intended as a value for the `:outline-title' setting in
`counsel-outline-settings', which see."
  (buffer-substring (point) (line-end-position)))

(defun counsel-outline-title-org ()
  "Return title of current outline heading.
Like `counsel-outline-title' (which see), but for `org-mode'
buffers."
  (apply #'org-get-heading (counsel--org-get-heading-args)))

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
  "Index of the presected candidate in `counsel-outline'.")

(defun counsel-outline-candidates (&optional settings)
  "Return an alist of outline heading completion candidates.
Each element is a pair (HEADING . MARKER), where the string
HEADING is located at the position of MARKER.  SETTINGS is a
plist entry from `counsel-outline-settings', which see."
  (let ((bol-regex (concat "^\\(?:"
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
        cands name level marker stack)
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
  (ivy-read "Switch to buffer: " (counsel-ibuffer--get-buffers)
            :history 'counsel-ibuffer-history
            :action #'counsel-ibuffer-visit-buffer
            :caller 'counsel-ibuffer))

(declare-function ibuffer-update "ibuffer")
(declare-function ibuffer-current-buffer "ibuffer")
(declare-function ibuffer-forward-line "ibuffer")
(defvar ibuffer-movement-cycle)

(defun counsel-ibuffer--get-buffers ()
  "Return list of buffer-related lines in Ibuffer as strings."
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
  (switch-to-buffer (cdr x)))

(defun counsel-ibuffer-visit-buffer-other-window (x)
  "Switch to buffer of candidate X in another window."
  (switch-to-buffer-other-window (cdr x)))

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
  (let ((ivy-sort-max-size (expt 256 6)))
    (setq ivy-completion-beg (point))
    (setq ivy-completion-end (point))
    (ivy-read "Unicode name: " counsel--unicode-table
              :history 'counsel-unicode-char-history
              :sort t
              :action (lambda (name)
                        (with-ivy-window
                          (delete-region ivy-completion-beg ivy-completion-end)
                          (setq ivy-completion-beg (point))
                          (insert-char (get-text-property 0 'code name) count)
                          (setq ivy-completion-end (point))))
              :caller 'counsel-unicode-char)))

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

(defun counsel-colors--formatter (formatter)
  "Turn FORMATTER into format function for `counsel-colors-*'.
Return closure suitable for `ivy-format-function'."
  (require 'shr-color)
  (lambda (colors)
    (ivy--format-function-generic
     (lambda (color)
       (let* ((hex (get-text-property 0 'hex color))
              (shr-color-visible-luminance-min 100)
              (fg (cadr (shr-color-visible hex "black" t))))
         (propertize (funcall formatter color)
                     'face (list :foreground fg :background hex))))
     formatter colors "\n")))

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
         (fmt (format "%%-%ds %%s %%s%%s"
                      (apply #'max 0 (mapcar #'string-width colors))))
         (blank (make-string 10 ?\s))
         (ivy-format-function
          (counsel-colors--formatter
           (lambda (color)
             (let ((fg (list :foreground color)))
               (format fmt color
                       (propertize (get-text-property 0 'hex color) 'face fg)
                       (propertize blank 'face (list :background color))
                       (propertize (mapconcat (lambda (dup)
                                                (concat " " dup))
                                              (get-text-property 0 'dups color)
                                              ",")
                                   'face fg)))))))
    (ivy-read "Emacs color: " colors
              :require-match t
              :history 'counsel-colors-emacs-history
              :action #'insert
              :caller 'counsel-colors-emacs)))

(ivy-set-actions
 'counsel-colors-emacs
 '(("h" counsel-colors-action-insert-hex "insert hexadecimal value")
   ("H" counsel-colors-action-kill-hex "kill hexadecimal value")))

;;** `counsel-colors-web'
(defvar shr-color-html-colors-alist)

(defun counsel-colors--web-alist ()
  "Return list of CSS colours for `counsel-colors-web'."
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
         (blank (make-string 10 ?\s))
         (fmt (format "%%-%ds %%s %%s"
                      (apply #'max 0 (mapcar #'string-width colors))))
         (ivy-format-function
          (counsel-colors--formatter
           (lambda (color)
             (let ((hex (get-text-property 0 'hex color)))
               (format fmt color
                       (propertize hex 'face (list :foreground hex))
                       (propertize blank 'face (list :background hex))))))))
    (ivy-read "Web color: " colors
              :require-match t
              :history 'counsel-colors-web-history
              :sort t
              :action #'insert
              :caller 'counsel-colors-web)))

(ivy-set-actions
 'counsel-colors-web
 '(("h" counsel-colors-action-insert-hex "insert hexadecimal value")
   ("H" counsel-colors-action-kill-hex "kill hexadecimal value")))

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

(defun counsel-rhythmbox-toggle-shuffle (_song)
  "Toggle Rhythmbox shuffle setting."
  (let* ((old-order (counsel--call "dconf" "read" "/org/gnome/rhythmbox/player/play-order"))
         (new-order (if (string= old-order "'shuffle'")
                        "'linear'"
                      "'shuffle'")))
    (counsel--call
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
            :history 'counsel-rhythmbox-history
            :preselect (counsel-rhythmbox-current-song)
            :action
            '(1
              ("p" counsel-rhythmbox-play-song "Play song")
              ("e" counsel-rhythmbox-enqueue-song "Enqueue song")
              ("s" counsel-rhythmbox-toggle-shuffle "Shuffle on/off"))
            :caller 'counsel-rhythmbox))

;;** `counsel-linux-app'
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
           (ivy--truncate-string exec 45)
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

(defun counsel-linux-apps-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir counsel-linux-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
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
         (new-files (mapcar 'cdr new-desktop-alist)))
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
  (ivy-read "Run a command: " (counsel-linux-apps-list)
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

(defun counsel-wmctrl ()
  "Select a desktop window using wmctrl."
  (interactive)
  (let* ((cands1 (counsel--sl "wmctrl -l -p"))
         (cands2 (delq nil (mapcar #'counsel--wmctrl-parse cands1))))
    (ivy-read "window: " cands2
              :action #'counsel-wmctrl-action
              :caller 'counsel-wmctrl)))

;;* `counsel-mode'
(defvar counsel-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding
              '((execute-extended-command . counsel-M-x)
                (describe-bindings . counsel-descbinds)
                (describe-function . counsel-describe-function)
                (describe-variable . counsel-describe-variable)
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
        (when (and (fboundp 'advice-add)
                   counsel-mode-override-describe-bindings)
          (advice-add #'describe-bindings :override #'counsel-descbinds))
        (define-key minibuffer-local-map (kbd "C-r")
          'counsel-minibuffer-history))
    (when (fboundp 'advice-remove)
      (advice-remove #'describe-bindings #'counsel-descbinds))))

(provide 'counsel)

;;; counsel.el ends here
