;;; ivy.el --- Incremental Vertical completYon -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.13.0
;; Package-Requires: ((emacs "24.5"))
;; Keywords: matching

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

;; This package provides `ivy-read' as an alternative to
;; `completing-read' and similar functions.
;;
;; There's no intricate code to determine the best candidate.
;; Instead, the user can navigate to it with `ivy-next-line' and
;; `ivy-previous-line'.
;;
;; The matching is done by splitting the input text by spaces and
;; re-building it into a regex.
;; So "for example" is transformed into "\\(for\\).*\\(example\\)".

;;; Code:

(require 'colir)
(require 'ivy-overlay)
(require 'ivy-faces)

(require 'cl-lib)
(require 'ring)

(eval-when-compile
  (require 'subr-x))

;;* Customization
(defgroup ivy nil
  "Incremental vertical completion."
  :group 'convenience)

(defcustom ivy-height 10
  "Number of lines for the minibuffer window.

See also `ivy-height-alist'."
  :type 'integer)

(defcustom ivy-count-format "%-4d "
  "The style to use for displaying the current candidate count for `ivy-read'.
Set this to \"\" to suppress the count visibility.
Set this to \"(%d/%d) \" to display both the index and the count."
  :type '(choice
          (const :tag "Count disabled" "")
          (const :tag "Count matches" "%-4d ")
          (const :tag "Count matches and show current match" "(%d/%d) ")
          string))

(defcustom ivy-pre-prompt-function nil
  "When non-nil, add strings before the `ivy-read' prompt."
  :type '(choice
          (const :tag "Do nothing" nil)
          (function :tag "Custom function")))

(defcustom ivy-add-newline-after-prompt nil
  "When non-nil, add a newline after the `ivy-read' prompt."
  :type 'boolean)

(defcustom ivy-wrap nil
  "When non-nil, wrap around after the first and the last candidate."
  :type 'boolean)

(defcustom ivy-display-style 'fancy
  "The style for formatting the minibuffer.

By default, the matched strings are copied as is.

The fancy display style highlights matching parts of the regexp,
a behavior similar to `swiper'."
  :type '(choice
          (const :tag "Plain" nil)
          (const :tag "Fancy" fancy)))

(defcustom ivy-on-del-error-function #'abort-recursive-edit
  "Function to call when deletion fails during completion.
The usual reason for `ivy-backward-delete-char' to fail is when
there is no text left to delete, i.e., when it is called at the
beginning of the minibuffer.
The default setting provides a quick exit from completion."
  :type '(choice
          (const :tag "Exit completion" abort-recursive-edit)
          (const :tag "Do nothing" ignore)
          (function :tag "Custom function")))

(defcustom ivy-extra-directories '("../" "./")
  "Add this to the front of the list when completing file names.
Only \"./\" and \"../\" apply here.  They appear in reverse order."
  :type '(repeat :tag "Dirs"
          (choice
           (const :tag "Parent Directory" "../")
           (const :tag "Current Directory" "./"))))

(defcustom ivy-use-virtual-buffers nil
  "When non-nil, add recent files and/or bookmarks to `ivy-switch-buffer'.
The value `recentf' includes only recent files to the virtual
buffers list, whereas the value `bookmarks' does the same for
bookmarks.  Any other non-nil value includes both."
  :type '(choice
          (const :tag "Don't use virtual buffers" nil)
          (const :tag "Recent files" recentf)
          (const :tag "Bookmarks" bookmarks)
          (const :tag "All virtual buffers" t)))

(defvar ivy--display-function nil
  "The display-function is used in current.")

(defvar ivy-display-functions-props
  '((ivy-display-function-overlay :cleanup ivy-overlay-cleanup))
  "Map Ivy display functions to their property lists.
Examples of properties include associated `:cleanup' functions.")

(defcustom ivy-display-functions-alist
  '((ivy-completion-in-region . ivy-display-function-overlay)
    (t . nil))
  "An alist for customizing where to display the candidates.

Each key is a caller symbol.  When the value is nil (the default),
the candidates are shown in the minibuffer.  Otherwise, the value
is a function which takes a string argument comprising the
current matching candidates and displays it somewhere.

See also `https://github.com/abo-abo/swiper/wiki/ivy-display-function'."
  :type '(alist
          :key-type symbol
          :value-type (choice
                       (const :tag "Minibuffer" nil)
                       (const :tag "LV" ivy-display-function-lv)
                       (const :tag "Popup" ivy-display-function-popup)
                       (const :tag "Overlay" ivy-display-function-overlay)
                       (function :tag "Custom function"))))

(defvar ivy-completing-read-dynamic-collection nil
  "Run `ivy-completing-read' with `:dynamic-collection t`.")

(defcustom ivy-completing-read-handlers-alist
  '((tmm-menubar . completing-read-default)
    (tmm-shortcut . completing-read-default)
    (bbdb-create . ivy-completing-read-with-empty-string-def)
    (auto-insert . ivy-completing-read-with-empty-string-def)
    (Info-on-current-buffer . ivy-completing-read-with-empty-string-def)
    (Info-follow-reference . ivy-completing-read-with-empty-string-def)
    (Info-menu . ivy-completing-read-with-empty-string-def)
    (Info-index . ivy-completing-read-with-empty-string-def)
    (Info-virtual-index . ivy-completing-read-with-empty-string-def)
    (info-display-manual . ivy-completing-read-with-empty-string-def))
  "An alist of handlers to replace `completing-read' in `ivy-mode'."
  :type '(alist :key-type symbol :value-type function))

(defcustom ivy-height-alist nil
  "An alist to customize `ivy-height'.

It is a list of (CALLER . HEIGHT).  CALLER is a caller of
`ivy-read' and HEIGHT is the number of lines displayed.
HEIGHT can also be a function that returns the number of lines."
  :type '(alist
          :key-type function
          :value-type (choice integer function)))

(defvar ivy-completing-read-ignore-handlers-depth -1
  "Used to avoid infinite recursion.

If `(minibuffer-depth)' equals this, `ivy-completing-read' will
act as if `ivy-completing-read-handlers-alist' is empty.")

(defvar ivy-highlight-grep-commands nil
  "List of grep-like commands.")

(defvar ivy--actions-list nil
  "A list of extra actions per command.")

(defun ivy-set-actions (cmd actions)
  "Set CMD extra exit points to ACTIONS."
  (setq ivy--actions-list
        (plist-put ivy--actions-list cmd actions)))

(defun ivy-add-actions (cmd actions)
  "Add extra exit points ACTIONS to CMD.
Existing exit points of CMD are overwritten by those in
ACTIONS that have the same key."
  (setq ivy--actions-list
        (plist-put ivy--actions-list cmd
                   (cl-delete-duplicates
                    (append (plist-get ivy--actions-list cmd) actions)
                    :key #'car :test #'equal))))

(defun ivy--compute-extra-actions (action caller)
  "Add extra actions to ACTION based on CALLER."
  (let* ((extra-actions (cl-delete-duplicates
                         (append (plist-get ivy--actions-list t)
                                 (plist-get ivy--actions-list this-command)
                                 (plist-get ivy--actions-list caller))
                         :key #'car :test #'equal))
         (override-default (assoc "o" extra-actions)))
    (cond (override-default
           (cons 1 (cons override-default
                         (cl-delete "o" extra-actions
                                    :key #'car :test #'equal))))
          ((not extra-actions)
           action)
          ((functionp action)
           `(1
             ("o" ,action "default")
             ,@extra-actions))
          ((null action)
           `(1
             ("o" identity "default")
             ,@extra-actions))
          (t
           (cons (car action)
                 (cl-delete-duplicates (cdr (append action extra-actions))
                                       :key #'car :test #'equal :from-end t))))))

(defvar ivy--prompts-list nil)

(defun ivy-set-prompt (caller prompt-fn)
  "Associate CALLER with PROMPT-FN.
PROMPT-FN is a function of no arguments that returns a prompt string."
  (setq ivy--prompts-list
        (plist-put ivy--prompts-list caller prompt-fn)))

(defvar ivy--display-transformers-alist nil
  "A list of str->str transformers per command.")

(defun ivy-set-display-transformer (cmd transformer)
  "Set CMD a displayed candidate TRANSFORMER.

It's a lambda that takes a string one of the candidates in the
collection and returns a string for display, the same candidate
plus some extra information.

This lambda is called only on the `ivy-height' candidates that
are about to be displayed, not on the whole collection."
  (declare (obsolete "Use `ivy-configure' :display-transformer-fn" "<2020-05-20 Wed>"))
  (ivy--alist-set 'ivy--display-transformers-alist cmd transformer))

(defvar ivy--sources-list nil
  "A list of extra sources per command.")

(defun ivy-set-sources (cmd sources)
  "Attach to CMD a list of extra SOURCES.

Each static source is a function that takes no argument and
returns a list of strings.

The (original-source) determines the position of the original
dynamic source.

Extra dynamic sources aren't supported yet.

Example:

    (defun small-recentf ()
      (cl-subseq recentf-list 0 20))

    (ivy-set-sources
     'counsel-locate
     '((small-recentf)
       (original-source)))"
  (setq ivy--sources-list
        (plist-put ivy--sources-list cmd sources)))

(defun ivy--compute-extra-candidates (caller)
  (let ((extra-sources (or (plist-get ivy--sources-list caller)
                           '((original-source))))
        (result nil))
    (dolist (source extra-sources)
      (cond ((equal source '(original-source))
             (push source result))
            ((null (cdr source))
             (push (list (car source) (funcall (car source))) result))))
    result))

(defvar ivy-current-prefix-arg nil
  "Prefix arg to pass to actions.
This is a global variable that is set by ivy functions for use in
action functions.")

;;* Keymap
(require 'delsel)
(defun ivy-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY DEF).
Remove DEF from `counsel-M-x' list."
  (put def 'no-counsel-M-x t)
  (define-key keymap key def))

(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (ivy-define-key map (kbd "C-m") 'ivy-done)
    (define-key map [down-mouse-1] 'ignore)
    (ivy-define-key map [mouse-1] 'ivy-mouse-done)
    (ivy-define-key map [mouse-3] 'ivy-mouse-dispatching-done)
    (ivy-define-key map (kbd "C-M-m") 'ivy-call)
    (ivy-define-key map (kbd "C-j") 'ivy-alt-done)
    (ivy-define-key map (kbd "C-M-j") 'ivy-immediate-done)
    (ivy-define-key map (kbd "TAB") 'ivy-partial-or-done)
    (ivy-define-key map [remap next-line] 'ivy-next-line)
    (ivy-define-key map [remap previous-line] 'ivy-previous-line)
    (ivy-define-key map (kbd "C-r") 'ivy-reverse-i-search)
    (define-key map (kbd "SPC") 'self-insert-command)
    (ivy-define-key map [remap delete-backward-char] 'ivy-backward-delete-char)
    (ivy-define-key map [remap backward-delete-char-untabify] 'ivy-backward-delete-char)
    (ivy-define-key map [remap backward-kill-word] 'ivy-backward-kill-word)
    (ivy-define-key map [remap delete-char] 'ivy-delete-char)
    (ivy-define-key map [remap forward-char] 'ivy-forward-char)
    (ivy-define-key map (kbd "<right>") 'ivy-forward-char)
    (ivy-define-key map [remap kill-word] 'ivy-kill-word)
    (ivy-define-key map [remap beginning-of-buffer] 'ivy-beginning-of-buffer)
    (ivy-define-key map [remap end-of-buffer] 'ivy-end-of-buffer)
    (ivy-define-key map (kbd "M-n") 'ivy-next-history-element)
    (ivy-define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (ivy-define-key map [remap scroll-up-command] 'ivy-scroll-up-command)
    (ivy-define-key map [remap scroll-down-command] 'ivy-scroll-down-command)
    (ivy-define-key map (kbd "<next>") 'ivy-scroll-up-command)
    (ivy-define-key map (kbd "<prior>") 'ivy-scroll-down-command)
    (ivy-define-key map (kbd "C-v") 'ivy-scroll-up-command)
    (ivy-define-key map (kbd "M-v") 'ivy-scroll-down-command)
    (ivy-define-key map (kbd "C-M-n") 'ivy-next-line-and-call)
    (ivy-define-key map (kbd "C-M-p") 'ivy-previous-line-and-call)
    (ivy-define-key map (kbd "M-a") 'ivy-toggle-marks)
    (ivy-define-key map (kbd "M-r") 'ivy-toggle-regexp-quote)
    (ivy-define-key map (kbd "M-j") 'ivy-yank-word)
    (ivy-define-key map (kbd "M-i") 'ivy-insert-current)
    (ivy-define-key map (kbd "C-M-y") 'ivy-insert-current-full)
    (ivy-define-key map (kbd "C-o") 'hydra-ivy/body)
    (ivy-define-key map (kbd "M-o") 'ivy-dispatching-done)
    (ivy-define-key map (kbd "C-M-o") 'ivy-dispatching-call)
    (ivy-define-key map [remap kill-line] 'ivy-kill-line)
    (ivy-define-key map [remap kill-whole-line] 'ivy-kill-whole-line)
    (ivy-define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (ivy-define-key map [remap kill-ring-save] 'ivy-kill-ring-save)
    (ivy-define-key map (kbd "C-M-a") 'ivy-read-action)
    (ivy-define-key map (kbd "C-c C-o") 'ivy-occur)
    (ivy-define-key map (kbd "C-c C-a") 'ivy-toggle-ignore)
    (ivy-define-key map (kbd "C-c C-s") 'ivy-rotate-sort)
    (ivy-define-key map [remap describe-mode] 'ivy-help)
    (ivy-define-key map "$" 'ivy-magic-read-file-env)
    map)
  "Keymap used in the minibuffer.")
(autoload 'hydra-ivy/body "ivy-hydra" "" t)
(autoload 'ivy-hydra-read-action "ivy-hydra" "" t)

(defvar ivy-mode-map
  (let ((map (make-sparse-keymap)))
    (ivy-define-key map [remap switch-to-buffer] 'ivy-switch-buffer)
    (ivy-define-key map [remap switch-to-buffer-other-window] 'ivy-switch-buffer-other-window)
    map)
  "Keymap for `ivy-mode'.")

;;* Globals
(cl-defstruct ivy-state
  prompt collection
  predicate require-match initial-input
  history preselect keymap update-fn sort
  ;; The frame in which `ivy-read' was called
  frame
  ;; The window in which `ivy-read' was called
  window
  ;; The buffer in which `ivy-read' was called
  buffer
  ;; The value of `ivy-text' to be used by `ivy-occur'
  text
  action
  unwind
  re-builder
  matcher
  ;; When this is non-nil, call it for each input change to get new candidates
  dynamic-collection
  ;; A lambda that transforms candidates only for display
  display-transformer-fn
  directory
  caller
  current
  def
  ignore
  multi-action
  extra-props)

(defvar ivy-last (make-ivy-state)
  "The last parameters passed to `ivy-read'.

This should eventually become a stack so that you could use
`ivy-read' recursively.")

(defvar ivy--sessions nil
  "Alist mapping session symbols to `ivy-state' objects.")

(defvar ivy-recursive-last nil)

(defvar ivy-recursive-restore t
  "When non-nil, restore the above state when exiting the minibuffer.
This variable is let-bound to nil by functions that take care of
the restoring themselves.")

(defsubst ivy-set-action (action)
  "Set the current `ivy-last' field to ACTION."
  (setf (ivy-state-action ivy-last) action))

(defvar inhibit-message)

(defvar ffap-machine-p-known)

(defun ivy-thing-at-point ()
  "Return a string that corresponds to the current thing at point."
  (substring-no-properties
   (cond
     ((use-region-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (eol (save-excursion (goto-char beg) (line-end-position))))
        (buffer-substring-no-properties beg (min end eol))))
     ((thing-at-point 'url))
     ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (let ((inhibit-message t)
                 (ffap-machine-p-known 'reject))
             (run-hook-with-args-until-success 'file-name-at-point-functions))))
     ((let ((s (thing-at-point 'symbol)))
        (and (stringp s)
             (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
                 (match-string 1 s)
               s))))
     ((looking-at "(+\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
      (match-string-no-properties 1))
     (t
      ""))))

(defvar ivy-history nil
  "History list of candidates entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length'.")

(defvar ivy--directory nil
  "Current directory when completing file names.")

(defvar ivy--directory-hist nil
  "Store the history of directories.
This allows RET to reverse consecutive DEL.")

(defvar ivy--length 0
  "Store the amount of viable candidates.")

(defvar ivy-text ""
  "Store the user's string as it is typed in.")

(defvar ivy-regex ""
  "Store the regex value that corresponds to `ivy-text'.")

(defvar ivy--regex-function 'ivy--regex
  "Current function for building a regex.")

(defun ivy-set-text (str)
  "Set `ivy-text' to STR."
  (setq ivy-text str)
  (setq ivy-regex (funcall ivy--regex-function ivy-text)))

(defvar ivy--index 0
  "Store the index of the current candidate.")

(defvar ivy--window-index 0
  "Store the index of the current candidate in the minibuffer window.

This means it's between 0 and `ivy-height'.")

(defvar ivy-exit nil
  "Store `done' if the completion was successfully selected.
Otherwise, store nil.")

(defvar ivy--all-candidates nil
  "Store the candidates passed to `ivy-read'.")

(defvar ivy--extra-candidates '((original-source))
  "Store candidates added by the extra sources.

This is an internal-use alist.  Each key is a function name, or
original-source (which represents where the current dynamic
candidates should go).

Each value is an evaluation of the function, in case of static
sources.  These values will subsequently be filtered on `ivy-text'.

This variable is set by `ivy-read' and used by `ivy--set-candidates'.")

(defcustom ivy-use-ignore-default t
  "The default policy for user-configured candidate filtering."
  :type '(choice
          (const :tag "Ignore ignored always" always)
          (const :tag "Ignore ignored when others exist" t)
          (const :tag "Don't ignore" nil)))

(defvar ivy-use-ignore t
  "Store policy for user-configured candidate filtering.
This may be changed dynamically by `ivy-toggle-ignore'.
Use `ivy-use-ignore-default' for a permanent configuration.")

(defvar ivy--default nil
  "Default initial input.")

(defvar ivy--prompt nil
  "Store the format-style prompt.
When non-nil, it should contain at least one %d.")

(defvar ivy--prompt-extra ""
  "Temporary modifications to the prompt.")

(defvar ivy--old-re nil
  "Store the old regexp.
Either a string or a list for `ivy-re-match'.")

(defvar ivy--old-cands nil
  "Store the candidates matched by `ivy--old-re'.")

(defvar ivy--highlight-function 'ivy--highlight-default
  "Current function for formatting the candidates.")

(defvar ivy--subexps 0
  "Number of groups in the current `ivy--regex'.")

(defvar ivy--full-length nil
  "The total amount of candidates when :dynamic-collection is non-nil.")

(defvar ivy--old-text ""
  "Store old `ivy-text' for dynamic completion.")

(defvar ivy--trying-to-resume-dynamic-collection nil
  "Non-nil if resuming from a dynamic collection.
When non-nil, ivy will wait until the first chunk of asynchronous
candidates has been received before selecting the last
preselected candidate.")

(defun ivy--set-index-dynamic-collection ()
  (when ivy--trying-to-resume-dynamic-collection
    (let ((preselect-index
           (ivy--preselect-index (ivy-state-preselect ivy-last) ivy--all-candidates)))
      (when preselect-index
        (ivy-set-index preselect-index)))
    (setq ivy--trying-to-resume-dynamic-collection nil)))

(defcustom ivy-case-fold-search-default
  (if search-upper-case
      'auto
    case-fold-search)
  "The default value for `case-fold-search' in Ivy operations.
The special value `auto' means case folding is performed so long
as the entire input string comprises lower-case characters.  This
corresponds to the default behaviour of most Emacs search
functionality, e.g. as seen in `isearch'."
  :link '(info-link "(emacs)Lax Search")
  :type '(choice
          (const :tag "Auto" auto)
          (const :tag "Always" t)
          (const :tag "Never" nil)))

(defvar ivy-case-fold-search ivy-case-fold-search-default
  "Store the current overriding `case-fold-search'.")

(defcustom ivy-more-chars-alist
  '((t . 3))
  "Map commands to their minimum required input length.
That is the number of characters prompted for before fetching
candidates.  The special key t is used as a fallback."
  :type '(alist :key-type symbol :value-type integer))

(defun ivy-more-chars ()
  "Return two fake candidates prompting for at least N input.
N is obtained from `ivy-more-chars-alist'."
  (let ((diff (- (ivy-alist-setting ivy-more-chars-alist)
                 (length ivy-text))))
    (when (> diff 0)
      (list "" (format "%d chars more" diff)))))

(defun ivy--case-fold-p (string)
  "Return nil if STRING should be matched case-sensitively."
  (if (eq ivy-case-fold-search 'auto)
      (string= string (downcase string))
    ivy-case-fold-search))

(defun ivy--case-fold-string= (s1 s2)
  "Like `string=', but obeys `case-fold-search'."
  (eq t (compare-strings s1 nil nil s2 nil nil case-fold-search)))

(defmacro ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  (declare (indent 0))
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@body)))
     (abort-recursive-edit)))

(defun ivy-exit-with-action (action &optional exit-code)
  "Quit the minibuffer and call ACTION afterwards."
  (ivy-set-action
   `(lambda (x)
      (funcall ',action x)
      (ivy-set-action ',(ivy-state-action ivy-last))))
  (setq ivy-exit (or exit-code 'done))
  (exit-minibuffer))

(defmacro with-ivy-window (&rest body)
  "Execute BODY in the window from which `ivy-read' was called."
  (declare (indent 0)
           (debug t))
  `(with-selected-window (ivy--get-window ivy-last)
     ,@body))

(defun ivy--expand-file-name (text)
  (cond
    ((eq (ivy-state-history ivy-last) 'grep-files-history)
     text)
    (ivy--directory
     (if (and (string-match-p "^/" text) (file-remote-p ivy--directory))
         (let ((parts (split-string ivy--directory ":")))
           (concat (nth 0 parts) ":" (nth 1 parts) ":" text))
       (expand-file-name text ivy--directory)))
    (t
     text)))

(defun ivy--done (text)
  "Insert TEXT and exit minibuffer."
  (if (member (ivy-state-prompt ivy-last) '("Create directory: " "Make directory: "))
      (ivy-immediate-done)
    (when (stringp text)
      (insert
       (setf (ivy-state-current ivy-last)
             (ivy--expand-file-name text))))
    (setq ivy-exit 'done)
    (exit-minibuffer)))

(defcustom ivy-use-selectable-prompt nil
  "When non-nil, make the prompt line selectable like a candidate.

The prompt line can be selected by calling `ivy-previous-line' when the first
regular candidate is selected.  Both actions `ivy-done' and `ivy-alt-done',
when called on a selected prompt, are forwarded to `ivy-immediate-done', which
results to the same as calling `ivy-immediate-done' explicitly when a regular
candidate is selected.

Note that if `ivy-wrap' is set to t, calling `ivy-previous-line' when the
prompt is selected wraps around to the last candidate, while calling
`ivy-next-line' on the last candidate wraps around to the first
candidate, not the prompt."
  :type 'boolean)

(defvar ivy--use-selectable-prompt nil
  "Store the effective `ivy-use-selectable-prompt' for current session.")

(defun ivy--prompt-selectable-p ()
  "Return t if the prompt line is selectable."
  (and ivy-use-selectable-prompt
       (or (memq (ivy-state-require-match ivy-last)
                 '(nil confirm confirm-after-completion))
           ;; :require-match is t, but "" is in the collection
           (let ((coll (ivy-state-collection ivy-last)))
             (and (listp coll)
                  (if (consp (car coll))
                      (member '("") coll)
                    (member "" coll)))))))

(defun ivy--prompt-selected-p ()
  "Return t if the prompt line is selected."
  (and ivy--use-selectable-prompt
       (= ivy--index -1)))

;;* Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (if (ivy--prompt-selected-p)
      (ivy-immediate-done)
    (setq ivy-current-prefix-arg current-prefix-arg)
    (delete-minibuffer-contents)
    (cond ((and (= ivy--length 0)
                (eq this-command 'ivy-dispatching-done))
           (ivy--done ivy-text))
          ((or (> ivy--length 0)
               ;; the action from `ivy-dispatching-done' may not need a
               ;; candidate at all
               (eq this-command 'ivy-dispatching-done))
           (ivy--done (ivy-state-current ivy-last)))
          ((and (memq (ivy-state-collection ivy-last)
                      '(read-file-name-internal internal-complete-buffer))
                (eq confirm-nonexistent-file-or-buffer t)
                (not (string= " (confirm)" ivy--prompt-extra)))
           (setq ivy--prompt-extra " (confirm)")
           (insert ivy-text)
           (ivy--exhibit))
          ((memq (ivy-state-require-match ivy-last)
                 '(nil confirm confirm-after-completion))
           (ivy--done ivy-text))
          (t
           (setq ivy--prompt-extra " (match required)")
           (insert ivy-text)
           (ivy--exhibit)))))

(defvar ivy-mouse-1-tooltip
  "Exit the minibuffer with the selected candidate."
  "The doc visible in the tooltip for mouse-1 binding in the minibuffer.")
(defvar ivy-mouse-3-tooltip
  "Display alternative actions."
  "The doc visible in the tooltip for mouse-3 binding in the minibuffer.")

(defun ivy-mouse-offset (event)
  "Compute the offset between the candidate at point and the selected one."
  (if event
      (let* ((line-number-at-point
              (max 2
                   (line-number-at-pos (posn-point (event-start event)))))

             (line-number-candidate ;; convert to 0 based index
              (- line-number-at-point 2))
             (offset
              (- line-number-candidate
                 ivy--window-index)))
        offset)
    nil))

(defun ivy-mouse-done (event)
  (interactive "@e")
  (let ((offset (ivy-mouse-offset event)))
    (when offset
      (ivy-next-line offset)
      (ivy--exhibit)
      (ivy-alt-done))))

(defun ivy-mouse-dispatching-done (event)
  (interactive "@e")
  (let ((offset (ivy-mouse-offset event)))
    (when offset
      (ivy-next-line offset)
      (ivy--exhibit)
      (ivy-dispatching-done))))

(defcustom ivy-read-action-format-function 'ivy-read-action-format-default
  "Function used to transform the actions list into a docstring."
  :type '(radio
          (function-item ivy-read-action-format-default)
          (function-item ivy-read-action-format-columns)))

(defun ivy-read-action-format-default (actions)
  "Create a docstring from ACTIONS.

ACTIONS is a list.  Each list item is a list of 3 items:
key (a string), cmd and doc (a string)."
  (format "%s\n%s\n"
          (if (eq this-command 'ivy-read-action)
              "Select action: "
            (ivy-state-current ivy-last))
          (mapconcat
           (lambda (x)
             (format "%s: %s"
                     (propertize
                      (car x)
                      'face 'ivy-action)
                     (nth 2 x)))
           actions
           "\n")))

(defun ivy-read-action-format-columns (actions)
  "Create a docstring from ACTIONS, using several columns if needed to preserve `ivy-height'.

ACTIONS is a list.  Each list item is a list of 3 items: key (a
string), cmd and doc (a string)."
  (let ((length (length actions))
        (i 0)
        (max-rows (- ivy-height 1))
        rows cols col lwidth rwidth)
    (while (< i length)
      (setq col (cl-subseq actions i (min length (cl-incf i max-rows))))
      (setq lwidth (apply 'max (mapcar (lambda (x)
                                         (length (nth 0 x)))
                                       col)))
      (setq rwidth (apply 'max (mapcar (lambda (x)
                                         (length (nth 2 x)))
                                       col)))
      (setq col (mapcar (lambda (x)
                          (format (format "%%%ds: %%-%ds" lwidth rwidth)
                                  (propertize (car x) 'face 'ivy-action)
                                  (nth 2 x)))
                        col))
      (cond
        ((null rows)
         (setq rows (length col)))
        ((< (length col) rows)
         (setq col (append col (make-list (- rows (length col)) "")))))
      (push col cols))
    (format "%s\n%s\n"
            (if (eq this-command 'ivy-read-action)
                "Select action: "
              (ivy-state-current ivy-last))
            (mapconcat 'identity
                       (apply 'cl-mapcar
                              (lambda (&rest args)
                                (mapconcat 'identity args " | "))
                              (nreverse cols))
                       "\n"))))

(defcustom ivy-read-action-function #'ivy-read-action-by-key
  "Function used to read an action."
  :type '(radio
          (function-item ivy-read-action-by-key)
          (function-item ivy-read-action-ivy)
          (function-item ivy-hydra-read-action)))

(defun ivy-read-action ()
  "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (if (not (ivy--actionp actions))
        t
      (let ((ivy--directory ivy--directory))
        (funcall ivy-read-action-function actions)))))

(defvar set-message-function)

(defun ivy-read-action-by-key (actions)
  (let* ((set-message-function nil)
         (hint (funcall ivy-read-action-format-function (cdr actions)))
         (resize-mini-windows t)
         (key "")
         action-idx)
    (while (and (setq action-idx (cl-position-if
                                  (lambda (x)
                                    (string-prefix-p key (car x)))
                                  (cdr actions)))
                (not (string= key (car (nth action-idx (cdr actions))))))
      (setq key (concat key (key-description (vector (read-key hint))))))
    (ivy-shrink-after-dispatching)
    (cond ((member key '("ESC" "C-g" "M-o"))
           nil)
          ((null action-idx)
           (message "%s is not bound" key)
           nil)
          (t
           (message "")
           (setcar actions (1+ action-idx))
           (ivy-set-action actions)))))

(defvar ivy-marked-candidates nil
  "List of marked candidates.
Use `ivy-mark' to populate this.

When this list is non-nil at the end of the session, the action
will be called for each element of this list.")

(defun ivy-read-action-ivy (actions)
  "Select an action from ACTIONS using Ivy."
  (let ((enable-recursive-minibuffers t))
    (if (and (> (minibuffer-depth) 1)
             (eq (ivy-state-caller ivy-last) 'ivy-read-action-ivy))
        (minibuffer-keyboard-quit)
      (let ((ivy-marked-candidates ivy-marked-candidates))
        (ivy-read (format "action (%s): " (ivy-state-current ivy-last))
                  (cl-mapcar
                   (lambda (a i) (cons (format "[%s] %s" (nth 0 a) (nth 2 a)) i))
                   (cdr actions) (number-sequence 1 (length (cdr actions))))
                  :action (lambda (a)
                            (setcar actions (cdr a))
                            (ivy-set-action actions))
                  :caller 'ivy-read-action-ivy)))))

(defun ivy-shrink-after-dispatching ()
  "Shrink the window after dispatching when action list is too large."
  (when (window-minibuffer-p)
    (window-resize nil (- ivy-height (window-height)))))

(defun ivy-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let ((ivy-exit 'ivy-dispatching-done))
    (when (ivy-read-action)
      (ivy-done)))
  (ivy-shrink-after-dispatching))

(defun ivy-dispatching-call ()
  "Select one of the available actions and call `ivy-call'."
  (interactive)
  (setq ivy-current-prefix-arg current-prefix-arg)
  (let ((actions (copy-sequence (ivy-state-action ivy-last)))
        (old-ivy-text ivy-text))
    (unwind-protect
         (when (ivy-read-action)
           (ivy-set-text old-ivy-text)
           (ivy-call))
      (ivy-set-action actions)))
  (ivy-shrink-after-dispatching))

(defun ivy-build-tramp-name (x)
  "Reconstruct X into a path.
Is is a cons cell, related to `tramp-get-completion-function'."
  (let ((user (car x))
        (domain (cadr x)))
    (if user
        (concat user "@" domain)
      domain)))

(declare-function Info-find-node "info")
(declare-function Info-read-node-name-1 "info")
(declare-function tramp-get-completion-function "tramp")

(defcustom ivy-alt-done-functions-alist nil
  "Customize what `ivy-alt-done' does per-collection."
  :type '(alist :key-type symbol :value-type function))

(defun ivy--completing-fname-p ()
  (eq 'file (cdr (assoc
                  'category
                  (ignore-errors
                    (funcall (ivy-state-collection ivy-last) ivy-text nil 'metadata))))))

(defun ivy-alt-done (&optional arg)
  "Exit the minibuffer with the selected candidate.
When ARG is t, exit with current text, ignoring the candidates.
When the current candidate during file name completion is a
directory, continue completion from within that directory instead
of exiting.  This function is otherwise like `ivy-done'."
  (interactive "P")
  (setq ivy-current-prefix-arg current-prefix-arg)
  (let (alt-done-fn)
    (cond ((or arg (ivy--prompt-selected-p))
           (ivy-immediate-done))
          ((setq alt-done-fn (ivy-alist-setting ivy-alt-done-functions-alist))
           (funcall alt-done-fn))
          ((ivy--completing-fname-p)
           (ivy--directory-done))
          (t
           (ivy-done)))))

(defun ivy--info-alt-done ()
  (if (member (ivy-state-current ivy-last) '("(./)" "(../)"))
      (ivy-quit-and-run
        (ivy-read "Go to file: " #'read-file-name-internal
                  :action (lambda (x)
                            (Info-find-node
                             (expand-file-name x ivy--directory)
                             "Top"))))
    (ivy-done)))

(defvar ivy-auto-select-single-candidate nil
  "When non-nil, auto-select the candidate if it is the only one.
When t, it is the same as if the user were prompted and selected the candidate
by calling the default action.  This variable has no use unless the collection
contains a single candidate.")

(defun ivy--directory-enter ()
  (let (dir)
    (when (and
           (> ivy--length 0)
           (not (string= (ivy-state-current ivy-last) "./"))
           (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
      (ivy--cd dir)
      (ivy--exhibit))))

(defun ivy--handle-directory (input)
  "Detect the next directory based on special values of INPUT."
  (cond ((string= input "/")
         "/")
        ((string= input "/sudo::")
         (concat input ivy--directory))))

(defun ivy--tramp-candidates ()
  (let ((method (match-string 1 ivy-text))
        (user (match-string 2 ivy-text))
        (rest (match-string 3 ivy-text))
        res)
    (dolist (x (tramp-get-completion-function method))
      (setq res (append res (funcall (car x) (cadr x)))))
    (setq res (delq nil res))
    (when user
      (dolist (x res)
        (setcar x user)))
    (setq res (delete-dups res))
    (let* ((old-ivy-last ivy-last)
           (enable-recursive-minibuffers t)
           (host (let ((ivy-auto-select-single-candidate nil))
                   (ivy-read "user@host: "
                             (mapcar #'ivy-build-tramp-name res)
                             :initial-input rest))))
      (setq ivy-last old-ivy-last)
      (when host
        (setq ivy--directory "/")
        (ivy--cd (concat "/" method ":" host ":/"))))))

(defun ivy--directory-done ()
  "Handle exit from the minibuffer when completing file names."
  (let ((dir (ivy--handle-directory ivy-text)))
    (cond ((equal (ivy-state-current ivy-last) (ivy-state-def ivy-last))
           (ivy-done))
          ((and (ivy-state-require-match ivy-last)
                (equal ivy-text "")
                (null ivy--old-cands))
           (ivy-immediate-done))
          (dir
           (let ((inhibit-message t))
             (ivy--cd dir)))
          ((ivy--directory-enter))
          ((unless (string= ivy-text "")
             ;; Obsolete since 26.1 and removed in 28.1.
             (defvar tramp-completion-mode)
             (with-no-warnings
               (let* ((tramp-completion-mode t)
                      (file (expand-file-name
                             (if (> ivy--length 0) (ivy-state-current ivy-last) ivy-text)
                             ivy--directory)))
                 (when (ignore-errors (file-exists-p file))
                   (if (file-directory-p file)
                       (ivy--cd (file-name-as-directory file))
                     (ivy-done))
                   ivy-text)))))
          ((or (and (equal ivy--directory "/")
                    (string-match-p "\\`[^/]+:.*:.*\\'" ivy-text))
               (string-match-p "\\`/[^/]+:.*:.*\\'" ivy-text))
           (ivy-done))
          ((ivy--tramp-prefix-p)
           (ivy--tramp-candidates))
          (t
           (ivy-done)))))

(defun ivy--tramp-prefix-p ()
  (or (and (equal ivy--directory "/")
           (cond ((string-match
                   "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                   ivy-text)
                  (save-match-data
                    (ivy-set-text (ivy-state-current ivy-last))))
                 ((string-match
                   "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                   (ivy-state-current ivy-last))
                  (save-match-data
                    (ivy-set-text (ivy-state-current ivy-last))))))
      (string-match
       "\\`/\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
       ivy-text)))

(defun ivy-expand-file-if-directory (file-name)
  "Expand FILE-NAME as directory.
When this directory doesn't exist, return nil."
  (when (stringp file-name)
    (let ((full-name
           ;; Ignore host name must not match method "ssh"
           (ignore-errors
             (file-name-as-directory
              (expand-file-name file-name ivy--directory)))))
      (when (and full-name (file-directory-p full-name))
        full-name))))

(defcustom ivy-tab-space nil
  "When non-nil, `ivy-partial-or-done' should insert a space."
  :type 'boolean)

(defun ivy-partial-or-done ()
  "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
  (interactive)
  (cond
    ((and (numberp completion-cycle-threshold)
          (< (length ivy--all-candidates) completion-cycle-threshold))
     (let ((ivy-wrap t))
       (ivy-next-line)))
    ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
          (or (and (equal ivy--directory "/")
                   (string-match-p "\\`[^/]+:.*\\'" ivy-text))
              (= (string-to-char ivy-text) ?/)))
     (let ((default-directory ivy--directory)
           dir)
       (minibuffer-complete)
       (ivy-set-text (ivy--input))
       (when (setq dir (ivy-expand-file-if-directory ivy-text))
         (ivy--cd dir))))
    (t
     (or (ivy-partial)
         (when (or (eq this-command last-command)
                   (eq ivy--length 1))
           (ivy-alt-done))))))

(defun ivy--partial-cd-for-single-directory ()
  (when (and
         (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
         (= 1 (length
               (ivy--re-filter
                (funcall ivy--regex-function
                         (concat "^" (string-remove-prefix "^" ivy-text)))
                ivy--all-candidates)))
         (let ((default-directory ivy--directory))
           (file-directory-p (ivy-state-current ivy-last))))
    (ivy--directory-done)))

(defun ivy-partial ()
  "Complete the minibuffer text as much as possible."
  (interactive)
  (let* ((parts (or (ivy--split-spaces ivy-text) (list "")))
         (tail (last parts))
         (postfix (car tail))
         (case-fold-search (ivy--case-fold-p ivy-text))
         (completion-ignore-case case-fold-search)
         (new (try-completion (string-remove-prefix "^" postfix)
                              (if (ivy-state-dynamic-collection ivy-last)
                                  ivy--all-candidates
                                (mapcar (lambda (str)
                                          (let ((i (string-match-p postfix str)))
                                            (and i (substring str i))))
                                        ivy--old-cands)))))
    (cond ((eq new t) nil)
          ((string= new ivy-text) nil)
          ((string= (car tail) (car (ivy--split-spaces new))) nil)
          (new
           (delete-region (minibuffer-prompt-end) (point-max))
           (setcar tail
                   (if (= (string-to-char postfix) ?^)
                       (concat "^" new)
                     new))
           (ivy-set-text
            (concat
             (mapconcat #'identity parts " ")
             (and ivy-tab-space (not (= (length ivy--old-cands) 1)) " ")))
           (insert ivy-text)
           (ivy--partial-cd-for-single-directory)
           t))))

(defvar ivy-completion-beg nil
  "Completion bounds start.")

(defvar ivy-completion-end nil
  "Completion bounds end.")

(defun ivy-immediate-done ()
  "Exit the minibuffer with current input instead of current candidate."
  (interactive)
  (delete-minibuffer-contents)
  (setf (ivy-state-current ivy-last)
        (cond ((or (not ivy--directory)
                   (eq (ivy-state-history ivy-last) 'grep-files-history))
               ivy-text)
              ((and (string= ivy-text "")
                    (eq (ivy-state-collection ivy-last)
                        #'read-file-name-internal))
               (if (ivy-state-def ivy-last)
                   (if (and
                        (file-exists-p (ivy-state-def ivy-last))
                        (/= (length ivy--directory)
                            (1+ (length (expand-file-name (ivy-state-def ivy-last))))))
                       ivy--directory
                     (copy-sequence (ivy-state-def ivy-last)))
                 ivy--directory))
              (t
               (expand-file-name ivy-text ivy--directory))))
  (insert (ivy-state-current ivy-last))
  (setq ivy-completion-beg ivy-completion-end)
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defun ivy--restore-session (&optional session)
  "Resume a recorded completion SESSION, if any exists."
  (when ivy--sessions
    (unless session
      (setq session (intern
                     (let ((ivy-last ivy-last)
                           ivy--all-candidates
                           ivy-text)
                       (ivy-read "Choose ivy session: "
                                 ivy--sessions
                                 :require-match t)))))
    (setq ivy-last (or (cdr (assq session ivy--sessions))
                       ivy-last)))
  (let ((data (plist-get (ivy-state-extra-props ivy-last) :ivy-data)))
    (when data
      (setq ivy--all-candidates (plist-get data :all-candidates))
      (setq ivy-text (plist-get data :text)))))

;;;###autoload
(defun ivy-resume (&optional session)
  "Resume the last completion session, or SESSION if non-nil.
With a prefix arg, try to restore a recorded completion session,
if one exists."
  (interactive)
  (when (or current-prefix-arg session)
    (ivy--restore-session session))

  (if (or (null (ivy-state-action ivy-last))
          (eq (ivy--get-action ivy-last) #'identity))
      (user-error "The last session isn't compatible with `ivy-resume'")
    (when (memq (ivy-state-caller ivy-last)
                '(swiper
                  swiper-isearch swiper-backward
                  swiper-isearch-backward
                  counsel-grep))
      (switch-to-buffer (ivy-state-buffer ivy-last)))
    (with-current-buffer (ivy-state-buffer ivy-last)
      (let ((default-directory (ivy-state-directory ivy-last))
            (ivy-use-ignore-default (ivy-state-ignore ivy-last)))
        (ivy-read
         (ivy-state-prompt ivy-last)
         (ivy-state-collection ivy-last)
         :predicate (ivy-state-predicate ivy-last)
         :require-match (ivy-state-require-match ivy-last)
         :initial-input ivy-text
         :history (ivy-state-history ivy-last)
         :preselect (ivy-state-current ivy-last)
         :keymap (ivy-state-keymap ivy-last)
         :update-fn (ivy-state-update-fn ivy-last)
         :sort (ivy-state-sort ivy-last)
         :action (ivy-state-action ivy-last)
         :unwind (ivy-state-unwind ivy-last)
         :re-builder (ivy-state-re-builder ivy-last)
         :matcher (ivy-state-matcher ivy-last)
         :dynamic-collection (ivy-state-dynamic-collection ivy-last)
         :extra-props (ivy-state-extra-props ivy-last)
         :caller (ivy-state-caller ivy-last))))))

(defvar-local ivy-calling nil
  "When non-nil, call the current action when `ivy--index' changes.")

(defun ivy-set-index (index)
  "Set `ivy--index' to INDEX."
  (setq ivy--index index)
  (when ivy-calling
    (ivy--exhibit)
    (ivy-call)))

(defun ivy-beginning-of-buffer ()
  "Select the first completion candidate."
  (interactive)
  (ivy-set-index 0))

(defun ivy-end-of-buffer ()
  "Select the last completion candidate."
  (interactive)
  (ivy-set-index (1- ivy--length)))

(defun ivy-scroll-up-command ()
  "Scroll the candidates upward by the minibuffer height."
  (interactive)
  (ivy-set-index (min (1- (+ ivy--index ivy-height))
                      (1- ivy--length))))

(defun ivy-scroll-down-command ()
  "Scroll the candidates downward by the minibuffer height."
  (interactive)
  (ivy-set-index (max (1+ (- ivy--index ivy-height))
                      0)))

(defun ivy-next-line (&optional arg)
  "Move cursor vertically down ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (+ ivy--index arg)))
    (if (> index (1- ivy--length))
        (if ivy-wrap
            (ivy-beginning-of-buffer)
          (ivy-set-index (1- ivy--length)))
      (ivy-set-index index))))

(defun ivy-next-line-or-history (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (let ((orig-index ivy--index))
    (ivy-next-line arg)
    (when (and (string= ivy-text "") (= ivy--index orig-index))
      (ivy-previous-history-element 1))))

(defun ivy-previous-line (&optional arg)
  "Move cursor vertically up ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (- ivy--index arg))
        (min-index (if ivy--use-selectable-prompt -1 0)))
    (if (< index min-index)
        (if ivy-wrap
            (ivy-end-of-buffer)
          (ivy-set-index min-index))
      (ivy-set-index index))))

(defun ivy-previous-line-or-history (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (let ((orig-index ivy--index))
    (ivy-previous-line arg)
    (when (and (string= ivy-text "") (= ivy--index orig-index))
      (ivy-previous-history-element 1))))

(defun ivy-toggle-calling ()
  "Flip `ivy-calling'."
  (interactive)
  (when (setq ivy-calling (not ivy-calling))
    (ivy-call)))

(defun ivy-toggle-ignore ()
  "Toggle user-configured candidate filtering."
  (interactive)
  (setq ivy-use-ignore
        (if ivy-use-ignore
            nil
          (or ivy-use-ignore-default t)))
  (setf (ivy-state-ignore ivy-last) ivy-use-ignore)
  ;; invalidate cache
  (setq ivy--old-cands nil))

(defun ivy--get-action (state)
  "Get the action function from STATE."
  (let ((action (ivy-state-action state)))
    (when action
      (if (functionp action)
          action
        (cadr (nth (car action) action))))))

(defun ivy--get-multi-action (state)
  "Get the multi-action function from STATE."
  (let* ((action (ivy-state-action state))
         (multi-action
          (and (listp action)
               (not (eq (car action) 'lambda))
               (nth 3 (nth (car action) action)))))
    (if multi-action
        multi-action
      (when (eq (car action) 1)
        (ivy-state-multi-action state)))))

(defun ivy--get-window (state)
  "Get the window from STATE."
  (if (ivy-state-p state)
      (let ((window (ivy-state-window state)))
        (if (window-live-p window)
            window
          (next-window)))
    (selected-window)))

(defun ivy--actionp (x)
  "Return non-nil when X is a list of actions."
  (and (consp x) (not (memq (car x) '(closure lambda)))))

(defcustom ivy-action-wrap nil
  "When non-nil, `ivy-next-action' and `ivy-prev-action' wrap."
  :type 'boolean)

(defun ivy-next-action ()
  "When the current action is a list, scroll it forwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (let ((len (1- (length action)))
            (idx (car action)))
        (if (>= idx len)
            (when ivy-action-wrap
              (setf (car action) 1))
          (cl-incf (car action)))))))

(defun ivy-prev-action ()
  "When the current action is a list, scroll it backwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (if (<= (car action) 1)
          (when ivy-action-wrap
            (setf (car action) (1- (length action))))
        (cl-decf (car action))))))

(defun ivy-action-name ()
  "Return the name associated with the current action."
  (let ((action (ivy-state-action ivy-last)))
    (if (ivy--actionp action)
        (format "[%d/%d] %s"
                (car action)
                (1- (length action))
                (nth 2 (nth (car action) action)))
      "[1/1] default")))

(defvar ivy-inhibit-action nil
  "When non-nil, `ivy-call' does nothing.

Example use:

    (let* ((ivy-inhibit-action t)
           (str (ivy-switch-buffer)))
     ;; do whatever with str - the corresponding buffer will not be opened
     )")

(defun ivy-recursive-restore ()
  "Restore the above state when exiting the minibuffer.
See variable `ivy-recursive-restore' for further information."
  (when (and ivy-recursive-last
             ivy-recursive-restore
             (not (eq ivy-last ivy-recursive-last)))
    (ivy--reset-state (setq ivy-last ivy-recursive-last))))

(defvar ivy-mark-prefix ">"
  "Prefix used by `ivy-mark'.")

(defun ivy--call-marked (action)
  (let* ((prefix-len (length ivy-mark-prefix))
         (marked-candidates
          (mapcar
           (lambda (s)
             (let ((cand (substring s prefix-len)))
               (if ivy--directory
                   (expand-file-name cand ivy--directory)
                 cand)))
           ivy-marked-candidates))
         (multi-action (ivy--get-multi-action ivy-last)))
    (if multi-action
        (let ((default-directory (ivy-state-directory ivy-last)))
          (funcall multi-action (mapcar #'ivy--call-cand marked-candidates)))
      (dolist (c marked-candidates)
        (let ((default-directory (ivy-state-directory ivy-last)))
          (funcall action (ivy--call-cand c)))))))

(defun ivy--call-cand (current)
  (let ((collection (ivy-state-collection ivy-last)))
    (cond
     ;; Alist type.
     ((and (consp (car-safe collection))
           ;; Previously, the cdr of the selected
           ;; candidate would be returned.  Now, the
           ;; whole candidate is returned.
           (let ((idx (get-text-property 0 'idx current)))
             (if idx
                 (progn
                   (ivy--remove-props current 'idx)
                   (nth idx collection))
               (assoc current collection)))))
     (ivy--directory
      (expand-file-name current ivy--directory))
     ((equal current "")
      ivy-text)
     (t
      current))))

(defun ivy-call ()
  "Call the current action without exiting completion."
  (interactive)
  ;; Testing with `ivy-with' seems to call `ivy-call' again,
  ;; in which case `this-command' is nil; so check for this.
  (unless (memq this-command '(nil
                               ivy-done
                               ivy-alt-done
                               ivy-dispatching-done))
    (setq ivy-current-prefix-arg current-prefix-arg))
  (let* ((action
          (if (functionp ivy-inhibit-action)
              ivy-inhibit-action
            (and (not ivy-inhibit-action)
                 (ivy--get-action ivy-last))))
         (current (ivy-state-current ivy-last))
         (x (ivy--call-cand current))
         (res
          (cond
           ((null action)
            current)
           (t
            (select-window (ivy--get-window ivy-last))
            (set-buffer (ivy-state-buffer ivy-last))
            (prog1 (unwind-protect
                       (if ivy-marked-candidates
                           (ivy--call-marked action)
                         (funcall action x))
                     (ivy-recursive-restore))
              (unless (or (eq ivy-exit 'done)
                          (minibuffer-window-active-p (selected-window))
                          (null (active-minibuffer-window)))
                (select-window (active-minibuffer-window))))))))
    (if ivy-inhibit-action
        res
      current)))

(defun ivy-call-and-recenter ()
  "Call action and recenter window according to the selected candidate."
  (interactive)
  (ivy-call)
  (with-ivy-window
    (recenter-top-bottom)))

(defun ivy-next-line-and-call (&optional arg)
  "Move cursor vertically down ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-next-line arg)
  (ivy--exhibit)
  (ivy-call))

(defun ivy-previous-line-and-call (&optional arg)
  "Move cursor vertically up ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-previous-line arg)
  (ivy--exhibit)
  (ivy-call))

(defun ivy-previous-history-element (arg)
  "Forward to `previous-history-element' with ARG."
  (interactive "p")
  (previous-history-element arg)
  (ivy--cd-maybe)
  (move-end-of-line 1)
  (ivy--maybe-scroll-history))

(defun ivy--insert-symbol-boundaries ()
  (undo-boundary)
  (beginning-of-line)
  (insert "\\_<")
  (end-of-line)
  (insert "\\_>"))

(defun ivy-next-history-element (arg)
  "Forward to `next-history-element' with ARG."
  (interactive "p")
  (if (and (= minibuffer-history-position 0)
           (equal ivy-text ""))
      (progn
        (when minibuffer-default
          (setq ivy--default (car minibuffer-default)))
        (insert ivy--default)
        (when (and (with-ivy-window (derived-mode-p 'prog-mode))
                   (eq (ivy-state-caller ivy-last) 'swiper)
                   (not (file-exists-p ivy--default))
                   (not (ivy-ffap-url-p ivy--default))
                   (not (ivy-state-dynamic-collection ivy-last))
                   (> (point) (minibuffer-prompt-end)))
          (ivy--insert-symbol-boundaries)))
    (next-history-element arg))
  (ivy--cd-maybe)
  (move-end-of-line 1)
  (ivy--maybe-scroll-history))

(defvar ivy-ffap-url-functions nil
  "List of functions that check if the point is on a URL.")

(defun ivy--cd-maybe ()
  "Check if the current input points to a different directory.
If so, move to that directory, while keeping only the file name."
  (when ivy--directory
    (let ((input (ivy--input))
          url)
      (if (setq url (or (ivy-ffap-url-p input)
                        (with-ivy-window
                          (cl-reduce
                           (lambda (a b)
                             (or a (funcall b)))
                           ivy-ffap-url-functions
                           :initial-value nil))))
          (ivy-exit-with-action
           (lambda (_)
             (ivy-ffap-url-fetcher url))
           'no-update-history)
        (setq input (expand-file-name input))
        (let ((file (file-name-nondirectory input))
              (dir (expand-file-name (file-name-directory input))))
          (if (string= dir ivy--directory)
              (progn
                (delete-minibuffer-contents)
                (insert file))
            (ivy--cd dir)
            (insert file)))))))

(defun ivy--maybe-scroll-history ()
  "If the selected history element has an index, scroll there."
  (let ((idx (ignore-errors
               (get-text-property
                (minibuffer-prompt-end)
                'ivy-index))))
    (when idx
      (ivy--exhibit)
      (ivy-set-index idx))))

(declare-function tramp-get-completion-methods "tramp")

(defun ivy--cd (dir)
  "When completing file names, move to directory DIR."
  (if (ivy--completing-fname-p)
      (progn
        (push dir ivy--directory-hist)
        (setq ivy--old-cands nil)
        (setq ivy--old-re nil)
        (ivy-set-index 0)
        (setq ivy--all-candidates
              (append
               (ivy--sorted-files (setq ivy--directory dir))
               (when (and (string= dir "/") (featurep 'tramp))
                 (sort
                  (mapcar
                   (lambda (s) (substring s 1))
                   (tramp-get-completion-methods ""))
                  #'string<))))
        (ivy-set-text "")
        (setf (ivy-state-directory ivy-last) dir)
        (delete-minibuffer-contents))
    (error "Unexpected")))

(defun ivy--parent-dir (filename)
  "Return parent directory of absolute FILENAME."
  (file-name-directory (directory-file-name filename)))

(defun ivy-backward-delete-char ()
  "Forward to `delete-backward-char'.
Call `ivy-on-del-error-function' if an error occurs, usually when
there is no more text to delete at the beginning of the
minibuffer."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (ivy--parent-dir (expand-file-name ivy--directory)))
        (ivy--exhibit))
    (setq prefix-arg current-prefix-arg)
    (condition-case nil
        (call-interactively #'delete-backward-char)
      (error
       (when ivy-on-del-error-function
         (funcall ivy-on-del-error-function))))))

(defun ivy-delete-char (arg)
  "Forward to `delete-char' ARG."
  (interactive "p")
  (unless (eolp)
    (delete-char arg)))

(defun ivy-forward-char (arg)
  "Forward to `forward-char' ARG."
  (interactive "p")
  (unless (eolp)
    (forward-char arg)))

(defun ivy-kill-word (arg)
  "Forward to `kill-word' ARG."
  (interactive "p")
  (unless (eolp)
    (kill-word arg)))

(defun ivy-kill-line ()
  "Forward to `kill-line'."
  (interactive)
  (if (eolp)
      (kill-region (minibuffer-prompt-end) (point))
    (kill-line)))

(defun ivy-kill-whole-line ()
  "Forward to `kill-whole-line'."
  (interactive)
  (kill-region (minibuffer-prompt-end) (line-end-position)))

(defun ivy-backward-kill-word ()
  "Forward to `backward-kill-word'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (ivy--parent-dir (expand-file-name ivy--directory)))
        (ivy--exhibit))
    (ignore-errors
      (let ((pt (point))
            (last-command (if (eq last-command 'ivy-backward-kill-word)
                              'kill-region
                            last-command)))
        (forward-word -1)
        (kill-region pt (point))))))

(defvar ivy--regexp-quote #'regexp-quote
  "Store the regexp quoting state.")

(defun ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function ivy--regexp-quote)
  (setq ivy--old-text "")
  (setq ivy-regex (funcall ivy--regex-function ivy-text)))

(defcustom ivy-format-functions-alist
  '((t . ivy-format-function-default))
  "An alist of functions that transform the list of candidates into a string.
This string is inserted into the minibuffer."
  :type '(alist
          :key-type symbol
          :value-type
          (choice
           (const :tag "Default" ivy-format-function-default)
           (const :tag "Arrow prefix" ivy-format-function-arrow)
           (const :tag "Full line" ivy-format-function-line)
           (function :tag "Custom function"))))

(defun ivy-sort-file-function-default (x y)
  "Compare two files X and Y.
Prioritize directories."
  (if (get-text-property 0 'dirp x)
      (if (get-text-property 0 'dirp y)
          (string< (directory-file-name x) (directory-file-name y))
        t)
    (if (get-text-property 0 'dirp y)
        nil
      (string< x y))))

(defun ivy-string< (x y)
  "Like `string<', but operate on CARs when given cons cells."
  (string< (if (consp x) (car x) x)
           (if (consp y) (car y) y)))

(define-obsolete-function-alias 'ivy-sort-file-function-using-ido
    'ido-file-extension-lessp "<2019-10-12 Sat>")

(defcustom ivy-sort-functions-alist
  '((t . ivy-string<))
  "An alist of sorting functions for each collection function.
Interactive functions that call completion fit in here as well.

Nil means no sorting, which is useful to turn off the sorting for
functions that have candidates in the natural buffer order, like
`org-refile' or `Man-goto-section'.

A list can be used to associate multiple sorting functions with a
collection.  The car of the list is the current sort
function.  This list can be rotated with `ivy-rotate-sort'.

The entry associated with t is used for all fall-through cases.

See also `ivy-sort-max-size'."
  :type
  '(alist
    :key-type (choice
               (const :tag "Fall-through" t)
               (symbol :tag "Collection"))
    :value-type (choice
                 (const :tag "Plain sort" ivy-string<)
                 (const :tag "File sort" ivy-sort-file-function-default)
                 (const :tag "File sort using Ido" ido-file-extension-lessp)
                 (const :tag "No sort" nil)
                 (function :tag "Custom function")
                 (repeat (function :tag "Custom function")))))

(defun ivy--sort-function (collection)
  "Retrieve sort function for COLLECTION from `ivy-sort-functions-alist'."
  (let ((entry (cdr (or (assq collection ivy-sort-functions-alist)
                        (assq (ivy-state-caller ivy-last) ivy-sort-functions-alist)
                        (assq t ivy-sort-functions-alist)))))
    (and (or (functionp entry)
             (functionp (setq entry (car-safe entry))))
         entry)))

(defun ivy-rotate-sort ()
  "Rotate through sorting functions available for current collection.
This only has an effect if multiple sorting functions are
specified for the current collection in
`ivy-sort-functions-alist'."
  (interactive)
  (let ((cell (or (assq (ivy-state-collection ivy-last) ivy-sort-functions-alist)
                  (assq (ivy-state-caller ivy-last) ivy-sort-functions-alist))))
    (when (consp (cdr cell))
      (setcdr cell (nconc (cddr cell) (list (cadr cell))))
      (ivy--reset-state ivy-last))))

(defcustom ivy-index-functions-alist
  '((t . ivy-recompute-index-zero))
  "An alist of index recomputing functions for each collection function.
When the input changes, the appropriate function returns an
integer - the index of the matched candidate that should be
selected."
  :type '(alist :key-type symbol :value-type function))

(defvar ivy-re-builders-alist
  '((t . ivy--regex-plus))
  "An alist of regex building functions for each collection function.

Each key is (in order of priority):
1. The actual collection function, e.g. `read-file-name-internal'.
2. The symbol passed by :caller into `ivy-read'.
3. `this-command'.
4. t.

Each value is a function that should take a string and return a
valid regex or a regex sequence (see below).

Possible choices: `ivy--regex', `regexp-quote',
`ivy--regex-plus', `ivy--regex-fuzzy', `ivy--regex-ignore-order'.

If a function returns a list, it should format like this:
'((\"matching-regexp\" . t) (\"non-matching-regexp\") ...).

The matches will be filtered in a sequence, you can mix the
regexps that should match and that should not match as you
like.")

(defvar ivy-highlight-functions-alist
  '((ivy--regex-ignore-order . ivy--highlight-ignore-order)
    (ivy--regex-fuzzy . ivy--highlight-fuzzy)
    (ivy--regex-plus . ivy--highlight-default))
  "An alist of highlighting functions for each regex builder function.")

(defcustom ivy-initial-inputs-alist
  '((org-refile . "^")
    (org-agenda-refile . "^")
    (org-capture-refile . "^")
    (Man-completion-table . "^")
    (woman . "^"))
  "An alist associating commands with their initial input.

Each cdr is either a string or a function called in the context
of a call to `ivy-read'."
  :type '(alist
          :key-type (symbol)
          :value-type (choice (string) (function))))

(defcustom ivy-hooks-alist nil
  "An alist associating commands to setup functions.
Examples: `toggle-input-method', (lambda () (insert \"^\")), etc.
May supersede `ivy-initial-inputs-alist'."
  :type '(alist :key-type symbol :value-type function))

(defvar ivy--occurs-list nil
  "A list of custom occur generators per command.")

(defun ivy-set-occur (cmd occur)
  "Assign CMD a custom OCCUR function."
  (setq ivy--occurs-list
        (plist-put ivy--occurs-list cmd occur)))

(defcustom ivy-update-fns-alist nil
  "An alist associating commands to their :update-fn values."
  :type '(alist
          :key-type symbol
          :value-type
          (radio
           (const :tag "Off" nil)
           (const :tag "Call action on change" auto))))

(defcustom ivy-unwind-fns-alist nil
  "An alist associating commands to their :unwind values."
  :type '(alist :key-type symbol :value-type function))

(defcustom ivy-init-fns-alist nil
  "An alist associating commands to their :init values.
An :init is a function with no arguments.
`ivy-read' calls it to initialize."
  :type '(alist :key-type symbol :value-type function))

(defun ivy--alist-set (alist-sym key val)
  (let ((curr-val (symbol-value alist-sym))
        (customized-val (get alist-sym 'customized-value))
        (default-val (eval (car (get alist-sym 'standard-value)))))
    ;; when the value was set by `customize-set-variable', don't touch it
    (unless customized-val
      ;; only works if the value wasn't customized by the user
      (when (or (null default-val) (equal curr-val default-val))
        (let ((cell (assoc key curr-val)))
          (if cell
              (setcdr cell val)
            (set alist-sym (cons (cons key val)
                                 (symbol-value alist-sym)))))
        (when default-val
          (put alist-sym 'standard-value
               (list (list 'quote (symbol-value alist-sym)))))))))

(declare-function counsel-set-async-exit-code "counsel")

(defvar ivy--parents-alist nil
  "Configure parent caller for child caller.
The child caller inherits and can override the settings of the parent.")

(cl-defun ivy-configure (caller
                         &key
                         parent
                         initial-input
                         height
                         occur
                         update-fn
                         init-fn
                         unwind-fn
                         index-fn
                         sort-fn
                         format-fn
                         display-fn
                         display-transformer-fn
                         alt-done-fn
                         more-chars
                         grep-p
                         exit-codes)
  "Configure `ivy-read' params for CALLER."
  (declare (indent 1))
  (when parent
    (ivy--alist-set 'ivy--parents-alist caller parent))
  (when initial-input
    (ivy--alist-set 'ivy-initial-inputs-alist caller initial-input))
  (when height
    (ivy--alist-set 'ivy-height-alist caller height))
  (when occur
    (ivy-set-occur caller occur))
  (when update-fn
    (ivy--alist-set 'ivy-update-fns-alist caller update-fn))
  (when unwind-fn
    (ivy--alist-set 'ivy-unwind-fns-alist caller unwind-fn))
  (when init-fn
    (ivy--alist-set 'ivy-init-fns-alist caller init-fn))
  (when index-fn
    (ivy--alist-set 'ivy-index-functions-alist caller index-fn))
  (when sort-fn
    (ivy--alist-set 'ivy-sort-functions-alist caller sort-fn))
  (when format-fn
    (ivy--alist-set 'ivy-format-functions-alist caller format-fn))
  (when display-fn
    (ivy--alist-set 'ivy-display-functions-alist caller display-fn))
  (when display-transformer-fn
    (ivy--alist-set 'ivy--display-transformers-alist caller display-transformer-fn))
  (when alt-done-fn
    (ivy--alist-set 'ivy-alt-done-functions-alist caller alt-done-fn))
  (when more-chars
    (ivy--alist-set 'ivy-more-chars-alist caller more-chars))
  (when grep-p
    (cl-pushnew caller ivy-highlight-grep-commands))
  (when exit-codes
    (let (code msg)
      (while (and (setq code (pop exit-codes))
                  (setq msg (pop exit-codes)))
        (counsel-set-async-exit-code caller code msg)))))

(defcustom ivy-sort-max-size 30000
  "Sorting won't be done for collections larger than this."
  :type 'integer)

(defalias 'ivy--dirname-p
  ;; Added in Emacs 25.1.
  (if (fboundp 'directory-name-p)
      #'directory-name-p
    (lambda (name)
      "Return non-nil if NAME ends with a directory separator."
      (string-suffix-p "/" name))))

(defun ivy--sorted-files (dir)
  "Return the list of files in DIR.
Directories come first."
  (let* ((default-directory dir)
         (seq (condition-case nil
                  (mapcar (lambda (s) (replace-regexp-in-string "\\$\\$" "$" s))
                          (all-completions "" #'read-file-name-internal
                                           (ivy-state-predicate ivy-last)))
                (error
                 (directory-files dir))))
         sort-fn)
    (setq seq (delete "./" (delete "../" seq)))
    (when (eq (setq sort-fn (ivy--sort-function #'read-file-name-internal))
              #'ivy-sort-file-function-default)
      (setq seq (mapcar (lambda (x)
                          (propertize x 'dirp (ivy--dirname-p x)))
                        seq)))
    (when sort-fn
      (setq seq (sort seq sort-fn)))
    (dolist (dir ivy-extra-directories)
      (push dir seq))
    (if (string= dir "/")
        (cl-remove-if (lambda (s) (string-match ":$" s)) (delete "../" seq))
      seq)))

(defun ivy-alist-setting (alist &optional key)
  "Return the value associated with KEY in ALIST, using `assq'.
KEY defaults to the last caller of `ivy-read'; if no entry is
found, it falls back to the key t."
  (let ((caller (or key (ivy-state-caller ivy-last))))
    (or
     (and caller (cdr (assq caller alist)))
     (let ((parent (cdr (assq caller ivy--parents-alist))))
       (when parent
         (ivy-alist-setting alist parent)))
     (cdr (assq t alist)))))

(defun ivy--height (caller)
  (let ((v (or (ivy-alist-setting ivy-height-alist caller)
               ivy-height)))
    (if (integerp v)
        v
      (if (functionp v)
          (funcall v caller)
        (error "Unexpected value: %S" v)))))

(defun ivy--remove-props (str &rest props)
  "Return STR with text PROPS destructively removed."
  (ignore-errors
    (remove-list-of-text-properties 0 (length str) props str))
  str)

(defun ivy--update-prompt (prompt)
  (cond ((equal prompt "Keyword, C-h: ")
         ;; auto-insert.el
         "Keyword (C-M-j to end): ")
        (t
         ;; misearch.el
         (replace-regexp-in-string "RET to end" "C-M-j to end" prompt))))

;;** Entry Point
;;;###autoload
(cl-defun ivy-read (prompt collection
                    &key
                      predicate require-match initial-input
                      history preselect def keymap update-fn sort
                      action multi-action
                      unwind re-builder matcher
                      dynamic-collection
                      extra-props
                      caller)
  "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table, supplied for `minibuffer-completion-table'.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

PRESELECT, when non-nil, determines which one of the candidates
matching INITIAL-INPUT to select initially.  An integer stands
for the position of the desired candidate in the collection,
counting from zero.  Otherwise, use the first occurrence of
PRESELECT in the collection.  Comparison is first done with
`equal'.  If that fails, and when applicable, match PRESELECT as
a regular expression.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is re-displayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes one argument, the selected candidate. If COLLECTION is
an alist, the argument is a cons cell, otherwise it's a string.

MULTI-ACTION, when non-nil, is called instead of ACTION when
there are marked candidates. It takes the list of candidates as
its only argument. When it's nil, ACTION is called on each marked
candidate.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

EXTRA-PROPS is a plist that can be used to store
collection-specific session-specific data.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session."
  (let ((init-fn (ivy-alist-setting ivy-init-fns-alist caller)))
    (when init-fn
      (funcall init-fn)))
  ;; get un-stuck from an existing `read-key' overriding minibuffer keys
  (when (equal overriding-local-map '(keymap))
    (keyboard-quit))
  (setq caller (or caller this-command))
  (let* ((ivy-recursive-last (and (active-minibuffer-window) ivy-last))
         (ivy--display-function
          (when (or ivy-recursive-last
                    (not (window-minibuffer-p)))
            (ivy-alist-setting ivy-display-functions-alist caller))))
    (setq update-fn (or update-fn (ivy-alist-setting ivy-update-fns-alist caller)))
    (setq unwind (or unwind (ivy-alist-setting ivy-unwind-fns-alist caller)))
    (setq ivy-last
          (make-ivy-state
           :prompt (ivy--update-prompt prompt)
           :collection collection
           :predicate predicate
           :require-match require-match
           :initial-input initial-input
           :history history
           :preselect preselect
           :keymap keymap
           :update-fn (if (eq update-fn 'auto)
                          (lambda ()
                            (with-ivy-window
                              (funcall
                               (ivy--get-action ivy-last)
                               (if (consp (car-safe (ivy-state-collection ivy-last)))
                                   (assoc (ivy-state-current ivy-last)
                                          (ivy-state-collection ivy-last))
                                 (ivy-state-current ivy-last)))))
                        update-fn)
           :sort sort
           :action (ivy--compute-extra-actions action caller)
           :multi-action multi-action
           :frame (selected-frame)
           :window (selected-window)
           :buffer (current-buffer)
           :unwind unwind
           :re-builder re-builder
           :matcher matcher
           :dynamic-collection dynamic-collection
           :display-transformer-fn (ivy-alist-setting ivy--display-transformers-alist caller)
           :directory default-directory
           :extra-props extra-props
           :caller caller
           :def def))
    (ivy--reset-state ivy-last)
    (unwind-protect
         (minibuffer-with-setup-hook
             #'ivy--minibuffer-setup
           (let* ((hist (or history 'ivy-history))
                  (minibuffer-completion-table collection)
                  (minibuffer-completion-predicate predicate)
                  (ivy-height (ivy--height caller))
                  (resize-mini-windows (unless (display-graphic-p)
                                         'grow-only)))
             (if (and ivy-auto-select-single-candidate
                      ivy--all-candidates
                      (null (cdr ivy--all-candidates)))
                 (progn
                   (setf (ivy-state-current ivy-last)
                         (car ivy--all-candidates))
                   (setq ivy-exit 'done))
               (condition-case err
                   (read-from-minibuffer
                    prompt
                    (ivy-state-initial-input ivy-last)
                    (make-composed-keymap keymap ivy-minibuffer-map)
                    nil
                    hist)
                 (error
                  (unless (equal err '(error "Selecting deleted buffer"))
                    (signal (car err) (cdr err))))))
             (when (eq ivy-exit 'done)
               (ivy--update-history hist))))
      (let ((session (or (plist-get extra-props :session)
                         (unless (or (minibufferp)
                                     (null (ivy-state-action ivy-last))
                                     (eq (ivy--get-action ivy-last) #'identity))
                           caller))))
        (when session
          (setf (ivy-state-extra-props ivy-last)
                (plist-put extra-props :ivy-data `(:all-candidates ,ivy--all-candidates
                                                   :text ,ivy-text)))
          (ivy--alist-set 'ivy--sessions session ivy-last)))
      (ivy--cleanup))
    (ivy-call)))

(defun ivy--update-history (hist)
  (let ((item
         (if (or (string= ivy-text "")
                 (eq
                  (plist-get (ivy-state-extra-props ivy-last) :caller)
                  'ivy-completing-read)
                 (eq (ivy-state-history ivy-last) 'file-name-history))
             (ivy-state-current ivy-last)
           ivy-text)))
    (cond ((equal item ""))
          ((stringp item)
           (set hist (cons (propertize item 'ivy-index ivy--index)
                           (delete item (symbol-value hist))))))))

(defun ivy--cleanup ()
  ;; Fixes a bug in ESS, #1660
  (put 'post-command-hook 'permanent-local nil)
  (remove-hook 'post-command-hook #'ivy--queue-exhibit)
  (remove-hook 'window-size-change-functions #'ivy--window-size-changed)
  (let ((cleanup (ivy--display-function-prop :cleanup))
        (unwind (ivy-state-unwind ivy-last)))
    (when (functionp cleanup)
      (funcall cleanup))
    (when unwind
      (funcall unwind)))
  (ivy--pulse-cleanup)
  (unless (eq ivy-exit 'done)
    (ivy-recursive-restore)))

(defun ivy--display-function-prop (prop)
  "Return PROP associated with current `ivy--display-function'."
  (plist-get (cdr (assq ivy--display-function
                        ivy-display-functions-props))
             prop))

(defvar Info-complete-menu-buffer)

(defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
  (setq ivy-marked-candidates nil)
  (unless (equal (selected-frame) (ivy-state-frame state))
    (select-window (active-minibuffer-window)))
  (let* ((prompt (or (ivy-state-prompt state) ""))
         (collection (ivy-state-collection state))
         (predicate (ivy-state-predicate state))
         (history (ivy-state-history state))
         (preselect (ivy-state-preselect state))
         (re-builder (ivy-state-re-builder state))
         (dynamic-collection (ivy-state-dynamic-collection state))
         (require-match (ivy-state-require-match state))
         (caller (or (ivy-state-caller state) this-command))
         (sort (or (ivy-state-sort state) (assoc caller ivy-sort-functions-alist)))
         (initial-input
          (or (ivy-state-initial-input state)
              (let ((init (ivy-alist-setting ivy-initial-inputs-alist caller)))
                (if (functionp init) (funcall init) init))))
         (def (ivy-state-def state)))
    (when (and (eq caller 'swiper-isearch) (buffer-modified-p))
      (setq preselect nil))
    (setq ivy--extra-candidates (ivy--compute-extra-candidates caller))
    (setq ivy--directory nil)
    (setq ivy--directory-hist (list default-directory))
    (setq ivy-case-fold-search ivy-case-fold-search-default)
    (setf (ivy-state-re-builder ivy-last)
          (setq ivy--regex-function
                (or re-builder
                    (and (functionp collection)
                         (cdr (assq collection ivy-re-builders-alist)))
                    (ivy-alist-setting ivy-re-builders-alist)
                    #'ivy--regex)))
    (setq ivy--subexps 0)
    (setq ivy--regexp-quote #'regexp-quote)
    (setq ivy--old-text "")
    (setq ivy--full-length nil)
    (ivy-set-text (or initial-input ""))
    (setq ivy--index 0)
    (setq ivy-calling nil)
    (setq ivy-use-ignore ivy-use-ignore-default)
    (setf (ivy-state-ignore state) ivy-use-ignore)
    (setq ivy--highlight-function
          (or (cdr (assq (ivy-alist-setting ivy-re-builders-alist)
                         ivy-highlight-functions-alist))
              #'ivy--highlight-default))
    (let ((ivy-recursive-restore nil)
          coll sort-fn)
      (cond ((eq collection #'Info-read-node-name-1)
             (setq coll
                   (if (equal (bound-and-true-p Info-current-file) "dir")
                       (mapcar (lambda (x) (format "(%s)" x))
                               (delete-dups
                                (all-completions "(" collection predicate)))
                     (all-completions "" collection predicate))))
            ((memq collection '(read-file-name-internal ffap-read-file-or-url-internal))
             (require 'tramp)
             (when (and (equal def initial-input)
                        (member "./" ivy-extra-directories))
               (setq def nil))
             (setq ivy--directory default-directory)
             (when (and initial-input
                        (not (equal initial-input "")))
               (cond ((file-directory-p initial-input)
                      (when (equal (file-name-nondirectory initial-input) "")
                        (setf (ivy-state-preselect state) (setq preselect nil))
                        (setq def nil))
                      (setq ivy--directory (file-name-as-directory initial-input))
                      (setq initial-input nil)
                      (when preselect
                        (let ((preselect-directory
                               (file-name-directory preselect)))
                          (when (and preselect-directory
                                     (not (equal
                                           (expand-file-name
                                            preselect-directory)
                                           (expand-file-name ivy--directory))))
                            (setf (ivy-state-preselect state)
                                  (setq preselect nil))))))
                     ((ignore-errors
                        (file-exists-p (file-name-directory initial-input)))
                      (setq ivy--directory (file-name-directory initial-input))
                      (setf (ivy-state-preselect state)
                            (file-name-nondirectory initial-input)))))
             (require 'dired)
             (when preselect
               (let ((preselect-directory (ivy--parent-dir preselect)))
                 (when (and preselect-directory
                            (not (string= preselect-directory
                                          default-directory)))
                   (setq ivy--directory preselect-directory))
                 (setq preselect (file-relative-name preselect
                                                     preselect-directory))
                 (setf (ivy-state-preselect state) preselect)))
             (setq sort nil)
             (setq coll (ivy--sorted-files ivy--directory))
             (when initial-input
               (unless (or require-match
                           (equal initial-input default-directory)
                           (equal initial-input ""))
                 (setq coll (cons initial-input coll)))
               (setq initial-input (file-name-nondirectory initial-input))))
            ((eq collection #'internal-complete-buffer)
             (setq coll (ivy--buffer-list
                         ""
                         (and ivy-use-virtual-buffers
                              (member caller '(ivy-switch-buffer
                                               ivy-switch-buffer-other-window
                                               counsel-switch-buffer)))
                         predicate)))
            (dynamic-collection
             (setq coll (if (and (eq this-command 'ivy-resume) (not (buffer-modified-p)))
                            ivy--all-candidates
                          (ivy--dynamic-collection-cands (or initial-input "")))))
            ((consp (car-safe collection))
             (setq collection (cl-remove-if-not predicate collection))
             (when (and sort (setq sort-fn (ivy--sort-function caller)))
               (setq collection (sort (copy-sequence collection) sort-fn))
               (setq sort nil))
             (setf (ivy-state-collection ivy-last) collection)
             (setq coll (let ((i -1))
                          (mapcar (lambda (x)
                                    (propertize x 'idx (cl-incf i)))
                                  (all-completions "" collection)))))
            ((or (functionp collection)
                 (byte-code-function-p collection)
                 (vectorp collection)
                 (hash-table-p collection)
                 (and (listp collection) (symbolp (car collection))))
             (let ((Info-complete-menu-buffer
                    ;; FIXME: This is a temporary workaround for issue #1803.
                    (or (bound-and-true-p Info-complete-menu-buffer)
                        (ivy-state-buffer state))))
               (setq coll (all-completions "" collection predicate))))
            (t
             (setq coll (all-completions "" collection predicate))))
      (unless (ivy-state-dynamic-collection ivy-last)
        (setq coll (delete "" coll)))
      (when (and sort
                 (or (functionp collection)
                     (not (eq history 'org-refile-history)))
                 (setq sort-fn (ivy--sort-function
                                (if (functionp collection) collection caller)))
                 (listp coll)
                 (null (nthcdr ivy-sort-max-size coll)))
        (setq coll (sort (copy-sequence coll) sort-fn)))
      (when def
        (cond ((stringp (car-safe def))
               (setq coll
                     (delete-dups
                      (append def coll))))
              ((and (stringp def) (not (member def coll)))
               (push def coll))))
      (setq coll (ivy--set-candidates coll))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (when initial-input
        ;; Needed for anchor to work
        (setq ivy--old-cands coll)
        (setq ivy--old-cands (ivy--filter initial-input coll)))
      (unless (setq ivy--trying-to-resume-dynamic-collection
                    (and preselect dynamic-collection))
        (when (integerp preselect)
          (setq ivy--old-re "")
          (ivy-set-index preselect)))
      (setq ivy--all-candidates coll)
      (unless (integerp preselect)
        (ivy-set-index (or
                        (and dynamic-collection
                             ivy--index)
                        (and preselect
                             (ivy--preselect-index
                              preselect
                              (if initial-input
                                  ivy--old-cands
                                coll)))
                        0))))
    (setq ivy-exit nil)
    (setq ivy--default
          (if (region-active-p)
              (buffer-substring (region-beginning) (region-end))
            (ivy-thing-at-point)))
    (setq ivy--prompt (ivy-add-prompt-count (ivy--quote-format-string prompt)))
    (setq ivy--use-selectable-prompt (ivy--prompt-selectable-p))
    (setf (ivy-state-initial-input ivy-last) initial-input)))

(defun ivy-add-prompt-count (prompt)
  "Add count information to PROMPT."
  (cond ((null ivy-count-format)
         (error "`ivy-count-format' must not be nil; set it to \"\" instead"))
        ((string-match "%d.*\\(%d\\)" ivy-count-format)
         (let* ((w
                  (if (listp ivy--all-candidates)
                      (1+ (floor (log (max 1 (length ivy--all-candidates)) 10)))
                      1))
                (s (replace-match (format "%%-%dd" w) t t ivy-count-format 1)))
           (string-match "%d" s)
           (concat (replace-match (format "%%%dd" w) t t s)
                   prompt)))
        ((string-match-p "%.*d" ivy-count-format)
         (concat ivy-count-format prompt))
        (t
         prompt)))

(defun ivy--quote-format-string (str)
  "Make STR suitable for `format' with no extra arguments."
  (replace-regexp-in-string "%" "%%" str t t))

;;;###autoload
(defun ivy-completing-read (prompt collection
                            &optional predicate require-match initial-input
                              history def inherit-input-method)
  "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value or a symbol.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored."
  (let ((handler
         (and (< ivy-completing-read-ignore-handlers-depth (minibuffer-depth))
              (assq this-command ivy-completing-read-handlers-alist))))
    (if handler
        (let ((completion-in-region-function #'completion--in-region)
              (ivy-completing-read-ignore-handlers-depth (1+ (minibuffer-depth))))
          (funcall (cdr handler)
                   prompt collection
                   predicate require-match
                   initial-input history
                   def inherit-input-method))
      ;; See the doc of `completing-read'.
      (when (consp history)
        (when (numberp (cdr history))
          (setq initial-input (nth (1- (cdr history))
                                   (symbol-value (car history)))))
        (setq history (car history)))
      (when (consp def)
        (setq def (car def)))
      (let ((str (ivy-read
                  prompt collection
                  :predicate predicate
                  :require-match (when (and collection require-match)
                                   require-match)
                  :initial-input (cond ((consp initial-input)
                                        (car initial-input))
                                       ((and (stringp initial-input)
                                             (not (eq collection #'read-file-name-internal))
                                             (string-match-p "\\+" initial-input))
                                        (replace-regexp-in-string
                                         "\\+" "\\\\+" initial-input))
                                       (t
                                        initial-input))
                  :preselect def
                  :def def
                  :history history
                  :keymap nil
                  :dynamic-collection ivy-completing-read-dynamic-collection
                  :extra-props '(:caller ivy-completing-read)
                  :caller (if (and collection (symbolp collection))
                              collection
                            this-command))))
        (if (string= str "")
            ;; For `completing-read' compat, return the first element of
            ;; DEFAULT, if it is a list; "", if DEFAULT is nil; or DEFAULT.
            (or def "")
          str)))))

(defun ivy-completing-read-with-empty-string-def
    (prompt collection
     &optional predicate require-match initial-input
       history def inherit-input-method)
  "Same as `ivy-completing-read' but with different handling of DEF.

Specifically, if DEF is nil, it is treated the same as if DEF was
the empty string. This mimics the behavior of
`completing-read-default'. This function can therefore be used in
place of `ivy-completing-read' for commands that rely on this
behavior."
  (ivy-completing-read
   prompt collection predicate require-match initial-input
   history (or def "") inherit-input-method))

(declare-function mc/all-fake-cursors "ext:multiple-cursors-core")

(defun ivy-completion-in-region-action (str)
  "Insert STR, erasing the previous one.
The previous string is between `ivy-completion-beg' and `ivy-completion-end'."
  (when (consp str)
    (setq str (cdr str)))
  (when (stringp str)
    (let ((fake-cursors (and (require 'multiple-cursors-core nil t)
                             (mc/all-fake-cursors)))
          (pt (point))
          (beg ivy-completion-beg)
          (end ivy-completion-end))
      (when beg
        (delete-region beg end))
      (setq ivy-completion-beg (point))
      (insert (substring-no-properties str))
      (completion--done str 'exact)
      (setq ivy-completion-end (point))
      (save-excursion
        (dolist (cursor fake-cursors)
          (goto-char (overlay-start cursor))
          (delete-region (+ (point) (- beg pt))
                         (+ (point) (- end pt)))
          (insert (substring-no-properties str))
          ;; manually move the fake cursor
          (move-overlay cursor (point) (1+ (point)))
          (set-marker (overlay-get cursor 'point) (point))
          (set-marker (overlay-get cursor 'mark) (point)))))))

(defun ivy-completion-common-length (str)
  "Return the amount of characters that match in  STR.

`completion-all-completions' computes this and returns the result
via text properties.

The first non-matching part is propertized:
- either with: (face (completions-first-difference))
- or: (font-lock-face completions-first-difference)."
  (let ((char-property-alias-alist '((face font-lock-face)))
        (i (1- (length str))))
    (catch 'done
      (while (>= i 0)
        (when (equal (get-text-property i 'face str)
                     '(completions-first-difference))
          (throw 'done i))
        (cl-decf i))
      (throw 'done (length str)))))

(defun ivy-completion-in-region (start end collection &optional predicate)
  "An Ivy function suitable for `completion-in-region-function'.
The function completes the text between START and END using COLLECTION.
PREDICATE (a function called with no arguments) says when to exit.
See `completion-in-region' for further information."
  (let* ((enable-recursive-minibuffers t)
         (str (buffer-substring-no-properties start end))
         (completion-ignore-case (ivy--case-fold-p str))
         (comps
          (completion-all-completions str collection predicate (- end start))))
    (cond ((null comps)
           (message "No matches"))
          ((progn
             (nconc comps nil)
             (and (null (cdr comps))
                  (string= str (car comps))))
           (message "Sole match"))
          (t
           (when (eq collection 'crm--collection-fn)
             (setq comps (delete-dups comps)))
           (let* ((len (ivy-completion-common-length (car comps)))
                  (initial (cond ((= len 0)
                                  "")
                                 ((let ((str-len (length str)))
                                    (when (> len str-len)
                                      (setq len str-len)
                                      str)))
                                 (t
                                  (substring str (- len))))))
             (delete-region (- end len) end)
             (setq ivy-completion-beg (- end len))
             (setq ivy-completion-end ivy-completion-beg)
             (if (null (cdr comps))
                 (progn
                   (unless (minibuffer-window-active-p (selected-window))
                     (setf (ivy-state-window ivy-last) (selected-window)))
                   (ivy-completion-in-region-action
                    (substring-no-properties (car comps))))
               (dolist (s comps)
                 ;; Remove face `completions-first-difference'.
                 (ivy--remove-props s 'face))
               (setq ivy--old-re nil)
               (unless (ivy--filter initial comps)
                 (setq initial nil)
                 (setq predicate nil)
                 (setq collection comps))
               (unless (derived-mode-p #'emacs-lisp-mode)
                 (setq collection comps)
                 (setq predicate nil))
               (ivy-read (format "(%s): " str) collection
                         :predicate predicate
                         :initial-input (concat
                                         (and (derived-mode-p #'emacs-lisp-mode)
                                              "^")
                                         initial)
                         :action #'ivy-completion-in-region-action
                         :unwind (lambda ()
                                   (unless (eq ivy-exit 'done)
                                     (goto-char ivy-completion-beg)
                                     (when initial
                                       (insert initial))))
                         :caller 'ivy-completion-in-region)))
           ;; Return value should be non-nil on valid completion;
           ;; see `completion-in-region'.
           t))))

(defun ivy-completion-in-region-prompt ()
  "Prompt function for `ivy-completion-in-region'.
See `ivy-set-prompt'."
  (and (window-minibuffer-p (ivy-state-window ivy-last))
       (ivy-add-prompt-count (ivy-state-prompt ivy-last))))

(ivy-set-prompt #'ivy-completion-in-region #'ivy-completion-in-region-prompt)

(defcustom ivy-do-completion-in-region t
  "When non-nil `ivy-mode' will set `completion-in-region-function'."
  :type 'boolean)

(defvar ivy--old-crf nil
  "Store previous value of `completing-read-function'.")

(defvar ivy--old-cirf nil
  "Store previous value of `completion-in-region-function'.")

;;;###autoload
(define-minor-mode ivy-mode
  "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
  :group 'ivy
  :global t
  :keymap ivy-mode-map
  :lighter " ivy"
  (if ivy-mode
      (progn
        (unless (eq completing-read-function #'ivy-completing-read)
          (setq ivy--old-crf completing-read-function)
          (setq completing-read-function #'ivy-completing-read))
        (when ivy-do-completion-in-region
          (unless (eq completion-in-region-function #'ivy-completion-in-region)
            (setq ivy--old-cirf completion-in-region-function)
            (setq completion-in-region-function #'ivy-completion-in-region))))
    (when (eq completing-read-function #'ivy-completing-read)
      (setq completing-read-function (or ivy--old-crf
                                         #'completing-read-default))
      (setq ivy--old-crf nil))
    (when (eq completion-in-region-function #'ivy-completion-in-region)
      (setq completion-in-region-function (or ivy--old-cirf
                                              #'completion--in-region))
      (setq ivy--old-cirf nil))))

(defun ivy--preselect-index (preselect candidates)
  "Return the index of PRESELECT in CANDIDATES."
  (or (cond ((integerp preselect)
             (if (integerp (car candidates))
                 (cl-position preselect candidates)
               preselect))
            ((cl-position preselect candidates :test #'equal))
            ((ivy--regex-p preselect)
             (cl-position preselect candidates :test #'string-match-p)))
      0))

;;* Implementation
;;** Regex
(defun ivy-re-match (re-seq str)
  "Return non-nil if RE-SEQ is matched by STR.

RE-SEQ is a list of (RE . MATCH-P).

RE is a regular expression.

MATCH-P is t when RE should match STR and nil when RE should not
match STR.

Each element of RE-SEQ must match for the function to return true.

This concept is used to generalize regular expressions for
`ivy--regex-plus' and `ivy--regex-ignore-order'."
  (let ((res t)
        re)
    (while (and res (setq re (pop re-seq)))
      (setq res
            (if (cdr re)
                (string-match-p (car re) str)
              (not (string-match-p (car re) str)))))
    res))

(defvar ivy--regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defvar ivy--input-garbage nil)

(defun ivy--split (str)
  "Split STR into list of substrings bounded by spaces.
Single spaces act as splitting points.  Consecutive spaces
\"quote\" their preceding spaces, i.e., guard them from being
split.  This allows the literal interpretation of N spaces by
inputting N+1 spaces.  Any substring not constituting a valid
regexp is passed to `regexp-quote'."
  (let ((len (length str))
        (i 0)
        (start 0)
        (res nil)
        match-len
        end
        c)
    (catch 'break
      (while (< i len)
        (setq c (aref str i))
        (cond ((= ?\[ c)
               (if (setq end (ivy--match-regex-brackets
                              (substring str i)))
                   (cl-incf i end)
                 (setq ivy--input-garbage (substring str i))
                 (throw 'break nil)))
              ((= ?\\ c)
               (if (and (< (1+ i) len) (= ?\( (aref str (1+ i))))
                   (progn
                     (when (> i start)
                       (push (substring str start i) res))
                     (if (eq (string-match "\\\\([^\0]*?\\\\)" str i) i)
                         (progn
                           (push (match-string 0 str) res)
                           (setq i (match-end 0))
                           (setq start i))
                       (setq ivy--input-garbage (substring str i))
                       (throw 'break nil)))
                 (cl-incf i)))
              ((= ?\  c)
               (string-match " +" str i)
               (setq match-len (- (match-end 0) (match-beginning 0)))
               (if (= match-len 1)
                   (progn
                     (when (> i start)
                       (push (substring str start i) res))
                     (setq start (1+ i)))
                 (setq str (replace-match
                            (make-string (1- match-len) ?\ )
                            nil nil str))
                 (setq len (length str))
                 (cl-incf i (1- match-len)))
               (cl-incf i))
              (t
               (cl-incf i)))))
    (when (< start i)
      (push (substring str start) res))
    (mapcar #'ivy--regex-or-literal (nreverse res))))

(defun ivy--match-regex-brackets (str)
  (let ((len (length str))
        (i 1)
        (open-count 1)
        c)
    (while (and (< i len)
                (> open-count 0))
      (setq c (aref str i))
      (cond ((= c ?\[)
             (cl-incf open-count))
            ((= c ?\])
             (cl-decf open-count)))
      (cl-incf i))
    (when (= open-count 0)
      (if (eq (string-match "[+*?]" str i) i)
          (match-end 0)
        i))))

(defun ivy--trim-trailing-re (regex)
  "Trim incomplete REGEX.
If REGEX ends with \\|, trim it, since then it matches an empty string."
  (if (string-match "\\`\\(.*\\)[\\]|\\'" regex)
      (match-string 1 regex)
    regex))

(defun ivy--regex (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str ivy--regex-hash))))
    (if hashed
        (progn
          (setq ivy--subexps (car hashed))
          (cdr hashed))
      (when (string-match-p "\\(?:[^\\]\\|^\\)\\\\\\'" str)
        (setq str (substring str 0 -1)))
      (setq str (ivy--trim-trailing-re str))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (if (string-match-p "\\`\\.[^.]" (car subs))
                               (concat "\\." (substring (car subs) 1))
                             (car subs)))
                        (cons
                         (setq ivy--subexps (length subs))
                         (replace-regexp-in-string
                          "\\.\\*\\??\\\\( "
                          "\\( "
                          (mapconcat
                           (lambda (x)
                             (if (string-match-p "\\`\\\\([^?][^\0]*\\\\)\\'" x)
                                 x
                               (format "\\(%s\\)" x)))
                           subs
                           (if greedy ".*" ".*?"))
                          nil t))))
                    ivy--regex-hash)))))

(defun ivy--regex-p (object)
  "Return OBJECT if it is a valid regular expression, else nil."
  (ignore-errors (string-match-p object "") object))

(defun ivy--regex-or-literal (str)
  "If STR isn't a legal regexp, escape it."
  (or (ivy--regex-p str) (regexp-quote str)))

(defun ivy--split-negation (str)
  "Split STR into text before and after ! delimiter.
Do not split if the delimiter is escaped as \\!.

Assumes there is at most one un-escaped delimiter and discards
text after delimiter if it is empty.  Modifies match data."
  (unless (string= str "")
    (let ((delim "\\(?:\\`\\|[^\\]\\)\\(!\\)"))
      (mapcar (lambda (split)
                ;; Store "\!" as "!".
                (replace-regexp-in-string "\\\\!" "!" split t t))
              (if (string-match delim str)
                  ;; Ignore everything past first un-escaped ! rather than
                  ;; crashing.  We can't warn or error because the minibuffer is
                  ;; already active.
                  (let* ((i (match-beginning 1))
                         (j (and (string-match delim str (1+ i))
                                 (match-beginning 1)))
                         (neg (substring str (1+ i) j)))
                    (cons (substring str 0 i)
                          (and (not (string= neg ""))
                               (list neg))))
                (list str))))))

(defun ivy--split-spaces (str)
  "Split STR on spaces, unless they're preceded by \\.
No un-escaped spaces are left in the output.  Any substring not
constituting a valid regexp is passed to `regexp-quote'."
  (when str
    (let ((i 0) ; End of last search.
          (j 0) ; End of last delimiter.
          parts)
      (while (string-match "\\(\\\\ \\)\\| +" str i)
        (setq i (match-end 0))
        (if (not (match-beginning 1))
            ;; Un-escaped space(s).
            (let ((delim (match-beginning 0)))
              (when (< j delim)
                (push (substring str j delim) parts))
              (setq j i))
          ;; Store "\ " as " ".
          (setq str (replace-match " " t t str 1))
          (setq i (1- i))))
      (when (< j (length str))
        (push (substring str j) parts))
      (mapcar #'ivy--regex-or-literal (nreverse parts)))))

(defun ivy--regex-ignore-order (str)
  "Re-build regex from STR by splitting at spaces and using ! for negation.

Examples:
foo          -> matches \"foo\"
foo bar      -> matches if both \"foo\" and \"bar\" match (any order)
foo !bar     -> matches if \"foo\" matches and \"bar\" does not match
foo !bar baz -> matches if \"foo\" matches and neither \"bar\" nor \"baz\" match
foo[a-z]     -> matches \"foo[a-z]\"

Escaping examples:
foo\\!bar -> matches \"foo!bar\"
foo\\ bar -> matches \"foo bar\"

Returns a list suitable for `ivy-re-match'."
  (setq str (ivy--trim-trailing-re str))
  (let* (regex-parts
         (raw-parts (ivy--split-negation str)))
    (dolist (part (ivy--split-spaces (car raw-parts)))
      (push (cons part t) regex-parts))
    (when (cdr raw-parts)
      (dolist (part (ivy--split-spaces (cadr raw-parts)))
        (push (cons part nil) regex-parts)))
    (if regex-parts (nreverse regex-parts)
      "")))

(defun ivy--regex-plus (str)
  "Build a regex sequence from STR.
Spaces are wild card characters, everything before \"!\" should
match.  Everything after \"!\" should not match."
  (let ((parts (ivy--split-negation str)))
    (cl-case (length parts)
      (0
       "")
      (1
       (if (= (aref str 0) ?!)
           (list (cons "" t)
                 (list (ivy--regex (car parts))))
         (ivy--regex (car parts))))
      (2
       (cons
        (cons (ivy--regex (car parts)) t)
        (mapcar #'list (split-string (cadr parts) " " t))))
      (t (error "Unexpected: use only one !")))))

(defun ivy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (setq str (ivy--trim-trailing-re str))
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (let ((lst (string-to-list (match-string 2 str))))
                    (apply #'concat
                           (cl-mapcar
                            #'concat
                            (cons "" (cdr (mapcar (lambda (c) (format "[^%c\n]*" c))
                                                  lst)))
                            (mapcar (lambda (x) (format "\\(%s\\)" (regexp-quote (char-to-string x))))
                                    lst))))
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))

(defcustom ivy-fixed-height-minibuffer nil
  "When non nil, fix the height of the minibuffer during ivy completion.
This effectively sets the minimum height at this level to `ivy-height' and
tries to ensure that it does not change depending on the number of candidates."
  :type 'boolean)

;;** Rest
(defcustom ivy-truncate-lines t
  "Minibuffer setting for `truncate-lines'."
  :type 'boolean)

(defun ivy--minibuffer-setup ()
  "Setup ivy completion in the minibuffer."
  (setq-local mwheel-scroll-up-function 'ivy-next-line)
  (setq-local mwheel-scroll-down-function 'ivy-previous-line)
  (setq-local completion-show-inline-help nil)
  (setq-local line-spacing nil)
  (setq-local minibuffer-default-add-function
              (lambda ()
                (list ivy--default)))
  (setq-local inhibit-field-text-motion nil)
  (setq truncate-lines ivy-truncate-lines)
  (setq-local max-mini-window-height ivy-height)
  (let ((height (cond ((and ivy-fixed-height-minibuffer
                            (not (eq (ivy-state-caller ivy-last)
                                     #'ivy-completion-in-region)))
                       (+ ivy-height (if ivy-add-newline-after-prompt 1 0)))
                      (ivy-add-newline-after-prompt 2))))
    (when height
      (set-window-text-height nil height)))
  (add-hook 'post-command-hook #'ivy--queue-exhibit nil t)
  (add-hook 'window-size-change-functions #'ivy--window-size-changed nil t)
  (let ((hook (ivy-alist-setting ivy-hooks-alist)))
    (when (functionp hook)
      (funcall hook))))

(defun ivy--input ()
  "Return the current minibuffer input."
  ;; assume one-line minibuffer input
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (let ((inhibit-field-text-motion t))
      (buffer-substring-no-properties
       (point)
       (line-end-position)))))

(defun ivy--minibuffer-cleanup ()
  "Delete the displayed completion candidates."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (delete-region (line-end-position) (point-max))))

(defun ivy-cleanup-string (str)
  "Destructively remove unwanted text properties from STR."
  (ivy--remove-props str 'field))

(defvar ivy-set-prompt-text-properties-function
  #'ivy-set-prompt-text-properties-default
  "Function to set the text properties of the default ivy prompt.
Called with two arguments, PROMPT and PROPS, where PROMPT is the
string to be propertized and PROPS is a plist of default text
properties that may be applied to PROMPT.  The function should
return the propertized PROMPT, which may be modified in-place.")

(defun ivy-set-prompt-text-properties-default (prompt props)
  "Propertize (confirm) and (match required) parts of PROMPT.
PROPS is a plist of default text properties to apply to these
parts beyond their respective faces `ivy-confirm-face' and
`ivy-match-required-face'."
  (dolist (pair '(("confirm" . ivy-confirm-face)
                  ("match required" . ivy-match-required-face)))
    (let ((i (string-match-p (car pair) prompt)))
      (when i
        (add-text-properties i (+ i (length (car pair)))
                             `(face ,(cdr pair) ,@props)
                             prompt))))
  prompt)

(defun ivy-prompt ()
  "Return the current prompt."
  (let* ((caller (ivy-state-caller ivy-last))
         (fn (plist-get ivy--prompts-list caller)))
    (if fn
        (condition-case err
            (funcall fn)
          (wrong-number-of-arguments
           (lwarn 'ivy :error "%s
  Prompt function set via `ivy-set-prompt' for caller `%s'
  should take no arguments."
                  (error-message-string err)
                  caller)
           ;; Old behavior.
           (funcall fn (ivy-state-prompt ivy-last))))
      ivy--prompt)))

(defun ivy--insert-prompt ()
  "Update the prompt according to `ivy--prompt'."
  (when (setq ivy--prompt (ivy-prompt))
    (unless (memq this-command '(ivy-done ivy-alt-done ivy-partial-or-done
                                 counsel-find-symbol))
      (setq ivy--prompt-extra ""))
    (let (head tail)
      (if (string-match "\\(.*?\\)\\(:? ?\\)\\'" ivy--prompt)
          (progn
            (setq head (match-string 1 ivy--prompt))
            (setq tail (match-string 2 ivy--prompt)))
        (setq head ivy--prompt)
        (setq tail ""))
      (let ((inhibit-read-only t)
            (std-props '(front-sticky t rear-nonsticky t field t read-only t))
            (n-str
             (concat
              (if (and (bound-and-true-p minibuffer-depth-indicate-mode)
                       (> (minibuffer-depth) 1))
                  (format "[%d] " (minibuffer-depth))
                "")
              (concat
               (if (string-match "%d.*%d" ivy-count-format)
                   (format head
                           (1+ ivy--index)
                           (or (and (ivy-state-dynamic-collection ivy-last)
                                    ivy--full-length)
                               ivy--length))
                 (format head
                         (or (and (ivy-state-dynamic-collection ivy-last)
                                  ivy--full-length)
                             ivy--length)))
               ivy--prompt-extra
               tail)))
            (d-str (if ivy--directory
                       (abbreviate-file-name ivy--directory)
                     "")))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point-min) (minibuffer-prompt-end))
          (let ((len-n (length n-str))
                (len-d (length d-str))
                (ww (window-width)))
            (setq n-str
                  (cond ((> (+ len-n len-d) ww)
                         (concat n-str "\n" d-str "\n"))
                        ((> (+ len-n len-d (length ivy-text)) ww)
                         (concat n-str d-str "\n"))
                        (t
                         (concat n-str d-str)))))
          (when ivy-pre-prompt-function
            (setq n-str (concat (funcall ivy-pre-prompt-function) n-str)))
          (when ivy-add-newline-after-prompt
            (setq n-str (concat n-str "\n")))
          (let ((regex (format "\\([^\n]\\{%d\\}\\)[^\n]" (window-width))))
            (while (string-match regex n-str)
              (setq n-str (replace-match
                           (concat (match-string 1 n-str) "\n")
                           nil t n-str 1))))
          (set-text-properties 0 (length n-str)
                               `(face minibuffer-prompt ,@std-props)
                               n-str)
          (setq n-str (funcall ivy-set-prompt-text-properties-function
                               n-str std-props))
          (insert n-str))
        ;; Mark prompt as selected if the user moves there or it is the only
        ;; option left.  Since the user input stays put, we have to manually
        ;; remove the face as well.
        (when ivy--use-selectable-prompt
          (if (= ivy--index -1)
              (add-face-text-property
               (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
            (remove-list-of-text-properties
             (minibuffer-prompt-end) (line-end-position) '(face))))
        ;; get out of the prompt area
        (constrain-to-field nil (point-max))))))

(defun ivy--sort-maybe (collection)
  "Sort COLLECTION if needed."
  (let ((sort (ivy-state-sort ivy-last)))
    (if (and sort
             (or (functionp sort)
                 (functionp (setq sort (ivy--sort-function
                                        (ivy-state-collection ivy-last))))))
        (sort (copy-sequence collection) sort)
      collection)))

(defcustom ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
  "Action to take when a slash is added to the end of a non existing directory.
Possible choices are 'ivy-magic-slash-non-match-cd-selected,
'ivy-magic-slash-non-match-create, or nil"
  :type '(choice
          (const :tag "Use currently selected directory"
           ivy-magic-slash-non-match-cd-selected)
          (const :tag "Create and use new directory"
           ivy-magic-slash-non-match-create)
          (const :tag "Do nothing"
           nil)))

(defun ivy--create-and-cd (dir)
  "When completing file names, create directory DIR and move there."
  (make-directory dir)
  (ivy--cd dir))

(defun ivy--magic-file-doubleslash-directory ()
  "Return an appropriate directory for when two slashes are entered."
  (let (remote)
    (cond
      ;; Windows
      ;; ((string-match "\\`[[:alpha:]]:/" ivy--directory)
      ;;  (match-string 0 ivy--directory))
      ;; Remote root if on remote
      ((setq remote (file-remote-p ivy--directory))
       (concat remote "/"))
      ;; Local root
      (t
       "/"))))

(defun ivy--magic-file-slash ()
  "Handle slash when completing file names."
  (when (or (and (eq this-command #'self-insert-command)
                 (eolp))
            (eq this-command #'ivy-partial-or-done))
    (let ((canonical (expand-file-name ivy-text ivy--directory))
          (magic (not (string= ivy-text "/"))))
      (cond ((member ivy-text ivy--all-candidates)
             (ivy--cd canonical))
            ((and (eq system-type 'windows-nt) (string= ivy-text "//")))
            ((string-suffix-p "//" ivy-text)
             (ivy--cd
              (ivy--magic-file-doubleslash-directory)))
            ((string-match-p "\\`/ssh:" ivy-text)
             (ivy--cd (file-name-directory ivy-text)))
            ((string-match "[[:alpha:]]:/\\'" ivy-text)
             (let ((drive-root (match-string 0 ivy-text)))
               (when (file-exists-p drive-root)
                 (ivy--cd drive-root))))
            ((and magic (file-directory-p canonical))
             (ivy--cd canonical))
            ((let ((default-directory ivy--directory))
               (and (or (> ivy--index 0)
                        (= ivy--length 1)
                        magic)
                    (not (ivy--prompt-selected-p))
                    (not (equal (ivy-state-current ivy-last) ""))
                    (file-directory-p (ivy-state-current ivy-last))
                    (or (eq ivy-magic-slash-non-match-action
                            'ivy-magic-slash-non-match-cd-selected)
                        (eq this-command #'ivy-partial-or-done))))
             (ivy--cd
              (expand-file-name (ivy-state-current ivy-last) ivy--directory)))
            ((and (eq ivy-magic-slash-non-match-action
                      'ivy-magic-slash-non-match-create)
                  magic)
             (ivy--create-and-cd canonical))))))

(defun ivy-magic-read-file-env ()
  "If reading filename, jump to environment variable location."
  (interactive)
  (if (and ivy--directory
           (equal ivy-text ""))
      (let* ((cands (cl-loop for pair in process-environment
                       for (var val) = (split-string pair "=" t)
                       if (and val (not (equal "" val)))
                       if (file-exists-p
                           (if (file-name-absolute-p val)
                               val
                             (setq val
                                   (expand-file-name val ivy--directory))))
                       collect (cons var val)))
             (enable-recursive-minibuffers t)
             (x (ivy-read "Env: " cands))
             (path (cdr (assoc x cands))))
        (insert (if (file-accessible-directory-p path)
                    (file-name-as-directory path)
                  path))
        (ivy--cd-maybe))
    (insert last-input-event)))

(defun ivy-make-magic-action (caller key)
  "Return a command that does the equivalent of `ivy-read-action' and KEY.
This happens only when the input is empty.
The intention is to bind the result to keys that are typically
bound to `self-insert-command'."
  (let* ((alist (assoc key
                       (plist-get
                        ivy--actions-list
                        caller)))
         (doc (format "%s (`%S')"
                      (nth 2 alist)
                      (nth 1 alist))))
    `(lambda (&optional arg)
       ,doc
       (interactive "p")
       (if (string= "" ivy-text)
           (execute-kbd-macro
            (kbd ,(concat "M-o " key)))
         (self-insert-command arg)))))

(defcustom ivy-magic-tilde t
  "When non-nil, ~ will move home when selecting files.
Otherwise, ~/ will move home."
  :type 'boolean)

(defcustom ivy-dynamic-exhibit-delay-ms 0
  "Delay in ms before dynamic collections are refreshed"
  :type 'integer)

(defvar ivy--exhibit-timer nil)

(defun ivy--queue-exhibit ()
  "Insert Ivy completions display, possibly after a timeout for
dynamic collections.
Should be run via minibuffer `post-command-hook'."
  (if (and (> ivy-dynamic-exhibit-delay-ms 0)
           (ivy-state-dynamic-collection ivy-last))
      (progn
        (when ivy--exhibit-timer (cancel-timer ivy--exhibit-timer))
        (setq ivy--exhibit-timer
              (run-with-timer
               (/ ivy-dynamic-exhibit-delay-ms 1000.0)
               nil
               'ivy--exhibit)))
    (ivy--exhibit)))

(defalias 'ivy--file-local-name
  (if (fboundp 'file-local-name)
      #'file-local-name
    (lambda (file)
      (or (file-remote-p file 'localname) file)))
  "Compatibility shim for `file-local-name'.
The function was added in Emacs 26.1.")

(defun ivy--magic-tilde-directory (dir)
  "Return an appropriate home for DIR for when ~ or ~/ are entered."
  (file-name-as-directory
   (expand-file-name
    (let* ((home (expand-file-name (concat (file-remote-p dir) "~/")))
           (dir-path (ivy--file-local-name dir))
           (home-path (ivy--file-local-name home)))
      (if (string= dir-path home-path)
          "~"
        home)))))

(defun ivy-update-candidates (cands)
  (ivy--insert-minibuffer
   (ivy--format
    (setq ivy--all-candidates cands))))

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (ivy-set-text (ivy--input))
    (let ((new-minibuffer (ivy--update-minibuffer)))
      (when new-minibuffer
        (ivy--insert-minibuffer new-minibuffer)))
    t))

(defun ivy--dynamic-collection-cands (input)
  (let ((coll (funcall (ivy-state-collection ivy-last) input)))
    (if (listp coll)
        (mapcar (lambda (x) (if (consp x) (car x) x)) coll)
      coll)))

(defun ivy--update-minibuffer ()
  (prog1
      (if (ivy-state-dynamic-collection ivy-last)
          ;; while-no-input would cause annoying
          ;; "Waiting for process to die...done" message interruptions
          (let ((inhibit-message t)
                coll in-progress)
            (unless (or (equal ivy--old-text ivy-text)
                        (eq this-command 'ivy-resume))
              (while-no-input
                (setq coll (ivy--dynamic-collection-cands ivy-text))
                (when (eq coll 0)
                  (setq coll nil)
                  (setq ivy--old-re nil)
                  (setq in-progress t))
                (setq ivy--all-candidates (ivy--sort-maybe coll))))
            (when (eq ivy--all-candidates 0)
              (setq ivy--all-candidates nil)
              (setq ivy--old-re nil)
              (setq in-progress t))
            (when (or ivy--all-candidates
                      (and (not (get-process " *counsel*"))
                           (not in-progress)))
              (ivy--set-index-dynamic-collection)
              (ivy--format ivy--all-candidates)))
        (cond (ivy--directory
               (cond ((or (string= "~/" ivy-text)
                          (and (string= "~" ivy-text)
                               ivy-magic-tilde))
                      (ivy--cd (ivy--magic-tilde-directory ivy--directory)))
                     ((string-match "/\\'" ivy-text)
                      (ivy--magic-file-slash))))
              ((eq (ivy-state-collection ivy-last) #'internal-complete-buffer)
               (when (or (and (string-match "\\` " ivy-text)
                              (not (string-match "\\` " ivy--old-text)))
                         (and (string-match "\\` " ivy--old-text)
                              (not (string-match "\\` " ivy-text))))
                 (setq ivy--all-candidates
                       (if (= (string-to-char ivy-text) ?\s)
                           (ivy--buffer-list " ")
                         (ivy--buffer-list "" ivy-use-virtual-buffers)))
                 (setq ivy--old-re nil))))
        (with-current-buffer (ivy-state-buffer ivy-last)
          (ivy--format
           (ivy--filter ivy-text ivy--all-candidates))))
    (setq ivy--old-text ivy-text)))

(defun ivy-display-function-fallback (str)
  (let ((buffer-undo-list t))
    (save-excursion
      (forward-line 1)
      (insert str))))

(defun ivy--insert-minibuffer (text)
  "Insert TEXT into minibuffer with appropriate cleanup."
  (let ((resize-mini-windows nil)
        (update-fn (ivy-state-update-fn ivy-last))
        (old-mark (marker-position (mark-marker)))
        (win (active-minibuffer-window))
        deactivate-mark)
    (when win
      (with-selected-window win
        (ivy--minibuffer-cleanup)
        (when update-fn
          (funcall update-fn))
        (ivy--insert-prompt)
        ;; Do nothing if while-no-input was aborted.
        (when (stringp text)
          (if ivy--display-function
              (funcall ivy--display-function text)
            (ivy-display-function-fallback text)))
        (ivy--resize-minibuffer-to-fit)
        ;; prevent region growing due to text remove/add
        (when (region-active-p)
          (set-mark old-mark))))))

(defvar ivy-auto-shrink-minibuffer nil
  "When non-nil and the height < `ivy-height', auto-shrink the minibuffer.")

(make-obsolete-variable 'ivy-auto-shrink-minibuffer
                        'ivy-auto-shrink-minibuffer-alist
                        "<2020-04-28 Tue>")

(defcustom ivy-auto-shrink-minibuffer-alist nil
  "An alist to configure auto-shrinking of the minibuffer.

Each key is a caller symbol.  When the value is non-nil, and the
height < `ivy-height', auto-shrink the minibuffer."
  :type '(alist
          :key-type symbol
          :value-type boolean))

(defun ivy--do-shrink-window ()
  (let ((h (save-excursion
             (goto-char (minibuffer-prompt-end))
             (let ((inhibit-field-text-motion t))
               (line-number-at-pos)))))
    (shrink-window (-
                    (/ (window-body-height nil t)
                       (frame-char-height))
                    ivy--length h))))

(defun ivy--resize-minibuffer-to-fit ()
  "Resize the minibuffer window size to fit the text in the minibuffer."
  (unless (or (frame-root-window-p (minibuffer-window))
              (memq this-command '(ivy-read-action
                                   ivy-dispatching-done
                                   ivy-dispatching-call)))
    (with-selected-window (minibuffer-window)
      (if (fboundp 'window-text-pixel-size)
          (let ((text-height (cdr (window-text-pixel-size)))
                (body-height (window-body-height nil t)))
            (cond ((> text-height body-height)
                   ;; Note: the size increment needs to be at least
                   ;; frame-char-height, otherwise resizing won't do
                   ;; anything.
                   (let ((delta (max (- text-height body-height)
                                     (frame-char-height))))
                     (window-resize nil delta nil t t)))
                  ((and (or ivy-auto-shrink-minibuffer
                            (ivy-alist-setting
                             ivy-auto-shrink-minibuffer-alist))
                        (< ivy--length ivy-height))
                   (ivy--do-shrink-window))))
        (let ((text-height (count-screen-lines))
              (body-height (window-body-height)))
          (when (> text-height body-height)
            (window-resize nil (- text-height body-height) nil t)))))))

(defun ivy--window-size-changed (&rest _)
  "Resize ivy window to fit with current frame's size."
  (when ivy-mode
    (ivy--resize-minibuffer-to-fit)))

(defun ivy--add-face (str face)
  "Propertize STR with FACE."
  (let ((len (length str)))
    (condition-case nil
        (progn
          (colir-blend-face-background 0 len face str)
          (let ((foreground (face-foreground face)))
            (when foreground
              (add-face-text-property
               0 len (list :foreground foreground) nil str))))
      (error
       (ignore-errors
         (font-lock-append-text-property 0 len 'face face str)))))
  str)

(declare-function flx-make-string-cache "ext:flx")
(declare-function flx-score "ext:flx")

(defvar ivy--flx-cache nil)

(with-eval-after-load 'flx
  (setq ivy--flx-cache (flx-make-string-cache)))

(defun ivy-toggle-case-fold ()
  "Toggle `case-fold-search' for Ivy operations.

Instead of modifying `case-fold-search' directly, this command
toggles `ivy-case-fold-search', which can take on more values
than the former, between nil and either `auto' or t.  See
`ivy-case-fold-search-default' for the meaning of these values.

In any Ivy completion session, the case folding starts with
`ivy-case-fold-search-default'."
  (interactive)
  (setq ivy-case-fold-search
        (and (not ivy-case-fold-search)
             (or ivy-case-fold-search-default 'auto)))
  ;; Reset cache so that the candidate list updates.
  (setq ivy--old-re nil))

(defun ivy--re-filter (re candidates &optional mkpred)
  "Return all RE matching CANDIDATES.
RE is a list of cons cells, with a regexp car and a boolean cdr.
When the cdr is t, the car must match.
Otherwise, the car must not match."
  (if (equal re "")
      candidates
    (ignore-errors
      (dolist (re (if (stringp re) (list (cons re t)) re))
        (let* ((re-str (car re))
               (pred
                (if mkpred
                    (funcall mkpred re-str)
                  (lambda (x) (string-match-p re-str x)))))
          (setq candidates
                (cl-remove nil candidates
                           (if (cdr re) :if-not :if)
                           pred))))
      candidates)))

(defun ivy--filter (name candidates)
  "Return all items that match NAME in CANDIDATES.
CANDIDATES are assumed to be static."
  (let ((re (funcall ivy--regex-function name)))
    (if (and
         ivy--old-re
         ivy--old-cands
         (equal re ivy--old-re))
        ;; quick caching for "C-n", "C-p" etc.
        ivy--old-cands
      (let* ((re-str (ivy-re-to-str re))
             (matcher (ivy-state-matcher ivy-last))
             (case-fold-search (ivy--case-fold-p name))
             (cands (cond
                      (matcher
                       (funcall matcher re candidates))
                      ((and ivy--old-re
                            (stringp re)
                            (stringp ivy--old-re)
                            (not (string-match-p "\\\\" ivy--old-re))
                            (not (equal ivy--old-re ""))
                            (memq (cl-search
                                   (if (string-match-p "\\\\)\\'" ivy--old-re)
                                       (substring ivy--old-re 0 -2)
                                     ivy--old-re)
                                   re)
                                  '(0 2))
                            ivy--old-cands
                            (ivy--re-filter re ivy--old-cands)))
                      (t
                       (ivy--re-filter re candidates)))))
        (if (memq (cdr (assq (ivy-state-caller ivy-last)
                             ivy-index-functions-alist))
                  '(ivy-recompute-index-swiper
                    ivy-recompute-index-swiper-async
                    ivy-recompute-index-swiper-async-backward
                    ivy-recompute-index-swiper-backward))
            (progn
              (ivy--recompute-index re-str cands)
              (setq ivy--old-cands (ivy--sort name cands)))
          (setq ivy--old-cands (ivy--sort name cands))
          (ivy--recompute-index re-str ivy--old-cands))
        (setq ivy--old-re re)
        ivy--old-cands))))

(defun ivy--set-candidates (x)
  "Update `ivy--all-candidates' with X."
  (let (res
        ;; (ivy--recompute-index-inhibit t)
        )
    (dolist (source ivy--extra-candidates)
      (if (equal source '(original-source))
          (if (null res)
              (setq res x)
            (setq res (append x res)))
        (setq ivy--old-re nil)
        (setq res (append
                   (ivy--filter ivy-text (cadr source))
                   res))))
    (setq ivy--all-candidates
          (if (cdr ivy--extra-candidates)
              (delete-dups res)
            res))))

(defun ivy--shorter-matches-first (_name cands)
  "Sort CANDS according to their length."
  (if (nthcdr ivy-sort-max-size cands)
      cands
    (cl-sort (copy-sequence cands) #'< :key #'length)))

(defcustom ivy-sort-matches-functions-alist
  '((t . nil)
    (ivy-completion-in-region . ivy--shorter-matches-first)
    (ivy-switch-buffer . ivy-sort-function-buffer))
  "An alist of functions for sorting matching candidates.

Unlike `ivy-sort-functions-alist', which is used to sort the
whole collection only once, this alist of functions are used to
sort only matching candidates after each change in input.

The alist KEY is either a collection function or t to match
previously unmatched collection functions.

The alist VAL is a sorting function with the signature of
`ivy--prefix-sort'."
  :type '(alist
          :key-type (choice
                     (const :tag "Fall-through" t)
                     (symbol :tag "Collection"))
          :value-type
          (choice
           (const :tag "Don't sort" nil)
           (const :tag "Put prefix matches ahead" ivy--prefix-sort)
           (function :tag "Custom sort function"))))

(defun ivy--sort-files-by-date (_name candidates)
  "Re-sort CANDIDATES according to file modification date."
  (let ((default-directory ivy--directory))
    (sort (copy-sequence candidates) #'file-newer-than-file-p)))

(defvar ivy--flx-featurep (require 'flx nil 'noerror))

(defun ivy--sort (name candidates)
  "Re-sort candidates by NAME.
All CANDIDATES are assumed to match NAME."
  (let (fun)
    (cond ((setq fun (ivy-alist-setting ivy-sort-matches-functions-alist))
           (funcall fun name candidates))
          ((and ivy--flx-featurep
                (eq ivy--regex-function 'ivy--regex-fuzzy))
           (ivy--flx-sort name candidates))
          (t
           candidates))))

(defun ivy--prefix-sort (name candidates)
  "Re-sort candidates by NAME.
All CANDIDATES are assumed to match NAME.
Prefix matches to NAME are put ahead of the list."
  (if (or (string= name "")
          (= (aref name 0) ?^))
      candidates
    (let ((re-prefix (concat "\\`" (funcall ivy--regex-function name)))
          res-prefix
          res-noprefix)
      (dolist (s candidates)
        (if (string-match-p re-prefix s)
            (push s res-prefix)
          (push s res-noprefix)))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)))))

(defvar ivy--virtual-buffers nil
  "Store the virtual buffers alist.")

(defun ivy-re-to-str (re)
  "Transform RE to a string.

Functions like `ivy--regex-ignore-order' return a cons list.
This function extracts a string from the cons list."
  (if (consp re) (caar re) re))

(defun ivy-sort-function-buffer (name candidates)
  "Re-sort candidates by NAME.
CANDIDATES is a list of buffer names each containing NAME.
Sort open buffers before virtual buffers, and prefix matches
before substring matches."
  (if (or (string= name "")
          (= (aref name 0) ?^))
      candidates
    (let* ((base-re (ivy-re-to-str (funcall ivy--regex-function name)))
           (re-star-prefix (concat "\\`\\*" base-re))
           (re-prefix (concat "\\`" base-re))
           res-prefix
           res-noprefix
           res-virtual-prefix
           res-virtual-noprefix)
      (dolist (s candidates)
        (cond
          ((and (assoc s ivy--virtual-buffers)
                (or (string-match-p re-star-prefix s)
                    (string-match-p re-prefix s)))
           (push s res-virtual-prefix))
          ((assoc s ivy--virtual-buffers)
           (push s res-virtual-noprefix))
          ((or (string-match-p re-star-prefix s)
               (string-match-p re-prefix s))
           (push s res-prefix))
          (t
           (push s res-noprefix))))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)
       (nreverse res-virtual-prefix)
       (nreverse res-virtual-noprefix)))))

(defvar ivy-flx-limit 200
  "Used to conditionally turn off flx sorting.

When the amount of matching candidates exceeds this limit, then
no sorting is done.")

(defvar ivy--recompute-index-inhibit nil
  "When non-nil, `ivy--recompute-index' is a no-op.")

(defun ivy--recompute-index (re-str cands)
  "Recompute index of selected candidate matching RE-STR.
CANDS are the current candidates."
  (let ((caller (ivy-state-caller ivy-last))
        (func (or (ivy-alist-setting ivy-index-functions-alist)
                  #'ivy-recompute-index-zero))
        (case-fold-search (ivy--case-fold-p re-str))
        (preselect (ivy-state-preselect ivy-last))
        (current (ivy-state-current ivy-last))
        (empty (string= re-str "")))
    (unless (or (memq this-command '(ivy-resume ivy-partial-or-done))
                ivy--recompute-index-inhibit)
      (let ((index (cond
                     ((or empty (string= re-str "^"))
                      (ivy--preselect-index preselect cands))
                     ((and (> (length cands) 10000) (eq func #'ivy-recompute-index-zero))
                      0)
                     ((cl-position (string-remove-prefix "^" re-str)
                                   cands
                                   :test #'ivy--case-fold-string=))
                     ((and (ivy--completing-fname-p)
                           (cl-position (concat re-str "/")
                                        cands
                                        :test #'ivy--case-fold-string=)))
                     ((and (eq caller 'ivy-switch-buffer)
                           (not empty))
                      (or (cl-position current cands :test #'string=)
                          0))
                     ((and (not empty)
                           (not (eq caller 'swiper))
                           (not (and ivy--flx-featurep
                                     (eq ivy--regex-function 'ivy--regex-fuzzy)
                                     ;; Limit to configured number of candidates
                                     (null (nthcdr ivy-flx-limit cands))))
                           ;; If there was a preselected candidate, don't try to
                           ;; keep it selected even if the regexp still matches it.
                           ;; See issue #1563.  See also `ivy--preselect-index',
                           ;; which this logic roughly mirrors.
                           (not (or
                                 (and (integerp preselect)
                                      (= ivy--index preselect))
                                 (equal current preselect)
                                 (and (ivy--regex-p preselect)
                                      (stringp current)
                                      (string-match-p preselect current))))
                           ivy--old-cands
                           (cl-position current cands :test #'equal)))
                     ((funcall func re-str cands))
                     (t 0))))
        (ivy-set-index index)))))

(defun ivy-recompute-index-swiper (_re-str cands)
  "Recompute index of selected candidate when using `swiper'.
CANDS are the current candidates."
  (condition-case nil
      (let ((tail (nthcdr ivy--index ivy--old-cands))
            idx)
        (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
            (progn
              (while (and tail (null idx))
                ;; Compare with eq to handle equal duplicates in cands
                (setq idx (cl-position (pop tail) cands)))
              (or
               idx
               (1- (length cands))))
          (if ivy--old-cands
              ivy--index
            ;; already in ivy-state-buffer
            (let ((n (line-number-at-pos))
                  (res 0)
                  (i 0))
              (dolist (c cands)
                (when (eq n (get-text-property 0 'swiper-line-number c))
                  (setq res i))
                (cl-incf i))
              res))))
    (error 0)))

(defun ivy-recompute-index-swiper-backward (re-str cands)
  "Recompute index of selected candidate when using `swiper-backward'.
CANDS are the current candidates."
  (let ((idx (ivy-recompute-index-swiper re-str cands)))
    (if (or (= idx -1)
            (<= (get-text-property 0 'swiper-line-number (nth idx cands))
                (line-number-at-pos)))
        idx
      (- idx 1))))

(defun ivy-recompute-index-swiper-async (_re-str cands)
  "Recompute index of selected candidate when using `swiper' asynchronously.
CANDS are the current candidates."
  (if (null ivy--old-cands)
      (let ((ln (with-ivy-window
                  (line-number-at-pos))))
        (or
         ;; closest to current line going forwards
         (cl-position-if (lambda (x)
                           (>= (string-to-number x) ln))
                         cands)
         ;; closest to current line going backwards
         (1- (length cands))))
    (let ((tail (nthcdr ivy--index ivy--old-cands))
          idx)
      (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
          (progn
            (while (and tail (null idx))
              ;; Compare with `equal', since the collection is re-created
              ;; each time with `split-string'
              (setq idx (cl-position (pop tail) cands :test #'equal)))
            (or idx 0))
        ivy--index))))

(defun ivy-recompute-index-swiper-async-backward (re-str cands)
  "Recompute index of selected candidate when using `swiper-backward'
asynchronously. CANDS are the current candidates."
  (if (= (length cands) 0)
      0
    (let ((idx (ivy-recompute-index-swiper-async re-str cands)))
      (if
          (<= (string-to-number (nth idx cands))
              (with-ivy-window (line-number-at-pos)))
          idx
        (- idx 1)))))

(defun ivy-recompute-index-zero (_re-str _cands)
  "Recompute index of selected candidate.
This function serves as a fallback when nothing else is available."
  0)

(defcustom ivy-minibuffer-faces
  '(ivy-minibuffer-match-face-1
    ivy-minibuffer-match-face-2
    ivy-minibuffer-match-face-3
    ivy-minibuffer-match-face-4)
  "List of `ivy' faces for minibuffer group matches."
  :type '(repeat :tag "Faces"
          (choice
           (const ivy-minibuffer-match-face-1)
           (const ivy-minibuffer-match-face-2)
           (const ivy-minibuffer-match-face-3)
           (const ivy-minibuffer-match-face-4)
           (face :tag "Other face"))))

(defun ivy--minibuffer-face (n)
  "Return Nth face from `ivy-minibuffer-faces'.
N wraps around, but skips the first element of the list."
  (let ((tail (cdr ivy-minibuffer-faces)))
    (nth (mod (+ n 2) (length tail)) tail)))

(defun ivy--flx-propertize (x)
  "X is (cons (flx-score STR ...) STR)."
  (let ((str (copy-sequence (cdr x)))
        (i 0)
        (last-j -2))
    (dolist (j (cdar x))
      (unless (eq j (1+ last-j))
        (cl-incf i))
      (setq last-j j)
      (add-face-text-property j (1+ j) (ivy--minibuffer-face i) nil str))
    str))

(defun ivy--flx-sort (name cands)
  "Sort according to closeness to string NAME the string list CANDS."
  (condition-case nil
      (let* ((bolp (= (string-to-char name) ?^))
             ;; An optimized regex for fuzzy matching
             ;; "abc" → "^[^a]*a[^b]*b[^c]*c"
             (fuzzy-regex (concat "\\`"
                                  (and bolp (regexp-quote (substring name 1 2)))
                                  (mapconcat
                                   (lambda (x)
                                     (setq x (char-to-string x))
                                     (concat "[^" x "]*" (regexp-quote x)))
                                   (if bolp (substring name 2) name)
                                   "")))
             ;; Strip off the leading "^" for flx matching
             (flx-name (if bolp (substring name 1) name))
             cands-left
             cands-to-sort)

        ;; Filter out non-matching candidates
        (dolist (cand cands)
          (when (string-match-p fuzzy-regex cand)
            (push cand cands-left)))

        ;; pre-sort the candidates by length before partitioning
        (setq cands-left (cl-sort cands-left #'< :key #'length))

        ;; partition the candidates into sorted and unsorted groups
        (dotimes (_ (min (length cands-left) ivy-flx-limit))
          (push (pop cands-left) cands-to-sort))

        (nconc
         ;; Compute all of the flx scores in one pass and sort
         (mapcar #'car
                 (sort (mapcar
                        (lambda (cand)
                          (cons cand
                                (car (flx-score cand flx-name ivy--flx-cache))))
                        cands-to-sort)
                       (lambda (c1 c2)
                         ;; Break ties by length
                         (if (/= (cdr c1) (cdr c2))
                             (> (cdr c1)
                                (cdr c2))
                           (< (length (car c1))
                              (length (car c2)))))))

         ;; Add the unsorted candidates
         cands-left))
    (error cands)))

(defun ivy--truncate-string (str width)
  "Truncate STR to WIDTH."
  (truncate-string-to-width str width nil nil t))

(defun ivy--format-function-generic (selected-fn other-fn cands separator)
  "Transform candidates into a string for minibuffer.
SELECTED-FN is called for the selected candidate, OTHER-FN for the others.
Both functions take one string argument each.  CANDS is a list of candidates
and SEPARATOR is used to join them."
  (let ((i -1))
    (mapconcat
     (lambda (str)
       (let ((curr (eq (cl-incf i) ivy--window-index)))
         (if curr
             (funcall selected-fn str)
           (funcall other-fn str))))
     cands
     separator)))

(defun ivy-format-function-default (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face str 'ivy-current-match))
   #'identity
   cands
   "\n"))

(defun ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(defun ivy-format-function-line (cands)
  "Transform CANDS into a string for minibuffer.
Note that since Emacs 27, `ivy-current-match' needs to have :extend t attribute.
It has it by default, but the current theme also needs to set it."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face (concat str "\n") 'ivy-current-match))
   (lambda (str)
     (concat str "\n"))
   cands
   ""))

(defun ivy--highlight-ignore-order (str)
  "Highlight STR, using the ignore-order method."
  (when (consp ivy--old-re)
    (let ((i 1))
      (dolist (re ivy--old-re)
        (when (string-match (car re) str)
          (add-face-text-property
           (match-beginning 0) (match-end 0)
           (ivy--minibuffer-face i)
           nil str))
        (cl-incf i))))
  str)

(defun ivy--highlight-fuzzy (str)
  "Highlight STR, using the fuzzy method."
  (if (and ivy--flx-featurep
           (eq (ivy-alist-setting ivy-re-builders-alist) 'ivy--regex-fuzzy))
      (let ((flx-name (string-remove-prefix "^" ivy-text)))
        (ivy--flx-propertize
         (cons (flx-score str flx-name ivy--flx-cache) str)))
    (ivy--highlight-default str)))

(defcustom ivy-use-group-face-if-no-groups t
  "If t, and the expression has no subgroups, highlight whole match as a group.

It will then use the second face (first of the \"group\" faces)
of `ivy-minibuffer-faces'.  Otherwise, always use the first face
in this case."
  :type 'boolean)

(defun ivy--highlight-default (str)
  "Highlight STR, using the default method."
  (unless ivy--old-re
    (setq ivy--old-re ivy-regex))
  (let ((regexps
         (if (listp ivy--old-re)
             (mapcar #'car (cl-remove-if-not #'cdr ivy--old-re))
           (list ivy--old-re)))
        start)
    (dolist (re regexps)
      (ignore-errors
        (while (and (string-match re str start)
                    (> (- (match-end 0) (match-beginning 0)) 0))
          (setq start (match-end 0))
          (let ((i 0)
                (n 0)
                prev)
            (while (<= i ivy--subexps)
              (let ((beg (match-beginning i))
                    (end (match-end i)))
                (when (and beg end)
                  (unless (or (and prev (= prev beg))
                              (zerop i))
                    (cl-incf n))
                  (let ((face
                         (cond ((and ivy-use-group-face-if-no-groups
                                     (zerop ivy--subexps))
                                (cadr ivy-minibuffer-faces))
                               ((zerop i)
                                (car ivy-minibuffer-faces))
                               (t
                                (ivy--minibuffer-face n)))))
                    (add-face-text-property beg end face nil str))
                  (unless (zerop i)
                    (setq prev end))))
              (cl-incf i)))))))
  str)

(defun ivy--format-minibuffer-line (str)
  "Format line STR for use in minibuffer."
  (let* ((str (ivy-cleanup-string (copy-sequence str)))
         (str (if (eq ivy-display-style 'fancy)
                  (if (memq (ivy-state-caller ivy-last)
                            ivy-highlight-grep-commands)
                      (let* ((start (if (string-match "\\`[^:]+:\\(?:[^:]+:\\)?" str)
                                        (match-end 0) 0))
                             (file (substring str 0 start))
                             (match (substring str start)))
                        (concat file (funcall ivy--highlight-function match)))
                    (funcall ivy--highlight-function str))
                str))
         (olen (length str))
         (annot (plist-get completion-extra-properties :annotation-function)))
    (add-text-properties
     0 olen
     '(mouse-face
       ivy-minibuffer-match-highlight
       help-echo
       (format
        (if tooltip-mode
            "mouse-1: %s\nmouse-3: %s"
          "mouse-1: %s   mouse-3: %s")
        ivy-mouse-1-tooltip ivy-mouse-3-tooltip))
     str)
    (when annot
      (setq str (concat str (funcall annot str)))
      (add-face-text-property
       olen (length str) 'ivy-completions-annotations nil str))
    str))

(defun ivy-read-file-transformer (str)
  "Transform candidate STR when reading files."
  (if (ivy--dirname-p str)
      (propertize str 'face 'ivy-subdir)
    str))

(defun ivy--minibuffer-index-bounds (idx len wnd-len)
  (let* ((half-height (/ wnd-len 2))
         (start (max 0
                     (min (- idx half-height)
                          (- len (1- wnd-len)))))
         (end (min (+ start (1- wnd-len)) len)))
    (list start end (- idx start))))

(defun ivy--format (cands)
  "Return a string for CANDS suitable for display in the minibuffer.
CANDS is a list of candidates that :display-transformer can turn into strings."
  (setq ivy--length (length cands))
  (when (>= ivy--index ivy--length)
    (ivy-set-index (max (1- ivy--length) 0)))
  (if (null cands)
      (setf (ivy-state-current ivy-last) "")
    (let ((cur (nth ivy--index cands)))
      (setf (ivy-state-current ivy-last) (if (stringp cur)
                                             (copy-sequence cur)
                                           cur)))
    (let* ((bnd (ivy--minibuffer-index-bounds
                 ivy--index ivy--length ivy-height))
           (wnd-cands (cl-subseq cands (car bnd) (cadr bnd)))
           (case-fold-search (ivy--case-fold-p ivy-text))
           transformer-fn)
      (setq ivy--window-index (nth 2 bnd))
      (when (setq transformer-fn (ivy-state-display-transformer-fn ivy-last))
        (with-ivy-window
          (with-current-buffer (ivy-state-buffer ivy-last)
            (setq wnd-cands (mapcar transformer-fn wnd-cands)))))
      (ivy--wnd-cands-to-str wnd-cands))))

(defun ivy--wnd-cands-to-str (wnd-cands)
  (let ((str (concat "\n"
                     (funcall (ivy-alist-setting ivy-format-functions-alist)
                              (condition-case nil
                                  (mapcar
                                   #'ivy--format-minibuffer-line
                                   wnd-cands)
                                (error wnd-cands))))))
    (put-text-property 0 (length str) 'read-only nil str)
    str))

(defvar recentf-list)
(defvar bookmark-alist)

(defcustom ivy-virtual-abbreviate 'name
  "The mode of abbreviation for virtual buffer names."
  :type '(choice
          (const :tag "Only name" name)
          (const :tag "Abbreviated path" abbreviate)
          (const :tag "Full path" full)
          ;; eventually, uniquify
          ))
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function bookmark-get-filename "bookmark")

(defun ivy--virtual-buffers ()
  "Adapted from `ido-add-virtual-buffers-to-list'."
  (require 'bookmark)
  (unless recentf-mode
    (recentf-mode 1))
  (bookmark-maybe-load-default-file)
  (let* ((vb-bkm (delete "   - no file -"
                         (delq nil (mapcar #'bookmark-get-filename
                                           bookmark-alist))))
         (vb-list (cond ((eq ivy-use-virtual-buffers 'recentf)
                         recentf-list)
                        ((eq ivy-use-virtual-buffers 'bookmarks)
                         vb-bkm)
                        (ivy-use-virtual-buffers
                         (append recentf-list vb-bkm))
                        (t nil)))
         virtual-buffers)
    (dolist (head vb-list)
      (let* ((file-name (if (stringp head)
                            head
                          (cdr head)))
             (name (cond ((eq ivy-virtual-abbreviate 'name)
                          (file-name-nondirectory file-name))
                         ((eq ivy-virtual-abbreviate 'abbreviate)
                          (abbreviate-file-name file-name))
                         (t
                          (expand-file-name file-name)))))
        (when (equal name "")
          (setq name
                (if (consp head)
                    (car head)
                  (file-name-nondirectory (directory-file-name file-name)))))
        (unless (or (equal name "")
                    (get-file-buffer file-name)
                    (assoc name virtual-buffers))
          (push (cons (copy-sequence name) file-name) virtual-buffers))))
    (when virtual-buffers
      (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
                           'face 'ivy-virtual
                           (car comp)))
      (setq ivy--virtual-buffers (nreverse virtual-buffers))
      (mapcar #'car ivy--virtual-buffers))))

(defcustom ivy-ignore-buffers '("\\` " "\\`\\*tramp/")
  "List of regexps or functions matching buffer names to ignore."
  :type '(repeat (choice regexp function)))

(defvar ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)
                                        (org-mode . ivy-org))
  "Store face customizations for `ivy-switch-buffer'.
Each KEY is `major-mode', each VALUE is a face name.")

(defun ivy--buffer-list (str &optional virtual predicate)
  "Return the buffers that match STR.
If VIRTUAL is non-nil, add virtual buffers.
If optional argument PREDICATE is non-nil, use it to test each
possible match.  See `all-completions' for further information."
  (delete-dups
   (nconc
    (all-completions str #'internal-complete-buffer predicate)
    (and virtual
         (ivy--virtual-buffers)))))

(defvar ivy-views (and nil
                       `(("ivy + *scratch* {}"
                          (vert
                           (file ,(expand-file-name "ivy.el"))
                           (buffer "*scratch*")))
                         ("swiper + *scratch* {}"
                          (horz
                           (file ,(expand-file-name "swiper.el"))
                           (buffer "*scratch*")))))
  "Store window configurations selectable by `ivy-switch-buffer'.

The default value is given as an example.

Each element is a list of (NAME VIEW). NAME is a string, it's
recommended to end it with a distinctive snippet e.g. \"{}\" so
that it's easy to distinguish the window configurations.

VIEW is either a TREE or a window-configuration (see
`ivy--get-view-config').

TREE is a nested list with the following valid cars:
- vert: split the window vertically
- horz: split the window horizontally
- file: open the specified file
- buffer: open the specified buffer

TREE can be nested multiple times to have multiple window splits.")

(defun ivy-default-view-name ()
  "Return default name for new view."
  (let* ((default-view-name
          (concat "{} "
                  (mapconcat #'identity
                             (sort
                              (mapcar (lambda (w)
                                        (let* ((b (window-buffer w))
                                               (f (buffer-file-name b)))
                                          (if f
                                              (file-name-nondirectory f)
                                            (buffer-name b))))
                                      (window-list))
                              #'string-lessp)
                             " ")))
         (view-name-re (concat "\\`"
                               (regexp-quote default-view-name)
                               " \\([0-9]+\\)"))
         old-view)
    (cond ((setq old-view
                 (cl-find-if
                  (lambda (x)
                    (string-match view-name-re (car x)))
                  ivy-views))
           (format "%s %d"
                   default-view-name
                   (1+ (string-to-number
                        (match-string 1 (car old-view))))))
          ((assoc default-view-name ivy-views)
           (concat default-view-name " 1"))
          (t
           default-view-name))))

(defun ivy--get-view-config ()
  "Get `current-window-configuration' for `ivy-views'."
  (dolist (w (window-list))
    (set-window-parameter w 'ivy-view-data
                          (with-current-buffer (window-buffer w)
                            (cond (buffer-file-name
                                   (list 'file buffer-file-name (point)))
                                  ((eq major-mode 'dired-mode)
                                   (list 'file default-directory (point)))
                                  (t
                                   (list 'buffer (buffer-name) (point)))))))
  (let ((window-persistent-parameters
         (append window-persistent-parameters
                 (list (cons 'ivy-view-data t)))))
    (current-window-configuration)))

(defun ivy-push-view (&optional arg)
  "Push the current window tree on `ivy-views'.

When ARG is non-nil, replace a selected item on `ivy-views'.

Currently, the split configuration (i.e. horizontal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
  (interactive "P")
  (let* ((view (ivy--get-view-config))
         (view-name
          (if arg
              (ivy-read "Update view: " ivy-views)
            (ivy-read "Name view: " nil
                      :initial-input (ivy-default-view-name)))))
    (when view-name
      (let ((x (assoc view-name ivy-views)))
        (if x
            (setcdr x (list view))
          (push (list view-name view) ivy-views))))))

(defun ivy-pop-view-action (view)
  "Delete VIEW from `ivy-views'."
  (setq ivy-views (delete view ivy-views))
  (setq ivy--all-candidates
        (delete (car view) ivy--all-candidates))
  (setq ivy--old-cands nil))

(defun ivy-pop-view ()
  "Delete a view to delete from `ivy-views'."
  (interactive)
  (ivy-read "Pop view: " ivy-views
            :preselect (caar ivy-views)
            :action #'ivy-pop-view-action
            :caller 'ivy-pop-view))

(defun ivy-source-views ()
  "Return the name of the views saved in `ivy-views'."
  (mapcar #'car ivy-views))

(ivy-set-sources
 'ivy-switch-buffer
 '((original-source)
   (ivy-source-views)))

(defun ivy-set-view-recur (view)
  "Set VIEW recursively."
  (cond  ((window-configuration-p view)
          (set-window-configuration view)
          (dolist (w (window-list))
            (with-selected-window w
              (ivy-set-view-recur
               (window-parameter w 'ivy-view-data)))))
         ((eq (car view) 'vert)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-vertically))
                (views (cdr view))
                (v (pop views))
                (temp-wnd))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (when views
                 (setq temp-wnd (split-window-vertically)))
               (ivy-set-view-recur v)
               (when views
                 (setq wnd2 temp-wnd))))))
        ((eq (car view) 'horz)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-horizontally))
                (views (cdr view))
                (v (pop views))
                (temp-wnd))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (when views
                 (setq temp-wnd (split-window-horizontally)))
               (ivy-set-view-recur v)
               (when views
                 (setq wnd2 temp-wnd))))))
        ((eq (car view) 'file)
         (let* ((name (nth 1 view))
                (virtual (assoc name ivy--virtual-buffers))
                buffer)
           (cond ((setq buffer (get-buffer name))
                  (switch-to-buffer buffer nil 'force-same-window))
                 (virtual
                  (find-file (cdr virtual)))
                 ((file-exists-p name)
                  (find-file name))))
         (when (and (> (length view) 2)
                    (numberp (nth 2 view)))
           (goto-char (nth 2 view))))
        ((eq (car view) 'buffer)
         (switch-to-buffer (nth 1 view))
         (when (and (> (length view) 2)
                    (numberp (nth 2 view)))
           (goto-char (nth 2 view))))
        ((eq (car view) 'sexp)
         (eval (nth 1 view)))))

(defun ivy--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer
       ivy-text nil 'force-same-window)
    (let ((virtual (assoc buffer ivy--virtual-buffers))
          (view (assoc buffer ivy-views)))
      (cond ((and virtual
                  (not (get-buffer buffer)))
             (find-file (cdr virtual)))
            (view
             (delete-other-windows)
             (let (
                   ;; silence "Directory has changed on disk"
                   (inhibit-message t))
               (ivy-set-view-recur (cadr view))))
            (t
             (switch-to-buffer
              buffer nil 'force-same-window))))))

(defun ivy--switch-buffer-other-window-action (buffer)
  "Switch to BUFFER in other window.
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer-other-window ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (find-file-other-window (cdr virtual))
        (switch-to-buffer-other-window buffer)))))

(defun ivy--rename-buffer-action (buffer)
  "Rename BUFFER."
  (let ((new-name (read-string "Rename buffer (to new name): ")))
    (with-current-buffer buffer
      (rename-buffer new-name))))

(defun ivy--find-file-action (buffer)
  "Find file from BUFFER's directory."
  (let* ((virtual (assoc buffer ivy--virtual-buffers))
         (default-directory (if virtual
                                (file-name-directory (cdr virtual))
                              (buffer-local-value 'default-directory
                                                  (or (get-buffer buffer)
                                                      (current-buffer))))))
    (call-interactively (if (functionp 'counsel-find-file)
                            #'counsel-find-file
                          #'find-file))))

(defun ivy--kill-buffer-or-virtual (buffer)
  (if (get-buffer buffer)
      (kill-buffer buffer)
    (setq recentf-list (delete
                        (cdr (assoc buffer ivy--virtual-buffers))
                        recentf-list))))

(defun ivy--kill-current-candidate ()
  (setf (ivy-state-preselect ivy-last) ivy--index)
  (setq ivy--old-re nil)
  (setq ivy--all-candidates (delete (ivy-state-current ivy-last) ivy--all-candidates))
  (let ((ivy--recompute-index-inhibit t))
    (ivy--exhibit)))

(defun ivy--kill-current-candidate-buffer ()
  (setf (ivy-state-preselect ivy-last) ivy--index)
  (setq ivy--old-re nil)
  (setq ivy--all-candidates (ivy--buffer-list "" ivy-use-virtual-buffers nil))
  (let ((ivy--recompute-index-inhibit t))
    (ivy--exhibit)))

(defun ivy--kill-buffer-action (buffer)
  "Kill BUFFER."
  (ivy--kill-buffer-or-virtual buffer)
  (unless (buffer-live-p (ivy-state-buffer ivy-last))
    (setf (ivy-state-buffer ivy-last)
          (with-ivy-window (current-buffer))))
  (ivy--kill-current-candidate-buffer))

(defvar ivy-switch-buffer-map
  (let ((map (make-sparse-keymap)))
    (ivy-define-key map (kbd "C-k") 'ivy-switch-buffer-kill)
    map))

(defun ivy-switch-buffer-kill ()
  "When at end-of-line, kill the current buffer in `ivy-switch-buffer'.
Otherwise, forward to `ivy-kill-line'."
  (interactive)
  (if (not (eolp))
      (ivy-kill-line)
    (ivy--kill-buffer-action
     (ivy-state-current ivy-last))))

(ivy-set-actions
 'ivy-switch-buffer
 '(("f"
    ivy--find-file-action
    "find file")
   ("j"
    ivy--switch-buffer-other-window-action
    "other window")
   ("k"
    ivy--kill-buffer-action
    "kill")
   ("r"
    ivy--rename-buffer-action
    "rename")))

(ivy-set-actions
 t
 '(("i" ivy--action-insert "insert")
   ("w" ivy--action-copy "copy")))

(defun ivy--trim-grep-line-number (x)
  (if (string-match ":[0-9]+:" x)
      (substring x (match-end 0))
    x))

(defun ivy--action-insert (x)
  (insert
   (if (stringp x)
       (ivy--trim-grep-line-number x)
       x (car x))))

(defun ivy--action-copy (x)
  (kill-new
   (if (stringp x)
       (ivy--trim-grep-line-number x)
     (car x))))

(defun ivy--switch-buffer-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
  (if (string-match-p "^:" ivy-text)
      (delete-dups
       (cl-remove-if-not
        (lambda (s)
          (let ((b (get-buffer s)))
            (and b
                 (string-match-p regexp (buffer-local-value 'default-directory b))
                 (not (string-match-p "^\\*" s)))))
        candidates))
    (let ((res (ivy--re-filter regexp candidates)))
      (if (or (null ivy-use-ignore)
              (null ivy-ignore-buffers))
          res
        (or (cl-remove-if
             (lambda (buf)
               (cl-find-if
                (lambda (f-or-r)
                  (if (functionp f-or-r)
                      (funcall f-or-r buf)
                    (string-match-p f-or-r buf)))
                ivy-ignore-buffers))
             res)
            (and (eq ivy-use-ignore t)
                 res))))))

(defun ivy-append-face (str face)
  "Append to STR the property FACE."
  (when face
    (setq str (copy-sequence str))
    (add-face-text-property 0 (length str) face t str))
  str)

(defun ivy--remote-buffer-p (buffer)
  "Return non-nil if BUFFER object is visiting a remote file.
If that is the case, value is a string identifying the remote
connection."
  (let ((dir (buffer-local-value 'default-directory buffer)))
    (ignore-errors (file-remote-p dir))))

(defun ivy-switch-buffer-transformer (str)
  "Transform candidate STR when switching buffers."
  (let ((buf (get-buffer str)))
    (cond ((not buf) str)
          ((let ((remote (ivy--remote-buffer-p buf)))
             (when remote
               (format "%s (%s)" (ivy-append-face str 'ivy-remote) remote))))
          ((not (verify-visited-file-modtime buf))
           (ivy-append-face str 'ivy-modified-outside-buffer))
          ((buffer-modified-p buf)
           (ivy-append-face str 'ivy-modified-buffer))
          (t
           (let* ((mode (buffer-local-value 'major-mode buf))
                  (face (cdr (assq mode ivy-switch-buffer-faces-alist))))
             (ivy-append-face str face))))))

(defun ivy-switch-buffer-occur (cands)
  "Occur function for `ivy-switch-buffer' using `ibuffer'.
CANDS are the candidates to be displayed."
  (unless cands
    (setq cands (all-completions ivy-text #'internal-complete-buffer)))
  (ibuffer
   nil (buffer-name)
   `((or ,@(cl-mapcan
            (lambda (cand)
              (unless (eq (get-text-property 0 'face cand) 'ivy-virtual)
                `((name . ,(format "\\_<%s\\_>" (regexp-quote cand))))))
            cands)))))

;;;###autoload
(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

(ivy-configure 'ivy-switch-buffer
  :parent 'internal-complete-buffer
  :occur #'ivy-switch-buffer-occur)

;;;###autoload
(defun ivy-switch-view ()
  "Switch to one of the window views stored by `ivy-push-view'."
  (interactive)
  (let ((ivy-initial-inputs-alist
         '((ivy-switch-buffer . "{}"))))
    (ivy-switch-buffer)))

;;;###autoload
(defun ivy-switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (ivy-read "Switch to buffer in other window: " #'internal-complete-buffer
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-other-window-action
            :keymap ivy-switch-buffer-map
            :caller 'ivy-switch-buffer-other-window))

(ivy-configure 'ivy-switch-buffer-other-window
  :parent 'ivy-switch-buffer)

(defun ivy--yank-handle-case-fold (text)
  (if (and (> (length ivy-text) 0)
           (string= (downcase ivy-text) ivy-text))
      (downcase text)
    text))

(defun ivy--yank-by (fn &rest args)
  "Pull buffer text from current line into search string.
The region to extract is determined by the respective values of
point before and after applying FN to ARGS."
  (let (text)
    (with-ivy-window
      (let ((beg (point))
            (bol (line-beginning-position))
            (eol (line-end-position))
            end)
        (unwind-protect
             (progn (apply fn args)
                    (setq end (goto-char (max bol (min (point) eol))))
                    (setq text (buffer-substring-no-properties beg end))
                    (ivy--pulse-region beg end))
          (unless text
            (goto-char beg)))))
    (when text
      (insert (replace-regexp-in-string
               "  +" " "
               (ivy--yank-handle-case-fold text)
               t t)))))

(defun ivy-yank-word (&optional arg)
  "Pull next word from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
words (previous if ARG is negative)."
  (interactive "p")
  (ivy--yank-by #'forward-word arg))

(defun ivy-yank-symbol (&optional arg)
  "Pull next symbol from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
symbols (previous if ARG is negative)."
  (interactive "p")
  (ivy--yank-by #'forward-symbol (or arg 1)))

(defun ivy-yank-char (&optional arg)
  "Pull next character from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
characters (previous if ARG is negative)."
  (interactive "p")
  (ivy--yank-by #'forward-char arg))

(defvar ivy--pulse-overlay nil
  "Overlay used to highlight yanked word.")

(defvar ivy--pulse-timer nil
  "Timer used to dispose of `ivy--pulse-overlay'.")

(defcustom ivy-pulse-delay 0.5
  "Number of seconds to display `ivy-yanked-word' highlight.
When nil, disable highlighting."
  :type '(choice
          (number :tag "Delay in seconds")
          (const :tag "Disable" nil)))

(defun ivy--pulse-region (start end)
  "Temporarily highlight text between START and END.
The \"pulse\" duration is determined by `ivy-pulse-delay'."
  (when ivy-pulse-delay
    (if ivy--pulse-overlay
        (let ((ostart (overlay-start ivy--pulse-overlay))
              (oend (overlay-end ivy--pulse-overlay)))
          (when (< end start)
            (cl-rotatef start end))
          ;; Extend the existing overlay's region to include START..END,
          ;; but only if the two regions are contiguous.
          (move-overlay ivy--pulse-overlay
                        (if (= start oend) ostart start)
                        (if (= end ostart) oend end)))
      (setq ivy--pulse-overlay (make-overlay start end))
      (overlay-put ivy--pulse-overlay 'face 'ivy-yanked-word))
    (when ivy--pulse-timer
      (cancel-timer ivy--pulse-timer))
    (setq ivy--pulse-timer
          (run-at-time ivy-pulse-delay nil #'ivy--pulse-cleanup))))

(defun ivy--pulse-cleanup ()
  "Cancel `ivy--pulse-timer' and delete `ivy--pulse-overlay'."
  (when ivy--pulse-timer
    (cancel-timer ivy--pulse-timer)
    (setq ivy--pulse-timer nil))
  (when ivy--pulse-overlay
    (delete-overlay ivy--pulse-overlay)
    (setq ivy--pulse-overlay nil)))

(defun ivy-kill-ring-save ()
  "Store the current candidates into the kill ring.
If the region is active, forward to `kill-ring-save' instead."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (kill-new
     (mapconcat
      #'identity
      ivy--old-cands
      "\n"))))

(defun ivy-insert-current ()
  "Make the current candidate into current input.
Don't finish completion."
  (interactive)
  (delete-minibuffer-contents)
  (let ((end (and ivy--directory
                  (ivy--dirname-p (ivy-state-current ivy-last))
                  -1)))
    (insert (substring-no-properties
             (ivy-state-current ivy-last) 0 end))))

(defun ivy-insert-current-full ()
  "Insert the full Yank the current directory into the minibuffer."
  (interactive)
  (insert ivy--directory))

(defcustom ivy-preferred-re-builders
  '((ivy--regex-plus . "ivy")
    (ivy--regex-ignore-order . "order")
    (ivy--regex-fuzzy . "fuzzy"))
  "Alist of preferred re-builders with display names.
This list can be rotated with `ivy-rotate-preferred-builders'."
  :type '(alist :key-type function :value-type string))

(defun ivy-rotate-preferred-builders ()
  "Switch to the next re builder in `ivy-preferred-re-builders'."
  (interactive)
  (when ivy-preferred-re-builders
    (setq ivy--old-re nil)
    (setq ivy--regex-function
          (let ((cell (assq ivy--regex-function ivy-preferred-re-builders)))
            (car (or (cadr (memq cell ivy-preferred-re-builders))
                     (car ivy-preferred-re-builders)))))))

(defun ivy-toggle-fuzzy ()
  "Toggle the re builder between `ivy--regex-fuzzy' and `ivy--regex-plus'."
  (interactive)
  (setq ivy--old-re nil)
  (if (eq ivy--regex-function 'ivy--regex-fuzzy)
      (setq ivy--regex-function 'ivy--regex-plus)
    (setq ivy--regex-function 'ivy--regex-fuzzy)))

(defun ivy--label-and-delete-dups (entries)
  "Label ENTRIES with history indices."
  (let ((ht (make-hash-table :test 'equal))
        (idx 0)
        entry
        accum)
    (while (setq entry (pop entries))
      (unless (gethash entry ht)
        (puthash entry t ht)
        (push `(,entry . ,idx) accum))
      (cl-incf idx))
    (nreverse accum)))

(defvar ivy--reverse-i-search-symbol nil
  "Store the history symbol.")

(defun ivy-reverse-i-search-kill ()
  "Remove the current item from history"
  (interactive)
  (if (not (eolp))
      (ivy-kill-line)
    (let ((current (ivy-state-current ivy-last)))
      (if (symbolp ivy--reverse-i-search-symbol)
          (set
           ivy--reverse-i-search-symbol
           (delete current (symbol-value ivy--reverse-i-search-symbol)))
        (ring-remove
         ivy--reverse-i-search-symbol
         (ring-member ivy--reverse-i-search-symbol (ivy-state-current ivy-last)))))
    (ivy--kill-current-candidate)))

(defvar ivy-reverse-i-search-map
  (let ((map (make-sparse-keymap)))
    (ivy-define-key map (kbd "C-k") 'ivy-reverse-i-search-kill)
    map))

(defun ivy-history-contents (history)
  "Copy contents of HISTORY.
A copy is necessary so that we don't clobber any string attributes.
Also set `ivy--reverse-i-search-symbol' to HISTORY."
  (setq ivy--reverse-i-search-symbol history)
  (cond ((symbolp history)
         (ivy--label-and-delete-dups
          (copy-sequence (symbol-value history))))
        ((ring-p history)
         (ivy--label-and-delete-dups
          (when (> (ring-size history) 0)
            (ring-elements history))))
        ((sequencep history)
         (ivy--label-and-delete-dups
          (copy-sequence history)))
        (t
         (error "Expected a symbol, ring, or sequence: %S" history))))

(defun ivy-reverse-i-search ()
  "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibuffer.
\\<ivy-reverse-i-search-map>
You can also delete an element from history with \\[ivy-reverse-i-search-kill]."
  (interactive)
  (cond
    ((= (minibuffer-depth) 0)
     (user-error
      "This command is intended to be called from within `ivy-read'"))
    ;; don't recur
    ((and (> (minibuffer-depth) 1)
          (eq (ivy-state-caller ivy-last) 'ivy-reverse-i-search)))
    (t
     (let ((enable-recursive-minibuffers t)
           (old-last ivy-last))
       (ivy-read "Reverse-i-search: "
                 (ivy-history-contents (ivy-state-history ivy-last))
                 :keymap ivy-reverse-i-search-map
                 :action (lambda (x)
                           (ivy--reset-state
                            (setq ivy-last old-last))
                           (delete-minibuffer-contents)
                           (insert (substring-no-properties (car x)))
                           (ivy--cd-maybe))
                 :caller 'ivy-reverse-i-search)))))

(defun ivy-restrict-to-matches ()
  "Restrict candidates to current input and erase input."
  (interactive)
  (delete-minibuffer-contents)
  (if (ivy-state-dynamic-collection ivy-last)
      (progn
        (setf (ivy-state-dynamic-collection ivy-last) nil)
        (setf (ivy-state-collection ivy-last)
              (setq ivy--all-candidates ivy--old-cands)))
    (setq ivy--all-candidates
          (ivy--filter ivy-text ivy--all-candidates))))

;;* Occur
(defvar-local ivy-occur-last nil
  "Buffer-local value of `ivy-last'.
Can't re-use `ivy-last' because using e.g. `swiper' in the same
buffer would modify `ivy-last'.")

(defvar ivy-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (ivy-define-key map [mouse-1] 'ivy-occur-click)
    (ivy-define-key map (kbd "RET") 'ivy-occur-press-and-switch)
    (ivy-define-key map (kbd "j") 'ivy-occur-next-line)
    (ivy-define-key map (kbd "k") 'ivy-occur-previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (ivy-define-key map (kbd "f") 'ivy-occur-press)
    (ivy-define-key map (kbd "g") 'ivy-occur-revert-buffer)
    (ivy-define-key map (kbd "a") 'ivy-occur-read-action)
    (ivy-define-key map (kbd "o") 'ivy-occur-dispatch)
    (ivy-define-key map (kbd "c") 'ivy-occur-toggle-calling)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "R") 'read-only-mode)
    (ivy-define-key map (kbd "C-d") 'ivy-occur-delete-candidate)
    map)
  "Keymap for Ivy Occur mode.")

(defun ivy-occur-toggle-calling ()
  "Toggle `ivy-calling'."
  (interactive)
  (if (setq ivy-calling (not ivy-calling))
      (progn
        (setq mode-name "Ivy-Occur [calling]")
        (ivy-occur-press))
    (setq mode-name "Ivy-Occur"))
  (force-mode-line-update))

(defun ivy--find-occur-buffer ()
  (let ((cb (current-buffer)))
    (cl-find-if
     (lambda (b)
       (with-current-buffer b
         (and (eq major-mode 'ivy-occur-grep-mode)
              (equal cb (ivy-state-buffer ivy-occur-last)))))
     (buffer-list))))

(defun ivy--select-occur-buffer ()
  (let* ((ob (ivy--find-occur-buffer))
         (ow (cl-find-if (lambda (w) (equal ob (window-buffer w)))
                         (window-list))))
    (if ow
        (select-window ow)
      (pop-to-buffer ob))))

(defun ivy-occur-next-line (&optional arg)
  "Move the cursor down ARG lines.
When `ivy-calling' isn't nil, call `ivy-occur-press'."
  (interactive "p")
  (let ((offset (cond ((derived-mode-p 'ivy-occur-grep-mode) 5)
                      ((derived-mode-p 'ivy-occur-mode) 2))))
    (if offset
        (progn
          (if (< (line-number-at-pos) offset)
              (progn
                (goto-char (point-min))
                (forward-line (1- offset)))
            (forward-line arg)
            (when (eolp)
              (forward-line -1)))
          (when ivy-calling
            (ivy-occur-press)))
      (ivy--select-occur-buffer)
      (ivy-occur-next-line arg)
      (ivy-occur-press-and-switch))))

(defun ivy-occur-previous-line (&optional arg)
  "Move the cursor up ARG lines.
When `ivy-calling' isn't nil, call `ivy-occur-press'."
  (interactive "p")
  (let ((offset (cond ((derived-mode-p 'ivy-occur-grep-mode) 5)
                      ((derived-mode-p 'ivy-occur-mode) 2))))
    (if offset
        (progn
          (forward-line (- arg))
          (when (< (line-number-at-pos) offset)
            (goto-char (point-min))
            (forward-line (1- offset)))
          (when ivy-calling
            (ivy-occur-press)))
      (ivy--select-occur-buffer)
      (ivy-occur-previous-line arg)
      (ivy-occur-press-and-switch))))

(defun ivy-occur-next-error (n &optional reset)
  "A `next-error-function' for `ivy-occur-mode'."
  (interactive "p")
  (when reset
    (goto-char (point-min)))
  (setq n (or n 1))
  (let ((ivy-calling t))
    (cond ((< n 0) (ivy-occur-previous-line (- n)))
          (t (ivy-occur-next-line n))))
  ;; The window's point overrides the buffer's point every time it's redisplayed
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-point window (point))))

(define-derived-mode ivy-occur-mode fundamental-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-mode-map}"
  (setq-local view-read-only nil))

(defvar ivy-occur-grep-mode-map
  (let ((map (copy-keymap ivy-occur-mode-map)))
    (ivy-define-key map (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode)
    (ivy-define-key map "w" 'ivy-wgrep-change-to-wgrep-mode)
    map)
  "Keymap for Ivy Occur Grep mode.")

(defun ivy-occur-delete-candidate ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position)
                   (1+ (line-end-position)))))

(define-derived-mode ivy-occur-grep-mode grep-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-grep-mode-map}"
  (setq-local view-read-only nil)
  (when (fboundp 'wgrep-setup)
    (wgrep-setup)))

(defun ivy--starts-with-dotslash (str)
  (string-match-p "\\`\\.[/\\]" str))

(defun ivy--occur-insert-lines (cands)
  "Insert CANDS into `ivy-occur' buffer."
  (font-lock-mode -1)
  (dolist (cand cands)
    (setq cand
          (if (string-match "\\`\\(.*:[0-9]+:\\)\\(.*\\)\\'" cand)
              (let ((file-and-line (match-string 1 cand))
                    (grep-line (match-string 2 cand)))
                (concat
                 (propertize file-and-line 'face 'ivy-grep-info)
                 (ivy--highlight-fuzzy grep-line)))
            (ivy--highlight-fuzzy (copy-sequence cand))))
    (add-text-properties
     0 (length cand)
     '(mouse-face
       highlight
       help-echo "mouse-1: call ivy-action")
     cand)
    (insert (if (ivy--starts-with-dotslash cand) "" "    ")
            cand ?\n)))

(defun ivy--occur-default (cands)
  "Insert CANDS into the current occur buffer."
  (unless cands
    (let ((coll (ivy-state-collection ivy-last)))
      (when (arrayp coll)
        (setq coll (all-completions "" coll (ivy-state-predicate ivy-last))))
      (setq cands (ivy--filter (ivy-state-text ivy-last) coll))))
  (ivy-occur-mode)
  (insert (format "%d candidates:\n" (length cands)))
  (ivy--occur-insert-lines cands)
  (read-only-mode))

(defun ivy-occur ()
  "Stop completion and put the current candidates into a new buffer.

The new buffer remembers current action(s).

While in the *ivy-occur* buffer, selecting a candidate with RET or
a mouse click will call the appropriate action for that candidate.

There is no limit on the number of *ivy-occur* buffers."
  (interactive)
  (if (not (window-minibuffer-p))
      (user-error "No completion session is active")
    (let* ((caller (ivy-state-caller ivy-last))
           (occur-fn (or (plist-get ivy--occurs-list caller)
                         #'ivy--occur-default))
           (buffer
            (generate-new-buffer
             (format "*ivy-occur%s \"%s\"*"
                     (if caller
                         (concat " " (prin1-to-string caller))
                       "")
                     ivy-text))))
      (with-current-buffer buffer
        (funcall occur-fn ivy--old-cands)
        (setf (ivy-state-text ivy-last) ivy-text)
        (setq ivy-occur-last ivy-last))
      (ivy-exit-with-action
       (lambda (_)
         (pop-to-buffer buffer)
         (setq next-error-last-buffer buffer)
         (setq-local next-error-function #'ivy-occur-next-error))))))

(defun ivy-occur-revert-buffer ()
  "Refresh the buffer making it up-to date with the collection.

Currently only works for `swiper'.  In that specific case, the
*ivy-occur* buffer becomes nearly useless as the original buffer
is updated, since the line numbers no longer match.

Calling this function is as if you called `ivy-occur' on the
updated original buffer."
  (interactive)
  (let ((caller (ivy-state-caller ivy-occur-last))
        (ivy-last ivy-occur-last))
    (let ((inhibit-read-only t)
          (line (line-number-at-pos)))
      (erase-buffer)
      (funcall (or (plist-get ivy--occurs-list caller)
                   #'ivy--occur-default) nil)
      (goto-char (point-min))
      (forward-line (1- line)))
    (setq ivy-occur-last ivy-last)))

(declare-function wgrep-change-to-wgrep-mode "ext:wgrep")

(defun ivy-wgrep-change-to-wgrep-mode ()
  "Forward to `wgrep-change-to-wgrep-mode'."
  (interactive)
  (if (require 'wgrep nil 'noerror)
      (wgrep-change-to-wgrep-mode)
    (error "Package wgrep isn't installed")))

(defun ivy-occur-read-action ()
  "Select one of the available actions as the current one."
  (interactive)
  (let ((ivy-last ivy-occur-last))
    (ivy-read-action)))

(defun ivy-occur-dispatch ()
  "Call one of the available actions on the current item."
  (interactive)
  (let* ((state-action (ivy-state-action ivy-occur-last))
         (actions (if (symbolp state-action)
                      state-action
                    (copy-sequence state-action))))
    (unwind-protect
         (progn
           (ivy-occur-read-action)
           (ivy-occur-press))
      (setf (ivy-state-action ivy-occur-last) actions))))

(defun ivy-occur-click (event)
  "Execute action for the current candidate.
EVENT gives the mouse position."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (ivy-occur-press))))

(declare-function swiper--cleanup "swiper")
(declare-function swiper--add-overlays "swiper")
(defvar ivy-occur-timer nil)

(defun ivy--occur-press-update-window ()
  (cond
    ((memq (ivy-state-caller ivy-occur-last)
           (append '(swiper swiper-isearch) ivy-highlight-grep-commands))
     (let ((window (ivy-state-window ivy-occur-last))
           (buffer (ivy-state-buffer ivy-occur-last)))
       (when (buffer-live-p buffer)
         (cond ((or (not (window-live-p window))
                    (equal window (selected-window)))
                (save-selected-window
                  (setf (ivy-state-window ivy-occur-last)
                        (display-buffer buffer))))
               ((not (equal (window-buffer window) buffer))
                (with-selected-window window
                  (switch-to-buffer buffer)))))))

    ((memq (ivy-state-caller ivy-occur-last)
           '(counsel-describe-function
             counsel-describe-variable
             counsel-describe-symbol))
     (setf (ivy-state-window ivy-occur-last)
           (selected-window))
     (selected-window))))

(defun ivy--occur-press-buffer ()
  (let ((buffer (ivy-state-buffer ivy-last)))
    (if (buffer-live-p buffer)
        buffer
      (current-buffer))))

(defun ivy-occur-press ()
  "Execute action for the current candidate."
  (interactive)
  (ivy--occur-press-update-window)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\(?:.[/\\]\\|    \\)\\(.*\\)$"))
    (let* ((ivy-last ivy-occur-last)
           (ivy-text (ivy-state-text ivy-last))
           (str (buffer-substring
                 (match-beginning 1)
                 (match-end 1)))
           (offset (or (get-text-property 0 'offset str) 0))
           (coll (ivy-state-collection ivy-last))
           (action (ivy--get-action ivy-last))
           (ivy-exit 'done))
      (with-ivy-window
        (with-current-buffer (ivy--occur-press-buffer)
          (save-restriction
            (widen)
            (funcall action
                     (if (and (consp coll)
                              (consp (car coll)))
                         (assoc str coll)
                       (substring str offset)))))
        (if (memq (ivy-state-caller ivy-last)
                  (append '(swiper swiper-isearch) ivy-highlight-grep-commands))
            (with-current-buffer (window-buffer (selected-window))
              (swiper--cleanup)
              (swiper--add-overlays
               (ivy--regex ivy-text)
               (line-beginning-position)
               (line-end-position)
               (selected-window))
              (when (timerp ivy-occur-timer)
                (cancel-timer ivy-occur-timer))
              (setq ivy-occur-timer
                    (run-at-time 1.0 nil 'swiper--cleanup))))))))

(defun ivy-occur-press-and-switch ()
  "Execute action for the current candidate and switch window."
  (interactive)
  (ivy-occur-press)
  (select-window (ivy--get-window ivy-occur-last)))

(defun ivy--marked-p ()
  (member (ivy-state-current ivy-last) ivy-marked-candidates))

(defun ivy--unmark (cand)
  (setcar (member cand ivy--all-candidates)
          (setcar (member cand ivy--old-cands)
                  (substring cand (length ivy-mark-prefix))))
  (setq ivy-marked-candidates
        (delete cand ivy-marked-candidates)))

(defun ivy--mark (cand)
  (let ((marked-cand (concat ivy-mark-prefix cand)))
    (setcar (member cand ivy--all-candidates)
            (setcar (member cand ivy--old-cands) marked-cand))
    (setq ivy-marked-candidates
          (append ivy-marked-candidates (list marked-cand)))))

(defun ivy-mark ()
  "Mark the selected candidate and move to the next one.

In `ivy-call', :action will be called in turn for all marked
candidates.

However, if :multi-action was supplied to `ivy-read', then it
will be called with `ivy-marked-candidates'. This way, it can
make decisions based on the whole marked list."
  (interactive)
  (unless (ivy--marked-p)
    (ivy--mark (ivy-state-current ivy-last)))
  (ivy-next-line))

(defun ivy-unmark ()
  "Unmark the selected candidate and move to the next one."
  (interactive)
  (when (ivy--marked-p)
    (ivy--unmark (ivy-state-current ivy-last)))
  (ivy-next-line))

(defun ivy-unmark-backward ()
  "Move to the previous candidate and unmark it."
  (interactive)
  (ivy-previous-line)
  (ivy--exhibit)
  (when (ivy--marked-p)
    (ivy--unmark (ivy-state-current ivy-last))))

(defun ivy-toggle-marks ()
  "Toggle mark for all narrowed candidates."
  (interactive)
  (dolist (cand ivy--old-cands)
    (if (member cand ivy-marked-candidates)
        (ivy--unmark cand)
      (ivy--mark cand))))

(defconst ivy-help-file (let ((default-directory
                               (if load-file-name
                                   (file-name-directory load-file-name)
                                 default-directory)))
                          (if (file-exists-p "ivy-help.org")
                              (expand-file-name "ivy-help.org")
                            (if (file-exists-p "doc/ivy-help.org")
                                (expand-file-name "doc/ivy-help.org"))))
  "The file for `ivy-help'.")

(defvar org-hide-emphasis-markers)

(defun ivy-help ()
  "Help for `ivy'."
  (interactive)
  (let ((buf (get-buffer "*Ivy Help*"))
        (inhibit-read-only t))
    (unless buf
      (setq buf (get-buffer-create "*Ivy Help*"))
      (cl-letf (((symbol-function #'help-buffer) (lambda () buf)))
        (describe-mode))
      (with-current-buffer buf
        (goto-char (point-min))
        (insert "* describe-mode\n")
        (goto-char (point-min))
        (insert-file-contents ivy-help-file)
        (org-mode)
        (setq-local org-hide-emphasis-markers t)
        (view-mode)
        (goto-char (point-min))
        (let ((inhibit-message t))
          (org-cycle '(64)))))
    (if (eq this-command 'ivy-help)
        (switch-to-buffer buf)
      (with-ivy-window
        (pop-to-buffer buf)))
    (view-mode)
    (goto-char (point-min))))

(declare-function ffap-url-p "ffap")
(defvar ffap-url-fetcher)

(defun ivy-ffap-url-p (string)
  "Forward to `ffap-url-p'."
  (require 'ffap)
  (ffap-url-p string))

(defun ivy-ffap-url-fetcher (url)
  "Calls `ffap-url-fetcher'."
  (require 'ffap)
  (funcall ffap-url-fetcher url))

(ivy-configure 'read-file-name-internal
  :sort-fn #'ivy-sort-file-function-default
  :display-transformer-fn #'ivy-read-file-transformer
  :alt-done-fn #'ivy--directory-done)

(ivy-configure 'internal-complete-buffer
  :display-transformer-fn #'ivy-switch-buffer-transformer)

(ivy-configure 'Info-read-node-name-1
  :alt-done-fn #'ivy--info-alt-done)

(provide 'ivy)

;;; ivy.el ends here
