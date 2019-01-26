;;; ivy.el --- Incremental Vertical completYon -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.11.0
;; Package-Requires: ((emacs "24.1"))
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

(require 'cl-lib)
(require 'ffap)
(require 'ivy-overlay)
(require 'colir)

;;* Customization
(defgroup ivy nil
  "Incremental vertical completion."
  :group 'convenience)

(defgroup ivy-faces nil
  "Font-lock faces for `ivy'."
  :group 'ivy
  :group 'faces)

(defface ivy-current-match
  '((((class color) (background light))
     :background "#1a4b77" :foreground "white")
    (((class color) (background dark))
     :background "#65a7e2" :foreground "black"))
  "Face used by Ivy for highlighting the current match.")

(defface ivy-minibuffer-match-highlight
  '((t :inherit highlight))
  "Face used by Ivy for highlighting the match under the cursor.")

(defface ivy-minibuffer-match-face-1
  '((((class color) (background light))
     :background "#d3d3d3")
    (((class color) (background dark))
     :background "#555555"))
  "The background face for `ivy' minibuffer matches.")

(defface ivy-minibuffer-match-face-2
  '((((class color) (background light))
     :background "#e99ce8" :weight bold)
    (((class color) (background dark))
     :background "#777777" :weight bold))
  "Face for `ivy' minibuffer matches numbered 1 modulo 3.")

(defface ivy-minibuffer-match-face-3
  '((((class color) (background light))
     :background "#bbbbff" :weight bold)
    (((class color) (background dark))
     :background "#7777ff" :weight bold))
  "Face for `ivy' minibuffer matches numbered 2 modulo 3.")

(defface ivy-minibuffer-match-face-4
  '((((class color) (background light))
     :background "#ffbbff" :weight bold)
    (((class color) (background dark))
     :background "#8a498a" :weight bold))
  "Face for `ivy' minibuffer matches numbered 3 modulo 3.")

(defface ivy-confirm-face
  '((t :foreground "ForestGreen" :inherit minibuffer-prompt))
  "Face used by Ivy for a confirmation prompt.")

(defface ivy-match-required-face
  '((t :foreground "red" :inherit minibuffer-prompt))
  "Face used by Ivy for a match required prompt.")

(defface ivy-subdir
  '((t :inherit dired-directory))
  "Face used by Ivy for highlighting subdirs in the alternatives.")

(defface ivy-org
  '((t :inherit org-level-4))
  "Face used by Ivy for highlighting Org buffers in the alternatives.")

(defface ivy-modified-buffer
  '((t :inherit default))
  "Face used by Ivy for highlighting modified file visiting buffers.")

(defface ivy-modified-outside-buffer
  '((t :inherit default))
  "Face used by Ivy for highlighting file visiting buffers modified outside Emacs.")

(defface ivy-remote
  '((((class color) (background light))
     :foreground "#110099")
    (((class color) (background dark))
     :foreground "#7B6BFF"))
  "Face used by Ivy for highlighting remotes in the alternatives.")

(defface ivy-virtual
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for matching virtual buffer names.")

(defface ivy-action
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for displaying keys in `ivy-read-action'.")

(defface ivy-highlight-face
  '((t :inherit highlight))
  "Face used by Ivy to highlight certain candidates.")

(defface ivy-prompt-match
  '((t :inherit ivy-current-match))
  "Face used by Ivy for highlighting the selected prompt line.")

(defface ivy-separator
  '((t :inherit font-lock-doc-face))
  "Face for multiline source separator.")

(defface ivy-grep-info
  '((t :inherit compilation-info))
  "Face for highlighting grep information such as file names.")

(defface ivy-grep-line-number
  '((t :inherit compilation-line-number))
  "Face for displaying line numbers in grep messages.")

(defface ivy-completions-annotations
  '((t :inherit completions-annotations))
  "Face for displaying completion annotations.")

(defface ivy-yanked-word
  '((t :inherit highlight))
  "Face used to highlight yanked word.")

;; Set default customization `:group' to `ivy' for the rest of the file.
(setcdr (assoc load-file-name custom-current-group-alist) 'ivy)

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

(defcustom ivy-add-newline-after-prompt nil
  "When non-nil, add a newline after the `ivy-read' prompt."
  :type 'boolean)

(defcustom ivy-wrap nil
  "When non-nil, wrap around after the first and the last candidate."
  :type 'boolean)

(defcustom ivy-display-style (and (fboundp 'add-face-text-property) 'fancy)
  "The style for formatting the minibuffer.

By default, the matched strings are copied as is.

The fancy display style highlights matching parts of the regexp,
a behavior similar to `swiper'.

This setting depends on `add-face-text-property' - a C function
available since Emacs 24.4.  Fancy style will render poorly in
earlier versions of Emacs."
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
  "When non-nil, add recent files and bookmarks to `ivy-switch-buffer'."
  :type 'boolean)

(defcustom ivy-display-function nil
  "Determine where to display candidates.
When nil (the default), candidates are shown in the minibuffer.
Otherwise, this can be set to a function which takes a string
argument comprising the current matching candidates and displays
it somewhere.

This user option acts as a global default for Ivy-based
completion commands.  You can customize the display function on a
per-command basis via `ivy-display-functions-alist', which see.
See also URL
`https://github.com/abo-abo/swiper/wiki/ivy-display-function'."
  :type '(choice
          (const :tag "Minibuffer" nil)
          (const :tag "LV" ivy-display-function-lv)
          (const :tag "Popup" ivy-display-function-popup)
          (const :tag "Overlay" ivy-display-function-overlay)))

(defvar ivy-display-functions-props
  '((ivy-display-function-overlay :cleanup ivy-overlay-cleanup))
  "Map Ivy display functions to their property lists.
Examples of properties include associated `:cleanup' functions.")

(defvar ivy-display-functions-alist
  '((ivy-completion-in-region . ivy-display-function-overlay))
  "An alist for customizing `ivy-display-function'.")

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
  :type '(alist :key-type function :value-type function))

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
  "List of counsel grep-like commands.")

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

(defvar ivy--prompts-list nil)

(defun ivy-set-prompt (caller prompt-fn)
  "Associate CALLER with PROMPT-FN.
PROMPT-FN is a function of no arguments that returns a prompt string."
  (setq ivy--prompts-list
        (plist-put ivy--prompts-list caller prompt-fn)))

(defvar ivy--display-transformers-list nil
  "A list of str->str transformers per command.")

(defun ivy-set-display-transformer (cmd transformer)
  "Set CMD a displayed candidate TRANSFORMER.

It's a lambda that takes a string one of the candidates in the
collection and returns a string for display, the same candidate
plus some extra information.

This lambda is called only on the `ivy-height' candidates that
are about to be displayed, not on the whole collection."
  (setq ivy--display-transformers-list
        (plist-put ivy--display-transformers-list cmd transformer)))

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

(defvar ivy-current-prefix-arg nil
  "Prefix arg to pass to actions.
This is a global variable that is set by ivy functions for use in
action functions.")

;;* Keymap
(require 'delsel)
(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map [down-mouse-1] 'ignore)
    (define-key map [mouse-1] 'ivy-mouse-done)
    (define-key map [mouse-3] 'ivy-mouse-dispatching-done)
    (define-key map (kbd "C-M-m") 'ivy-call)
    (define-key map (kbd "C-j") 'ivy-alt-done)
    (define-key map (kbd "C-M-j") 'ivy-immediate-done)
    (define-key map (kbd "TAB") 'ivy-partial-or-done)
    (define-key map [remap next-line] 'ivy-next-line)
    (define-key map [remap previous-line] 'ivy-previous-line)
    (define-key map (kbd "C-s") 'ivy-next-line-or-history)
    (define-key map (kbd "C-r") 'ivy-reverse-i-search)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map [remap delete-backward-char] 'ivy-backward-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'ivy-backward-delete-char)
    (define-key map [remap backward-kill-word] 'ivy-backward-kill-word)
    (define-key map [remap delete-char] 'ivy-delete-char)
    (define-key map [remap forward-char] 'ivy-forward-char)
    (define-key map (kbd "<right>") 'ivy-forward-char)
    (define-key map [remap kill-word] 'ivy-kill-word)
    (define-key map [remap beginning-of-buffer] 'ivy-beginning-of-buffer)
    (define-key map [remap end-of-buffer] 'ivy-end-of-buffer)
    (define-key map (kbd "M-n") 'ivy-next-history-element)
    (define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key map [remap scroll-up-command] 'ivy-scroll-up-command)
    (define-key map [remap scroll-down-command] 'ivy-scroll-down-command)
    (define-key map (kbd "<next>") 'ivy-scroll-up-command)
    (define-key map (kbd "<prior>") 'ivy-scroll-down-command)
    (define-key map (kbd "C-v") 'ivy-scroll-up-command)
    (define-key map (kbd "M-v") 'ivy-scroll-down-command)
    (define-key map (kbd "C-M-n") 'ivy-next-line-and-call)
    (define-key map (kbd "C-M-p") 'ivy-previous-line-and-call)
    (define-key map (kbd "M-r") 'ivy-toggle-regexp-quote)
    (define-key map (kbd "M-j") 'ivy-yank-word)
    (define-key map (kbd "M-i") 'ivy-insert-current)
    (define-key map (kbd "C-o") 'hydra-ivy/body)
    (define-key map (kbd "M-o") 'ivy-dispatching-done)
    (define-key map (kbd "C-M-o") 'ivy-dispatching-call)
    (define-key map [remap kill-line] 'ivy-kill-line)
    (define-key map [remap kill-whole-line] 'ivy-kill-whole-line)
    (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (define-key map [remap kill-ring-save] 'ivy-kill-ring-save)
    (define-key map (kbd "C-'") 'ivy-avy)
    (define-key map (kbd "C-M-a") 'ivy-read-action)
    (define-key map (kbd "C-c C-o") 'ivy-occur)
    (define-key map (kbd "C-c C-a") 'ivy-toggle-ignore)
    (define-key map (kbd "C-c C-s") 'ivy-rotate-sort)
    (define-key map [remap describe-mode] 'ivy-help)
    map)
  "Keymap used in the minibuffer.")
(autoload 'hydra-ivy/body "ivy-hydra" "" t)

(defvar ivy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap switch-to-buffer]
      'ivy-switch-buffer)
    (define-key map [remap switch-to-buffer-other-window]
      'ivy-switch-buffer-other-window)
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
  def)

(defvar ivy-last (make-ivy-state)
  "The last parameters passed to `ivy-read'.

This should eventually become a stack so that you could use
`ivy-read' recursively.")

(defvar ivy-recursive-last nil)

(defvar ivy-recursive-restore t
  "When non-nil, restore the above state when exiting the minibuffer.
This variable is let-bound to nil by functions that take care of
the restoring themselves.")

(defsubst ivy-set-action (action)
  "Set the current `ivy-last' field to ACTION."
  (setf (ivy-state-action ivy-last) action))

(defvar inhibit-message)

(defun ivy-thing-at-point ()
  "Return a string that corresponds to the current thing at point."
  (substring-no-properties
   (cond
    ((thing-at-point 'url))
    ((and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
          (let ((inhibit-message t))
            (ignore-errors
              (ffap-file-at-point)))))
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

(defvar ivy--length 0
  "Store the amount of viable candidates.")

(defvar ivy-text ""
  "Store the user's string as it is typed in.")

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

(defvar ivy--regex-function 'ivy--regex
  "Current function for building a regex.")

(defvar ivy--highlight-function 'ivy--highlight-default
  "Current function for formatting the candidates.")

(defvar ivy--subexps 0
  "Number of groups in the current `ivy--regex'.")

(defvar ivy--full-length nil
  "The total amount of candidates when :dynamic-collection is non-nil.")

(defvar ivy--old-text ""
  "Store old `ivy-text' for dynamic completion.")

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

(defvar ivy-more-chars-alist
  '((counsel-grep . 2)
    (t . 3))
  "Map commands to their minimum required input length.
That is the number of characters prompted for before fetching
candidates.  The special key t is used as a fallback.")

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

(eval-and-compile
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

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

(defun ivy-exit-with-action (action)
  "Quit the minibuffer and call ACTION afterwards."
  (ivy-set-action
   `(lambda (x)
      (funcall ',action x)
      (ivy-set-action ',(ivy-state-action ivy-last))))
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defmacro with-ivy-window (&rest body)
  "Execute BODY in the window from which `ivy-read' was called."
  (declare (indent 0)
           (debug t))
  `(with-selected-window (ivy--get-window ivy-last)
     ,@body))

(defun ivy--done (text)
  "Insert TEXT and exit minibuffer."
  (insert
   (setf (ivy-state-current ivy-last)
         (if (and ivy--directory
                  (not (eq (ivy-state-history ivy-last) 'grep-files-history)))
             (expand-file-name text ivy--directory)
           text)))
  (setq ivy-exit 'done)
  (exit-minibuffer))

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

(defun ivy--prompt-selectable-p ()
  "Return t if the prompt line is selectable."
  (and ivy-use-selectable-prompt
       (memq (ivy-state-require-match ivy-last)
             '(nil confirm confirm-after-completion))))

(defun ivy--prompt-selected-p ()
  "Return t if the prompt line is selected."
  (and (ivy--prompt-selectable-p)
       (= ivy--index -1)))

;;* Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (if (ivy--prompt-selected-p)
      (ivy-immediate-done)
    (setq ivy-current-prefix-arg current-prefix-arg)
    (delete-minibuffer-contents)
    (cond ((or (> ivy--length 0)
               ;; the action from `ivy-dispatching-done' may not need a
               ;; candidate at all
               (eq this-command 'ivy-dispatching-done))
           (ivy--done (ivy-state-current ivy-last)))
          ((memq (ivy-state-collection ivy-last)
                 '(read-file-name-internal internal-complete-buffer))
           (if (or (not (eq confirm-nonexistent-file-or-buffer t))
                   (equal " (confirm)" ivy--prompt-extra))
               (ivy--done ivy-text)
             (setq ivy--prompt-extra " (confirm)")
             (insert ivy-text)
             (ivy--exhibit)))
          ((memq (ivy-state-require-match ivy-last)
                 '(nil confirm confirm-after-completion))
           (ivy--done ivy-text))
          (t
           (setq ivy--prompt-extra " (match required)")
           (insert ivy-text)
           (ivy--exhibit)))))

(defvar ivy-mouse-1-tooltip
  "Exit the minibuffer with the selected candidate."
  "The doc visible in the tooltip for mouse-1 binding in the minibuffer")
(defvar ivy-mouse-3-tooltip
  "Display alternative actions."
  "The doc visible in the tooltip for mouse-3 binding in the minibuffer")

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

(defvar ivy-read-action-format-function 'ivy-read-action-format-default
  "Function used to transform the actions list into a docstring.")

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

(defun ivy-read-action ()
  "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (if (not (ivy--actionp actions))
        t
      (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
             (resize-mini-windows t)
             (key "")
             action-idx)
        (while (and (setq action-idx (cl-position-if
                                      (lambda (x)
                                        (string-prefix-p key (car x)))
                                      (cdr actions)))
                    (not (string= key (car (nth action-idx (cdr actions))))))
          (setq key (concat key (string (read-key hint)))))
        (cond ((member key '("" ""))
               nil)
              ((null action-idx)
               (message "%s is not bound" key)
               nil)
              (t
               (message "")
               (setcar actions (1+ action-idx))
               (ivy-set-action actions)))))))

(defun ivy-shrink-after-dispatching ()
  "Shrink the window after dispatching when action list is too large."
  (window-resize nil (- ivy-height (window-height))))

(defun ivy-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (when (ivy-read-action)
    (ivy-done))
  (ivy-shrink-after-dispatching))

(defun ivy-dispatching-call ()
  "Select one of the available actions and call `ivy-call'."
  (interactive)
  (setq ivy-current-prefix-arg current-prefix-arg)
  (let ((actions (copy-sequence (ivy-state-action ivy-last))))
    (unwind-protect
         (when (ivy-read-action)
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

(defun ivy-alt-done (&optional arg)
  "Exit the minibuffer with the selected candidate.
When ARG is t, exit with current text, ignoring the candidates.
When the current candidate during file name completion is a
directory, continue completion from within that directory instead
of exiting.  This function is otherwise like `ivy-done'."
  (interactive "P")
  (setq ivy-current-prefix-arg current-prefix-arg)
  (cond ((or arg
             (ivy--prompt-selected-p))
         (ivy-immediate-done))
        (ivy--directory
         (ivy--directory-done))
        ((eq (ivy-state-collection ivy-last) #'Info-read-node-name-1)
         (if (member (ivy-state-current ivy-last) '("(./)" "(../)"))
             (ivy-quit-and-run
               (ivy-read "Go to file: " #'read-file-name-internal
                         :action (lambda (x)
                                   (Info-find-node
                                    (expand-file-name x ivy--directory)
                                    "Top"))))
           (ivy-done)))
        (t
         (ivy-done))))

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

(defun ivy--directory-done ()
  "Handle exit from the minibuffer when completing file names."
  (let (dir)
    (cond
      ((equal ivy-text "/sudo::")
       (setq dir (concat ivy-text (expand-file-name ivy--directory)))
       (ivy--cd dir)
       (ivy--exhibit))
      ((ivy--directory-enter))
      ((unless (string= ivy-text "")
         (let ((file (expand-file-name
                      (if (> ivy--length 0) (ivy-state-current ivy-last) ivy-text)
                      ivy--directory)))
           (when (ignore-errors (file-exists-p file))
             (if (file-directory-p file)
                 (ivy--cd (file-name-as-directory file))
               (ivy-done))
             ivy-text))))
      ((or (and (equal ivy--directory "/")
                (string-match-p "\\`[^/]+:.*:.*\\'" ivy-text))
           (string-match-p "\\`/[^/]+:.*:.*\\'" ivy-text))
       (ivy-done))
      ((or (and (equal ivy--directory "/")
                (cond ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        ivy-text)
                       (setq ivy-text (ivy-state-current ivy-last)))
                      ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        (ivy-state-current ivy-last))
                       (setq ivy-text (ivy-state-current ivy-last)))))
           (string-match
            "\\`/\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
            ivy-text))
       (let ((method (match-string 1 ivy-text))
             (user (match-string 2 ivy-text))
             (rest (match-string 3 ivy-text))
             res)
         (require 'tramp)
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
             (ivy--cd (concat "/" method ":" host ":"))))))
      (t
       (ivy-done)))))

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
  (if (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (or (and (equal ivy--directory "/")
                    (string-match-p "\\`[^/]+:.*\\'" ivy-text))
               (= (string-to-char ivy-text) ?/)))
      (let ((default-directory ivy--directory)
            dir)
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (setq dir (ivy-expand-file-if-directory ivy-text))
          (ivy--cd dir)))
    (or (ivy-partial)
        (when (or (eq this-command last-command)
                  (eq ivy--length 1))
          (ivy-alt-done)))))

(defun ivy--remove-prefix (prefix string)
  "Compatibility shim for `string-remove-prefix'."
  (if (string-prefix-p prefix string)
      (substring string (length prefix))
    string))

(defun ivy-partial ()
  "Complete the minibuffer text as much as possible."
  (interactive)
  (let* ((parts (or (split-string ivy-text " " t) (list "")))
         (tail (last parts))
         (postfix (car tail))
         (case-fold-search (ivy--case-fold-p ivy-text))
         (completion-ignore-case case-fold-search)
         (new (try-completion (ivy--remove-prefix "^" postfix)
                              (if (ivy-state-dynamic-collection ivy-last)
                                  ivy--all-candidates
                                (mapcar (lambda (str)
                                          (let ((i (string-match-p postfix str)))
                                            (and i (substring str i))))
                                        ivy--old-cands)))))
    (cond ((eq new t) nil)
          ((string= new ivy-text) nil)
          (new
           (delete-region (minibuffer-prompt-end) (point-max))
           (setcar tail
                   (if (= (string-to-char postfix) ?^)
                       (concat "^" new)
                     new))
           (insert
            (setq ivy-text
                  (concat
                   (mapconcat #'identity parts " ")
                   (and ivy-tab-space " "))))
           (when (and
                  (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                  (= 1 (length
                        (all-completions ivy-text ivy--all-candidates)))
                  (let ((default-directory ivy--directory))
                    (file-directory-p (ivy-state-current ivy-last))))
             (ivy--directory-done))
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
                ;; For `read-file-name' compat, unchanged initial input means
                ;; that `ivy-read' shall return INITIAL-INPUT.
                ;; `read-file-name-default' `string-equal' return value with
                ;; provided INITIAL-INPUT to detect that the user choose the
                ;; default, `default-filename'.  We must return `ivy--directory'
                ;; in unchanged form in cased `ivy--directory' started out as
                ;; INITIAL-INPUT in abbreviated form.
               ivy--directory)          ; Unchanged (unexpanded)
              (t
               (expand-file-name ivy-text ivy--directory))))
  (insert (ivy-state-current ivy-last))
  (setq ivy-completion-beg ivy-completion-end)
  (setq ivy-exit 'done)
  (exit-minibuffer))

;;;###autoload
(defun ivy-resume ()
  "Resume the last completion session."
  (interactive)
  (if (null (ivy-state-action ivy-last))
      (user-error "The last session isn't compatible with `ivy-resume'")
    (when (eq (ivy-state-caller ivy-last) 'swiper)
      (switch-to-buffer (ivy-state-buffer ivy-last)))
    (with-current-buffer (ivy-state-buffer ivy-last)
      (let ((default-directory (ivy-state-directory ivy-last)))
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

(defun ivy-minibuffer-grow ()
  "Grow the minibuffer window by 1 line."
  (interactive)
  (setq-local max-mini-window-height
              (cl-incf ivy-height)))

(defun ivy-minibuffer-shrink ()
  "Shrink the minibuffer window by 1 line."
  (interactive)
  (when (> ivy-height 2)
    (setq-local max-mini-window-height
                (cl-decf ivy-height))
    (window-resize nil -1)))

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
  (if (string= ivy-text "")
      (ivy-previous-history-element 1)
    (ivy-next-line arg)))

(defun ivy-previous-line (&optional arg)
  "Move cursor vertically up ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (- ivy--index arg))
        (min-index (if (ivy--prompt-selectable-p) -1 0)))
    (if (< index min-index)
        (if ivy-wrap
            (ivy-end-of-buffer)
          (ivy-set-index min-index))
      (ivy-set-index index))))

(defun ivy-previous-line-or-history (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (when (and (zerop ivy--index) (string= ivy-text ""))
    (ivy-previous-history-element 1))
  (ivy-previous-line arg))

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
  ;; invalidate cache
  (setq ivy--old-cands nil))

(defun ivy--get-action (state)
  "Get the action function from STATE."
  (let ((action (ivy-state-action state)))
    (when action
      (if (functionp action)
          action
        (cadr (nth (car action) action))))))

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
          (str (counsel-locate \"lispy.el\")))
     ;; do whatever with str - the corresponding file will not be opened
     )")

(defun ivy-recursive-restore ()
  "Restore the above state when exiting the minibuffer.
See variable `ivy-recursive-restore' for further information."
  (when (and ivy-recursive-last
             ivy-recursive-restore
             (not (eq ivy-last ivy-recursive-last)))
    (ivy--reset-state (setq ivy-last ivy-recursive-last))))

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
  (let ((action (and (not ivy-inhibit-action)
                     (ivy--get-action ivy-last))))
    (when action
      (let* ((collection (ivy-state-collection ivy-last))
             (current (ivy-state-current ivy-last))
             (x (cond
                 ;; Alist type.
                 ((and (consp (car-safe collection))
                       ;; Previously, the cdr of the selected
                       ;; candidate would be returned.  Now, the
                       ;; whole candidate is returned.
                       (let ((idx (get-text-property 0 'idx current)))
                         (if idx
                             (nth idx collection)
                           (assoc current collection)))))
                 (ivy--directory
                  (expand-file-name current ivy--directory))
                 ((equal current "")
                  ivy-text)
                 (t
                  current))))
        (if (eq action #'identity)
            (funcall action x)
          (select-window (ivy--get-window ivy-last))
          (set-buffer (ivy-state-buffer ivy-last))
          (prog1 (unwind-protect (funcall action x)
                   (ivy-recursive-restore))
            (unless (or (eq ivy-exit 'done)
                        (minibuffer-window-active-p (selected-window))
                        (null (active-minibuffer-window)))
              (select-window (active-minibuffer-window)))))))))

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
  "Move cursor vertically down ARG candidates.
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

(defun ivy-next-history-element (arg)
  "Forward to `next-history-element' with ARG."
  (interactive "p")
  (if (and (= minibuffer-history-position 0)
           (equal ivy-text ""))
      (progn
        (insert ivy--default)
        (when (and (with-ivy-window (derived-mode-p 'prog-mode))
                   (eq (ivy-state-caller ivy-last) 'swiper)
                   (not (file-exists-p ivy--default))
                   (not (ffap-url-p ivy--default))
                   (not (ivy-state-dynamic-collection ivy-last))
                   (> (point) (minibuffer-prompt-end)))
          (undo-boundary)
          (insert "\\_>")
          (goto-char (minibuffer-prompt-end))
          (insert "\\_<")
          (forward-char (+ 2 (length ivy--default)))))
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
      (if (setq url (or (ffap-url-p input)
                        (with-ivy-window
                          (cl-reduce
                           (lambda (a b)
                             (or a (funcall b)))
                           ivy-ffap-url-functions
                           :initial-value nil))))
          (ivy-exit-with-action
           (lambda (_)
             (funcall ffap-url-fetcher url)))
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
  (if (null ivy--directory)
      (error "Unexpected")
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
    (setq ivy-text "")
    (setf (ivy-state-directory ivy-last) dir)
    (delete-minibuffer-contents)))

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
      (let ((pt (point)))
        (forward-word -1)
        (delete-region (point) pt)))))

(defvar ivy--regexp-quote #'regexp-quote
  "Store the regexp quoting state.")

(defun ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function ivy--regexp-quote))

(defvar avy-all-windows)
(defvar avy-action)
(defvar avy-keys)
(defvar avy-keys-alist)
(defvar avy-style)
(defvar avy-styles-alist)
(declare-function avy--process "ext:avy")
(declare-function avy--style-fn "ext:avy")

(defcustom ivy-format-function #'ivy-format-function-default
  "Function to transform the list of candidates into a string.
This string is inserted into the minibuffer."
  :type '(choice
          (const :tag "Default" ivy-format-function-default)
          (const :tag "Arrow prefix" ivy-format-function-arrow)
          (const :tag "Full line" ivy-format-function-line)))

(eval-after-load 'avy
  '(add-to-list 'avy-styles-alist '(ivy-avy . pre)))

(defun ivy--avy-candidates ()
  (let (candidates)
    (save-excursion
      (save-restriction
        (narrow-to-region
         (window-start)
         (window-end))
        (goto-char (point-min))
        (forward-line)
        (while (< (point) (point-max))
          (push
           (cons (point)
                 (selected-window))
           candidates)
          (forward-line))))
    (nreverse candidates)))

(defun ivy--avy-action (pt)
  (when (number-or-marker-p pt)
    (ivy--done
     (substring-no-properties
      (nth (- (line-number-at-pos pt) 2) ivy--old-cands)))))

(defun ivy-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (let* ((avy-all-windows nil)
         (avy-keys (or (cdr (assq 'ivy-avy avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq 'ivy-avy avy-styles-alist))
                        avy-style))
         (avy-action #'ivy--avy-action))
    (avy--process
     (ivy--avy-candidates))))

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

(declare-function ido-file-extension-lessp "ido")

(defun ivy-sort-file-function-using-ido (x y)
  "Compare two files X and Y using `ido-file-extensions-order'.

This function is suitable as a replacement for
`ivy-sort-file-function-default' in `ivy-sort-functions-alist'."
  (if (and (bound-and-true-p ido-file-extensions-order))
      (ido-file-extension-lessp x y)
    (ivy-sort-file-function-default x y)))

(defun ivy-string< (x y)
  "Like `string<', but operate on CARs when given cons cells."
  (string< (if (consp x) (car x) x)
           (if (consp y) (car y) y)))

(defcustom ivy-sort-functions-alist
  '((read-file-name-internal . ivy-sort-file-function-default)
    (internal-complete-buffer . nil)
    (ivy-completion-in-region . nil)
    (counsel-git-grep-function . nil)
    (Man-goto-section . nil)
    (org-refile . nil)
    (t . ivy-string<))
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
                 (const :tag "Plain sort" string-lessp)
                 (const :tag "File sort" ivy-sort-file-function-default)
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

(defvar ivy-index-functions-alist
  '((swiper . ivy-recompute-index-swiper)
    (swiper-multi . ivy-recompute-index-swiper)
    (counsel-git-grep . ivy-recompute-index-swiper)
    (counsel-grep . ivy-recompute-index-swiper-async)
    (t . ivy-recompute-index-zero))
  "An alist of index recomputing functions for each collection function.
When the input changes, the appropriate function returns an
integer - the index of the matched candidate that should be
selected.")

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
  "An alist of highlighting functions for each regex buidler function.")

(defvar ivy-initial-inputs-alist
  '((org-refile . "^")
    (org-agenda-refile . "^")
    (org-capture-refile . "^")
    (counsel-M-x . "^")
    (counsel-describe-function . "^")
    (counsel-describe-variable . "^")
    (counsel-org-capture . "^")
    (Man-completion-table . "^")
    (woman . "^"))
  "An alist associating commands with their initial input.

Each cdr is either a string or a function called in the context
of a call to `ivy-read'.")

(defcustom ivy-hooks-alist nil
  "An alist associating commands to setup functions.
Examples: `toggle-input-method', (lambda () (insert \"^\")), etc.
May supersede `ivy-initial-inputs-alist'."
  :type '(alist :key-type symbol :value-type function))

(defcustom ivy-sort-max-size 30000
  "Sorting won't be done for collections larger than this."
  :type 'integer)

(defalias 'ivy--dirname-p
  (if (fboundp 'directory-name-p)
      #'directory-name-p
    (lambda (name)
      "Return non-nil if NAME ends with a directory separator."
      (string-match-p "/\\'" name))))

(defun ivy--sorted-files (dir)
  "Return the list of files in DIR.
Directories come first."
  (let* ((default-directory dir)
         (seq (condition-case nil
                  (all-completions "" #'read-file-name-internal
                                   (ivy-state-predicate ivy-last))
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
  (cdr (or (let ((caller (or key (ivy-state-caller ivy-last))))
             (and caller (assq caller alist)))
           (assq t alist))))

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
  (remove-list-of-text-properties 0 (length str) props str)
  str)

;;** Entry Point
;;;###autoload
(cl-defun ivy-read (prompt collection
                    &key
                      predicate require-match initial-input
                      history preselect def keymap update-fn sort
                      action unwind re-builder matcher
                      dynamic-collection caller)
  "Read a string in the minibuffer, with completion.

PROMPT is a string, normally ending in a colon and a space.
`ivy-count-format' is prepended to PROMPT during completion.

COLLECTION is either a list of strings, a function, an alist, or
a hash table.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for compatibility with `completing-read'.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected.

If INITIAL-INPUT is non-nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

If PRESELECT is not nil, then select the corresponding candidate
out of the ones that match the INITIAL-INPUT.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the candidate list is redisplayed.

When SORT is non-nil, `ivy-sort-functions-alist' determines how
to sort candidates before displaying them.

ACTION is a function to call after selecting a candidate.
It takes the candidate, which is a string, as its only argument.

UNWIND is a function of no arguments to call before exiting.

RE-BUILDER is a function transforming input text into a regex
pattern.

MATCHER is a function which can override how candidates are
filtered based on user input.  It takes a regex pattern and a
list of candidates, and returns the list of matching candidates.

DYNAMIC-COLLECTION is a boolean specifying whether the list of
candidates is updated after each input by calling COLLECTION.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session."
  (let ((extra-actions (delete-dups
                        (append (plist-get ivy--actions-list t)
                                (plist-get ivy--actions-list this-command)
                                (plist-get ivy--actions-list caller)))))
    (when extra-actions
      (setq action
            (cond ((functionp action)
                   `(1
                     ("o" ,action "default")
                     ,@extra-actions))
                  ((null action)
                   `(1
                     ("o" identity "default")
                     ,@extra-actions))
                  (t
                   (delete-dups (append action extra-actions)))))))
  (unless caller
    (setq caller this-command))
  (let ((extra-sources (plist-get ivy--sources-list caller)))
    (if extra-sources
        (progn
          (setq ivy--extra-candidates nil)
          (dolist (source extra-sources)
            (cond ((equal source '(original-source))
                   (setq ivy--extra-candidates
                         (cons source ivy--extra-candidates)))
                  ((null (cdr source))
                   (setq ivy--extra-candidates
                         (cons
                          (list (car source) (funcall (car source)))
                          ivy--extra-candidates))))))
      (setq ivy--extra-candidates '((original-source)))))
  (let ((ivy-recursive-last (and (active-minibuffer-window) ivy-last))
        (transformer-fn
         (plist-get ivy--display-transformers-list
                    (cond (caller)
                          ((functionp collection)
                           collection))))
        (ivy-display-function
         (unless (window-minibuffer-p)
           (or ivy-display-function
               (ivy-alist-setting ivy-display-functions-alist caller))))
        (height (ivy--height caller)))
    (setq ivy-last
          (make-ivy-state
           :prompt prompt
           :collection collection
           :predicate predicate
           :require-match require-match
           :initial-input initial-input
           :history history
           :preselect preselect
           :keymap keymap
           :update-fn update-fn
           :sort sort
           :action action
           :frame (selected-frame)
           :window (selected-window)
           :buffer (current-buffer)
           :unwind unwind
           :re-builder re-builder
           :matcher matcher
           :dynamic-collection dynamic-collection
           :display-transformer-fn transformer-fn
           :directory default-directory
           :caller caller
           :def def))
    (ivy--reset-state ivy-last)
    (prog1
        (unwind-protect
             (minibuffer-with-setup-hook
                 #'ivy--minibuffer-setup
               (let* ((hist (or history 'ivy-history))
                      (minibuffer-completion-table collection)
                      (minibuffer-completion-predicate predicate)
                      (ivy-height height)
                      (resize-mini-windows (unless (display-graphic-p)
                                             'grow-only)))
                 (if (and ivy-auto-select-single-candidate
                          ivy--all-candidates
                          (null (cdr ivy--all-candidates)))
                     (progn
                       (setf (ivy-state-current ivy-last)
                             (car ivy--all-candidates))
                       (setq ivy-exit 'done))
                   (read-from-minibuffer
                    prompt
                    (ivy-state-initial-input ivy-last)
                    (make-composed-keymap keymap ivy-minibuffer-map)
                    nil
                    hist))
                 (when (eq ivy-exit 'done)
                   (let ((item (if ivy--directory
                                   (ivy-state-current ivy-last)
                                 ivy-text)))
                     (unless (equal item "")
                       (set hist (cons (propertize item 'ivy-index ivy--index)
                                       (delete item
                                               (cdr (symbol-value hist))))))))
                 (ivy-state-current ivy-last)))
          ;; Fixes a bug in ESS, #1660
          (put 'post-command-hook 'permanent-local nil)
          (remove-hook 'post-command-hook #'ivy--queue-exhibit)
          (let ((cleanup (ivy--display-function-prop :cleanup)))
            (when (functionp cleanup)
              (funcall cleanup)))
          (when (setq unwind (ivy-state-unwind ivy-last))
            (funcall unwind))
          (ivy--pulse-cleanup)
          (unless (eq ivy-exit 'done)
            (ivy-recursive-restore)))
      (ivy-call)
      (ivy--remove-props (ivy-state-current ivy-last) 'idx))))

(defun ivy--display-function-prop (prop)
  "Return PROP associated with current `ivy-display-function'."
  (plist-get (cdr (assq ivy-display-function
                        ivy-display-functions-props))
             prop))

(defvar Info-complete-menu-buffer)

(defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
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
              (let ((init (cdr (assq caller ivy-initial-inputs-alist))))
                (cond ((functionp init)
                       (funcall init))
                      (t
                       init)))))
         (def (ivy-state-def state)))
    (setq ivy--directory nil)
    (setq ivy-case-fold-search ivy-case-fold-search-default)
    (setq ivy--regex-function
          (or re-builder
              (and (functionp collection)
                   (cdr (assq collection ivy-re-builders-alist)))
              (ivy-alist-setting ivy-re-builders-alist)
              #'ivy--regex))
    (setq ivy--subexps 0)
    (setq ivy--regexp-quote #'regexp-quote)
    (setq ivy--old-text "")
    (setq ivy--full-length nil)
    (setq ivy-text "")
    (setq ivy--index 0)
    (setq ivy-calling nil)
    (setq ivy-use-ignore ivy-use-ignore-default)
    (setq ivy--highlight-function
          (or (cdr (assq ivy--regex-function ivy-highlight-functions-alist))
              #'ivy--highlight-default))
    (let (coll sort-fn)
      (cond ((eq collection #'Info-read-node-name-1)
             (setq coll
                   (if (equal (bound-and-true-p Info-current-file) "dir")
                       (mapcar (lambda (x) (format "(%s)" x))
                               (delete-dups
                                (all-completions "(" collection predicate)))
                     (all-completions "" collection predicate))))
            ((eq collection #'read-file-name-internal)
             (when (and (equal def initial-input)
                        (member "./" ivy-extra-directories))
               (setf (ivy-state-def state) (setq def nil)))
             (setq ivy--directory default-directory)
             (when (and initial-input
                        (not (equal initial-input "")))
               (cond ((file-directory-p initial-input)
                      (when (equal (file-name-nondirectory initial-input) "")
                        (setf (ivy-state-preselect state) (setq preselect nil))
                        (setf (ivy-state-def state) (setq def nil)))
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
               (when (or (not (ivy-state-action ivy-last))
                         (equal (ivy--get-action ivy-last) 'identity))
                 (setq initial-input nil))))
            ((eq collection #'internal-complete-buffer)
             (setq coll (ivy--buffer-list "" ivy-use-virtual-buffers predicate)))
            (dynamic-collection
             (setq coll (funcall collection ivy-text)))
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
      (when def
        (cond ((stringp (car-safe def))
               (setq coll (cl-union def coll :test #'equal)))
              ((and (stringp def) (not (member def coll)))
               (push def coll))))
      (when (and sort
                 (or (functionp collection)
                     (not (eq history 'org-refile-history)))
                 (setq sort-fn (ivy--sort-function
                                (if (functionp collection) collection caller)))
                 (null (nthcdr ivy-sort-max-size coll)))
        (setq coll (sort (copy-sequence coll) sort-fn)))
      (setq coll (ivy--set-candidates coll))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (when (integerp preselect)
        (setq ivy--old-re "")
        (ivy-set-index preselect))
      (when initial-input
        ;; Needed for anchor to work
        (setq ivy--old-cands coll)
        (setq ivy--old-cands (ivy--filter initial-input coll)))
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
    (setf (ivy-state-initial-input ivy-last) initial-input)))

(defun ivy-add-prompt-count (prompt)
  "Add count information to PROMPT."
  (cond ((null ivy-count-format)
         (error
          "`ivy-count-format' can't be nil.  Set it to \"\" instead"))
        ((string-match "%d.*\\(%d\\)" ivy-count-format)
         (let* ((w (1+ (floor (log (max 1 (length ivy--all-candidates)) 10))))
                (s (replace-match (format "%%-%dd" w) t t ivy-count-format 1)))
           (string-match "%d" s)
           (concat (replace-match (format "%%%dd" w) t t s)
                   prompt)))
        ((string-match-p "%.*d" ivy-count-format)
         (concat ivy-count-format prompt))
        (ivy--directory
         prompt)
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
REQUIRE-MATCH is a boolean value.  See `completing-read'.
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
                  :require-match (and collection require-match)
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
                  :sort t
                  :dynamic-collection ivy-completing-read-dynamic-collection
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

(defun ivy-completion-in-region-action (str)
  "Insert STR, erasing the previous one.
The previous string is between `ivy-completion-beg' and `ivy-completion-end'."
  (when (consp str)
    (setq str (cdr str)))
  (when (stringp str)
    (let ((fake-cursors (and (fboundp 'mc/all-fake-cursors)
                             (mc/all-fake-cursors)))
          (pt (point))
          (beg ivy-completion-beg)
          (end ivy-completion-end))
      (when beg
        (delete-region beg end))
      (setq ivy-completion-beg (point))
      (insert (substring-no-properties str))
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
           (let* ((len (ivy-completion-common-length (car comps)))
                  (initial (cond ((= len 0)
                                  "")
                                 ((let ((str-len (length str)))
                                    (when (> len str-len)
                                      (setq len str-len)
                                      str)))
                                 (t
                                  (substring str (- len))))))
             (setq ivy--old-re nil)
             (unless (ivy--filter initial comps)
               (setq initial nil))
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
               (ivy-read (format "(%s): " str) comps
                         ;; Predicate was already applied by
                         ;; `completion-all-completions'.
                         :predicate nil
                         :initial-input initial
                         :sort t
                         :action #'ivy-completion-in-region-action
                         :unwind (lambda ()
                                   (unless (eq ivy-exit 'done)
                                     (goto-char ivy-completion-beg)
                                     (insert initial)))
                         :caller 'ivy-completion-in-region)
               t))))))

(defun ivy-completion-in-region-prompt ()
  "Prompt function for `ivy-completion-in-region'.
See `ivy-set-prompt'."
  (and (window-minibuffer-p (ivy-state-window ivy-last))
       (ivy-add-prompt-count (ivy-state-prompt ivy-last))))

(ivy-set-prompt #'ivy-completion-in-region #'ivy-completion-in-region-prompt)

(defcustom ivy-do-completion-in-region t
  "When non-nil `ivy-mode' will set `completion-in-region-function'."
  :type 'boolean)

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
        (setq completing-read-function 'ivy-completing-read)
        (when ivy-do-completion-in-region
          (setq completion-in-region-function 'ivy-completion-in-region)))
    (setq completing-read-function 'completing-read-default)
    (setq completion-in-region-function 'completion--in-region)))

(defun ivy--preselect-index (preselect candidates)
  "Return the index of PRESELECT in CANDIDATES."
  (cond ((integerp preselect)
         preselect)
        ((cl-position preselect candidates :test #'equal))
        ((stringp preselect)
         (let ((re preselect))
           (cl-position-if
            (lambda (x)
              (string-match-p re x))
            candidates)))))

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

(defun ivy--split (str)
  "Split STR into list of substrings bounded by spaces.
Single spaces act as splitting points.  Consecutive spaces
\"quote\" their preceding spaces, i.e., guard them from being
split.  This allows the literal interpretation of N spaces by
inputting N+1 spaces.  Any substring not constituting a valid
regexp is passed to `regexp-quote'."
  (let ((len (length str))
        start0
        (start1 0)
        res s
        match-len)
    (while (and (string-match " +" str start1)
                (< start1 len))
      (if (and (> (match-beginning 0) 2)
               (string= "[^" (substring
                              str
                              (- (match-beginning 0) 2)
                              (match-beginning 0))))
          (progn
            (setq start0 start1)
            (setq start1 (match-end 0)))
        (setq match-len (- (match-end 0) (match-beginning 0)))
        (if (= match-len 1)
            (progn
              (when start0
                (setq start1 start0)
                (setq start0 nil))
              (push (substring str start1 (match-beginning 0)) res)
              (setq start1 (match-end 0)))
          (setq str (replace-match
                     (make-string (1- match-len) ?\ )
                     nil nil str))
          (setq start0 (or start0 start1))
          (setq start1 (1- (match-end 0))))))
    (if start0
        (push (substring str start0) res)
      (setq s (substring str start1))
      (unless (= (length s) 0)
        (push s res)))
    (mapcar #'ivy--regex-or-literal (nreverse res))))

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
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (car subs))
                        (cons
                         (setq ivy--subexps (length subs))
                         (mapconcat
                          (lambda (x)
                            (if (string-match-p "\\`\\\\([^?].*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy ".*" ".*?")))))
                    ivy--regex-hash)))))

(defun ivy--legal-regex-p (str)
  "Return t if STR is valid regular expression."
  (condition-case nil
      (progn
        (string-match-p str "")
        t)
    (invalid-regexp nil)))

(defun ivy--regex-or-literal (str)
  "If STR isn't a legal regex, escape it."
  (if (ivy--legal-regex-p str) str (regexp-quote str)))

(defun ivy--split-negation (str)
  "Split STR into text before and after ! delimiter.
Do not split if the delimiter is escaped as \\!.

Assumes there is at most one unescaped delimiter and discards
text after delimiter if it is empty.  Modifies match data."
  (unless (string= str "")
    (let ((delim "\\(?:\\`\\|[^\\]\\)\\(!\\)"))
      (mapcar (lambda (split)
                ;; Store "\!" as "!".
                (replace-regexp-in-string "\\\\!" "!" split t t))
              (if (string-match delim str)
                  ;; Ignore everything past first unescaped ! rather than
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
No unescaped spaces are left in the output.  Any substring not
constituting a valid regexp is passed to `regexp-quote'."
  (when str
    (let ((i 0) ; End of last search.
          (j 0) ; End of last delimiter.
          parts)
      (while (string-match "\\(\\\\ \\)\\| +" str i)
        (setq i (match-end 0))
        (if (not (match-beginning 1))
            ;; Unescaped space(s).
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
foo\!bar -> matches \"foo!bar\"
foo\ bar -> matches \"foo bar\"

Returns a list suitable for `ivy-re-match'."
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
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (let ((lst (string-to-list (match-string 2 str))))
                    (apply #'concat
                           (cl-mapcar
                            #'concat
                            (cons "" (cdr (mapcar (lambda (c) (format "[^%c]*" c))
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
  (let ((hook (ivy-alist-setting ivy-hooks-alist)))
    (when (functionp hook)
      (funcall hook)))
  ;; Show completions with empty input.
  (ivy--exhibit))

(defun ivy--input ()
  "Return the current minibuffer input."
  ;; assume one-line minibuffer input
  (buffer-substring-no-properties
   (minibuffer-prompt-end)
   (line-end-position)))

(defun ivy--cleanup ()
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
        (when (ivy--prompt-selectable-p)
          (if (or (= ivy--index -1)
                  (= ivy--length 0))
              (ivy-add-face-text-property
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

(defun ivy--magic-file-slash ()
  "Handle slash when completing file names."
  (when (or (and (eq this-command #'self-insert-command)
                 (eolp))
            (eq this-command #'ivy-partial-or-done))
    (let ((canonical (expand-file-name ivy-text ivy--directory))
          (magic (not (string= ivy-text "/"))))
      (cond ((member ivy-text ivy--all-candidates)
             (ivy--cd canonical))
            ((string-match-p "//\\'" ivy-text)
             (ivy--cd (if (string-match "\\`[[:alpha:]]:/" default-directory)
                          (match-string 0 default-directory)
                        "/")))
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

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--queue-exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (setq ivy-text (ivy--input))
    (if (ivy-state-dynamic-collection ivy-last)
        ;; while-no-input would cause annoying
        ;; "Waiting for process to die...done" message interruptions
        (let ((inhibit-message t))
          (unless (equal ivy--old-text ivy-text)
            (while-no-input
              (setq ivy--all-candidates
                    (ivy--sort-maybe
                     (funcall (ivy-state-collection ivy-last) ivy-text)))
              (setq ivy--old-text ivy-text)))
          (when (or ivy--all-candidates
                    (not (get-process " *counsel*")))
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))))
      (cond (ivy--directory
             (cond ((or (string= "~/" ivy-text)
                        (and (string= "~" ivy-text)
                             ivy-magic-tilde))
                    (ivy--cd (expand-file-name "~/")))
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
      (ivy--insert-minibuffer
       (with-current-buffer (ivy-state-buffer ivy-last)
         (ivy--format
          (ivy--filter ivy-text ivy--all-candidates))))
      (setq ivy--old-text ivy-text))))

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
        (ivy--cleanup)
        (when update-fn
          (funcall update-fn))
        (ivy--insert-prompt)
        ;; Do nothing if while-no-input was aborted.
        (when (stringp text)
          (if ivy-display-function
              (funcall ivy-display-function text)
            (ivy-display-function-fallback text)))
        (ivy--resize-minibuffer-to-fit)
        ;; prevent region growing due to text remove/add
        (when (region-active-p)
          (set-mark old-mark))))))

(defun ivy--resize-minibuffer-to-fit ()
  "Resize the minibuffer window size to fit the text in the minibuffer."
  (unless (frame-root-window-p (minibuffer-window))
    (with-selected-window (minibuffer-window)
      (if (fboundp 'window-text-pixel-size)
          (let ((text-height (cdr (window-text-pixel-size)))
                (body-height (window-body-height nil t)))
            (when (> text-height body-height)
              ;; Note: the size increment needs to be at least
              ;; frame-char-height, otherwise resizing won't do
              ;; anything.
              (let ((delta (max (- text-height body-height)
                                (frame-char-height))))
                (window-resize nil delta nil t t))))
        (let ((text-height (count-screen-lines))
              (body-height (window-body-height)))
          (when (> text-height body-height)
            (window-resize nil (- text-height body-height) nil t)))))))

(defun ivy--add-face (str face)
  "Propertize STR with FACE."
  (let ((len (length str)))
    (condition-case nil
        (progn
          (colir-blend-face-background 0 len face str)
          (let ((foreground (face-foreground face)))
            (when foreground
              (ivy-add-face-text-property
               0 len (list :foreground foreground) str))))
      (error
       (ignore-errors
         (font-lock-append-text-property 0 len 'face face str)))))
  str)

(declare-function flx-make-string-cache "ext:flx")
(declare-function flx-score "ext:flx")

(defvar ivy--flx-cache nil)

(eval-after-load 'flx
  '(setq ivy--flx-cache (flx-make-string-cache)))

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
    candidates))

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
                                  '(0 2)))
                       (ignore-errors
                         (cl-remove-if-not
                          (lambda (x) (string-match-p re x))
                          ivy--old-cands)))
                      (t
                       (ivy--re-filter re candidates)))))
        (if (memq (cdr (assq (ivy-state-caller ivy-last)
                             ivy-index-functions-alist))
                  '(ivy-recompute-index-swiper
                    ivy-recompute-index-swiper-async))
            (progn
              (ivy--recompute-index name re-str cands)
              (setq ivy--old-cands (ivy--sort name cands)))
          (setq ivy--old-cands (ivy--sort name cands))
          (ivy--recompute-index name re-str ivy--old-cands))
        (setq ivy--old-re re)
        ivy--old-cands))))

(defun ivy--set-candidates (x)
  "Update `ivy--all-candidates' with X."
  (let (res)
    (dolist (source ivy--extra-candidates)
      (if (equal source '(original-source))
          (if (null res)
              (setq res x)
            (setq res (append x res)))
        (setq ivy--old-re nil)
        (setq res (append
                   (ivy--filter ivy-text (cadr source))
                   res))))
    (setq ivy--all-candidates res)))

(defcustom ivy-sort-matches-functions-alist
  '((t . nil)
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
  (let ((key (or (ivy-state-caller ivy-last)
                 (when (functionp (ivy-state-collection ivy-last))
                   (ivy-state-collection ivy-last))
                 this-command))
        fun)
    (cond ((and ivy--flx-featurep
                (eq ivy--regex-function 'ivy--regex-fuzzy))
           (ivy--flx-sort name candidates))
          ((setq fun (ivy-alist-setting ivy-sort-matches-functions-alist key))
           (funcall fun name candidates))
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

(define-obsolete-function-alias 'ivy-generic-regex-to-str
    'ivy-re-to-str "0.10.0")

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
           (re-prefix (concat "\\`\\*" base-re))
           res-prefix
           res-noprefix
           res-virtual-prefix
           res-virtual-noprefix)
      (unless (cl-find-if (lambda (s) (string-match-p re-prefix s)) candidates)
        (setq re-prefix (concat "\\`" base-re)))
      (dolist (s candidates)
        (cond
          ((and (assoc s ivy--virtual-buffers) (string-match-p re-prefix s))
           (push s res-virtual-prefix))
          ((assoc s ivy--virtual-buffers)
           (push s res-virtual-noprefix))
          ((string-match-p re-prefix s)
           (push s res-prefix))
          (t
           (push s res-noprefix))))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)
       (nreverse res-virtual-prefix)
       (nreverse res-virtual-noprefix)))))

(defun ivy--recompute-index (name re-str cands)
  "Recompute index of selected candidate matching NAME.
RE-STR is the regexp, CANDS are the current candidates."
  (let ((caller (ivy-state-caller ivy-last))
        (func (or (ivy-alist-setting ivy-index-functions-alist)
                  #'ivy-recompute-index-zero))
        (case-fold-search (ivy--case-fold-p name))
        (preselect (ivy-state-preselect ivy-last))
        (current (ivy-state-current ivy-last))
        (empty (string= name "")))
    (unless (eq this-command 'ivy-resume)
      (ivy-set-index
       (or
        (cl-position (ivy--remove-prefix "^" name)
                     cands
                     :test #'ivy--case-fold-string=)
        (and ivy--directory
             (cl-position (concat re-str "/")
                          cands
                          :test #'ivy--case-fold-string=))
        (and (eq caller 'ivy-switch-buffer)
             (not empty)
             0)
        (and (not empty)
             (not (eq caller 'swiper))
             (not (and ivy--flx-featurep
                       (eq ivy--regex-function 'ivy--regex-fuzzy)
                       ;; Limit to 200 candidates
                       (null (nthcdr 200 cands))))
             ;; If there was a preselected candidate, don't try to
             ;; keep it selected even if the regexp still matches it.
             ;; See issue #1563.  See also `ivy--preselect-index',
             ;; which this logic roughly mirrors.
             (not (or
                   (and (integerp preselect)
                        (= ivy--index preselect))
                   (equal current preselect)
                   (and (stringp preselect)
                        (stringp current)
                        (string-match-p preselect current))))
             ivy--old-cands
             (cl-position current cands :test #'equal))
        (funcall func re-str cands))))
    (when (or empty (string= name "^"))
      (ivy-set-index
       (or (ivy--preselect-index preselect cands)
           ivy--index)))))

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
                (when (eq n (read (get-text-property 0 'swiper-line-number c)))
                  (setq res i))
                (cl-incf i))
              res))))
    (error 0)))

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

(defvar ivy-flx-limit 200
  "Used to conditionally turn off flx sorting.

When the amount of matching candidates exceeds this limit, then
no sorting is done.")

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
      (ivy-add-face-text-property j (1+ j) (ivy--minibuffer-face i) str))
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
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face (concat str "\n") 'ivy-current-match))
   (lambda (str)
     (concat str "\n"))
   cands
   ""))

(defalias 'ivy-add-face-text-property
  (if (fboundp 'add-face-text-property)
      (lambda (start end face &optional object append)
        (add-face-text-property start end face append object))
    (lambda (start end face &optional object append)
      (funcall (if append
                   #'font-lock-append-text-property
                 #'font-lock-prepend-text-property)
               start end 'face face object)))
  "Compatibility shim for `add-face-text-property'.
Fall back on `font-lock-prepend-text-property' in Emacs versions
prior to 24.4 (`font-lock-append-text-property' when APPEND is
non-nil).
Note: The usual last two arguments are flipped for convenience.")

(defun ivy--highlight-ignore-order (str)
  "Highlight STR, using the ignore-order method."
  (when (consp ivy--old-re)
    (let ((i 1))
      (dolist (re ivy--old-re)
        (when (string-match (car re) str)
          (ivy-add-face-text-property
           (match-beginning 0) (match-end 0)
           (ivy--minibuffer-face i)
           str))
        (cl-incf i))))
  str)

(defun ivy--highlight-fuzzy (str)
  "Highlight STR, using the fuzzy method."
  (if (and ivy--flx-featurep
           (eq (ivy-alist-setting ivy-re-builders-alist) 'ivy--regex-fuzzy))
      (let ((flx-name (ivy--remove-prefix "^" ivy-text)))
        (ivy--flx-propertize
         (cons (flx-score str flx-name ivy--flx-cache) str)))
    (ivy--highlight-default str)))

(defun ivy--highlight-default (str)
  "Highlight STR, using the default method."
  (unless ivy--old-re
    (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
  (let ((start
         (if (and (memq (ivy-state-caller ivy-last) ivy-highlight-grep-commands)
                  (string-match "\\`[^:]+:[^:]+:" str))
             (match-end 0)
           0))
        (regexps
         (if (listp ivy--old-re)
             (mapcar #'car (cl-remove-if-not #'cdr ivy--old-re))
           (list ivy--old-re))))
    (dolist (re regexps)
      (ignore-errors
        (while (and (string-match re str start)
                    (> (- (match-end 0) (match-beginning 0)) 0))
          (setq start (match-end 0))
          (let ((i 0))
            (while (<= i ivy--subexps)
              (let ((face
                     (cond ((zerop ivy--subexps)
                            (cadr ivy-minibuffer-faces))
                           ((zerop i)
                            (car ivy-minibuffer-faces))
                           (t
                            (ivy--minibuffer-face i)))))
                (ivy-add-face-text-property
                 (match-beginning i) (match-end i)
                 face str))
              (cl-incf i)))))))
  str)

(defun ivy--format-minibuffer-line (str)
  "Format line STR for use in minibuffer."
  (let* ((str (ivy-cleanup-string (copy-sequence str)))
         (str (if (eq ivy-display-style 'fancy)
                  (funcall ivy--highlight-function str)
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
      (ivy-add-face-text-property
       olen (length str) 'ivy-completions-annotations str))
    str))

(ivy-set-display-transformer
 'counsel-find-file 'ivy-read-file-transformer)
(ivy-set-display-transformer
 'read-file-name-internal 'ivy-read-file-transformer)

(defun ivy-read-file-transformer (str)
  "Transform candidate STR when reading files."
  (if (ivy--dirname-p str)
      (propertize str 'face 'ivy-subdir)
    str))

(defun ivy--format (cands)
  "Return a string for CANDS suitable for display in the minibuffer.
CANDS is a list of strings."
  (setq ivy--length (length cands))
  (when (>= ivy--index ivy--length)
    (ivy-set-index (max (1- ivy--length) 0)))
  (if (null cands)
      (setf (ivy-state-current ivy-last) "")
    (setf (ivy-state-current ivy-last) (copy-sequence (nth ivy--index cands)))
    (let* ((half-height (/ ivy-height 2))
           (start (max 0 (- ivy--index half-height)))
           (end (min (+ start (1- ivy-height)) ivy--length))
           (start (max 0 (min start (- end (1- ivy-height)))))
           (wnd-cands (cl-subseq cands start end))
           transformer-fn)
      (setq ivy--window-index (- ivy--index start))
      (when (setq transformer-fn (ivy-state-display-transformer-fn ivy-last))
        (with-ivy-window
          (with-current-buffer (ivy-state-buffer ivy-last)
            (setq wnd-cands (mapcar transformer-fn wnd-cands)))))
      (ivy--wnd-cands-to-str wnd-cands))))

(defun ivy--wnd-cands-to-str (wnd-cands)
  (let ((str (concat "\n"
                     (funcall ivy-format-function
                              (mapcar
                               #'ivy--format-minibuffer-line
                               wnd-cands)))))
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
  (let (virtual-buffers)
    (bookmark-maybe-load-default-file)
    (dolist (head (append recentf-list
                          (delete "   - no file -"
                                  (delq nil (mapcar #'bookmark-get-filename
                                                    bookmark-alist)))))
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

(defcustom ivy-ignore-buffers '("\\` ")
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
    (mapcar
     (lambda (x)
       (let* ((buf (get-buffer x))
              (dir (buffer-local-value 'default-directory buf))
              (face (if (and dir
                             (ignore-errors
                               (file-remote-p (abbreviate-file-name dir))))
                        'ivy-remote
                      (cdr (assq (buffer-local-value 'major-mode buf)
                                 ivy-switch-buffer-faces-alist)))))
         (if face
             (propertize x 'face face)
           x)))
     (all-completions str #'internal-complete-buffer predicate))
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

Each element is a list of (NAME TREE).  NAME is a string, it's
recommended to end it with a distinctive snippet e.g. \"{}\" so
that it's easy to distinguish the window configurations.

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

(defun ivy-push-view (&optional arg)
  "Push the current window tree on `ivy-views'.

When ARG is non-nil, replace a selected item on `ivy-views'.

Currently, the split configuration (i.e. horizonal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
  (interactive "P")
  (let* ((view (cl-labels
                   ((ft (tr)
                      (if (consp tr)
                          (if (eq (car tr) t)
                              (cons 'vert
                                    (mapcar #'ft (cddr tr)))
                            (cons 'horz
                                  (mapcar #'ft (cddr tr))))
                        (with-current-buffer (window-buffer tr)
                          (cond (buffer-file-name
                                 (list 'file buffer-file-name (point)))
                                ((eq major-mode 'dired-mode)
                                 (list 'file default-directory (point)))
                                (t
                                 (list 'buffer (buffer-name) (point))))))))
                 (ft (car (window-tree)))))
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
  (cond ((eq (car view) 'vert)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-vertically))
                (views (cdr view))
                (v (pop views)))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (ivy-set-view-recur v))
             (when views
               (setq wnd2 (split-window-vertically))))))
        ((eq (car view) 'horz)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-horizontally))
                (views (cdr view))
                (v (pop views)))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (ivy-set-view-recur v))
             (when views
               (setq wnd2 (split-window-horizontally))))))
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
  (let ((default-directory (buffer-local-value 'default-directory
                                               (or (get-buffer buffer)
                                                   (current-buffer)))))
    (call-interactively (if (functionp 'counsel-find-file)
                            #'counsel-find-file
                          #'find-file))))

(defun ivy--kill-buffer-or-virtual (buffer)
  (if (get-buffer buffer)
      (kill-buffer buffer)
    (setq recentf-list (delete
                        (cdr (assoc buffer ivy--virtual-buffers))
                        recentf-list))))

(defun ivy--kill-buffer-action (buffer)
  "Kill BUFFER."
  (ivy--kill-buffer-or-virtual buffer)
  (unless (buffer-live-p (ivy-state-buffer ivy-last))
    (setf (ivy-state-buffer ivy-last) (current-buffer)))
  (setq ivy--index 0)
  (ivy--reset-state ivy-last))

(defvar ivy-switch-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'ivy-switch-buffer-kill)
    map))

(defun ivy-switch-buffer-kill ()
  "Kill the current buffer in `ivy-switch-buffer'."
  (interactive)
  (let ((bn (ivy-state-current ivy-last)))
    (ivy--kill-buffer-or-virtual bn)
    (unless (buffer-live-p (ivy-state-buffer ivy-last))
      (setf (ivy-state-buffer ivy-last)
            (with-ivy-window (current-buffer))))
    (setf (ivy-state-preselect ivy-last) ivy--index)
    (setq ivy--old-re nil)
    (setq ivy--all-candidates (delete bn ivy--all-candidates))
    (ivy--exhibit)))

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
 `(("i" ,(lambda (x) (insert (if (stringp x) x (car x)))) "insert")
   ("w" ,(lambda (x) (kill-new (if (stringp x) x (car x)))) "copy")))

(defun ivy--switch-buffer-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
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
               res)))))

(ivy-set-display-transformer
 'ivy-switch-buffer 'ivy-switch-buffer-transformer)
(ivy-set-display-transformer
 'internal-complete-buffer 'ivy-switch-buffer-transformer)

(defun ivy-append-face (str face)
  "Append to STR the property FACE."
  (setq str (copy-sequence str))
  (ivy-add-face-text-property 0 (length str) face str t)
  str)

(defun ivy-switch-buffer-transformer (str)
  "Transform candidate STR when switching buffers."
  (let ((b (get-buffer str)))
    (if (and b (buffer-file-name b))
        (cond
          ((and (not (ignore-errors (file-remote-p (buffer-file-name b))))
                (not (verify-visited-file-modtime b)))
           (ivy-append-face str 'ivy-modified-outside-buffer))
          ((buffer-modified-p b)
           (ivy-append-face str 'ivy-modified-buffer))
          (t str))
      str)))

(defun ivy-switch-buffer-occur ()
  "Occur function for `ivy-switch-buffer' using `ibuffer'."
  (ibuffer nil (buffer-name) (list (cons 'name ivy--old-re))))

;;;###autoload
(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (setq this-command #'ivy-switch-buffer)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer))

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

(define-obsolete-function-alias 'ivy-recentf 'counsel-recentf "0.8.0")

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
      (insert (replace-regexp-in-string "  +" " " text t t)))))

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
  ;; Emacs < 24.4 compatibility
  (unless (fboundp 'forward-symbol)
    (require 'thingatpt))
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
  (if (and ivy--directory
           (ivy--dirname-p (ivy-state-current ivy-last)))
      (insert (substring (ivy-state-current ivy-last) 0 -1))
    (insert (ivy-state-current ivy-last))))

(define-obsolete-variable-alias 'ivy--preferred-re-builders
    'ivy-preferred-re-builders "0.10.0")

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

(defun ivy-reverse-i-search ()
  "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibuffer."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (history (symbol-value (ivy-state-history ivy-last)))
        (old-last ivy-last)
        (ivy-recursive-restore nil))
    (ivy-read "Reverse-i-search: "
              (delete-dups (copy-sequence history))
              :action (lambda (x)
                        (ivy--reset-state
                         (setq ivy-last old-last))
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties x))
                        (ivy--cd-maybe)))))

(defun ivy-restrict-to-matches ()
  "Restrict candidates to current input and erase input."
  (interactive)
  (delete-minibuffer-contents)
  (setq ivy--all-candidates
        (ivy--filter ivy-text ivy--all-candidates)))

;;* Occur
(defvar-local ivy-occur-last nil
  "Buffer-local value of `ivy-last'.
Can't re-use `ivy-last' because using e.g. `swiper' in the same
buffer would modify `ivy-last'.")

(defvar ivy-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ivy-occur-click)
    (define-key map (kbd "RET") 'ivy-occur-press-and-switch)
    (define-key map (kbd "j") 'ivy-occur-next-line)
    (define-key map (kbd "k") 'ivy-occur-previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "f") 'ivy-occur-press)
    (define-key map (kbd "g") 'ivy-occur-revert-buffer)
    (define-key map (kbd "a") 'ivy-occur-read-action)
    (define-key map (kbd "o") 'ivy-occur-dispatch)
    (define-key map (kbd "c") 'ivy-occur-toggle-calling)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "R") 'read-only-mode)
    (define-key map (kbd "C-d") 'ivy-occur-delete-candidate)
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

(define-derived-mode ivy-occur-mode fundamental-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-mode-map}"
  (setq-local view-read-only nil))

(defvar ivy-occur-grep-mode-map
  (let ((map (copy-keymap ivy-occur-mode-map)))
    (define-key map (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode)
    (define-key map "w" 'ivy-wgrep-change-to-wgrep-mode)
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

(defvar ivy--occurs-list nil
  "A list of custom occur generators per command.")

(defun ivy-set-occur (cmd occur)
  "Assign CMD a custom OCCUR function."
  (setq ivy--occurs-list
        (plist-put ivy--occurs-list cmd occur)))

(ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
(ivy-set-occur 'ivy-switch-buffer-other-window 'ivy-switch-buffer-occur)

(defun ivy--starts-with-dotslash (str)
  (string-match-p "\\`\\.[/\\]" str))

(defun ivy--occur-insert-lines (cands)
  "Insert CANDS into `ivy-occur' buffer."
  (font-lock-mode -1)
  (dolist (str cands)
    (setq str (ivy--highlight-fuzzy (copy-sequence str)))
    (add-text-properties
     0 (length str)
     '(mouse-face
       highlight
       help-echo "mouse-1: call ivy-action")
     str)
    (insert (if (string-match-p "\\`.[/\\]" str) "" "    ")
            str ?\n))
  (goto-char (point-min))
  (forward-line 4)
  (while (re-search-forward "^.*:[[:digit:]]+:" nil t)
    (ivy-add-face-text-property
     (match-beginning 0) (match-end 0) 'ivy-grep-info nil t)))

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
           (occur-fn (plist-get ivy--occurs-list caller))
           (buffer
            (generate-new-buffer
             (format "*ivy-occur%s \"%s\"*"
                     (if caller
                         (concat " " (prin1-to-string caller))
                       "")
                     ivy-text))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if occur-fn
              (funcall occur-fn)
            (ivy-occur-mode)
            (insert (format "%d candidates:\n" (length ivy--old-cands)))
            (read-only-mode)
            (ivy--occur-insert-lines
             ivy--old-cands)))
        (setf (ivy-state-text ivy-last) ivy-text)
        (setq ivy-occur-last ivy-last)
        (setq-local ivy--directory ivy--directory))
      (ivy-exit-with-action
       (lambda (_) (pop-to-buffer buffer))))))

(defun ivy-occur-revert-buffer ()
  "Refresh the buffer making it up-to date with the collection.

Currently only works for `swiper'.  In that specific case, the
*ivy-occur* buffer becomes nearly useless as the orignal buffer
is updated, since the line numbers no longer match.

Calling this function is as if you called `ivy-occur' on the
updated original buffer."
  (interactive)
  (let ((caller (ivy-state-caller ivy-occur-last))
        (ivy-last ivy-occur-last))
    (cond ((eq caller 'swiper)
           (let ((buffer (ivy-state-buffer ivy-occur-last)))
             (unless (buffer-live-p buffer)
               (error "Buffer was killed"))
             (let ((inhibit-read-only t))
               (erase-buffer)
               (funcall (plist-get ivy--occurs-list caller) t)
               (ivy-occur-grep-mode))))
          ((memq caller '(counsel-git-grep counsel-grep counsel-ag counsel-rg))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (funcall (plist-get ivy--occurs-list caller)))))
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
(defvar counsel-grep-last-line)

(defun ivy--occur-press-update-window ()
  (cl-case (ivy-state-caller ivy-occur-last)
    ((swiper counsel-git-grep counsel-grep counsel-ag counsel-rg)
     (let ((window (ivy-state-window ivy-occur-last))
           (buffer (ivy-state-buffer ivy-occur-last)))
       (when (and (or (not (window-live-p window))
                      (equal window (selected-window)))
                  (buffer-live-p buffer))
         (save-selected-window
           (setf (ivy-state-window ivy-occur-last)
                 (display-buffer buffer))))))

    ((counsel-describe-function counsel-describe-variable)
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
          (looking-at "\\(?:./\\|    \\)\\(.*\\)$"))
    (let* ((ivy-last ivy-occur-last)
           (ivy-text (ivy-state-text ivy-last))
           (str (buffer-substring
                 (match-beginning 1)
                 (match-end 1)))
           (coll (ivy-state-collection ivy-last))
           (action (ivy--get-action ivy-last))
           (ivy-exit 'done))
      (with-ivy-window
        (setq counsel-grep-last-line nil)
        (with-current-buffer (ivy--occur-press-buffer)
          (funcall action
                   (if (and (consp coll)
                            (consp (car coll)))
                       (assoc str coll)
                     str)))
        (if (memq (ivy-state-caller ivy-last)
                  '(swiper counsel-git-grep counsel-grep counsel-ag counsel-rg))
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

(defconst ivy-help-file (let ((default-directory
                               (if load-file-name
                                   (file-name-directory load-file-name)
                                 default-directory)))
                          (if (file-exists-p "ivy-help.org")
                              (expand-file-name "ivy-help.org")
                            (if (file-exists-p "doc/ivy-help.org")
                                (expand-file-name "doc/ivy-help.org"))))
  "The file for `ivy-help'.")

(defun ivy-help ()
  "Help for `ivy'."
  (interactive)
  (let ((buf (get-buffer "*Ivy Help*")))
    (unless buf
      (setq buf (get-buffer-create "*Ivy Help*"))
      (with-current-buffer buf
        (insert-file-contents ivy-help-file)
        (org-mode)
        (view-mode)
        (goto-char (point-min))))
    (if (eq this-command 'ivy-help)
        (switch-to-buffer buf)
      (with-ivy-window
        (pop-to-buffer buf)))
    (view-mode)
    (goto-char (point-min))))

(provide 'ivy)

;;; ivy.el ends here
