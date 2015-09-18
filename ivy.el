;;; ivy.el --- Incremental Vertical completYon -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
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
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
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

;;* Customization
(defgroup ivy nil
  "Incremental vertical completion."
  :group 'convenience)

(defface ivy-current-match
  '((((class color) (background light))
     :background "#1a4b77" :foreground "white")
    (((class color) (background dark))
     :background "#65a7e2" :foreground "black"))
  "Face used by Ivy for highlighting first match.")

(defface ivy-confirm-face
  '((t :foreground "ForestGreen" :inherit minibuffer-prompt))
  "Face used by Ivy to issue a confirmation prompt.")

(defface ivy-match-required-face
  '((t :foreground "red" :inherit minibuffer-prompt))
  "Face used by Ivy to issue a match required prompt.")

(defface ivy-subdir
  '((t (:inherit 'dired-directory)))
  "Face used by Ivy for highlighting subdirs in the alternatives.")

(defface ivy-remote
  '((t (:foreground "#110099")))
  "Face used by Ivy for highlighting remotes in the alternatives.")

(defcustom ivy-height 10
  "Number of lines for the minibuffer window."
  :type 'integer)

(defcustom ivy-count-format "%-4d "
  "The style of showing the current candidate count for `ivy-read'.
Set this to nil if you don't want the count.  You can also set it
to e.g. \"(%d/%d) \" if you want to see both the candidate index
and the candidate count."
  :type '(choice (const :tag "Count disabled" nil) string))

(defcustom ivy-wrap nil
  "Whether to wrap around after the first and last candidate."
  :type 'boolean)

(defcustom ivy-display-style nil
  "The style for formatting the minibuffer.

By default, the matched strings will be copied as they are.

With the fancy method, the matching parts of the regexp will be
additionally highlighted, just like `swiper' does it."
  :type '(choice
          (const :tag "Plain" nil)
          (const :tag "Fancy" fancy)))

(defcustom ivy-on-del-error-function 'minibuffer-keyboard-quit
  "The handler for when `ivy-backward-delete-char' throws.
This is usually meant as a quick exit out of the minibuffer."
  :type 'function)

(defcustom ivy-extra-directories '("../" "./")
  "Add this to the front of the list when completing file names.
Only \"./\" and \"../\" apply here. They appear in reverse order."
  :type 'list)

(defcustom ivy-use-virtual-buffers nil
  "When non-nil, add `recentf-mode' and bookmarks to the list of buffers."
  :type 'boolean)

(defvar ivy--actions-list nil
  "A list of extra actions per command.")

(defun ivy-set-actions (cmd actions)
  "Set CMD extra exit points to ACTIONS."
  (setq ivy--actions-list
        (plist-put ivy--actions-list cmd actions)))

;;* Keymap
(require 'delsel)
(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map (kbd "C-M-m") 'ivy-call)
    (define-key map (kbd "C-j") 'ivy-alt-done)
    (define-key map (kbd "C-M-j") 'ivy-immediate-done)
    (define-key map (kbd "TAB") 'ivy-partial-or-done)
    (define-key map (kbd "C-n") 'ivy-next-line)
    (define-key map (kbd "C-p") 'ivy-previous-line)
    (define-key map (kbd "<down>") 'ivy-next-line)
    (define-key map (kbd "<up>") 'ivy-previous-line)
    (define-key map (kbd "C-s") 'ivy-next-line-or-history)
    (define-key map (kbd "C-r") 'ivy-reverse-i-search)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map (kbd "DEL") 'ivy-backward-delete-char)
    (define-key map (kbd "M-DEL") 'ivy-backward-kill-word)
    (define-key map (kbd "C-d") 'ivy-delete-char)
    (define-key map (kbd "C-f") 'ivy-forward-char)
    (define-key map (kbd "M-d") 'ivy-kill-word)
    (define-key map (kbd "M-<") 'ivy-beginning-of-buffer)
    (define-key map (kbd "M->") 'ivy-end-of-buffer)
    (define-key map (kbd "M-n") 'ivy-next-history-element)
    (define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key map (kbd "C-v") 'ivy-scroll-up-command)
    (define-key map (kbd "M-v") 'ivy-scroll-down-command)
    (define-key map (kbd "C-M-n") 'ivy-next-line-and-call)
    (define-key map (kbd "C-M-p") 'ivy-previous-line-and-call)
    (define-key map (kbd "M-q") 'ivy-toggle-regexp-quote)
    (define-key map (kbd "M-j") 'ivy-yank-word)
    (define-key map (kbd "M-i") 'ivy-insert-current)
    (define-key map (kbd "C-o") 'hydra-ivy/body)
    (define-key map (kbd "M-o") 'ivy-dispatching-done)
    (define-key map (kbd "C-k") 'ivy-kill-line)
    (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (define-key map (kbd "M-w") 'ivy-kill-ring-save)
    map)
  "Keymap used in the minibuffer.")
(autoload 'hydra-ivy/body "ivy-hydra" "" t)

(defvar ivy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap switch-to-buffer] 'ivy-switch-buffer)
    map)
  "Keymap for `ivy-mode'.")

;;* Globals
(cl-defstruct ivy-state
  prompt collection
  predicate require-match initial-input
  history preselect keymap update-fn sort
  ;; The window in which `ivy-read' was called
  window
  action
  unwind
  re-builder
  matcher
  ;; When this is non-nil, call it for each input change to get new candidates
  dynamic-collection)

(defvar ivy-last nil
  "The last parameters passed to `ivy-read'.")

(defsubst ivy-set-action (action)
  (setf (ivy-state-action ivy-last) action))

(defvar ivy-history nil
  "History list of candidates entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")

(defvar ivy--directory nil
  "Current directory when completing file names.")

(defvar ivy--length 0
  "Store the amount of viable candidates.")

(defvar ivy-text ""
  "Store the user's string as it is typed in.")

(defvar ivy--current ""
  "Current candidate.")

(defvar ivy--index 0
  "Store the index of the current candidate.")

(defvar ivy-exit nil
  "Store 'done if the completion was successfully selected.
Otherwise, store nil.")

(defvar ivy--all-candidates nil
  "Store the candidates passed to `ivy-read'.")

(defvar ivy--default nil
  "Default initial input.")

(defvar ivy--prompt nil
  "Store the format-style prompt.
When non-nil, it should contain one %d.")

(defvar ivy--prompt-extra ""
  "Temporary modifications to the prompt.")

(defvar ivy--old-re nil
  "Store the old regexp.")

(defvar ivy--old-cands nil
  "Store the candidates matched by `ivy--old-re'.")

(defvar ivy--regex-function 'ivy--regex
  "Current function for building a regex.")

(defvar ivy--subexps 0
  "Number of groups in the current `ivy--regex'.")

(defvar ivy--full-length nil
  "When :dynamic-collection is non-nil, this can be the total amount of candidates.")

(defvar ivy--old-text ""
  "Store old `ivy-text' for dynamic completion.")

(defvar Info-current-file)

(defmacro ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defmacro with-ivy-window (&rest body)
  "Execute BODY in the window from which `ivy-read' was called."
  (declare (indent 0)
           (debug t))
  `(with-selected-window (ivy-state-window ivy-last)
     ,@body))

(defun ivy--done (text)
  "Insert TEXT and exit minibuffer."
  (if (and ivy--directory
           (not (eq (ivy-state-history ivy-last) 'grep-files-history)))
      (insert (setq ivy--current (expand-file-name
                                  text ivy--directory)))
    (insert (setq ivy--current text)))
  (setq ivy-exit 'done)
  (exit-minibuffer))

;;* Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (delete-minibuffer-contents)
  (cond ((> ivy--length 0)
         (ivy--done ivy--current))
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
         (ivy--exhibit))))

(defun ivy-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (let* ((hint (concat ivy--current
                           "\n"
                           (mapconcat
                            (lambda (x)
                              (format "%s: %s"
                                      (propertize
                                       (car x)
                                       'face 'font-lock-builtin-face)
                                      (nth 2 x)))
                            (cdr actions)
                            "\n")
                           "\n"))
             (key (string (read-key hint)))
             (action (assoc key (cdr actions))))
        (cond ((string= key ""))
              ((null action)
               (error "%s is not bound" key))
              (t
               (message "")
               (ivy-set-action (nth 1 action))
               (ivy-done)))))))

(defun ivy-build-tramp-name (x)
  "Reconstruct X into a path.
Is is a cons cell, related to `tramp-get-completion-function'."
  (let ((user (car x))
        (domain (cadr x)))
    (if user
        (concat user "@" domain)
      domain)))

(declare-function tramp-get-completion-function "tramp")
(declare-function Info-find-node "info")

(defun ivy-alt-done (&optional arg)
  "Exit the minibuffer with the selected candidate.
When ARG is t, exit with current text, ignoring the candidates."
  (interactive "P")
  (let (dir)
    (cond (arg
           (ivy-immediate-done))
          ((and ivy--directory
                (or
                 (and
                  (not (string= ivy--current "./"))
                  (cl-plusp ivy--length)
                  (file-directory-p
                   (setq dir (expand-file-name
                              ivy--current ivy--directory))))))
           (ivy--cd dir)
           (ivy--exhibit))
          ((eq (ivy-state-collection ivy-last) 'Info-read-node-name-1)
           (if (or (equal ivy--current "(./)")
                   (equal ivy--current "(../)"))
               (ivy-quit-and-run
                (ivy-read "Go to file: " 'read-file-name-internal
                          :action (lambda (x)
                                    (Info-find-node
                                     (expand-file-name x ivy--directory)
                                     "Top"))))
             (ivy-done)))
          ((and ivy--directory
                (string-match "\\`/[^/]+:.*:.*\\'" ivy-text))
           (ivy-done))
          ((and ivy--directory
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
             (setq res (cl-delete-duplicates res :test #'equal))
             (let* ((old-ivy-last ivy-last)
                    (enable-recursive-minibuffers t)
                    (host (ivy-read "Find File: "
                                    (mapcar #'ivy-build-tramp-name res)
                                    :initial-input rest)))
               (setq ivy-last old-ivy-last)
               (when host
                 (setq ivy--directory "/")
                 (ivy--cd (concat "/" method ":" host ":"))))))
          (t
           (ivy-done)))))

(defcustom ivy-tab-space nil
  "When non-nil, `ivy-partial-or-done' should insert a space."
  :type 'boolean)

(defun ivy-partial-or-done ()
  "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
  (interactive)
  (if (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (string-match "\\`/" ivy-text))
      (let ((default-directory ivy--directory))
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (and (file-directory-p ivy-text)
                   (= ivy--length 1))
          (ivy--cd (expand-file-name ivy-text))))
    (or (ivy-partial)
        (when (or (eq this-command last-command)
                  (eq ivy--length 1))
          (ivy-alt-done)))))

(defun ivy-partial ()
  "Complete the minibuffer text as much as possible."
  (interactive)
  (let* ((parts (or (split-string ivy-text " " t) (list "")))
         (postfix (car (last parts)))
         (completion-ignore-case t)
         (startp (string-match "^\\^" postfix))
         (new (try-completion (if startp
                                  (substring postfix 1)
                                postfix)
                              (mapcar (lambda (str) (substring str (string-match postfix str)))
                                      ivy--old-cands))))
    (cond ((eq new t) nil)
          ((string= new ivy-text) nil)
          (new
           (delete-region (minibuffer-prompt-end) (point-max))
           (setcar (last parts)
                   (if startp
                       (concat "^" new)
                     new))
           (insert (mapconcat #'identity parts " ")
                   (if ivy-tab-space " " ""))
           t))))

(defun ivy-immediate-done ()
  "Exit the minibuffer with the current input."
  (interactive)
  (delete-minibuffer-contents)
  (insert (setq ivy--current ivy-text))
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defun ivy-resume ()
  "Resume the last completion session."
  (interactive)
  (ivy-read
   (ivy-state-prompt ivy-last)
   (ivy-state-collection ivy-last)
   :predicate (ivy-state-predicate ivy-last)
   :require-match (ivy-state-require-match ivy-last)
   :initial-input ivy-text
   :history (ivy-state-history ivy-last)
   :preselect (unless (eq (ivy-state-collection ivy-last)
                          'read-file-name-internal)
                (regexp-quote ivy--current))
   :keymap (ivy-state-keymap ivy-last)
   :update-fn (ivy-state-update-fn ivy-last)
   :sort (ivy-state-sort ivy-last)
   :action (ivy-state-action ivy-last)
   :unwind (ivy-state-unwind ivy-last)
   :re-builder (ivy-state-re-builder ivy-last)
   :matcher (ivy-state-matcher ivy-last)
   :dynamic-collection (ivy-state-dynamic-collection ivy-last)))

(defvar ivy-calling nil
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
  (ivy-set-index (min (+ ivy--index ivy-height)
                      (1- ivy--length))))

(defun ivy-scroll-down-command ()
  "Scroll the candidates downward by the minibuffer height."
  (interactive)
  (ivy-set-index (max (- ivy--index ivy-height)
                      0)))
(defun ivy-minibuffer-grow ()
  "Grow the minibuffer window by 1 line."
  (interactive)
  (setq-local max-mini-window-height
              (cl-incf ivy-height)))

(defun ivy-minibuffer-shrink ()
  "Shrink the minibuffer window by 1 line."
  (interactive)
  (unless (<= ivy-height 2)
    (setq-local max-mini-window-height
                (cl-decf ivy-height))
    (window-resize (selected-window) -1)))

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
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (ivy-next-line arg))

(defun ivy-previous-line (&optional arg)
  "Move cursor vertically up ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (- ivy--index arg)))
    (if (< index 0)
        (if ivy-wrap
            (ivy-end-of-buffer)
          (ivy-set-index 0))
      (ivy-set-index index))))

(defun ivy-previous-line-or-history (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (ivy-previous-line arg))

(defun ivy-toggle-calling ()
  "Flip `ivy-calling'."
  (interactive)
  (when (setq ivy-calling (not ivy-calling))
    (ivy-call)))

(defun ivy--get-action (state)
  "Get the action function from STATE."
  (let ((action (ivy-state-action state)))
    (when action
      (if (functionp action)
          action
        (cadr (nth (car action) action))))))

(defun ivy--actionp (x)
  "Return non-nil when X is a list of actions."
  (and x (listp x) (not (eq (car x) 'closure))))

(defun ivy-next-action ()
  "When the current action is a list, scroll it forwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (unless (>= (car action) (1- (length action)))
        (cl-incf (car action))))))

(defun ivy-prev-action ()
  "When the current action is a list, scroll it backwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (unless (<= (car action) 1)
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

(defun ivy-call ()
  "Call the current action without exiting completion."
  (interactive)
  (let ((action (ivy--get-action ivy-last)))
    (when action
      (let* ((collection (ivy-state-collection ivy-last))
             (x (if (and (consp collection)
                         (consp (car collection)))
                    (cdr (assoc ivy--current collection))
                  (if (equal ivy--current "")
                      ivy-text
                    ivy--current))))
        (funcall action x)))))

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
  (next-history-element arg)
  (ivy--cd-maybe)
  (move-end-of-line 1)
  (ivy--maybe-scroll-history))

(defun ivy--cd-maybe ()
  "Check if the current input points to a different directory.
If so, move to that directory, while keeping only the file name."
  (when ivy--directory
    (let* ((input (expand-file-name (ivy--input)))
           (file (file-name-nondirectory input))
           (dir (expand-file-name (file-name-directory input))))
      (if (string= dir ivy--directory)
          (progn
            (delete-minibuffer-contents)
            (insert file))
        (ivy--cd dir)
        (insert file)))))

(defun ivy--maybe-scroll-history ()
  "If the selected history element has an index, scroll there."
  (let ((idx (ignore-errors
               (get-text-property
                (minibuffer-prompt-end)
                'ivy-index))))
    (when idx
      (ivy--exhibit)
      (setq ivy--index idx))))

(defun ivy--cd (dir)
  "When completing file names, move to directory DIR."
  (if (null ivy--directory)
      (error "Unexpected")
    (setq ivy--old-cands nil)
    (setq ivy--old-re nil)
    (setq ivy--index 0)
    (setq ivy--all-candidates
          (ivy--sorted-files (setq ivy--directory dir)))
    (setq ivy-text "")
    (delete-minibuffer-contents)))

(defun ivy-backward-delete-char ()
  "Forward to `backward-delete-char'.
On error (read-only), call `ivy-on-del-error-function'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (file-name-directory
                  (directory-file-name
                   (expand-file-name
                    ivy--directory))))
        (ivy--exhibit))
    (condition-case nil
        (backward-delete-char 1)
      (error
       (when ivy-on-del-error-function
         (funcall ivy-on-del-error-function))))))

(defun ivy-delete-char (arg)
  "Forward to `delete-char' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (delete-char arg)))

(defun ivy-forward-char (arg)
  "Forward to `forward-char' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (forward-char arg)))

(defun ivy-kill-word (arg)
  "Forward to `kill-word' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (kill-word arg)))

(defun ivy-kill-line ()
  "Forward to `kill-line'."
  (interactive)
  (if (eolp)
      (kill-region (minibuffer-prompt-end) (point))
    (kill-line)))

(defun ivy-backward-kill-word ()
  "Forward to `backward-kill-word'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (file-name-directory
                  (directory-file-name
                   (expand-file-name
                    ivy--directory))))
        (ivy--exhibit))
    (ignore-errors
      (let ((pt (point)))
        (forward-word -1)
        (delete-region (point) pt)))))

(defvar ivy--regexp-quote 'regexp-quote
  "Store the regexp quoting state.")

(defun ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function ivy--regexp-quote))

(defun ivy-sort-file-function-default (x y)
  "Compare two files X and Y.
Prioritize directories."
  (if (get-text-property 0 'dirp x)
      (if (get-text-property 0 'dirp y)
          (string< x y)
        t)
    (if (get-text-property 0 'dirp y)
        nil
      (string< x y))))

(defvar ivy-sort-functions-alist
  '((read-file-name-internal . ivy-sort-file-function-default)
    (internal-complete-buffer . nil)
    (counsel-git-grep-function . nil)
    (Man-goto-section . nil)
    (org-refile . nil)
    (t . string-lessp))
  "An alist of sorting functions for each collection function.
Interactive functions that call completion fit in here as well.

For each entry, nil means no sorting.  It's very useful to turn
off the sorting for functions that have candidates in the natural
buffer order, like `org-refile' or `Man-goto-section'.

The entry associated to t is used for all fall-through cases.")

(defvar ivy-re-builders-alist
  '((t . ivy--regex-plus))
  "An alist of regex building functions for each collection function.
Each function should take a string and return a valid regex or a
regex sequence (see below).

The entry associated to t is used for all fall-through cases.
Possible choices: `ivy--regex', `regexp-quote', `ivy--regex-plus'.

In case a function returns a list, it should look like this:
'((\"matching-regexp\" . t) (\"non-matching-regexp\") ...).

The matches will be filtered in a sequence, you can mix the
regexps that should match and that should not match as you
like.")

(defvar ivy-initial-inputs-alist
  '((org-refile . "^")
    (org-agenda-refile . "^")
    (org-capture-refile . "^")
    (counsel-M-x . "^")
    (counsel-describe-function . "^")
    (counsel-describe-variable . "^")
    (man . "^")
    (woman . "^"))
  "Command to initial input table.")

(defcustom ivy-sort-max-size 30000
  "Sorting won't be done for collections larger than this."
  :type 'integer)

(defun ivy--sorted-files (dir)
  "Return the list of files in DIR.
Directories come first."
  (let* ((default-directory dir)
         (seq (all-completions "" 'read-file-name-internal))
         sort-fn)
    (if (equal dir "/")
        seq
      (setq seq (delete "./" (delete "../" seq)))
      (when (eq (setq sort-fn (cdr (assoc 'read-file-name-internal
                                          ivy-sort-functions-alist)))
                #'ivy-sort-file-function-default)
        (setq seq (mapcar (lambda (x)
                            (propertize x 'dirp (string-match-p "/\\'" x)))
                          seq)))
      (when sort-fn
        (setq seq (cl-sort seq sort-fn)))
      (dolist (dir ivy-extra-directories)
        (push dir seq))
      seq)))

;;** Entry Point
(cl-defun ivy-read (prompt collection
                    &key predicate require-match initial-input
                      history preselect keymap update-fn sort
                      action unwind re-builder matcher dynamic-collection)
  "Read a string in the minibuffer, with completion.

PROMPT is a string to prompt with; normally it ends in a colon
and a space.  When PROMPT contains %d, it will be updated with
the current number of matching candidates.  If % appears elsewhere
in the PROMPT it should be quoted as %%.
See also `ivy-count-format'.

COLLECTION is a list of strings.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.

KEYMAP is composed together with `ivy-minibuffer-map'.

If PRESELECT is non-nil select the corresponding candidate out of
the ones that match INITIAL-INPUT.

UPDATE-FN is called each time the current candidate(s) is changed.

When SORT is t, refer to `ivy-sort-functions-alist' for sorting.

ACTION is a lambda to call after a result was selected. It should
take a single argument, usually a string.

UNWIND is a lambda to call before exiting.

RE-BUILDER is a lambda that transforms text into a regex.

MATCHER can completely override matching.

DYNAMIC-COLLECTION is a function to call to update the list of
candidates with each input."
  (let ((extra-actions (plist-get ivy--actions-list this-command)))
    (when extra-actions
      (setq action
            (if (functionp action)
                `(1
                  ("o" ,action "default")
                  ,@extra-actions)
              (delete-dups (append action extra-actions))))))
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
         :window (selected-window)
         :unwind unwind
         :re-builder re-builder
         :matcher matcher
         :dynamic-collection dynamic-collection))
  (ivy--reset-state ivy-last)
  (prog1
      (unwind-protect
           (minibuffer-with-setup-hook
               #'ivy--minibuffer-setup
             (let* ((hist (or history 'ivy-history))
                    (minibuffer-completion-table collection)
                    (minibuffer-completion-predicate predicate)
                    (res (read-from-minibuffer
                          prompt
                          (ivy-state-initial-input ivy-last)
                          (make-composed-keymap keymap ivy-minibuffer-map)
                          nil
                          hist)))
               (when (eq ivy-exit 'done)
                 (let ((item (if ivy--directory
                                 ivy--current
                               ivy-text)))
                   (unless (equal item "")
                     (set hist (cons (propertize item 'ivy-index ivy--index)
                                     (delete item
                                             (cdr (symbol-value hist)))))))
                 res)))
        (remove-hook 'post-command-hook #'ivy--exhibit)
        (when (setq unwind (ivy-state-unwind ivy-last))
          (funcall unwind)))
    (ivy-call)))

(defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
  (let ((prompt (ivy-state-prompt state))
        (collection (ivy-state-collection state))
        (predicate (ivy-state-predicate state))
        (history (ivy-state-history state))
        (preselect (ivy-state-preselect state))
        (sort (ivy-state-sort state))
        (re-builder (ivy-state-re-builder state))
        (dynamic-collection (ivy-state-dynamic-collection state))
        (initial-input (ivy-state-initial-input state))
        (require-match (ivy-state-require-match state))
        (matcher (ivy-state-matcher state)))
    (unless initial-input
      (setq initial-input (cdr (assoc this-command
                                      ivy-initial-inputs-alist))))
    (setq ivy--directory nil)
    (setq ivy--regex-function
          (or re-builder
              (and (functionp collection)
                   (cdr (assoc collection ivy-re-builders-alist)))
              (cdr (assoc t ivy-re-builders-alist))
              'ivy--regex))
    (setq ivy--subexps 0)
    (setq ivy--regexp-quote 'regexp-quote)
    (setq ivy--old-text "")
    (setq ivy--full-length nil)
    (setq ivy-text "")
    (setq ivy-calling nil)
    (let (coll sort-fn)
      (cond ((eq collection 'Info-read-node-name-1)
             (if (equal Info-current-file "dir")
                 (setq coll
                       (mapcar (lambda (x) (format "(%s)" x))
                               (cl-delete-duplicates
                                (all-completions "(" collection predicate)
                                :test #'equal)))
               (setq coll (all-completions "" collection predicate))))
            ((eq collection 'read-file-name-internal)
             (setq ivy--directory default-directory)
             (require 'dired)
             (when preselect
               (let ((preselect-directory (file-name-directory preselect)))
                 (unless (or (null preselect-directory)
                             (string= preselect-directory
                                      default-directory))
                   (setq ivy--directory preselect-directory))
                 (setq preselect (file-name-nondirectory preselect))))
             (setq coll (ivy--sorted-files ivy--directory))
             (when initial-input
               (unless (or require-match
                           (equal initial-input default-directory)
                           (equal initial-input ""))
                 (setq coll (cons initial-input coll)))
               (setq initial-input nil)))
            ((eq collection 'internal-complete-buffer)
             (setq coll (ivy--buffer-list "" ivy-use-virtual-buffers)))
            ((or (functionp collection)
                 (byte-code-function-p collection)
                 (vectorp collection)
                 (listp (car collection)))
             (setq coll (all-completions "" collection predicate)))
            ((hash-table-p collection)
             (error "Hash table as a collection unsupported"))
            (t
             (setq coll collection)))
      (when sort
        (if (and (functionp collection)
                 (setq sort-fn (assoc collection ivy-sort-functions-alist)))
            (when (and (setq sort-fn (cdr sort-fn))
                       (not (eq collection 'read-file-name-internal)))
              (setq coll (cl-sort coll sort-fn)))
          (unless (eq history 'org-refile-history)
            (if (and (setq sort-fn (cdr (assoc t ivy-sort-functions-alist)))
                     (<= (length coll) ivy-sort-max-size))
                (setq coll (cl-sort (copy-sequence coll) sort-fn))))))
      (when preselect
        (unless (or (and require-match
                         (not (eq collection 'internal-complete-buffer)))
                    (let ((re (format "\\`%s" (regexp-quote preselect))))
                      (cl-find-if (lambda (x) (string-match re x))
                                  coll)))
          (setq coll (cons preselect coll))))
      (setq ivy--index (or
                        (and dynamic-collection
                             ivy--index)
                        (and preselect
                             (ivy--preselect-index
                              coll initial-input preselect matcher))
                        0))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (setq ivy--all-candidates coll))
    (setq ivy-exit nil)
    (setq ivy--default (or (thing-at-point 'symbol) ""))
    (setq ivy--prompt
          (cond ((string-match "%.*d" prompt)
                 prompt)
                ((null ivy-count-format)
                 nil)
                ((string-match "%d.*%d" ivy-count-format)
                 (let ((w (length (number-to-string
                                   (length ivy--all-candidates))))
                       (s (copy-sequence ivy-count-format)))
                   (string-match "%d" s)
                   (match-end 0)
                   (string-match "%d" s (match-end 0))
                   (setq s (replace-match (format "%%-%dd" w) nil nil s))
                   (string-match "%d" s)
                   (concat (replace-match (format "%%%dd" w) nil nil s)
                           prompt)))
                ((string-match "%.*d" ivy-count-format)
                 (concat ivy-count-format prompt))
                (ivy--directory
                 prompt)
                (t
                 nil)))
    (setf (ivy-state-initial-input ivy-last) initial-input)))

(defun ivy-completing-read (prompt collection
                            &optional predicate require-match initial-input
                              history def _inherit-input-method)
  "Read a string in the minibuffer, with completion.

This is an interface that conforms to `completing-read', so that
it can be used for `completing-read-function'.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is considered boolean. See `completing-read'.
INITIAL-INPUT is a string that can be inserted into the minibuffer initially.
_HISTORY is ignored for now.
DEF is the default value.
_INHERIT-INPUT-METHOD is ignored for now.

The history, defaults and input-method arguments are ignored for now."
  (ivy-read (replace-regexp-in-string "%" "%%" prompt)
            collection
            :predicate predicate
            :require-match require-match
            :initial-input (if (consp initial-input)
                               (car initial-input)
                             (if (and (stringp initial-input)
                                      (string-match "\\+" initial-input))
                                 (replace-regexp-in-string
                                  "\\+" "\\\\+" initial-input)
                               initial-input))
            :preselect (if (listp def) (car def) def)
            :history history
            :keymap nil
            :sort
            (let ((sort (assoc this-command ivy-sort-functions-alist)))
              (if sort
                  (cdr sort)
                t))))

;;;###autoload
(define-minor-mode ivy-mode
  "Toggle Ivy mode on or off.
With ARG, turn Ivy mode on if arg is positive, off otherwise.
Turning on Ivy mode will set `completing-read-function' to
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
      (setq completing-read-function 'ivy-completing-read)
    (setq completing-read-function 'completing-read-default)))

(defun ivy--preselect-index (candidates initial-input preselect matcher)
  "Return the index in CANDIDATES filtered by INITIAL-INPUT for PRESELECT.
When MATCHER is non-nil it's used instead of `cl-remove-if-not'."
  (if initial-input
      (progn
        (setq initial-input (ivy--regex-plus initial-input))
        (setq candidates
              (if matcher
                  (funcall matcher initial-input candidates)
                (cl-remove-if-not
                 (lambda (x)
                   (string-match initial-input x))
                 candidates))))
    (when matcher
      (setq candidates (funcall matcher "" candidates))))
  (or (cl-position preselect candidates :test #'equal)
      (cl-position-if
       (lambda (x)
         (string-match (regexp-quote preselect) x))
       candidates)))

;;* Implementation
;;** Regex
(defvar ivy--regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defun ivy--split (str)
  "Split STR into a list by single spaces.
The remaining spaces stick to their left.
This allows to \"quote\" N spaces by inputting N+1 spaces."
  (let ((len (length str))
        start0
        (start1 0)
        res s
        match-len)
    (while (and (string-match " +" str start1)
                (< start1 len))
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
        (setq start1 (1- (match-end 0)))))
    (if start0
        (push (substring str start0) res)
      (setq s (substring str start1))
      (unless (= (length s) 0)
        (push s res)))
    (nreverse res)))

(defun ivy--regex (str &optional greedy)
  "Re-build regex from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str ivy--regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
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
                            (if (string-match "\\`\\\\(.*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy
                              ".*"
                            ".*?")))))
                    ivy--regex-hash)))))

(defun ivy--regex-ignore-order (str)
  "Re-build regex from STR by splitting it on spaces.
Ignore the order of each group."
  (let* ((subs (split-string str " +" t))
         (len (length subs)))
    (cl-case len
      (1
       (setq ivy--subexps 0)
       (car subs))
      (t
       (setq ivy--subexps len)
       (let ((all (mapconcat #'identity subs "\\|")))
         (mapconcat
          (lambda (x)
            (if (string-match "\\`\\\\(.*\\\\)\\'" x)
                x
              (format "\\(%s\\)" x)))
          (make-list len all)
          ".*?"))))))

(defun ivy--regex-plus (str)
  "Build a regex sequence from STR.
Spaces are wild, everything before \"!\" should match.
Everything after \"!\" should not match."
  (let ((parts (split-string str "!" t)))
    (cl-case (length parts)
      (0
       "")
      (1
       (ivy--regex (car parts)))
      (2
       (let ((res
              (mapcar #'list
                      (split-string (cadr parts) " " t))))
         (cons (cons (ivy--regex (car parts)) t)
               res)))
      (t (error "Unexpected: use only one !")))))

(defun ivy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (concat (match-string 1 str)
              (mapconcat #'string (string-to-list (match-string 2 str)) ".*")
              (match-string 3 str))
    str))

;;** Rest
(defun ivy--minibuffer-setup ()
  "Setup ivy completion in the minibuffer."
  (set (make-local-variable 'completion-show-inline-help) nil)
  (set (make-local-variable 'minibuffer-default-add-function)
       (lambda ()
         (list ivy--default)))
  (when (and (display-graphic-p) (= (length (frame-list)) 1))
    (setq truncate-lines t))
  (setq-local max-mini-window-height ivy-height)
  (add-hook 'post-command-hook #'ivy--exhibit nil t)
  ;; show completions with empty input
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

(defun ivy--insert-prompt ()
  "Update the prompt according to `ivy--prompt'."
  (when ivy--prompt
    (unless (memq this-command '(ivy-done ivy-alt-done ivy-partial-or-done
                                 counsel-find-symbol))
      (setq ivy--prompt-extra ""))
    (let (head tail)
      (if (string-match "\\(.*\\): \\'" ivy--prompt)
          (progn
            (setq head (match-string 1 ivy--prompt))
            (setq tail ": "))
        (setq head (substring ivy--prompt 0 -1))
        (setq tail " "))
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
               tail)
              (if ivy--directory
                  (abbreviate-file-name ivy--directory)
                ""))))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point-min) (minibuffer-prompt-end))
          (set-text-properties 0 (length n-str)
                               `(face minibuffer-prompt ,@std-props)
                               n-str)
          (ivy--set-match-props n-str "confirm"
                                `(face ivy-confirm-face ,@std-props))
          (ivy--set-match-props n-str "match required"
                                `(face ivy-match-required-face ,@std-props))
          (insert n-str))
        ;; get out of the prompt area
        (constrain-to-field nil (point-max))))))

(defun ivy--set-match-props (str match props)
  "Set STR text proprties that match MATCH to PROPS."
  (when (string-match match str)
    (set-text-properties
     (match-beginning 0)
     (match-end 0)
     props
     str)))

(defvar inhibit-message)

(defun ivy--sort-maybe (collection)
  "Sort COLLECTION if needed."
  (let ((sort (ivy-state-sort ivy-last))
        entry)
    (if (null sort)
        collection
      (let ((sort-fn (cond ((functionp sort)
                            sort)
                           ((setq entry (assoc (ivy-state-collection ivy-last)
                                               ivy-sort-functions-alist))
                            (cdr entry))
                           (t
                            (cdr (assoc t ivy-sort-functions-alist))))))
        (if (functionp sort-fn)
            (cl-sort (copy-sequence collection) sort-fn)
          collection)))))

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--exhibit post-command-hook)
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
          (when ivy--all-candidates
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))))
      (cond (ivy--directory
             (if (string-match "/\\'" ivy-text)
                 (if (member ivy-text ivy--all-candidates)
                     (ivy--cd (expand-file-name ivy-text ivy--directory))
                   (when (string-match "//\\'" ivy-text)
                     (if (and default-directory
                              (string-match "\\`[[:alpha:]]:/" default-directory))
                         (ivy--cd (match-string 0 default-directory))
                       (ivy--cd "/")))
                   (when (string-match "[[:alpha:]]:/" ivy-text)
                     (let ((drive-root (match-string 0 ivy-text)))
                       (when (file-exists-p drive-root)
                         (ivy--cd drive-root)))))
               (if (string-match "\\`~\\'" ivy-text)
                   (ivy--cd (expand-file-name "~/")))))
            ((eq (ivy-state-collection ivy-last) 'internal-complete-buffer)
             (when (or (and (string-match "\\` " ivy-text)
                            (not (string-match "\\` " ivy--old-text)))
                       (and (string-match "\\` " ivy--old-text)
                            (not (string-match "\\` " ivy-text))))
               (setq ivy--all-candidates
                     (if (and (> (length ivy-text) 0)
                              (eq (aref ivy-text 0)
                                  ?\ ))
                         (ivy--buffer-list " ")
                       (ivy--buffer-list "" ivy-use-virtual-buffers)))
               (setq ivy--old-re nil))))
      (ivy--insert-minibuffer
       (ivy--format
        (ivy--filter ivy-text ivy--all-candidates)))
      (setq ivy--old-text ivy-text))))

(defun ivy--insert-minibuffer (text)
  "Insert TEXT into minibuffer with appropriate cleanup."
  (let ((resize-mini-windows nil)
        (update-fn (ivy-state-update-fn ivy-last))
        deactivate-mark)
    (ivy--cleanup)
    (when update-fn
      (funcall update-fn))
    (ivy--insert-prompt)
    ;; Do nothing if while-no-input was aborted.
    (when (stringp text)
      (let ((buffer-undo-list t))
        (save-excursion
          (forward-line 1)
          (insert text))))
    (when (display-graphic-p)
      (ivy--resize-minibuffer-to-fit))))

(defun ivy--resize-minibuffer-to-fit ()
  "Resize the minibuffer window so it has enough space to display
all of the text contained in the minibuffer."
  (with-selected-window (minibuffer-window)
    (if (fboundp 'window-text-pixel-size)
        (let ((text-height (cdr (window-text-pixel-size)))
              (body-height (window-body-height nil t)))
          (when (> text-height body-height)
            (window-resize nil (- text-height body-height) nil t t)))
      (fit-window-to-buffer))))

(declare-function colir-blend-face-background "ext:colir")

(defun ivy--add-face (str face)
  "Propertize STR with FACE.
`font-lock-append-text-property' is used, since it's better than
`propertize' or `add-face-text-property' in this case."
  (require 'colir)
  (condition-case nil
      (progn
        (colir-blend-face-background 0 (length str) face str)
        (let ((foreground (face-foreground face)))
          (when foreground
            (add-face-text-property
             0 (length str)
             `(:foreground ,foreground)
             nil
             str))))
    (error
     (ignore-errors
       (font-lock-append-text-property 0 (length str) 'face face str))))
  str)

(defun ivy--filter (name candidates)
  "Return all items that match NAME in CANDIDATES.
CANDIDATES are assumed to be static."
  (let* ((re (funcall ivy--regex-function name))
         (re-str (if (listp re) (caar re) re))
         (matcher (ivy-state-matcher ivy-last))
         (case-fold-search (string= name (downcase name)))
         (cands (cond
                  (matcher
                   (funcall matcher re candidates))
                  ((and (equal re ivy--old-re)
                        ivy--old-cands)
                   ivy--old-cands)
                  ((and ivy--old-re
                        (stringp re)
                        (stringp ivy--old-re)
                        (not (string-match "\\\\" ivy--old-re))
                        (not (equal ivy--old-re ""))
                        (memq (cl-search
                               (if (string-match "\\\\)\\'" ivy--old-re)
                                   (substring ivy--old-re 0 -2)
                                 ivy--old-re)
                               re)
                              '(0 2)))
                   (ignore-errors
                     (cl-remove-if-not
                      (lambda (x) (string-match re x))
                      ivy--old-cands)))
                  (t
                   (let ((re-list (if (stringp re) (list (cons re t)) re))
                         (res candidates))
                     (dolist (re re-list)
                       (setq res
                             (ignore-errors
                               (funcall
                                (if (cdr re)
                                    #'cl-remove-if-not
                                  #'cl-remove-if)
                                (let ((re (car re)))
                                  (lambda (x) (string-match re x)))
                                res))))
                     res))))
         (tail (nthcdr ivy--index ivy--old-cands))
         idx)
    (when (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
      (unless (and (not (equal re-str ivy--old-re))
                   (or (setq ivy--index
                             (or
                              (cl-position (if (and (> (length re-str) 0)
                                                    (eq ?^ (aref re-str 0)))
                                               (substring re-str 1)
                                             re-str) cands
                                             :test #'equal)
                              (and ivy--directory
                                   (cl-position
                                    (concat re-str "/") cands
                                    :test #'equal))))))
        (while (and tail (null idx))
          ;; Compare with eq to handle equal duplicates in cands
          (setq idx (cl-position (pop tail) cands)))
        (setq ivy--index (or idx 0))))
    (when (and (string= name "") (not (equal ivy--old-re "")))
      (setq ivy--index
            (or (cl-position (ivy-state-preselect ivy-last)
                             cands :test #'equal)
                ivy--index)))
    (setq ivy--old-re (if cands re-str ""))
    (setq ivy--old-cands cands)))

(defvar ivy-format-function 'ivy-format-function-default
  "Function to transform the list of candidates into a string.
This string will be inserted into the minibuffer.")

(defun ivy-format-function-default (cands)
  "Transform CANDS into a string for minibuffer."
  (if (bound-and-true-p truncate-lines)
      (mapconcat #'identity cands "\n")
    (let ((ww (- (window-width)
                 (if (and (boundp 'fringe-mode) (eq fringe-mode 0)) 1 0))))
      (mapconcat
       (lambda (s)
         (if (> (length s) ww)
             (concat (substring s 0 (- ww 3)) "...")
           s))
       cands "\n"))))

(defun ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (let ((i -1))
    (mapconcat
     (lambda (s)
       (concat (if (eq (cl-incf i) ivy--index)
                   "> "
                 "  ")
               s))
     cands "\n")))

(defcustom swiper-minibuffer-faces
  '(swiper-minibuffer-match-face-1
    swiper-minibuffer-match-face-2
    swiper-minibuffer-match-face-3
    swiper-minibuffer-match-face-4)
  "List of `swiper' faces for minibuffer group matches.")

(defun ivy--format-minibuffer-line (str)
  (let ((start 0)
        (str (copy-sequence str)))
    (when (eq ivy-display-style 'fancy)
      (unless ivy--old-re
        (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
      (while (and (string-match ivy--old-re str start)
                  (> (- (match-end 0) (match-beginning 0)) 0))
        (setq start (match-end 0))
        (let ((i 0))
          (while (<= i ivy--subexps)
            (let ((face
                   (cond ((zerop ivy--subexps)
                          (cadr swiper-minibuffer-faces))
                         ((zerop i)
                          (car swiper-minibuffer-faces))
                         (t
                          (nth (1+ (mod (+ i 2) (1- (length swiper-minibuffer-faces))))
                               swiper-minibuffer-faces)))))
              (add-face-text-property
               (match-beginning i)
               (match-end i)
               face
               nil
               str))
            (cl-incf i)))))
    str))

(defun ivy--format (cands)
  "Return a string for CANDS suitable for display in the minibuffer.
CANDS is a list of strings."
  (setq ivy--length (length cands))
  (when (>= ivy--index ivy--length)
    (setq ivy--index (max (1- ivy--length) 0)))
  (if (null cands)
      (setq ivy--current "")
    (let* ((half-height (/ ivy-height 2))
           (start (max 0 (- ivy--index half-height)))
           (end (min (+ start (1- ivy-height)) ivy--length))
           (start (max 0 (min start (- end (1- ivy-height)))))
           (cands (cl-subseq cands start end))
           (index (- ivy--index start)))
      (when ivy--directory
        (setq cands (mapcar (lambda (x)
                              (if (string-match-p "/\\'" x)
                                  (propertize x 'face 'ivy-subdir)
                                x))
                            cands)))
      (setq ivy--current (copy-sequence (nth index cands)))
      (setq cands (mapcar
                   #'ivy--format-minibuffer-line
                   cands))
      (setf (nth index cands)
            (ivy--add-face (nth index cands) 'ivy-current-match))
      (let* ((ivy--index index)
             (res (concat "\n" (funcall ivy-format-function cands))))
        (put-text-property 0 (length res) 'read-only nil res)
        res))))

(defvar ivy--virtual-buffers nil
  "Store the virtual buffers alist.")

(defvar recentf-list)

(defface ivy-virtual '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for matching virtual buffer names.")

(defun ivy--virtual-buffers ()
  "Adapted from `ido-add-virtual-buffers-to-list'."
  (unless recentf-mode
    (recentf-mode 1))
  (let ((bookmarks (and (boundp 'bookmark-alist)
                        bookmark-alist))
        virtual-buffers name)
    (dolist (head (append
                   recentf-list
                   (delete "   - no file -"
                           (delq nil (mapcar (lambda (bookmark)
                                               (cdr (assoc 'filename bookmark)))
                                             bookmarks)))))
      (setq name (file-name-nondirectory head))
      (when (equal name "")
        (setq name (file-name-nondirectory (directory-file-name head))))
      (when (equal name "")
        (setq name head))
      (and (not (equal name ""))
           (null (get-file-buffer head))
           (not (assoc name virtual-buffers))
           (push (cons name head) virtual-buffers)))
    (when virtual-buffers
      (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
                           'face 'ivy-virtual
                           (car comp)))
      (setq ivy--virtual-buffers (nreverse virtual-buffers))
      (mapcar #'car ivy--virtual-buffers))))

(defun ivy--buffer-list (str &optional virtual)
  "Return the buffers that match STR.
When VIRTUAL is non-nil, add virtual buffers."
  (delete-dups
   (append
    (mapcar
     (lambda (x)
       (if (with-current-buffer x
             (file-remote-p
              (abbreviate-file-name default-directory)))
           (propertize x 'face 'ivy-remote)
         x))
     (all-completions str 'internal-complete-buffer))
    (and virtual
         (ivy--virtual-buffers)))))

(defun ivy--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (with-ivy-window
    (if (zerop (length buffer))
        (switch-to-buffer
         ivy-text nil 'force-same-window)
      (let ((virtual (assoc buffer ivy--virtual-buffers)))
        (if (and virtual
                 (not (get-buffer buffer)))
            (find-file (cdr virtual))
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

(defvar ivy-switch-buffer-map (make-sparse-keymap))

(ivy-set-actions
 'ivy-switch-buffer
 '(("k"
    (lambda (x)
      (kill-buffer x)
      (ivy--reset-state ivy-last))
    "kill")
   ("j"
    ivy--switch-buffer-other-window-action
    "other")
   ("r"
    ivy--rename-buffer-action
    "rename")))

(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (if (not ivy-mode)
      (call-interactively 'switch-to-buffer)
    (let ((this-command 'ivy-switch-buffer))
      (ivy-read "Switch to buffer: " 'internal-complete-buffer
                :preselect (buffer-name (other-buffer (current-buffer)))
                :action #'ivy--switch-buffer-action
                :keymap ivy-switch-buffer-map))))

(defun ivy-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (ivy-read "Recentf: " recentf-list
            :action
            (lambda (f)
              (with-ivy-window
                (find-file f)))))

(defun ivy-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (let (amend)
    (with-ivy-window
      (let ((pt (point))
            (le (line-end-position)))
        (forward-word 1)
        (if (> (point) le)
            (goto-char pt)
          (setq amend (buffer-substring-no-properties pt (point))))))
    (when amend
      (insert amend))))

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
           (string-match "/$" ivy--current))
      (insert (substring ivy--current 0 -1))
    (insert ivy--current)))

(defun ivy-toggle-fuzzy ()
  "Toggle the re builder between `ivy--regex-fuzzy' and `ivy--regex-plus'."
  (interactive)
  (setq ivy--old-re nil)
  (if (eq ivy--regex-function 'ivy--regex-fuzzy)
      (setq ivy--regex-function 'ivy--regex-plus)
    (setq ivy--regex-function 'ivy--regex-fuzzy)))

(defun ivy-reverse-i-search ()
  "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibufer."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (history (symbol-value (ivy-state-history ivy-last)))
        (old-last ivy-last))
    (ivy-read "Reverse-i-search: "
              history
              :action (lambda (x)
                        (ivy--reset-state
                         (setq ivy-last old-last))
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties x))
                        (ivy--cd-maybe)))))

(defun ivy-restrict-to-matches ()
  "Restrict candidates to current matches and erase input."
  (interactive)
  (delete-minibuffer-contents)
  (setq ivy--all-candidates
        (ivy--filter ivy-text ivy--all-candidates)))

(provide 'ivy)

;;; ivy.el ends here
