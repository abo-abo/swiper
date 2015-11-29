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
(require 'ffap)

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
  "Face used by Ivy for a confirmation prompt.")

(defface ivy-match-required-face
  '((t :foreground "red" :inherit minibuffer-prompt))
  "Face used by Ivy for a match required prompt.")

(defface ivy-subdir
  '((t (:inherit 'dired-directory)))
  "Face used by Ivy for highlighting subdirs in the alternatives.")

(defface ivy-modified-buffer
  '((t :inherit 'default))
  "Face used by Ivy for highlighting modified file visiting buffers.")

(defface ivy-remote
  '((t (:foreground "#110099")))
  "Face used by Ivy for highlighting remotes in the alternatives.")

(defcustom ivy-height 10
  "Number of lines for the minibuffer window."
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

(defcustom ivy-wrap nil
  "When non-nil, wrap around after the first and the last candidate."
  :type 'boolean)

(defcustom ivy-display-style (unless (version< emacs-version "24.5") 'fancy)
  "The style for formatting the minibuffer.

By default, the matched strings are copied as is.

The fancy display style highlights matching parts of the regexp,
a behavior similar to `swiper'.

This setting depends on `add-face-text-property' - a C function
available as of Emacs 24.5. Fancy style will render poorly in
earlier versions of Emacs."
  :type '(choice
          (const :tag "Plain" nil)
          (const :tag "Fancy" fancy)))

(defcustom ivy-on-del-error-function 'minibuffer-keyboard-quit
  "The handler for when `ivy-backward-delete-char' throws.
Usually a quick exit out of the minibuffer."
  :type 'function)

(defcustom ivy-extra-directories '("../" "./")
  "Add this to the front of the list when completing file names.
Only \"./\" and \"../\" apply here. They appear in reverse order."
  :type '(repeat :tag "Dirs"
          (choice
           (const :tag "Parent Directory" "../")
           (const :tag "Current Directory" "./"))))

(defcustom ivy-use-virtual-buffers nil
  "When non-nil, add `recentf-mode' and bookmarks to `ivy-switch-buffer'."
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
    (define-key map (kbd "C-M-o") 'ivy-dispatching-call)
    (define-key map (kbd "C-k") 'ivy-kill-line)
    (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (define-key map (kbd "M-w") 'ivy-kill-ring-save)
    (define-key map (kbd "C-'") 'ivy-avy)
    (define-key map (kbd "C-M-a") 'ivy-read-action)
    (define-key map (kbd "C-c C-o") 'ivy-occur)
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
  caller)

(defvar ivy-last nil
  "The last parameters passed to `ivy-read'.

This should eventually become a stack so that you could use
`ivy-read' recursively.")

(defsubst ivy-set-action (action)
  (setf (ivy-state-action ivy-last) action))

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
When non-nil, it should contain at least one %d.")

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

(defvar ivy-case-fold-search 'auto
  "Store the current overriding `case-fold-search'.")

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

(defun ivy-read-action ()
  "Change the action to one of the available ones."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (unless (null (ivy--actionp actions))
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
             (action-idx (cl-position-if
                          (lambda (x) (equal (car x) key))
                          (cdr actions))))
        (cond ((string= key ""))
              ((null action-idx)
               (error "%s is not bound" key))
              (t
               (message "")
               (setcar actions (1+ action-idx))
               (ivy-set-action actions)))))))

(defun ivy-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (ivy-read-action)
  (ivy-done))

(defun ivy-dispatching-call ()
  "Select one of the available actions and call `ivy-call'."
  (interactive)
  (let ((actions (copy-sequence (ivy-state-action ivy-last))))
    (unwind-protect
         (when (ivy-read-action)
           (ivy-call))
      (ivy-set-action actions))))

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
  (cond (arg
         (ivy-immediate-done))
        (ivy--directory
         (ivy--directory-done))
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
        (t
         (ivy-done))))

(defun ivy--directory-done ()
  "Handle exit from the minibuffer when completing file names."
  (let (dir)
    (cond
      ((equal ivy-text "/sudo::")
       (setq dir (concat ivy-text ivy--directory))
       (ivy--cd dir)
       (ivy--exhibit))
      ((or
        (and
         (not (equal ivy-text ""))
         (ignore-errors
           (file-directory-p
            (setq dir
                  (file-name-as-directory
                   (expand-file-name
                    ivy-text ivy--directory))))))
        (and
         (not (string= ivy--current "./"))
         (cl-plusp ivy--length)
         (ignore-errors
           (file-directory-p
            (setq dir (file-name-as-directory
                       (expand-file-name
                        ivy--current ivy--directory)))))))
       (ivy--cd dir)
       (ivy--exhibit))
      ((or (and (equal ivy--directory "/")
                (string-match "\\`[^/]+:.*:.*\\'" ivy-text))
           (string-match "\\`/[^/]+:.*:.*\\'" ivy-text))
       (ivy-done))
      ((or (and (equal ivy--directory "/")
                (cond ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        ivy-text))
                      ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        ivy--current)
                       (setq ivy-text ivy--current))))
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
           (or (and (equal ivy--directory "/")
                    (string-match "\\`[^/]+:.*\\'" ivy-text))
               (string-match "\\`/" ivy-text)))
      (let ((default-directory ivy--directory))
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (file-directory-p
               (expand-file-name ivy-text ivy--directory))
          (ivy--cd (file-name-as-directory
                    (expand-file-name ivy-text ivy--directory)))))
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
                              (mapcar (lambda (str)
                                        (let ((i (string-match postfix str)))
                                          (when i
                                            (substring str i))))
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
  (insert (setq ivy--current
                (if ivy--directory
                    (expand-file-name ivy-text ivy--directory)
                  ivy-text)))
  (setq ivy-exit 'done)
  (exit-minibuffer))

;;;###autoload
(defun ivy-resume ()
  "Resume the last completion session."
  (interactive)
  (when (eq (ivy-state-caller ivy-last) 'swiper)
    (switch-to-buffer (ivy-state-buffer ivy-last)))
  (with-current-buffer (ivy-state-buffer ivy-last)
    (ivy-read
     (ivy-state-prompt ivy-last)
     (ivy-state-collection ivy-last)
     :predicate (ivy-state-predicate ivy-last)
     :require-match (ivy-state-require-match ivy-last)
     :initial-input ivy-text
     :history (ivy-state-history ivy-last)
     :preselect (unless (eq (ivy-state-collection ivy-last)
                            'read-file-name-internal)
                  ivy--current)
     :keymap (ivy-state-keymap ivy-last)
     :update-fn (ivy-state-update-fn ivy-last)
     :sort (ivy-state-sort ivy-last)
     :action (ivy-state-action ivy-last)
     :unwind (ivy-state-unwind ivy-last)
     :re-builder (ivy-state-re-builder ivy-last)
     :matcher (ivy-state-matcher ivy-last)
     :dynamic-collection (ivy-state-dynamic-collection ivy-last)
     :caller (ivy-state-caller ivy-last))))

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

(defun ivy--get-window (state)
  "Get the window from STATE."
  (let ((window (ivy-state-window state)))
    (if (window-live-p window)
        window
      (if (= (length (window-list)) 1)
          (selected-window)
        (next-window)))))

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
        (prog1 (funcall action x)
          (unless (or (eq ivy-exit 'done)
                      (equal (selected-window)
                             (active-minibuffer-window))
                      (null (active-minibuffer-window)))
            (select-window (active-minibuffer-window))))))))

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
    (let ((input (ivy--input))
          url)
      (if (setq url (ffap-url-p input))
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

(defvar avy-all-windows)
(defvar avy-action)
(defvar avy-keys)
(defvar avy-keys-alist)
(defvar avy-style)
(defvar avy-styles-alist)
(declare-function avy--process "ext:avy")
(declare-function avy--style-fn "ext:avy")

(eval-after-load 'avy
  '(add-to-list 'avy-styles-alist '(ivy-avy . pre)))

(defun ivy-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (let* ((avy-all-windows nil)
         (avy-keys (or (cdr (assq 'ivy-avy avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq 'ivy-avy
                                   avy-styles-alist))
                        avy-style))
         (candidate
          (let ((candidates))
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
            (setq avy-action #'identity)
            (avy--process
             (nreverse candidates)
             (avy--style-fn avy-style)))))
    (ivy-set-index (- (line-number-at-pos candidate) 2))
    (ivy--exhibit)
    (ivy-done)))

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

(defcustom ivy-sort-functions-alist
  '((read-file-name-internal . ivy-sort-file-function-default)
    (internal-complete-buffer . nil)
    (counsel-git-grep-function . nil)
    (Man-goto-section . nil)
    (org-refile . nil)
    (t . string-lessp))
  "An alist of sorting functions for each collection function.
Interactive functions that call completion fit in here as well.

Nil means no sorting, which is useful to turn off the sorting for
functions that have candidates in the natural buffer order, like
`org-refile' or `Man-goto-section'.

The entry associated with t is used for all fall-through cases.

See also `ivy-sort-max-size'."
  :type
  '(alist
    :key-type (choice
               (const :tag "All other functions" t)
               (symbol :tag "Function"))
    :value-type (choice
                 (const :tag "plain sort" string-lessp)
                 (const :tag "file sort" ivy-sort-file-function-default)
                 (const :tag "no sort" nil)))
  :group 'ivy)

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
Each function should take a string and return a valid regex or a
regex sequence (see below).

The entry associated with t is used for all fall-through cases.
Possible choices: `ivy--regex', `regexp-quote', `ivy--regex-plus'.

If a function returns a list, it should format like this:
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
                           action unwind re-builder matcher dynamic-collection caller)
  "Read a string in the minibuffer, with completion.

PROMPT is a format string, normally ending in a colon and a
space; %d anywhere in the string is replaced by the current
number of matching candidates. For the literal % character,
escape it with %%. See also `ivy-count-format'.

COLLECTION is either a list of strings, a function, an alist, or
a hash table.

If INITIAL-INPUT is not nil, then insert that input in the
minibuffer initially.

KEYMAP is composed with `ivy-minibuffer-map'.

If PRESELECT is not nil, then select the corresponding candidate
out of the ones that match the INITIAL-INPUT.

UPDATE-FN is called each time the current candidate(s) is changed.

When SORT is t, use `ivy-sort-functions-alist' for sorting.

ACTION is a lambda function to call after selecting a result. It
takes a single string argument.

UNWIND is a lambda function to call before exiting.

RE-BUILDER is a lambda function to call to transform text into a
regex pattern.

MATCHER is to override matching.

DYNAMIC-COLLECTION is a boolean to specify if the list of
candidates is updated after each input by calling COLLECTION.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session."
  (let ((extra-actions (plist-get ivy--actions-list this-command)))
    (when extra-actions
      (setq action
            (if (functionp action)
                `(1
                  ("o" ,action "default")
                  ,@extra-actions)
              (delete-dups (append action extra-actions))))))
  (let ((recursive-ivy-last (and (active-minibuffer-window) ivy-last)))
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
           :buffer (current-buffer)
           :unwind unwind
           :re-builder re-builder
           :matcher matcher
           :dynamic-collection dynamic-collection
           :caller caller))
    (ivy--reset-state ivy-last)
    (prog1
        (unwind-protect
            (minibuffer-with-setup-hook
                #'ivy--minibuffer-setup
              (let* ((hist (or history 'ivy-history))
                     (minibuffer-completion-table collection)
                     (minibuffer-completion-predicate predicate)
                     (resize-mini-windows (cond
                                           ((display-graphic-p) nil)
                                           ((null resize-mini-windows) 'grow-only)
                                           (t resize-mini-windows)))
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
      (ivy-call)
      (when recursive-ivy-last
        (ivy--reset-state (setq ivy-last recursive-ivy-last))))))

(defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
  (let ((prompt (or (ivy-state-prompt state) ""))
        (collection (ivy-state-collection state))
        (predicate (ivy-state-predicate state))
        (history (ivy-state-history state))
        (preselect (ivy-state-preselect state))
        (sort (ivy-state-sort state))
        (re-builder (ivy-state-re-builder state))
        (dynamic-collection (ivy-state-dynamic-collection state))
        (initial-input (ivy-state-initial-input state))
        (require-match (ivy-state-require-match state)))
    (unless initial-input
      (setq initial-input (cdr (assoc this-command
                                      ivy-initial-inputs-alist))))
    (setq ivy--directory nil)
    (setq ivy-case-fold-search 'auto)
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
                 (setf
                  (ivy-state-preselect state)
                  (setq preselect (file-name-nondirectory preselect)))))
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
                 (and (consp collection) (listp (car collection)))
                 (hash-table-p collection))
             (setq coll (all-completions "" collection predicate)))
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
                    dynamic-collection
                    (let ((re (regexp-quote preselect)))
                      (cl-find-if (lambda (x) (string-match re x))
                                  coll)))
          (setq coll (cons preselect coll))))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (when (integerp preselect)
        (setq ivy--old-re "")
        (setq ivy--index preselect))
      (when initial-input
        ;; Needed for anchor to work
        (setq ivy--old-cands coll)
        (setq ivy--old-cands (ivy--filter initial-input coll)))
      (setq ivy--all-candidates coll)
      (unless (integerp preselect)
        (setq ivy--index (or
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
    (setq ivy--default (or
                        (thing-at-point 'url)
                        (thing-at-point 'symbol)
                        ""))
    (setq ivy--prompt
          (cond ((string-match "%.*d" prompt)
                 prompt)
                ((null ivy-count-format)
                 (error
                  "`ivy-count-format' can't be nil.  Set it to an empty string instead"))
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

;;;###autoload
(defun ivy-completing-read (prompt collection
                            &optional predicate require-match initial-input
                              history def _inherit-input-method)
  "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION can be a list of strings, an alist, an obarray or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is specified with a boolean value.  See `completing-read'.
INITIAL-INPUT is a string that can be inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
_INHERIT-INPUT-METHOD is currently ignored."

  ;; See the doc of `completing-read'.
  (when (consp history)
    (when (numberp (cdr history))
      (setq initial-input (nth (1- (cdr history))
                               (symbol-value (car history)))))
    (setq history (car history)))
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
      (setq completing-read-function 'ivy-completing-read)
    (setq completing-read-function 'completing-read-default)))

(defun ivy--preselect-index (preselect candidates)
  "Return the index of PRESELECT in CANDIDATES."
  (cond ((integerp preselect)
         preselect)
        ((cl-position preselect candidates :test #'equal))
        ((stringp preselect)
         (let ((re (regexp-quote preselect)))
           (cl-position-if
            (lambda (x)
              (string-match re x))
            candidates)))))

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
  "Re-build regex pattern from STR in case it has a space.
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
  "Re-build regex from STR by splitting at spaces.
Ignore the order of each group.

ATTENTION: This is just a proof of concept and may not work as
expected. Besides ignoring the order of the tokens where 'foo'
and 'bar', 'bar' and 'foo' are matched, it also matches multiple
occurrences of 'foo' and 'bar'. To ignore the sort order and avoid
multiple matches, use `ivy-restrict-to-matches' instead.
"
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
Spaces are wild card characters, everything before \"!\" should
match. Everything after \"!\" should not match."
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
      (prog1
          (concat (match-string 1 str)
                  (mapconcat
                   (lambda (x)
                     (format "\\(%c\\)" x))
                   (string-to-list (match-string 2 str)) ".*")
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))

;;** Rest
(defun ivy--minibuffer-setup ()
  "Setup ivy completion in the minibuffer."
  (set (make-local-variable 'completion-show-inline-help) nil)
  (set (make-local-variable 'minibuffer-default-add-function)
       (lambda ()
         (list ivy--default)))
  (when (display-graphic-p)
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
               tail)))
            (d-str (if ivy--directory
                       (abbreviate-file-name ivy--directory)
                     "")))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point-min) (minibuffer-prompt-end))
          (if (> (+ (mod (+ (length n-str) (length d-str)) (window-width))
                    (length ivy-text))
                 (window-width))
              (setq n-str (concat n-str "\n" d-str))
            (setq n-str (concat n-str d-str)))
          (let ((regex (format "\\([^\n]\\{%d\\}\\)[^\n]" (window-width))))
            (while (string-match regex n-str)
              (setq n-str (replace-match (concat (match-string 1 n-str) "\n") nil t n-str 1))))
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
  "Set STR text properties that match MATCH to PROPS."
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
                   (when (string-match "[[:alpha:]]:/$" ivy-text)
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
       (with-current-buffer (ivy-state-buffer ivy-last)
         (ivy--format
          (ivy--filter ivy-text ivy--all-candidates))))
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
  "Resize the minibuffer window size to fit the text in the minibuffer."
  (with-selected-window (minibuffer-window)
    (if (fboundp 'window-text-pixel-size)
        (let ((text-height (cdr (window-text-pixel-size)))
              (body-height (window-body-height nil t)))
          (when (> text-height body-height)
            (window-resize nil (- text-height body-height) nil t t)))
      (let ((text-height (count-screen-lines))
            (body-height (window-body-height)))
        (when (> text-height body-height)
          (window-resize nil (- text-height body-height) nil t))))))

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

(declare-function flx-make-string-cache "ext:flx")
(declare-function flx-score "ext:flx")

(defvar ivy--flx-cache nil)

(eval-after-load 'flx
  '(setq ivy--flx-cache (flx-make-string-cache)))

(defun ivy-toggle-case-fold ()
  "Toggle the case folding between nil and auto.
In any completion session, the case folding starts in auto:

- when the input is all lower case, `case-fold-search' is t
- otherwise nil.

You can toggle this to make `case-fold-search' nil regardless of input."
  (interactive)
  (setq ivy-case-fold-search
        (if ivy-case-fold-search
            nil
          'auto))
  ;; reset cache so that the candidate list updates
  (setq ivy--old-re nil))

(defun ivy--filter (name candidates)
  "Return all items that match NAME in CANDIDATES.
CANDIDATES are assumed to be static."
  (let ((re (funcall ivy--regex-function name)))
    (if (and (equal re ivy--old-re)
             ivy--old-cands)
        ;; quick caching for "C-n", "C-p" etc.
        ivy--old-cands
      (let* ((re-str (if (listp re) (caar re) re))
             (matcher (ivy-state-matcher ivy-last))
             (case-fold-search
              (and ivy-case-fold-search
                   (string= name (downcase name))))
             (cands (cond
                     (matcher
                      (funcall matcher re candidates))
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
                                    (let ((re-str (car re)))
                                      (lambda (x) (string-match re-str x)))
                                    res))))
                         res)))))
        (ivy--recompute-index name re-str cands)
        (setq ivy--old-re (if cands re-str ""))
        (setq ivy--old-cands (ivy--sort name cands))))))

(defcustom ivy-sort-matches-functions-alist '((t . nil))
  "An alist of functions used to sort the matching candidates.

This is different from `ivy-sort-functions-alist', which is used
to sort the whole collection only once.  The functions taken from
here are instead used on each input change, but they are used
only on already matching candidates, not on all of them.

The alist KEY is a collection function or t to match previously
not matched collection functions.

The alist VAL is a sorting function with the signature of
`ivy--prefix-sort'.")

(defun ivy--sort-files-by-date (_name candidates)
  "Re-soft CANDIDATES according to file modification date."
  (let ((default-directory ivy--directory))
    (cl-sort (copy-sequence candidates)
             (lambda (f1 f2)
               (time-less-p
                (nth 5 (file-attributes f2))
                (nth 5 (file-attributes f1)))))))

(defun ivy--sort (name candidates)
  "Re-sort CANDIDATES by NAME.
All CANDIDATES are assumed to match NAME."
  (let ((key (or (ivy-state-caller ivy-last)
                 (when (functionp (ivy-state-collection ivy-last))
                   (ivy-state-collection ivy-last))))
        fun)
    (cond ((and (require 'flx nil 'noerror)
                (eq ivy--regex-function 'ivy--regex-fuzzy))
           (ivy--flx-sort name candidates))
          ((setq fun (cdr (or (assoc key ivy-sort-matches-functions-alist)
                              (assoc t ivy-sort-matches-functions-alist))))
           (funcall fun name candidates))
          (t
           candidates))))

(defun ivy--prefix-sort (name candidates)
  "Re-sort CANDIDATES.
Prefix matches to NAME are put ahead of the list."
  (if (or (string-match "^\\^" name) (string= name ""))
      candidates
    (let ((re-prefix (concat "^" (funcall ivy--regex-function name)))
          res-prefix
          res-noprefix)
      (dolist (s candidates)
        (if (string-match re-prefix s)
            (push s res-prefix)
          (push s res-noprefix)))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)))))

(defun ivy--recompute-index (name re-str cands)
  (let* ((caller (ivy-state-caller ivy-last))
         (func (or (and caller (cdr (assoc caller ivy-index-functions-alist)))
                   (cdr (assoc t ivy-index-functions-alist))
                   #'ivy-recompute-index-zero)))
    (unless (eq this-command 'ivy-resume)
      (setq ivy--index
            (or
             (cl-position (if (and (> (length re-str) 0)
                                   (eq ?^ (aref re-str 0)))
                              (substring re-str 1)
                            re-str) cands
                            :test #'equal)
             (and ivy--directory
                  (cl-position
                   (concat re-str "/") cands
                   :test #'equal))
             (and (not (string= name ""))
                  (not (and (require 'flx nil 'noerror)
                            (eq ivy--regex-function 'ivy--regex-fuzzy)
                            (< (length cands) 200)))

                  (cl-position (nth ivy--index ivy--old-cands)
                               cands))
             (funcall func re-str cands))))
    (when (and (or (string= name "")
                   (string= name "^"))
               (not (equal ivy--old-re "")))
      (setq ivy--index
            (or (ivy--preselect-index
                 (ivy-state-preselect ivy-last)
                 cands)
                ivy--index)))))

(defun ivy-recompute-index-swiper (_re-str cands)
  (let ((tail (nthcdr ivy--index ivy--old-cands))
        idx)
    (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
        (progn
          (while (and tail (null idx))
            ;; Compare with eq to handle equal duplicates in cands
            (setq idx (cl-position (pop tail) cands)))
          (or idx 0))
      (if ivy--old-cands
          ivy--index
        ;; already in ivy-state-buffer
        (let ((n (line-number-at-pos))
              (res 0)
              (i 0))
          (dolist (c cands)
            (when (eq n (read (get-text-property 0 'display c)))
              (setq res i))
            (cl-incf i))
          res)))))

(defun ivy-recompute-index-swiper-async (_re-str cands)
  (let ((tail (nthcdr ivy--index ivy--old-cands))
        idx)
    (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
        (progn
          (while (and tail (null idx))
            ;; Compare with `equal', since the collection is re-created
            ;; each time with `split-string'
            (setq idx (cl-position (pop tail) cands :test #'equal)))
          (or idx 0))
      ivy--index)))

(defun ivy-recompute-index-zero (_re-str _cands)
  0)

(defun ivy--flx-sort (name cands)
  "Sort according to closeness to string NAME the string list CANDS."
  (condition-case nil
      (if (and cands
               (< (length cands) 200))
          (let* ((flx-name (if (string-match "^\\^" name)
                               (substring name 1)
                             name))
                 (cands-with-score
                  (delq nil
                        (mapcar
                         (lambda (x)
                           (let ((score (car (flx-score x flx-name ivy--flx-cache))))
                             (and score
                                  (cons score x))))
                         cands))))
            (if cands-with-score
                (mapcar #'cdr
                        (sort cands-with-score
                              (lambda (x y)
                                (> (car x) (car y)))))
              cands))
        cands)
    (error
     cands)))

(defcustom ivy-format-function 'ivy-format-function-default
  "Function to transform the list of candidates into a string.
This string is inserted into the minibuffer."
  :type '(choice
          (const :tag "Default" ivy-format-function-default)
          (const :tag "Arrow prefix" ivy-format-function-arrow)
          (const :tag "Full line" ivy-format-function-line)))

(defun ivy--truncate-string (str width)
  "Truncate STR to WIDTH."
  (if (> (string-width str) width)
      (concat (substring str 0 (min (- width 3)
                                    (- (length str) 3))) "...")
    str))

(defun ivy--format-function-generic (selected-fn other-fn cand-pairs separator)
  "Transform CAND-PAIRS into a string for minibuffer.
SELECTED-FN and OTHER-FN each take two string arguments.
SEPARATOR is used to join the candidates."
  (let ((i -1))
    (mapconcat
     (lambda (pair)
       (let ((str (car pair))
             (extra (cdr pair))
             (curr (eq (cl-incf i) ivy--index)))
         (if curr
             (funcall selected-fn str extra)
           (funcall other-fn str extra))))
     cand-pairs
     separator)))

(defun ivy-format-function-default (cand-pairs)
  "Transform CAND-PAIRS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str extra)
     (concat (ivy--add-face str 'ivy-current-match) extra))
   #'concat
   cand-pairs
   "\n"))

(defun ivy-format-function-arrow (cand-pairs)
  "Transform CAND-PAIRS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str extra)
     (concat "> " (ivy--add-face str 'ivy-current-match) extra))
   (lambda (str extra)
     (concat "  " str extra))
   cand-pairs
   "\n"))

(defun ivy-format-function-line (cand-pairs)
  "Transform CAND-PAIRS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str extra)
     (ivy--add-face (concat str extra "\n") 'ivy-current-match))
   (lambda (str extra)
     (concat str extra "\n"))
   cand-pairs
   ""))

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
  "Face for `ivy' minibuffer matches modulo 1.")

(defface ivy-minibuffer-match-face-3
  '((((class color) (background light))
     :background "#bbbbff" :weight bold)
    (((class color) (background dark))
     :background "#7777ff" :weight bold))
  "Face for `ivy' minibuffer matches modulo 2.")

(defface ivy-minibuffer-match-face-4
  '((((class color) (background light))
     :background "#ffbbff" :weight bold)
    (((class color) (background dark))
     :background "#8a498a" :weight bold))
  "Face for `ivy' minibuffer matches modulo 3.")

(defcustom ivy-minibuffer-faces
  '(ivy-minibuffer-match-face-1
    ivy-minibuffer-match-face-2
    ivy-minibuffer-match-face-3
    ivy-minibuffer-match-face-4)
  "List of `ivy' faces for minibuffer group matches.")

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
                          (cadr ivy-minibuffer-faces))
                         ((zerop i)
                          (car ivy-minibuffer-faces))
                         (t
                          (nth (1+ (mod (+ i 2) (1- (length ivy-minibuffer-faces))))
                               ivy-minibuffer-faces)))))
              (if (fboundp 'add-face-text-property)
                  (add-face-text-property
                   (match-beginning i)
                   (match-end i)
                   face
                   nil
                   str)
                (font-lock-append-text-property
                 (match-beginning i)
                 (match-end i)
                 'face
                 face
                 str)))
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
      (cond (ivy--directory
             (setq cands (mapcar (lambda (x)
                                   (if (string-match-p "/\\'" x)
                                       (propertize x 'face 'ivy-subdir)
                                     x))
                                 cands)))
            ((eq (ivy-state-collection ivy-last) 'internal-complete-buffer)
             (setq cands (mapcar (lambda (x)
                                   (let ((b (get-buffer x)))
                                     (if (and b
                                              (buffer-file-name b)
                                              (buffer-modified-p b))
                                         (propertize x 'face 'ivy-modified-buffer)
                                       x)))
                                 cands))))
      (setq ivy--current (copy-sequence (nth index cands)))
      (let* ((ivy--index index)
             (cand-pairs (mapcar
                          (lambda (cand)
                            (cons (ivy--format-minibuffer-line cand) nil)) cands))
             (res (concat "\n" (funcall ivy-format-function cand-pairs))))
        (put-text-property 0 (length res) 'read-only nil res)
        res))))

(defvar ivy--virtual-buffers nil
  "Store the virtual buffers alist.")

(defvar recentf-list)

(defface ivy-virtual '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for matching virtual buffer names.")

(defcustom ivy-virtual-abbreviate 'name
  "The mode of abbreviation for virtual buffer names."
  :type '(choice
          (const :tag "Only name" name)
          (const :tag "Full path" full)
          ;; eventually, uniquify
          ))

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
      (setq name
            (if (eq ivy-virtual-abbreviate 'name)
                (file-name-nondirectory head)
              (expand-file-name head)))
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

;;;###autoload
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

;;;###autoload
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
      (insert (replace-regexp-in-string "  +" " " amend)))))

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
The selected history element will be inserted into the minibuffer."
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

;;* Occur
(defvar-local ivy-occur-last nil
  "Buffer-local value of `ivy-last'.
Can't re-use `ivy-last' because using e.g. `swiper' in the same
buffer would modify `ivy-last'.")

(defvar ivy-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ivy-occur-click)
    (define-key map (kbd "RET") 'ivy-occur-press)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "g") 'ivy-occur-press)
    (define-key map (kbd "a") 'ivy-occur-read-action)
    (define-key map (kbd "o") 'ivy-occur-dispatch)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for Ivy Occur mode.")

(define-derived-mode ivy-occur-mode fundamental-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-mode-map}")

(defvar ivy-occur-grep-mode-map
  (let ((map (copy-keymap ivy-occur-mode-map)))
    (define-key map (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode)
    map)
  "Keymap for Ivy Occur Grep mode.")

(define-derived-mode ivy-occur-grep-mode grep-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-grep-mode-map}")

(defvar counsel-git-grep-cmd)

(defun ivy-occur ()
  "Stop completion and put the current matches into a new buffer.

The new buffer remembers current action(s).

While in the *ivy-occur* buffer, selecting a candidate with RET or
a mouse click will call the appropriate action for that candidate.

There is no limit on the number of *ivy-occur* buffers."
  (interactive)
  (let ((buffer
         (generate-new-buffer
          (format "*ivy-occur%s \"%s\"*"
                  (let (caller)
                    (if (setq caller (ivy-state-caller ivy-last))
                        (concat " " (prin1-to-string caller))
                      ""))
                  ivy-text)))
        (do-grep (eq (ivy-state-caller ivy-last) 'counsel-git-grep)))
    (with-current-buffer buffer
      (if do-grep
          (progn
            (setq ivy--old-cands
                  (split-string
                   (shell-command-to-string
                    (format counsel-git-grep-cmd ivy--old-re))
                   "\n"
                   t))
            (ivy-occur-grep-mode))
        (ivy-occur-mode))
      (setf (ivy-state-text ivy-last) ivy-text)
      (setq ivy-occur-last ivy-last)
      (setq-local ivy--directory ivy--directory)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when do-grep
          ;; Need precise number of header lines for `wgrep' to work.
          (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                          default-directory)))
        (insert (format "%d candidates:\n" (length ivy--old-cands)))
        (dolist (cand ivy--old-cands)
          (let ((str (if do-grep
                         (concat "./" cand)
                       (concat "    " cand))))
            (add-text-properties
             0 (length str)
             `(mouse-face
               highlight
               help-echo "mouse-1: call ivy-action")
             str)
            (insert str "\n")))))
    (ivy-exit-with-action
     `(lambda (_) (pop-to-buffer ,buffer)))))

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

(defun ivy-occur-press ()
  "Execute action for the current candidate."
  (interactive)
  (require 'pulse)
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
        (funcall action
                 (if (and (consp coll)
                          (consp (car coll)))
                     (cdr (assoc str coll))
                   str))
        (if (memq (ivy-state-caller ivy-last)
                  '(swiper counsel-git-grep))
            (with-current-buffer (window-buffer (selected-window))
              (swiper--cleanup)
              (swiper--add-overlays
               (ivy--regex ivy-text)
               (line-beginning-position)
               (line-end-position)
               (selected-window))
              (run-at-time 0.5 nil 'swiper--cleanup))
          (pulse-momentary-highlight-one-line (point)))))))

(provide 'ivy)

;;; ivy.el ends here
