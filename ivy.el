;;; ivy.el --- Incremental Vertical completYon -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: matching

;; This file is not part of GNU Emacs

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
;;* Customization
(defgroup ivy nil
  "Incremental vertical completion."
  :group 'convenience)

(defface ivy-current-match
  '((t (:inherit highlight)))
  "Face used by Ivy for highlighting first match.")

(defcustom ivy-height 10
  "Number of lines for the minibuffer window."
  :type 'integer)

(defcustom ivy-count-format "%-4d "
  "The style of showing the current candidate count for `ivy-read'.
Set this to nil if you don't want the count."
  :type 'string)

(defcustom ivy-wrap nil
  "Whether to wrap around after the first and last candidate."
  :type 'boolean)

;;* User Visible
;;** Keymap
(require 'delsel)
(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map (kbd "C-n") 'ivy-next-line)
    (define-key map (kbd "C-p") 'ivy-previous-line)
    (define-key map (kbd "C-s") 'ivy-next-line-or-history)
    (define-key map (kbd "C-r") 'ivy-previous-line-or-history)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map (kbd "DEL") 'ivy-backward-delete-char)
    (define-key map (kbd "M-<") 'ivy-beginning-of-buffer)
    (define-key map (kbd "M->") 'ivy-end-of-buffer)
    (define-key map (kbd "M-n") 'ivy-next-history-element)
    (define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    map)
  "Keymap used in the minibuffer.")

(defvar ivy-history nil
  "History list of candidates entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length', which see.")

;;** Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (delete-minibuffer-contents)
  (unless (zerop ivy--length)
    (insert ivy--current)
    (setq ivy-exit 'done))
  (exit-minibuffer))

(defun ivy-beginning-of-buffer ()
  "Select the first completion candidate."
  (interactive)
  (setq ivy--index 0))

(defun ivy-end-of-buffer ()
  "Select the last completion candidate."
  (interactive)
  (setq ivy--index (1- ivy--length)))

(defun ivy-next-line ()
  "Select the next completion candidate."
  (interactive)
  (if (>= ivy--index (1- ivy--length))
      (when ivy-wrap
        (ivy-beginning-of-buffer))
    (cl-incf ivy--index)))

(defun ivy-next-line-or-history ()
  "Select the next completion candidate.
If the input is empty, select the previous history element instead."
  (interactive)
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (if (>= ivy--index (1- ivy--length))
      (when ivy-wrap
        (ivy-beginning-of-buffer))
    (cl-incf ivy--index)))

(defun ivy-previous-line ()
  "Select the previous completion candidate."
  (interactive)
  (if (zerop ivy--index)
      (when ivy-wrap
        (ivy-end-of-buffer))
    (cl-decf ivy--index)))

(defun ivy-previous-line-or-history ()
  "Select the previous completion candidate.
If the input is empty, select the previous history element instead."
  (interactive)
  (when (string= ivy-text "")
    (ivy-previous-history-element 1))
  (if (zerop ivy--index)
      (when ivy-wrap
        (ivy-end-of-buffer))
    (cl-decf ivy--index)))

(defun ivy-previous-history-element (arg)
  "Forward to `previous-history-element' with ARG."
  (interactive "p")
  (previous-history-element arg)
  (move-end-of-line 1))

(defun ivy-next-history-element (arg)
  "Forward to `next-history-element' with ARG."
  (interactive "p")
  (next-history-element arg)
  (move-end-of-line 1))

(defun ivy-backward-delete-char ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (minibuffer-keyboard-quit))))

;;** Entry Point
(defun ivy-read (prompt collection &optional initial-input update-fn preselect)
  "Read a string in the minibuffer, with completion.

PROMPT is a string to prompt with; normally it ends in a colon
and a space.  When PROMPT contains %d, it will be updated with
the current number of matching candidates.

COLLECTION is a list of strings.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.

UPDATE-FN is called each time the current candidate(s) is changed.

If PRESELECT is non-nil select the corresponding candidate out of
the ones that match INITIAL-INPUT."
  (cl-case (length collection)
    (0 nil)
    (1 (car collection))
    (t
     (setq ivy--index (or
                       (and preselect
                            (ivy--preselect-index
                             collection initial-input preselect))
                       0))
     (setq ivy--old-re nil)
     (setq ivy--old-cands nil)
     (setq ivy-text "")
     (setq ivy--all-candidates collection)
     (setq ivy--update-fn update-fn)
     (setq ivy-exit nil)
     (setq ivy--default (or (thing-at-point 'symbol) ""))
     (setq ivy--prompt
           (cond ((string-match "%.*d" prompt)
                  prompt)
                 ((string-match "%.*d" ivy-count-format)
                  (concat ivy-count-format prompt))
                 (t
                  nil)))
     (unwind-protect
          (minibuffer-with-setup-hook
              #'ivy--minibuffer-setup
            (let ((res (read-from-minibuffer
                        prompt
                        initial-input
                        ivy-minibuffer-map
                        nil
                        'ivy-history)))
              (when (eq ivy-exit 'done)
                (pop ivy-history)
                (setq ivy-history
                      (cons ivy-text (delete ivy-text ivy-history)))
                res)))
       (remove-hook 'post-command-hook #'ivy--exhibit)))))

(defun ivy--preselect-index (candidates initial-input preselect)
  "Return the index in CANDIDATES filtered by INITIAL-INPUT for PRESELECT."
  (when initial-input
    (setq candidates
          (cl-remove-if-not
           (lambda (x)
             (string-match initial-input x))
           candidates)))
  (cl-position-if
   (lambda (x)
     (string-match preselect x))
   candidates))

(defvar ivy-text ""
  "Stores the user's string as it is typed in.")

(defvar ivy-exit nil
  "Store 'done if the completion was successfully selected.
Otherwise, store nil.")

;;* Implementation
;;** Regex
(defvar ivy--subexps 0
  "Number of groups in the current `ivy--regex'.")

(defvar ivy--regex-hash
  (make-hash-table :test 'equal)
  "Store pre-computed regex.")

(defun ivy--regex (str)
  "Re-build regex from STR in case it has a space."
  (let ((hashed (gethash str ivy--regex-hash)))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (cdr (puthash str
                    (let ((subs (split-string str " +" t)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (car subs))
                        (cons
                         (setq ivy--subexps (length subs))
                         (mapconcat
                          (lambda (x) (format "\\(%s\\)" x))
                          subs
                          ".*"))))
                    ivy--regex-hash)))))

;;** Rest
(defun ivy--minibuffer-setup ()
  "Setup ivy completion in the minibuffer."
  (set (make-local-variable 'completion-show-inline-help) nil)
  (set (make-local-variable 'minibuffer-default-add-function)
       (lambda ()
         (list ivy--default)))
  (use-local-map (make-composed-keymap ivy-minibuffer-map
                                       (current-local-map)))
  (setq-local max-mini-window-height ivy-height)
  (add-hook 'post-command-hook #'ivy--exhibit nil t)
  ;; show completions with empty input
  (ivy--exhibit))

(defvar ivy--all-candidates nil
  "Store the candidates passed to `ivy-read'.")

(defvar ivy--index 0
  "Store the index of the current candidate.")

(defvar ivy--length 0
  "Store the amount of viable candidates.")

(defvar ivy--current ""
  "Current candidate.")

(defvar ivy--default nil
  "Default initial input.")

(defvar ivy--update-fn nil
  "Current function to call when current candidate(s) update.")

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

(defvar ivy--prompt nil
  "Store the format-style prompt.
When non-nil, it should contain one %d.")

(defun ivy--insert-prompt ()
  "Update the prompt according to `ivy--prompt'."
  (when ivy--prompt
    (let ((inhibit-read-only t)
          (n-str (format ivy--prompt ivy--length)))
      (save-excursion
        (goto-char (point-min))
        (delete-region (point-min) (minibuffer-prompt-end))
        (set-text-properties
         0 (length n-str)
         '(front-sticky t rear-nonsticky t field t read-only t face minibuffer-prompt)
         n-str)
        (insert n-str))
      ;; get out of the prompt area
      (constrain-to-field nil (point-max)))))

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (setq ivy-text (ivy--input))
  (ivy--cleanup)
  (let ((text (ivy-completions
               ivy-text
               ivy--all-candidates))
        (buffer-undo-list t)
        deactivate-mark)
    (when ivy--update-fn
      (funcall ivy--update-fn))
    (ivy--insert-prompt)
    ;; Do nothing if while-no-input was aborted.
    (when (stringp text)
      (save-excursion
        (forward-line 1)
        (insert text)))))

(defvar ivy--old-re nil
  "Store the old regexp.")

(defvar ivy--old-cands nil
  "Store the candidates matched by `ivy--old-re'.")

(defun ivy--add-face (str face)
  "Propertize STR with FACE.
`font-lock-append-text-property' is used, since it's better than
`propertize' or `add-face-text-property' in this case."
  (font-lock-append-text-property 0 (length str) 'face face str)
  str)

(defun ivy-completions (name candidates)
  "Return as text the current completions.
NAME is a string of words separated by spaces that is used to
build a regex.
CANDIDATES is a list of strings."
  (let* ((re (ivy--regex name))
         (cands (if (and (equal re ivy--old-re)
                         ivy--old-cands)
                    ivy--old-cands
                  (setq ivy--old-re re)
                  (ignore-errors
                    (cl-remove-if-not
                     (lambda (x) (string-match re x))
                     candidates))))
         (tail (nthcdr ivy--index ivy--old-cands))
         (ww (window-width))
         idx)
    (setq ivy--length (length cands))
    (when (and tail ivy--old-cands)
      (while (and tail
                  (null (setq idx (cl-position (pop tail) cands
                                               :test #'equal)))))
      (setq ivy--index (or idx 0)))
    (setq ivy--old-cands cands)
    (when (>= ivy--index ivy--length)
      (setq ivy--index (max (1- ivy--length) 0)))
    (if (null cands)
        ""
      (let* ((half-height (/ ivy-height 2))
             (start (max 0 (- ivy--index half-height)))
             (end (min (+ start (1- ivy-height)) ivy--length))
             (cands (cl-subseq cands start end))
             (index (min ivy--index half-height (1- (length cands)))))
        (setq ivy--current (copy-sequence (nth index cands)))
        (setf (nth index cands)
              (ivy--add-face ivy--current 'ivy-current-match))
        (concat "\n" (mapconcat
                      (lambda (s)
                        (if (> (length s) ww)
                            (concat (substring s 0 (- ww 3)) "...")
                          s))
                      cands "\n"))))))

(provide 'ivy)

;;; ivy.el ends here
