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

;;* User Visible
;;** Keymap
(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map (kbd "C-n") 'ivy-next-line)
    (define-key map (kbd "C-p") 'ivy-previous-line)
    (define-key map (kbd "C-s") 'ivy-next-line)
    (define-key map (kbd "C-r") 'ivy-previous-line)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map (kbd "DEL") 'ivy-backward-delete-char)
    (define-key map (kbd "M-<") 'ivy-beginning-of-buffer)
    (define-key map (kbd "M->") 'ivy-end-of-buffer)
    map)
  "Keymap used in the minibuffer.")

;;** Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (delete-minibuffer-contents)
  (insert ivy--current)
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defun ivy-next-line ()
  "Select the next completion candidate."
  (interactive)
  (unless (>= ivy--index (1- ivy--length))
    (cl-incf ivy--index)))

(defun ivy-beginning-of-buffer ()
  "Select the first completion candidate."
  (interactive)
  (setq ivy--index 0))

(defun ivy-end-of-buffer ()
  "Select the last completion candidate."
  (interactive)
  (setq ivy--index (1- ivy--length)))

(defun ivy-previous-line ()
  "Select the previous completion candidate."
  (interactive)
  (unless (zerop ivy--index)
    (decf ivy--index)))

(defun ivy-backward-delete-char ()
  "Forward to `backward-delete-char'.
On error (read-only), quit without selecting."
  (interactive)
  (condition-case nil
      (backward-delete-char 1)
    (error
     (require 'delsel)
     (minibuffer-keyboard-quit))))

;;** Entry Point
(defun ivy-read (prompt collection &optional initial-input update-fn)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
COLLECTION is a list of strings.
If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
UPDATE-FN is called each time the current candidate(s) is changed."
  (setq ivy--index 0)
  (setq ivy--old-re nil)
  (setq ivy-text "")
  (setq ivy--all-candidates collection)
  (setq ivy--update-fn update-fn)
  (setq ivy-exit nil)
  (unwind-protect
       (minibuffer-with-setup-hook
           #'ivy--minibuffer-setup
         (read-from-minibuffer prompt initial-input))
    (remove-hook 'post-command-hook #'ivy--exhibit)))

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

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (setq ivy-text (ivy--input))
  (ivy--cleanup)
  (let ((text (while-no-input
                (ivy-completions
                 ivy-text
                 ivy--all-candidates)))
        (buffer-undo-list t)
        deactivate-mark)
    (when ivy--update-fn
      (funcall ivy--update-fn))
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
A better function `add-face-text-property' is used if it's available.
Otherwise, `propertize'."
  (if (fboundp 'add-face-text-property)
      (progn
        (add-face-text-property 0 (length str) face t str)
        str)
    (propertize str 'face face)))

(defun ivy-completions (name candidates)
  "Return as text the current completions.
NAME is a string of words separated by spaces that is used to
build a regex.
CANDIDATES is a list of strings."
  (let* ((re (ivy--regex name))
         (cands (if (equal re ivy--old-re)
                    ivy--old-cands
                  (setq ivy--old-re re)
                  (setq ivy--old-cands
                        (ignore-errors
                          (cl-remove-if-not
                           (lambda (x) (string-match re x))
                           candidates))))))
    (setq ivy--length (length cands))
    ;; should do a re-anchor here
    (when (>= ivy--index ivy--length)
      (setq ivy--index (1- ivy--length)))
    (if (null cands)
        ""
      (let ((index ivy--index))
        (if (< index (/ ivy-height 2))
            (setq cands
                  (cl-subseq cands 0 (min (1- ivy-height) ivy--length)))
          (setq cands
                (cl-subseq cands
                        (- index (/ ivy-height 2))
                        (min (+ index (/ ivy-height 2))
                             ivy--length)))
          (setq index (min (/ ivy-height 2)
                           (1- (length cands)))))
        (setq ivy--current (copy-sequence
                            (nth index cands)))
        (setf (nth index cands)
              (ivy--add-face ivy--current 'ivy-current-match))
        (concat "\n" (mapconcat #'identity cands "\n"))))))

(provide 'ivy)

;;; ivy.el ends here
