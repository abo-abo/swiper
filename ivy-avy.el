;;; ivy-avy.el --- Avy integration for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.13.0
;; Package-Requires: ((emacs "24.5") (ivy "0.13.0") (avy "0.5.0"))
;; Keywords: convenience

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
;;
;; This package adds a "C-'" binding to Ivy minibuffer that uses Avy.

;;; Code:

(require 'ivy)
(require 'avy)

(defcustom ivy-avy-style 'pre
  "The `avy-style' setting for `ivy-avy'."
  :type '(choice
          (const :tag "Pre" pre)
          (const :tag "At" at)
          (const :tag "At Full" at-full)
          (const :tag "Post" post)
          (const :tag "De Bruijn" de-bruijn)
          (const :tag "Words" words))
  :group 'ivy)

(defun ivy-avy--candidates ()
  "List of candidates for `ivy-avy'."
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

(defun ivy-avy--action (pt)
  "Select the candidate represented by PT."
  (when (number-or-marker-p pt)
    (let ((bnd (ivy--minibuffer-index-bounds
                ivy--index ivy--length ivy-height)))
      (ivy--done
       (substring-no-properties
        (nth (+ (car bnd) (- (line-number-at-pos pt) 2)) ivy--old-cands))))))

(defun ivy-avy--handler-function (char)
  "Handle CHAR that's not on `avy-keys'."
  (let (cmd)
    (cond ((memq char '(?\C-\[ ?\C-g))
           ;; exit silently
           (throw 'done 'abort))
          ((memq (setq cmd (lookup-key ivy-minibuffer-map (vector char)))
                 '(ivy-scroll-up-command
                   ivy-scroll-down-command))
           (funcall cmd)
           (ivy--exhibit)
           (throw 'done 'exit))
          ;; ignore wrong key
          (t
           (throw 'done 'restart)))))

(defun ivy-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (if (= (minibuffer-depth) 0)
      (user-error
       "This command is intended to be called from within `ivy-read'")
    (let* ((avy-all-windows nil)
           (avy-keys (or (cdr (assq 'ivy-avy avy-keys-alist))
                         avy-keys))
           (avy-style (or (cdr (assq 'ivy-avy avy-styles-alist))
                          avy-style))
           (avy-action #'identity)
           (avy-handler-function #'ivy-avy--handler-function)
           res)
      (while (eq (setq res (avy-process (ivy-avy--candidates))) t))
      (when res
        (ivy-avy--action res)))))

(put 'ivy-avy 'no-counsel-M-x t)
(define-key ivy-minibuffer-map (kbd "C-'") 'ivy-avy)
(add-to-list 'avy-styles-alist `(ivy-avy . ,ivy-avy-style))

(provide 'ivy-avy)

;;; ivy-avy.el ends here
