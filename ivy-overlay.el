;;; ivy-overlay.el --- Overlay display functions for Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows to setup Ivy's completion at point to actually
;; show the candidates and the input at point, instead of in the
;; minibuffer.

;;; Code:
(defvar ivy-overlay-at nil
  "Overlay variable for `ivy-display-function-overlay'.")

(defun ivy-left-pad (str width)
  "Pad STR from left with WIDTH spaces."
  (let ((padding (make-string width ?\ )))
    (mapconcat (lambda (x) (concat padding x))
               (split-string str "\n")
               "\n")))

(defun ivy-overlay-cleanup ()
  "Clean up after `ivy-display-function-overlay'."
  (when (overlayp ivy-overlay-at)
    (delete-overlay ivy-overlay-at)
    (setq ivy-overlay-at nil))
  (setq cursor-type t))

(defun ivy-overlay-show-after (str)
  "Display STR in an overlay at point.

First, fill each line of STR with spaces to the current column.
Then attach the overlay the character before point."
  (if ivy-overlay-at
      (progn
        (move-overlay ivy-overlay-at (1- (point)) (line-end-position))
        (overlay-put ivy-overlay-at 'invisible nil))
    (setq ivy-overlay-at (make-overlay (1- (point)) (line-end-position)))
    (overlay-put ivy-overlay-at 'priority 9999))
  (overlay-put ivy-overlay-at 'display str)
  (overlay-put ivy-overlay-at 'after-string ""))

(defun ivy-display-function-overlay (str)
  "Called from the minibuffer, display STR in an overlay in Ivy window.
Hide the minibuffer contents and cursor."
  (add-face-text-property (minibuffer-prompt-end) (point-max)
                          '(:foreground "white"))
  (setq cursor-type nil)
  (with-ivy-window
    (setq cursor-type nil)
    (ivy-overlay-show-after
     (concat
      (buffer-substring (1- (point)) (point))
      ivy-text
      (buffer-substring (point) (line-end-position))
      (ivy-left-pad str
                    (+ (if (eq major-mode 'org-mode)
                           (* org-indent-indentation-per-level (org-current-level))
                         0)
                       (- ivy-completion-beg ivy-completion-end)
                       (current-column)))))))

(provide 'ivy-overlay)
;;; ivy-overlay.el ends here
