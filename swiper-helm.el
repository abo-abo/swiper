;;; swiper.el --- Help version of Swiper -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (swiper "0.1.0") (helm "1.5.3"))
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
;; This package gives an overview of the current regex search
;; candidates.  The search regex can be split into groups with a
;; space.  Each group is highlighted with a different face.
;;
;; The overview back end is `helm'.
;;
;; It can double as a quick `regex-builder', although only single
;; lines will be matched.

;;; Code:

(require 'swiper)
(require 'helm)

(defvar swiper-helm-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'helm-next-line)
    (define-key map (kbd "C-r") 'helm-previous-line)
    map)
  "Allows you to go to next and previous hit isearch-style.")

(defun swiper-helm (&optional initial-input)
  "`isearch' with an overview using `helm'.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (require 'helm)
  (require 'helm-match-plugin)
  (swiper--init)
  (unwind-protect
       (let ((helm-display-function
              (lambda (buf)
                (when (one-window-p)
                  (split-window-vertically))
                (other-window 1)
                (switch-to-buffer buf)))
             helm-candidate-number-limit)
         (helm :sources
               `((name . ,(buffer-name))
                 (init . (lambda ()
                           (add-hook 'helm-move-selection-after-hook
                                     #'swiper--update-sel)
                           (add-hook 'helm-update-hook
                                     #'swiper--update-input-helm)
                           (add-hook 'helm-after-update-hook
                                     #'swiper--reanchor)))
                 (match-strict . (lambda (x)
                                   (ignore-errors
                                     (string-match (ivy--regex helm-input) x))))
                 (candidates . ,(swiper--candidates))
                 (filtered-candidate-transformer
                  helm-fuzzy-highlight-matches)
                 (action . swiper--action-helm))
               :keymap (make-composed-keymap
                        swiper-helm-keymap
                        helm-map)
               :input initial-input
               :preselect
               (format "^%d " swiper--anchor)
               :buffer "*swiper*"))
    ;; cleanup
    (remove-hook 'helm-move-selection-after-hook #'swiper--update-sel)
    (remove-hook 'helm-update-hook #'swiper--update-input-helm)
    (remove-hook 'helm-after-update-hook #'swiper--reanchor)
    (swiper--cleanup)))

(defun swiper--update-input-helm ()
  "Update selection."
  (swiper--cleanup)
  (with-selected-window swiper--window
    (swiper--add-overlays
     (ivy--regex helm-input)
     (window-start swiper--window)
     (window-end swiper--window t)))
  (when (/= (length helm-input) swiper--len)
    (setq swiper--len (length helm-input))
    (swiper--reanchor)))

(defun swiper--binary (beg end)
  "Find anchor between BEG and END."
  (if (<= (- end beg) 10)
      (let ((min 1000)
            n
            ln
            d)
        (goto-char (point-min))
        (forward-line (1- beg))
        (while (< beg end)
          (beginning-of-line)
          (setq n (read (current-buffer)))
          (when (< (setq d (abs (- n swiper--anchor))) min)
            (setq min d)
            (setq ln beg))
          (cl-incf beg)
          (forward-line 1))
        (goto-char (point-min))
        (when ln
          (forward-line (1- ln))))
    (let ((mid (+ beg (/ (- end beg) 2))))
      (goto-char (point-min))
      (forward-line mid)
      (beginning-of-line)
      (let ((n (read (current-buffer))))
        (if (> n swiper--anchor)
            (swiper--binary beg mid)
          (swiper--binary mid end))))))

(defun swiper--update-sel ()
  "Update selection."
  (let* ((re (ivy--regex helm-input))
         (str (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position)))
         (num (if (string-match "^[0-9]+" str)
                  (string-to-number (match-string 0 str))
                0))
         pt)
    (when (> (length re) 0)
      (with-selected-window swiper--window
        (goto-char (point-min))
        (forward-line (1- num))
        (when (re-search-forward re (point-max) t)
          (setq pt (match-beginning 0))))
      (when pt
        (with-selected-window
            (helm-persistent-action-display-window)
          (goto-char pt)
          (recenter)
          (swiper--update-input-helm))))
    (with-selected-window swiper--window
      (let ((ov (make-overlay
                 (line-beginning-position)
                 (1+ (line-end-position)))))
        (overlay-put ov 'face 'swiper-line-face)
        (push ov swiper--overlays)))))

(defun swiper--reanchor ()
  "Move to a valid match closest to `swiper--anchor'."
  (with-selected-window (helm-window)
    (goto-char (point-min))
    (if (re-search-forward (format "^%d " swiper--anchor) nil t)
        nil
      (forward-line 1)
      (swiper--binary 2 (1+ (count-lines (point) (point-max)))))
    (when (> (count-lines (point-min) (point-max)) 1)
      (forward-line -1)
      (helm-next-line 1))))

(defun swiper--action-helm (x)
  "Goto line X."
  (swiper--action x helm-input))

(provide 'swiper-helm)

;;; swiper-helm.el ends here
