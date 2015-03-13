;;; swiper.el --- Isearch with an overview. Oh, man! -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (ivy "0.1.0"))
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
;; The overview back end can be either `helm' or `ivy'.
;;
;; It can double as a quick `regex-builder', although only single
;; lines will be matched.

;;; Code:
(require 'ivy)

(defgroup swiper nil
  "`isearch' with an overview."
  :group 'matching
  :prefix "swiper-")

(defcustom swiper-completion-method 'helm
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)))

(defface swiper-match-face-1
    '((t (:background "#FEEA89")))
  "Face for `swiper' matches.")

(defface swiper-match-face-2
  '((t (:background "#F9A35A")))
  "Face for `swiper' matches.")

(defface swiper-match-face-3
  '((t (:background "#fb7905")))
  "Face for `swiper' matches.")

(defface swiper-match-face-4
  '((t (:background "#F15C79")))
  "Face for `swiper' matches.")

(defface swiper-line-face
  '((t (:background "#f3d3d3")))
  "Face for current `swiper' line.")

(defcustom swiper-faces '(swiper-match-face-1
                          swiper-match-face-2
                          swiper-match-face-3
                          swiper-match-face-4)
  "List of `swiper' faces for group matches.")

(defvar swiper--window nil
  "Store the current window.")

(defalias 'swiper-font-lock-ensure
    (if (fboundp 'font-lock-ensure)
        'font-lock-ensure
      'font-lock-fontify-buffer))

(defun swiper--candidates ()
  "Return a list of this buffer lines."
  (let* ((line-width (1+ (floor (log (count-lines
                                      (point-min) (point-max))
                                     10))))
         (fspec (format "%%-%dd %%s" line-width))
         (line-number 0)
         candidates)
    (save-excursion
      (goto-char (point-min))
      (swiper-font-lock-ensure)
      (while (< (point) (point-max))
        (push (format fspec
                      (cl-incf line-number)
                      (buffer-substring
                       (line-beginning-position)
                       (line-end-position)))
              candidates)
        (zerop (forward-line 1)))
      (nreverse candidates))))

(defvar swiper-helm-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'helm-next-line)
    (define-key map (kbd "C-r") 'helm-previous-line)
    map)
  "Allows you to go to next and previous hit isearch-style.")

;;;###autoload
(defun swiper ()
  "`isearch' with an overview."
  (interactive)
  (if (and (eq 'swiper-completion-method 'helm)
           (featurep 'helm))
      (swiper--helm)
    (swiper--ivy)))

(defun swiper--init ()
  "Perform initialization common to both completion methods."
  (deactivate-mark)
  (setq swiper--len 0)
  (setq swiper--anchor (line-number-at-pos))
  (setq swiper--window (selected-window)))

(defun swiper--ivy ()
  "`isearch' with an overview using `ivy'."
  (interactive)
  (ido-mode -1)
  (swiper--init)
  (unwind-protect
       (let ((res (ivy-read "pattern: "
                            (swiper--candidates)
                            #'swiper--update-input-ivy)))
         (goto-char (point-min))
         (forward-line (1- (read res)))
         (re-search-forward
          (ivy--regex ivy-text)
          (line-end-position)
          t))
    (ido-mode 1)
    (swiper--cleanup)))

(defun swiper--helm ()
  "`isearch' with an overview using `helm'."
  (interactive)
  (require 'helm)
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
                 (action . swiper--action))
               :keymap (make-composed-keymap
                        swiper-helm-keymap
                        helm-map)
               :preselect
               (format "^%d " swiper--anchor)
               :buffer "*swiper*"))
    ;; cleanup
    (remove-hook 'helm-move-selection-after-hook #'swiper--update-sel)
    (remove-hook 'helm-update-hook #'swiper--update-input-helm)
    (remove-hook 'helm-after-update-hook #'swiper--reanchor)
    (swiper--cleanup)))

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (delete-overlay (pop swiper--overlays))))

(defvar swiper--overlays nil
  "Store overlays.")

(defvar swiper--anchor nil
  "A line number to which the search should be anchored.")

(defvar swiper--len 0
  "The last length of input for which an anchoring was made.")

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

(defun swiper--update-input-ivy ()
  "Called when `ivy' input is updated."
  (swiper--cleanup)
  (let* ((re (ivy--regex ivy-text))
         (str ivy--current)
         (num (if (string-match "^[0-9]+" str)
                  (string-to-number (match-string 0 str))
                0)))
    (with-selected-window swiper--window
      (goto-char (point-min))
      (when (plusp num)
        (goto-char (point-min))
        (forward-line (1- num))
        (recenter))
      (let ((ov (make-overlay
                 (line-beginning-position)
                 (1+ (line-end-position)))))
        (overlay-put ov 'face 'swiper-line-face)
        (overlay-put ov 'window swiper--window)
        (push ov swiper--overlays))
      (swiper--add-overlays
       re
       (window-start swiper--window)
       (window-end swiper--window t)))))

(defun swiper--add-overlays (re beg end)
  "Add overlays for RE regexp in current buffer between BEG and END."
  (when (> (length re) 1)
    (save-excursion
      (goto-char beg)
      ;; RE can become an invalid regexp
      (while (ignore-errors (re-search-forward re end t))
        (let ((i 0))
          (while (<= i ivy--subexps)
            (when (match-beginning i)
              (let ((overlay (make-overlay (match-beginning i)
                                           (match-end i)))
                    (face
                     (cond ((zerop ivy--subexps)
                            (cl-caddr swiper-faces))
                           ((zerop i)
                            (car swiper-faces))
                           (t
                            (nth (1+ (mod (1- i) (1- (length swiper-faces))))
                                 swiper-faces)))))
                (push overlay swiper--overlays)
                (overlay-put overlay 'face face)
                (overlay-put overlay 'window swiper--window)
                (overlay-put overlay 'priority i)))
            (cl-incf i)))))))

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
  (with-helm-window
    (goto-char (point-min))
    (if (re-search-forward (format "^%d " swiper--anchor) nil t)
        nil
      (forward-line 1)
      (swiper--binary 2 (1+ (count-lines (point) (point-max)))))
    (when (> (count-lines (point-min) (point-max)) 1)
      (forward-line -1)
      (helm-next-line 1))))

(defun swiper--action (x)
  "Goto line X."
  (goto-char (point-min))
  (forward-line (1- (read x)))
  (re-search-forward
   (ivy--regex helm-input) (line-end-position) t))

(provide 'swiper)

;;; swiper.el ends here
