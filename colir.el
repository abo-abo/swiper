;;; colir.el --- Color blending library -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>

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
;; This package solves the problem of adding a face with a background
;; to text which may already have a background.  In all conflicting
;; areas, instead of choosing either the original or the new
;; background face, their alpha blended sum is used.

;;; Code:

(defun colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun colir-blend-face-background (start end face &optional object)
  "Append to the face property of the text from START to END the face FACE.
When the text already has a face with a non-plain background,
blend it with the background of FACE.
Optional argument OBJECT is the string or buffer containing the text.
See also `font-lock-append-text-property'."
  (let (next prev)
    (while (/= start end)
      (setq next (next-single-property-change start 'face object end)
            prev (get-text-property start 'face object))
      (if prev
          (let ((background-prev (face-background prev)))
            (progn
              (put-text-property
               start next 'face
               (if background-prev
                   (cons `(background-color
                           . ,(colir-blend
                               (color-values background-prev)
                               (color-values (face-background face nil t))))
                         prev)
                 (list face prev))
               object)))
        (put-text-property start next 'face face object))
      (setq start next))))

(provide 'colir)

;;; colir.el ends here
