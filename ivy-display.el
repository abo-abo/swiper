;;; ivy-display.el --- Experimental display functions for Ivy  -*- lexical-binding: t; -*-

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
;; This package experiments with using other packages' display
;; functions for Ivy.

;;; Code:
(require 'lv nil t)
(require 'popup nil t)

(defun ivy-display-function-lv (text)
  (let ((lv-force-update t))
    (lv-message
     (if (string-match "\\`\n" text)
         (substring text 1)
       text))))

(defun ivy-display-function-popup (text)
  (with-ivy-window
    (popup-tip
     (setq ivy-insert-debug
           (substring text 1))
     :nostrip t)))

(provide 'ivy-display)
;;; ivy-display.el ends here
