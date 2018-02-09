;;; ivy-hint-lv.el --- secondary echo area

;; Copyright (C) 2017, Boruch Baum (assignable to Free Software Foundation, Inc.)

;; Author: Boruch Baum <boruch_baum@gmx.com>
;; URL: https://github.com/Boruch-Baum/swiper
;; Version: 1
;; Package-Requires: ((emacs "24.1") (ivy "0.10.0"))
;; Keywords: matching

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; IMPORTANT: This is a shameless copy of file `lv-el' of the `hydra'
;; package, current as of 2017-12-19, Copyright (C) 2015 Free Software
;; Foundation, Inc., authored by Oleh Krehel, and part of GNU Emacs.
;;
;; The reason for the copying was to keep package `ivy-hint' free of
;; dependency on `hydra'. For reasons I don't understand, the `lv'
;; package is bundled with `hydra', despite it having being designed
;; as a stand-alone, and IM{NS}HO of potential utility to many emacs
;; packages that have need for `hydra'.
;;
;; In order to avoid a conflict with installs that do include `hydra',
;; the symbols in this copy have been renamed, and reference to the
;; `hydra' customization group has been removed.
;;
;; A possible side-benefit of this action might be that package
;; `ivy-hint' may play nicely with `ivy-hydra', because they are now
;; using different windows (stay tuned).
;;
;; UPDATE INSTRUCTIONS:
;; 1] manually remove (defgroup ..)
;; 2] (while (search-forward "lv" nil t) (replace-match "ivy-hint-lv"))
;; 3] (while (search-forward ":group 'ivy-hint-lv" nil t) (replace-match ":group 'ivy"))

;;; Commentary:
;;
;; This package provides `ivy-hint-lv-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo Area.
;;
;;    "Я тихо-тихо пiдглядаю,
;;     І тiшуся собi, як бачу то,
;;     Шо страшить i не пiдпускає,
;;     А iншi п’ють тебе, як воду пiсок."
;;     --  Андрій Кузьменко, L.V.

;;; Code:

(require 'ivy) ; for references to customization group

(defcustom ivy-hint-lv-use-separator nil
  "Whether to draw a line between the IVY-HINT-LV window and the Echo Area."
  :group 'ivy
  :type 'boolean)

(defface ivy-hint-lv-separator
  '((((class color) (background light)) :background "grey80")
    (((class color) (background  dark)) :background "grey30"))
  "Face used to draw line between the ivy-hint-lv window and the echo area.
This is only used if option `ivy-hint-lv-use-separator' is non-nil.
Only the background color is significant."
  :group 'ivy)

(defvar ivy-hint-lv-wnd nil
  "Holds the current IVY-HINT-LV window.")

(defun ivy-hint-lv-window ()
  "Ensure that IVY-HINT-LV window is live and return it."
  (if (window-live-p ivy-hint-lv-wnd)
      ivy-hint-lv-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq ivy-hint-lv-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))))
        (if (setq buf (get-buffer " *IVY-HINT-LV*"))
            (switch-to-buffer buf)
          (switch-to-buffer " *IVY-HINT-LV*")
          (set-window-hscroll ivy-hint-lv-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (set-window-dedicated-p ivy-hint-lv-wnd t)
          (set-window-parameter ivy-hint-lv-wnd 'no-other-window t))
        (select-window ori)))))

(defvar golden-ratio-mode)

(defvar ivy-hint-lv-force-update nil
  "When non-nil, `ivy-hint-lv-message' will refresh even for the same string.")

(defun ivy-hint-lv-message (format-string &rest args)
  "Set IVY-HINT-LV window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (ivy-hint-lv-window)
      (unless (and (string= (buffer-string) str)
                   (null ivy-hint-lv-force-update))
        (delete-region (point-min) (point-max))
        (insert str)
        (when (and (window-system) ivy-hint-lv-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face 'ivy-hint-lv-separator 'display '(space :height (1)))
           (propertize "\n" 'face 'ivy-hint-lv-separator 'line-height t)))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun ivy-hint-lv-delete-window ()
  "Delete IVY-HINT-LV window and kill its buffer."
  (when (window-live-p ivy-hint-lv-wnd)
    (let ((buf (window-buffer ivy-hint-lv-wnd)))
      (delete-window ivy-hint-lv-wnd)
      (kill-buffer buf))))

(provide 'ivy-hint-lv)

;;; ivy-hint-lv.el ends here
