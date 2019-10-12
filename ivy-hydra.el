;;; ivy-hydra.el --- Additional key bindings for Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2019  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.13.0
;; Package-Requires: ((emacs "24.5") (ivy "0.13.0") (hydra "0.15.0"))
;; Keywords: convenience

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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the `hydra-ivy/body' command, which is a
;; quasi-prefix map, with many useful bindings.  These bindings are
;; shorter than usual, using mostly unprefixed keys.

;;; Code:

(require 'ivy)
(require 'hydra)

(defun ivy--matcher-desc ()
  "Return description of `ivy--regex-function'."
  (let ((cell (assq ivy--regex-function ivy-preferred-re-builders)))
    (if cell
        (cdr cell)
      "other")))

(defhydra hydra-ivy (:hint nil
                     :color pink)
  "
^ ^ ^ ^ ^ ^ | ^Call^      ^ ^  | ^Cancel^ | ^Options^ | Action _w_/_s_/_a_: %-14s(ivy-action-name)
^-^-^-^-^-^-+-^-^---------^-^--+-^-^------+-^-^-------+-^^^^^^^^^^^^^^^^^^^^^^^^^^^^^---------------------------
^ ^ _k_ ^ ^ | _f_ollow occ_U_r | _i_nsert | _c_: calling %-5s(if ivy-calling \"on\" \"off\") _C_ase-fold: %-10`ivy-case-fold-search
_h_ ^+^ _l_ | _d_one      ^ ^  | _o_ops   | _M_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _T_runcate: %-11`truncate-lines
^ ^ _j_ ^ ^ | _g_o        ^ ^  | ^ ^      | _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _D_efinition of this menu
"
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-end-of-buffer)
  ;; mark
  ("m" ivy-mark)
  ("u" ivy-unmark)
  ("DEL" ivy-unmark-backward)
  ("t" ivy-toggle-marks)
  ;; actions
  ("o" keyboard-escape-quit :exit t)
  ("r" ivy-dispatching-done-hydra :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("i" nil)
  ("C-o" nil)
  ("f" ivy-alt-done :exit nil)
  ("C-j" ivy-alt-done :exit nil)
  ("d" ivy-done :exit t)
  ("g" ivy-call)
  ("C-m" ivy-done :exit t)
  ("c" ivy-toggle-calling)
  ("M" ivy-rotate-preferred-builders)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" (let ((ivy-read-action-function #'ivy-read-action-by-key))
         (ivy-read-action)))
  ("T" (setq truncate-lines (not truncate-lines)))
  ("C" ivy-toggle-case-fold)
  ("U" ivy-occur :exit t)
  ("D" (ivy-exit-with-action
        (lambda (_) (find-function 'hydra-ivy/body)))
       :exit t))

(defvar ivy-dispatching-done-columns 2
  "Number of columns to use if the hint does not fit on one line.")

(defvar ivy-dispatching-done-idle nil
  "When non-nil, the hint will be delayed by this many seconds.")

(defvar ivy-dispatching-done-hydra-exit-keys '(("M-o" nil "back")
                                               ("C-g" nil))
  "Keys that can be used to exit `ivy-dispatching-done-hydra'.")

(defun ivy-dispatching-done-hydra ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (let* ((actions (ivy-state-action ivy-last))
         (extra-actions ivy-dispatching-done-hydra-exit-keys)
         (doc (concat "action: "
                      (mapconcat
                       (lambda (x) (format "[%s] %s" (nth 0 x) (nth 2 x)))
                       (append (cdr actions)
                               extra-actions) ", ")))
         (estimated-len (length doc))
         (n-columns (if (> estimated-len (window-width))
                        ivy-dispatching-done-columns
                      nil))
         (i 0))
    (if (null (ivy--actionp actions))
        (ivy-done)
      (funcall
       (eval
        `(defhydra ivy-read-action (:color teal :columns ,n-columns :idle ,ivy-dispatching-done-idle)
           "action"
           ,@(mapcar (lambda (x)
                       (list (nth 0 x)
                             `(progn
                                (setcar (ivy-state-action ivy-last) ,(cl-incf i))
                                (ivy-done))
                             (nth 2 x)))
                     (cdr actions))
           ,@extra-actions))))))

(setq ivy-read-action-function (lambda (_) (ivy-dispatching-done-hydra)))

(provide 'ivy-hydra)

;;; ivy-hydra.el ends here
