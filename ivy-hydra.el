;;; ivy-hydra.el --- Additional key bindings for Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2021 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.13.5
;; Package-Requires: ((emacs "24.5") (ivy "0.13.4") (hydra "0.14.0"))
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

(defun ivy-minibuffer-grow ()
  "Grow the minibuffer window by 1 line."
  (interactive)
  (setq-local max-mini-window-height
              (cl-incf ivy-height)))

(defun ivy-minibuffer-shrink ()
  "Shrink the minibuffer window by 1 line."
  (interactive)
  (when (> ivy-height 2)
    (setq-local max-mini-window-height
                (cl-decf ivy-height))
    (window-resize nil -1)))

(defun ivy-hydra--read-action ()
  "Read one of the available actions.
Like `ivy-read-action', but unaffected by
`ivy-read-action-function'."
  (interactive)
  (let ((ivy-read-action-function #'ivy-read-action-by-key))
    (ivy-read-action)))

(defun ivy-hydra--toggle-truncate-lines ()
  "Toggle `truncate-lines'."
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(defun ivy-hydra--find-definition ()
  "Find the definition of `hydra-ivy'."
  (interactive)
  (ivy-exit-with-action
   (lambda (_) (find-function #'hydra-ivy/body))))

(defhydra hydra-ivy (:hint nil :color pink)
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
  ("r" ivy-dispatching-done :exit t)
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
  ("a" ivy-hydra--read-action)
  ("T" ivy-hydra--toggle-truncate-lines)
  ("C" ivy-toggle-case-fold)
  ("U" ivy-occur :exit t)
  ("D" ivy-hydra--find-definition :exit t))
(dolist (cmd '(;; These commands have a binding here.
               ivy-hydra--find-definition
               ivy-hydra--read-action
               ivy-hydra--toggle-truncate-lines
               ivy-next-action ivy-prev-action
               ivy-unmark-backward ivy-toggle-case-fold
               ivy-minibuffer-grow ivy-minibuffer-shrink
               ivy-rotate-preferred-builders ivy-toggle-calling
               ;; No binding.
               ivy-next-line-or-history ivy-previous-line-or-history
               ivy-toggle-fuzzy ivy-yank-symbol
               ivy-occur-next-error))
  (function-put cmd 'no-counsel-M-x t))

(defvar ivy-dispatching-done-columns 2
  "Number of columns to use if the hint does not fit on one line.")

(defvar ivy-dispatching-done-idle nil
  "When non-nil, the hint will be delayed by this many seconds.")

(defvar ivy-dispatching-done-hydra-exit-keys '(("M-o" nil "back")
                                               ("C-g" nil))
  "Keys that can be used to exit `ivy-hydra-read-action'.")

(defun ivy-hydra-read-action (actions)
  "Select one of the available actions and call `ivy-done'."
  (let* ((extra-actions ivy-dispatching-done-hydra-exit-keys)
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
                                (let ((prev-idx (car (ivy-state-action ivy-last))))
                                  (setcar (ivy-state-action ivy-last) ,(cl-incf i))
                                  ,@(if (eq ivy-exit 'ivy-dispatching-done)
                                        '((ivy-done))
                                      '((ivy-call)
                                        (setcar (ivy-state-action ivy-last) prev-idx)))))
                             (nth 2 x)))
                     (cdr actions))
           ,@extra-actions)))
      nil)))


(provide 'ivy-hydra)

;;; ivy-hydra.el ends here
