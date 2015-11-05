;;; ivy-hydra.el --- Additional key bindings for Ivy  -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

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
;; This package provides the `hydra-ivy/body' command, which is a
;; quasi-prefix map, with many useful bindings.  These bindings are
;; shorter than usual, using mostly unprefixed keys.

;;; Code:
(require 'hydra nil t)
(require 'ivy)

(eval-when-compile
  (unless (or (featurep 'hydra) (package-installed-p 'hydra))
    (defmacro defhydra (name &rest _)
      "This is a stub for the uninstalled `hydra' package."
      `(defun ,(intern (format "%S/body" name)) ()
         (interactive)
         (let ((enable-recursive-minibuffers t))
           (if (yes-or-no-p "Package `hydra' not installed. Install?")
               (progn
                 (package-install 'hydra)
                 (save-window-excursion
                   (find-library "ivy-hydra")
                   (byte-compile-file (buffer-file-name) t)))
             (error "Please install `hydra' and recompile/reinstall `ivy-hydra'")))))))

(defun ivy--matcher-desc ()
  (if (eq ivy--regex-function
          'ivy--regex-fuzzy)
      "fuzzy"
    "ivy"))

(defhydra hydra-ivy (:hint nil
                     :color pink)
  "
^^^^^^          ^Yes^    ^^      ^No^     ^Maybe^            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^Action^               ^
^^^^^^^^^^^^^^^----------------------------------------------^^^^^^^^^^^^^^^^^^^^^^^^^^^^^-----------------------
^ ^ _k_ ^ ^     _f_ollow occ_u_r _i_nsert _c_: calling %-5s(if ivy-calling \"on\" \"off\") _w_/_s_/_a_: %-14s(ivy-action-name)
_h_ ^+^ _l_     _d_one      ^ ^  _o_ops   _m_: matcher %-5s(ivy--matcher-desc)^^^^^^^^^^^^ _C_ase-fold: %-10`ivy-case-fold-search
^ ^ _j_ ^ ^     _g_o        ^ ^  ^ ^      _<_/_>_: shrink/grow^^^^^^^^^^^^^^^^^^^^^^^^^^^^ _t_runcate: %-11`truncate-lines
"
  ;; arrows
  ("h" ivy-beginning-of-buffer)
  ("j" ivy-next-line)
  ("k" ivy-previous-line)
  ("l" ivy-end-of-buffer)
  ;; actions
  ("o" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("i" nil)
  ("C-o" nil)
  ("f" ivy-alt-done :exit nil)
  ("C-j" ivy-alt-done :exit nil)
  ("d" ivy-done :exit t)
  ("g" ivy-call)
  ("C-m" ivy-done :exit t)
  ("c" ivy-toggle-calling)
  ("m" ivy-toggle-fuzzy)
  (">" ivy-minibuffer-grow)
  ("<" ivy-minibuffer-shrink)
  ("w" ivy-prev-action)
  ("s" ivy-next-action)
  ("a" ivy-read-action)
  ("t" (setq truncate-lines (not truncate-lines)))
  ("C" ivy-toggle-case-fold)
  ("u" ivy-occur :exit t))

(provide 'ivy-hydra)

;;; ivy-hydra.el ends here
