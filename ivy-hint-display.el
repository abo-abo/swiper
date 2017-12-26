;; ivy-hint-display.el --- Alternative interface for ivy -*- lexical-binding: t -*-

;; Copyright (C) 2017, Boruch Baum (assignable to Free Software Foundation, Inc.)

;; Author: Boruch Baum <boruch_baum@gmx.com>
;; URL: https://github.com/Boruch-Baum/swiper
;; Version: 1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: matching

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
;; This package provides an informative interface for ivy.
;;
;; 1. Although Ivy has multiple modes of operation, multiple regex
;;    techniques, and multiple actions, Ivy does not give a user
;;    visual feedback of its current state of operation, this even
;;    though a user can dynamically change any of those in
;;    mid-selection. It is very easy to lose track.
;;
;; 2. Ivy users may also suffer confusion from the many keybindings
;;    for the many options that Ivy offers.
;;
;; This package addresses both those issues. It provides a
;; single-line informative display above the minibuffer, in the
;; form: "case[%s] regex[%s] action: %s%s", and optionally also
;; displays usage hints, which can be edited according to the user's
;; need.
;;
;; The features of this package are toggled by evaluating function
;; `ivy-hint-display-mode'. When the feature is enabled and focus is in
;; an ivy minibuffer, display of hints are toggled by keybinding
;; `M-?'.
;;
;; TODO:
;; 1. Each function has a single action labeled 'default', which isn't
;;    as helpful a description for a user toggling through the actions
;;    as actually saying what the default is, especially since in most
;;    cases it is just a duplicate for one of the other actions. It
;;    would be nice to have that terminology eliminated in favor of a
;;    more descriptive label.

;;; Code:
(require 'ivy)
(require 'ivy-hint-lv) ; for function `lv-hint-lv-message'

;;* Customization
(defface ivy-hint-item-face
  '((t :foreground "00aaff"))
  "Face used for elements in an ivy-hint-display prompt."
  :group 'ivy)

(defface ivy-hint-face
  '((t :foreground "yellow" :background "color-234"))
  "Face used for elements in an `ivy-hint-msgs'."
  :group 'ivy)

(defcustom ivy-hint-show-default-msgs t
"Whether to display the default list of hints when function
`ivy-hint-display' is called with the optional HINT arg."
  :type 'boolean
  :group 'ivy)

(defcustom ivy-hint-custom-msgs-text nil
"A list of CUSTOM messages to display above the ivy minibuffer
when function `ivy-hint-display' is called with the optional HINT
arg. These will be displayed in addition to, and prior to, the
default message list, if that feature is enabled (ie.
`ivy-hint-show-default-msgs' set to `t'."
  :type  '(repeat string)
  :group 'ivy)

;;* Globals
(defvar ivy-hint-show-display t
  "Whether the ivy-hint-display is currently enabled.

DO NOT set this variable directly. Instead, evaluate function `ivy-hint-display-mode'.")

(defvar ivy-hint-msgs ""
"At run-time will hold the propertized CONS of `ivy-hint-msgs-text'.")

(defvar num-ivy-hints nil
"At run-time will hold the integer number of hint messages.")

(defvar curr-ivy-hint nil
"The current index into the list of hint messages.")

(defvar ivy-hints-default-template '(
  (('ivy-rotate-preferred-builders "   toggle regex method")
   ('ivy-toggle-calling      "   toggle manual / auto act")
   ('ivy-toggle-case-fold    "   toggle case-sensitivity"))
  (('ivy-immediate-done      "   act on what you typed, ie. not current selection")
   ('ivy-yank-word           "   insert word from point in main buffer")
   ('ivy-insert-current      "   insert current selection in mini-buffer"))
  (('ivy-minibuffer-grow     "  grow list height")
   ('ivy-minibuffer-shrink   "  shrink list height"))
  (('ivy-restrict-to-matches "      begin new regex on current candiadate subset")
   ('ivy-occur               "  copy list to actionable buffer")
   ('ivy-kill-ring-save      "      copy list to kill-ring"))
  ((nil "Act without exiting:")
   ('ivy-call                    "  on current selection")
   ('ivy-next-line-and-call      "  on next selection")
   ('ivy-previous-line-and-call  "  on prev selection"))
  ((nil "Change default action:")
   ('ivy-next-action "    next action")
   ('ivy-prev-action "    prev action")
   ('ivy-read-action "  choose from a list"))))

(defvar ivy-hints-default-bindings '(
  ("M-^" t   ivy-toggle-case-fold)
  ("M-t" t   ivy-toggle-fuzzy)
  ("M-*" t   ivy-rotate-preferred-builders)
  ("M-q" t   ivy-toggle-regexp-quote)
  ("M-c" t   ivy-toggle-calling)
  ("M-+" nil ivy-minibuffer-grow)
  ("M--" nil ivy-minibuffer-shrink)
  ("M-." t   ivy-next-action)
  ("M-," t   ivy-prev-action))
"A list of default bindings for certain ivy functions.
An entry should exist for each ivy function described in
`ivy-hints-default-template'. For each entry, the first element
is a default keybinding, the second element is a boolean value
indicating whether the `ivy-hint-lv-display' should be refreshed
after the function is called, and the final element is the
function name. This list will be used at run-time to assign
keybindings if they have not already been set, and to remap the
functions to ensure that the `ivy-hint-lv-display' is always
properly refreshed.")

;;* Commands

(defun ivy-hints-propertize (msg)
  "Apply `ivy-hint-face' to all lines of the hint message, in a
manner such that if the face includes a BACKGROUND property, that
property appears upon a rectangular region."
  (let* (
    (lines (split-string msg "\n"))
    (maxlen 1) (maxlen
      (dolist (line lines maxlen)
        (setq maxlen (max maxlen (or (length line) 0)))))
    (fmt (format "%%-%ds" maxlen)))
   (seq-mapcat
     (lambda(x)
       (concat (propertize (format fmt x) 'face 'ivy-hint-face) "\n"))
     lines
     'string)))

(defun ivy-hint-build-hints ()
  (let* (
    (make-hint-part (lambda(x) (format "%s%s"
      (if (not (car x)) ""
       (or (key-description (where-is-internal (eval (car x)) ivy-minibuffer-map 'non-ascii))
           "---"))
      (cadr x))))
    (make-hint-whole (lambda(x) (mapconcat make-hint-part x "\n")))
    (text
      (if (not ivy-hint-show-default-msgs) nil
       (mapcar make-hint-whole ivy-hints-default-template)))
    (text (cond
      ((and ivy-hint-custom-msgs-text ivy-hint-show-default-msgs)
        (cons ivy-hint-custom-msgs-text text))
      (ivy-hint-show-default-msgs text)
      (ivy-hint-custom-msgs-text ivy-hint-custom-msgs-text)
      (t nil))))
  (if text (mapcar 'ivy-hints-propertize text) nil)))

(defun ivy-hint-display(&optional hint)
  "Display an extended, possibly multi-line message above the minibuffer.

The basic message is a single-line status indication of
currently-enabled `ivy' options: whether searches are case
sensitive, which regex method is being used, what is the current
action to be taken on a selection, and whether that action is
performed automatically (\"calling\" in `ivy' terminology).

When called with the optional HINT argument non-nil, this
function also cycles through a user-configurable set of
additional messages, originally meant as a local cheat sheet for
ivy keybindings."
  (when hint
    (setq num-ivy-hints (length ivy-hint-msgs)
          curr-ivy-hint (% (1+ curr-ivy-hint) (1+ num-ivy-hints))))
  (ivy-hint-lv-message "  case[%s] regex[%s] action: %s %s\n%s"
    (propertize (symbol-name ivy-case-fold-search) 'face 'ivy-hint-item-face)
    (propertize (substring (symbol-name ivy--regex-function)
                  (1+ (search "-" (symbol-name ivy--regex-function)
                        :from-end t)))             'face 'ivy-hint-item-face)
    (propertize (ivy-action-name)                  'face 'ivy-hint-item-face)
    (propertize (if ivy-calling "auto" "")         'face 'ivy-hint-item-face)
    (if (< curr-ivy-hint num-ivy-hints)
      (nth curr-ivy-hint ivy-hint-msgs)
     "")))

(defun ivy-hint-remap ()
  (dolist (this ivy-hints-default-bindings)
    (unless (where-is-internal (caddr this) ivy-minibuffer-map)
      (define-key ivy-minibuffer-map (kbd (car this)) (caddr this)))
    (when (cadr this)
      (define-key ivy-minibuffer-map (vector 'remap (caddr this))
        (lambda()(interactive) (funcall (caddr this)) (ivy-hint-display))))))

(defun ivy-hint-unmap ()
  (dolist (this ivy-hints-default-bindings)
    (when (cadr this)
      (define-key ivy-minibuffer-map (vector 'remap (caddr this)) nil))))

(defun ivy-hint-display-mode(&optional arg)
  "Toggle display of the ivy hint display.

Interactively, toggles the mode. Programmatically, enables the
mode when ARG is nil, and disables it when ARG is a negative
integer. Note that this is NOT a 'mode'. The term is borrowed
because taht is word emacs user will expect."
  (interactive "p")
  (setq ivy-hint-show-display
    (cond
      ((not arg) t)
      ((< arg 0) nil)
      (t (not ivy-hint-show-display))))
  (cond
    (ivy-hint-show-display
      (ivy-hint-remap)
      (setq ivy-hint-msgs (ivy-hint-build-hints))
      (setq num-ivy-hints (length ivy-hint-msgs))
      (setq curr-ivy-hint num-ivy-hints)
      (define-key ivy-minibuffer-map (kbd "M-?")
        (lambda()(interactive)(ivy-hint-display t)))
      (message "ivy-hint-display enabled"))
    (t
      (ivy-hint-unmap)
      (define-key ivy-minibuffer-map (kbd "M-?") nil)
      (message "ivy-hint-display disabled"))))

(provide 'ivy-hint-display)

;;; ivy-hint-display.el ends here
