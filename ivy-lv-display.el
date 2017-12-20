;; ivy-lv-display.el --- Alternative interface for ivy -*- lexical-binding: t -*-

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
;; These features are toggled by evaluating function
;; `ivy-lv-display-mode'. When the feature is enabled and focus is in
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
(require 'lv)

;;* Customization
(defface ivy-lv-item
  '((t :foreground "00aaff"))
  "Face used for elements in an ivy-lv-display prompt."
  :group 'ivy)

(defface ivy-lv-hint-face
  '((t :foreground "yellow" :background "color-234"))
  "Face used for elements in an `ivy-lv-hint-msgs'."
  :group 'ivy)

(defcustom ivy-lv-hint-show-default-msgs t
"Whether to display the default list of hints when function
`ivy-lv-display' is called with the optional HINT arg."
  :type 'boolean
  :group 'ivy)

(defcustom ivy-lv-custom-hint-msgs-text nil
"A list of CUSTOM messages to display above the ivy minibuffer
when function `ivy-lv-display' is called with the optional HINT
arg. These will be displayed in addition to, and prior to, the
default message list, if that feature is enabled (ie.
`ivy-lv-hint-show-default-msgs' set to `t'."
  :type  '(repeat string)
  :group 'ivy)

;;* Globals
(defvar ivy-show-lv-display t
  "Whether the ivy-lv-display is currently enabled.

DO NOT set this variable directly. Instead, evaluate function `ivy-lv-display-mode'.")

(defvar ivy-lv-hint-msgs-text ""
"At run-time will hold the un-propertized CONS of `ivy-lv-custom-hint-msgs-text'
and `ivy-lv-hint-default-msg-text'.")

(defvar ivy-lv-hint-msgs ""
"At run-time will hold the propertized CONS of `ivy-lv-hint-msgs-text'.")

(defvar num-ivy-lv-hints nil
"At run-time will hold the integer number of hint messages.")

(defvar curr-ivy-lv-hint nil
"The current index into the list of hint messages.")

(defvar ivy-lv-hints-default-template '(
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

(defvar ivy-lv-hints-default-msgs-text ""
  "When ivy is loaded it will construct the default hints, in order
to accurately present any custom keybindings that the user has
set.")

;;* Commands
(defun ivy-lv-single-hint-build-default (hint)
  "Construct a single default hint, respecting any custom keybinding
the user has set."
  (mapconcat (lambda(x)
    (format "%s%s"
      (if (car x)
        (or (key-description
              (where-is-internal (eval (car x)) ivy-minibuffer-map 'non-ascii))
            "---")
       "")
      (cadr x)))
    hint "\n"))

(defun ivy-lv-hints-build-default ()
  "Construct `ivy-lv-hints-default-msgs-text', based upon
`ivy-lv-hints-default-template', respecting any custom keybinding
the user has set."
  (when ivy-lv-hint-show-default-msgs
    (setq ivy-lv-hints-default-msgs-text
      (mapcar 'ivy-lv-single-hint-build-default ivy-lv-hints-default-template))))

(defun ivy-lv-build-hints ()
  (setq ivy-lv-hint-msgs-text (ivy-lv-hints-build-default))
  (when ivy-lv-custom-hint-msgs-text
    (setq ivy-lv-hint-msgs-text (cons ivy-lv-custom-hint-msgs-text ivy-lv-hint-msgs-text))))

(defun lv-hint-propertize (msg)
  "Apply `ivy-lv-hint-face' to all lines of the hint message, in a
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
       (concat (propertize (format fmt x) 'face 'ivy-lv-hint-face) "\n"))
     lines
     'string)))

(defun ivy-lv-display(&optional hint)
  "Display an extended, possibly multi-line message above the minibuffer.

This message contains the detailed status of `ivy'. When called with the optional HINT argument non-nil, this function also cycles through a user-configurable set of additional messages, originally meant as a local cheat sheet for ivy keybindings."
  (when hint
    (setq num-ivy-lv-hints (length ivy-lv-hint-msgs-text)
          curr-ivy-lv-hint (% (1+ curr-ivy-lv-hint) (1+ num-ivy-lv-hints))))
  (lv-message "  case[%s] regex[%s] action: %s %s\n%s"
    (propertize (symbol-name ivy-case-fold-search) 'face 'ivy-lv-item)
    (propertize (substring (symbol-name ivy--regex-function)
                  (1+ (search "-" (symbol-name ivy--regex-function)
                        :from-end t)))             'face 'ivy-lv-item)
    (propertize (ivy-action-name)                  'face 'ivy-lv-item)
    (propertize (if ivy-calling "auto" "")         'face 'ivy-lv-item)
    (if (< curr-ivy-lv-hint num-ivy-lv-hints)
      (nth curr-ivy-lv-hint ivy-lv-hint-msgs)
     "")))

(defun ivy-lv-display-mode(&optional arg)
  "Toggle display of the ivy LV display.

Interactively, toggles the mode. Programmatically, enables the
mode when ARG is nil, and disables it when ARG is a negative
integer. Note that this is NOT a 'mode'. The term is borrowed
because taht is word emacs user will expect."
  (interactive "p")
  (setq ivy-show-lv-display
    (cond
      ((not arg) t)
      ((< arg 0) nil)
      (t (not ivy-show-lv-display))))
  (cond
    (ivy-show-lv-display
       (message "ivy-lv-display enabled")
       (setq ivy-lv-hint-msgs (mapcar 'lv-hint-propertize (ivy-lv-build-hints)))
       (setq num-ivy-lv-hints (length ivy-lv-hint-msgs))
       (define-key ivy-minibuffer-map (kbd "M-?")
         (lambda()(interactive)(ivy-lv-display t)))
       (define-key ivy-minibuffer-map [remap ivy-toggle-case-fold]
         (lambda()(interactive)(ivy-toggle-case-fold)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-toggle-fuzzy]
         (lambda()(interactive)(ivy-toggle-fuzzy)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-rotate-preferred-builders]
         (lambda()(interactive)(ivy-rotate-preferred-builders)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-toggle-regexp-quote]
         (lambda()(interactive)(ivy-toggle-regexp-quote)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-toggle-calling]
         (lambda()(interactive)(ivy-toggle-calling)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-next-action]
         (lambda()(interactive)(ivy-next-action)
            (ivy-lv-display)))
       (define-key ivy-minibuffer-map [remap ivy-prev-action]
         (lambda()(interactive)(ivy-prev-action)
            (ivy-lv-display))))
    (t (message "ivy-lv-display disabled")
       (define-key ivy-minibuffer-map (kbd "M-?") nil)
       (define-key ivy-minibuffer-map [remap ivy-toggle-case-fold] nil)
       (define-key ivy-minibuffer-map [remap ivy-toggle-fuzzy] nil)
       (define-key ivy-minibuffer-map [remap ivy-rotate-preferred-builders] nil)
       (define-key ivy-minibuffer-map [remap ivy-toggle-regexp-quote] nil)
       (define-key ivy-minibuffer-map [remap ivy-toggle-calling] nil)
       (define-key ivy-minibuffer-map [remap ivy-next-action] nil)
       (define-key ivy-minibuffer-map [remap ivy-prev-action] nil))))

(provide 'ivy-lv-display)

;;; ivy-lv-display.el ends here
