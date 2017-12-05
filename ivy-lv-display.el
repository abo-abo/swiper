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
;; display usage hints, which can be edited according to the user's
;; need.
;;
;; These features are toggled by evaluating function
;; `ivy-lv-display-mode'. When the feature is enabled and focus is in
;; an ivy minibuffer, display of hints are toggled by keybinding
;; `M-x ?'.
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
  "Face used for elements in an `ivy-lv-hint-msg'."
  :group 'ivy)

(defcustom ivy-lv-hint-msg-text '(
"M-*   toggle regex method
M-c   toggle manual / auto act
M-^   toggle case-sensitivity"
"M-;   act on what you typed, ie. not current selection
C-w   insert word from point in main buffer
M-i   insert current selection in mini-buffer"
"M-+  grow list height
M--  shrink list height"
"M-=      begin new regex on current candiadate subset
C-c C-o  copy list to actionable buffer
M-w      copy list to kill-ring"
"Act without exiting:
  C-M-m  on current selection
  C-M-n  on next selection
  C-M-p  on prefv selection"
"Change default action:
  M-.    next action
  M-,    prev action
  C-M-a  choose from a list")
"A list of messages to display above the ivy minibuffer when function `ivy-lv-display' is called with the optional HINT arg."
  :type  '(repeat string)
  :group 'ivy)


;;* Globals
(defvar ivy-show-lv-display t
  "Whether the ivy-lv-display is enabled.

DO NOT set this variable directly. Instead, evaluate function `ivy-lv-display-mode'.")

(defvar ivy-lv-hint-msg "")

(defvar num-ivy-lv-hints (length ivy-lv-hint-msg))

(defvar ivy-lv-hint num-ivy-lv-hints)


;;* Commands
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
    (setq num-ivy-lv-hints (length ivy-lv-hint-msg)
          ivy-lv-hint (% (1+ ivy-lv-hint) (1+ num-ivy-lv-hints))))
  (lv-message "  case[%s] regex[%s] action: %s %s\n%s"
    (propertize (symbol-name ivy-case-fold-search) 'face 'ivy-lv-item)
    (propertize (substring (symbol-name ivy--regex-function)
                  (1+ (search "-" (symbol-name ivy--regex-function)
                        :from-end t)))             'face 'ivy-lv-item)
    (propertize (ivy-action-name)                  'face 'ivy-lv-item)
    (propertize (if ivy-calling "auto" "")         'face 'ivy-lv-item)
    (if (< ivy-lv-hint num-ivy-lv-hints)
      (nth ivy-lv-hint ivy-lv-hint-msg)
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
       (setq ivy-lv-hint-msg (mapcar 'lv-hint-propertize ivy-lv-hint-msg-text))
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
