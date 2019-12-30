;;; ivy-resize.el --- Auto resize `ivy' window base on number of candidates  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Keyword: resize

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Implementation of automatically resize the `ivy' window depends on
;; the height of the minibuffer.

;;; Code:

(require 'ivy)


(defgroup ivy-resize nil
  "Auto resize `ivy' window base on number of candidates."
  :prefix "ivy-resize-"
  :group 'tools
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/ivy-resize"))


;;;###autoload
(define-minor-mode ivy-resize-mode
  "Minor mode 'ivy-resize-mode'."
  :global t
  :require 'ivy-resize
  :group 'ivy-resize)

(defun ivy-resize--minibuffer-setup-hook ()
  "Minibuffer setup hook."
  (add-hook 'post-command-hook #'ivy-resize--post-command-hook nil t))

(defun ivy-resize--post-command-hook ()
  "Hook run every command in minibuffer."
  (when (and ivy-mode ivy-resize-mode)
    (shrink-window (1+ ivy-height))))  ; Plus 1 for the input field.

(defun ivy-resize--minibuffer-do-stuff (fnc &rest args)
  "Execute FNC and ARGS in minibuffer the safe way."
  (if (not (active-minibuffer-window))
      (user-error "[ERROR] Minibuffer not active to do stuff: %s" fnc)
    (save-selected-window
      (select-window (active-minibuffer-window))
      (apply fnc args))))

(defun ivy-resize-window-once ()
  "Resize the ivy window once."
  (when (minibufferp)
    (ivy-resize--minibuffer-do-stuff
     (lambda ()
       (shrink-window (1+ ivy-height))  ; Plus 1 for the input field.
       (ivy--resize-minibuffer-to-fit)))))

(defun ivy-resize--window-size-change-functions (&rest _)
  "When window changed size."
  (when ivy-resize-mode (ivy-resize-window-once)))

(add-hook 'minibuffer-setup-hook 'ivy-resize--minibuffer-setup-hook)
(add-hook 'window-size-change-functions 'ivy-resize--window-size-change-functions)


(provide 'ivy-resize)
;;; ivy-resize.el ends here
