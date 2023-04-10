;;; ivy-faces.el --- Faces for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup ivy-faces nil
  "Font-lock faces for `ivy'."
  :group 'ivy
  :group 'faces)

(defface ivy-current-match
  '((((class color) (background light))
     :background "#1a4b77" :foreground "white" :extend t)
    (((class color) (background dark))
     :background "#65a7e2" :foreground "black" :extend t))
  "Face used by Ivy for highlighting the current match.")

(defface ivy-minibuffer-match-highlight
  '((t :inherit highlight))
  "Face used by Ivy for highlighting the match under the cursor.")

(defface ivy-minibuffer-match-face-1
  '((((class color) (background light))
     :background "#d3d3d3")
    (((class color) (background dark))
     :background "#555555"))
  "The background face for `ivy' minibuffer matches.")

(defface ivy-minibuffer-match-face-2
  '((((class color) (background light))
     :background "#e99ce8" :weight bold)
    (((class color) (background dark))
     :background "#777777" :weight bold))
  "Face for `ivy' minibuffer matches numbered 1 modulo 3.")

(defface ivy-minibuffer-match-face-3
  '((((class color) (background light))
     :background "#bbbbff" :weight bold)
    (((class color) (background dark))
     :background "#7777ff" :weight bold))
  "Face for `ivy' minibuffer matches numbered 2 modulo 3.")

(defface ivy-minibuffer-match-face-4
  '((((class color) (background light))
     :background "#ffbbff" :weight bold)
    (((class color) (background dark))
     :background "#8a498a" :weight bold))
  "Face for `ivy' minibuffer matches numbered 3 modulo 3.")

(defface ivy-confirm-face
  '((t :foreground "ForestGreen" :inherit minibuffer-prompt))
  "Face used by Ivy for a confirmation prompt.")

(defface ivy-match-required-face
  '((t :foreground "red" :inherit minibuffer-prompt))
  "Face used by Ivy for a match required prompt.")

(defface ivy-subdir
  '((t :inherit dired-directory))
  "Face used by Ivy for highlighting subdirs in the alternatives.")

(defface ivy-org
  '((t :inherit org-level-4))
  "Face used by Ivy for highlighting Org buffers in the alternatives.")

(defface ivy-modified-buffer
  '((t :inherit default))
  "Face used by Ivy for highlighting modified file visiting buffers.")

(defface ivy-modified-outside-buffer
  '((t :inherit default))
  "Face used by Ivy for highlighting file visiting buffers modified outside Emacs.")

(defface ivy-remote
  '((((class color) (background light))
     :foreground "#110099")
    (((class color) (background dark))
     :foreground "#7B6BFF"))
  "Face used by Ivy for highlighting remotes in the alternatives.")

(defface ivy-virtual
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for matching virtual buffer names.")

(defface ivy-action
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for displaying keys in `ivy-read-action'.")

(defface ivy-highlight-face
  '((t :inherit highlight))
  "Face used by Ivy to highlight certain candidates.")

(defface ivy-prompt-match
  '((t :inherit ivy-current-match))
  "Face used by Ivy for highlighting the selected prompt line.")

(defface ivy-separator
  '((t :inherit font-lock-doc-face))
  "Face for multiline source separator.")

(defface ivy-grep-info
  '((t :inherit compilation-info))
  "Face for highlighting grep information such as file names.")

(defface ivy-grep-line-number
  '((t :inherit compilation-line-number))
  "Face for displaying line numbers in grep messages.")

(defface ivy-completions-annotations
  '((t :inherit completions-annotations))
  "Face for displaying completion annotations.")

(defface ivy-yanked-word
  '((t :inherit highlight))
  "Face used to highlight yanked word.")

(provide 'ivy-faces)

;;; ivy-faces.el ends here
