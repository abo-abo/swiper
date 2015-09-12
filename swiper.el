;;; swiper.el --- Isearch with an overview. Oh, man! -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.5.1
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
;; This package gives an overview of the current regex search
;; candidates.  The search regex can be split into groups with a
;; space.  Each group is highlighted with a different face.
;;
;; It can double as a quick `regex-builder', although only single
;; lines will be matched.
;;
;; It also provides `ivy-mode': a global minor mode that uses the
;; matching back end of `swiper' for all matching on your system,
;; including file matching. You can use it in place of `ido-mode'
;; (can't have both on at once).

;;; Code:
(require 'ivy)

(defgroup swiper nil
  "`isearch' with an overview."
  :group 'matching
  :prefix "swiper-")

(defface swiper-match-face-1
  '((t (:inherit isearch-lazy-highlight-face)))
  "The background face for `swiper' matches.")

(defface swiper-match-face-2
  '((t (:inherit isearch)))
  "Face for `swiper' matches modulo 1.")

(defface swiper-match-face-3
  '((t (:inherit match)))
  "Face for `swiper' matches modulo 2.")

(defface swiper-match-face-4
  '((t (:inherit isearch-fail)))
  "Face for `swiper' matches modulo 3.")

(defface swiper-minibuffer-match-face-1
  '((((class color) (background light))
     :background "#d3d3d3")
    (((class color) (background dark))
     :background "#555555"))
  "The background face for `swiper' minibuffer matches."
  :group 'function-args-faces)

(defface swiper-minibuffer-match-face-2
  '((((class color) (background light))
     :background "#e99ce8" :weight bold)
    (((class color) (background dark))
     :background "#777777" :weight bold))
  "Face for `swiper' minibuffer matches modulo 1.")

(defface swiper-minibuffer-match-face-3
  '((((class color) (background light))
     :background "#bbbbff" :weight bold)
    (((class color) (background dark))
     :background "#7777ff" :weight bold))
  "Face for `swiper' minibuffer matches modulo 2.")

(defface swiper-minibuffer-match-face-4
  '((((class color) (background light))
     :background "#ffbbff" :weight bold)
    (((class color) (background dark))
     :background "#8a498a" :weight bold))
  "Face for `swiper' minibuffer matches modulo 3.")

(defface swiper-line-face
  '((t (:inherit highlight)))
  "Face for current `swiper' line.")

(defcustom swiper-faces '(swiper-match-face-1
                          swiper-match-face-2
                          swiper-match-face-3
                          swiper-match-face-4)
  "List of `swiper' faces for group matches.")

(defcustom swiper-min-highlight 2
  "Only highlight matches for regexps at least this long."
  :type 'integer)

(defvar swiper-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") 'swiper-query-replace)
    (define-key map (kbd "C-l") 'swiper-recenter-top-bottom)
    (define-key map (kbd "C-'") 'swiper-avy)
    map)
  "Keymap for swiper.")

(defun swiper-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (if (null (window-minibuffer-p))
      (user-error "Should only be called in the minibuffer through `swiper-map'")
    (let* ((enable-recursive-minibuffers t)
           (from (ivy--regex ivy-text))
           (to (query-replace-read-to from "Query replace" t)))
      (delete-minibuffer-contents)
      (ivy-set-action (lambda (_)
                        (with-ivy-window
                          (move-beginning-of-line 1)
                          (perform-replace from to
                                           t t nil))))
      (swiper--cleanup)
      (exit-minibuffer))))

(defvar avy-background)
(defvar avy-all-windows)
(declare-function avy--regex-candidates "ext:avy")
(declare-function avy--process "ext:avy")
(declare-function avy--overlay-post "ext:avy")
(declare-function avy-action-goto "ext:avy")

;;;###autoload
(defun swiper-avy ()
  "Jump to one of the current swiper candidates."
  (interactive)
  (unless (string= ivy-text "")
    (with-ivy-window
      (let* ((avy-all-windows nil)
             (candidates
              (avy--regex-candidates
               (ivy--regex ivy-text)))
             (avy-background nil)
             (candidate
              (avy--process candidates #'avy--overlay-post)))
        (ivy-quit-and-run
         (avy-action-goto candidate))))))

(defun swiper-recenter-top-bottom (&optional arg)
  "Call (`recenter-top-bottom' ARG)."
  (interactive "P")
  (with-ivy-window
    (recenter-top-bottom arg)))

(defun swiper-font-lock-ensure ()
  "Ensure the entired buffer is highlighted."
  (unless (or (derived-mode-p 'magit-mode)
              (bound-and-true-p magit-blame-mode)
              (memq major-mode '(package-menu-mode
                                 gnus-summary-mode
                                 gnus-article-mode
                                 gnus-group-mode
                                 emms-playlist-mode erc-mode
                                 org-agenda-mode
                                 dired-mode
                                 jabber-chat-mode
                                 elfeed-search-mode
                                 fundamental-mode
                                 Man-mode
                                 woman-mode)))
    (unless (> (buffer-size) 100000)
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer))))))

(defvar swiper--format-spec ""
  "Store the current candidates format spec.")

(defvar swiper--width nil
  "Store the amount of digits needed for the longest line nubmer.")

(defun swiper--candidates ()
  "Return a list of this buffer lines."
  (let ((n-lines (count-lines (point-min) (point-max))))
    (unless (zerop n-lines)
      (setq swiper--width (1+ (floor (log n-lines 10))))
      (setq swiper--format-spec
            (format "%%-%dd " swiper--width))
      (let ((line-number 0)
            candidates)
        (save-excursion
          (goto-char (point-min))
          (swiper-font-lock-ensure)
          (while (< (point) (point-max))
            (let ((str (concat " " (buffer-substring
                                    (line-beginning-position)
                                    (line-end-position)))))
              (put-text-property 0 1 'display
                                 (format swiper--format-spec
                                         (cl-incf line-number))
                                 str)
              (push str candidates))
            (forward-line 1))
          (nreverse candidates))))))

(defvar swiper--opoint 1
  "The point when `swiper' starts.")

;;;###autoload
(defun swiper (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--ivy initial-input))

(defvar swiper--anchor nil
  "A line number to which the search should be anchored.")

(defvar swiper--len 0
  "The last length of input for which an anchoring was made.")

(defun swiper--init ()
  "Perform initialization common to both completion methods."
  (setq swiper--opoint (point))
  (setq swiper--len 0)
  (setq swiper--anchor (line-number-at-pos)))

(defun swiper--re-builder (str)
  "Transform STR into a swiper regex.
This is the regex used in the minibuffer, since the candidates
there have line numbers. In the buffer, `ivy--regex' should be used."
  (cond
    ((equal str "")
     "")
    ((equal str "^")
     ".")
    ((string-match "^\\^" str)
     (setq ivy--old-re "")
     (let ((re (ivy--regex-plus (substring str 1))))
       (format "^[0-9][0-9 ]\\{%d\\}%s"
               swiper--width
               (if (zerop ivy--subexps)
                   (prog1 (format "\\(%s\\)" re)
                     (setq ivy--subexps 1))
                 re))))
    (t
     (ivy--regex-plus str))))

(defvar swiper-history nil
  "History for `swiper'.")

(defun swiper--ivy (&optional initial-input)
  "`isearch' with an overview using `ivy'.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (unless (eq (length (help-function-arglist 'ivy-read)) 4)
    (warn "You seem to be using the outdated stand-alone \"ivy\" package.
Please remove it and update the \"swiper\" package."))
  (swiper--init)
  (let ((candidates (swiper--candidates))
        (preselect (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
    (unwind-protect
         (ivy-read
          "Swiper: "
          candidates
          :initial-input initial-input
          :keymap swiper-map
          :preselect preselect
          :require-match t
          :update-fn #'swiper--update-input-ivy
          :unwind #'swiper--cleanup
          :re-builder #'swiper--re-builder
          :history 'swiper-history)
      (if (null ivy-exit)
          (goto-char swiper--opoint)
        (swiper--action ivy--current ivy-text)))))

(defun swiper--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defvar swiper--overlays nil
  "Store overlays.")

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (delete-overlay (pop swiper--overlays)))
  (save-excursion
    (goto-char (point-min))
    (isearch-clean-overlays)))

(defun swiper--update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (swiper--cleanup)
    (when (> (length ivy--current) 0)
      (let* ((re (funcall ivy--regex-function ivy-text))
             (re (if (stringp re) re (caar re)))
             (str (get-text-property 0 'display ivy--current))
             (num (if (string-match "^[0-9]+" str)
                      (string-to-number (match-string 0 str))
                    0)))
        (goto-char (point-min))
        (when (cl-plusp num)
          (goto-char (point-min))
          (forward-line (1- num))
          (if (and (equal ivy-text "")
                   (>= swiper--opoint (line-beginning-position))
                   (<= swiper--opoint (line-end-position)))
              (goto-char swiper--opoint)
            (re-search-forward re (line-end-position) t))
          (isearch-range-invisible (line-beginning-position)
                                   (line-end-position))
          (unless (and (>= (point) (window-start))
                       (<= (point) (window-end (ivy-state-window ivy-last) t)))
            (recenter)))
        (swiper--add-overlays re)))))

(defun swiper--add-overlays (re &optional beg end)
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds."
  (let ((ov (make-overlay
             (line-beginning-position)
             (1+ (line-end-position)))))
    (overlay-put ov 'face 'swiper-line-face)
    (overlay-put ov 'window (ivy-state-window ivy-last))
    (push ov swiper--overlays)
    (let* ((wh (window-height))
           (beg (or beg (save-excursion
                          (forward-line (- wh))
                          (point))))
           (end (or end (save-excursion
                          (forward-line wh)
                          (point)))))
      (when (>= (length re) swiper-min-highlight)
        (save-excursion
          (goto-char beg)
          ;; RE can become an invalid regexp
          (while (and (ignore-errors (re-search-forward re end t))
                      (> (- (match-end 0) (match-beginning 0)) 0))
            (let ((i 0))
              (while (<= i ivy--subexps)
                (when (match-beginning i)
                  (let ((overlay (make-overlay (match-beginning i)
                                               (match-end i)))
                        (face
                         (cond ((zerop ivy--subexps)
                                (cadr swiper-faces))
                               ((zerop i)
                                (car swiper-faces))
                               (t
                                (nth (1+ (mod (+ i 2) (1- (length swiper-faces))))
                                     swiper-faces)))))
                    (push overlay swiper--overlays)
                    (overlay-put overlay 'face face)
                    (overlay-put overlay 'window (ivy-state-window ivy-last))
                    (overlay-put overlay 'priority i)))
                (cl-incf i)))))))))

(defun swiper--action (x input)
  "Goto line X and search for INPUT."
  (if (null x)
      (user-error "No candidates")
    (goto-char (point-min))
    (forward-line (1- (read (get-text-property 0 'display x))))
    (re-search-forward
     (ivy--regex input) (line-end-position) t)
    (swiper--ensure-visible)
    (when (/= (point) swiper--opoint)
      (unless (and transient-mark-mode mark-active)
        (push-mark swiper--opoint t)
        (message "Mark saved where search started")))))

;; (define-key isearch-mode-map (kbd "C-o") 'swiper-from-isearch)
(defun swiper-from-isearch ()
  "Invoke `swiper' from isearch."
  (interactive)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (isearch-exit)
    (swiper query)))

(defvar swiper-multi-buffers nil
  "Store the current list of buffers.")

(defvar swiper-multi-candidates nil
  "Store the list of candidates for `swiper-multi'.")

(defun swiper-multi-prompt ()
  (format "Buffers (%s): "
          (mapconcat #'identity swiper-multi-buffers ", ")))

(defun swiper-multi ()
  "Select one or more buffers.
Run `swiper' for those buffers."
  (interactive)
  (setq swiper-multi-buffers nil)
  (setq swiper-multi-candidates nil)
  (ivy-read (swiper-multi-prompt)
            'internal-complete-buffer
            :action 'swiper-multi-action-1)
  (ivy-read "Swiper: " swiper-multi-candidates
            :action 'swiper-multi-action-2
            :unwind #'swiper--cleanup))

(defun swiper-multi-action-1 (x)
  (if (member x swiper-multi-buffers)
      (progn
        (setq swiper-multi-buffers (delete x swiper-multi-buffers)))
    (unless (equal x "")
      (setq swiper-multi-buffers (append swiper-multi-buffers (list x)))))
  (let ((prompt (swiper-multi-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "%-4d " prompt)))
  (cond ((memq this-command '(ivy-done
                              ivy-alt-done
                              ivy-immediate-done))
         (let ((ww (window-width)))
           (dolist (buf swiper-multi-buffers)
             (with-current-buffer buf
               (setq swiper-multi-candidates
                     (append
                      (mapcar
                       (lambda (s)
                         (setq s (concat s " "))
                         (let ((len (length s)))
                           (put-text-property
                            (1- len) len 'display
                            (concat
                             (make-string
                              (max
                               (- ww
                                  (string-width s)
                                  (length (buffer-name))
                                  1)
                               0)
                              ?\ )
                             (buffer-name))
                            s)
                           s))
                       (swiper--candidates))
                      swiper-multi-candidates))))))
        ((eq this-command 'ivy-call)
         (delete-minibuffer-contents))))

(defun swiper-multi-action-2 (x)
  (let ((buf-space (get-text-property (1- (length x)) 'display x)))
    (with-ivy-window
      (when (string-match "\\` *\\([^ ]+\\)\\'" buf-space)
        (switch-to-buffer (match-string 1 buf-space))
        (goto-char (point-min))
        (forward-line (1- (read x)))
        (re-search-forward
         (ivy--regex ivy-text)
         (line-end-position) t)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(provide 'swiper)

;;; swiper.el ends here
