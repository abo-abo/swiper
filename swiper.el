;;; swiper.el --- Isearch with an overview. Oh, man! -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.6.0
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

(define-obsolete-face-alias 'swiper-minibuffer-match-face-1
    'ivy-minibuffer-match-face-1 "0.6.0")

(define-obsolete-face-alias 'swiper-minibuffer-match-face-2
    'ivy-minibuffer-match-face-2 "0.6.0")

(define-obsolete-face-alias 'swiper-minibuffer-match-face-3
    'ivy-minibuffer-match-face-3 "0.6.0")

(define-obsolete-face-alias 'swiper-minibuffer-match-face-4
    'ivy-minibuffer-match-face-4 "0.6.0")

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
    (define-key map (kbd "C-7") 'swiper-mc)
    (define-key map (kbd "C-c C-f") 'swiper-toggle-face-matching)
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
      (swiper--cleanup)
      (ivy-exit-with-action
       (lambda (_)
         (with-ivy-window
           (move-beginning-of-line 1)
           (perform-replace from to
                            t t nil)))))))

(defvar avy-background)
(defvar avy-all-windows)
(defvar avy-style)
(defvar avy-keys)
(declare-function avy--regex-candidates "ext:avy")
(declare-function avy--process "ext:avy")
(declare-function avy--overlay-post "ext:avy")
(declare-function avy-action-goto "ext:avy")
(declare-function avy--done "ext:avy")
(declare-function avy--make-backgrounds "ext:avy")
(declare-function avy-window-list "ext:avy")
(declare-function avy-read "ext:avy")
(declare-function avy-read-de-bruijn "ext:avy")
(declare-function avy-tree "ext:avy")
(declare-function avy-push-mark "ext:avy")
(declare-function avy--remove-leading-chars "ext:avy")

;;;###autoload
(defun swiper-avy ()
  "Jump to one of the current swiper candidates."
  (interactive)
  (unless (string= ivy-text "")
    (let* ((avy-all-windows nil)
           (candidates (append
                        (with-ivy-window
                          (avy--regex-candidates
                           (ivy--regex ivy-text)))
                        (save-excursion
                          (save-restriction
                            (narrow-to-region (window-start) (window-end))
                            (goto-char (point-min))
                            (forward-line)
                            (let ((cands))
                              (while (< (point) (point-max))
                                (push (cons (1+ (point))
                                            (selected-window))
                                      cands)
                                (forward-line))
                              cands)))))
           (candidate (unwind-protect
                          (prog2
                              (avy--make-backgrounds
                               (append (avy-window-list)
                                       (list  (ivy-state-window ivy-last))))
                              (if (eq avy-style 'de-bruijn)
                                  (avy-read-de-bruijn
                                   candidates avy-keys)
                                (avy-read (avy-tree candidates avy-keys)
                                          #'avy--overlay-post
                                          #'avy--remove-leading-chars))
                            (avy-push-mark))
                        (avy--done))))
      (if (window-minibuffer-p (cdr candidate))
          (progn
            (ivy-set-index (- (line-number-at-pos (car candidate)) 2))
            (ivy--exhibit)
            (ivy-done)
            (ivy-call))
        (ivy-quit-and-run
         (avy-action-goto (caar candidate)))))))

(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors-core")
(declare-function multiple-cursors-mode "ext:multiple-cursors-core")

;;;###autoload
(defun swiper-mc ()
  (interactive)
  (unless (require 'multiple-cursors nil t)
    (error "multiple-cursors isn't installed"))
  (let ((cands (nreverse ivy--old-cands)))
    (unless (string= ivy-text "")
      (ivy-exit-with-action
       (lambda (_)
         (let (cand)
           (while (setq cand (pop cands))
             (swiper--action cand)
             (when cands
               (mc/create-fake-cursor-at-point))))
         (multiple-cursors-mode 1))))))

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
                                 emms-playlist-mode
                                 emms-stream-mode
                                 erc-mode
                                 org-agenda-mode
                                 dired-mode
                                 jabber-chat-mode
                                 elfeed-search-mode
                                 elfeed-show-mode
                                 fundamental-mode
                                 Man-mode
                                 woman-mode
                                 mu4e-view-mode
                                 mu4e-headers-mode
                                 help-mode
                                 debbugs-gnu-mode
                                 occur-mode
                                 occur-edit-mode
                                 bongo-mode
                                 eww-mode
                                 twittering-mode
                                 w3m-mode)))
    (unless (> (buffer-size) 100000)
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer))))))

(defvar swiper--format-spec ""
  "Store the current candidates format spec.")

(defvar swiper--width nil
  "Store the amount of digits needed for the longest line nubmer.")

(defvar swiper-use-visual-line nil
  "When non-nil, use `line-move' instead of `forward-line'.")

(declare-function outline-show-all "outline")

(defun swiper--candidates (&optional numbers-width)
  "Return a list of this buffer lines.

NUMBERS-WIDTH, when specified, is used for line numbers width
spec, instead of calculating it as the log of the buffer line
count."
  (if (and visual-line-mode
           ;; super-slow otherwise
           (< (buffer-size) 20000))
      (progn
        (when (eq major-mode 'org-mode)
          (require 'outline)
          (if (fboundp 'outline-show-all)
              (outline-show-all)
            (show-all)))
        (setq swiper-use-visual-line t))
    (setq swiper-use-visual-line nil))
  (let ((n-lines (count-lines (point-min) (point-max))))
    (unless (zerop n-lines)
      (setq swiper--width (or numbers-width
                              (1+ (floor (log n-lines 10)))))
      (setq swiper--format-spec
            (format "%%-%dd " swiper--width))
      (let ((line-number 0)
            (advancer (if swiper-use-visual-line
                          (lambda (arg) (line-move arg t))
                        #'forward-line))
            candidates)
        (save-excursion
          (goto-char (point-min))
          (swiper-font-lock-ensure)
          (while (< (point) (point-max))
            (let ((str (concat
                        " "
                        (replace-regexp-in-string
                         "\t" "    "
                         (if swiper-use-visual-line
                             (buffer-substring
                              (save-excursion
                                (beginning-of-visual-line)
                                (point))
                              (save-excursion
                                (end-of-visual-line)
                                (point)))
                           (buffer-substring
                            (point)
                            (line-end-position)))))))
              (when (eq major-mode 'twittering-mode)
                (remove-text-properties 0 (length str) '(field) str))
              (put-text-property 0 1 'display
                                 (format swiper--format-spec
                                         (cl-incf line-number))
                                 str)
              (push str candidates))
            (funcall advancer 1))
          (nreverse candidates))))))

(defvar swiper--opoint 1
  "The point when `swiper' starts.")

;;;###autoload
(defun swiper (&optional initial-input)
  "`isearch' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--ivy initial-input))

(declare-function evil-jumper--set-jump "ext:evil-jumper")

(defun swiper--init ()
  "Perform initialization common to both completion methods."
  (setq swiper--opoint (point))
  (when (bound-and-true-p evil-jumper-mode)
    (evil-jumper--set-jump)))

(defun swiper--re-builder (str)
  "Transform STR into a swiper regex.
This is the regex used in the minibuffer, since the candidates
there have line numbers. In the buffer, `ivy--regex' should be used."
  (cond
    ((equal str "")
     "")
    ((equal str "^")
     (setq ivy--subexps 0)
     ".")
    ((string-match "^\\^" str)
     (setq ivy--old-re "")
     (let ((re (ivy--regex-plus (substring str 1))))
       (if (zerop ivy--subexps)
           (prog1 (format "^ ?\\(%s\\)" re)
             (setq ivy--subexps 1))
         (format "^ %s" re))))
    (t
     (ivy--regex-plus str))))

(defvar swiper-history nil
  "History for `swiper'.")

(defvar swiper-invocation-face nil
  "The face at the point of invocation of `swiper'.")

(defun swiper--ivy (&optional initial-input)
  "`isearch' with an overview using `ivy'.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (swiper--init)
  (setq swiper-invocation-face
        (plist-get (text-properties-at (point)) 'face))
  (let ((candidates (swiper--candidates))
        (preselect
         (if swiper-use-visual-line
             (count-screen-lines
              (point-min)
              (save-excursion (beginning-of-visual-line) (point)))
           (1- (line-number-at-pos))))
        (minibuffer-allow-text-properties t)
        res)
    (unwind-protect
         (setq res
               (ivy-read
                "Swiper: "
                candidates
                :initial-input initial-input
                :keymap swiper-map
                :preselect preselect
                :require-match t
                :update-fn #'swiper--update-input-ivy
                :unwind #'swiper--cleanup
                :action #'swiper--action
                :re-builder #'swiper--re-builder
                :history 'swiper-history
                :caller 'swiper))
      (unless res
        (goto-char swiper--opoint)))))

(defun swiper-toggle-face-matching ()
  "Toggle matching only the candidates with `swiper-invocation-face'."
  (interactive)
  (setf (ivy-state-matcher ivy-last)
        (if (ivy-state-matcher ivy-last)
            nil
          #'swiper--face-matcher))
  (setq ivy--old-re nil))

(defun swiper--face-matcher (regexp candidates)
  "Return REGEXP-matching CANDIDATES.
Matched candidates should have `swiper-invocation-face'."
  (cl-remove-if-not
   (lambda (x)
     (and
      (string-match regexp x)
      (let ((s (match-string 0 x))
            (i 0))
        (while (and (< i (length s))
                    (text-property-any
                     i (1+ i)
                     'face swiper-invocation-face
                     s))
          (cl-incf i))
        (eq i (length s)))))
   candidates))

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
          (if swiper-use-visual-line
              (line-move (1- num))
            (forward-line (1- num)))
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

(defun swiper--add-overlays (re &optional beg end wnd)
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds.
WND, when specified is the window."
  (setq wnd (or wnd (ivy-state-window ivy-last)))
  (let ((ov (if visual-line-mode
                (make-overlay
                 (save-excursion
                   (beginning-of-visual-line)
                   (point))
                 (save-excursion
                   (end-of-visual-line)
                   (point)))
              (make-overlay
               (line-beginning-position)
               (1+ (line-end-position))))))
    (overlay-put ov 'face 'swiper-line-face)
    (overlay-put ov 'window wnd)
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
                    (overlay-put overlay 'window wnd)
                    (overlay-put overlay 'priority i)))
                (cl-incf i)))))))))

(defun swiper--action (x)
  "Goto line X."
  (if (null x)
      (user-error "No candidates")
    (with-ivy-window
      (unless (equal (current-buffer)
                     (ivy-state-buffer ivy-last))
        (switch-to-buffer (ivy-state-buffer ivy-last)))
      (goto-char (point-min))
      (funcall (if swiper-use-visual-line
                   #'line-move
                 #'forward-line)
               (1- (read (get-text-property 0 'display x))))
      (re-search-forward
       (ivy--regex ivy-text) (line-end-position) t)
      (swiper--ensure-visible)
      (when (/= (point) swiper--opoint)
        (unless (and transient-mark-mode mark-active)
          (when (eq ivy-exit 'done)
            (push-mark swiper--opoint t)
            (message "Mark saved where search started")))))))

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
  (ivy-read (swiper-multi-prompt)
            'internal-complete-buffer
            :action 'swiper-multi-action-1)
  (ivy-read "Swiper: " swiper-multi-candidates
            :action 'swiper-multi-action-2
            :unwind #'swiper--cleanup
            :caller 'swiper-multi))

(defun swiper-all ()
  "Run `swiper' for all opened buffers."
  (interactive)
  (ivy-read "Swiper: " (swiper--multi-candidates
                        (cl-remove-if-not
                         #'buffer-file-name
                         (buffer-list)))
            :action 'swiper-multi-action-2
            :unwind #'swiper--cleanup
            :caller 'swiper-multi))

(defun swiper--multi-candidates (buffers)
  (let* ((ww (window-width))
         (res nil)
         (column-2 (apply #'max
                          (mapcar
                           (lambda (b)
                             (length (buffer-name b)))
                           buffers)))
         (column-1 (- ww 4 column-2 1)))
    (dolist (buf buffers)
      (with-current-buffer buf
        (setq res
              (append
               (mapcar
                (lambda (s)
                  (setq s (concat (ivy--truncate-string s column-1) " "))
                  (let ((len (length s)))
                    (put-text-property
                     (1- len) len 'display
                     (concat
                      (make-string
                       (- ww (string-width s) (length (buffer-name)) 3)
                       ?\ )
                      (buffer-name))
                     s)
                    s))
                (swiper--candidates 4))
               res))
        nil))
    res))

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
         (setq swiper-multi-candidates
               (swiper--multi-candidates
                (mapcar #'get-buffer swiper-multi-buffers))))
        ((eq this-command 'ivy-call)
         (delete-minibuffer-contents))))

(defun swiper-multi-action-2 (x)
  (let ((buf-space (get-text-property (1- (length x)) 'display x)))
    (with-ivy-window
      (when (string-match "\\` *\\([^ ]+\\)\\'" buf-space)
        (switch-to-buffer (match-string 1 buf-space))
        (goto-char (point-min))
        (forward-line (1- (read (get-text-property 0 'display x))))
        (re-search-forward
         (ivy--regex ivy-text)
         (line-end-position) t)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(provide 'swiper)

;;; swiper.el ends here
