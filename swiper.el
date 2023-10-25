;;; swiper.el --- Isearch with an overview.  Oh, man! -*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Maintainer: Basil L. Contovounesios <contovob@tcd.ie>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.14.2
;; Package-Requires: ((emacs "24.5") (ivy "0.14.2"))
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
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package gives an overview of the current regex search
;; candidates.  The search regex can be split into groups with a
;; space.  Each group is highlighted with a different face.
;;
;; It can double as a quick `regex-builder', although only single
;; lines will be matched.

;;; Code:

(require 'cl-lib)
(require 'ivy)

(defgroup swiper nil
  "`isearch' with an overview."
  :group 'matching
  :prefix "swiper-")

(defface swiper-match-face-1
  '((t :inherit lazy-highlight))
  "The background face for `swiper' matches."
  :group 'ivy-faces)

(defface swiper-match-face-2
  '((t :inherit isearch))
  "Face for `swiper' matches modulo 1."
  :group 'ivy-faces)

(defface swiper-match-face-3
  '((t :inherit match))
  "Face for `swiper' matches modulo 2."
  :group 'ivy-faces)

(defface swiper-match-face-4
  '((t :inherit isearch-fail))
  "Face for `swiper' matches modulo 3."
  :group 'ivy-faces)

(defface swiper-background-match-face-1
  '((t :inherit swiper-match-face-1))
  "The background face for non-current `swiper' matches."
  :group 'ivy-faces)

(defface swiper-background-match-face-2
  '((t :inherit swiper-match-face-2))
  "Face for non-current `swiper' matches modulo 1."
  :group 'ivy-faces)

(defface swiper-background-match-face-3
  '((t :inherit swiper-match-face-3))
  "Face for non-current `swiper' matches modulo 2."
  :group 'ivy-faces)

(defface swiper-background-match-face-4
  '((t :inherit swiper-match-face-4))
  "Face for non-current `swiper' matches modulo 3."
  :group 'ivy-faces)

(defface swiper-line-face
  '((t :inherit highlight))
  "Face for current `swiper' line."
  :group 'ivy-faces)

(defcustom swiper-faces '(swiper-match-face-1
                          swiper-match-face-2
                          swiper-match-face-3
                          swiper-match-face-4)
  "List of `swiper' faces for group matches."
  :group 'ivy-faces
  :type '(repeat face))

(defvar swiper-background-faces
  '(swiper-background-match-face-1
    swiper-background-match-face-2
    swiper-background-match-face-3
    swiper-background-match-face-4)
  "Like `swiper-faces', but used for all matches except the current one.")

(defun swiper--recompute-background-faces ()
  (let ((faces '(swiper-background-match-face-1
                 swiper-background-match-face-2
                 swiper-background-match-face-3
                 swiper-background-match-face-4))
        (colir-compose-method #'colir-compose-soft-light))
    (cl-mapc (lambda (f1 f2)
               (let* ((bg (face-background f1))
                      ;; FIXME: (colir-color-parse "color-22") is nil.
                      (bg (and bg (colir-color-parse bg))))
                 (when bg
                   (setq bg (colir-blend bg (colir-color-parse "#ffffff")))
                   (set-face-background f2 bg))))
             swiper-faces
             faces)))
(swiper--recompute-background-faces)

(defcustom swiper-min-highlight 2
  "Only highlight matches for regexps at least this long."
  :type 'integer)

(defcustom swiper-include-line-number-in-search nil
  "Include line number in text of search candidates."
  :type 'boolean
  :group 'swiper)

(defcustom swiper-goto-start-of-match nil
  "When non-nil, go to the start of the match, not its end.
Treated as non-nil when searching backwards."
  :type 'boolean
  :group 'swiper)

(defun swiper-C-s (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-previous-history-element 1)
    (ivy-next-line arg)))

(defvar swiper-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'swiper-C-s)
    (define-key map (kbd "M-q") 'swiper-query-replace)
    (define-key map (kbd "C-l") 'swiper-recenter-top-bottom)
    (define-key map (kbd "C-'") 'swiper-avy)
    (define-key map (kbd "C-7") 'swiper-mc)
    (define-key map (kbd "C-c C-f") 'swiper-toggle-face-matching)
    map)
  "Keymap for swiper.")

(defvar swiper--query-replace-overlays nil)

(defun swiper--query-replace-updatefn ()
  (let ((lisp (ignore-errors (nth 2 (query-replace-compile-replacement ivy-text t)))))
    (dolist (ov swiper--query-replace-overlays)
      (overlay-put
       ov 'after-string
       (propertize
        (condition-case nil
            (with-current-buffer (overlay-buffer ov)
              (set-match-data (overlay-get ov 'md))
              (if (consp lisp)
                  (eval lisp)
                (match-substitute-replacement ivy-text)))
          (error ivy-text))
        'face 'error)))))

(defun swiper--query-replace-cleanup ()
  (while swiper--query-replace-overlays
    (delete-overlay (pop swiper--query-replace-overlays))))

(defun swiper--query-replace-setup ()
  (with-ivy-window
    (let ((end (window-end (selected-window) t))
          (re (ivy-re-to-str ivy-regex)))
      (save-excursion
        (beginning-of-line)
        (while (re-search-forward re end t)
          (let ((ov (make-overlay (1- (match-end 0)) (match-end 0)))
                (md (match-data t)))
            (overlay-put
             ov 'matches
             (mapcar
              (lambda (x)
                (list `(match-string ,x) (match-string x)))
              (number-sequence 0 (1- (/ (length md) 2)))))
            (overlay-put ov 'md md)
            (push ov swiper--query-replace-overlays))
          (unless (> (match-end 0) (match-beginning 0))
            (forward-char)))))))

(defun swiper-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (cond ((null (window-minibuffer-p))
         (user-error "Should only be called in the minibuffer through `swiper-map'"))
        ((string= "" ivy-text)
         (user-error "Empty input"))
        (t
         (swiper--query-replace-setup)
         (unwind-protect
              (let* ((enable-recursive-minibuffers t)
                     (from (ivy-re-to-str ivy-regex))
                     (groups (number-sequence 1 ivy--subexps))
                     (default
                      (list
                       (mapconcat (lambda (i) (format "\\%d" i)) groups " ")
                       (format "\\,(concat %s)"
                               (if (<= ivy--subexps 1)
                                   "\\&"
                                 (mapconcat
                                  (lambda (i) (format "\\%d" i))
                                  groups
                                  " \" \" ")))))
                     (to
                      (query-replace-compile-replacement
                       (ivy-read
                        (format "Query replace %s with: " from) nil
                        :def default
                        :caller 'swiper-query-replace)
                       t)))
                (swiper--cleanup)
                (ivy-exit-with-action
                 (lambda (_)
                   (with-ivy-window
                     (move-beginning-of-line 1)
                     (let ((inhibit-read-only t))
                       (perform-replace from to
                                        t t nil))))))
           (swiper--query-replace-cleanup)))))

(ivy-configure 'swiper-query-replace
  :update-fn #'swiper--query-replace-updatefn)
(put 'swiper-query-replace 'no-counsel-M-x t)

(defvar inhibit-message)

(defun swiper-all-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (if (null (window-minibuffer-p))
      (user-error
       "Should only be called in the minibuffer through `swiper-all-map'")
    (let* ((enable-recursive-minibuffers t)
           (from (ivy--regex ivy-text))
           (to (query-replace-read-to from "Query replace" t)))
      (swiper--cleanup)
      (ivy-exit-with-action
       (lambda (_)
         (let ((wnd-conf (current-window-configuration))
               (inhibit-message t))
           (unwind-protect
                (dolist (cand ivy--old-cands)
                  (let ((buffer (get-text-property 0 'buffer cand)))
                    (switch-to-buffer buffer)
                    (goto-char (point-min))
                    (perform-replace from to t t nil)))
             (set-window-configuration wnd-conf))))))))
(put 'swiper-all-query-replace 'no-counsel-M-x t)

(defvar avy-all-windows)
(defvar avy-style)
(defvar avy-keys)
(declare-function avy--overlay-post "ext:avy")
(declare-function avy-action-goto "ext:avy")
(declare-function avy-candidate-beg "ext:avy")
(declare-function avy--done "ext:avy")
(declare-function avy--make-backgrounds "ext:avy")
(declare-function avy-window-list "ext:avy")
(declare-function avy-read "ext:avy")
(declare-function avy-read-de-bruijn "ext:avy")
(declare-function avy-tree "ext:avy")
(declare-function avy-push-mark "ext:avy")
(declare-function avy--remove-leading-chars "ext:avy")

(defun swiper--avy-candidates ()
  (let* (
         ;; We'll have overlapping overlays, so we sort all the
         ;; overlays in the visible region by their start, and then
         ;; throw out non-Swiper overlays or overlapping Swiper
         ;; overlays.
         (visible-overlays (cl-sort (with-ivy-window
                                      (overlays-in (window-start)
                                                   (window-end)))
                                    #'< :key #'overlay-start))
         (min-overlay-start 0)
         (overlays-for-avy
          (cl-remove-if-not
           (lambda (ov)
             (when (and (>= (overlay-start ov)
                            min-overlay-start)
                        (memq (overlay-get ov 'face)
                              (append swiper-faces swiper-background-faces)))
               (setq min-overlay-start (overlay-start ov))))
           visible-overlays))
         (offset (if (eq (ivy-state-caller ivy-last) 'swiper) 1 0)))
    (nconc
     (mapcar (lambda (ov)
               (cons (overlay-start ov)
                     (overlay-get ov 'window)))
             overlays-for-avy)
     (save-excursion
       (save-restriction
         (narrow-to-region (window-start) (window-end))
         (goto-char (point-min))
         (forward-line)
         (let ((win (selected-window))
               cands)
           (while (not (eobp))
             (push (cons (+ (point) offset) win)
                   cands)
             (forward-line))
           cands))))))

(defun swiper--avy-candidate ()
  (let ((candidates (swiper--avy-candidates))
        (avy-all-windows nil))
    (unwind-protect
         (prog2
             (avy--make-backgrounds
              (append (avy-window-list)
                      (list (ivy-state-window ivy-last))))
             (if (eq avy-style 'de-bruijn)
                 (avy-read-de-bruijn candidates avy-keys)
               (avy-read (avy-tree candidates avy-keys)
                         #'avy--overlay-post
                         #'avy--remove-leading-chars))
           (avy-push-mark))
      (avy--done))))

(defun swiper--avy-index (pos)
  "Return `ivy--index' for `avy' candidate at minibuffer POS."
  ;; Position in original buffer.
  (let ((opos (get-text-property pos 'point)))
    (or
     ;; Find `swiper-isearch' index based on buffer position.
     (and opos (cl-position opos ivy--all-candidates))
     ;; Find `swiper' index based on line number.
     (let ((nlines (count-lines (point-min) (point-max))))
       (+ (car (ivy--minibuffer-index-bounds
                ivy--index ivy--length ivy-height))
          (line-number-at-pos pos)
          (if (or (= nlines (1+ ivy-height))
                  (< ivy--length ivy-height))
              0
            (- ivy-height nlines))
          -2)))))

(defun swiper--avy-goto (candidate)
  (cond ((let ((win (cdr-safe candidate)))
           (and win (window-minibuffer-p win)))
         (setq ivy--index (swiper--avy-index (car candidate)))
         (ivy--exhibit)
         (ivy-done)
         (ivy-call))
        ((or (consp candidate)
             (number-or-marker-p candidate))
         (ivy-quit-and-run
           (avy-action-goto (avy-candidate-beg candidate))))))

;;;###autoload
(defun swiper-avy ()
  "Jump to one of the current swiper candidates with `avy'."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (user-error "Package avy isn't installed"))
  (cl-case (length ivy-text)
    (0
     (user-error "Need at least one char of input"))
    (1
     ;; FIXME: `swiper--update-input-ivy' expects string candidates,
     ;; but `swiper-isearch' now uses buffer positions.
     (when (stringp (ivy-state-current ivy-last))
       (let ((swiper-min-highlight 1))
         (swiper--update-input-ivy)))))
  (swiper--avy-goto (swiper--avy-candidate)))

(declare-function mc/create-fake-cursor-at-point "ext:multiple-cursors-core")
(declare-function multiple-cursors-mode "ext:multiple-cursors-core")

(defun swiper-mc ()
  "Create a fake cursor for each `swiper' candidate.
Make sure `swiper-mc' is on `mc/cmds-to-run-once' list."
  (interactive)
  (unless (require 'multiple-cursors nil t)
    (error "Multiple-cursors isn't installed"))
  (unless (window-minibuffer-p)
    (error "Call me only from `swiper'"))
  (let ((cands (nreverse ivy--old-cands))
        (action (ivy--get-action ivy-last)))
    (unless (string= ivy-text "")
      (ivy-exit-with-action
       (lambda (_)
         (let (cand)
           (while (setq cand (pop cands))
             (funcall action cand)
             (when cands
               (mc/create-fake-cursor-at-point))))
         (multiple-cursors-mode 1))))))

(defvar swiper--current-window-start nil
  "Store `window-start' to restore it later.
This prevents a \"jumping\" behavior which occurs when variables
such as `scroll-conservatively' are set to a high value.")

(defun swiper-recenter-top-bottom (&optional arg)
  "Call (`recenter-top-bottom' ARG)."
  (interactive "P")
  (with-ivy-window
    (recenter-top-bottom arg)
    (setq swiper--current-window-start (window-start))))

(defvar swiper-font-lock-exclude
  '(Man-mode
    adoc-mode
    bbdb-mode
    bongo-library-mode
    bongo-mode
    bongo-playlist-mode
    bookmark-bmenu-mode
    circe-channel-mode
    circe-query-mode
    circe-server-mode
    deadgrep-mode
    debbugs-gnu-mode
    dired-mode
    elfeed-search-mode
    elfeed-show-mode
    emms-playlist-mode
    emms-stream-mode
    erc-mode
    eshell-mode
    eww-mode
    forth-block-mode
    forth-mode
    fundamental-mode
    gnus-article-mode
    gnus-group-mode
    gnus-summary-mode
    help-mode
    helpful-mode
    jabber-chat-mode
    magit-popup-mode
    matrix-client-mode
    matrix-client-room-list-mode
    mu4e-headers-mode
    mu4e-view-mode
    nix-mode
    notmuch-search-mode
    notmuch-tree-mode
    occur-edit-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    rcirc-mode
    sauron-mode
    sieve-mode
    treemacs-mode
    twittering-mode
    vc-dir-mode
    w3m-mode
    woman-mode
    xref--xref-buffer-mode)
  "List of major-modes that are incompatible with `font-lock-ensure'.")

(defun swiper-font-lock-ensure-p ()
  "Return non-nil if we should `font-lock-ensure'."
  (or (derived-mode-p 'magit-mode)
      (bound-and-true-p magit-blame-mode)
      (memq major-mode swiper-font-lock-exclude)
      (not (derived-mode-p 'prog-mode))))

(defun swiper-font-lock-ensure ()
  "Ensure the entire buffer is highlighted."
  (unless (swiper-font-lock-ensure-p)
    (unless (or (> (buffer-size) 100000) (null font-lock-mode))
      (if (fboundp 'font-lock-ensure)
          ;; Added in Emacs 25.1.
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer))))))

(defvar swiper--format-spec ""
  "Store the current candidates format spec.")

(defvar swiper--width nil
  "Store the number of digits needed for the longest line number.")

(defvar swiper-use-visual-line nil
  "When non-nil, use `line-move' instead of `forward-line'.")

(defvar dired-isearch-filenames)
(declare-function dired-move-to-filename "dired")

(defun swiper--line ()
  (let* ((beg (cond ((and (eq major-mode 'dired-mode)
                          (bound-and-true-p dired-isearch-filenames))
                     (dired-move-to-filename)
                     (point))
                    (swiper-use-visual-line
                     (save-excursion
                       (beginning-of-visual-line)
                       (point)))
                    (t
                     (point))))
         (end (if swiper-use-visual-line
                  (save-excursion
                    (end-of-visual-line)
                    (point))
                (line-end-position))))

    (concat
     " "
     (buffer-substring beg end))))

(defvar swiper-use-visual-line-p
  (lambda (n-lines)
    (and visual-line-mode
         ;; super-slow otherwise
         (< (buffer-size) 20000)
         (< n-lines 400)))
  "A predicate that decides whether `line-move' or `forward-line' is used.
Note that `line-move' can be very slow.")

(defun swiper--candidates (&optional numbers-width)
  "Return a list of this buffer lines.

NUMBERS-WIDTH, when specified, is used for width spec of line
numbers; replaces calculating the width from buffer line count."
  (let* ((inhibit-field-text-motion t)
         (n-lines (count-lines (point-min) (point-max))))
    (if (funcall swiper-use-visual-line-p n-lines)
        (progn
          (when (eq major-mode 'org-mode)
            (require 'outline)
            (if (fboundp 'outline-show-all)
                ;; Added in Emacs 25.1.
                (outline-show-all)
              (with-no-warnings
                (show-all))))
          (setq swiper-use-visual-line t))
      (setq swiper-use-visual-line nil))
    (unless (zerop n-lines)
      (setq swiper--width (or numbers-width
                              (1+ (floor (log n-lines 10)))))
      (setq swiper--format-spec
            (format "%%-%dd " swiper--width))
      (let ((line-number 1)
            (advancer (if swiper-use-visual-line
                          (lambda (arg) (line-move arg t))
                        #'forward-line))
            candidates)
        (save-excursion
          (goto-char (point-min))
          (swiper-font-lock-ensure)
          (while (< (point) (point-max))
            (when (swiper-match-usable-p)
              (let ((str (swiper--line)))
                (setq str (ivy-cleanup-string str))
                (let ((line-number-str
                       (format swiper--format-spec line-number)))
                  (if swiper-include-line-number-in-search
                      (setq str (concat line-number-str str))
                    (put-text-property
                     0 1 'display line-number-str str))
                  (put-text-property
                   0 1 'swiper-line-number line-number str))
                (push str candidates)))
            (funcall advancer 1)
            (cl-incf line-number))
          (nreverse candidates))))))

(defvar swiper--opoint nil
  "Value of point when `swiper' or `swiper-isearch' starts.")

;;;###autoload
(defun swiper-backward (&optional initial-input)
  "`isearch-backward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let ((ivy-index-functions-alist
         '((swiper . ivy-recompute-index-swiper-backward))))
    (swiper initial-input)))

;;;###autoload
(defun swiper-thing-at-point ()
  "`swiper' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (swiper (regexp-quote thing))))

;;;###autoload
(defun swiper-all-thing-at-point ()
  "`swiper-all' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (swiper-all (regexp-quote thing))))

(defun swiper--extract-matches (regex cands)
  "Extract captured REGEX groups from CANDS."
  (let (res)
    (dolist (cand cands)
      (setq cand (substring cand 1))
      (when (string-match regex cand)
        (push (mapconcat (lambda (n) (match-string-no-properties n cand))
                         (number-sequence
                          1
                          (/ (- (length (match-data)) 2) 2))
                         " ")
              res)))
    (nreverse res)))

(defun swiper--occur-cands (fname cands)
  (when cands
    (with-current-buffer (ivy-state-buffer ivy-last)
      (when (eq (ivy-state-caller ivy-last) 'swiper-isearch)
        (setq cands (mapcar #'swiper--line-at-point cands)))
      (let* ((pt-min (point-min))
             (line-delta
              (save-restriction
                (widen)
                (1- (line-number-at-pos pt-min))))
             (lines
              (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
                  (swiper--isearch-occur-cands cands)
                (mapcar (lambda (s)
                          (let ((n (swiper--line-number s)))
                            (setq s (substring s 1))
                            (add-text-properties 0 1 (list 'swiper-line-number n) s)
                            (cons n s)))
                        cands)))
             (offset (+ (length fname) 2)))
        (mapcar (lambda (x)
                  (let ((nn (number-to-string
                             (+ (car x) line-delta))))
                    (remove-text-properties 0 1 '(display) (cdr x))
                    (put-text-property 0 (length nn) 'face 'ivy-grep-line-number nn)
                    (put-text-property 0 1 'offset (+ offset (length nn)) fname)
                    (format "%s:%s:%s" fname nn (cdr x))))
                lines)))))

(defun swiper--isearch-occur-cands (cands)
  (let* ((last-pt (get-text-property 0 'point (car cands)))
         (line (1+ (line-number-at-pos last-pt)))
         res pt)
    (dolist (cand cands)
      (setq pt (get-text-property 0 'point cand))
      (cl-incf line (1- (count-lines last-pt pt)))
      (push (cons line cand) res)
      (setq last-pt pt))
    (nreverse res)))

(defun swiper--occur-insert-lines (cands)
  (let ((inhibit-read-only t))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines cands)
    (goto-char (point-min))
    (forward-line 4)))

(defun swiper--occur-buffer ()
  (let ((buffer (ivy-state-buffer ivy-last)))
    (unless (buffer-live-p buffer)
      (setq buffer
            (setf (ivy-state-buffer ivy-last)
                  (find-file-noselect
                   (plist-get (ivy-state-extra-props ivy-last) :fname))))
      (save-selected-window
        (pop-to-buffer buffer))
      (setf (ivy-state-window ivy-last) (selected-window)))
    buffer))

(defun swiper-occur (&optional cands)
  "Generate a custom occur buffer for `swiper'.
When capture groups are present in the input, print them instead of lines."
  (setq cands (or ivy-marked-candidates cands))
  (let* ((buffer (swiper--occur-buffer))
         (fname (propertize
                 (with-ivy-window
                   (if (buffer-file-name buffer)
                       (file-name-nondirectory
                        (buffer-file-name buffer))
                     (buffer-name buffer)))
                 'face
                 'ivy-grep-info))
         (re
          (progn
            (string-match "\"\\(.*\\)\"" (buffer-name))
            (ivy-set-text (match-string 1 (buffer-name)))
            (mapconcat #'identity (ivy--split ivy-text) ".*?")))
         (cands
          (swiper--occur-cands
           fname
           (or cands
               (save-window-excursion
                 (switch-to-buffer buffer)
                 (if (eq (ivy-state-caller ivy-last) 'swiper)
                     (let ((ivy--regex-function 'swiper--re-builder))
                       (setq ivy--old-re nil)
                       (ivy--filter re (swiper--candidates)))
                   (swiper-isearch-function ivy-text)))))))
    (if (string-match-p "\\\\(" re)
        (insert
         (mapconcat #'identity
                    (swiper--extract-matches
                     re (with-current-buffer buffer
                          (swiper--candidates)))
                    "\n"))
      (unless (eq major-mode 'ivy-occur-grep-mode)
        (ivy-occur-grep-mode)
        (font-lock-mode -1))
      (swiper--occur-insert-lines
       (mapcar (lambda (cand) (concat "./" cand)) cands)))))

(declare-function evil-set-jump "ext:evil-jumps")

(defvar swiper--current-line nil)
(defvar swiper--current-match-start nil)
(defvar swiper--point-min nil)
(defvar swiper--point-max nil)
(defvar swiper--reveal-mode nil)

(defun swiper--init ()
  "Perform initialization common to both completion methods."
  (setq swiper--current-line nil)
  (setq swiper--current-match-start nil)
  (setq swiper--current-window-start nil)
  (setq swiper--opoint (point))
  (setq swiper--point-min (point-min))
  (setq swiper--point-max (point-max))
  (when (setq swiper--reveal-mode
              (bound-and-true-p reveal-mode))
    (reveal-mode -1))
  (lazy-highlight-cleanup t)
  (setq isearch-opened-overlays nil)
  (when (bound-and-true-p evil-mode)
    (evil-set-jump)))

(defun swiper--normalize-regex (re)
  "Normalize the Swiper regexp RE.
Add a space after a leading `^' for `swiper', and apply
`search-default-mode' if bound in the original buffer."
  (replace-regexp-in-string
   "^\\(?:\\\\(\\)?\\^"
   (concat "\\&" (if (eq 'swiper (ivy-state-caller ivy-last)) " " ""))
   (let ((mode (with-ivy-window (bound-and-true-p search-default-mode))))
     (if (functionp mode)
         (mapconcat
          (lambda (x)
            (if (string-match-p "\\`[^$\\^]+\\'" x)
                (funcall mode x)
              x))
          (split-string re "\\b") "")
       re))
   t))

(defun swiper--re-builder (str)
  "Transform STR into a swiper regex.
This is the regex used in the minibuffer where candidates have
line numbers.  For the buffer, use `ivy--regex' instead."
  (let* ((re-builder (ivy-alist-setting ivy-re-builders-alist))
         (str (replace-regexp-in-string "\\\\n" "\n" str))
         (re (funcall re-builder str)))
    (if (consp re)
        (mapcar
         (lambda (x)
           (cons (swiper--normalize-regex (car x))
                 (cdr x)))
         re)
      (swiper--normalize-regex re))))

(defvar swiper-history nil
  "History for `swiper'.")

(defvar swiper-invocation-face nil
  "The face at the point of invocation of `swiper'.")

(defcustom swiper-stay-on-quit nil
  "When non-nil don't go back to search start on abort."
  :type 'boolean)

;;;###autoload
(defun swiper (&optional initial-input)
  "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let ((candidates (swiper--candidates)))
    (swiper--init)
    (setq swiper-invocation-face
          (plist-get (text-properties-at (point)) 'face))
    (let ((preselect
           (if (or swiper-use-visual-line (null search-invisible))
               (count-screen-lines
                (point-min)
                (save-excursion (beginning-of-visual-line) (point)))
             (1- (line-number-at-pos))))
          (minibuffer-allow-text-properties t)
          res)
      (unwind-protect
           (and
            (setq res
                  (ivy-read
                   "Swiper: "
                   candidates
                   :initial-input initial-input
                   :keymap swiper-map
                   :preselect
                   (if initial-input
                       (cl-position-if
                        (lambda (x)
                          (<= (1+ preselect) (swiper--line-number x)))
                        (progn
                          (setq ivy--old-re nil)
                          (ivy--filter initial-input candidates)))
                     preselect)
                   :require-match t
                   :action #'swiper--action
                   :re-builder #'swiper--re-builder
                   :history 'swiper-history
                   :extra-props (list :fname (buffer-file-name))
                   :caller 'swiper))
            (point))
        (unless (or res swiper-stay-on-quit)
          (goto-char swiper--opoint))
        (isearch-clean-overlays)
        (unless (or res (string= ivy-text ""))
          (cl-pushnew ivy-text swiper-history))
        (setq swiper--current-window-start nil)
        (when swiper--reveal-mode
          (reveal-mode 1))))))

(ivy-configure 'swiper
  :occur #'swiper-occur
  :update-fn #'swiper--update-input-ivy
  :unwind-fn #'swiper--cleanup
  :index-fn #'ivy-recompute-index-swiper)

(defun swiper-toggle-face-matching ()
  "Toggle matching only the candidates with `swiper-invocation-face'."
  (interactive)
  (setf (ivy-state-matcher ivy-last)
        (if (ivy-state-matcher ivy-last)
            nil
          #'swiper--face-matcher))
  (setq ivy--old-re nil))

(defun swiper--face-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Matched candidates should have `swiper-invocation-face'."
  (cl-remove-if-not
   (lambda (x)
     (and (string-match regexp x)
          (let* ((s (match-string 0 x))
                 (n (length s))
                 (i 0))
            (while (and (< i n)
                        (text-property-any
                         i (1+ i)
                         'face swiper-invocation-face
                         s))
              (cl-incf i))
            (= i n))))
   candidates))

(defun swiper--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (1- (point))))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defvar swiper--overlays nil
  "Store overlays.")

(defvar swiper--isearch-highlight-timer nil
  "This timer used by `swiper--delayed-add-overlays'.")

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (delete-overlay (pop swiper--overlays)))
  ;; force cleanup unless it's :unwind
  (lazy-highlight-cleanup
   (if (eq ivy-exit 'done) lazy-highlight-cleanup t))
  (when (timerp swiper--isearch-highlight-timer)
    (cancel-timer swiper--isearch-highlight-timer)
    (setq swiper--isearch-highlight-timer nil)))

(defun swiper--add-cursor-overlay (wnd)
  (let* ((special (or (eolp) (looking-at "\t")))
         (ov (make-overlay (point) (if special (point) (1+ (point))))))
    (if special
        (overlay-put ov 'after-string (propertize " " 'face 'ivy-cursor))
      (overlay-put ov 'face 'ivy-cursor))
    (overlay-put ov 'window wnd)
    (overlay-put ov 'priority 2)
    (push ov swiper--overlays)))

(defun swiper--add-line-overlay (wnd)
  (let ((beg (if visual-line-mode
                 (save-excursion
                   (beginning-of-visual-line)
                   (point))
               (line-beginning-position)))
        (end (if visual-line-mode
                 (save-excursion
                   (end-of-visual-line)
                   (point))
               (1+ (line-end-position)))))
    (push (swiper--make-overlay beg end 'swiper-line-face wnd 0)
          swiper--overlays)))

(defun swiper--make-overlay (beg end face wnd priority)
  "Create an overlay bound by BEG and END.
FACE, WND and PRIORITY are properties corresponding to
the face, window and priority of the overlay."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'window wnd)
    (overlay-put overlay 'priority priority)
    overlay))

(defun swiper--recenter-p ()
  (or (display-graphic-p)
      (not recenter-redisplay)))

(defun swiper--positive-regexps ()
  (if (listp ivy-regex)
      (mapcar #'car (cl-remove-if-not #'cdr ivy-regex))
    (list ivy-regex)))

(defun swiper--update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (swiper--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let ((regexps (swiper--positive-regexps))
            (re-idx -1)
            (case-fold-search (ivy--case-fold-p ivy-text)))
        (dolist (re regexps)
          (setq re-idx (1+ re-idx))
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re))
                 (num (swiper--line-number (ivy-state-current ivy-last))))
            (unless (memq this-command '(ivy-yank-word
                                         ivy-yank-symbol
                                         ivy-yank-char
                                         scroll-other-window))
              (when (cl-plusp num)
                (unless (if swiper--current-line
                            (eq swiper--current-line num)
                          (eq (line-number-at-pos) num))
                  (goto-char swiper--point-min)
                  (if swiper-use-visual-line
                      (line-move (1- num))
                    (forward-line (1- num))))
                (if (and (equal ivy-text "")
                         (<= (line-beginning-position)
                             swiper--opoint
                             (line-end-position)))
                    (goto-char swiper--opoint)
                  (if (eq swiper--current-line num)
                      (when swiper--current-match-start
                        (goto-char swiper--current-match-start))
                    (setq swiper--current-line num))
                  (when (re-search-forward re (line-end-position) t)
                    (setq swiper--current-match-start (match-beginning 0))))
                (funcall isearch-filter-predicate
                         (line-beginning-position)
                         (line-end-position))
                (swiper--maybe-recenter)))
            (swiper--add-overlays
             re
             (max
              (if (swiper--recenter-p)
                  (window-start)
                (line-beginning-position (- (window-height))))
              swiper--point-min)
             (min
              (if (swiper--recenter-p)
                  (window-end (selected-window) t)
                (line-end-position (window-height)))
              swiper--point-max)
             nil
             re-idx)))))))

(defun swiper--add-overlays (re &optional beg end wnd re-idx)
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds.
WND, when specified is the window."
  (setq wnd (or wnd (ivy-state-window ivy-last)))
  (swiper--add-line-overlay wnd)
  (let* ((pt (point))
         (wh (window-height))
         (beg (or beg (save-excursion
                        (forward-line (- wh))
                        (point))))
         (end (or end (save-excursion
                        (forward-line wh)
                        (point))))
         (case-fold-search (ivy--case-fold-p re)))
    (when (>= (length re) swiper-min-highlight)
      (save-excursion
        (goto-char beg)
        ;; RE can become an invalid regexp
        (while (progn
                 (when (eolp)
                   (unless (eobp)
                     (forward-char)))
                 (and (ignore-errors (re-search-forward re end t))
                      (> (- (match-end 0) (match-beginning 0)) 0)))
          ;; Don't highlight a match if it spans multiple
          ;; lines. `count-lines' returns 1 if the match is within a
          ;; single line, even if it includes the newline, and 2 or
          ;; greater otherwise. We hope that the inclusion of the
          ;; newline will not ever be a problem in practice.
          (when (< (count-lines (match-beginning 0) (match-end 0)) 2)
            (let* ((faces (if (= (match-end 0) pt)
                              swiper-faces
                            swiper-background-faces))
                   (adder-fn (lambda (beg end face priority)
                               (push (swiper--make-overlay beg end face wnd priority)
                                     isearch-lazy-highlight-overlays))))
              (unless (and (consp ivy--old-re)
                           (null
                            (save-match-data
                              (ivy--re-filter ivy--old-re
                                              (list
                                               (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position)))))))
                (swiper--add-properties faces adder-fn re-idx)))))))))

(defun swiper--add-properties (faces adder-fn &optional re-idx)
  (let ((mb (match-beginning 0))
        (me (match-end 0)))
    (unless (> (- me mb) 2017)
      (funcall adder-fn
               mb me
               (if (and ivy-use-group-face-if-no-groups (zerop ivy--subexps))
                   (nth (1+ (mod (or re-idx 0) (1- (length faces)))) faces)
                 (car faces))
               0)))
  (let ((i 1)
        (j 0))
    (while (<= (cl-incf j) ivy--subexps)
      (let ((bm (match-beginning j))
            (em (match-end j)))
        (when (and (integerp em)
                   (integerp bm))
          (when (eq (ivy-alist-setting ivy-re-builders-alist t) #'ivy--regex-fuzzy)
            (while (and (< j ivy--subexps)
                        (integerp (match-beginning (+ j 1)))
                        (= em (match-beginning (+ j 1))))
              (setq em (match-end (cl-incf j)))))
          (funcall adder-fn
                   bm em
                   (nth (1+ (mod (+ i 2) (1- (length faces))))
                        faces)
                   i)
          (cl-incf i))))))

(defcustom swiper-action-recenter nil
  "When non-nil, recenter after exiting `swiper'."
  :type 'boolean)
(defvar evil-search-module)
(defvar evil-ex-search-pattern)
(defvar evil-ex-search-persistent-highlight)
(defvar evil-ex-search-direction)
(declare-function evil-ex-search-activate-highlight "evil-ex")

(defun swiper--maybe-recenter ()
  (cond (swiper-action-recenter
         (recenter))
        ((swiper--recenter-p)
         (when swiper--current-window-start
           (set-window-start (selected-window) swiper--current-window-start))
         (when (or
                (< (point) (window-start))
                (> (point) (window-end (ivy-state-window ivy-last) t)))
           (recenter))))
  (setq swiper--current-window-start (window-start)))

(defun swiper--line-number (x)
  (or (get-text-property 0 'swiper-line-number x)
      (get-text-property 1 'swiper-line-number x)))

(defcustom swiper-verbose t
  "When non-nil, print more informational messages."
  :type 'boolean)

(defun swiper--push-mark ()
  (when (/= (point) swiper--opoint)
    (unless (and transient-mark-mode mark-active)
      (when (eq ivy-exit 'done)
        (push-mark swiper--opoint t)
        (when swiper-verbose
          (message "Mark saved where search started"))))))

(defun swiper--action (x)
  "Goto line X."
  (let ((ln (1- (swiper--line-number x)))
        (re (ivy--regex ivy-text))
        (case-fold-search (ivy--case-fold-p ivy-text)))
    (if (null x)
        (user-error "No candidates")
      (with-ivy-window
        (unless (equal (current-buffer)
                       (ivy-state-buffer ivy-last))
          (switch-to-buffer (ivy-state-buffer ivy-last)))
        (goto-char
         (if (buffer-narrowed-p)
             swiper--point-min
           (point-min)))
        (funcall (if swiper-use-visual-line
                     #'line-move
                   #'forward-line)
                 ln)
        (when (and (re-search-forward re (line-end-position) t)
                   swiper-goto-start-of-match)
          (goto-char (match-beginning 0)))
        (swiper--ensure-visible)
        (swiper--maybe-recenter)
        (swiper--push-mark)
        (swiper--remember-search-history re)))))

(defun swiper--remember-search-history (re)
  "Add the search pattern RE to the search history ring."
  (add-to-history
   'regexp-search-ring
   re
   regexp-search-ring-max)
  ;; integration with evil-mode's search
  (when (bound-and-true-p evil-mode)
    (when (eq evil-search-module 'isearch)
      (setq isearch-string ivy-text))
    (when (eq evil-search-module 'evil-search)
      (add-to-history 'evil-ex-search-history re)
      (setq evil-ex-search-pattern (list re t t))
      (setq evil-ex-search-direction 'forward)
      (when evil-ex-search-persistent-highlight
        (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

(defun swiper-from-isearch ()
  "Invoke `swiper' from isearch."
  (interactive)
  (swiper (prog1 (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))
            (let ((search-nonincremental-instead nil))
              (isearch-exit)))))

(defvar swiper-multi-buffers nil
  "Store the current list of buffers.")

(defvar swiper-multi-candidates nil
  "Store the list of candidates for `swiper-multi'.")

(defun swiper-multi-prompt ()
  "Return prompt for `swiper-multi'."
  (format "Buffers (%s): "
          (mapconcat #'identity swiper-multi-buffers ", ")))

(defvar swiper-window-width 80)

(defun swiper-multi ()
  "Select one or more buffers.
Run `swiper' for those buffers."
  (interactive)
  (setq swiper-multi-buffers nil)
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (swiper-multi-prompt)
              #'internal-complete-buffer
              :action #'swiper-multi-action-1))
  (let ((swiper-window-width (- (- (frame-width) (if (display-graphic-p) 0 1)) 4)))
    (ivy-read "Swiper: " swiper-multi-candidates
              :action #'swiper-multi-action-2
              :caller 'swiper-multi)))

(ivy-configure 'swiper-multi
  :unwind-fn #'swiper--cleanup
  :index-fn #'ivy-recompute-index-swiper
  :format-fn #'swiper--all-format-function)

(defun swiper-multi-action-1 (x)
  "Add X to list of selected buffers `swiper-multi-buffers'.
If X is already part of the list, remove it instead.  Quit the selection if
X is selected by either `ivy-done', `ivy-alt-done' or `ivy-immediate-done',
otherwise continue prompting for buffers."
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
         (with-selected-window (active-minibuffer-window)
           (delete-minibuffer-contents)))))

(defun swiper-multi-action-2 (x)
  "Move to candidate X from `swiper-multi'."
  (when (> (length x) 0)
    (let ((buffer-name (get-text-property 0 'buffer x)))
      (when buffer-name
        (with-ivy-window
          (switch-to-buffer buffer-name)
          (goto-char (point-min))
          (forward-line (1- (swiper--line-number x)))
          (re-search-forward
           (ivy--regex ivy-text)
           (line-end-position) t)
          (funcall isearch-filter-predicate
                   (line-beginning-position)
                   (line-end-position))
          (unless (eq ivy-exit 'done)
            (swiper--cleanup)
            (swiper--add-overlays (ivy--regex ivy-text))))))))

(defun swiper-all-buffer-p (buffer)
  "Return non-nil if BUFFER should be considered by `swiper-all'."
  (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
    (cond
      ;; Ignore TAGS buffers, they tend to add duplicate results.
      ((eq mode #'tags-table-mode) nil)
      ;; Always consider dired buffers, even though they're not backed
      ;; by a file.
      ((eq mode #'dired-mode) t)
      ;; Always consider stash buffers too, as they may have
      ;; interesting content not present in any buffers. We don't #'
      ;; quote to satisfy the byte-compiler.
      ((eq mode 'magit-stash-mode) t)
      ;; Email buffers have no file, but are useful to search
      ((eq mode 'gnus-article-mode) t)
      ;; Otherwise, only consider the file if it's backed by a file.
      (t (buffer-file-name buffer)))))

;;* `swiper-all'
(defun swiper-all-function (str)
  "Search in all open buffers for STR."
  (or
   (ivy-more-chars)
   (let* ((buffers (cl-remove-if-not #'swiper-all-buffer-p (buffer-list)))
          (re-full ivy-regex)
          re re-tail
          cands match
          (case-fold-search (ivy--case-fold-p str)))
     (setq re (ivy-re-to-str re-full))
     (when (consp re-full)
       (setq re-tail (cdr re-full)))
     (dolist (buffer buffers)
       (with-current-buffer buffer
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward re nil t)
             (setq match (if (memq major-mode '(org-mode dired-mode))
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))
                           (buffer-substring
                            (line-beginning-position)
                            (line-end-position))))
             (put-text-property
              0 1 'buffer
              (buffer-name)
              match)
             (put-text-property 0 1 'point (point) match)
             (when (or (null re-tail) (ivy-re-match re-tail match))
               (push match cands))))))
     (setq ivy--old-re re-full)
     (if (null cands)
         (list "")
       (setq ivy--old-cands (nreverse cands))))))

(defun swiper--all-format-function (cands)
  "Format CANDS for `swiper-all'.
See `ivy-format-functions-alist' for further information."
  (let* ((ww swiper-window-width)
         (col2 1)
         (cands-with-buffer
          (mapcar (lambda (s)
                    (let ((buffer (get-text-property 0 'buffer s)))
                      (setq col2 (max col2 (length buffer)))
                      (cons s buffer))) cands))
         (col1 (- ww 4 col2)))
    (setq cands
          (mapcar (lambda (x)
                    (if (cdr x)
                        (let ((s (ivy--truncate-string (car x) col1)))
                          (concat
                           s
                           (make-string
                            (max 0
                                 (- ww (string-width s) (length (cdr x))))
                            ?\ )
                           (cdr x)))
                      (car x)))
                  cands-with-buffer))
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face str 'ivy-current-match))
     (lambda (str)
       str)
     cands
     "\n")))

(defvar swiper-all-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") 'swiper-all-query-replace)
    map)
  "Keymap for `swiper-all'.")

;;;###autoload
(defun swiper-all (&optional initial-input)
  "Run `swiper' for all open buffers."
  (interactive)
  (let ((swiper-window-width (- (frame-width) (if (display-graphic-p) 0 1))))
    (ivy-read "swiper-all: " 'swiper-all-function
              :action #'swiper-all-action
              :dynamic-collection t
              :keymap swiper-all-map
              :initial-input initial-input
              :caller 'swiper-all)))

(ivy-configure 'swiper-all
  :update-fn 'auto
  :unwind-fn #'swiper--cleanup
  :format-fn #'swiper--all-format-function)

(defun swiper-all-action (x)
  "Move to candidate X from `swiper-all'."
  (when (> (length x) 0)
    (let ((buffer-name (get-text-property 0 'buffer x)))
      (when buffer-name
        (with-ivy-window
          (switch-to-buffer buffer-name)
          (goto-char (get-text-property 0 'point x))
          (funcall isearch-filter-predicate
                   (line-beginning-position)
                   (line-end-position))
          (unless (eq ivy-exit 'done)
            (swiper--cleanup)
            (swiper--add-overlays (ivy--regex ivy-text))))))))

(defun swiper--multi-candidates (buffers)
  "Extract candidates from BUFFERS."
  (let ((res nil))
    (dolist (buf buffers)
      (with-current-buffer buf
        (setq res
              (nconc
               (mapcar
                (lambda (s) (put-text-property 0 1 'buffer (buffer-name) s) s)
                (swiper--candidates 4))
               res))))
    res))

;;* `swiper-isearch'
(defun swiper-isearch-function (str)
  "Collect STR matches in the current buffer for `swiper-isearch'."
  (with-ivy-window
    (swiper--isearch-function str)))

(defun swiper-match-usable-p ()
  (or search-invisible
      (not (cl-find-if
            (lambda (ov)
              (invisible-p (overlay-get ov 'invisible)))
            (overlays-at (point))))))

(defvar swiper--isearch-backward nil
  "Non-nil when performing `swiper-isearch-backward'.")

(defun swiper--isearch-function-1 (re backward)
  (unless (string= re ".")
    (let (cands)
      (save-excursion
        (goto-char (if backward (point-max) (point-min)))
        (while (and (funcall (if backward #'re-search-backward #'re-search-forward) re nil t)
                    (not (and
                          (= (match-beginning 0) (match-end 0))
                          (if backward (bobp) (eobp)))))
          (when (swiper-match-usable-p)
            (let ((pos (if (or backward swiper-goto-start-of-match)
                           (match-beginning 0)
                         (point))))
              (push pos cands)))
          (when (= (match-beginning 0) (match-end 0))
            (if backward
                (backward-char)
              (forward-char)))))
      (if backward
          cands
        (nreverse cands)))))

(defun swiper--isearch-next-item (re cands)
  (or (if swiper--isearch-backward
          (save-excursion
            ;; Match RE starting at each position in CANDS.
            (setq re (concat "\\=\\(?:" re "\\)"))
            (cl-position-if
             (lambda (x)
               (when (< x swiper--opoint)
                 (goto-char x)
                 ;; Note: Not quite the same as `looking-at' + `match-end'.
                 (re-search-forward re swiper--opoint t)))
             cands
             :from-end t))
        (cl-position swiper--opoint cands :test #'<))
      0))

(defun swiper--isearch-filter-ignore-order (re-full cands)
  (let (filtered-cands)
    (dolist (re-cons re-full cands)
      (save-excursion
        (dolist (cand cands)
          (goto-char cand)
          (beginning-of-line)
          (unless (if (re-search-forward (car re-cons) (line-end-position) t)
                      (not (cdr re-cons))
                    (cdr re-cons))
            (push cand filtered-cands))))
      (setq cands (nreverse filtered-cands))
      (setq filtered-cands nil))))

(defun swiper--isearch-function (str)
  (let ((re-full ivy-regex))
    (unless (equal re-full "")
      (let* ((case-fold-search (ivy--case-fold-p str))
             (re
              (if (stringp re-full)
                  re-full
                (mapconcat
                 #'ivy--regex-or-literal
                 (delq nil (mapcar (lambda (x) (and (cdr x) (car x))) re-full))
                 "\\|")))
             (cands (swiper--isearch-function-1 re swiper--isearch-backward)))
        (when (consp re-full)
          (setq cands (swiper--isearch-filter-ignore-order re-full cands)))
        (setq ivy--old-re re)
        (ivy-set-index (swiper--isearch-next-item re cands))
        (setq ivy--old-cands cands)))))

(defcustom swiper-isearch-highlight-delay '(2 0.2)
  "When `ivy-text' is too short, delay showing the overlay.

The default value will delay showing the overlay by 0.2 seconds
if `ivy-text' is shorter than 2 characters.

The aim is to reduce the visual clutter, since it's very rare
that we search only for one character."
  :type '(list
          (integer :tag "Text length")
          (float :tag "Delay in seconds")))

(defun swiper--delayed-add-overlays ()
  (if (and swiper-isearch-highlight-delay
           (< (length ivy-text) (car swiper-isearch-highlight-delay)))
      (setq swiper--isearch-highlight-timer
            (run-with-idle-timer
             (cadr swiper-isearch-highlight-delay) nil
             (lambda ()
               (with-ivy-window
                 (swiper--add-overlays (ivy--regex ivy-text))))))
    (dolist (re (swiper--positive-regexps))
      (swiper--add-overlays re))))

(defun swiper--isearch-candidate-pos (cand)
  "Return the buffer position of `swiper-isearch' CAND, or nil."
  (cond ((integer-or-marker-p cand) cand)
        ((and (stringp cand) (> (length cand) 0))
         (get-text-property 0 'point cand))))

(defun swiper--isearch-candidate-string (cand)
  "Return full match of `swiper-isearch' candidate CAND.
Signal an error on failure."
  ;; FIXME: Better way of getting current candidate?
  (or (let ((pos (swiper--isearch-candidate-pos cand))
            (re (ivy-re-to-str ivy-regex)))
        (save-match-data
          (save-excursion
            (and pos (goto-char pos)
                 (if (or swiper--isearch-backward swiper-goto-start-of-match)
                     (looking-at re)
                   (looking-back re (point-min)))
                 (match-string 0)))))
      (error "Could not extract `swiper-isearch' candidate: %S" cand)))

(defun swiper-isearch-action (x)
  "Move to X for `swiper-isearch'."
  (if (setq x (swiper--isearch-candidate-pos x))
      (with-ivy-window
        (goto-char x)
        (when (and (or (eq this-command 'ivy-previous-line-or-history)
                       (and (eq this-command 'ivy-done)
                            (eq last-command 'ivy-previous-line-or-history)))
                   (looking-back ivy-regex (line-beginning-position)))
          (goto-char (match-beginning 0)))
        (funcall isearch-filter-predicate (point) (1+ (point)))
        (swiper--maybe-recenter)
        (if (or (eq ivy-exit 'done)
                ;; FIXME: With the default action 'M-o o', `ivy-exit' remains
                ;; nil for some reason, so check `this-command' instead to
                ;; tell whether we're "done".
                (eq this-command #'ivy-dispatching-done))
            (progn
              (swiper--push-mark)
              (swiper--remember-search-history (ivy--regex ivy-text)))
          (swiper--cleanup)
          (swiper--delayed-add-overlays)
          (swiper--add-cursor-overlay
           (ivy-state-window ivy-last))))
    (swiper--cleanup)))

(defun swiper-action-copy (_x)
  "Copy line at point and go back."
  (kill-new
   (buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))
  (goto-char swiper--opoint))

(defun swiper-isearch-action-copy (cand)
  "Save `swiper-isearch' candidate CAND to `kill-ring'.
Return to original position."
  (unwind-protect
      (kill-new (swiper--isearch-candidate-string cand))
    ;; In case of unexpected error.
    (goto-char swiper--opoint)))

(defun swiper-isearch-action-insert (cand)
  "Insert `swiper-isearch' candidate CAND where invoked.
This cannot currently be called repeatedly without exiting
completion."
  (goto-char swiper--opoint)
  (unwind-protect
      ;; FIXME: This seems to invalidate many cached buffer positions, thus
      ;; breaking `ivy-dispatching-call'.
      (insert (swiper--isearch-candidate-string cand))
    ;; In case of unexpected error.
    (goto-char swiper--opoint)))

(ivy-add-actions 'swiper-isearch '(("w" swiper-isearch-action-copy "copy")))
(ivy-add-actions 'swiper-isearch '(("i" swiper-isearch-action-insert "insert")))
(ivy-add-actions 'swiper '(("w" swiper-action-copy "copy")))

(defun swiper--isearch-insert-current ()
  "Replace minibuffer contents with the current candidate.
Like `ivy-insert-current', but tailored for `swiper-isearch'."
  (interactive)
  (delete-minibuffer-contents)
  (let ((cur (ivy-state-current ivy-last)))
    (insert (with-ivy-window (swiper--isearch-candidate-string cur)))))

(defun swiper-isearch-thing-at-point ()
  "Insert `symbol-at-point' into the minibuffer of `swiper-isearch'.
When not running `swiper-isearch' already, start it."
  (interactive)
  (if (window-minibuffer-p)
      (let (bnd str regionp)
        (with-ivy-window
          (setq bnd
                (if (setq regionp (region-active-p))
                    (prog1 (cons (region-beginning) (region-end))
                      (deactivate-mark))
                  (bounds-of-thing-at-point 'symbol)))
          (setq str (buffer-substring-no-properties (car bnd) (cdr bnd))))
        (insert str)
        (unless regionp
          (ivy--insert-symbol-boundaries)))
    (let (thing)
      (if (use-region-p)
          (progn
            (setq thing (buffer-substring-no-properties
                         (region-beginning) (region-end)))
            (goto-char (region-beginning))
            (deactivate-mark))
        (let ((bnd (bounds-of-thing-at-point 'symbol)))
          (when bnd
            (goto-char (car bnd)))
          (setq thing (ivy-thing-at-point))))
      (swiper-isearch thing))))

(defun swiper-isearch-C-r (&optional arg)
  "Move cursor vertically up ARG candidates.
When the input is empty, browse the search history instead."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-reverse-i-search)
    (ivy-previous-line arg)))

(defvar swiper-isearch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map swiper-map)
    (define-key map [remap ivy-insert-current] #'swiper--isearch-insert-current)
    (define-key map (kbd "M-n") #'swiper-isearch-thing-at-point)
    (define-key map (kbd "C-r") #'swiper-isearch-C-r)
    map)
  "Keymap for `swiper-isearch'.")

(defun swiper--isearch-same-line-p (s1 s2)
  "Check if S1 and S2 are equal and on the same line."
  (and (equal s1 s2)
       (<= (count-lines
            (get-text-property 0 'point s2)
            (get-text-property 0 'point s1))
           1)))

(defun swiper-isearch-format-function (cands)
  (if (numberp (car-safe cands))
      (let ((re (ivy-re-to-str ivy-regex)))
        (if (string= re "^$")
            ""
          (mapconcat
           #'identity
           (swiper--isearch-format
            ivy--index ivy--length (or ivy--old-cands ivy--all-candidates)
            re
            (ivy-state-current ivy-last)
            (ivy-state-buffer ivy-last))
           "\n")))
    (funcall
     (ivy-alist-setting ivy-format-functions-alist t)
     cands)))

(defun swiper--line-at-point (pt)
  (save-excursion
    (goto-char pt)
    (let ((s (buffer-substring
              (line-beginning-position)
              (line-end-position))))
      (if (string= s "")
          s
        (put-text-property 0 1 'point pt s)
        (ivy-cleanup-string s)))))

(defun swiper--isearch-highlight (str &optional current)
  (let ((start 0)
        (i 0)
        (re (ivy-re-to-str ivy-regex)))
    (catch 'done
      (while (string-match re str start)
        (if (= (match-beginning 0) (match-end 0))
            (throw 'done t)
          (setq start (match-end 0)))
        (swiper--add-properties
         (if (eq current i)
             swiper-faces
           swiper-background-faces)
         (lambda (beg end face _priority)
           (add-face-text-property beg end face nil str)))
        (cl-incf i)))
    str))

(defun swiper--isearch-format (index length cands regex current buffer)
  (let* ((half-height (/ ivy-height 2))
         (i (1- index))
         (j 0)
         (len 0)
         res s)
    (with-current-buffer buffer
      (while (and (>= i 0)
                  (swiper--isearch-same-line-p
                   (swiper--line-at-point (nth i cands))
                   (swiper--line-at-point current)))
        (cl-decf i)
        (cl-incf j))
      (while (and (>= i 0)
                  (< len half-height))
        (setq s (swiper--line-at-point (nth i cands)))
        (unless (swiper--isearch-same-line-p s (car res))
          (push (swiper--isearch-highlight s) res)
          (cl-incf len))
        (cl-decf i))
      (setq res (nreverse res))
      (let ((current-str
             (swiper--line-at-point current))
            (start 0))
        (dotimes (_ (1+ j))
          (string-match regex current-str start)
          (setq start (match-end 0)))
        (font-lock-prepend-text-property
         0 (length current-str)
         'face 'swiper-line-face current-str)
        (swiper--isearch-highlight current-str j)
        (push current-str res))
      (cl-incf len)
      (setq i (1+ index))
      (while (and (< i length)
                  (swiper--isearch-same-line-p
                   (swiper--line-at-point (nth i cands))
                   (swiper--line-at-point current)))
        (cl-incf i))
      (while (and (< i length)
                  (< len ivy-height))
        (setq s (swiper--line-at-point (nth i cands)))
        (unless (swiper--isearch-same-line-p s (car res))
          (push (swiper--isearch-highlight s) res)
          (cl-incf len))
        (cl-incf i))
      (nreverse res))))

(defun swiper--isearch-init ()
  "Initialize `swiper-isearch'."
  (swiper--init)
  (swiper-font-lock-ensure))

(defun swiper--isearch-unwind ()
  (swiper--cleanup)
  (unless (or (eq ivy-exit 'done) swiper-stay-on-quit)
    (goto-char swiper--opoint))
  (isearch-clean-overlays)
  (swiper--ensure-visible)
  (unless (or (eq ivy-exit 'done) (string= ivy-text ""))
    (cl-pushnew ivy-text swiper-history)))

;;;###autoload
(defun swiper-isearch (&optional initial-input)
  "A `swiper' that's not line-based."
  (interactive)
  (let ((ivy-fixed-height-minibuffer t)
        (cursor-in-non-selected-windows nil)
        (swiper-min-highlight 1))
    (ivy-read
     "Swiper: "
     #'swiper-isearch-function
     :initial-input initial-input
     :keymap swiper-isearch-map
     :dynamic-collection t
     :require-match t
     :action #'swiper-isearch-action
     :re-builder #'swiper--re-builder
     :history 'swiper-history
     :extra-props (list :fname (buffer-file-name))
     :caller 'swiper-isearch)))

(ivy-configure 'swiper-isearch
  :occur #'swiper-occur
  :init-fn #'swiper--isearch-init
  :update-fn 'auto
  :unwind-fn #'swiper--isearch-unwind
  :format-fn #'swiper-isearch-format-function)

;;;###autoload
(defun swiper-isearch-backward (&optional initial-input)
  "Like `swiper-isearch' but the first result is before the point."
  (interactive)
  (let ((swiper--isearch-backward t))
    (swiper-isearch initial-input)))

(defun swiper-isearch-toggle ()
  "Two-way toggle between `swiper-isearch' and isearch.
Intended to be bound in `isearch-mode-map' and `swiper-map'."
  (interactive)
  (if isearch-mode
      (let ((query (if isearch-regexp
                       isearch-string
                     (regexp-quote isearch-string))))
        (isearch-exit)
        (goto-char (or (and isearch-forward isearch-other-end)
                       (point)))
        (swiper-isearch query))
    (ivy-exit-with-action
     (lambda (_)
       (when (looking-back (ivy-re-to-str ivy-regex) (line-beginning-position))
         (goto-char (match-beginning 0)))
       (isearch-mode t)
       (unless (string= ivy-text "")
         (isearch-yank-string ivy-text))))))

(provide 'swiper)

;;; swiper.el ends here
