;;; swiper.el --- Isearch with an overview. Oh, man! -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.11.0
;; Package-Requires: ((emacs "24.1") (ivy "0.11.0"))
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

(require 'ivy)

(defgroup swiper nil
  "`isearch' with an overview."
  :group 'matching
  :prefix "swiper-")

(defface swiper-match-face-1
  '((t (:inherit lazy-highlight)))
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

(defface swiper-background-match-face-1
  '((t (:inherit swiper-match-face-1)))
  "The background face for non-current `swiper' matches.")

(defface swiper-background-match-face-2
  '((t (:inherit swiper-match-face-2)))
  "Face for non-current `swiper' matches modulo 1.")

(defface swiper-background-match-face-3
  '((t (:inherit swiper-match-face-3)))
  "Face for non-current `swiper' matches modulo 2.")

(defface swiper-background-match-face-4
  '((t (:inherit swiper-match-face-4)))
  "Face for non-current `swiper' matches modulo 3.")

(defface swiper-line-face
  '((t (:inherit highlight)))
  "Face for current `swiper' line.")

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
               (let ((bg (face-background f1)))
                 (when bg
                   (set-face-background
                    f2
                    (colir-blend
                     (colir-color-parse bg)
                     (colir-color-parse "#ffffff"))))))
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
  "When non-nil, go to the start of the match, not its end."
  :type 'boolean
  :group 'swiper)

(defvar swiper-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-q") 'swiper-query-replace)
    (define-key map (kbd "C-l") 'swiper-recenter-top-bottom)
    (define-key map (kbd "C-'") 'swiper-avy)
    (define-key map (kbd "C-7") 'swiper-mc)
    (define-key map (kbd "C-c C-f") 'swiper-toggle-face-matching)
    map)
  "Keymap for swiper.")

(defvar swiper--query-replace-overlays nil)

(defun swiper--query-replace-updatefn ()
  (dolist (ov swiper--query-replace-overlays)
    (overlay-put ov 'after-string (propertize ivy-text 'face 'error))))

(defun swiper--query-replace-cleanup ()
  (while swiper--query-replace-overlays
    (delete-overlay (pop swiper--query-replace-overlays))))

(defun swiper--query-replace-setup ()
  (with-ivy-window
    (let ((end (window-end (selected-window) t))
          (re (ivy--regex ivy-text)))
      (save-excursion
        (goto-char (window-start))
        (while (re-search-forward re end t)
          (push (make-overlay (1- (match-end 0)) (match-end 0))
                swiper--query-replace-overlays))))))

(defun swiper-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (if (null (window-minibuffer-p))
      (user-error "Should only be called in the minibuffer through `swiper-map'")
    (swiper--query-replace-setup)
    (unwind-protect
         (let* ((enable-recursive-minibuffers t)
                (from (ivy--regex ivy-text))
                (to (minibuffer-with-setup-hook
                        (lambda ()
                          (setq minibuffer-default
                                (if (string-match "\\`\\\\_<\\(.*\\)\\\\_>\\'" ivy-text)
                                    (match-string 1 ivy-text)
                                  ivy-text)))
                      (ivy-read
                       (format "Query replace %s with: " from) nil
                       :update-fn #'swiper--query-replace-updatefn))))
           (swiper--cleanup)
           (ivy-exit-with-action
            (lambda (_)
              (with-ivy-window
                (move-beginning-of-line 1)
                (let ((inhibit-read-only t))
                  (perform-replace from to
                                   t t nil))))))
      (swiper--query-replace-cleanup))))

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

(defun swiper--avy-goto (candidate)
  (cond ((let ((win (cdr-safe candidate)))
           (and win (window-minibuffer-p win)))
         (let ((cand-text (save-excursion
                            (goto-char (car candidate))
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
           (ivy-set-index (cl-position-if
                           (lambda (x) (cl-search x cand-text))
                           ivy--old-cands))
           (ivy--exhibit)
           (ivy-done)
           (ivy-call)))
        ((or (consp candidate)
             (number-or-marker-p candidate))
         (ivy-quit-and-run
           (avy-action-goto (avy-candidate-beg candidate))))))

;;;###autoload
(defun swiper-avy ()
  "Jump to one of the current swiper candidates."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (cl-case (length ivy-text)
    (0
     (user-error "Need at least one char of input"))
    (1
     (let ((swiper-min-highlight 1))
       (swiper--update-input-ivy))))
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

(defun swiper-recenter-top-bottom (&optional arg)
  "Call (`recenter-top-bottom' ARG)."
  (interactive "P")
  (with-ivy-window
    (recenter-top-bottom arg)))

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
  "Ensure the entired buffer is highlighted."
  (unless (swiper-font-lock-ensure-p)
    (unless (or (> (buffer-size) 100000) (null font-lock-mode))
      (if (fboundp 'font-lock-ensure)
          (font-lock-ensure)
        (with-no-warnings (font-lock-fontify-buffer))))))

(defvar swiper--format-spec ""
  "Store the current candidates format spec.")

(defvar swiper--width nil
  "Store the number of digits needed for the longest line nubmer.")

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

(declare-function outline-show-all "outline")

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
      (let ((line-number 0)
            (advancer (if swiper-use-visual-line
                          (lambda (arg) (line-move arg t))
                        #'forward-line))
            candidates)
        (save-excursion
          (goto-char (point-min))
          (swiper-font-lock-ensure)
          (while (< (point) (point-max))
            (let ((str (swiper--line)))
              (setq str (ivy-cleanup-string str))
              (let ((line-number-str
                     (format swiper--format-spec (cl-incf line-number))))
                (if swiper-include-line-number-in-search
                    (setq str (concat line-number-str str))
                  (put-text-property
                   0 1 'display line-number-str str))
                (put-text-property
                 0 1 'swiper-line-number line-number-str str))
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
  (swiper--ivy (swiper--candidates) initial-input))

;;;###autoload
(defun swiper-thing-at-point ()
  "`swiper' with `ivy-thing-at-point'."
  (interactive)
  (let ((thing (ivy-thing-at-point)))
    (when (use-region-p)
      (deactivate-mark))
    (swiper thing)))

(defvar swiper--current-window-start nil
  "Store `window-start' to restore it later.
This prevents a \"jumping\" behavior which occurs when variables
such as `scroll-conservatively' are set to a high value.")

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
      (let* ((pt-min (point-min))
             (line-delta
              (save-restriction
                (widen)
                (1- (line-number-at-pos pt-min))))
             (lines
              (if (eq (ivy-state-caller ivy-last) 'swiper-isearch)
                  (swiper--isearch-occur-cands cands)
                (mapcar (lambda (s)
                          (let ((l (get-text-property 0 'swiper-line-number s)))
                            (setq s (substring s 1))
                            (put-text-property 0 1 'swiper-line-number l s)
                            (cons (read l) s)))
                        cands)))
             (offset (+ (length fname) 2)))
        (mapcar (lambda (x)
                  (let ((nn (number-to-string
                             (+ (car x) line-delta))))
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

(defun swiper-occur (&optional revert)
  "Generate a custom occur buffer for `swiper'.
When REVERT is non-nil, regenerate the current *ivy-occur* buffer.
When capture groups are present in the input, print them instead of lines."
  (let* ((buffer (ivy-state-buffer ivy-last))
         (fname (propertize
                 (with-ivy-window
                   (if (buffer-file-name buffer)
                       (file-name-nondirectory
                        (buffer-file-name buffer))
                     (buffer-name buffer)))
                 'face
                 'ivy-grep-info))
         (ivy-text (progn (string-match "\"\\(.*\\)\"" (buffer-name))
                          (match-string 1 (buffer-name))))
         (re (mapconcat #'identity (ivy--split ivy-text) ".*?"))
         (cands
          (swiper--occur-cands
           fname
           (if (not revert)
               ivy--old-cands
             (setq ivy--old-re nil)
             (save-window-excursion
               (switch-to-buffer buffer)
               (if (eq (ivy-state-caller ivy-last) 'swiper)
                   (let ((ivy--regex-function 'swiper--re-builder))
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
      (setq swiper--current-window-start nil)
      (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                      default-directory))
      (insert (format "%d candidates:\n" (length cands)))
      (ivy--occur-insert-lines
       (mapcar
        (lambda (cand) (concat "./" cand))
        cands))
      (goto-char (point-min))
      (forward-line 4))))

(ivy-set-occur 'swiper 'swiper-occur)

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
  (when (bound-and-true-p evil-mode)
    (evil-set-jump)))

(declare-function char-fold-to-regexp "char-fold")

(defun swiper--re-builder (str)
  "Transform STR into a swiper regex.
This is the regex used in the minibuffer where candidates have
line numbers.  For the buffer, use `ivy--regex' instead."
  (let* ((re-builder (ivy-alist-setting ivy-re-builders-alist))
         (re (cond
               ((equal str "")
                "")
               ((equal str "^")
                (setq ivy--subexps 0)
                ".")
               ((= (aref str 0) ?^)
                (let* ((re (funcall re-builder (substring str 1)))
                       (re (if (listp re)
                               (mapconcat (lambda (x)
                                            (format "\\(%s\\)" (car x)))
                                          (cl-remove-if-not #'cdr re)
                                          ".*?")
                             re)))
                  (if (zerop ivy--subexps)
                      (prog1 (format "^ ?\\(%s\\)" re)
                        (setq ivy--subexps 1))
                    (format "^ %s" re))))
               ((eq (bound-and-true-p search-default-mode) 'char-fold-to-regexp)
                (mapconcat #'char-fold-to-regexp (ivy--split str) ".*"))
               (t
                (funcall re-builder str)))))
    re))

(defvar swiper-history nil
  "History for `swiper'.")

(defvar swiper-invocation-face nil
  "The face at the point of invocation of `swiper'.")

(defcustom swiper-stay-on-quit nil
  "When non-nil don't go back to search start on abort."
  :type 'boolean)

(defun swiper--ivy (candidates &optional initial-input)
  "Select one of CANDIDATES and move there.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (swiper--init)
  (setq swiper-invocation-face
        (plist-get (text-properties-at (point)) 'face))
  (let ((preselect
         (if swiper-use-visual-line
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
                 :preselect preselect
                 :require-match t
                 :update-fn #'swiper--update-input-ivy
                 :unwind #'swiper--cleanup
                 :action #'swiper--action
                 :re-builder #'swiper--re-builder
                 :history 'swiper-history
                 :caller 'swiper))
          (point))
      (unless (or res swiper-stay-on-quit)
        (goto-char swiper--opoint))
      (unless (or res (string= ivy-text ""))
        (cl-pushnew ivy-text swiper-history))
      (when swiper--reveal-mode
        (reveal-mode 1)))))

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

(defun swiper--cleanup ()
  "Clean up the overlays."
  (while swiper--overlays
    (delete-overlay (pop swiper--overlays)))
  (save-excursion
    (goto-char (point-min))
    (isearch-clean-overlays)))

(defun swiper--recenter-p ()
  (or (display-graphic-p)
      (not recenter-redisplay)))

(defun swiper--update-input-ivy ()
  "Called when `ivy' input is updated."
  (with-ivy-window
    (swiper--cleanup)
    (when (> (length (ivy-state-current ivy-last)) 0)
      (let* ((regexp-or-regexps (funcall ivy--regex-function ivy-text))
             (regexps
              (if (listp regexp-or-regexps)
                  (mapcar #'car (cl-remove-if-not #'cdr regexp-or-regexps))
                (list regexp-or-regexps))))
        (dolist (re regexps)
          (let* ((re (replace-regexp-in-string
                      "    " "\t"
                      re))
                 (str (get-text-property 0 'swiper-line-number (ivy-state-current ivy-last)))
                 (num (if (string-match "^[0-9]+" str)
                          (string-to-number (match-string 0 str))
                        0)))
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
                         (>= swiper--opoint (line-beginning-position))
                         (<= swiper--opoint (line-end-position)))
                    (goto-char swiper--opoint)
                  (if (eq swiper--current-line num)
                      (when swiper--current-match-start
                        (goto-char swiper--current-match-start))
                    (setq swiper--current-line num))
                  (when (re-search-forward re (line-end-position) t)
                    (setq swiper--current-match-start (match-beginning 0))))
                (isearch-range-invisible (line-beginning-position)
                                         (line-end-position))
                (when (and (swiper--recenter-p)
                           (or
                            (< (point) (window-start))
                            (> (point) (window-end (ivy-state-window ivy-last) t))))
                  (recenter))
                (setq swiper--current-window-start (window-start))))
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
              swiper--point-max))))))))

(defun swiper--add-overlays (re &optional beg end wnd)
  "Add overlays for RE regexp in visible part of the current buffer.
BEG and END, when specified, are the point bounds.
WND, when specified is the window."
  (setq wnd (or wnd (ivy-state-window ivy-last)))
  (let ((pt (point))
        (ov (if visual-line-mode
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
                          (point))))
           (case-fold-search (ivy--case-fold-p re)))
      (when (>= (length re) swiper-min-highlight)
        (save-excursion
          (goto-char beg)
          ;; RE can become an invalid regexp
          (while (and (ignore-errors (re-search-forward re end t))
                      (> (- (match-end 0) (match-beginning 0)) 0))
            ;; Don't highlight a match if it spans multiple
            ;; lines. `count-lines' returns 1 if the match is within a
            ;; single line, even if it includes the newline, and 2 or
            ;; greater otherwise. We hope that the inclusion of the
            ;; newline will not ever be a problem in practice.
            (when (< (count-lines (match-beginning 0) (match-end 0)) 2)
              (let ((faces (if (= (match-end 0) pt)
                               swiper-faces
                             swiper-background-faces)))
                (unless (and (consp ivy--old-re)
                             (null
                              (save-match-data
                                (ivy--re-filter ivy--old-re
                                                (list
                                                 (buffer-substring-no-properties
                                                  (line-beginning-position)
                                                  (line-end-position)))))))
                  (let ((mb (match-beginning 0))
                        (me (match-end 0)))
                    (unless (> (- me mb) 2017)
                      (swiper--add-overlay mb me
                                           (if (zerop ivy--subexps)
                                               (cadr faces)
                                             (car faces))
                                           wnd 0))))
                (let ((i 1)
                      (j 0))
                  (while (<= (cl-incf j) ivy--subexps)
                    (let ((bm (match-beginning j))
                          (em (match-end j)))
                      (when (and (integerp em)
                                 (integerp bm))
                        (while (and (< j ivy--subexps)
                                    (integerp (match-beginning (+ j 1)))
                                    (= em (match-beginning (+ j 1))))
                          (setq em (match-end (cl-incf j))))
                        (swiper--add-overlay
                         bm em
                         (nth (1+ (mod (+ i 2) (1- (length faces))))
                              faces)
                         wnd i)
                        (cl-incf i)))))))))))))

(defun swiper--add-overlay (beg end face wnd priority)
  "Add overlay bound by BEG and END to `swiper--overlays'.
FACE, WND and PRIORITY are properties corresponding to
the face, window and priority of the overlay."
  (let ((overlay (make-overlay beg end)))
    (push overlay swiper--overlays)
    (overlay-put overlay 'face face)
    (overlay-put overlay 'window wnd)
    (overlay-put overlay 'priority priority)))

(defcustom swiper-action-recenter nil
  "When non-nil, recenter after exiting `swiper'."
  :type 'boolean)
(defvar evil-search-module)
(defvar evil-ex-search-pattern)
(defvar evil-ex-search-persistent-highlight)
(defvar evil-ex-search-direction)
(declare-function evil-ex-search-activate-highlight "evil-ex")


(defun swiper--action (x)
  "Goto line X."
  (let ((ln (1- (read (get-text-property 0 'swiper-line-number x))))
        (re (ivy--regex ivy-text)))
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
        (when (and (re-search-forward re (line-end-position) t) swiper-goto-start-of-match)
          (goto-char (match-beginning 0)))
        (swiper--ensure-visible)
        (cond (swiper-action-recenter
               (recenter))
              ((and swiper--current-window-start
                    (swiper--recenter-p))
               (set-window-start (selected-window) swiper--current-window-start)))
        (when (/= (point) swiper--opoint)
          (unless (and transient-mark-mode mark-active)
            (when (eq ivy-exit 'done)
              (push-mark swiper--opoint t)
              (message "Mark saved where search started"))))
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
              (evil-ex-search-activate-highlight evil-ex-search-pattern))))))))

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
  "Return prompt for `swiper-multi'."
  (format "Buffers (%s): "
          (mapconcat #'identity swiper-multi-buffers ", ")))

(defun swiper-multi ()
  "Select one or more buffers.
Run `swiper' for those buffers."
  (interactive)
  (setq swiper-multi-buffers nil)
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (swiper-multi-prompt)
              #'internal-complete-buffer
              :action #'swiper-multi-action-1))
  (ivy-read "Swiper: " swiper-multi-candidates
            :action #'swiper-multi-action-2
            :unwind #'swiper--cleanup
            :caller 'swiper-multi))

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
          (forward-line (1- (read (get-text-property 0 'swiper-line-number x))))
          (re-search-forward
           (ivy--regex ivy-text)
           (line-end-position) t)
          (isearch-range-invisible (line-beginning-position)
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
          (re-full (funcall ivy--regex-function str))
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

(defvar swiper-window-width 80)

(defun swiper--all-format-function (cands)
  "Format CANDS for `swiper-all'.
See `ivy-format-function' for further information."
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
  (let* ((swiper-window-width (- (frame-width) (if (display-graphic-p) 0 1)))
         (ivy-format-function #'swiper--all-format-function))
    (ivy-read "swiper-all: " 'swiper-all-function
              :action #'swiper-all-action
              :unwind #'swiper--cleanup
              :update-fn 'auto
              :dynamic-collection t
              :keymap swiper-all-map
              :initial-input initial-input
              :caller 'swiper-multi)))

(defun swiper-all-action (x)
  "Move to candidate X from `swiper-all'."
  (when (> (length x) 0)
    (let ((buffer-name (get-text-property 0 'buffer x)))
      (when buffer-name
        (with-ivy-window
          (switch-to-buffer buffer-name)
          (goto-char (get-text-property 0 'point x))
          (isearch-range-invisible (line-beginning-position)
                                   (line-end-position))
          (unless (eq ivy-exit 'done)
            (swiper--cleanup)
            (swiper--add-overlays (ivy--regex ivy-text))))))))

(defun swiper--multi-candidates (buffers)
  "Extract candidates from BUFFERS."
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
                       (max 0
                            (- ww (string-width s) (length (buffer-name)) 3))
                       ?\ )
                      (buffer-name))
                     s)
                    (put-text-property 0 len 'buffer buf s)
                    s))
                (swiper--candidates 4))
               res))
        nil))
    res))

;;* `swiper-isearch'
(defvar swiper--isearch-point-history nil
  "Store the current input and point history for a single search.
Each element is a cons cell of an input and a point position that
corresponds to it.

This ensures that if the user enters \"ab\", the point will
come back to the same place as when \"a\" was initially entered.")

(defun swiper-isearch-function (str)
  "Collect STR matches in the current buffer for `swiper-isearch'."
  (let* ((case-fold-search (ivy--case-fold-p str))
         (re-full (funcall ivy--regex-function str))
         (re (ivy-re-to-str re-full)))
    (unless (string= re "")
      (let ((re (if (string-match "\\`\\(.*\\)[\\]|\\'" re)
                    (match-string 1 re)
                  re))
            (pt-hist (cdr (assoc str swiper--isearch-point-history)))
            cands
            idx-found
            (idx 0))
        (with-ivy-window
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward re nil t)
              (unless idx-found
                (when (or
                       (eq (match-beginning 0) pt-hist)
                       (>= (match-beginning 0) (cdar swiper--isearch-point-history)))
                  (push (cons str (match-beginning 0)) swiper--isearch-point-history)
                  (setq idx-found idx)))
              (cl-incf idx)
              (let ((line (buffer-substring
                           (line-beginning-position)
                           (line-end-position)))
                    (pos (if swiper-goto-start-of-match
                             (match-beginning 0)
                           (point))))
                (put-text-property 0 1 'point pos line)
                (push line cands)))))
        (setq ivy--old-re re)
        (when idx-found
          (ivy-set-index idx-found))
        (setq ivy--old-cands (nreverse cands))))))

(defun swiper--add-cursor-overlay ()
  (let ((ov (make-overlay (point) (if (eolp) (point) (1+ (point))))))
    (if (eolp)
        (overlay-put ov 'after-string (propertize " " 'face 'ivy-cursor))
      (overlay-put ov 'face 'ivy-cursor))
    (push ov swiper--overlays)))

(defun swiper-isearch-action (x)
  "Move to X for `swiper-isearch'."
  (if (> (length x) 0)
      (with-ivy-window
        (goto-char (get-text-property 0 'point x))
        (isearch-range-invisible (line-beginning-position)
                                 (line-end-position))
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text))
          (swiper--add-cursor-overlay)))
    (swiper--cleanup)))

(defun swiper-isearch-thing-at-point ()
  "Insert `symbol-at-point' into the minibuffer of `swiper-isearch'.
When not running `swiper-isearch' already, start it."
  (interactive)
  (if (window-minibuffer-p)
      (let (bnd str)
        (with-ivy-window
          (setq bnd (bounds-of-thing-at-point 'symbol))
          (setq str (buffer-substring-no-properties (car bnd) (cdr bnd))))
        (setq swiper--isearch-point-history
              (list (cons "" (car bnd))))
        (insert str)
        (ivy--insert-symbol-boundaries))
    (let (thing)
      (if (use-region-p)
          (progn
            (goto-char (region-beginning))
            (setq thing (ivy-thing-at-point))
            (deactivate-mark))
        (let ((bnd (bounds-of-thing-at-point 'symbol)))
          (when bnd
            (goto-char (car bnd)))
          (setq thing (ivy-thing-at-point))))
      (swiper-isearch thing))))

(defvar swiper-isearch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map swiper-map)
    (define-key map (kbd "M-n") 'swiper-isearch-thing-at-point)
    map)
  "Keymap for `swiper-isearch'.")

(defface swiper-isearch-current-match
  '((((class color) (background light))
     :background "#65a7e2" :foreground "white")
    (((class color) (background dark))
     :background "#1a4b77" :foreground "black"))
  "Face used by `swiper-isearch' for highlighting the current match.")

(defun swiper-isearch-format-function (_cands)
  (let* ((half-height (/ ivy-height 2))
         (current (ivy-state-current ivy-last))
         (i (1- ivy--index))
         (j 0)
         (len 0)
         res s)
    (while (and (>= i 0)
                (string= (nth i ivy--old-cands)
                         current))
      (cl-decf i)
      (cl-incf j))
    (while (and (>= i 0)
                (< len half-height))
      (setq s (nth i ivy--old-cands))
      (unless (equal s (car res))
        (push (ivy--format-minibuffer-line s) res)
        (cl-incf len))
      (cl-decf i))
    (setq res (nreverse res))
    (let ((current-str
           (ivy--add-face
            (ivy--format-minibuffer-line current)
            'ivy-current-match))
          (start 0))
      (dotimes (_ (1+ j))
        (string-match ivy--old-re current-str start)
        (setq start (match-end 0)))
      (ivy-add-face-text-property
       (match-beginning 0) (match-end 0)
       'swiper-isearch-current-match current-str)
      (push current-str res))
    (cl-incf len)
    (setq i (1+ ivy--index))
    (while (and (< i ivy--length)
                (string= (nth i ivy--old-cands) current))
      (cl-incf i))
    (while (and (< i ivy--length)
                (< len ivy-height))
      (setq s (nth i ivy--old-cands))
      (unless (equal s (car res))
        (push (ivy--format-minibuffer-line s) res)
        (cl-incf len))
      (cl-incf i))
    (mapconcat #'identity (nreverse res) "\n")))

;;;###autoload
(defun swiper-isearch (&optional initial-input)
  "A `swiper' that's not line-based."
  (interactive)
  (swiper--init)
  (swiper-font-lock-ensure)
  (setq swiper--isearch-point-history
        (list
         (cons "" (point))))
  (let ((ivy-fixed-height-minibuffer t)
        (cursor-in-non-selected-windows nil)
        (swiper-min-highlight 1)
        (ivy-format-function #'swiper-isearch-format-function)
        res)
    (unwind-protect
         (and
          (setq res
                (ivy-read
                 "Swiper: "
                 #'swiper-isearch-function
                 :initial-input initial-input
                 :keymap swiper-isearch-map
                 :dynamic-collection t
                 :require-match t
                 :action #'swiper-isearch-action
                 :update-fn 'auto
                 :unwind #'swiper--cleanup
                 :history 'swiper-history
                 :caller 'swiper-isearch))
          (point))
      (unless (or res swiper-stay-on-quit)
        (goto-char swiper--opoint))
      (unless (or res (string= ivy-text ""))
        (cl-pushnew ivy-text swiper-history)))))

(ivy-set-occur 'swiper-isearch 'swiper-occur)

(provide 'swiper)

;;; swiper.el ends here
