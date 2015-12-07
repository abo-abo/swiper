;;; counsel.el --- Various completion functions using Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1") (swiper "0.4.0"))
;; Keywords: completion, matching

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
;; Just call one of the interactive functions in this file to complete
;; the corresponding thing using `ivy'.
;;
;; Currently available: Elisp symbols, Clojure symbols, Git files.

;;; Code:

(require 'swiper)
(require 'etags)

(defvar counsel-completion-beg nil
  "Completion bounds start.")

(defvar counsel-completion-end nil
  "Completion bounds end.")

;;;###autoload
(defun counsel-el ()
  "Elisp completion at point."
  (interactive)
  (let* ((bnd (unless (and (looking-at ")")
                           (eq (char-before) ?\())
                (bounds-of-thing-at-point
                 'symbol)))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd)
                   (cdr bnd))
                ""))
         (ivy-height 7)
         (funp (eq (char-before (car bnd)) ?\())
         symbol-names)
    (if bnd
        (progn
          (setq counsel-completion-beg
                (move-marker (make-marker) (car bnd)))
          (setq counsel-completion-end
                (move-marker (make-marker) (cdr bnd))))
      (setq counsel-completion-beg nil)
      (setq counsel-completion-end nil))
    (if (string= str "")
        (mapatoms
         (lambda (x)
           (when (symbolp x)
             (push (symbol-name x) symbol-names))))
      (setq symbol-names
            (all-completions str obarray
                             (and funp
                                  (lambda (x)
                                    (or (functionp x)
                                        (macrop x)
                                        (special-form-p x)))))))
    (ivy-read "Symbol name: " symbol-names
              :predicate (and funp #'functionp)
              :initial-input str
              :action #'counsel--el-action)))

(declare-function slime-symbol-start-pos "ext:slime")
(declare-function slime-symbol-end-pos "ext:slime")
(declare-function slime-contextual-completions "ext:slime-c-p-c")

;;;###autoload
(defun counsel-cl ()
  "Common Lisp completion at point."
  (interactive)
  (setq counsel-completion-beg (slime-symbol-start-pos))
  (setq counsel-completion-end (slime-symbol-end-pos))
  (ivy-read "Symbol name: "
            (car (slime-contextual-completions
                  counsel-completion-beg
                  counsel-completion-end))
            :action #'counsel--el-action))

(defun counsel--el-action (symbol)
  "Insert SYMBOL, erasing the previous one."
  (when (stringp symbol)
    (with-ivy-window
      (when counsel-completion-beg
        (delete-region
         counsel-completion-beg
         counsel-completion-end))
      (setq counsel-completion-beg
            (move-marker (make-marker) (point)))
      (insert symbol)
      (setq counsel-completion-end
            (move-marker (make-marker) (point))))))

(declare-function deferred:sync! "ext:deferred")
(declare-function jedi:complete-request "ext:jedi-core")
(declare-function jedi:ac-direct-matches "ext:jedi")

(defun counsel-jedi ()
  "Python completion at point."
  (interactive)
  (let ((bnd (bounds-of-thing-at-point 'symbol)))
    (if bnd
        (progn
          (setq counsel-completion-beg (car bnd))
          (setq counsel-completion-end (cdr bnd)))
      (setq counsel-completion-beg nil)
      (setq counsel-completion-end nil)))
  (deferred:sync!
   (jedi:complete-request))
  (ivy-read "Symbol name: " (jedi:ac-direct-matches)
            :action #'counsel--py-action))

(defun counsel--py-action (symbol)
  "Insert SYMBOL, erasing the previous one."
  (when (stringp symbol)
    (with-ivy-window
      (when counsel-completion-beg
        (delete-region
         counsel-completion-beg
         counsel-completion-end))
      (setq counsel-completion-beg
            (move-marker (make-marker) (point)))
      (insert symbol)
      (setq counsel-completion-end
            (move-marker (make-marker) (point)))
      (when (equal (get-text-property 0 'symbol symbol) "f")
        (insert "()")
        (setq counsel-completion-end
              (move-marker (make-marker) (point)))
        (backward-char 1)))))

(defvar counsel-describe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'counsel-find-symbol)
    (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
    map))

(defun counsel-find-symbol ()
  "Jump to the definition of the current symbol."
  (interactive)
  (ivy-exit-with-action #'counsel--find-symbol))

(defun counsel--info-lookup-symbol ()
  "Lookup the current symbol in the info docs."
  (interactive)
  (ivy-exit-with-action #'counsel-info-lookup-symbol))

(defun counsel--find-symbol (x)
  "Find symbol definition that corresponds to string X."
  (with-no-warnings
    (ring-insert find-tag-marker-ring (point-marker)))
  (let ((full-name (get-text-property 0 'full-name x)))
    (if full-name
        (find-library full-name)
      (let ((sym (read x)))
        (cond ((and (eq (ivy-state-caller ivy-last)
                        'counsel-describe-variable)
                    (boundp sym))
               (find-variable sym))
              ((fboundp sym)
               (find-function sym))
              ((boundp sym)
               (find-variable sym))
              ((or (featurep sym)
                   (locate-library
                    (prin1-to-string sym)))
               (find-library
                (prin1-to-string sym)))
              (t
               (error "Couldn't fild definition of %s"
                      sym)))))))

(defvar counsel-describe-symbol-history nil
  "History for `counsel-describe-variable' and `counsel-describe-function'.")

(defun counsel-symbol-at-point ()
  "Return current symbol at point as a string."
  (let ((s (thing-at-point 'symbol)))
    (and (stringp s)
         (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
             (match-string 1 s)
           s))))

(defun counsel-variable-list ()
  "Return the list of all currently bound variables."
  (let (cands)
    (mapatoms
     (lambda (vv)
       (when (or (get vv 'variable-documentation)
                 (and (boundp vv) (not (keywordp vv))))
         (push (symbol-name vv) cands))))
    cands))

;;;###autoload
(defun counsel-describe-variable ()
  "Forward to `describe-variable'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read
     "Describe variable: "
     (counsel-variable-list)
     :keymap counsel-describe-map
     :preselect (counsel-symbol-at-point)
     :history 'counsel-describe-symbol-history
     :require-match t
     :sort t
     :action (lambda (x)
               (describe-variable
                (intern x)))
     :caller 'counsel-describe-variable)))

(ivy-set-actions
 'counsel-describe-variable
 '(("i" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(ivy-set-actions
 'counsel-describe-function
 '(("i" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(ivy-set-actions
 'counsel-M-x
 '(("d" counsel--find-symbol "definition")))

;;;###autoload
(defun counsel-describe-function ()
  "Forward to `describe-function'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe function: "
              (let (cands)
                (mapatoms
                 (lambda (x)
                   (when (fboundp x)
                     (push (symbol-name x) cands))))
                cands)
              :keymap counsel-describe-map
              :preselect (counsel-symbol-at-point)
              :history 'counsel-describe-symbol-history
              :require-match t
              :sort t
              :action (lambda (x)
                        (describe-function
                         (intern x)))
              :caller 'counsel-describe-function)))

(defvar info-lookup-mode)
(declare-function info-lookup->completions "info-look")
(declare-function info-lookup->mode-value "info-look")
(declare-function info-lookup-select-mode "info-look")
(declare-function info-lookup-change-mode "info-look")
(declare-function info-lookup "info-look")

;;;###autoload
(defun counsel-info-lookup-symbol (symbol &optional mode)
  "Forward to (`info-describe-symbol' SYMBOL MODE) with ivy completion."
  (interactive
   (progn
     (require 'info-look)
     (let* ((topic 'symbol)
            (mode (cond (current-prefix-arg
                         (info-lookup-change-mode topic))
                        ((info-lookup->mode-value
                          topic (info-lookup-select-mode))
                         info-lookup-mode)
                        ((info-lookup-change-mode topic))))
            (completions (info-lookup->completions topic mode))
            (enable-recursive-minibuffers t)
            (value (ivy-read
                    "Describe symbol: "
                    (mapcar #'car completions)
                    :sort t)))
       (list value info-lookup-mode))))
  (require 'info-look)
  (info-lookup 'symbol symbol mode))

(defvar counsel-unicode-char-history nil
  "History for `counsel-unicode-char'.")

;;;###autoload
(defun counsel-unicode-char ()
  "Insert a Unicode character at point."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (setq counsel-completion-beg (point))
    (setq counsel-completion-end (point))
    (ivy-read "Unicode name: "
              (mapcar (lambda (x)
                        (propertize
                         (format "% -60s%c" (car x) (cdr x))
                         'result (cdr x)))
                      (ucs-names))
              :action (lambda (char)
                        (with-ivy-window
                          (delete-region counsel-completion-beg counsel-completion-end)
                          (setq counsel-completion-beg (point))
                          (insert-char (get-text-property 0 'result char))
                          (setq counsel-completion-end (point))))
              :history 'counsel-unicode-char-history)))

(declare-function cider-sync-request:complete "ext:cider-client")
;;;###autoload
(defun counsel-clj ()
  "Clojure completion at point."
  (interactive)
  (counsel--generic
   (lambda (str)
     (mapcar
      #'cl-caddr
      (cider-sync-request:complete str ":same")))))

;;;###autoload
(defun counsel-git ()
  "Find file in the current Git repository."
  (interactive)
  (let* ((default-directory (locate-dominating-file
                             default-directory ".git"))
         (cands (split-string
                 (shell-command-to-string
                  "git ls-files --full-name --")
                 "\n"
                 t))
         (action `(lambda (x)
                    (let ((default-directory ,default-directory))
                      (find-file x)))))
    (ivy-read "Find file: " cands
              :action action)))

(defvar counsel--git-grep-dir nil
  "Store the base git directory.")

(defvar counsel--git-grep-count nil
  "Store the line count in current repository.")

(defun counsel-more-chars (n)
  "Return two fake candidates prompting for at least N input."
  (list ""
        (format "%d chars more" (- n (length ivy-text)))))

(defvar counsel-git-grep-cmd "git --no-pager grep --full-name -n --no-color -i -e %S"
  "Store the command for `counsel-git-grep'.")

(defun counsel-git-grep-function (string &optional _pred &rest _unused)
  "Grep in the current git repository for STRING."
  (if (and (> counsel--git-grep-count 20000)
           (< (length string) 3))
      (counsel-more-chars 3)
    (let* ((default-directory counsel--git-grep-dir)
           (cmd (format counsel-git-grep-cmd
                        (setq ivy--old-re (ivy--regex string t)))))
      (if (<= counsel--git-grep-count 20000)
          (split-string (shell-command-to-string cmd) "\n" t)
        (counsel--gg-candidates (ivy--regex string))
        nil))))

(defvar counsel-git-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'counsel-git-grep-recenter)
    (define-key map (kbd "M-q") 'counsel-git-grep-query-replace)
    map))

(defun counsel-git-grep-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (if (null (window-minibuffer-p))
      (user-error
       "Should only be called in the minibuffer through `counsel-git-grep-map'")
    (let* ((enable-recursive-minibuffers t)
           (from (ivy--regex ivy-text))
           (to (query-replace-read-to from "Query replace" t)))
      (ivy-exit-with-action
       (lambda (_)
         (let (done-buffers)
           (dolist (cand ivy--old-cands)
             (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
               (with-ivy-window
                 (let ((file-name (match-string-no-properties 1 cand)))
                   (setq file-name (expand-file-name file-name counsel--git-grep-dir))
                   (unless (member file-name done-buffers)
                     (push file-name done-buffers)
                     (find-file file-name)
                     (goto-char (point-min)))
                   (perform-replace from to t t nil)))))))))))

(defun counsel-git-grep-recenter ()
  (interactive)
  (with-ivy-window
    (counsel-git-grep-action ivy--current)
    (recenter-top-bottom)))

(defun counsel-git-grep-action (x)
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (with-ivy-window
      (let ((file-name (match-string-no-properties 1 x))
            (line-number (match-string-no-properties 2 x)))
        (find-file (expand-file-name file-name counsel--git-grep-dir))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))
        (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(defvar counsel-git-grep-history nil
  "History for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd-history
  '("git --no-pager grep --full-name -n --no-color -i -e %S")
  "History for `counsel-git-grep' shell commands.")

;;;###autoload
(defun counsel-git-grep (&optional cmd initial-input)
  "Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive "P")
  (cond
    ((stringp cmd)
     (setq counsel-git-grep-cmd cmd))
    (cmd
     (setq counsel-git-grep-cmd
           (ivy-read "cmd: " counsel-git-grep-cmd-history
                     :history 'counsel-git-grep-cmd-history))
     (setq counsel-git-grep-cmd-history
           (delete-dups counsel-git-grep-cmd-history)))
    (t
     (setq counsel-git-grep-cmd "git --no-pager grep --full-name -n --no-color -i -e %S")))
  (setq counsel--git-grep-dir
        (locate-dominating-file default-directory ".git"))
  (if (null counsel--git-grep-dir)
      (error "Not in a git repository")
    (setq counsel--git-grep-count (counsel--gg-count "" t))
    (ivy-read "git grep: " 'counsel-git-grep-function
              :initial-input initial-input
              :matcher #'counsel-git-grep-matcher
              :dynamic-collection (> counsel--git-grep-count 20000)
              :keymap counsel-git-grep-map
              :action #'counsel-git-grep-action
              :unwind #'swiper--cleanup
              :history 'counsel-git-grep-history
              :caller 'counsel-git-grep)))

(defcustom counsel-find-file-at-point nil
  "When non-nil, add file-at-point to the list of candidates."
  :type 'boolean
  :group 'ivy)

(declare-function ffap-guesser "ffap")

(defvar counsel-find-file-map (make-sparse-keymap))

;;;###autoload
(defun counsel-find-file ()
  "Forward to `find-file'."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (with-ivy-window
                (find-file (expand-file-name x ivy--directory))))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (ffap-guesser))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map))

(defcustom counsel-find-file-ignore-regexp nil
  "A regexp of files to ignore while in `counsel-find-file'.
These files are un-ignored if `ivy-text' matches them.
The common way to show all files is to start `ivy-text' with a dot.
Possible value: \"\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\"."
  :group 'ivy)

(defun counsel--find-file-matcher (regexp candidates)
  "Return REGEXP-matching CANDIDATES.
Skip some dotfiles unless `ivy-text' requires them."
  (let ((res (cl-remove-if-not
              (lambda (x)
                (string-match regexp x))
              candidates)))
    (if (or (null counsel-find-file-ignore-regexp)
            (string-match counsel-find-file-ignore-regexp ivy-text))
        res
      (cl-remove-if
       (lambda (x)
         (string-match counsel-find-file-ignore-regexp x))
       res))))

(defun counsel-git-grep-matcher (regexp candidates)
  (or (and (equal regexp ivy--old-re)
           ivy--old-cands)
      (prog1
          (setq ivy--old-cands
                (cl-remove-if-not
                 (lambda (x)
                   (ignore-errors
                     (when (string-match "^[^:]+:[^:]+:" x)
                       (setq x (substring x (match-end 0)))
                       (if (stringp regexp)
                           (string-match regexp x)
                         (let ((res t))
                           (dolist (re regexp)
                             (setq res
                                   (and res
                                        (ignore-errors
                                          (if (cdr re)
                                              (string-match (car re) x)
                                            (not (string-match (car re) x)))))))
                           res)))))
                 candidates))
        (setq ivy--old-re regexp))))

(defvar counsel--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defun counsel--async-command (cmd)
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (setq counsel--async-time (current-time))
    (set-process-sentinel proc #'counsel--async-sentinel)
    (set-process-filter proc #'counsel--async-filter)))

(defun counsel--async-sentinel (process event)
  (if (string= event "finished\n")
      (progn
        (with-current-buffer (process-buffer process)
          (setq ivy--all-candidates
                (ivy--sort-maybe
                 (split-string (buffer-string) "\n" t)))
          (if (null ivy--old-cands)
              (setq ivy--index
                    (or (ivy--preselect-index
                         (ivy-state-preselect ivy-last)
                         ivy--all-candidates)
                        0))
            (ivy--recompute-index
             ivy-text
             (funcall ivy--regex-function ivy-text)
             ivy--all-candidates))
          (setq ivy--old-cands ivy--all-candidates))
        (ivy--exhibit))
    (if (string= event "exited abnormally with code 1\n")
        (progn
          (setq ivy--all-candidates '("Error"))
          (setq ivy--old-cands ivy--all-candidates)
          (ivy--exhibit)))))

(defun counsel--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
0.5 seconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (let (size)
    (when (time-less-p
           ;; 0.5s
           '(0 0 500000 0)
           (time-since counsel--async-time))
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (setq size (- (buffer-size) (forward-line (buffer-size)))))
      (ivy--insert-minibuffer
       (format "\ncollected: %d" size))
      (setq counsel--async-time (current-time)))))

(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command on X."
  (call-process shell-file-name nil
                nil nil
                shell-command-switch
                (format "%s %s"
                        (if (eq system-type 'darwin)
                                    "open"
                                  "xdg-open")
                        (shell-quote-argument x))))

(declare-function dired-jump "dired-x")
(defun counsel-locate-action-dired (x)
  "Use `dired-jump' on X."
  (dired-jump nil x))

(defvar counsel-locate-history nil
  "History for `counsel-locate'.")

(defcustom counsel-locate-options (if (eq system-type 'darwin)
                                      '("-i")
                                    '("-i" "--regex"))
  "Command line options for `locate`."
  :group 'ivy
  :type  '(repeat string))

(ivy-set-actions
 'counsel-locate
 '(("x" counsel-locate-action-extern "xdg-open")
   ("d" counsel-locate-action-dired "dired")))

(defun counsel-unquote-regex-parens (str)
  (replace-regexp-in-string
   "\\\\)" ")"
   (replace-regexp-in-string
    "\\\\(" "("
    str)))

(defun counsel-locate-function (str &rest _u)
  (if (< (length str) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "locate %s '%s'"
             (mapconcat #'identity counsel-locate-options " ")
             (counsel-unquote-regex-parens
              (ivy--regex str))))
    '("" "working...")))

(defun counsel-delete-process ()
  (let ((process (get-process " *counsel*")))
    (when process
      (delete-process process))))

;;;###autoload
(defun counsel-locate (&optional initial-input)
  "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " #'counsel-locate-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (with-ivy-window
                        (when file
                          (find-file file))))
            :unwind #'counsel-delete-process))

(defun counsel--generic (completion-fn)
  "Complete thing at point with COMPLETION-FN."
  (let* ((bnd (bounds-of-thing-at-point 'symbol))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd) (cdr bnd))
                ""))
         (candidates (funcall completion-fn str))
         (ivy-height 7)
         (res (ivy-read (format "pattern (%s): " str)
                        candidates)))
    (when (stringp res)
      (when bnd
        (delete-region (car bnd) (cdr bnd)))
      (insert res))))

(defun counsel-directory-parent (dir)
  "Return the directory parent of directory DIR."
  (concat (file-name-nondirectory
           (directory-file-name dir)) "/"))

(defun counsel-string-compose (prefix str)
  "Make PREFIX the display prefix of STR though text properties."
  (let ((str (copy-sequence str)))
    (put-text-property
     0 1 'display
     (concat prefix (substring str 0 1))
     str)
    str))

;;;###autoload
(defun counsel-load-library ()
  "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((dirs load-path)
        (suffix (concat (regexp-opt '(".el" ".el.gz") t) "\\'"))
        (cands (make-hash-table :test #'equal))
        short-name
        old-val
        dir-parent
        res)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions "" dir))
          (when (string-match suffix file)
            (unless (string-match "pkg.elc?$" file)
              (setq short-name (substring file 0 (match-beginning 0)))
              (if (setq old-val (gethash short-name cands))
                  (progn
                    ;; assume going up directory once will resolve name clash
                    (setq dir-parent (counsel-directory-parent (cdr old-val)))
                    (puthash short-name
                             (cons
                              (counsel-string-compose dir-parent (car old-val))
                              (cdr old-val))
                             cands)
                    (setq dir-parent (counsel-directory-parent dir))
                    (puthash (concat dir-parent short-name)
                             (cons
                              (propertize
                               (counsel-string-compose
                                dir-parent short-name)
                               'full-name (expand-file-name file dir))
                              dir)
                             cands))
                (puthash short-name
                         (cons (propertize
                                short-name
                                'full-name (expand-file-name file dir))
                               dir) cands)))))))
    (maphash (lambda (_k v) (push (car v) res)) cands)
    (ivy-read "Load library: " (nreverse res)
              :action (lambda (x)
                        (load-library
                         (get-text-property 0 'full-name x)))
              :keymap counsel-describe-map)))

(defvar counsel-gg-state nil
  "The current state of candidates / count sync.")

(defun counsel--gg-candidates (regex)
  "Return git grep candidates for REGEX."
  (setq counsel-gg-state -2)
  (counsel--gg-count regex)
  (let* ((default-directory counsel--git-grep-dir)
         (counsel-gg-process " *counsel-gg*")
         (proc (get-process counsel-gg-process))
         (buff (get-buffer counsel-gg-process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel-gg-process
                counsel-gg-process
                (concat
                 (format counsel-git-grep-cmd regex)
                 " | head -n 200")))
    (set-process-sentinel
     proc
     #'counsel--gg-sentinel)))

(defun counsel--gg-sentinel (process event)
  (if (string= event "finished\n")
      (progn
        (with-current-buffer (process-buffer process)
          (setq ivy--all-candidates
                (or (split-string (buffer-string) "\n" t)
                    '("")))
          (setq ivy--old-cands ivy--all-candidates))
        (when (= 0 (cl-incf counsel-gg-state))
          (ivy--exhibit)))
    (if (string= event "exited abnormally with code 1\n")
        (progn
          (setq ivy--all-candidates '("Error"))
          (setq ivy--old-cands ivy--all-candidates)
          (ivy--exhibit)))))

(defun counsel--gg-count (regex &optional no-async)
  "Quickly and asynchronously count the amount of git grep REGEX matches.
When NO-ASYNC is non-nil, do it synchronously."
  (let ((default-directory counsel--git-grep-dir)
        (cmd
         (concat
          (format
           (replace-regexp-in-string
            "--full-name" "-c"
            counsel-git-grep-cmd)
           ;; "git grep -i -c '%s'"
           (replace-regexp-in-string
            "-" "\\\\-"
            (replace-regexp-in-string "'" "''" regex)))
          " | sed 's/.*:\\(.*\\)/\\1/g' | awk '{s+=$1} END {print s}'"))
        (counsel-ggc-process " *counsel-gg-count*"))
    (if no-async
        (string-to-number (shell-command-to-string cmd))
      (let ((proc (get-process counsel-ggc-process))
            (buff (get-buffer counsel-ggc-process)))
        (when proc
          (delete-process proc))
        (when buff
          (kill-buffer buff))
        (setq proc (start-process-shell-command
                    counsel-ggc-process
                    counsel-ggc-process
                    cmd))
        (set-process-sentinel
         proc
         #'(lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (setq ivy--full-length (string-to-number (buffer-string))))
               (when (= 0 (cl-incf counsel-gg-state))
                 (ivy--exhibit)))))))))

(defun counsel--M-x-transformer (cand-pair)
  "Add a binding to CAND-PAIR cdr if the car is bound in the current window.
CAND-PAIR is (command-name . extra-info)."
  (let* ((command-name (car cand-pair))
         (extra-info (cdr cand-pair))
         (binding (substitute-command-keys (format "\\[%s]" command-name))))
    (setq binding (replace-regexp-in-string "C-x 6" "<f2>" binding))
    (if (string-match "^M-x" binding)
        cand-pair
      (cons command-name
            (if extra-info
                (format " %s (%s)" extra-info (propertize binding 'face 'font-lock-keyword-face))
              (format " (%s)" (propertize binding 'face 'font-lock-keyword-face)))))))

(defvar smex-initialized-p)
(defvar smex-ido-cache)
(declare-function smex-initialize "ext:smex")
(declare-function smex-detect-new-commands "ext:smex")
(declare-function smex-update "ext:smex")
(declare-function smex-rank "ext:smex")

(defun counsel--M-x-prompt ()
  "M-x plus the string representation of `current-prefix-arg'."
  (if (not current-prefix-arg)
      "M-x "
    (concat
     (if (eq current-prefix-arg '-)
         "- "
       (if (integerp current-prefix-arg)
           (format "%d " current-prefix-arg)
         (if (= (car current-prefix-arg) 4)
             "C-u "
           (format "%d " (car current-prefix-arg)))))
     "M-x ")))

;;;###autoload
(defun counsel-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer."
  (interactive)
  (unless initial-input
    (setq initial-input (cdr (assoc this-command
                                    ivy-initial-inputs-alist))))
  (let* ((store ivy-format-function)
         (ivy-format-function
          (lambda (cand-pairs)
            (funcall
             store
             (with-ivy-window
               (mapcar #'counsel--M-x-transformer cand-pairs)))))
         (cands obarray)
         (pred 'commandp)
         (sort t))
    (when (require 'smex nil 'noerror)
      (unless smex-initialized-p
        (smex-initialize))
      (smex-detect-new-commands)
      (smex-update)
      (setq cands smex-ido-cache)
      (setq pred nil)
      (setq sort nil))
    (ivy-read (counsel--M-x-prompt) cands
              :predicate pred
              :require-match t
              :history 'extended-command-history
              :action
              (lambda (cmd)
                (when (featurep 'smex)
                  (smex-rank (intern cmd)))
                (let ((prefix-arg current-prefix-arg)
                      (ivy-format-function store))
                  (command-execute (intern cmd) 'record)))
              :sort sort
              :keymap counsel-describe-map
              :initial-input initial-input
              :caller 'counsel-M-x)))

(declare-function powerline-reset "ext:powerline")

(defun counsel--load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x))
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x)))

;;;###autoload
(defun counsel-load-theme ()
  "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'symbol-name
                    (custom-available-themes))
            :action #'counsel--load-theme-action))

(defvar rhythmbox-library)
(declare-function rhythmbox-load-library "ext:helm-rhythmbox")
(declare-function dbus-call-method "dbus")
(declare-function rhythmbox-song-uri "ext:helm-rhythmbox")
(declare-function helm-rhythmbox-candidates "ext:helm-rhythmbox")

(defun counsel-rhythmbox-enqueue-song (song)
  "Let Rhythmbox enqueue SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dbus-call-method :session service path interface
                      "AddToQueue" (rhythmbox-song-uri song))))

(defvar counsel-rhythmbox-history nil
  "History for `counsel-rhythmbox'.")

;;;###autoload
(defun counsel-rhythmbox ()
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive)
  (unless (require 'helm-rhythmbox nil t)
    (error "Please install `helm-rhythmbox'"))
  (unless rhythmbox-library
    (rhythmbox-load-library)
    (while (null rhythmbox-library)
      (sit-for 0.1)))
  (ivy-read "Rhythmbox: "
            (helm-rhythmbox-candidates)
            :history 'counsel-rhythmbox-history
            :action
            '(1
              ("p" helm-rhythmbox-play-song "Play song")
              ("e" counsel-rhythmbox-enqueue-song "Enqueue song"))
            :caller 'counsel-rhythmbox))

(defvar counsel-org-tags nil
  "Store the current list of tags.")

(defvar org-outline-regexp)
(defvar org-indent-mode)
(defvar org-indent-indentation-per-level)
(defvar org-tags-column)
(declare-function org-get-tags-string "org")
(declare-function org-move-to-column "org-compat")

(defun counsel-org-change-tags (tags)
  (let ((current (org-get-tags-string))
        (col (current-column))
        level)
    ;; Insert new tags at the correct column
    (beginning-of-line 1)
    (setq level (or (and (looking-at org-outline-regexp)
                         (- (match-end 0) (point) 1))
                    1))
    (cond
      ((and (equal current "") (equal tags "")))
      ((re-search-forward
        (concat "\\([ \t]*" (regexp-quote current) "\\)[ \t]*$")
        (point-at-eol) t)
       (if (equal tags "")
           (delete-region
            (match-beginning 0)
            (match-end 0))
         (goto-char (match-beginning 0))
         (let* ((c0 (current-column))
                ;; compute offset for the case of org-indent-mode active
                (di (if (bound-and-true-p org-indent-mode)
                        (* (1- org-indent-indentation-per-level) (1- level))
                      0))
                (p0 (if (equal (char-before) ?*) (1+ (point)) (point)))
                (tc (+ org-tags-column (if (> org-tags-column 0) (- di) di)))
                (c1 (max (1+ c0) (if (> tc 0) tc (- (- tc) (string-width tags)))))
                (rpl (concat (make-string (max 0 (- c1 c0)) ?\ ) tags)))
           (replace-match rpl t t)
           (and c0 indent-tabs-mode (tabify p0 (point)))
           tags)))
      (t (error "Tags alignment failed")))
    (org-move-to-column col)))

(defun counsel-org--set-tags ()
  (counsel-org-change-tags
   (if counsel-org-tags
       (format ":%s:"
               (mapconcat #'identity counsel-org-tags ":"))
     "")))

(defvar org-agenda-bulk-marked-entries)

(declare-function org-get-at-bol "org")
(declare-function org-agenda-error "org-agenda")

(defun counsel-org-tag-action (x)
  (if (member x counsel-org-tags)
      (progn
        (setq counsel-org-tags (delete x counsel-org-tags)))
    (unless (equal x "")
      (setq counsel-org-tags (append counsel-org-tags (list x)))
      (unless (member x ivy--all-candidates)
        (setq ivy--all-candidates (append ivy--all-candidates (list x))))))
  (let ((prompt (counsel-org-tag-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "%-4d " prompt)))
  (cond ((memq this-command '(ivy-done
                              ivy-alt-done
                              ivy-immediate-done))
         (if (eq major-mode 'org-agenda-mode)
             (if (null org-agenda-bulk-marked-entries)
                 (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                                     (org-agenda-error))))
                   (with-current-buffer (marker-buffer hdmarker)
                     (goto-char hdmarker)
                     (counsel-org--set-tags)))
               (let ((add-tags (copy-sequence counsel-org-tags)))
                 (dolist (m org-agenda-bulk-marked-entries)
                   (with-current-buffer (marker-buffer m)
                     (save-excursion
                       (goto-char m)
                       (setq counsel-org-tags
                             (delete-dups
                              (append (split-string (org-get-tags-string) ":" t)
                                      add-tags)))
                       (counsel-org--set-tags))))))
           (counsel-org--set-tags)))
        ((eq this-command 'ivy-call)
         (delete-minibuffer-contents))))

(defun counsel-org-tag-prompt ()
  (format "Tags (%s): "
          (mapconcat #'identity counsel-org-tags ", ")))

(defvar org-setting-tags)
(defvar org-last-tags-completion-table)
(defvar org-tag-persistent-alist)
(defvar org-tag-alist)
(defvar org-complete-tags-always-offer-all-agenda-tags)

(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-get-buffer-tags "org")
(declare-function org-global-tags-completion-table "org")
(declare-function org-agenda-files "org")
(declare-function org-agenda-set-tags "org-agenda")

;;;###autoload
(defun counsel-org-tag ()
  "Add or remove tags in org-mode."
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags
                    (split-string (org-get-tags-string) ":" t)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (split-string (org-get-tags-string) ":" t)))
    (let ((org-setting-tags t)
          (org-last-tags-completion-table
           (append org-tag-persistent-alist
                   (or org-tag-alist (org-get-buffer-tags))
                   (and
                    (or org-complete-tags-always-offer-all-agenda-tags
                        (eq major-mode 'org-agenda-mode))
                    (org-global-tags-completion-table
                     (org-agenda-files))))))
      (ivy-read (counsel-org-tag-prompt)
                (lambda (str &rest _unused)
                  (delete-dups
                   (all-completions str 'org-tags-completion-function)))
                :history 'org-tags-history
                :action 'counsel-org-tag-action))))

;;;###autoload
(defun counsel-org-tag-agenda ()
  "Set tags for the current agenda item."
  (interactive)
  (let ((store (symbol-function 'org-set-tags)))
    (unwind-protect
         (progn
           (fset 'org-set-tags
                 (symbol-function 'counsel-org-tag))
           (org-agenda-set-tags nil nil))
      (fset 'org-set-tags store))))

(defun counsel-ag-function (string &optional _pred &rest _unused)
  "Grep in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command
       (format "ag --vimgrep %S" regex))
      nil)))

;;;###autoload
(defun counsel-ag (&optional initial-input initial-directory)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read "ag: " 'counsel-ag-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))))

;;;###autoload
(defun counsel-grep ()
  "Grep for a string in the current file."
  (interactive)
  (setq counsel--git-grep-dir (buffer-file-name))
  (ivy-read "grep: " 'counsel-grep-function
            :dynamic-collection t
            :preselect (format "%d:%s"
                               (line-number-at-pos)
                               (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position)))
            :history 'counsel-git-grep-history
            :update-fn (lambda ()
                         (counsel-grep-action ivy--current))
            :action #'counsel-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-grep))

(defun counsel-grep-function (string &optional _pred &rest _unused)
  "Grep in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command
       (format "grep -nP --ignore-case '%s' %s" regex counsel--git-grep-dir))
      nil)))

(defun counsel-grep-action (x)
  (when (string-match "\\`\\([0-9]+\\):\\(.*\\)\\'" x)
    (with-ivy-window
      (let ((file-name counsel--git-grep-dir)
            (line-number (match-string-no-properties 1 x)))
        (find-file file-name)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))
        (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(defun counsel-recoll-function (string &optional _pred &rest _unused)
  "Grep in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "recoll -t -b '%s'" string))
    nil))

;; This command uses the recollq command line tool that comes together
;; with the recoll (the document indexing database) source:
;;     http://www.lesbonscomptes.com/recoll/download.html
;; You need to build it yourself (together with recoll):
;;     cd ./query && make && sudo cp recollq /usr/local/bin
;; You can try the GUI version of recoll with:
;;     sudo apt-get install recoll
;; Unfortunately, that does not install recollq.
(defun counsel-recoll (&optional initial-input)
  "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "recoll: " 'counsel-recoll-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match "file://\\(.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))))

(defvar tmm-km-list nil)
(declare-function tmm-get-keymap "tmm")
(declare-function tmm--completion-table "tmm")
(declare-function tmm-get-keybind "tmm")

(defun counsel-tmm-prompt (menu)
  "Select and call an item from the MENU keymap."
  (let (out
        choice
        chosen-string)
    (setq tmm-km-list nil)
    (map-keymap (lambda (k v) (tmm-get-keymap (cons k v))) menu)
    (setq tmm-km-list (nreverse tmm-km-list))
    (setq out (ivy-read "Menu bar: " (tmm--completion-table tmm-km-list)
                        :require-match t
                        :sort nil))
    (setq choice (cdr (assoc out tmm-km-list)))
    (setq chosen-string (car choice))
    (setq choice (cdr choice))
    (cond ((keymapp choice)
           (counsel-tmm-prompt choice))
          ((and choice chosen-string)
           (setq last-command-event chosen-string)
           (call-interactively choice)))))

(defun counsel-tmm ()
  "Text-mode emulation of looking and choosing from a menubar."
  (interactive)
  (require 'tmm)
  (run-hooks 'menu-bar-update-hook)
  (counsel-tmm-prompt (tmm-get-keybind [menu-bar])))

(defcustom counsel-yank-pop-truncate nil
  "When non-nil, truncate the display of long strings."
  :group 'ivy)

;;;###autoload
(defun counsel-yank-pop ()
  "Ivy replacement for `yank-pop'."
  (interactive)
  (if (eq last-command 'yank)
      (progn
        (setq counsel-completion-end (point))
        (setq counsel-completion-beg
              (save-excursion
                (search-backward (car kill-ring))
                (point))))
    (setq counsel-completion-beg (point))
    (setq counsel-completion-end (point)))
  (let ((candidates (cl-remove-if
                     (lambda (s)
                       (or (< (length s) 3)
                           (string-match "\\`[\n[:blank:]]+\\'" s)))
                     (delete-dups kill-ring))))
    (when counsel-yank-pop-truncate
      (setq candidates
            (mapcar (lambda (s)
                      (if (string-match "\\`\\(.*\n.*\n.*\n.*\\)\n" s)
                          (progn
                            (let ((s (copy-sequence s)))
                              (put-text-property
                               (match-end 1)
                               (length s)
                               'display
                               " [...]"
                               s)
                              s))
                        s))
                    candidates)))
    (ivy-read "kill-ring: " candidates
              :action 'counsel-yank-pop-action)))

(defun counsel-yank-pop-action (s)
  "Insert S into the buffer, overwriting the previous yank."
  (with-ivy-window
    (delete-region counsel-completion-beg
                   counsel-completion-end)
    (insert (substring-no-properties s))
    (setq counsel-completion-end (point))))

(provide 'counsel)

;;; counsel.el ends here
