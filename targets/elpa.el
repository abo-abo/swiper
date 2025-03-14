;;; targets/elpa.el --- Optional Ivy dependencies -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Free Software Foundation, Inc.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'package)

(defvar ivy--elpa-stable
  (or (getenv "ELPA_STABLE")
      (getenv "MELPA_STABLE"))
  "Non-nil if GNU ELPA should be used instead of GNU-devel ELPA.")

(defvar ivy--elpa-dir "~/.elpa"
  "Parent directory for installing optional dependencies.")

(defvar ivy--elpa-user-dir
  (expand-file-name
   (format "%s%s/elpa" emacs-version (if ivy--elpa-stable "-stable" ""))
   ivy--elpa-dir)
  "Instance-specific value for `package-user-dir'.")

;; FIXME: Switch to `gnu' once https://bugs.gnu.org/76264 is resolved.
(defvar ivy--elpa-archive 'melpa
  "Preferred ELPA archive; keys `ivy--elpa-archives'.")

(defvar ivy--elpa-archives
  ;; Check default value rather than `gnutls-available-p': even when
  ;; the latter is non-nil my Emacs 24.5 fails with https://.
  (let ((s (if (string-prefix-p "https" (cdar package-archives)) "s" "")))
    `((gnu
       ("gnu" . ,(format "http%s://elpa.gnu.org/%s/"
                         s (if ivy--elpa-stable "packages" "devel")))
       ;; For `wgrep'.
       ("nongnu" . ,(format "http%s://elpa.nongnu.org/nongnu%s/"
                            s (if ivy--elpa-stable "" "-devel"))))
      (melpa
       ("melpa" . ,(format "https://%smelpa.org/packages/"
                           (if ivy--elpa-stable "stable." ""))))))
  "Map ELPA archive symbols to their `package-archives'.")

(defvar ivy--elpa-pkgs
  '(avy
    hydra
    wgrep)
  "List of optional (or development) package dependencies.")

(defvar ivy--elpa-activated nil
  "Non-nil if `ivy--elpa-activate' succeeded.")

(defvar ivy--elpa-refreshed nil
  "Non-nil if `ivy--elpa-refresh' succeeded.")

(defun ivy--elpa-activate ()
  "Ensure packages under `ivy--elpa-dir' are activated."
  (unless ivy--elpa-activated
    (setq package-user-dir ivy--elpa-user-dir)
    (let ((msg (format "Activating packages in %s" package-user-dir)))
      (message "%s..." msg)
      (package-initialize)
      (message "%s...done" msg))
    (setq ivy--elpa-activated t)))

(defun ivy--elpa-refresh ()
  "Ensure archive contents are refreshed."
  (defvar gnutls-algorithm-priority)
  (unless ivy--elpa-refreshed
    (let ((archive ivy--elpa-archive))
      (setq package-archives (cdr (assq archive ivy--elpa-archives)))
      (and (eq archive 'melpa)
           (version< emacs-version "26.3")
           ;; See https://melpa.org/#/getting-started.
           (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))
    (package-refresh-contents)
    (setq ivy--elpa-refreshed (and package-archive-contents t))))

(defun ivy--elpa-install-pkg (pkg)
  "Compatibility shim for Emacs 25 `package-install'."
  (condition-case nil
      (package-install pkg t)
    (wrong-number-of-arguments
     (package-install pkg))))

(defun ivy--elpa-install ()
  "Install any missing `ivy--elpa-pkgs' with demoted errors."
  (ivy--elpa-activate)
  (ivy--elpa-refresh)
  (let ((msg-all (format "Installing in %s" package-user-dir))
        any-ok any-err)
    (message "%s..." msg-all)
    (dolist (pkg ivy--elpa-pkgs)
      (unless (package-installed-p pkg)
        (let ((msg (format "Installing %s" pkg))
              err)
          (message "%s..." msg)
          (condition-case-unless-debug e
              (ivy--elpa-install-pkg pkg)
            (error (message "Error: %s" (error-message-string e))
                   (message "%s...INCOMPLETE" msg)
                   (setq any-err t)
                   (setq err e)))
          (unless err
            (message "%s...done" msg)
            (setq any-ok t)))))
    (message "%s...%s" msg-all
             (cond (any-err "INCOMPLETE")
                   (any-ok "done")
                   (t "already present")))))

;; TODO: upgrade-deps target?

(provide 'targets/elpa)
