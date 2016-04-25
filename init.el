(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/org-mode/lisp")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/swiper/doc")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/eclipse-theme")
(add-to-list 'load-path "~/Dropbox/source/site-lisp/git/htmlize")
(require 'ivy-ox)
(require 'org)
(require 'eclipse-theme)
(require 'htmlize)

(setq org-confirm-babel-evaluate nil)

(defun org-export-get-reference (_datum _info)
  "orgheadline")

(defun doexport ()
  (interactive)
  (org-html-export-to-html)
  (kill-emacs))

(defun add-hlines ()
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^\\*\\{3,\\}" nil t)
    (cond ((save-excursion
             (beginning-of-line 0)
             (looking-at "\\*\\{2,\\}")))
          ((looking-back "-----\n\\*+"))
          (t
           (end-of-line 0)
           (insert "\n-----")
           (forward-line 2)))))

(defun add-custom-id ()
  (interactive)
  (goto-char (point-min))
  (when (looking-at "^\\* \\([0-9]+\\.[0-9]+\\.[0-9]+\\)")
    (let ((release (replace-regexp-in-string
                    "\\." "-" (match-string-no-properties 1)))
          (unique-ids nil)
          (chapter nil))
      (while (re-search-forward "^\\*\\{2,3\\}" nil t)
        (let* ((el (org-element-at-point))
               (name (org-element-property :raw-value el))
               (id (concat release
                           (if chapter
                               (concat "-" chapter "-")
                             "-")
                           (mapconcat (lambda (s)
                                        (replace-regexp-in-string "[=~'()<>\\:%\"]" "" s))
                                      (split-string (downcase name) " ")
                                      "-"))))
          (when (< (org-element-property :level el) 4)
            (cond ((string= name "Fixes")
                   (setq chapter "fx"))
                  ((string= name "New Features")
                   (setq chapter "nf"))
                  ((string= name "New Commands")
                   (setq chapter "nc")))
            (if (member id unique-ids)
                (message "warning: duplicate id: %s" id)
              (push id unique-ids))
            (org-set-property "CUSTOM_ID" id)))))))


