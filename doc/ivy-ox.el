;;; ivy-ox.el --- org-export settings for Ivy

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

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

;;* ox-texinfo
(require 'ox-texinfo)
(require 'subr-x)
(org-export-define-backend 'texinfo
  '((bold . org-texinfo-bold)
    (center-block . org-texinfo-center-block)
    (clock . org-texinfo-clock)
    (code . org-texinfo-kbd)
    (drawer . org-texinfo-drawer)
    (dynamic-block . org-texinfo-dynamic-block)
    (entity . org-texinfo-entity)
    (example-block . org-texinfo-example-block)
    (export-block . org-texinfo-export-block)
    (export-snippet . org-texinfo-export-snippet)
    (fixed-width . org-texinfo-fixed-width)
    (footnote-definition . org-texinfo-footnote-definition)
    (footnote-reference . org-texinfo-footnote-reference)
    (headline . org-texinfo-headline)
    (inline-src-block . org-texinfo-inline-src-block)
    (inlinetask . org-texinfo-inlinetask)
    (italic . org-texinfo-italic)
    (item . org-texinfo-item)
    (keyword . org-texinfo-keyword)
    (line-break . org-texinfo-line-break)
    (link . org-texinfo-link)
    (node-property . org-texinfo-node-property)
    (paragraph . org-texinfo-paragraph)
    (plain-list . org-texinfo-plain-list)
    (plain-text . org-texinfo-plain-text)
    (planning . org-texinfo-planning)
    (property-drawer . org-texinfo-property-drawer)
    (quote-block . org-texinfo-quote-block)
    (radio-target . org-texinfo-radio-target)
    (section . org-texinfo-section)
    (special-block . org-texinfo-special-block)
    (src-block . org-texinfo-src-block)
    (statistics-cookie . org-texinfo-statistics-cookie)
    (subscript . org-texinfo-subscript)
    (superscript . org-texinfo-superscript)
    (table . org-texinfo-table)
    (table-cell . org-texinfo-table-cell)
    (table-row . org-texinfo-table-row)
    (target . org-texinfo-target)
    (template . org-texinfo-template)
    (timestamp . org-texinfo-timestamp)
    (verbatim . org-texinfo-code)
    (verse-block . org-texinfo-verse-block))
  :filters-alist
  '((:filter-headline . org-texinfo--filter-section-blank-lines)
    (:filter-parse-tree . org-texinfo--normalize-headlines)
    (:filter-section . org-texinfo--filter-section-blank-lines))
  :menu-entry
  '(?i "Export to Texinfo"
    ((?t "As TEXI file" org-texinfo-export-to-texinfo)
     (?i "As INFO file" org-texinfo-export-to-info)
     (?o "As INFO file and open"
      (lambda (a s v b)
        (if a (org-texinfo-export-to-info t s v b)
          (org-open-file (org-texinfo-export-to-info nil s v b)))))))
  :options-alist
  '((:texinfo-filename "TEXINFO_FILENAME" nil nil t)
    (:texinfo-class "TEXINFO_CLASS" nil org-texinfo-default-class t)
    (:texinfo-header "TEXINFO_HEADER" nil nil newline)
    (:texinfo-post-header "TEXINFO_POST_HEADER" nil nil newline)
    (:subtitle "SUBTITLE" nil nil parse)
    (:subauthor "SUBAUTHOR" nil nil newline)
    (:texinfo-dircat "TEXINFO_DIR_CATEGORY" nil nil t)
    (:texinfo-dirtitle "TEXINFO_DIR_TITLE" nil nil t)
    (:texinfo-dirdesc "TEXINFO_DIR_DESC" nil nil t)
    (:texinfo-printed-title "TEXINFO_PRINTED_TITLE" nil nil t)
    ;; Other variables.
    (:texinfo-classes nil nil org-texinfo-classes)
    (:texinfo-format-headline-function nil nil org-texinfo-format-headline-function)
    (:texinfo-node-description-column nil nil org-texinfo-node-description-column)
    (:texinfo-active-timestamp-format nil nil org-texinfo-active-timestamp-format)
    (:texinfo-inactive-timestamp-format nil nil org-texinfo-inactive-timestamp-format)
    (:texinfo-diary-timestamp-format nil nil org-texinfo-diary-timestamp-format)
    (:texinfo-link-with-unknown-path-format nil nil org-texinfo-link-with-unknown-path-format)
    (:texinfo-tables-verbatim nil nil org-texinfo-tables-verbatim)
    (:texinfo-table-scientific-notation nil nil org-texinfo-table-scientific-notation)
    (:texinfo-def-table-markup nil nil org-texinfo-def-table-markup)
    (:texinfo-text-markup-alist nil nil org-texinfo-text-markup-alist)
    (:texinfo-format-drawer-function nil nil org-texinfo-format-drawer-function)
    (:texinfo-format-inlinetask-function nil nil org-texinfo-format-inlinetask-function)))

(defun org-texinfo-kbd (code _contents _info)
  "Transcode a CODE object from Org to Texinfo."
  (format "@kbd{%s}" (org-element-property :value code)))

(defun org-texinfo-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Texinfo.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  (let* ((attr (org-export-read-attribute :attr_texinfo plain-list))
         (indic (or (plist-get attr :indic)
                    (plist-get info :texinfo-def-table-markup)))
         (table-type (plist-get attr :table-type))
         (type (org-element-property :type plain-list))
         (list-type (cond
                      ((eq type 'ordered) "enumerate")
                      ((eq type 'unordered) "itemize")
                      ((member table-type '("ftable" "vtable")) table-type)
                      (t "table"))))
    (if (equal list-type "table")
        (mapconcat (lambda (s)
                     (cond ((string-match "\\`User Option @code{\\(.*\\)}$" s)
                            (format "@defopt %s\n%s\n@end defopt\n"
                                    (match-string-no-properties 1 s)
                                    (string-trim
                                     (substring s (1+ (match-end 1))))))
                           ((string-match "\\(.*\\)$" s)
                            (let* ((line (match-string 1 s))
                                   (body (string-trim
                                          (substring s (1+ (match-end 1)))))
                                   (symbol-index
                                    (if (string-match "@code{\\(\\(?:ivy\\|swiper\\|counsel\\)-[^}]+\\)}" line)
                                        (format "@vindex %s\n" (match-string 1 line))
                                      ""))
                                   (key-index
                                    (apply #'concat
                                           (mapcar
                                            (lambda (s)
                                              (format "@kindex %s\n" s))
                                            (iox-extract-kbd line)))))
                              (format "@subsubheading %s\n%s@indentedblock\n%s\n@end indentedblock"
                                      line
                                      (concat symbol-index
                                              key-index)
                                      body)))
                           (t
                            (concat "@subsubheading " s))))
                   (split-string (substring-no-properties contents) "^@item " t)
                   "\n")
      (format "@%s\n%s@end %s"
              (if (eq type 'descriptive) (concat list-type " " indic) list-type)
              contents
              list-type))))

(defun iox-extract-kbd (str)
  (let ((start 0)
        res)
    (while (string-match "@kbd{\\([^}]+\\)}" str start)
      (setq start (match-end 0))
      (push (match-string 1 str) res))
    (nreverse res)))

;;* ox-html
(require 'ox-html)
(setq org-html-validation-link nil)
(setq org-html-postamble nil)
(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<kbd>%s</kbd>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<code>%s</code>")))
(setq org-html-style-default nil)

(defvar ivy-info-dir (file-name-directory
                      (or load-file-name
                          (buffer-file-name))))

(defun info-ivy ()
  (interactive)
  (let ((buf (get-buffer "*info*")))
    (when buf
      (kill-buffer buf)))
  (Info-find-node
   (expand-file-name "ivy.info" ivy-info-dir)
   "Top"))

(provide 'ivy-ox)
