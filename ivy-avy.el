;;; ivy-avy.el --- Avy integration for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.13.5
;; Package-Requires: ((ivy "0.13.5") (avy "0.5.0"))
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

;; *N.B.:* This package has been absorbed, and is therefore made
;; obsolete, by the `ivy' package, version 0.13.5.
;;
;; If you maintain a package that depends on `ivy-avy', then you
;; should change that to instead depend on `ivy' version 0.13.5, and
;; remove all references to `ivy-avy'.
;;
;; If you use any packages that depend on `ivy-avy', either directly
;; or indirectly, then you will have to wait until all of them have
;; transitioned away from it before you can uninstall it.

;;; Code:

(require 'avy)
(require 'ivy)

(eval-and-compile
  (let ((msg "Package ivy-avy is obsolete; use ivy 0.13.5 instead"))
    (if (and noninteractive (fboundp 'byte-compile-warn))
        (byte-compile-warn msg)
      (message "%s" msg))))

(provide 'ivy-avy)

;;; ivy-avy.el ends here
