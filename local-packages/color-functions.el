;;; color-functions.el --- Manipulate colors -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>

;; This file is not part of GNU Emacs

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

;; Manipulate colors

;;; Code:

(defun cc/color-lerp (a b i)
  "Linear interpolation between two colors.

Linear interpolation between color A and B in HEX format.

Parameter I is a float between 0 and 1, 0 means the result color
will be A-RGB, and 1 means the result color will be B-RGB."
  (require 'color)
  (let ((a-rgb (color-name-to-rgb a))
        (b-rgb (color-name-to-rgb b)))
    (pcase-let ((`(,a-r ,a-g ,a-b) a-rgb)
                (`(,b-r ,b-g ,b-b) b-rgb))
      (cc/color-rgb-to-hex
       (list
        (+ a-r (* (- b-r a-r) i))
        (+ a-g (* (- b-g a-g) i))
        (+ a-b (* (- b-b a-b) i)))))))

(defun cc/color-to-rgb (color)
  "Convert COLOR from whatever format to RGB."
  (or (cc/color-hex-to-rgb color)
      (color-name-to-rgb color)
      color))

(defun cc/color-hex-to-rgb (color)
  "Convert COLOR from HEX to RGB format."
  (when (string-match "#\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)" color)
    (list
     (string-to-number (match-string 1 color))
     (string-to-number (match-string 2 color))
     (string-to-number (match-string 3 color)))))

(defun cc/color-name-to-rgb (color)
  "Convert COLOR from its name to RGB format."
  (color-values color))

(defun cc/color-rgb-to-hex (color)
  "Convert COLOR from list of RGB to RGB string format."
  (format "#%02x%02x%02x"
          (nth 0 color)
          (nth 1 color)
          (nth 2 color)))

(defun cc/color-rgb-lighter (color-rgb percent)
  "Make COLOR-RGB lighter of PERCENT."
  (mapcar (lambda (c) (/ c (/ (- 100 percent) 100.0))) color-rgb))

(defun cc/color-rgb-darker (color-rgb percent)
  "Make COLOR-RGB darker of PERCENT."
  (mapcar (lambda (c) (/ c (/ percent 100.0))) color-rgb))

(defun cc/color-lighter (color percent)
  "Make COLOR lighter of PERCENT."
  (cc/color-rgb-to-hex (cc/color-rgb-lighter (cc/color-to-rgb color) percent)))

(defun cc/color-darker (color percent)
  "Make COLOR darker of PERCENT."
  (cc/color-rgb-to-hex (cc/color-rgb-darker (cc/color-to-rgb color) percent)))

(provide 'color-functions)

;; Local Variables:
;; coding: utf-8
;; End:
;;; color-functions.el ends here
