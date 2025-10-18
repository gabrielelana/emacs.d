;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: (_)
;; Homepage: http://github.com/gabrielelana/_
;; Keywords: _

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

;; Early initialization

;;; Code:

;; Disable package.el completely in favor of straight.el
(setq package-enable-at-startup nil)
(setq package-archives nil)
(setq package-quickstart nil)
;; Prevent package.el from loading packages automatically
(setq package-load-list '((all nil)))
;; Completely disable package.el initialization
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(setq package-gnupghome-dir (expand-file-name "elpa/gnupg" user-emacs-directory))
;; Prevent package.el from auto-loading
(defvar package--initialized t)

;; Suppress lexical-binding warnings for external packages
(defun cc/suppress-lexical-binding-warnings (original-function &rest args)
  "Suppress `lexical-binding' warnings for external packages.

To be used as an advice around an ORIGINAL-FUNCTION with ARGS."
  (let ((message (nth 1 args)))
    (if (and (stringp message)
             (string-match-p "lexical-binding" message)
             (or (string-match-p "/straight/build/" message)
                 (string-match-p "/agda2.*\\.el" message)
                 (string-match-p "/.mc-lists\\.el" message)
                 (string-match-p "/elpa/" message)))
        nil
      (apply original-function args))))

(advice-add 'display-warning :around #'cc/suppress-lexical-binding-warnings)

(provide 'early-init)

;; Local Variables:
;; coding: utf-8
;; End:
;;; early-init.el ends here
