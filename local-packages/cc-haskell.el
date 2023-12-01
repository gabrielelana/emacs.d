;;; cc-haskell.el --- Haskell related functions -*- lexical-binding: t -*-

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

;; Haskell related functions

;;; Code:

(require 'haskell)

(defun cc/haskell-interactive-start ()
  "Start an interactive session loading current buffer.

Whenever the buffer is saved it will be also reloaded in the
current interactive session."
  (interactive)
  (save-excursion
    (haskell-interactive-bring)
    (setq-local global-hl-line-mode nil))
  (haskell-process-load-file)
  (add-hook 'after-save-hook 'haskell-process-reload nil t))

(provide 'cc-haskell)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-haskell.el ends here
