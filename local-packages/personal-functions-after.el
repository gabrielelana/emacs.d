;;; personal-functions-after.el --- summary -*- lexical-binding: t -*-

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

;; summary

;;; Code:

(require 'vterm)
(require 'projectile)

(defun cc/project-vterm-other-window (&optional terminal-name)
  "Create a terminal in other window named *{PROJECT-NAME}-{TERMINAL-NAME}*."
  (interactive "sName: ")
  (if (not (projectile-project-p))
      (error "ERROR: seems like you are not currently in a project")
    (let* ((-project-name (projectile-project-name))
           (-project-root-directory (projectile-project-root))
           (-current-directory default-directory)
           (-current-vterm-buffer-name vterm-buffer-name)
           (-terminal-buffer-name (format "*%s-%s*" -project-name terminal-name)))
      (unwind-protect
          (progn
            (setq default-directory -project-root-directory
                  vterm-buffer-name -terminal-buffer-name)
            (vterm-other-window -terminal-buffer-name))
        (setq default-directory -current-directory
              vterm-buffer-name -current-vterm-buffer-name)))))

(provide 'personal-functions-after)

;; Local Variables:
;; coding: utf-8
;; End:
;;; personal-functions-after.el ends here
