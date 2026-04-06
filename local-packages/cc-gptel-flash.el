;;; cc-gptel-flash.el --- summary -*- lexical-binding: t -*-

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

;;; TODO: create a tool to read/write the currently visible buffers

(require 'gptel)
(require 'personal-functions)

(defun cc/gptel-flash--quit ()
  "Close the current flash gptel window and kill its buffer."
  (interactive)
  (if (one-window-p)
      (kill-buffer (current-buffer))
    (kill-buffer-and-window)))

(defun cc/gptel-flash--buffer-name ()
  "Return a unique buffer name for a flash gptel session."
  (format "*gptel-flash:%s*" (cc/random-token 6)))

(defun cc/gptel-flash--normalize-tools (tools)
  "Normalize TOOLS to a list of gptel tool objects."
  (mapcar (lambda (tool)
            (if (gptel-tool-p tool)
                tool
              (gptel-get-tool tool)))
          tools))

;;;###autoload
(defun cc/gptel-flash (backend model system-message tools)
  "Open a new temporary gptel buffer with a specific configuration.

Supported configuration are: BACKEND, MODEL, SYSTEM-MESSAGE and TOOLS.

When called from a buffer with an active region, the selected text
is pasted into the new gptel buffer inside an Org source block."
  (let ((selection (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))))
        (lang (if (use-region-p)
                  (string-trim-right (symbol-name major-mode)
                                     "\\(?:-ts\\)?-mode")
                "text")))
    (let ((buffer (gptel (cc/gptel-flash--buffer-name) nil nil t)))
      (with-current-buffer buffer
        (setq-local gptel-backend backend)
        (setq-local gptel-model model)
        (setq-local gptel--system-message system-message)
        (setq-local gptel-tools (cc/gptel-flash--normalize-tools tools))
        (local-set-key (kbd "C-c C-q") #'cc/gptel-flash--quit)
        (when selection
          (goto-char (point-max))
          (unless (bolp) (insert "\n\n"))
          (save-excursion
            (insert "\n\n#+begin_src " lang "\n"
                    selection
                    (if (string-suffix-p "\n" selection) "" "\n")
                    "#+end_src\n\n"))))
      buffer)))

(provide 'cc-gptel-flash)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-gptel-flash.el ends here
