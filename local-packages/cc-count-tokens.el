;;; cc-count-tokens.el --- summary -*- lexical-binding: t -*-

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

;; TODO: when in `Dired' count tokens of currently marked files or current file
;; if none are marked

;;; Code:

(require 'cl-lib)
(require 'dired)

(defun cc/count-tokens--original-count-words-message ()
  "Return the message produced by interactively calling `count-words'."
  (let (result)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq result (apply #'format format-string args)))))
      (call-interactively #'count-words))
    result))

(defun cc/count-tokens--binary-file-p (file)
  "Return non-nil if FILE appears to be a binary file.
Checks the first 4096 bytes for null bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 4096)
    (seq-contains-p (buffer-string) 0)))

(defun cc/count-tokens--file-token-count (file)
  "Return the token count for FILE.
FILE must be an absolute path."
  (unless (file-name-absolute-p file)
    (user-error "FILE must be an absolute path: %s" file))
  (when (cc/count-tokens--binary-file-p file)
    (user-error "Cannot count tokens in binary file: %s" file))
  (with-temp-buffer
    (insert-file-contents file)
    (cc/count-tokens--text-token-count
     (buffer-substring-no-properties (point-min) (point-max)))))

(defun cc/count-tokens--text-token-count (text)
  "Return the number of tokens in TEXT using the external `token-count' tool.
Signal a `user-error' if the tool is unavailable or fails."
  (unless (executable-find "token-count")
    (user-error "Required executable not found: token-count"))
  (with-temp-buffer
    (insert text)
    (let ((status (call-process-region
                   (point-min) (point-max)
                   "token-count"
                   t
                   '(t nil)
                   nil)))
      (unless (and (integerp status) (zerop status))
        (user-error "External executable failed: token-count"))
      (string-to-number (string-trim (buffer-string))))))

(defun cc/count-tokens--format-message (tokens)
  "Return a formatted message string for TOKENS count."
  (let ((k-tokens (/ tokens 1000)))
    (if (eq k-tokens 0)
        (format "Tokens: %d" tokens)
      (format "Tokens: %dk (%d)" k-tokens tokens))))

;;;###autoload
(defun cc/count-tokens-dwim ()
  "Count tokens in the most sensible way for the current context.
In Dired, count tokens for marked files or file at point.
Otherwise, count tokens in the current buffer or region."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (cc/count-tokens-dired)
    (call-interactively #'cc/count-tokens-buffer)))

;;;###autoload
(defun cc/count-tokens-buffer (&optional begin end)
  "Count tokens in region or buffer and display a message.
When the region is active, count in the region from BEGIN to END.
Otherwise count in the current buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (text (buffer-substring-no-properties begin end))
         (tokens (cc/count-tokens--text-token-count text)))
    (message (cc/count-tokens--format-message tokens))))

;;;###autoload
(defun cc/count-tokens-dired ()
  "Count tokens for marked files or file at point in Dired.
If there are marked files, count tokens for all of them and report
the total.  Otherwise count tokens for the file at point."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a Dired buffer"))
  (let* ((files (dired-get-marked-files nil nil nil t))
         ;; dired-get-marked-files with 4th arg t returns (t file) for
         ;; a single file at point when nothing is marked
         (files (if (eq (car files) t) (cdr files) files))
         (tokens (cl-loop for file in files
                          sum (cc/count-tokens--file-token-count file))))
    (message (cc/count-tokens--format-message tokens))))

;;;###autoload
(defun cc/count-words (&optional begin end)
  "Like `count-words', but also report token count.

When the region is active, count in the region from BEGIN to END.
Otherwise count in the current buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (let* ((begin (or begin (point-min)))
         (end (or end (point-max)))
         (text (buffer-substring-no-properties begin end))
         (tokens (cc/count-tokens--text-token-count text))
         (original-message (cc/count-tokens--original-count-words-message)))
    (message "%s (token%s: %d)"
             (or original-message "Count complete")
             (if (eq tokens 1) "" "s")
             tokens)))

(provide 'cc-count-tokens)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-count-tokens.el ends here
