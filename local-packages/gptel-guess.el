;;; gptel-guess.el --- Guess code based on context -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: (gptel)
;; Homepage: http://github.com/gabrielelana/emacs.d
;; Keywords: Gptel, AI, LLM

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

;; Guess code based on context

;;; Code:

(require 'gptel)

(defconst cc/--gptel-guess-system-message
  "You are an experienced coder familiar with many programming languages.
You are here to write code and only that. Reply only using code without
any markdown formatting, code fences, or explanations."
  "System message for coding assistant.")

(defconst cc/--gptel-guess-next-line-directive
  "Given the code so far and especially the last few lines, do you identify
a pattern? If so then write only the next line which follow the pattern."
  "Directive to guess the next line to follow the pattern.")

(defconst cc/--gptel-guess-next-n-lines-directive
  "Given the code so far and especially the last few lines, do you identify
a pattern? If so then write exactly %d lines that follow the pattern."
  "Directive to guess the next N lines to follow the pattern.")

(defconst cc/--gptel-guess-complete-pattern-directive
  "Given the code so far and especially the last few lines, do you identify a
pattern? If so then write all the lines needed to complete the pattern.
In any case do not write more than 20 lines of code."
  "Directive to guess the next lines to comple the pattern.")

(defvar cc/--gptel-guess-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'cc/gptel-guess-next-line)
    map)
  "Keymap for repeating gptel-guess-next-line with `n'.")

(defun cc/--gptel-cleanup-response (response)
  "Remove code fence wrapping from RESPONSE if present."
  (if (string-match "^```[a-zA-Z]*\n\\(\\(?:.\\|\n\\)*?\\)\n```$" (string-trim response))
      (match-string 1 response)
    response))

(defun cc/gptel-guess-next-line ()
  "Guess and insert the next line after point using gptel."
  (interactive)
  (let* ((context (buffer-substring-no-properties (point-min) (point)))
         (prompt (concat cc/--gptel-guess-next-line-directive "\n\nCode so far:\n" context))
         (insert-point (point))
         (target-buffer (current-buffer)))
    (message "gptel-guess: Guessing next line... (press 'n' to repeat)")
    (gptel-request prompt
      :system cc/--gptel-guess-system-message
      :callback
      (lambda (response _info)
        (if (not response)
            (message "gptel-guess: Failed to get response")
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (goto-char insert-point)
              (unless (bolp)
                (end-of-line)
                (newline))
              (let ((cleaned-response (cc/--gptel-cleanup-response response))
                    (start-pos (point)))
                (insert cleaned-response)
                (unless (string-suffix-p "\n" cleaned-response)
                  (newline))
                (let ((end-pos (point)))
                  (indent-region start-pos end-pos)
                  (goto-char end-pos)))
              (set-transient-map cc/--gptel-guess-repeat-map t
                                 (lambda () (message "gptel-guess: Ready (press 'n' to repeat, any other key to exit)")))
              (message "gptel-guess: Done (press 'n' to do it again)"))))))))

(defun cc/gptel-guess-next-n-lines (n)
  "Guess and insert the next N lines after point using gptel.
If called interactively without a prefix argument, prompt for N."
  (interactive
   (list (if current-prefix-arg
             (prefix-numeric-value current-prefix-arg)
           (read-number "Lines: " 1))))
  (let* ((context (buffer-substring-no-properties (point-min) (point)))
         (prompt (concat (format cc/--gptel-guess-next-n-lines-directive n)
                         "\n\nCode so far:\n" context))
         (insert-point (point))
         (target-buffer (current-buffer)))
    (message "gptel-guess: Guessing next %d line%s..." n (if (= n 1) "" "s"))
    (gptel-request
        prompt
      :system cc/--gptel-guess-system-message
      :callback
      (lambda (response _info)
        (if (not response)
            (message "gptel-guess: Failed to get response")
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (goto-char insert-point)
              (unless (bolp)
                (end-of-line)
                (newline))
              (let ((cleaned-response (cc/--gptel-cleanup-response response))
                    (start-pos (point)))
                (insert cleaned-response)
                (unless (string-suffix-p "\n" cleaned-response)
                  (newline))
                (let ((end-pos (point)))
                  (indent-region start-pos end-pos)
                  (goto-char end-pos))))))))))

(defun cc/gptel-guess-complete-pattern ()
  "Guess and insert multiple lines after point using gptel to complete a pattern."
  (interactive)
  (let* ((context (buffer-substring-no-properties (point-min) (point)))
         (prompt (concat cc/--gptel-guess-complete-pattern-directive "\n\nCode so far:\n" context))
         (insert-point (point))
         (target-buffer (current-buffer)))
    (message "gptel-guess: Guessing pattern...")
    (gptel-request prompt
      :system cc/--gptel-guess-system-message
      :callback
      (lambda (response _info)
        (if (not response)
            (message "gptel-guess: Failed to get response")
          (when (buffer-live-p target-buffer)
            (with-current-buffer target-buffer
              (goto-char insert-point)
              (unless (bolp)
                (end-of-line)
                (newline))
              (let ((cleaned-response (cc/--gptel-cleanup-response response))
                    (start-pos (point)))
                (insert cleaned-response)
                (unless (string-suffix-p "\n" cleaned-response)
                  (newline))
                (goto-char (+ start-pos (length cleaned-response))))
              (message "gptel-guess: Pattern completed"))))))))

(provide 'gptel-guess)

;; Local Variables:
;; coding: utf-8
;; End:
;;; gptel-guess.el ends here
