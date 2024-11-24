;;; cc-go-run.el --- summary -*- lexical-binding: t -*-

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

(defgroup cc/go-run nil
  "Commands for interacting with tests in go."
  :group 'languages
  :prefix "cc/go-run-")

(defcustom cc/go-run-use-gocheck? nil
  "Whether to use gocheck. If nil, fall back to `-run`."
  :group 'cc/go-run
  :type 'boolean)

(defconst cc/go-run-main-buffer "*go run*")
(defconst cc/go-run-test-buffer "*go test*")

(defun cc/go-run--run-test (str)
  "Run all test(s) matching STR."
  (let ((compilation-buffer-name-function (lambda (_) cc/go-run-test-buffer)))
    (compile (concat "go test " str))))

;;;###autoload
(defun cc/go-run-test-with-names (names)
  "Run all unit test(s) with NAMES."
  (interactive "sNames: ")
  (cc/go-run--run-test (shell-quote-argument names)))

;;;###autoload
(defun cc/go-run-test-current-package ()
  "Run test(s) in current package."
  (interactive)
  (cc/go-run--run-test ""))

;;;###autoload
(defun cc/go-run-test-current-packages ()
  "Run test(s) in current package and all its enclosing packages."
  (interactive)
  (cc/go-run--run-test "./..."))

;;;###autoload
(defun cc/go-run-test-current-function ()
  "Run test(s) for the current function."
  (interactive)
  (unless (string-match "_test\\.go" buffer-file-name)
    (user-error "Must be in a _test.go file"))
  (save-excursion
    (let ((test-method (if cc/go-run-use-gocheck? "-check.f" "-run")))
      (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
      (cc/go-run--run-test (format "%s='%s'" test-method (match-string 2))))))

;;;###autoload
(defun cc/go-run-test-current-suite ()
  "Run current test suite."
  (interactive)
  (unless (string-match "_test\.go" buffer-file-name)
    (user-error "Must be in a _test.go file to run go-test-current-suite"))
  (unless cc/go-run-use-gocheck?
    (user-error "Gocheck is needed to test the current suite"))
  (save-excursion
    (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?\\([[:alnum:]]+\\))[ ]+\\)?Test[[:alnum:]_]+(.*)")
    (cc/go-run--run-test (format "-check.f='%s'" (match-string 2)))))

;;;###autoload
(defun cc/go-run-main ()
  "Run the main function in the current buffer."
  (interactive)
  (save-buffer)
  (let ((compilation-buffer-name-function (lambda (_) cc/go-run-main-buffer)))
    (compile (concat "go run " (shell-quote-argument (buffer-file-name))))))

(provide 'cc-go-run)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-go-run.el ends here
