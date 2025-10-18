;;; cc-gptel-prompts.el --- Organize gptel prompts/directives -*- lexical-binding: t -*-

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

;; Organize gptel prompts/directives

;;; Code:

;; LOOK-AT: https://github.com/jwiegley/gptel-prompts

;; TODO: for every file in ~/.emacs.d/prompts/ create a directive in gptel (cc/gptel-update-prompts)
;; TODO: load prompt from the current project (.prompt.md, .agents.d/prompt.md)
;; TODO: include in the prompt some files of the current project, can you do that with presets? (.instructions.md, .agents.d/instructions.md)

(require 'gptel)

(defgroup cc-gptel-prompts nil
  "Prompt management for gptel."
  :group 'gptel)

(defcustom cc/gptel-prompts-directory
  (expand-file-name "prompts" user-emacs-directory)
  "Directory containing prompt files."
  :type 'directory
  :group 'cc-gptel-prompts)

(defcustom cc/gptel-instructions-directory
  (expand-file-name "prompts/instructions" user-emacs-directory)
  "Directory containing instruction files."
  :type 'directory
  :group 'cc-gptel-prompts)

(defun cc/gptel-prompts--load-instruction (instruction-name)
  "Load the content of an instruction file named INSTRUCTION-NAME."
  (let ((instruction-file (expand-file-name instruction-name cc/gptel-instructions-directory)))
    (unless (file-readable-p instruction-file)
      (user-error "Instruction file not found or not readable: %s" instruction-file))
    (with-temp-buffer
      (insert-file-contents instruction-file)
      (buffer-string))))

(defun cc/gptel-prompts--load-prompt (prompt-name)
  "Load the content of a prompt file named PROMPT-NAME, handling inclusions."
  (let ((prompt-file (expand-file-name prompt-name cc/gptel-prompts-directory)))
    (unless (file-readable-p prompt-file)
      (user-error "Prompt file not found or not readable: %s" prompt-file))
    (with-temp-buffer
      (insert-file-contents prompt-file)
      (goto-char (point-min))
      (while (re-search-forward "{{include \\([^}]+\\)}}" nil t)
        (let ((inclusion (match-string 1)))
          (replace-match (cc/gptel-prompts--load-instruction inclusion) t t)))
      (buffer-string))))

(defun cc/gptel-build-prompt (prompt-name &rest instruction-names)
  "Build a prompt by combining PROMPT-NAME with INSTRUCTION-NAMES.

PROMPT-NAME is the name of a file in `cc/gptel-prompts-directory'.
INSTRUCTION-NAMES are names of files in `cc/gptel-instructions-directory'.

Returns a string containing the concatenated contents of all files,
separated by newlines, with the prompt first followed by instructions
in the order specified."
  (let ((contents (list (cc/gptel-prompts--load-prompt prompt-name))))
    ;; Read instruction files
    (dolist (instruction-name instruction-names)
      (push (cc/gptel-prompts--load-instruction instruction-name) contents))
    ;; Join with newlines, preserving order (reverse because we pushed)
    (mapconcat #'identity (nreverse contents) "\n")))

(provide 'cc-gptel-prompts)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-gptel-prompts.el ends here
