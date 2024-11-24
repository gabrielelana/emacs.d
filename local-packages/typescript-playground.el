;;; typescript-playground.el --- summary -*- lexical-binding: t -*-

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

;; Create playground link for the current buffer

;;; Code:

(require 'org)

;; (defun cc/typescript-playground-url ()
;;   "Typescript Playground URL code section of current buffer."
;;   (with-current-buffer (current-buffer)
;;     (shell-command-to-string (concat "cat " buffer-file-name " | typescript-playground-url"))))

;; (defun cc/typescript-playground-url-file (filename)
;;   "Typescript Playground URL code section of current buffer."
;;   (with-current-buffer (current-buffer)
;;     (shell-command-to-string (concat "cat " buffer-file-name " | typescript-playground-url"))))

(defun cc/org-code-block-tangle-file ()
  "Get tangle file of the code block at point."
  (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'no-eval)))))

;; (concat "solutions/" (file-name-nondirectory "snippets/example.ts"))

(defun type-aided-programming-workshop/code-block-org-links ()
  "Links to files related to the code block at point."
  (interactive)
  (let* (snippet-playground-url
         solution-playground-url
         (links-snippet "")
         (snippet-file-name (cc/org-code-block-tangle-file))
         (solution-file-name (concat "solutions/" (file-name-nondirectory snippet-file-name)))
         (snippet-absolute-file-name (expand-file-name snippet-file-name))
         (solution-absolute-file-name (expand-file-name solution-file-name))
         ;; (solution-file-name (expand-file-name (concat (file-name-directory snippet-file-name)
         ;;                                               "/../solutions/"
         ;;                                               (file-name-nondirectory snippet-file-name))))
         )
    (when (file-exists-p snippet-absolute-file-name)
      (setq snippet-playground-url (shell-command-to-string (concat "cat " snippet-absolute-file-name " | typescript-playground-url")))
      (setq links-snippet (concat links-snippet (format "\n- [[file:%s][Snippet]] ([[%s][Playground]])"
                                                        snippet-file-name
                                                        (string-trim snippet-playground-url)))))
    (when (file-exists-p solution-absolute-file-name)
      (setq solution-playground-url (shell-command-to-string (concat "cat " solution-absolute-file-name " | typescript-playground-url")))
      (setq links-snippet (concat links-snippet (format "\n- [[file:%s][Solution]] ([[%s][Playground]])"
                                                        solution-file-name
                                                        (string-trim solution-playground-url)))))
    (if (not (string-empty-p links-snippet))
        (kill-new links-snippet)
      (user-error "Needed files do not exists, you need to tangle first"))
    ;; (message "***%s***" snippet-file-name)
    ;; (message "***%s***" solution-file-name)
    ;; (message "%s" links-snippet)
    ;; (cons snippet-file-name solution-file-name)
    ))

;; (expand-file-name "/home/coder/code/type-aided-programming-ts/snippets/example.ts/../../solutions/example.ts")
;; (file-name-directory "/home/coder/code/type-aided-programming-ts/snippets/example.ts")
;; (file-name-nondirectory "/home/coder/code/type-aided-programming-ts/snippets/example.ts")

(defun cc/org-babel-goto-tangle-file ()
  "Go to visit the tangled file of the code block at point."
  (if-let* ((args (nth 2 (org-babel-get-src-block-info t)))
            (tangle (alist-get :tangle args)))
      (when (not (equal "no" tangle))
        (find-file tangle)
        t)))

;;; org-open-at-point, or C-c C-o
(add-hook 'org-open-at-point-functions 'cc/org-babel-goto-tangle-file)

(provide 'typescript-playground)

;; Local Variables:
;; coding: utf-8
;; End:
;;; typescript-playground.el ends here
