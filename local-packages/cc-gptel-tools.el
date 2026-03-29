;;; cc-gptel-tools.el --- summary -*- lexical-binding: t -*-

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

(require 'f)
(require 'plz)
(require 'shr)
(require 'gptel)
(require 'personal-functions)
(require 'find-func)
(require 'straight)

;;; CATEGORY reflection TODO (https://github.com/gregoryg/emacs-gregoryg?tab=readme-ov-file#custom-tools)
;; TODO: - emacs_package_description
;; TODO: - emacs_function_description
;; TODO: - emacs_variable_description
;; TODO: - emacs_variable_current_value
;; TODO: - emacs_function_source
;; TODO: - emacs_lisp_eval

(defun cc/locate-readme-in-directory (dir)
  "Locate the README file in DIR.

The README file is the first regular file in DIR whose name starts with
\"readme\" (case insensitive).

Returns the full path to the file, or nil if no such file is found or if
DIR is not a valid directory."
  (when (file-directory-p dir)
    (let* ((case-fold-search t)
           (readme-filename (seq-find (lambda (fname)
                                        (and (string-match-p "^readme" fname)
                                             (file-regular-p (expand-file-name fname dir))))
                                      (directory-files dir nil "[^.]"))))
      (when readme-filename
        (expand-file-name readme-filename dir)))))

(defun cc/read-emacs-package-readme (package-name)
  "Extract and return the README file for package PACKAGE-NAME.

This version attempts to robustly find the source directory for
packages, including those managed by straight.el.
Return =nil' if no README was found."
  (if (stringp package-name)
      (let* ((library-file-in-load-path (find-library-name package-name))
             (true-library-file-path (when library-file-in-load-path
                                       (file-truename library-file-in-load-path)))
             (package-source-dir (when true-library-file-path
                                   (file-name-directory true-library-file-path)))
             (readme-full-path (when (and package-source-dir (file-directory-p package-source-dir))
                                 (cc/locate-readme-in-directory package-source-dir))))
        (if readme-full-path
            (with-temp-buffer
              (goto-char (point-min))
              (insert-file-contents readme-full-path)
              (buffer-string))
          (progn
            (user-error (concat "No README file found for package '%s'.\n"
                                "  Searched in directory: %s\n"
                                "  Library file in load-path: %s\n"
                                "  Resolved true library file path: %s")
                        package-name
                        (or package-source-dir "unknown or not a directory")
                        (or library-file-in-load-path "not found by find-library-name")
                        (or true-library-file-path "could not be resolved by file-truename"))
            nil)))
    (user-error "PACKAGE-NAME argument must be the package name as a string")))

(gptel-make-tool
 :name "emacs_package_readme"
 :description "Return the README file for a package. You can use the README to know what's the package about and decide if get its full source code with `emacs_package_source` tool."
 :category "reflection"
 :include t
 :function #'cc/read-emacs-package-readme
 :args '((:name "package_name"
          :type string
          :description "Name of the Emacs package."
          :optional nil)))

(defun cc/read-emacs-package-source (package-name &optional full)
  "Extract and return the source code of a package PACKAGE-NAME.

The result is a string using org mode syntax with a top level headline
per file and all files are putted in an emacs-lisp code block.

If FULL is nil (default) then only the primary file is included (aka the
Emacs Lisp file with the same name as the package. Otherwise all the
files of the package are included."
  (if (stringp package-name)
      (let* ((library-path (find-library-name package-name))
             (all-elisp (directory-files (f-dirname library-path) t ".*\\.el\\(\\.gz\\)?$")))
        (with-temp-buffer
          ;; run through the list of .el files
          ;; start with the "primary" .el file, returned by =find-library-name=

          (goto-char (point-min))
          (insert (format "\n* file %s (primary)\n" library-path))
          (insert "#+begin_src emacs-lisp\n")
          (insert-file-contents library-path)
          (goto-char (point-max))
          (insert "\n#+end_src\n")
          (when full
            (dolist (file (remove library-path all-elisp))
              (insert (format "\n* file %s\n" file))
              (insert "#+begin_src emacs-lisp\n")
              (insert-file-contents file)
              (goto-char (point-max))
              (insert "\n#+end_src\n")))
          (buffer-string)))
    (user-error "PACKAGE-NAME must be the package name as a string")))

(gptel-make-tool
 :name "emacs_package_source"
 :description "Fetch all source code for the given Emacs package"
 :category "reflection"
 :include nil
 :function #'cc/read-emacs-package-source
 :args '((:name "package_name"
          :type string
          :description "Name of the Emacs package."
          :optional nil)
         (:name "full_source"
          :type boolean
          :description "If true/t, return source from ALL source code files in the package, not just the primary source file. Let this default in most cases to save on tokens!"
          :optional t)))

(defun cc/list-available-packages ()
  "List all available packages in current Emacs session."
  (string-join (append (mapcar #'symbol-name (mapcar #'car package--builtins))
                       (hash-table-keys straight--recipe-cache))
               " "))

(gptel-make-tool
 :name "emacs_available_packages"
 :description "List all available packages in the current Emacs session. All of these names can be used as `package_name` argument in the other tools (`emacs_package_readme` `emacs_package_source`)."
 :category "reflection"
 :include t
 :function #'cc/list-available-packages)

(defun cc/list-configuration-files ()
  "List all Emacs Lisp files in the user configuration.
This includes `init.el` and `early-init.el` in `user-emacs-directory`
plus all `.el` files recursively in the `local-packages` subdirectory."
  (let* ((config-dir user-emacs-directory)
         (local-packages-dir (expand-file-name "local-packages" config-dir))
         (root-files '("init.el" "early-init.el"))
         (package-files (when (file-directory-p local-packages-dir)
                          (directory-files-recursively local-packages-dir "\\.el\\'"))))
    (mapcar (lambda (file) (file-relative-name file config-dir))
            (append (mapcar (lambda (f) (expand-file-name f config-dir)) root-files)
                    package-files))))

(gptel-make-tool
 :name "emacs_list_configuration_files"
 :description "List all Emacs Lisp files in the user configuration. This includes init.el and early-init.el in the main config directory plus all .el files recursively in the local-packages subdirectory."
 :category "reflection"
 :include t
 :function #'cc/list-configuration-files
 :args '())

(defun cc/read-configuration-file (file-path)
  "Read the content of a configuration FILE-PATH.
FILE-PATH can be relative to the configuration directory or absolute."
  (let* ((config-dir user-emacs-directory)
         (full-path (if (file-name-absolute-p file-path)
                        file-path
                      (expand-file-name file-path config-dir))))
    (if (file-exists-p full-path)
        (with-temp-buffer
          (insert-file-contents full-path)
          (buffer-string))
      (user-error "File not found: %s" file-path))))

(gptel-make-tool
 :name "emacs_read_configuration_file"
 :description "Read the content of a configuration file. The file path can be relative to the configuration directory or absolute."
 :category "reflection"
 :include t
 :function #'cc/read-configuration-file
 :args '((:name "file_path"
          :type string
          :description "Path to the configuration file (relative or absolute)."
          :optional nil)))

(defun cc/read-visible-buffers ()
  "Return the contents of all buffers currently visible to the user.

A buffer is considered visible if it is displayed in a live window on the
current frame. Each buffer is included only once, even if shown in
multiple windows.

The result is a string using org mode syntax with one top level headline
per buffer."
  (let ((buffers (delete-dups
                  (mapcar #'window-buffer
                          (window-list nil 'no-minibuf)))))
    (with-temp-buffer
      (dolist (buffer buffers)
        (let* ((major-mode-name (with-current-buffer buffer major-mode))
               (lang (string-trim-right (symbol-name major-mode-name) "\\(?:-ts\\)?-mode")))
          (insert (format "\n* buffer %s\n" (buffer-name buffer)))
          (insert ":PROPERTIES:\n")
          (insert (format ":MODE: %s\n" major-mode-name))
          (when-let* ((file-name (buffer-file-name buffer)))
            (insert (format ":FILE: %s\n" file-name)))
          (insert ":END:\n")
          (insert (format "#+begin_src %s\n" lang))
          (insert (with-current-buffer buffer
                    (buffer-substring-no-properties (point-min) (point-max))))
          (goto-char (point-max))
          (insert "\n#+end_src\n")))
      (buffer-string))))

(gptel-make-tool
 :name "emacs_visible_buffers"
 :description "Return the contents of all buffers currently visible to the user. When the user refers to something not present in the prompt but somehow visible, then use this tool to get it."
 :category "reflection"
 :include t
 :function #'cc/read-visible-buffers
 :args '())

;;; CATEGORY agentic TODO

;;; CATEGORY web

(gptel-make-tool
 :category "web"
 :name "search"
 :async t
 :function (lambda (cb keyword)
             (cc/tavily-search-async cb keyword))
 :description "Search the Internet; If you used any search results, be sure to include the references in your response."
 :args (list '(:name "keyword"
               :type string
               :description "The keyword to search")))

(gptel-make-tool
 :category "web"
 :name "read_url"
 :async t
 :description "Fetch the Markdown contents from an HTML page specified by its URL"
 :args (list '(:name "url"
               :type string
               :description "The url of the web page"))
 :function (lambda (cb url)
             (cc/fetch-url-markdown-async cb url)))

(defun cc/tavily-search-async (callback query &optional search-depth max-results)
  "Search QUERY using the Tavily API and call CALLBACK with JSON result.
QUERY is the search query string.
Optional SEARCH-DEPTH is either \"basic\" (default) or \"advanced\".
Optional MAX-RESULTS is the maximum number of results (default 5)."
  (let* ((plz-curl-default-args (cons "-k" plz-curl-default-args))
         (url "https://api.tavily.com/search")
         (search-depth (or search-depth "basic"))
         (max-results (or max-results 5))
         (request-data
          `(("api_key" . ,(cc/read-key-from-env "TAVILY_API_KEY"))
            ("query" . ,query)
            ("search_depth" . ,search-depth)
            ("max_results" . ,max-results))))
    (plz 'post url
      :headers '(("Content-Type" . "application/json"))
      :body (json-encode request-data)
      :as 'string
      :then (lambda (result) (funcall callback result)))))

(defun cc/fetch-url-markdown-async (callback url)
  "Fetch URL and call CALLBACK with MARKDOWN result."
  (require 'plz)
  (let ((plz-curl-default-args (cons "-k" plz-curl-default-args)))
    (plz 'get url
      :as 'string
      :then (lambda (html)
              (with-temp-buffer
                (insert html)
                (shr-render-region (point-min) (point-max))
                (cc/shr-link-to-markdown)
                (funcall callback (buffer-substring-no-properties (point-min) (point-max))))))))

(defun cc/shr-link-to-markdown ()
  "Replace all shr-link in the current buffer to markdown format."
  (goto-char (point-min))
  (let (prop)
    (while (setq prop (text-property-search-forward 'shr-url))
      (let* ((start (prop-match-beginning prop))
             (end (prop-match-end prop))
             (text (buffer-substring-no-properties start end))
             (link (prop-match-value prop)))
        (delete-region start end)
        (goto-char start)
        (insert (format "[%s](%s)" text link))))))

(provide 'cc-gptel-tools)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-gptel-tools.el ends here
