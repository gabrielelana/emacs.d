;;; cc-gptel-openrouter.el --- Generate OpenRouter models for gptel -*- lexical-binding: t -*-

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

;; Fetch OpenRouter model metadata from:
;;
;;   https://openrouter.ai/api/frontend/models
;;
;; and convert it to the model descriptor format expected by gptel.
;;
;; Main entry points:
;;
;; - `cc/gptel-openrouter-models'
;;   Return gptel model descriptors for a list of OpenRouter model slug globs.
;;   Results are cached in `cc/gptel-openrouter-cache-file'.  If the cache was
;;   created with different globs, it is refreshed automatically.
;;
;; - `cc/gptel-openrouter-update-models'
;;   Interactively refresh the cache using the globs stored in the cache file.
;;
;; Example:
;;
;;   (gptel-make-openai "OpenRouter"
;;     :host "openrouter.ai"
;;     :endpoint "/api/v1/chat/completions"
;;     :stream t
;;     :key gptel-api-key
;;     :models (cc/gptel-openrouter-models
;;              '("openai/*" "anthropic/*" "google/gemini*")))

;;; Code:

(require 'json)
(require 'seq)
(require 'url)

(defvar cc/gptel-openrouter-cache-file
  (expand-file-name "gptel-openrouter-models.cache" user-emacs-directory)
  "File where cached OpenRouter gptel model descriptors are stored.")

(defun cc/gptel-openrouter--get (object key)
  "Get KEY from OpenRouter JSON OBJECT.

OBJECT may be an alist, plist, or hash table.  KEY should be a
symbol such as `slug' or `context_length'."
  (cond
   ((hash-table-p object)
    (or (gethash (symbol-name key) object)
        (gethash key object)))
   ((and (listp object) (keywordp (car object)))
    (plist-get object (intern (concat ":" (symbol-name key)))))
   ((listp object)
    (or (alist-get key object)
        (alist-get (symbol-name key) object nil nil #'string=)))
   (t nil)))

(defun cc/gptel-openrouter--price-per-million (price)
  "Convert OpenRouter PRICE string per token to gptel price per million tokens."
  (when price
    (* 1000000
       (cond
        ((numberp price) price)
        ((stringp price) (string-to-number price))
        (t 0)))))

(defun cc/gptel-openrouter--context-window-k (tokens)
  "Convert OpenRouter context length TOKENS to gptel's thousands-of-tokens form."
  (when (and tokens (numberp tokens) (> tokens 0))
    (ceiling tokens 1000)))

(defun cc/gptel-openrouter--json-true-p (value)
  "Return non-nil if VALUE represents JSON true.

`json-read' represents JSON false as `:json-false', which is non-nil
in Emacs Lisp and must not be treated as true."
  (and value (not (eq value :json-false))))

(defun cc/gptel-openrouter--text-model-p (model)
  "Return non-nil if OpenRouter MODEL produces text output."
  (let ((has-text-output (cc/gptel-openrouter--get model 'has_text_output))
        (output-modalities (cc/gptel-openrouter--get model 'output_modalities)))
    (or (cc/gptel-openrouter--json-true-p has-text-output)
        (member "text" output-modalities))))

(defun cc/gptel-openrouter-model->gptel-model (model)
  "Convert one OpenRouter MODEL entry to a gptel model descriptor.

MODEL is one item from the `data' array returned by:

  https://openrouter.ai/api/frontend/models

The return value is suitable for use inside a gptel backend's
`:models' list.  A typical descriptor has the shape:

  (MODEL-SYMBOL
   :description DESCRIPTION
   :capabilities CAPABILITIES
   :mime-types MIME-TYPES
   :context-window CONTEXT-WINDOW
   :input-cost INPUT-COST
   :output-cost OUTPUT-COST)

Return nil for entries that do not look useful for normal gptel chat."
  (let* ((slug (cc/gptel-openrouter--get model 'slug))
         (name (cc/gptel-openrouter--get model 'name))
         (description (cc/gptel-openrouter--get model 'description))
         (context-length (cc/gptel-openrouter--get model 'context_length))
         (input-modalities (cc/gptel-openrouter--get model 'input_modalities))
         (knowledge-cutoff (cc/gptel-openrouter--get model 'knowledge_cutoff))
         (model-supports-reasoning (cc/gptel-openrouter--get model 'supports_reasoning))
         (endpoint (cc/gptel-openrouter--get model 'endpoint))
         (supported-params (cc/gptel-openrouter--get endpoint 'supported_parameters))
         (pricing (cc/gptel-openrouter--get endpoint 'pricing))
         (features (cc/gptel-openrouter--get endpoint 'features))
         (capabilities nil)
         (mime-types nil)
         (props nil))
    (when (and slug (cc/gptel-openrouter--text-model-p model))
      (when (seq-some (lambda (modality)
                        (member modality '("image" "file" "audio" "video")))
                      input-modalities)
        (push 'media capabilities))

      (when (member "image" input-modalities)
          (setq mime-types
                '("image/jpeg"
                  "image/png"
                  "image/gif"
                  "image/webp")))

        (when (member "audio" input-modalities)
          (push 'audio capabilities))

        (when (member "video" input-modalities)
          (push 'video capabilities))

        (when (or (cc/gptel-openrouter--get endpoint 'supports_tool_parameters)
                  (member "tools" supported-params))
          (push 'tool-use capabilities))

        (when (seq-some (lambda (param)
                          (member param '("response_format"
                                          "structured_outputs")))
                        supported-params)
          (push 'json capabilities))

        (when (or model-supports-reasoning
                  (cc/gptel-openrouter--get endpoint 'supports_reasoning))
          (push 'reasoning capabilities))

        (when (cc/gptel-openrouter--get features 'supports_file_urls)
          (push 'url capabilities))

        (when-let* ((desc (or name description)))
          (setq props (plist-put props :description desc)))

        (when capabilities
          (setq props (plist-put props :capabilities
                                 (nreverse (delete-dups capabilities)))))

        (when mime-types
          (setq props (plist-put props :mime-types mime-types)))

        (when-let* ((window (cc/gptel-openrouter--context-window-k context-length)))
          (setq props (plist-put props :context-window window)))

        (when-let* ((input-cost
                     (cc/gptel-openrouter--price-per-million
                      (cc/gptel-openrouter--get pricing 'prompt))))
          (setq props (plist-put props :input-cost input-cost)))

        (when-let* ((output-cost
                     (cc/gptel-openrouter--price-per-million
                      (cc/gptel-openrouter--get pricing 'completion))))
          (setq props (plist-put props :output-cost output-cost)))

        (when knowledge-cutoff
          (setq props (plist-put props :cutoff-date knowledge-cutoff)))

        (cons (intern slug) props))))

(defun cc/gptel-openrouter--glob-match-p (glob string)
  "Return non-nil if GLOB matches STRING.

Matching is case-insensitive.  GLOB uses Emacs file-wildcard syntax
as understood by `wildcard-to-regexp'."
  (when (and glob string)
    (let ((case-fold-search t))
      (string-match-p (wildcard-to-regexp glob) string))))

(defun cc/gptel-openrouter-filter-models (models globs)
  "Return MODELS whose OpenRouter slug matches one of GLOBS.

MODELS is a list of OpenRouter model entries, usually the `data'
array returned by `https://openrouter.ai/api/frontend/models'.

GLOBS is a list of glob patterns, such as patterns matching OpenAI
or Qwen model slugs.

A model is kept when at least one glob matches its `slug' field."
  (seq-filter
   (lambda (model)
     (let ((model-name (cc/gptel-openrouter--get model 'slug)))
       (and (cc/gptel-openrouter--text-model-p model)
            (seq-some (lambda (glob)
                        (cc/gptel-openrouter--glob-match-p glob model-name))
                      globs))))
   models))

(defun cc/gptel-openrouter--fetch-models (globs)
  "Fetch OpenRouter models matching GLOBS and return gptel descriptors.

GLOBS is a list of glob patterns matched against each OpenRouter
model `slug' by `cc/gptel-openrouter-filter-models'.

The models are fetched synchronously from:

  https://openrouter.ai/api/frontend/models

Each matching OpenRouter model is converted with
`cc/gptel-openrouter-model->gptel-model'.  Entries that convert to nil are
omitted from the returned list."
  (let ((url-request-extra-headers
         '(("Accept" . "application/json"))))
    (with-current-buffer
        (or (url-retrieve-synchronously "https://openrouter.ai/api/frontend/models" t t)
            (error "Failed to fetch OpenRouter models"))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "^$" nil t)
              (error "Could not find end of OpenRouter response headers"))
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol)
                   (response (json-read))
                   (models (cc/gptel-openrouter--get response 'data)))
              (delq nil
                    (mapcar #'cc/gptel-openrouter-model->gptel-model
                            (cc/gptel-openrouter-filter-models models globs)))))
        (kill-buffer (current-buffer))))))

(defun cc/gptel-openrouter--read-cache ()
  "Read and return the OpenRouter models cache data.

Return nil if `cc/gptel-openrouter-cache-file' does not exist."
  (when (file-exists-p cc/gptel-openrouter-cache-file)
    (with-temp-buffer
      (insert-file-contents cc/gptel-openrouter-cache-file)
      (read (current-buffer)))))

(defun cc/gptel-openrouter--update-models (globs)
  "Fetch OpenRouter models matching GLOBS and cache gptel descriptors.

The cache is written to `cc/gptel-openrouter-cache-file' together
with the GLOBS used to produce it.  Return only the model descriptors
written to the cache."
  (let ((models (cc/gptel-openrouter--fetch-models globs)))
    (make-directory (file-name-directory cc/gptel-openrouter-cache-file) t)
    (with-temp-file cc/gptel-openrouter-cache-file
      (let ((print-length nil)
            (print-level nil)
            (print-circle nil))
        (insert ";;; gptel-openrouter-models.cache --- generated file -*- lexical-binding: t -*-\n")
        (insert ";;; Do not edit by hand.  Regenerate with `cc/gptel-openrouter--update-models'.\n\n")
        (prin1 `(:globs ,globs :models ,models) (current-buffer))
        (insert "\n")))
    models))

(defun cc/gptel-openrouter-models (globs)
  "Return cached OpenRouter gptel models matching GLOBS.

If `cc/gptel-openrouter-cache-file' does not exist, or if it was
created with different GLOBS, fetch models from OpenRouter with
`cc/gptel-openrouter--update-models' first.

This function always returns only the list of gptel model descriptors,
never the cache metadata."
  (let ((cache (cc/gptel-openrouter--read-cache)))
    (if (and cache
             (listp cache)
             (equal globs (plist-get cache :globs)))
        (plist-get cache :models)
      (cc/gptel-openrouter--update-models globs))))

(defun cc/gptel-openrouter-update-models ()
  "Refresh the OpenRouter gptel models cache using cached globs.

Read the globs from `cc/gptel-openrouter-cache-file', fetch fresh model
metadata from OpenRouter, update the cache file, and return the updated
list of gptel model descriptors.  Signal an error if the cache file does
not exist or does not contain globs."
  (interactive)
  (message "Reading OpenRouter model cache from %s..." cc/gptel-openrouter-cache-file)
  (let* ((cache (cc/gptel-openrouter--read-cache))
         (globs (and (listp cache) (plist-get cache :globs))))
    (unless globs
      (error "No OpenRouter model globs found in cache file: %s"
             cc/gptel-openrouter-cache-file))
    (message "Fetching OpenRouter models for globs: %S..." globs)
    (let ((models (cc/gptel-openrouter--update-models globs)))
      (message "Updated OpenRouter model cache with %d models: %s"
               (length models)
               cc/gptel-openrouter-cache-file)
      models)))

(provide 'cc-gptel-openrouter)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-gptel-openrouter.el ends here
