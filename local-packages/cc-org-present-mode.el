;;; cc-org-present-mode.el --- Turn an org buffer to a slide presentation -*- lexical-binding: t -*-

;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Package-Requires: (_)
;; Homepage: http://github.com/gabrielelana/_
;; Keywords: org-mode slides presentation

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

;; Treat the headings of an Org buffer as slides in a presentation. When you
;; activate the minor mode, the current slide is narrowed, and the buffer
;; becomes read-only. Additional graphical simplifications can be applied to
;; make it resemble a presentation rather than an Org buffer. Exiting the minor
;; mode will restore the buffer to its original state.

;; TODO: rename file/package to cc-org-present

;;; Code:

(require 'org)
(require 'org-element)

(defvar-local cc/org-present--slides nil
  "List of (LEVEL . POSITION) for slide headings of current buffer.")

(defvar-local cc/org-present--current-slide-index nil
  "Index of the current slide in `cc/org-present--slides'.")

(defvar-local cc/org-present--overlays nil
  "List of overlays currently managed by `cc/org-present-mode'.")

(defconst cc/org-present--invisibility-spec '(cc/org-present)
  "Invisibility spec entry used `cc/org-present-mode' to show/hide elements.")

(defvar cc/org-present-visible-options '("title:" "author:" "date:" "email:")
  "Org options whose values remain visible when overlays are active.")

(defvar cc/org-present-hide-stars-in-headings t
  "Non-nil means conceal heading stars when overlays are active.")

(defvar cc/org-present-headings-are-slide-till-level 2
  "Level till an heading is recognized as a slide.")

(defvar cc/org-present-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'cc/org-present-next)
    (define-key map (kbd "C-c C-p") #'cc/org-present-prev)
    (define-key map (kbd "C-c C-q") #'cc/org-present--quit)
    map)
  "Keymap used while `cc/org-present-mode' is active.")

(define-minor-mode cc/org-present-mode
  "Slide navigation over Org headings."
  :lighter " OrgP"
  :keymap cc/org-present-mode-map
  (unless (derived-mode-p 'org-mode)
    (setq cc/org-present-mode nil)
    (user-error "Mode cc/org-present-mode is only available in Org buffers"))
  (if cc/org-present-mode
      (cc/org-present--enter)
    (cc/org-present--leave)))

(defun cc/org-present--quit ()
  "Quit org-present mode."
  (interactive)
  (cc/org-present-mode -1))

(defun cc/org-present-next ()
  "Present the next slide."
  (interactive)
  (widen)
  (setq-local cc/org-present--current-slide-index
              (min (1- (length cc/org-present--slides))
                   (1+ cc/org-present--current-slide-index)))
  (cc/org-present--show))

(defun cc/org-present-prev ()
  "Present the previous slide."
  (interactive)
  (widen)
  (setq-local cc/org-present--current-slide-index
              (max 0 (1- cc/org-present--current-slide-index)))
  (cc/org-present--show))

(defun cc/org-present--add-overlay (start end)
  "Create overlay from START to END and remember it in the current buffer."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'invisible 'cc/org-present)
    (push ov cc/org-present--overlays)))

(defun cc/org-present--visible-option-p (keyword)
  "Return non-nil when KEYWORD (e.g. \"title:\") should stay visible."
  (let ((case-fold-search t))
    (member keyword cc/org-present-visible-options)))

(defun cc/org-present--show-elements ()
  "Remove overlays installed by `cc/org-present--hide-elements'."
  (when cc/org-present--overlays
    (mapc #'delete-overlay cc/org-present--overlays)
    (setq cc/org-present--overlays nil))
  (remove-from-invisibility-spec cc/org-present--invisibility-spec))

(defun cc/org-present--hide-elements ()
  "Add overlays to conceal org cluttering elements while presenting."
  (cc/org-present--show-elements)
  (add-to-invisibility-spec cc/org-present--invisibility-spec)
  (save-excursion
    ;; Hide #+OPTION lines unless whitelisted.
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\([^[:space:]]+\\).*" nil t)
      (let* ((keyword (match-string 2))
             (end (if (cc/org-present--visible-option-p keyword) 2 0)))
        (cc/org-present--add-overlay (match-beginning 1) (match-end end))))
    ;; Hide leading stars in headings.
    (when cc/org-present-hide-stars-in-headings
      (goto-char (point-min))
      (while (re-search-forward "^\\(*+\\)" nil t)
        (cc/org-present--add-overlay (match-beginning 1) (match-end 1))))
    ;; Hide emphasis markers if Org isn't already doing so.
    (unless org-hide-emphasis-markers
      (goto-char (point-min))
      (while (re-search-forward org-emph-re nil t)
        (cc/org-present--add-overlay (match-beginning 2) (1+ (match-beginning 2)))
        (cc/org-present--add-overlay (1- (match-end 2)) (match-end 2))))
    ;; Hide verbatim markers in the same way.
    (unless org-hide-emphasis-markers
      (goto-char (point-min))
      (while (re-search-forward org-verbatim-re nil t)
        (cc/org-present--add-overlay (match-beginning 2) (1+ (match-beginning 2)))
        (cc/org-present--add-overlay (1- (match-end 2)) (match-end 2))))))

(defun cc/org-present--discover-slides ()
  "Return a list of cons cells (LEVEL . POSITION) for slide headings."
  (org-with-wide-buffer
   (org-element-map (org-element-parse-buffer) 'headline
     (lambda (headline)
       (let ((level (org-element-property :level headline)))
         (when (<= level cc/org-present-headings-are-slide-till-level)
           (cons level (org-element-property :begin headline))))))))

(defun cc/org-present--slide-index-at-point (slides)
  "Find out the index in SLIDES of the slide containing point."
  (thread-last slides
               (seq-map-indexed (lambda (slide i) (cons slide i)))
               (seq-filter (lambda (slide) (<= (cdar slide) (point))))
               (last)
               (cdar)))

(defun cc/org-present--enter ()
  "Enter presentation mode."
  (setq-local cc/org-present--slides (cc/org-present--discover-slides)
              cc/org-present--current-slide-index (cc/org-present--slide-index-at-point cc/org-present--slides))
  (cc/org-present--hide-elements)
  (read-only-mode 1)
  (cc/org-present--show))

(defun cc/org-present--leave ()
  "Leave presentation mode."
  (widen)
  (read-only-mode -1)
  (cc/org-present--show-elements)
  (setq-local cc/org-present--slides nil
              cc/org-present--current-slide-index nil))

(defun cc/org-present--last-slide-p ()
  "Are we at the last slide?"
  (eq (1+ cc/org-present--current-slide-index) (length cc/org-present--slides)))

(defun cc/org-present--current-slide-region ()
  "Return the region as (BEGINNING . END) of the current slide."
  (let* ((current-slide-position (cdr (nth cc/org-present--current-slide-index cc/org-present--slides)))
         (next-slide-position (if (cc/org-present--last-slide-p)
                                  (point-max)
                                (cdr (nth (1+ cc/org-present--current-slide-index) cc/org-present--slides)))))
    (cons current-slide-position next-slide-position)))

(defun cc/org-present--show ()
  "Show the current slide."
  (pcase-let ((`(,beg . ,end) (cc/org-present--current-slide-region)))
    (goto-char beg)
    (narrow-to-region beg end)))

(provide 'cc-org-present-mode)

;; Local Variables:
;; coding: utf-8
;; End:
;;; cc-org-present-mode.el ends here
