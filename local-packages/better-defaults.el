;;; better-defaults.el --- Early builtin configuration  -*- lexical-binding: t -*-


;; Author: Gabriele Lana <gabriele.lana@gmail.com>
;; Maintainer: Gabriele Lana <gabriele.lana@gmail.com>
;; Version: 0.0.1
;; Homepage: http://github.com/gabrielelana/dotfiles
;; Keywords: dotfiles, configuration

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

;; Early configuration of built-in functionalities

;;; Code:

;; Windows
(setq help-window-select t)

;; Put custom configurations aside
(setq custom-file "~/.emacs.d/custom.el")
(when (not (file-exists-p custom-file))
  (write-region "" nil custom-file))
(load custom-file)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; ;; Don't highlight matches with jump-char - it's distracting
;; (setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(setq sentence-end-without-space ".!?$")
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t;#/]+[-+*] \\|[ \t;#/]*\\(TODO\\|FIX\\|NOTE\\):? ")

;; Don't show byte compile warnings
(setq byte-compile-warnings nil)

;; Save a list of recent files visited. (open recent file with C-x f)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Hide me empty lines after buffer end
(set-default 'indicate-empty-lines nil)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Better scrolling
(setq scroll-conservatively 128) ; > 100
(setq scroll-preserve-screen-position t)
(setq auto-window-vscroll nil)
(setq hscroll-step 1)
(setq hscroll-margin 16)
(setq auto-hscroll-mode t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(require 'ediff)
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; No electric indent
(setq electric-indent-mode 1)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening. Nic is wrong.
(setq eval-expression-print-level 100)

;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun cc/create-non-existent-directory ()
  "Offer to create parent directories if they do not exist."
  (let* ((parent-directory (file-name-directory buffer-file-name))
         (prompt (format "Directory `%s' does not exist! Create it?" parent-directory)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p prompt))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions 'cc/create-non-existent-directory)

;; Better plist indentation for emacs lisp from https://emacs.stackexchange.com/a/52789
(defun cc/calculate-lisp-indent (&optional parse-start)
  "Add better indentation for quoted and backquoted lists.

Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don’t change the indentation
because the line starts inside a string.

PARSE-START may be a buffer position to start parsing from, or a
parse state as returned by calling ‘parse-partial-sexp’ up to the
beginning of the current line."
  ;; This line because `calculate-lisp-indent-last-sexp` was defined with
  ;; `defvar` with it's value ommited, marking it special and only defining it
  ;; locally. So if you don't have this, you'll get a void variable error.
  (defvar calculate-lisp-indent-last-sexp)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          calculate-lisp-indent-last-sexp containing-sexp)
      (cond ((or (markerp parse-start) (integerp parse-start))
             (goto-char parse-start))
            ((null parse-start) (beginning-of-defun))
            (t (setq state parse-start)))
      (unless state
        ;; Find outermost containing sexp
        (while (< (point) indent-point)
          (setq state (parse-partial-sexp (point) indent-point 0))))
      ;; Find innermost containing sexp
      (while (and retry
                  state
                  (> (elt state 0) 0))
        (setq retry nil)
        (setq calculate-lisp-indent-last-sexp (elt state 2))
        (setq containing-sexp (elt state 1))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and calculate-lisp-indent-last-sexp
                 (> calculate-lisp-indent-last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp calculate-lisp-indent-last-sexp
                                            indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek)))))
      (if retry
          nil
        ;; Innermost containing sexp found
        (goto-char (1+ containing-sexp))
        (if (not calculate-lisp-indent-last-sexp)
            ;; indent-point immediately follows open paren.
            ;; Don't call hook.
            (setq desired-indent (current-column))
          ;; Find the start of first element of containing sexp.
          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
          (cond ((looking-at "\\s(")
                 ;; First element of containing sexp is a list.
                 ;; Indent under that list.
                 )
                ((> (save-excursion (forward-line 1) (point))
                    calculate-lisp-indent-last-sexp)
                 ;; This is the first line to start within the containing sexp.
                 ;; It's almost certainly a function call.
                 (if (or
                      ;; Containing sexp has nothing before this line
                      ;; except the first element. Indent under that element.
                      (= (point) calculate-lisp-indent-last-sexp)

                      ;; First sexp after `containing-sexp' is a keyword. This
                      ;; condition is more debatable. It's so that I can have
                      ;; unquoted plists in macros. It assumes that you won't
                      ;; make a function whose name is a keyword.
                      ;; (when-let (char-after (char-after (1+ containing-sexp)))
                      ;;   (char-equal char-after ?:))

                      ;; Check for quotes or backquotes around.
                      (let* ((positions (elt state 9))
                             (last (car (last positions)))
                             (rest (reverse (butlast positions)))
                             (any-quoted-p nil)
                             (point nil))
                        (or
                         (when-let* ((char (char-before last)))
                           (or (char-equal char ?')
                               (char-equal char ?`)))
                         (progn
                           (while (and rest (not any-quoted-p))
                             (setq point (pop rest))
                             (setq any-quoted-p
                                   (or
                                    (when-let* ((char (char-before point)))
                                      (or (char-equal char ?')
                                          (char-equal char ?`)))
                                    (save-excursion
                                      (goto-char (1+ point))
                                      (looking-at-p
                                       "\\(?:back\\)?quote[\t\n\f\s]+(")))))
                           any-quoted-p))))
                     ;; Containing sexp has nothing before this line
                     ;; except the first element.  Indent under that element.
                     nil
                   ;; Skip the first element, find start of second (the first
                   ;; argument of the function call) and indent under.
                   (progn (forward-sexp 1)
                          (parse-partial-sexp (point)
                                              calculate-lisp-indent-last-sexp
                                              0 t)))
                 (backward-prefix-chars))
                (t
                 ;; Indent beneath first sexp on same line as
                 ;; `calculate-lisp-indent-last-sexp'.  Again, it's
                 ;; almost certainly a function call.
                 (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point) calculate-lisp-indent-last-sexp
                                     0 t)
                 (backward-prefix-chars)))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((elt state 3)
               ;; Inside a string, don't change indentation.
               nil)
              ((and (integerp lisp-indent-offset) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) lisp-indent-offset))
              ;; in this case calculate-lisp-indent-last-sexp is not nil
              (calculate-lisp-indent-last-sexp
               (or
                ;; try to align the parameters of a known function
                (and lisp-indent-function
                     (not retry)
                     (funcall lisp-indent-function indent-point state))
                ;; If the function has no special alignment
                ;; or it does not apply to this argument,
                ;; try to align a constant-symbol under the last
                ;; preceding constant symbol, if there is such one of
                ;; the last 2 preceding symbols, in the previous
                ;; uncommented line.
                (and (save-excursion
                       (goto-char indent-point)
                       (skip-chars-forward " \t")
                       (looking-at ":"))
                     ;; The last sexp may not be at the indentation
                     ;; where it begins, so find that one, instead.
                     (save-excursion
                       (goto-char calculate-lisp-indent-last-sexp)
                       ;; Handle prefix characters and whitespace
                       ;; following an open paren.  (Bug#1012)
                       (backward-prefix-chars)
                       (while (not (or (looking-back "^[ \t]*\\|([ \t]+"
                                                     (line-beginning-position))
                                       (and containing-sexp
                                            (>= (1+ containing-sexp) (point)))))
                         (forward-sexp -1)
                         (backward-prefix-chars))
                       (setq calculate-lisp-indent-last-sexp (point)))
                     (> calculate-lisp-indent-last-sexp
                        (save-excursion
                          (goto-char (1+ containing-sexp))
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
                          (point)))
                     (let ((parse-sexp-ignore-comments t)
                           indent)
                       (goto-char calculate-lisp-indent-last-sexp)
                       (or (and (looking-at ":")
                                (setq indent (current-column)))
                           (and (< (line-beginning-position)
                                   (prog2 (backward-sexp) (point)))
                                (looking-at ":")
                                (setq indent (current-column))))
                       indent))
                ;; another symbols or constants not preceded by a constant
                ;; as defined above.
                normal-indent))
              ;; in this case calculate-lisp-indent-last-sexp is nil
              (desired-indent)
              (t
               normal-indent))))))
(advice-add #'calculate-lisp-indent :override #'cc/calculate-lisp-indent)

;; Personal profile
(require 'straight)
(setq user-full-name "Gabriele Lana")
(setq user-mail-address "gabriele.lana@gmail.com")
(setq straight-host-usernames '((github . "gabrielelana")
                                (gitlab . "gabrielelana")
                                (bitbucket . "gabrielelana")))

;; Tramp
(require 'tramp)
(setq tramp-terminal-type "dumb")
(setq tramp-default-method "ssh")
(setq tramp-default-remote-shell "/bin/sh")

;; Dont't backup files
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Don't blink the cursor
(customize-set-variable 'blink-cursor-mode nil)

;; When scroll to the bottom/top then place the cursor to the very last/first line
(customize-set-variable 'scroll-error-top-bottom t)

;; Highlight current line
(global-hl-line-mode)

;; Show matching parenthesis
(show-paren-mode)
(setq show-paren-delay 0.2)
(setq show-paren-style 'parenthesis)

;; More room in the macro's kill ring
(require 'kmacro)
(setq kmacro-ring-max 100)

;; More room for echo area, up to 75% of the current window
(setq max-mini-window-height 0.75)

;; Better performance sacrificing right-to-left languages
(setq-default bidi-display-reordering nil)

;; Underlining
(setq underline-minimum-offset 2)
(setq x-underline-at-descent-line t)

;;; Do not display margins
;;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Display-Margins.html
(setq-default left-margin-width 0)
(setq-default right-margin-width 0)

(provide 'better-defaults)

;; Local Variables:
;; coding: utf-8
;; End:
;;; better-defaults.el ends here
