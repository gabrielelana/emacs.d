;;; init.el --- Bootstrap configuration -*- lexical-binding: t -*-

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

;; Bootstrap configuration

;; Compile Emacs with the following commands
;;     $ git nuke && git pull && git checkout emacs-29.1.90
;;     $ ./autogen.sh && ./configure CFLAGS="-O2 -march=native" --prefix=/home/coder/opt/emacs-29.1.90 --bindir=/home/coder/opt/emacs-29.1.90/bin --with-json --with-tree-sitter --with-imagemagick --with-x --with-x-toolkit=gtk3 --with-xwidgets --without-dbus --with-native-compilation=aot --with-wide-int --with-mailutils --with-harfbuzz
;;     $ make -j8 && make install

;; Start Emacs with the following commands
;;     $ ~/opt/emacs-29.1.90/bin/emacs -Q --load=~/code/emacs-experimental/init.el &

;;; Code:

;; The following two lines can be removed when done and when this
;; file will be moved in the canonical place ~/.emacs.d/init.el
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(add-to-list 'load-path (concat user-emacs-directory "local-packages"))

;; Appearance as early as possible
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode -1)
(electric-indent-mode +1)
(setq visible-bell nil
      inhibit-splash-screen t
      frame-resize-pixelwise t
      split-width-threshold 100)

;; Native compilation
(require 'comp)
(setq native-comp-speed 2
      native-comp-async-report-warnings-errors nil
      native-comp-async-query-on-exit t)

;; Prevent native compilation of .dir-locals.el files.
(let ((deny-list '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)")))
  (when (boundp 'native-comp-deferred-compilation-deny-list)
    (setq native-comp-deferred-compilation-deny-list deny-list)))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 64 1024 1024))
      (startup-gc-cons-threshold (* 256 1024 1024))
      (normal-gc-cons-percentage 0.1)
      (startup-gc-cons-percentage 0.6))
  (setq gc-cons-threshold startup-gc-cons-threshold)
  (setq gc-cons-percentage startup-gc-cons-percentage)
  (defun cc/restore-gc-settings ()
    "Restore garbage collection settings."
    (setq gc-cons-threshold normal-gc-cons-threshold
          gc-cons-percentage normal-gc-cons-percentage)
    (message "Garbage collection settings restored."))
  (add-hook 'emacs-startup-hook #'cc/restore-gc-settings))

;; Libraries
(require 'personal-functions)
(require 'color-functions)

;; Font
(set-frame-font "PragmataPro Mono 22")

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate use-package with straight
(straight-use-package 'use-package)
(when (boundp 'straight-use-package-by-default)
  (setq straight-use-package-by-default t))

;; Defaults of built-in stuff
(require 'better-defaults)

;; Libraries
(use-package bind-key)
(use-package s)
(use-package f)
(use-package ht)
(use-package uuidgen)
(use-package dash)
(use-package request)

;; Themes
(defvar after-enable-theme-hook nil
  "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

(defun cc/load-theme (theme)
  "Load THEME first disabling the all the enabled custom themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm))

(use-package kaolin-themes)
(use-package dracula-theme)
(use-package night-owl-theme)
(use-package apropospriate-theme)
(use-package subatomic-theme
  :config
  (custom-set-faces
   '(org-block ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-code ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-verbatim ((t (:inherit font-lock-comment-delimiter-face))))
   '(org-document-title ((t (:inherit org-document-info))))
   '(term-color-black ((t (:foreground "#2e3043" :background "#2e3043"))))
   '(term-color-red ((t (:foreground "#ea8673" :background "#ea8673"))))
   '(term-color-green ((t (:foreground "#a9dc69" :background "#a9dc69"))))
   '(term-color-yellow ((t (:foreground "#ffd700" :background "#ffd700"))))
   '(term-color-blue ((t (:foreground "#8aa6bc" :background "#8aa6bc"))))
   '(term-color-magenta ((t (:foreground "#feccd4" :background "#feccd4"))))
   '(term-color-cyan ((t (:foreground "#9c71a5" :background "#9c71a5"))))
   '(term-color-white ((t (:foreground "#e5e5e5" :background "#e5e5e5")))))
  :custom
  subatomic-more-visible-comment-delimiters t)

(cc/load-theme 'modus-operandi)        ; light
;; (cc/load-theme 'kaolin-light t)	  ; light
;; (cc/load-theme 'apropospriate-light t) ; light
;; (cc/load-theme 'dracula t)             ; dark
;; (cc/load-theme 'nord t)		  ; dark
;; (cc/load-theme 'night-owl t)           ; dark
;; (cc/load-theme 'subatomic)             ; dark


;; Completion system
(use-package vertico
  :preface
  (defun cc/--backward-kill-dwim ()
    "Kills backward component path when in file completion, backward word otherwise."
    (interactive)
    (if-let* ((metadata (completion-metadata 'category minibuffer-completion-table minibuffer-completion-predicate))
              (completion-category (completion-metadata-get metadata 'category)))
        (if (eq completion-category 'file)
            (cc/backward-kill-path-component)
          (backward-kill-word 1))
      (error "This function is supposed to be called only when completion is active")))
  :bind (:map vertico-map
              ("C-j" . vertico-insert)
              ("M-w" . vertico-save)
              ("C-l" . cc/--backward-kill-dwim))
  :custom
  (vertico-cycle t "Enable cycling for `vertico-next` and `vertico-previous`")
  (vertico-resize t "Grow and shrink the Vertico minibuffer")
  :init
  (vertico-mode))

(use-package orderless
  :preface
  (defun first-is-prefix (pattern index _total)
    (when (= index 0)
      `(orderless-regexp . ,(format "^%s" pattern))))
  (defun literal-if-equal (pattern _index _total)
    (when (string-prefix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 1))))
  (defun negate-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun initialism-if-dot (pattern _index _total)
    (when (string-prefix-p "." pattern)
      `(orderless-initialism . ,(substring pattern 1))))
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-regexp))
  (orderless-component-separator "[ &]+" "In company use `&` to separate components")
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-style-dispatchers '(literal-if-equal
                                 negate-if-bang
                                 initialism-if-dot
                                 flex-if-twiddle)))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Consult
(use-package consult
  :bind (;; C-c bindings
         ("C-c o h" . consult-history)
         ("C-c o m" . consult-mode-command)
         ("C-c o b" . consult-bookmark)
         ("C-c o k" . consult-kmacro)
         ;; C-x bindings
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x C-b" . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("C-h a" . consult-apropos)               ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)              ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g l" . consult-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  :custom
  (consult-preview-key '(:debounce 0.2 any))
  (consult-preview-key 'any)
  (consult-project-root-function #'projectile-project-root)

  :init
  ;; Improves the register preview for `consult-register',
  ;; `consult-register-load', `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Tweaks the register preview window.
  (advice-add #'register-preview :override #'consult-register-window))

;; Embark
(use-package embark
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings))
  :custom
  ;; optionally replace the key help with a completing-read interface
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark--vertico-indicator))
  (embark-prompter #'embark-completing-read-prompter)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :requires (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Projectile
(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-mode-line-prefix " ")
  (projectile-project-search-path '("~/code"))
  (projectile-indexing-method 'hybrid)
  (projectile-generic-command "rg --files --hidden --null")
  :init
  (projectile-mode +1))

(use-package rg
  :init
  (rg-enable-default-bindings))

;; Company
(use-package company
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("M-." . company-show-location)
         ("<tab>" . company-complete-common-or-cycle)
         ("C-s" . company-search-candidates)
         ("C-d" . company-show-doc-buffer)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-other-buffers nil)
  (company-backends '(company-nxml
                      company-css
                      company-capf
                      company-dabbrev-code
                      company-files
                      company-dabbrev)))

;; Magit
(use-package magit
  :preface
  (defun cc/git-add-with-force-current-buffer ()
    "Track (git add -f) the file of the current buffer."
    (interactive)
    (shell-command (concat "git add -f " (shell-quote-argument buffer-file-name))))
  :bind (("C-c g s" . magit-status)
         ("C-c g f" . cc/git-add-with-force-current-buffer)
         ("H-s" . magit-status))
  :custom
  (magit-section-visibility-indicator nil)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :config
  (transient-bind-q-to-quit))

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine)))

(use-package git-gutter
  :preface
  (defun cc/--setup-git-gutter-theme ()
    "Configure git gutter in accordance to the current theme."
    (let ((default-fg (face-attribute 'default :foreground))
          (default-bg (face-attribute 'default :background)))
      (set-face-attribute 'fringe nil
                          :foreground default-bg
                          :background default-bg)
      (set-face-attribute 'git-gutter:separator nil
                          :foreground default-bg
                          :background default-bg)
      (set-face-attribute 'git-gutter:added nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)
      (set-face-attribute 'git-gutter:deleted nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)
      (set-face-attribute 'git-gutter:modified nil
                          :foreground default-fg
                          :background default-bg
                          :height 0.8)))
  :bind (("C-c g n" . git-gutter:next-hunk)
         ("C-c g p" . git-gutter:previous-hunk)
         ("C-c g r" . git-gutter:revert-hunk)
         ("C-c g a" . git-gutter:stage-hunk)
         ("C-c g u" . git-gutter:update-all-windows))
  :hook ((after-load-theme . cc/--setup-git-gutter-theme))
  :custom
  (git-gutter:window-width 2)
  (git-gutter:added-sign "\uf067")
  (git-gutter:deleted-sign "\uf068")
  (git-gutter:modified-sign "\uf054")
  (git-gutter:hide-gutter nil)
  (global-git-gutter-mode 1)
  :config
  (cc/--setup-git-gutter-theme))

;; Terminal
(use-package vterm
  :preface
  ;; TODO: projectile integration
  ;; - add terminal buffer to project's buffer (possible?)
  ;; - close project's terminal buffer when close project (hook?)
  ;; - completing-read between terminals of current project
  ;; TODO: improve cc/project-vterm-other-window
  ;; - with C-u completeting-read which directory of the project start the terminal
  (defun cc/--setup-vterm ()
    (setq-local global-hl-line-mode nil)
    (setq-local line-spacing nil))
  :hook ((vterm-mode . #'cc/--setup-vterm))
  :bind (("H-v" . #'cc/project-vterm-other-window))
  :custom
  (vterm-max-scrollback 32768))

;; PDF
(use-package pdf-tools
  :config
  (pdf-tools-install)
  :custom
  (pdf-view-use-scaling nil)
  (pdf-view-use-unicode-ligther nil))

;; Dired
(use-package dired
  :straight (:type built-in)
  :bind (("C-x C-j" . dired-jump))
  :custom
  (dired-listing-switches "-aGFhlv --color=never --group-directories-first --time-style=long-iso")
  (dired-dwim-target t))

;; Utilities
(use-package scratch)
(use-package rainbow-mode)

(use-package savehist
  ;; Saves minibuffer history
  :init
  (savehist-mode))

(use-package whitespace
  :hook (before-save . whitespace-cleanup))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package which-key
  :config
  (setq which-key-idle-delay 3)
  (which-key-mode))

(use-package direnv
  :demand t
  :config (direnv-mode))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package visual-regexp
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))

(use-package string-inflection
  ;; TODO: cc/string-inflection-dwim interactive bound to C-*
  ;; cicle between inflections used in the current programming language if any
  :bind (("C-*" . string-inflection-all-cycle)
         ("C-c q l" . string-inflection-lower-camelcase)
         ("C-c q c" . string-inflection-camelcase)
         ("C-c q u" . string-inflection-underscore)
         ("C-c q U" . string-inflection-upcase)
         ("C-c q k" . string-inflection-kebab-case)))

(use-package drag-stuff
  :custom
  (drag-stuff-except-modes '(org-mode))
  :config
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (yas-new-snippet-default nil)
  :config
  (yas-global-mode))

(use-package consult-yasnippet
  :requires consult
  :bind (("H-y" . consult-yasnippet)))

(use-package highlight-indent-guides
  :hook ((yaml-mode . highlight-indent-guides-mode)
         (json-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character))

;; Prettify
(use-package apheleia
  :init
  (apheleia-global-mode +1))

;; Multiple cursors
(use-package multiple-cursors
  ;; TODO: restore hydra
  :bind (("M-\\" . mc/mark-next-like-this)
         :map mc/keymap
         ("<return>" . nil)
         ("C-'" . mc-hide-unmatched-lines-mode)))


;; Flymake
(use-package flymake
  :straight (:type built-in)
  :preface
  (defun cc/--setup-flymake-theme ()
    "Setup flymake faces in accordance to the current theme."
    (let ((foreground (face-attribute 'default :foreground))
          (background (face-attribute 'default :background)))
      (set-face-attribute 'flymake-error nil
                          :box `(:line-width 1 :color ,foreground :style nil)
                          :underline nil)
      ;; less standing up than error, more towards background
      (set-face-attribute 'flymake-warning nil
                          :box `(:line-width 1 :color ,(cc/color-lerp foreground background 0.2) :style nil)
                          :underline nil)
      ;; less standing up than warning, more towards background
      (set-face-attribute 'flymake-note nil
                          :box `(:line-width 1 :color ,(cc/color-lerp foreground background 0.4) :style nil)
                          :underline nil)
      (setq flymake-error-bitmap nil
            flymake-warning-bitmap nil)))
  :hook ((after-load-theme . cc/--setup-flymake-theme))
  :config
  (cc/--setup-flymake-theme))

;; Flycheck
(use-package flycheck
  :preface
  (defun cc/--setup-flycheck-theme ()
    "Setup flycheck faces in accordance to the current theme."
    (let ((default-foreground (face-attribute 'default :foreground)))
      (set-face-attribute 'flycheck-error nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)
      (set-face-attribute 'flycheck-warning nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)
      (set-face-attribute 'flycheck-info nil
                          :box `(:line-width 1 :color ,default-foreground :style nil)
                          :underline nil)))
  :hook ((after-load-theme . cc/--setup-flymake-theme))
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-idle-change-delay 0.5)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil)
  :config
  (cc/--setup-flycheck-theme))

;; YAML
(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.neon\\'" . yaml-mode)))

;; JSON
(use-package json-mode
  :mode (("\\.json\\'"  . json-mode)
         (".babelrc" . json-mode)
         (".prettierrc" . json-mode)
         (".eslintrc" . json-mode))
  :hook (json-mode . cc/--setup-json)
  :preface
  (defun cc/--setup-json ()
    (flycheck-mode)
    (flycheck-disable-checker 'javascript-eslint)
    (when (executable-find "jsonlint")
      (flycheck-select-checker 'json-jsonlint)))
  :custom
  (js-indent-level 2))

;; Dockerfile
(use-package dockerfile-mode
  :hook ((dockerfile-mode . flycheck-mode)))

;; CSV
(use-package csv-mode)

;; GraphQL
(use-package graphql-mode)

;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom-face (markdown-code-face ((t (:inherit nil))))
  :custom
  (markdown-fontify-code-blocks-natively nil)
  (markdown-command "pandoc --from markdown_github -t html5 -s"))

;; EmacsLisp
(use-package emacs-lisp
  :straight (:type built-in)
  :preface
  (defun cc/--setup-emacs-lisp ()
    (setq-local flycheck-emacs-lisp-load-path 'inherit
                flycheck-emacs-lisp-initialize-packages 'auto)
    (flycheck-mode)
    (company-mode)
    (eldoc-mode))
  :hook ((emacs-lisp-mode . cc/--setup-emacs-lisp))
  :bind (:map emacs-lisp-mode-map
              ("C-<backspace>" . backward-kill-sexp))
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("*scratch*" . emacs-lisp-mode)))

(use-package cask-mode)
(use-package package-lint)

(use-package paredit
  :hook (emacs-lisp-mode . enable-paredit-mode))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode)
  :init
  (use-package highlight)
  :custom
  (eval-sexp-fu-flash-face 'widget-field)
  (eval-sexp-fu-flash-error-face 'font-lock-warning-face)
  (eval-sexp-fu-flash-duration 0.5)
  :config
  (esf-initialize))

;; Bindings
(bind-key "RET" 'newline-and-indent)
(bind-key "H-p" #'cc/open-line-above)
(bind-key "H-n" #'cc/open-line-below)
(bind-key "H-<return>" #'cc/open-line-here)
(bind-key "M-<space>" #'rectangle-mark-mode)
(bind-key "H-u" #'cc/copy-character-from-above)
(bind-key "H-d" #'cc/copy-character-from-below)
(bind-key "H-<right>" #'windmove-swap-states-right)
(bind-key "H-<left>" #'windmove-swap-states-left)
(bind-key "H-<up>" #'windmove-swap-states-up)
(bind-key "H-<down>" #'windmove-swap-states-down)
(bind-key "H-+" #'text-scale-increase)
(bind-key "H--" #'text-scale-decrease)
(bind-key "C-a" #'cc/smarter-move-beginning-of-line)
(bind-key "C-c l" #'org-store-link)
(bind-key "C-^" #'cc/join-with-next-line)
(bind-key "C-;" #'cc/comment-or-uncomment-line-or-region)
(bind-key "C-x e" #'cc/eval-and-replace)
(bind-key "M-p" #'cc/duplicate-line-or-region-above)
(bind-key "M-n" #'cc/duplicate-line-or-region-below)
(bind-key "M-o" #'other-window)
(bind-key "C-M-y" #'yank-pop)
(bind-key "C-c D" #'cc/delete-current-buffer-and-file)
(bind-key "C-c R" #'cc/rename-current-buffer-and-file)
(bind-key "C-x &" #'kmacro-call-macro)
(bind-key "C-c k f n" #'cc/kill-current-file-name)
(bind-key "C-c k f p" #'cc/kill-current-file-path)
(bind-key "C-c k l" #'kill-whole-line)
(bind-key "C-c e b" #'eval-buffer emacs-lisp-mode-map)
(bind-key "C-c e d" #'eval-defun emacs-lisp-mode-map)
(bind-key "C-c e f" #'emacs-lisp-byte-compile-and-load emacs-lisp-mode-map)
(bind-key "C-c e n" #'emacs-lisp-native-compile-and-load emacs-lisp-mode-map)
(bind-key "C-c e r" #'eval-region emacs-lisp-mode-map)
(bind-key "C-c e =" #'cc/eval-and-replace)
(bind-key "C-c e t" #'ert emacs-lisp-mode-map)
(bind-key "C-c e e" #'toggle-debug-on-error)
(bind-key "C-c e s" #'scratch)
(bind-key "C-c e m" #'view-echo-area-messages)

(global-set-key (kbd "C-c u l") "λ")
(global-set-key (kbd "C-c u a") "∧")
(global-set-key (kbd "C-c u o") "∨")
(global-set-key (kbd "C-c u >") "→")
(global-set-key (kbd "C-c u <") "←")
(global-set-key (kbd "C-c u =") "≡")
(global-set-key (kbd "C-c u t") "⊤")
(global-set-key (kbd "C-c u b") "⊥")
(global-set-key (kbd "C-c u f") "∀")
(global-set-key (kbd "C-c u e") "∊")
(global-set-key (kbd "C-c u u") "∪")
(global-set-key (kbd "C-c u i") "∩")


;; TODO: LSP
;; TODO: language Rust
;; TODO: modeline

;; TODO: YAML language server https://github.com/redhat-developer/yaml-language-server
;; TODO: Docker language server
;; TODO: M-q should not join line which begins with `-`,`TODO`,...
;; TODO: hydra
;; TODO: expand-region or combobulate
;; TODO: dabbrev
;; TODO: hippie-exp
;; TODO: org-mode
;; TODO: language TypeScript
;; TODO: language Haskell
;; TODO: language Elixir
;; TODO: language C/C++ with LSP server and linter configured
;; TODO: Makefile
;; TODO: JSON
;; TODO: Dockerfile linter ???
;; TODO: Dockerfile language server ???
;; TODO: TOML
;; TODO: YAML
;; TODO: OpenAI integration
;; TODO: NYXT integration
;; TODO: tree-sitter setup
;; TODO: TLA+
;; TODO: Grammarly integration
;; TODO: Dictionary integration
;; TODO: grok consult
;; TODO: grok embark
;; TODO: grok magit
;; TODO: grok dabbrev
;; TODO: grok hippie-exp
;; TODO: H-v rename buffer with NAME-PROJECT-P
;; TODO: H-v in dired will create a terminal in that directory SHELL-DIRED

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
