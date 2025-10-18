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

;; Scratch buffer
(setq initial-scratch-message
      ";;; -*- lexical-binding: t; -*-\n;; Welcome to Emacs! Happy hacking ;-)\n\n")

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

;; Font
;; Main typeface
(set-face-attribute 'default nil :family "PragmataPro Mono" :height 120)
;; Proportionately spaced typeface
(set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
;; Monospaced typeface
(set-face-attribute 'variable-pitch nil :family (face-attribute 'default :family))

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

;; Make sure to use the Melpa version of org-mode
(straight-use-package 'org)

;; Integrate use-package with straight
(straight-use-package 'use-package)
(when (boundp 'straight-use-package-by-default)
  (setq straight-use-package-by-default t
        use-package-verbose t
        use-package-expand-minimally t
        debug-on-error nil))

;; Constants
(defconst JSON_FILES_RX
  (rx (or (seq "." (or "json" "avsc"))
          ".babelrc"
          ".prettierrc"
          ".eslintrc")
      eos)
  "Regular expression to identify files that are JSON.")

(defconst BIOME_FILES_RX
  (rx-to-string `(or (regexp ,JSON_FILES_RX)
                  (seq "." (or "tsx" "jsx" "ts" "js" "mts" "mjs" "cts" "cjs")
                   eos)))
  "Regular expression to identify files which sholud be handled by Biome.")

;; Defaults of built-in stuff
(require 'better-defaults)

;; Libraries
(use-package bind-key)
(use-package s :demand t)
(use-package f :demand t)
(use-package plz)
(use-package ht)
(use-package uuidgen)
(use-package dash)
(use-package request)

;; Libraries
(require 'personal-functions)
(require 'color-functions)

;; Themes
(defvar after-enable-theme-hook nil
  "Hook run after enabling a theme.")

(defvar before-enable-theme-hook nil
  "Hook run after enabling a theme.")

(defun cc/load-theme (theme)
  "Load THEME first disabling the all the enabled custom themes."
  (run-hooks 'before-enable-theme-hook)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme :no-confirm)
  (run-hooks 'after-enable-theme-hook))

(use-package catppuccin-theme
  :preface
  (defun cc/--setup-catppuccin-theme ()
    (when (eq (car custom-enabled-themes) 'catppuccin)
      (custom-theme-set-faces
       'catppuccin
       '(diff-added ((t (:inherit diff-indicator-added))) t)
       '(diff-changed ((t (:inherit diff-indicator-changed))) t)
       '(diff-removed ((t (:inherit diff-indicator-removed))) t))))
  (add-hook 'after-enable-theme-hook #'cc/--setup-catppuccin-theme)
  :custom
  (catppuccin-height-title-1 1.0)
  (catppuccin-height-title-2 1.0)
  (catppuccin-height-title-3 1.0)
  (catppuccin-height-doc-title 1.0))

(use-package kaolin-themes)
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

(progn
  (setq catppuccin-flavor 'latte)
  (cc/load-theme 'catppuccin))
;; (progn
;;   (setq catppuccin-flavor 'macchiato)
;;   (cc/load-theme 'catppuccin))
;; (cc/load-theme 'modus-operandi)        ; light
;; (cc/load-theme 'kaolin-light)	  ; light
;; (cc/load-theme 'apropospriate-light) ; light
;; (cc/load-theme 'dracula)             ; dark
;; (cc/load-theme 'nord)		  ; dark
;; (cc/load-theme 'night-owl)           ; dark
;; (cc/load-theme 'subatomic)             ; dark
;; (cc/load-theme 'modus-vivendi-tinted)
;; (cc/load-theme 'modus-operandi-tinted)
;;; TODO: try https://github.com/crmsnbleyd/evangelion-theme
;;; TODO: try https://github.com/petergardfjall/emacs-immaterial-theme
;;; TODO: try https://github.com/muirdm/emacs-nova-theme
;;; TODO: try https://github.com/purcell/color-theme-sanityinc-tomorrow

;; Unicode, Icons & Emoji
(use-package nerd-icons
  :config
  (setq nerd-icons-scale-factor 1.1))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

;; ;; One day I'll be able to make "Noto Color Emoji" work, now stick with Segoe  "Segoe UI Emoji"
(let ((emoji-font "Noto Color Emoji"))
  (when (member emoji-font (font-family-list))
    (set-fontset-font t 'symbol emoji-font nil 'prepend)
    (set-fontset-font t 'emoji emoji-font nil 'prepend)))

;; Local projects
(when (file-exists-p (expand-file-name "~/code/retro.el"))
  (use-package retro
    :straight `(retro :local-repo ,(expand-file-name "~/code/retro.el"))))

;; (when (file-exists-p (expand-file-name "~/code/emacs-nes"))
;;   (use-package nes
;;     :straight `(nes :local-repo ,(expand-file-name "~/code/emacs-nes"))))

(when (file-exists-p (expand-file-name "~/code/emacs-chip-8"))
  (use-package chip8
    :straight `(chip8 :local-repo ,(expand-file-name "~/code/emacs-chip-8"))))

;; Completion system
(use-package vertico
  :preface
  (defun cc/--backward-kill-dwim ()
    "Kills backward component path when in file completion, backward word otherwise."
    (interactive)
    (if-let* ((metadata (completion-metadata "category" minibuffer-completion-table minibuffer-completion-predicate))
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
         ("C-h B" . embark-bindings)
         :map embark-file-map
         ("O" . cc/embark-org-insert-link))
  :preface
  (defun cc/embark-org-insert-link (file)
    "Insert an org-mode link to FILE in the current buffer.
The path will be absolute. Only works if the current buffer is in
`org-mode'. Prompts for link description, defaulting to the filename."
    (unless (derived-mode-p 'org-mode)
      (user-error "Cannot insert an org mode link in a not org mode buffer"))
    (let* ((default-description (file-name-nondirectory file))
           (description (read-string
                         (format "Description (default %s): " default-description)
                         nil nil default-description)))
      (insert (org-link-make-string (concat "file:" file) description))))
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

;; Window management

(use-package shackle
  :if (not (bound-and-true-p disable-pkg-shackle))
  :config
  (setq shackle-lighter "")
  (setq shackle-select-reused-windows nil)
  (setq shackle-default-alignment 'right)
  (setq shackle-default-size 0.5)
  (setq shackle-rules
        '(
          ("*Help*"          :select t :other t :align right :same nil)
          ;; buffer created by `shr-render-buffer'
          ("*html*"          :select t :inhibit-window-quit t :same t)
          (magit-status-mode :select t :inhibit-window-quit t :same t)
          (magit-log-mode    :select t :other t)
          (compilation-mode :select nil :other t :align below :size 0.3)
          ("*Agda information*"  :select nil :other t :align below :size 0.4)
          ;; (compilation-mode            :select nil                                               )
          ;; ("*undo-tree*"            :size 0.25 :align right)
          ;; ("*eshell*"               :select t                          :other t               )
          ;; ("*Shell Command Output*" :select nil                                               )
          ;; ("\\*Async Shell.*\\*"    :regexp t :ignore t                                                 )
          ;; (occur-mode               :select nil                                   :align t    )
          ;; ("*Completions*"          :size 0.3  :align t    )
          ;; ("*Messages*"             :select nil :inhibit-window-quit t :other t               )
          ;; ("\\*[Wo]*Man.*\\*"       :regexp t :select t   :inhibit-window-quit t :other t               )
          ;; ("\\*poporg.*\\*"         :regexp t :select t                          :other t               )
          ;; ("\\`\\*helm.*?\\*\\'"    :regexp t                                    :size 0.3  :align t    )
          ;; ("*Calendar*"             :select t                          :size 0.3  :align below)
          ;; ("*info*"                 :select t   :inhibit-window-quit t                         :same t)
          ))
  (shackle-mode 1))

;; Elements of the `shackle-rules' alist:
;;
;; |-----------+------------------------+--------------------------------------------------|
;; | CONDITION | symbol                 | Major mode of the buffer to match                |
;; |           | string                 | Name of the buffer                               |
;; |           |                        | - which can be turned into regexp matching       |
;; |           |                        | by using the :regexp key with a value of t       |
;; |           |                        | in the key-value part                            |
;; |           | list of either         | a list groups either symbols or strings          |
;; |           | symbol or string       | (as described earlier) while requiring at        |
;; |           |                        | least one element to match                       |
;; |           | t                      | t as the fallback rule to follow when no         |
;; |           |                        | other match succeeds.                            |
;; |           |                        | If you set up a fallback rule, make sure         |
;; |           |                        | it's the last rule in shackle-rules,             |
;; |           |                        | otherwise it will always be used.                |
;; |-----------+------------------------+--------------------------------------------------|
;; | KEY-VALUE | :select t              | Select the popped up window. The                 |
;; |           |                        | `shackle-select-reused-windows' option makes     |
;; |           |                        | this the default for windows already             |
;; |           |                        | displaying the buffer.                           |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :inhibit-window-quit t | Special buffers usually have `q' bound to        |
;; |           |                        | `quit-window' which commonly buries the buffer   |
;; |           |                        | and deletes the window. This option inhibits the |
;; |           |                        | latter which is especially useful in combination |
;; |           |                        | with :same, but can also be used with other keys |
;; |           |                        | like :other as well.                             |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :ignore t              | Skip handling the display of the buffer in       |
;; |           |                        | question. Keep in mind that while this avoids    |
;; |           |                        | switching buffers, popping up windows and        |
;; |           |                        | displaying frames, it does not inhibit what may  |
;; |           |                        | have preceded this command, such as the          |
;; |           |                        | creation/update of the buffer to be displayed.   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :same t                | Display buffer in the current window.            |
;; |           | :popup t               | Pop up a new window instead of displaying        |
;; |           | *mutually exclusive*   | the buffer in the current one.                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :other t               | Reuse the window `other-window' would select if  |
;; |           | *must not be used      | there's more than one window open, otherwise pop |
;; |           | with :align, :size*    | up a new window. When used in combination with   |
;; |           |                        | the :frame key, do the equivalent to             |
;; |           |                        | other-frame or a new frame                       |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :align                 | Align a new window at the respective side of     |
;; |           | 'above, 'below,        | the current frame or with the default alignment  |
;; |           | 'left, 'right,         | (customizable with `shackle-default-alignment')  |
;; |           | or t (default)         | by deleting every other window than the          |
;; |           |                        | currently selected one, then wait for the window |
;; |           |                        | to be "dealt" with. This can either happen by    |
;; |           |                        | burying its buffer with q or by deleting its     |
;; |           |                        | window with C-x 0.                               |
;; |           | :size                  | Aligned window use a default ratio of 0.5 to     |
;; |           | a floating point       | split up the original window in half             |
;; |           | value between 0 and 1  | (customizable with `shackle-default-size'), the  |
;; |           | is interpreted as a    | size can be changed on a per-case basis by       |
;; |           | ratio. An integer >=1  | providing a different floating point value like  |
;; |           | is interpreted as a    | 0.33 to make it occupy a third of the original   |
;; |           | number of lines.       | window's size.                                   |
;; |-----------+------------------------+--------------------------------------------------|
;; |           | :frame t               | Pop buffer to a frame instead of a window.       |
;; |-----------+------------------------+--------------------------------------------------|
;;
;; http://emacs.stackexchange.com/a/13687/115

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

;; AI
(use-package gptel
  :bind (("C-c i s" . gptel-send)
         ("C-c i c" . gptel))
  :config
  (require 'cc-gptel-prompts)
  (add-to-list
   'gptel-directives
   `(emacs-wizard . ,(cc/gptel-build-prompt "emacs-wizard.md" "formatting.md")))
  (add-to-list
   'gptel-directives
   `(scala-wizard . ,(cc/gptel-build-prompt "scala-wizard.md" "formatting.md")))
  (add-to-list
   'gptel-directives
   `(typescript-wizard . ,(cc/gptel-build-prompt "typescript-wizard.md" "formatting.md")))
  (add-to-list
   'gptel-directives
   `(frontend-ninja . ,(cc/gptel-build-prompt "frontend-ninja.md" "formatting.md")))
  (add-to-list
   'gptel-directives
   `(haskell-tutor . ,(cc/gptel-build-prompt "haskell-tutor.md" "formatting.md")))
  (add-to-list
   'gptel-directives
   `(programming-tutor . ,(cc/gptel-build-prompt "programming-tutor.md")))
  (add-to-list
   'gptel-directives
   `(technical-writer . ,(cc/gptel-build-prompt "technical-writer.md" "formatting.md")))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (cc/read-key-from-env "ANTHROPIC_API_KEY"))
  (gptel-make-gemini "Gemini"
    :stream t
    :key (cc/read-key-from-env "GEMINI_API_KEY"))
  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key (cc/read-key-from-env "DEEPSEEK_API_KEY"))
  (require 'gptel-tools)
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (cc/read-key-from-env "OPENROUTER_API_KEY")
    :models '((z-ai/glm-4.6
               :description "As the latest iteration in the GLM series, GLM-4.6 achieves comprehensive enhancements across multiple domains, including real-world coding, long-context processing, reasoning, searching, writing, and agentic applications."
               :capabilities (reasoning tool-use json url media)
               :input-cost 0.5
               :output-cost 1.75
               :context-window 200
               :cutoff-date "30-09-2025")
              (x-ai/grok-code-fast-1
               :description "Grok Code Fast 1 is a speedy and economical reasoning model that excels at agentic coding. With reasoning traces visible in the response, developers can steer Grok Code for high-quality work flows."
               :capabilities (reasoning tool-use json url media)
               :input-cost 0.2
               :output-cost 0.5
               :context-window 2000
               :cutoff-date "19-09-2025")
              (openai/gpt-5-codex
               :description "GPT-5-Codex is a specialized version of GPT-5 optimized for software engineering and coding workflows. It is designed for both interactive development sessions and long, independent execution of complex engineering tasks."
               :capabilities (reasoning tool-use json url media)
               :input-cost 1.25
               :output-cost 10.00
               :context-window 400
               :cutoff-date "23-09-2025")
              (anthropic/claude-haiku-4.5
               :description "Claude Haiku 4.5 is Anthropic’s fastest and most efficient model, delivering near-frontier intelligence at a fraction of the cost and latency of larger Claude models."
               :capabilities (reasoning tool-use json url media)
               :input-cost 1
               :output-cost 5
               :context-window 200
               :cutoff-date "15-10-2025")
              (google/gemini-2.5-flash-lite-preview-09-2025
               :description "Gemini 2.5 Flash-Lite is a lightweight reasoning model in the Gemini 2.5 family, optimized for ultra-low latency and cost efficiency. It offers improved throughput, faster token generation, and better performance across common benchmarks compared to earlier Flash models"
               :capabilities (reasoning tool-use json url media)
               :input-cost 0.1
               :output-cost 0.4
               :context-window 1000
               :cutoff-date "25-09-2025")
              (qwen/qwen3-coder-plus
               :description "Qwen3 Coder Plus is Alibaba's proprietary version of the Open Source Qwen3 Coder 480B A35B. It is a powerful coding agent model specializing in autonomous programming via tool calling and environment interaction, combining coding proficiency with versatile general-purpose abilities."
               :capabilities (reasoning tool-use json url media)
               :input-cost 1
               :output-cost 5
               :context-window 128
               :cutoff-date "23-09-2025")
              (qwen/qwen3-coder:free
               :description "Is a Mixture-of-Experts (MoE) code generation model developed by the Qwen team. It is optimized for agentic coding tasks such as function calling, tool use, and long-context reasoning over repositories"
               :capabilities (reasoning tool-use json url)
               :input-cost 0
               :output-cost 0
               :context-window 256
               :cutoff-date "20-07-2025")))
  (gptel-make-ollama "Ollama"
    :host "starbuck.local:11434"
    :stream t
    :models '((devstral:24b
               :description "Advanced model for complex tasks; optimized for reasoning and code generation"
               :capabilities (reasoning tool-use json url media)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 128)
              (qwen2.5-coder:32b
               :description "Flagship model for code generation and reasoning; achieves state-of-the-art performance among open-source models"
               :capabilities (code-generation code-repair code-reasoning tool-use json url media)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 32)
              (qwen3:30b
               :description "Advanced model for complex reasoning and agent capabilities; excels in mathematics, code generation, and logical reasoning"
               :capabilities (reasoning code-generation tool-use json url media)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 256)
              (qwen3-coder:30b
               :description "Advanced model for complex reasoning and agent capabilities tuned for coding"
               :capabilities (reasoning code-generation tool-use json url media)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 256)
              (codegemma:code
               :description "A a 7b pretrained variant of CodeGemma that specializes in code completion and generation from code prefixes and/or suffixes"
               :capabilities (code-generation tool-use json url media)
               :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
               :context-window 8)))
  :custom
  (gptel-api-key (cc/read-key-from-env "OPENAI_API_KEY"))
  (gptel-prompt-prefix-alist '((markdown-mode . "# PROMPT⟩ ")
                               (org-mode . "* PROMPT⟩ ")
                               (text-mode . "# PROMPT⟩ ")))
  (gptel-response-prefix-alist '((markdown-mode . "\n")
                                 (org-mode . "\n")
                                 (text-mode . "\n")))
  (gptel-default-mode 'org-mode)
  (gptel-include-reasoning 'ignore)
  (gptel-model 'gpt-4o)
  (gptel-temperature 0)
  (gptel-track-media t))

(use-package gptel-magit
  :after magit
  :hook (magit-mode . gptel-magit-install)
  :custom
  (gptel-magit-commit-prompt
   (let ((prompt-file (f-join user-emacs-directory "prompts" "conventional-commit.md")))
     (if (f-exists-p prompt-file)
         (f-read-text prompt-file)
       gptel-magit-prompt-conventional-commits))))

;; LSP
(use-package lsp-mode
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition)
              ("M-?" . lsp-find-references)
              ("C-c l a" . lsp-execute-code-action)
              ("C-c l d" . lsp-describe-thing-at-point)
              ;; TODO: "C-c l i" should be a command that toggles inlay hints and lenses
              ;; ("C-c l i" . lsp-inlay-hint-enable)
              ("C-c l r" . lsp-rename)
              ("C-c l e" . lsp-ui-flycheck-list)
              )
  :hook ((lsp-mode . flycheck-mode))
  :custom
  (read-process-output-max (* 1024 1024))
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure t)
  (lsp-auto-guess-root t)
  (lsp-completion-enable t)
  (lsp-completion-show-detail t)
  (lsp-completion-show-kind t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-inlay-hint-enable t)
  (lsp-lens-enable t)
  (lsp-idle-delay 0.5)
  (lsp-prefer-flymake nil)
  (lsp-prefer-capf t)
  (lsp-keep-workspace-alive nil)
  (lsp-print-io nil)
  (lsp-log-io nil))

(use-package lsp-ui
  :preface
  (defun cc/--setup-lsp-ui-theme ()
    (let ((default-foreground (face-attribute 'default :foreground))
          (default-background (face-attribute 'default :background)))
      (set-face-attribute 'lsp-details-face nil
                          :height 2)
      (set-face-attribute 'lsp-ui-doc-background nil
                          :foreground default-foreground
                          :background default-background)
      (set-face-attribute 'lsp-ui-doc-header nil
                          :foreground default-foreground
                          :background default-background
                          :weight 'bold
                          :height 2
                          :slant 'italic)
      (setq lsp-ui-doc-border default-foreground)))
  :init
  (add-hook 'after-enable-theme-hook #'cc/--setup-lsp-ui-theme)
  :config
  (cc/--setup-lsp-ui-theme)
  (lsp-ui-sideline-mode 1)
  (lsp-ui-doc-mode 1)
  :custom
  (lsp-inlay-hint-enable nil)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-max-height 40)
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-text-scale-level 1)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-delay 1))

;; Magit
(use-package magit
  :preface
  (defun cc/git-add-with-force-current-buffer ()
    "Track (git add -f) the file of the current buffer."
    (interactive)
    (shell-command (concat "git add -f " (shell-quote-argument buffer-file-name))))
  :bind (("C-c g s" . magit-status)
         ("C-c g d" . magit-dispatch)
         ("C-c g f" . magit-file-dispatch)
         ("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g !" . cc/git-add-with-force-current-buffer))
  :custom
  (magit-section-visibility-indicator nil)
  (transient-display-buffer-action '(display-buffer-below-selected))
  (git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
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
  :custom
  (git-gutter:window-width 2)
  (git-gutter:added-sign "\uf067")
  (git-gutter:deleted-sign "\uf068")
  (git-gutter:modified-sign "\uf054")
  (git-gutter:hide-gutter nil)
  (global-git-gutter-mode 1)
  :init
  (add-hook 'after-enable-theme-hook #'cc/--setup-git-gutter-theme)
  :config
  (cc/--setup-git-gutter-theme))

;; Terminal
(use-package vterm
  :preface
  ;; TODO: projectile integration
  ;; - add terminal buffer as project's buffer (possible?)
  ;; - close project's terminal buffer when close project (hook?)
  ;; - completing-read between terminals of current project
  ;; TODO: improve cc/project-vterm-other-window
  ;; - with C-u completing-read which directory of the project start the terminal
  ;; - rename cc/vterm-other-window-dwim
  ;; - when not in a project puts `global` as prefix for the buffer
  (defun cc/--setup-vterm ()
    (setq-local global-hl-line-mode nil
                line-spacing nil))
  :hook ((vterm-mode . cc/--setup-vterm))
  :bind (("C-c t p" . #'cc/projectile-vterm-other-window))
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

(use-package load-env-vars
  :config
  (let ((global-secret-env-file "~/.env"))
    (when (file-exists-p global-secret-env-file)
      (load-env-vars global-secret-env-file))))

(use-package direnv
  :demand t
  :config (direnv-mode))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package password-generator)

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
  :demand t
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)  ; Set this before enabling
  (drag-stuff-define-keys)
  (drag-stuff-global-mode 1))

(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode)
  :custom
  (page-break-lines-max-width 80))

(use-package yasnippet
  :custom
  (yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  (yas-new-snippet-default nil)
  :config
  (yas-global-mode))

(use-package consult-yasnippet
  :requires consult
  :bind (("C-x y" . consult-yasnippet)))

(use-package highlight-indent-guides
  :hook ((yaml-mode . highlight-indent-guides-mode)
         (json-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character))

;; Prettify
(use-package apheleia
  :config
  (push '(cuefmt . ("cue" "fmt" inplace)) apheleia-formatters)
  (push '(scalafmt . ("scalafmt" "--quiet" "--non-interactive" "--config" (concat (projectile-project-root) ".scalafmt.conf") inplace)) apheleia-formatters)
  (push '(cue-mode . cuefmt) apheleia-mode-alist)
  (push '(regofmt . ("opa" "fmt" inplace)) apheleia-formatters)
  (push '(rego-mode . regofmt) apheleia-mode-alist)
  ;; TODO: only if the project has biome configured
  (push '(biome . ("apheleia-npx" "biome" "format" "--stdin-file-path" filepath)) apheleia-formatters)
  (push '(typescript-mode . biome) apheleia-mode-alist)
  (push '(scala-ts-mode . scalafmt) apheleia-mode-alist)
  (push '(markdown-mode . prettier-markdown) apheleia-mode-alist)
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
  :init
  (add-hook 'after-enable-theme-hook #'cc/--setup-flymake-theme)
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
  :init
  (add-hook 'after-enable-theme-hook #'cc/--setup-flycheck-theme)
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  (flycheck-idle-change-delay 0.5)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-indication-mode nil)
  :config
  (cc/--setup-flycheck-theme))

;;; Treesit
(use-package treesit
  :straight (:type built-in)
  :config
  ;; Add to the alist instead of overriding
  (add-to-list 'treesit-language-source-alist
               '(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
  (add-to-list 'treesit-language-source-alist
               '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  (add-to-list 'treesit-language-source-alist
               '(scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src"))
  ;; Auto-install missing grammars when needed
  (defun cc/--ensure-treesit-grammars ()
    "Install tree-sitter grammars if they are missing."
    (interactive)
    (dolist (grammar '(typescript tsx scala))
      (unless (treesit-language-available-p grammar)
        (treesit-install-language-grammar grammar))))
  ;; Run once at startup to ensure grammars are installed
  (cc/--ensure-treesit-grammars))

;;; EPUB
(use-package nov
  :mode (("\\.epub\\'" . nov-mode))
  :hook ((nov-mode . cc/--setup-nov))
  :preface
  (defun cc/--setup-nov ()
    (message "cc/--setup-nov")
    (set-window-margins
     (car (get-buffer-window-list (current-buffer) nil t)) 2 2)))

;;; Terraform
(use-package terraform-mode
  :hook ((terraform-mode . lsp))
  :custom
  (terraform-indent-level 2)
  (lsp-terraform-ls-validate-on-save t)
  (lsp-terraform-ls-prefill-required-fields t))

;; CUE
(use-package cue-mode
  :straight (cue-mode :type git :host github :repo "russell/cue-mode"))

;; Mermaid
(use-package mermaid-mode)

;; Rego
(use-package rego-mode)
;; TODO: linting with regal
;; TODO: evaluate rule at point
;; TODO: support tests
;;
;;   :custom
;;   (rego-repl-executable "/home/sibi/bin/opa")
;;   (rego-opa-command "/home/sibi/bin/opa")

;; HURL
(use-package hurl-mode
  :straight (hurl-mode :type git :host github :repo "jaszhe/hurl-mode")
  :mode (("\\.hurl\\'" . hurl-mode)
         ("*hurl-response*" . hurl-response-mode)))

;; Biome
(use-package lsp-biome
  :straight (lsp-biome :type git :host github :repo "cxa/lsp-biome")
  :custom
  (lsp-biome-active-file-types (list BIOME_FILES_RX))
  (lsp-biome-organize-imports-on-save t)
  (lsp-biome-format-on-save t))

;; YAML
(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.neon\\'" . yaml-mode))
  :hook ((yaml-mode . lsp))
  ;; :custom
  ;; ;; TODO: apparently the format is wrong
  ;; ;; look for schemas at https://www.schemastore.org/json/
  ;; (lsp-yaml-schemas
  ;;  [((fileMatch . ["*docker-compose*"])
  ;;    (url . "https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"))])
  )

;; JSON
(use-package json-mode
  :hook ((json-mode . lsp)
         (json-mode . cc/--setup-json))
  :preface
  (defun cc/--setup-json ()
    (when (executable-find "biome")
      (apheleia-mode 1)
      ;; XXX: we are letting apheleia do his job, biome/lsp-biome adds an
      ;; extra `}` at the end of the file for some reason
      (setq-local lsp-biome-format-on-save nil))
    (when (executable-find "jsonlint")
      (flycheck-mode t)
      ;; NOTE: needed to override the JSON lsp server will
      (run-at-time 1 nil (lambda ()
                           (flycheck-select-checker 'json-jsonlint)))))
  :init
  ;; cannot use :mode macro because mode list contains a non literal value
  (add-to-list 'auto-mode-alist `(,JSON_FILES_RX . json-mode))
  :custom
  ;; look for schemas at https://www.schemastore.org/json/
  (lsp-json-schemas
   [((fileMatch . ["*.avsc"])
     (url . "https://json.schemastore.org/avro-avsc.json"))])
  (js-indent-level 2))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

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
         ("*scratch*" . emacs-lisp-mode))
  :custom
  (lisp-indent-function 'common-lisp-indent-function))

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

;; Rust
(use-package rustic
  :commands rustic-mode
  :hook ((rustic-mode . lsp))
  :bind (:map rustic-mode-map
              ("C-c l s" . lsp-rust-analyzer-status))
  :custom
  (rustic-format-on-save nil))

(use-package rust-playground)
(use-package toml-mode)

;; Golang
(use-package go-mode
  :hook ((go-mode . lsp))
  :custom
  (tab-width 4)
  (standard-indent 4))

;; TODO: run golangci-lint before save, apheleia?
;; TODO: run goimports before save, apheleia?
;; TODO: run staticcheck before save, apheleia?

(use-package go-tag
  :after go-mode
  :custom
  (go-tag-args '("-transform" "camelcase")))

;; (use-package go-mode
;;   :straight t
;;   :after lsp-mode
;;   :mode ("\\.go\\'" . go-mode)
;;   :preface
;;   (defun rk-go--modules-p ()
;;     "Return non-nil if this buffer is part of a Go Modules project."
;;     (locate-dominating-file default-directory "go.mod"))

;;   (defun rk-go--setup-go ()
;;     "Run setup for Go buffers."
;;     (if (rk-go--modules-p)
;;         (setenv "GO111MODULE" "on")
;;       (setenv "GO111MODULE" "auto"))
;;     (if-let ((gopath (getenv "GOPATH")))
;;         (setq lsp-go-gopls-server-path (f-join gopath "bin/gopls")))
;;     (lsp-deferred))
;;   :hook
;;   (go-mode . rk-go--setup-go))

;; (add-hook 'paredit-mode-hook #'clojure-paredit-setup nil 'local)

;; (add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)

;; '(display-buffer-alist '(("\\*Buffer List\\*" (my-switch-to-buffer-list))))

(use-package cc-go-run
  :straight nil
  :after go-mode
  :bind (:map go-mode-map
              ("C-c l t ." . cc/go-run-test-current-function)
              ("C-c l t t" . cc/go-run-test-current-suite)
              ("C-c l t p" . cc/go-run-test-current-package)
              ("C-c l t P" . cc/go-run-test-current-packages)
              ("C-c l t n" . cc/go-run-test-with-names)
              ("C-c l e s" . cc/go-run-on-save)
              ("C-c l e e" . cc/go-run-main))
  :preface
  (defvar cc/go-run-on-save nil)
  (defun cc/go-run-on-save-toggle ()
    "Toggle run current main file after save."
    (if cc/go-run-on-save
        (remove-hook 'after-save-hook 'cc/go-run-main t)
      (add-hook 'after-save-hook 'cc/go-run-main t))
    (setq cc/go-run-on-save (not cc/go-run-on-save))))

;; (use-package rk-go-run
;;   :after go-mode
;;   :init
;;   (rk-local-leader-def :keymaps 'go-mode-map
;;     "."   '(gofmt :wk "fmt")
;;     "t"   '(:ignore t :wk "test")
;;     "t t" '(rk-go-run-test-current-function :wk "current fn")
;;     "t s" '(rk-go-run-test-current-suite :wk "current suite")
;;     "t p" '(rk-go-run-package-tests :wk "package")
;;     "t P" '(rk-go-run-package-tests-nested :wk "package (nested)")
;;     "x"   '(rk-go-run-main :wk "run"))
;;   :config
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx bos "*go " (or "test" "run") "*" eos)
;;                  (display-buffer-reuse-window
;;                   display-buffer-in-side-window)
;;                  (reusable-frames . visible)
;;                  (side            . bottom)
;;                  (slot            . 0)
;;                  (window-height   . 0.2))))

;; JavaScript
(use-package js-mode
  :straight (:type built-in)
  :mode "\\.jsx?\\'"            ; TODO: add other file types, see BIOME_FILES_RX
  :hook ((js-mode . lsp)
         (js-mode . cc/--setup-js))
  :preface
  (defun cc/--setup-js ()
    "Configure JavaScript buffer."
    (apheleia-mode -1)
    (setq-local lsp-enable-on-type-formatting nil
                lsp-enable-indentation nil))
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset t))

;; TypeScript
(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (typescript-ts-mode . cc/--setup-ts)
         (tsx-ts-mode . cc/--setup-ts))
  :preface
  (defun cc/--setup-ts ()
    "Configure TypeScript buffer."
    (apheleia-mode -1)
    (setq-local lsp-enable-on-type-formatting nil
                lsp-enable-indentation nil))
  :custom
  (typescript-ts-mode-indent-offset 2))

;; Haskell
(use-package haskell-mode
  :hook ((haskell-mode . lsp))
  :custom
  ;; (haskell-prompt-regexp "^\\(> *\\|| *\\)+")
  (haskell-prompt-regexp "^ghci[^>]*>")
  (haskell-process-type 'ghci)
  (haskell-process-path-ghci (executable-find "ghci"))
  (haskell-process-args-ghci nil)
  (inferior-haskell-root-dir "/tmp")
  :config
  (require 'cc-haskell))

(use-package lsp-haskell
  :requires haskell
  :custom
  (lsp-log-io nil)
  (lsp-haskell-plugin-eval-global-on t)
  (lsp-haskell-plugin-import-lens-global-on t)
  (lsp-haskell-plugin-hlint-global-on t))

(use-package consult-hoogle
  :straight (consult-hoogle :type git :host nil :repo "https://codeberg.org/rahguzar/consult-hoogle")
  :after consult
  :bind (:map haskell-mode-map
              ("C-c h p" . consult-hoogle-project)))

;; C
(use-package cc-mode
  :straight (:type built-in)
  :hook ((c-mode . lsp))
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode))
  :bind (:map c-mode-map
              ("C-c C-c" . compile)))

;; Zig
(use-package zig-mode
  :hook ((zig-mode . lsp)))

;; Scala
(use-package scala-ts-mode
  :hook ((scala-ts-mode . cc/--setup-scala))
  :mode (("\\.scala\\'" . scala-ts-mode)
         ("\\.sbt\\'" . scala-ts-mode)
         ("\\.sc\\'" . scala-ts-mode))
  :preface
  (defun cc/ensure-scala-ts-mode ()
    "Ensure scala-ts-mode is used instead of scala-mode for all Scala files."
    ;; Remove all scala-mode associations from auto-mode-alist
    (setq auto-mode-alist
          (seq-remove (lambda (pair)
                        (eq (cdr pair) 'scala-mode))
                      auto-mode-alist))
    ;; Ensure scala-ts-mode is used for all Scala files
    (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-ts-mode)))
  (defun cc/--setup-scala ()
    (setq-local treesit-font-lock-level 4
                scala-indent:step 2)
    (eldoc-mode -1)
    (treesit-font-lock-recompute-features)
    (lsp)
    (company-mode)))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :after scala-ts-mode  ; Load after scala-ts-mode to ensure proper ordering
  :config
  (cc/ensure-scala-ts-mode)
  :custom
  ;; TODO: create a transient menu to enable/disable inlays
  (lsp-metals-inlay-hints-enable-implicit-arguments t)
  (lsp-metals-inlay-hints-enable-implicit-conversions t)
  (lsp-metals-inlay-hints-enable-inferred-types t)
  (lsp-metals-inlay-hints-enable-type-parameters t)
  (lsp-metals-inlay-hints-enable-hints-in-pattern-match t)
  (lsp-metals-install-scala-version "3.3.6"))

(with-eval-after-load 'scala-mode
  (cc/ensure-scala-ts-mode))

;; Protobuf mode
(use-package protobuf-mode
  :mode (("\\.proto$" . protobuf-mode)
         ("\\.proto3$" . protobuf-mode))
  :hook ((protobuf-mode . lsp)))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("protobuf-language-server" "-stdio"))
                    :activation-fn (lsp-activate-on "protobuf")
                    :server-id 'protobuf-language-server
                    :download-server-fn (lambda (_client callback error-callback _update?)
                                          (lsp-async-start-process
                                           callback
                                           error-callback
                                           "go" "install" "github.com/lasorda/protobuf-language-server@latest")))))

;; Agda
(when (executable-find "agda-mode")
  (let ((coding-system-for-read 'utf-8)
        (agda-mode-source-path (shell-command-to-string "agda-mode locate")))
    (load-file agda-mode-source-path)
    (native-compile-async (list agda-mode-source-path) nil t))

  (with-eval-after-load 'agda2
    (setq agda2-backend "GHC")

    (defun agda2-align-equality-reasoning-block ()
      "Align proofs in equality reasoning blocks between `begin' and `∎'"
      (interactive)
      (let ((region (if (region-active-p)
                        (cons (region-beginning) (region-end))
                      (cons (point-min) (point-max)))))
        (align-regexp (car region) (cdr region) "\\(\\s-*\\)≡⟨.*⟩$")))

    (defun cc/--setup-agda ()
      "Setup Agda major mode after loading in buffer"
      (keymap-set agda2-mode-map "C-c C-q"
                  (lambda ()
                    (interactive)
                    (dolist (buf (buffer-list))
                      (with-current-buffer buf
                        (when (equal (buffer-name buf) "*Agda information*")
                          (let ((windows (get-buffer-window-list buf)))
                            (dolist (win windows)
                              (delete-window win))))))))
      ;; TODO: agda settings
      ;; (setq agda2-program-args '("--show-implicit")))
      )

    (add-hook 'agda2-mode-hook #'cc/--setup-agda)))

;; Misc
(use-package font-lock-studio)

;; Org
(use-package ob-http)
(use-package ob-mongo)
(use-package ob-mermaid)
(use-package org-present)
(use-package org
  :preface
  (defun cc/org-move-up ()
    "Move current element up based on context.
If at item, move item up. If at subtree, move subtree up.
Otherwise use drag-stuff-up."
    (interactive)
    (cond
     ((org-at-item-p) (org-move-item-up))
     ((org-at-heading-p) (org-move-subtree-up))
     ((org-at-table-p) (org-table-move-row 'up))
     (t (drag-stuff-up))))
  (defun cc/org-move-down ()
    "Move current element down based on context.
If at item, move item down. If at subtree, move subtree down.
Otherwise use drag-stuff-down."
    (interactive)
    (cond
     ((org-at-item-p) (org-move-item-down))
     ((org-at-heading-p) (org-move-subtree-down))
     ((org-at-table-p) (org-table-move-row 'down))
     (t (drag-stuff-down))))
  :bind (("C-c c" . org-capture)
         ("C-M-<return>" . org-insert-todo-subheading)
         :map org-mode-map
         ("M-<up>" . cc/org-move-up)
         ("M-<down>" . cc/org-move-down)
         ("M-<right>" . org-demote-subtree)
         ("M-<left>" . org-promote-subtree)
         ("C-c k C-l" . org-insert-link)
         ("C-x k s" . org-cut-subtree)
         ("C-c C-x C-i" . org-clock-in)
         ("C-c C-x C-o" . org-clock-out))
  :custom
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation nil)
  (org-src-tab-acts-natively t)
  (org-src-fontify-natively t)
  (org-image-actual-width nil)
  (org-src-mode t)
  (org-pretty-entities t)
  (org-use-property-inheritance t)
  (org-confirm-babel-evaluate nil)
  (org-catch-invisible-edits 'error)
  (org-tags-column -100)
  (org-startup-indented t)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-link-frame-setup '((file . find-file)))
  (org-support-shift-select 'always)
  (org-tag-persistent-alist '(("drill" . ?r)
                              ("doing" . ?d)
                              ("next" . ?n)
                              ("today" . ?t)
                              ("blocked" . ?b)))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (haskell . t)
     (http . t)
     (mongo . t)
     (mermaid . t)
     (sql . t)
     (js . t)
     (shell . t))))

;; Bindings
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c C-p" #'cc/open-line-above)
(bind-key "C-c C-n" #'cc/open-line-below)
;;; TODO replace with other bindings C-c d enter special keymap `j` or `n` copy below, `k` or `p` copy above
;; (bind-key "H-u" #'cc/copy-character-from-above)
;; (bind-key "H-d" #'cc/copy-character-from-below)
(bind-key "C-c w <right>" #'windmove-swap-states-right)
(bind-key "C-c w <left>" #'windmove-swap-states-left)
(bind-key "C-c w <up>" #'windmove-swap-states-up)
(bind-key "C-c w <down>" #'windmove-swap-states-down)
(bind-key "C-c w l" #'windmove-swap-states-right)
(bind-key "C-c w h" #'windmove-swap-states-left)
(bind-key "C-c w k" #'windmove-swap-states-up)
(bind-key "C-c w j" #'windmove-swap-states-down)
(bind-key "C-a" #'cc/smarter-move-beginning-of-line)
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
(bind-key "C-c k k" #'kill-whole-line)
(bind-key "C-c k l" #'org-store-link)
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
;; The following is needed to override a strange behaviour from latest org-mode
;; which ovverides the standard keybiding
(bind-key* "C-x C-s" #'save-buffer)
;; Using gptel
(bind-key "C-c i n" #'cc/gptel-guess-next-line)
(bind-key "C-c i N" #'cc/gptel-guess-next-n-lines)
(bind-key "C-c i C-n" #'cc/gptel-guess-complete-pattern)

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

;;; Small functions to relocate somewhere else

(defun cc/shr-render-current-buffer ()
  "Render current HTML buffer."
  (interactive)
  (unless (eq major-mode 'mhtml-mode) (error "Not an HTML buffer, nothing to render here"))
  (shr-render-buffer (current-buffer))
  (switch-to-buffer "*html*")
  (read-only-mode t))

;; VERSION=emacs-30.1
;; CFLAGS="-O2 -fno-semantic-interposition -floop-parallelize-all -ftree-parallelize-loops=4 -g3"
;; ./configure --prefix=~/opt/$VERSION --bindir=~/opt/$VERSION/bin --with-tree-sitter --with-imagemagick --with-pgtk --without-dbus --with-native-compilation=aot --with-wide-int --with-mailutils
;; make -j"$(nproc)" && make install
;; ln -ls "$HOME/opt/emacs-30.0.92/bin/emacs" "$HOME/.local/bin/emacs-30.0.92"

;;; BUILD INSTALL LATEST IGC BRANCH
;; export VERSION=emacs-igc
;; export CFLAGS="-Os -march=native -mtune=native -fno-semantic-interposition -floop-parallelize-all -ftree-parallelize-loops=4 -g0"
;; # You will need the mps library, checkout the library https://github.com/Ravenbrook/mps.git compile and install artifacts into /home/coder/opt/mps
;; export CPPFLAGS="-I/home/coder/opt/mps"
;; export LDFLAGS="-L/home/coder/opt/mps"
;; ./configure --prefix=~/opt/$VERSION --bindir=~/opt/$VERSION/bin --with-tree-sitter --with-imagemagick --with-pgtk --without-dbus --with-native-compilation=aot --with-wide-int --with-mailutils --with-mps
;; make -j"$(nproc)" && make install
;; ln -ls "$HOME/opt/$VERSION/bin/emacs" "$HOME/.local/bin/$VERSION"

;; TODO: gptel explore presets and how to select them
;; TODO: gptel-prompt to compose prompts
;; TODO: add prompt "Never use first level heading in your response, only use headings from level 2 to level 6."
;; TODO: add prompt to instruct on using tools
;; TODO: add prompt to agentic coding (https://github.com/gregoryg/AIPIHKAL/blob/main/the-one-true-holy-and-apostolic-coding-and-tool-usage-prompt.org)
;; TODO: add prompt to look at /docs/TOOLS.md with libraries and the url where an LLM can find their documentation

;; https://github.com/ksqsf/emacs-config/blob/99efd403d4b8be96e612a8d2ad2f7a9b42fba098/modules/prelude-ai.el

;; TODO: tools category emacs_reflection (https://github.com/gregoryg/emacs-gregoryg?tab=readme-ov-file#custom-tools)
;; TODO: - emacs_package_readme
;; TODO: - emacs_package_description
;; TODO: - emacs_package_source
;; TODO: - emacs_packages_list
;; TODO: - emacs_function_source
;; TODO: - emacs_function_documentation
;; TODO: - emacs_variable_documentation
;; TODO: - emacs_lisp_eval

;; TODO: tools category agentic
;; TODO: - current_directory probably unnecessary?
;; TODO: - list_files
;; TODO: - read_file
;; TODO: - write_file
;; TODO: - run_format
;; TODO: - run_checks
;; TODO: - run_tests

;; TODO: tools category web
;; TODO: - search
;; TODO: - read_url

;; TODO: copy the (flycheck) error at point
;; TODO: open the documentation of thing at point given lsp-mode

;; TODO: evil with setup so that emacs mode is the default
;; TODO: rename all local packages in cc-*
;; FIX: completing in interactive-haskell-mode is not done through vertico/orderless/marginalia..
;; TODO: hydra, look at pretty-hydra (https://github.com/jerrypnz/major-mode-hydra.el)
;; TODO: modeline with https://github.com/seagle0128/doom-modeline or https://gitlab.com/jessieh/mood-line
;; FIX: lisp indent with https://gitlab.com/magus/mes/-/blob/main/lisp/mes-dev-elisp.el?ref_type=heads#L19
;; TODO: look-at https://gitlab.com/magus/mes/-/blob/main/lisp/mes-usability.el
;; - https://gitlab.com/magus/mes/-/blob/main/lisp/mes-usability.el#L238

;; PROJECT: Minor mode for Rust to be able to run `cargo build` on the current
;; project, split on every error, create a read only buffer to be able to
;; navigate through the errors. The original source file should be linked. On
;; the bottom of the buffer should be the full explanation of the error retrived
;; by cargo

;; FEATURE: H-f/H-b forward-symbols/backward-symbols jump to next/previous group
;; of non words, learn about syntax table of current major mode

;; LOOK-AT: https://github.com/akirak/emacs-config/blob/b85e113743ba2ec4e074e40a2e66da190f66eae2/emacs-config.org
;; LOOK-AT: https://github.com/ksqsf/emacs-config/blob/99efd403d4b8be96e612a8d2ad2f7a9b42fba098/modules
;; TODO: Hoogle for Haskell?
;; TODO: consult-lsp
;; TODO: Create a shell *root-shell* when emacs starts, add keybinding to switch to it
;; TODO: YAML language server https://github.com/redhat-developer/yaml-language-server
;; TODO: Top level comments with triple ; to work as outlines as per documentation https://www.gnu.org/software/emacs/manual/html_node/elisp/Comment-Tips.html
;; TODO: M-q should not join line which begins with `-`,`TODO`,...
;; TODO: expand-region or combobulate
;; TODO: dabbrev
;; TODO: hippie-exp
;; TODO: language Elixir
;; TODO: Makefile
;; TODO: Dockerfile linter ???
;; TODO: Dockerfile language server ???
;; TODO: OpenAI integration
;; TODO: NYXT integration
;; TODO: TLA+
;; TODO: Grammarly integration
;; TODO: Dictionary integration
;; TODO: try theme https://github.com/catppuccin/emacs
;; TODO: replace Cask with https://emacs-eldev.github.io/eldev ???
;; TODO: spell checker https://github.com/minad/jinx
;; TODO: look at https://github.com/minad/cape
;; TODO: grok consult
;; TODO: grok embark
;; TODO: grok magit
;; TODO: grok dabbrev
;; TODO: Better than rg ? https://github.com/Wilfred/deadgrep
;; TODO: https://www.reddit.com/r/emacs/comments/179t67l/window_management_share_your_displaybufferalist
;; TODO: H-v rename buffer with NAME-PROJECT-P
;; TODO: H-v in dired will create a terminal in that directory SHELL-DIRED
;; TODO: create haskell-playground like rust-playground
;; TODO: show full error at point, depends on major mode, flycheck doesn't have the original message

(require 'personal-functions-after)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
