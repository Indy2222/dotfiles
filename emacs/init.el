;; Try UTF-8 first when detecting encoding.
(prefer-coding-system 'utf-8)
;; Use UTF-8 for storing, input and output.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'uniquify)
(require 'desktop)

(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/")
                    ("elpy" . "https://jorgenschaefer.github.io/packages/"))
 ;; not useful once you learn Emacs
 inhibit-startup-message t
 ;; full screen terminal on my laptop has 50 lines (including tmux,
 ;; mode lines, etc.)
 split-height-threshold 100
 ;; display column number in modeline
 ;; TODO: is this implied with xx?
 column-number-mode t
 uniquify-buffer-name-style 'forward
 ;; there may be only one space after a sentence
 ;; TODO:
 ;;  * Learn to use double spaces.
 ;;  * Change this based on buffer.
 sentence-end-double-space nil
 ;; display read only buffers in view-mode for better scrolling
 view-read-only t
 ;; two is too little, eight is too much
 tab-width 4
 ispell-dictionary "american"
 ;; set which ls switches Dired uses
 ;; -l is mandatory, -h for human readable sizes, -a to see everything
 dired-listing-switches "-lha"
 ;; I don't want Custom to mess up with this file
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; Ask when new line is missing at the end of file
 require-final-newline "ask"
 mode-require-final-newline "ask"
 dired-dwim-target t
 tls-checktrust t
 elpy-rpc-python-command "python3"
 flyspell-issue-message-flag nil
 vc-handled-backends nil
 desktop-save 'ask-if-new
 desktop-dirname "~/.emacs.d/desktop"
 enable-local-variables nil)

(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-modified
        mode-line-remote
        " "
        mode-line-position
        " "
        mode-name
        " "
        mode-line-buffer-identification
        " "
        mode-line-misc-info
        mode-line-end-spaces))

(setq-default
 ;; tabs are cool but non of the project I participate on use them
 indent-tabs-mode nil
 ;; Python's PEP8's recommendation but good for all text files
 ;; 79 columns still leave some space on my laptop even with a large font
 fill-column 79
 ;; Truncate lines instead of wrapping them.
 truncate-lines t)

(global-set-key "\M- " 'hippie-expand)

;; enable fixing vertical movement column
(put 'set-goal-column 'disabled nil)
;; enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; enable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;; enable horizontal scrolling
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)

(desktop-save-mode t)
(delete-selection-mode t)
(savehist-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq default-frame-alist '((width . 100) (height . 25)
      (vertical-scroll-bars)))

(set-face-attribute 'default nil :font "UbuntuMono" :height 150)

;; answer y or n instead of long yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets
(show-paren-mode 1)

(defun indy/highlight-trailing-whitespace ()
  "Turn on trailing whitespace highlighting."
  (setq-local show-trailing-whitespace t)
  (setq-local indicate-empty-lines t))

(add-hook 'prog-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'copyright-update)

(add-to-list 'auto-mode-alist '("\\(\\/\\.?zshrc\\|\\.zsh\\)\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.*/neomutt-.*\\'" . text-mode))

;; Install use-package if not already installed.
(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package in the rest of the file for installation and configuration
;; of all packages.
(require 'use-package)

;; this one is among the most popular Emacs themes for a reason
(use-package zenburn-theme
  :ensure t
  :demand t
  :load-path "themes"
  :config
  (load-theme 'zenburn t))

;; spellcheck on the fly
(use-package avy-flycheck
  :ensure t
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

;; jumping over visible parts of displayed buffers
(use-package avy
  :ensure t
  :bind
  (("M-g e" . avy-goto-word-0)
   ("M-g f" . avy-goto-line)))

;; better selection narrowing
(use-package ivy
  :ensure t
  ;; make :after in magit load properly
  :demand t
  :bind
  (("C-c C-r" . ivy-resume))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  (use-package counsel
    :ensure t
    :bind
    (("M-g g" . counsel-git-grep)
     ("M-g a" . counsel-ag))))

(use-package counsel-projectile
  :ensure t
  :demand t
  :bind
  (("s-j" . counsel-projectile-find-file)
   ("s-i" . counsel-projectile-switch-project)
   ("s-k" . projectile-toggle-between-implementation-and-test)
   ("s-u" . projectile-find-implementation-or-test-other-window))
  :config
  (counsel-projectile-mode))

(use-package switch-window
  :ensure t
  :bind
  (("C-x o" . switch-window)
   ("C-x 4 s" . switch-window-then-swap-buffer)
   ("C-x 4 d" . switch-window-then-dired)
   ("C-x 4 f" . switch-window-then-find-file)
   ("C-x 4 m" . switch-window-then-compose-mail)
   ("C-x 4 r" . switch-window-then-find-file-read-only)
   ("C-x 4 C-f" . switch-window-then-find-file)
   ("C-x 4 C-o" . switch-window-then-display-buffer))
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-shortcut-appearance 'asciiart))

(use-package org
  :ensure t
  :bind
  (("C-c l" . org-capture)
   ("C-c a" . org-agenda))
  :config
  (setq org-catch-invisible-edits 'error
        org-default-notes-file "~/notes/notes.org"
        org-agenda-files '("~/notes/")
        org-capture-templates
        '(("t" "Todo" entry
           (file "~/notes/todo.org")
           "* TODO %?\n\n%a\n%i\n")
          ("n" "Notes" entry
           (file "~/notes/notes.org")
           "* %? %U\n\n%a\n%i\n")
          ("j" "Journal" entry
           (file+datetree (concat "~/diary/" (format-time-string "%Y") ".org.gpg"))
           "* %?" :empty-lines 1))))

(use-package json-mode
  :ensure t
  ;; so even .geojson works out
  :mode "\\.[a-z]*json\\'")

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")

;; python package provides python-mode
(use-package python
  :ensure t
  :mode
  ("\\.py\\'" . python-mode)
  :interpreter
  ("python" . python-mode)
  :config
  ;; Python environment
  (use-package elpy
    :ensure t
    :demand t
    :after python
    :config
    (elpy-enable)
    (define-key python-mode-map (kbd "M-.") #'elpy-goto-definition))
  (use-package sphinx-doc
    :ensure t
    :demand t
    :config
    (add-hook 'python-mode-hook (lambda () (sphinx-doc-mode t))))
  (use-package py-isort
    :ensure t
    :demand t
    :config
    (add-hook 'before-save-hook 'py-isort-before-save)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package racer
    :ensure t
    :demand t
    :config
    (setq rust-format-on-save t)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (define-key rust-mode-map (kbd "C-c d") #'racer-describe)))

;; awesome Emacs interface to Git porcelain
(use-package magit
  :ensure t
  :after ivy
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package magit-todos
    :ensure t
    :demand t)
  (setq git-commit-summary-max-length 50
        magit-completing-read-function 'ivy-completing-read)
  (add-hook 'git-commit-mode-hook
            (lambda () (setq-local fill-column 72)))
  (global-magit-file-mode t))

(use-package google-translate
  :ensure t
  :init
  (setq google-translate-default-source-language "cs"
        google-translate-default-target-language "en")
  :bind
  ;; C-c t -- from text
  ;; C-c T -- from mind
  (("C-c t" . google-translate-at-point-reverse)
   ("C-c T". google-translate-query-translate))
  :config
  (require 'google-translate-default-ui))

(use-package wttrin
  :ensure t
  :config
  (setq wttrin-default-cities '("Prague")
        wttrin-default-accept-language '("Accept-Language" . "en-US")))

(use-package keyfreq
  :ensure t
  :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package dashboard
  :ensure t
  :demand t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Let the hacking begin!"
        dashboard-items '((recents . 10)
                          (agenda . 10)
                          (projects . 3)))
  (dashboard-setup-startup-hook))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package wgrep
  :ensure t
  :demand t)

(use-package mediawiki
  :ensure t
  :demand t)

(use-package csv-mode
  :ensure t
  :demand t)

(use-package ess
  :ensure t
  :demand t
  :config
  (setq ess-tab-complete-in-script t))

(use-package jdee
  :ensure t
  :demand t)

(use-package guess-language
  :ensure t
  :demand t
  :config
  (setq guess-language-languages '(en cs))
  (add-hook 'text-mode-hook (lambda () (guess-language-mode 1))))

(use-package dockerfile-mode
  :ensure t
  :demand t)

(use-package poly-markdown
  :ensure t
  :demand t)

(use-package adoc-mode
  :ensure t
  :demand t)

(use-package omnisharp
  :ensure t
  :demand t
  :config
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  ;; omnisharp uses Company Mode for code competition.
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook #'company-mode)
  (define-key csharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition))

(use-package arduino-mode
  :ensure t)

(use-package company
  :demand t
  :ensure t
  :config
  (setq company-show-numbers 1))

(use-package eyebrowse
  :demand t
  :ensure t
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t))

(use-package multi-term
  :demand t
  :ensure t
  :config
  (use-package eterm-256color
    :ensure t
    :demand t
    :config
    (add-hook 'term-mode-hook #'eterm-256color-mode))
  (setq multi-term-program "/usr/bin/zsh")
  (setq-default term-buffer-maximum-size 131072)
  (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
  (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
  (add-hook 'term-mode-hook (lambda ()
                              (setq-local column-number-mode nil)
                              (setq-local line-number-mode nil))))

(use-package crux
  :demand t
  :ensure t
  :config
  (crux-reopen-as-root-mode))

(use-package goto-last-change
  :demand t
  :ensure t
  :bind
  (("M-g l" . goto-last-change-with-auto-marks)))

(defun indy/kill-file-name ()
  "Place buffer-file-name kill ring and display it."
  (interactive)
  (if buffer-file-name
      (progn (kill-new buffer-file-name)
             (message "Buffer file: %s" buffer-file-name))
    (error "Current buffer is not visiting a file!")))

(global-set-key "\C-cif" 'indy/kill-file-name)

(defun indy/date-time ()
    "Place current date-time to kill ring and display it."
    (interactive)
    (let ((date-time-string (format-time-string "%Y-%m-%d %T")))
      (kill-new date-time-string)
      (message "Current date-time: %s" date-time-string)))

(global-set-key "\C-cid" 'indy/date-time)

(when (file-exists-p custom-file)
  (load custom-file))

(server-start)
