;; Try UTF-8 first when detecting encoding.
(prefer-coding-system 'utf-8)
;; Use UTF-8 for storing, input and output.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
 auto-hscroll-mode 'current-line
 elpy-rpc-python-command "python3")

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

(delete-selection-mode t)
(savehist-mode)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(display-battery-mode 1)
(setq sml/battery-format "%t")

(defun indy/2k-scaled ()
  (interactive)
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 140))
(defun indy/2k ()
  (interactive)
  (set-face-attribute 'default nil :font "Ubuntu Mono" :height 165))
(indy/2k)

;; answer y or n instead of long yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets
(show-paren-mode 1)

(defun indy-highlight-trailing-whitespace ()
  "Turn on trailing whitespace highlighting."
  (setq-local show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'indy-highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'indy-highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'copyright-update)

;; a SpaceKnow hack requires some rst files to end with .pub
(add-to-list 'auto-mode-alist '("\\.pub\\'" . rst-mode))
(add-to-list 'auto-mode-alist '("\\(\\/\\.?zshrc\\|\\.zsh\\)\\'" . sh-mode))

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
(use-package monokai-theme
  :ensure t
  :demand t
  :load-path "themes"
  :config
  (setq sml/no-confirm-load-theme t)
  (load-theme 'monokai t))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (sml/setup)
  (setq sml/hidden-modes ".*"))

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
  (setq org-default-notes-file "~/notes/notes.org"
        org-agenda-files '("~/notes/todo.org")
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

(defun indy/py-isort ()
  "Sort Python imports only on desired files."
  (interactive)
  (when (and buffer-file-name
             (not (string-match "backend/\\(scripts\\|devtools\\)"
                                buffer-file-name)))
      (py-isort-before-save)))

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
    (add-hook 'before-save-hook 'indy/py-isort)))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package racer
    :ensure t
    :demand t
    :config
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (define-key rust-mode-map (kbd "C-c d") #'racer-describe)))

;; awesome Emacs interface to Git porcelain
(use-package magit
  :ensure t
  :after ivy
  :bind
  ("C-x g" . magit-status)
  :config
  (defun indy/magit-push-to-gerrit ()
    (interactive)
    (magit-git-command-topdir "push gerrit HEAD:refs/for/master"))
  (magit-define-popup-action
    'magit-push-popup ?m "Push to Gerrit" 'indy/magit-push-to-gerrit)
  (setq git-commit-summary-max-length 50
        magit-completing-read-function 'ivy-completing-read)
  (add-hook 'git-commit-mode-hook
            (lambda () (setq-local fill-column 72))))

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

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

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

(when (file-exists-p custom-file)
  (load custom-file))
