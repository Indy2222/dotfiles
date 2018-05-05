;; Try UTF-8 first when detecting encoding.
(prefer-coding-system 'utf-8)
;; Use UTF-8 for storing, input and output.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
		    ("melpa" . "http://melpa.org/packages/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/"))
 ;; not useful once you learn Emacs
 inhibit-startup-message t
 ;; full screen terminal on my laptop has 50 lines (including tmux,
 ;; mode lines, etc.)
 split-height-threshold 60
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
 require-final-newline t)

(setq-default
 ;; tabs are cool but non of the project I participate on use them
 indent-tabs-mode nil
 ;; Python's PEP8's recommendation but good for all text files
 ;; 79 columns still leave some space on my laptop even with a large font
 fill-column 79)

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

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Ubuntu Mono" :height 160)

;; answer y or n instead of long yes and no
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1)
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
(use-package zenburn-theme
  :ensure t
  :demand t
  :load-path "themes"
  :config
  (load-theme 'zenburn t))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config
  (sml/setup))

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

;; GPG interface, work with .gpg files like they are plain text
(use-package epa-file
  :config
  ;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
  (setf epa-pinentry-mode 'loopback)
  (epa-file-enable))

(use-package org
  :ensure t
  :bind
  ("C-c l" . org-capture)
  :config
  (setq org-default-notes-file "~/notes/notes.org"
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
    (define-key python-mode-map (kbd "M-.") #'elpy-goto-definition)))

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

(when (file-exists-p custom-file)
  (load custom-file))
