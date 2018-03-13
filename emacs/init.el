(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("elpy" . "http://jorgenschaefer.github.io/packages/")))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(package-initialize)

(setq user-full-name "Martin Indra")
(setq user-mail-address "indra@spaceknow.com")

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; default looks ugly and is difficult to read sometimes, this is way better
(load-theme 'zenburn t)

;; IDO -- for better auto-completition
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; answer y or n instead of full yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; don't split window horizontally
(setq split-height-threshold 100)
;; once you learn Emacs this becomes only annoying
(setq inhibit-startup-message t)
;; menu is for noobs
(menu-bar-mode -1)

;; to avoid leftover trailing whitespace in sourcodes and text files
(defun highlight-trailing-whitespace () (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'highlight-trailing-whitespace)

;; display column number
(setq column-number-mode t)

;; turn on the fly syntax checking
(global-flycheck-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; highlight matching brackets
(show-paren-mode 1)

;; indent with four spaces and never use \t
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; close brackets as soon as you open them
(require 'autopair)
(autopair-global-mode 1)

;; enable feature of moving vertically on fixed column
(put 'set-goal-column 'disabled nil)

(add-hook 'before-save-hook 'copyright-update)

;; git from Emacs
(global-set-key (kbd "C-x g") 'magit-status)
(setq git-commit-summary-max-length 50)

;; python specific
(require 'column-marker)
(add-hook 'python-mode-hook (lambda () (interactive) (column-marker-1 79)))
(elpy-enable)

;; C specific
(setq-default c-basic-offset 8)

;; org mode
(require 'org)
(setq org-default-notes-file "~/notes/notes.txt")

;; from http://orgmode.org/guide/Activation.html#Activation
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; xclip over x11Forwarding
(xclip-mode 1)

;; GPG interface, work with .gpg files like they are plain text
(require 'epa-file)
(epa-file-enable)
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setf epa-pinentry-mode 'loopback)

(add-to-list 'auto-mode-alist '("\\.geojson\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.pub\\'" . rst-mode))

;; the silver search (ag.el)

(require 'ag)
(add-to-list 'ag-arguments "--hidden")

(setq ispell-dictionary "american")

(setq dired-listing-switches "-lh")

(setq mm-text-html-renderer (quote gnus-w3m))

(sml/setup)

(require 'google-translate)
(require 'google-translate-default-ui)
(setq google-translate-default-source-language "cs")
(setq google-translate-default-target-language "en")
(global-set-key "\C-ct" 'google-translate-at-point-reverse)
(global-set-key "\C-cT" 'google-translate-query-translate)

(global-set-key "\M-n" 'avy-goto-char-2)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
