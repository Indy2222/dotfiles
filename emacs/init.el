(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
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
(load-theme 'material t)

;; IDO -- for better auto-completition
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; answer y or n instead of full yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Who use the bar to scroll?
;; (scroll-bar-mode -1) ;; not needed in text terminals
;; Don't split window vertically unless its large
;; I have ~83 lines on 26" at work
(setq split-height-threshold 100)
;; once you learn Emacs this becomes only annoying
(setq inhibit-startup-message t)
;; menu is for noobs
(menu-bar-mode -1)

;; to avoid leftover trailing whitespace in sourcodes and text files
(defun highlight-trailing-whitespace () (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'highlight-trailing-whitespace)

;; (add-hook 'prog-mode-hook 'linum-mode)
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

;; save emacs session
;; (desktop-save-mode 1)
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


;; org mode
(require 'org)
(setq org-default-notes-file "~/notes/notes.txt")

;; from http://orgmode.org/guide/Activation.html#Activation
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; calendar
;; install czech-holidays package
(require 'czech-holidays)
(czech-holidays-add)
(setq calendar-latitude 50.098723)
(setq calendar-longitude 14.407082)
(setq calendar-location-name "Prague, Czech Republic")

(setq calendar-time-zone +60)
(setq calendar-standard-time-zone-name "CET")
(setq calendar-daylight-time-zone-name "CEST")
(setq calendar-daylight-time-offset +60)
(setq calendar-daylight-savings-starts (calendar-nth-named-day -1 0 3 2017))
(setq calendar-daylight-savings-ends (calendar-nth-named-day -1 0 10 2017))
(setq calendar-daylight-savings-starts-time +120) ;; this is local time
(setq alendar-daylight-savings-ends-time +180)

;; xclip over x11Forwarding
(xclip-mode 1)

;; mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; use mu4e as default emacs email client
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir "~/mail/sk-gmail")
(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; something about ourselves
(setq mu4e-compose-signature
      (concat "Martin Indra\n" "Chief Software Architect"))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; tell message-mode how to send mail
(setq message-send-mail-function 'smtpmail-send-it)
;; if our mail server lives at smtp.example.org; if you have a local
;; mail-server, simply use 'localhost' here.
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-default-smtp-server "smtp.gmail.com")
(setq smtpmail-stream-type 'starttls)
(setq smtpmail-smtp-service 587)

;; GPG interface, work with .gpg files like they are plain text
(require 'epa-file)
(epa-file-enable)
;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
(setf epa-pinentry-mode 'loopback)

(add-to-list 'auto-mode-alist '("\\.geojson\\'" . json-mode))

;; so emacsclient can be used
(require 'server)
(unless (server-running-p)
  (server-start))

;; generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode zenburn-theme czech-holidays igrep dockerfile-mode markdown-mode elpy column-marker magit flycheck material-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
