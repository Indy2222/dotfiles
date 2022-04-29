;; Try UTF-8 first when detecting encoding.
(prefer-coding-system 'utf-8)
;; Use UTF-8 for storing, input and output.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'uniquify)

(setq
 package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                    ("melpa" . "https://melpa.org/packages/"))
 ;; not useful once you learn Emacs
 inhibit-startup-message t
 ;; full screen terminal on my laptop has 50 lines (including tmux,
 ;; mode lines, etc.)
 split-height-threshold 100
 split-width-threshold 160
 frame-resize-pixelwise t
 ;; display column number in modeline
 ;; TODO: is this implied with xx?
 column-number-mode t
 uniquify-buffer-name-style 'forward
 ;; there may be only one space after a sentence
 sentence-end-double-space nil
 ;; display read only buffers in view-mode for better scrolling
 view-read-only t
 ;; two is too little, eight is too much
 tab-width 4
 ispell-program-name "aspell"
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
 flyspell-issue-message-flag nil
 vc-handled-backends nil
 enable-local-variables nil
 visible-bell t
 save-interprogram-paste-before-kill t
 bookmark-save-flag 1)

(setq-default mode-line-format
      '("%e"
        mode-line-front-space
        mode-line-mule-info
        mode-line-modified
        mode-line-remote
        " "
        mode-line-position
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
 truncate-lines t
 python-indent-def-block-scale 1)

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
(blink-cursor-mode 0)

(setq
 default-frame-alist '((width . 100)
                       (height . 25)
                       (vertical-scroll-bars)
                       (alpha . (100 . 100))))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 160)
(set-fontset-font t nil (font-spec :height 160 :name "Noto Color Emoji"))

(defun indy/czech ()
  "Switch input method to Czech & use Czech Ispell dictionary."
  (interactive)
  (set-input-method "czech-qwerty")
  (ispell-change-dictionary "czech"))

(defun indy/portuguese ()
  "Switch input method to Portuguese & use Portuguese Ispell dictionary."
  (interactive)
  (set-input-method "portuguese-prefix")
  (ispell-change-dictionary "portugues"))

(defun indy/english ()
  "Switch input method to Czech & use Czech Ispell dictionary."
  (interactive)
  (deactivate-input-method)
  (ispell-change-dictionary "english"))

;; answer y or n instead of long yes and no
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching brackets
(show-paren-mode 1)
(global-auto-revert-mode 1)

(defun indy/highlight-trailing-whitespace ()
  "Turn on trailing whitespace highlighting."
  (setq-local show-trailing-whitespace t)
  (setq-local indicate-empty-lines t))

(add-hook 'prog-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'before-save-hook 'copyright-update)

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

(defun indy/dark ()
  "Enable a dark theme and disable all other themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (enable-theme 'solarized-dark))

(defun indy/light ()
  "Enable a light theme and disable all other themes."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (enable-theme 'solarized-light))

(use-package solarized-theme
  :ensure t
  :demand t
  :load-path "themes"
  :config
  (setq
   x-underline-at-descent-line t
   solarized-use-variable-pitch nil
   solarized-height-minus-1 1.0
   solarized-height-plus-1 1.0
   solarized-height-plus-2 1.0
   solarized-height-plus-3 1.0
   solarized-height-plus-4 1.0
   solarized-distinct-fringe-background t
   solarized-high-contrast-mode-line nil)
  (load-theme 'solarized-light t nil)
  (load-theme 'solarized-dark t nil)
  (indy/dark))

(use-package xclip
  :ensure t
  :demand t
  :config
  (xclip-mode 1))

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
    :config
    (defun indy/git-grep (dir)
      (interactive "D")
        (counsel-git-grep nil dir))
    :bind
    (("M-g g" . indy/git-grep))))

(use-package counsel-projectile
  :ensure t
  :demand t
  :bind
  (("C-c p" . projectile-command-map)
   ("s-j"   . counsel-projectile-find-file)
   ("s-i"   . counsel-projectile-switch-project)
   ("s-k"   . projectile-toggle-between-implementation-and-test)
   ("s-u"   . projectile-find-implementation-or-test-other-window))
  :config
  (setq projectile-completion-system 'ivy)
  (counsel-projectile-mode))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)))

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


(use-package py-isort
  :ensure t
  :demand t)

(use-package lsp-mode
  :ensure t
  :bind ("C-c d" . lsp-describe-thing-at-point)
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)))

(use-package lsp-ui
    :ensure t
    :config
    (setq
     lsp-ui-sideline-update-mode 'point
     lsp-ui-sideline-show-hover nil
     lsp-ui-sideline-delay 0.1
     lsp-ui-sideline-show-code-actions nil
     lsp-ui-doc-show-with-mouse nil
     lsp-ui-doc-show-with-cursor t
     lsp-ui-doc-position 'bottom)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-pyright
  :ensure t
  :demand t
  :after (lsp-mode)
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package rustic
  :ensure t
  :config
  (setq
   rustic-analyzer-command '("rustup" "run" "nightly" "rust-analyzer")
   rustic-format-trigger 'on-save
   rustic-ansi-faces ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#93a1a1"]))

(use-package sass-mode
  :ensure t)

(use-package toml-mode
  :ensure t
  :demand t)

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
            (lambda () (setq-local fill-column 72))))

(use-package dashboard
  :ensure t
  :demand t
  :config
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-banner-logo-title "Let the hacking begin!"
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-items '((agenda   . 20)
                          (projects . 10)))
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
  (add-hook 'csharp-mode-hook #'flycheck-mode)
  (define-key csharp-mode-map (kbd "M-.") #'omnisharp-go-to-definition))

(use-package company
  :demand t
  :ensure t
  :config
  (setq company-show-numbers 1
        company-idle-delay 0.2))

(use-package perspective
  :demand t
  :ensure t
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :config
  (persp-mode))

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

(use-package dired-subtree
  :demand t
  :ensure t
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package dired-collapse
  :demand t
  :ensure t)

(use-package powershell
  :demand t
  :ensure t)

(use-package restclient
  :demand t
  :ensure t)

(use-package csv-mode
  :demand t
  :ensure t
  :config
  (setq csv-align-max-width 100))

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

(defun indy/poetry (workspace)
  "Activate Poetry env and restart lsp WORKSPACE."
  (interactive (list (lsp--read-workspace)))
  (let ((venv-path
         (string-trim
          (shell-command-to-string
           (concat
            "bash -c \"cd "
            (file-name-directory (buffer-file-name))
            " && poetry env info -p\"")))))
    (message "Working on %s" venv-path)
    (pyvenv-activate venv-path)
    (lsp-workspace-restart workspace)))

(load-file "~/dotfiles/emacs/org.el")
(load-file "~/dotfiles/emacs/mu4e.el")
(load-file "~/dotfiles/emacs/term.el")

(when (file-exists-p custom-file)
  (load custom-file))

(put 'magit-diff-edit-hunk-commit 'disabled nil)

(require 'server)
(unless (server-running-p)
    (server-start))
