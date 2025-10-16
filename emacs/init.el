;; Try UTF-8 first when detecting encoding.
(prefer-coding-system 'utf-8)
;; Use UTF-8 for storing, input and output.
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(load-file "~/dotfiles/emacs/packages.el")

(require 'uniquify)

(setq
 inhibit-startup-message t
 column-number-mode t
 uniquify-buffer-name-style 'forward
 tab-width 4
 view-read-only t
 ispell-program-name "aspell"
 ;; -l is mandatory, -h for human readable sizes, -a to see everything
 dired-listing-switches "-lha"
 require-final-newline "ask"
 mode-require-final-newline "ask"
 dired-dwim-target t
 enable-local-variables nil
 visible-bell t
 save-interprogram-paste-before-kill t)

(setq-default
 indent-tabs-mode nil
 fill-column 79
 truncate-lines t)

(delete-selection-mode t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(global-auto-revert-mode 1)
(add-to-list 'default-frame-alist '(undecorated . t))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun indy/highlight-trailing-whitespace ()
  "Turn on trailing whitespace highlighting."
  (setq-local show-trailing-whitespace t)
  (setq-local indicate-empty-lines t))

(add-hook 'prog-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'indy/highlight-trailing-whitespace)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(load-file "~/dotfiles/emacs/theme.el")

(use-package xclip
  :straight t
  :config
  (xclip-mode 1))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package ivy
  :straight t
  :config
  (ivy-mode 1))

(use-package counsel
  :straight t
  :config
  (defun indy/git-grep (dir)
    (interactive "D")
    (counsel-git-grep nil dir))
  :bind
  (("M-g g" . indy/git-grep)))

(use-package wgrep
  :straight t)

(use-package perspective
  :straight t
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  :config
  (persp-mode))

(use-package crux
  :straight t
  :config
  (crux-reopen-as-root-mode))

(use-package goto-last-change
  :straight t
  :bind
  (("M-g l" . goto-last-change-with-auto-marks)))

(use-package ace-window
  :straight t
  :bind
  (("C-x o" . ace-window))
  :config
  (setq aw-scope 'frame))

(use-package projectile
  :straight t
  :bind
  (("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

(use-package yaml-mode
  :straight t)

(use-package just-mode
  :straight t)

(load-file "~/dotfiles/emacs/magit.el")

(load-file "~/dotfiles/emacs/lsp.el")
(load-file "~/dotfiles/emacs/rust.el")
(load-file "~/dotfiles/emacs/python.el")

(load-file "~/dotfiles/emacs/utils.el")

(require 'server)
(unless (server-running-p)
  (server-start))
