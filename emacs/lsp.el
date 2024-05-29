(use-package lsp-mode
  :straight t
  :bind ("C-c d" . lsp-describe-thing-at-point)
  :init
  (setq lsp-keymap-prefix "s-l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
