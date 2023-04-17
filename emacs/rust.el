(use-package rustic
  :ensure t
  :custom
  (rustic-ansi-faces ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#93a1a1"])
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-compile-directory-method 'rustic-buffer-workspace)
  (rustic-rustfmt-args "+nightly")
  (rustic-format-trigger 'on-save)
  (rustic-default-clippy-arguments "--workspace --benches --tests --all-features")
  (rustic-default-test-arguments "--workspace --tests --all-features"))
