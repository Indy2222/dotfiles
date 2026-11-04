;; -*- lexical-binding: t -*-

;; TypeScript & TSX via the built-in tree-sitter modes and lsp-mode.
;;
;; Requires `typescript-language-server' on PATH (npm install -g
;; typescript-language-server) and `typescript' as a dev dependency of
;; each project; the project's own tsserver is used.
;;
;; Grammars need to be installed once:
;;   M-x treesit-install-language-grammar RET typescript RET
;;   M-x treesit-install-language-grammar RET tsx RET

(require 'treesit)

(add-to-list
 'treesit-language-source-alist
 '(typescript "https://github.com/tree-sitter/tree-sitter-typescript"
              "master" "typescript/src"))
(add-to-list
 'treesit-language-source-alist
 '(tsx "https://github.com/tree-sitter/tree-sitter-typescript"
       "master" "tsx/src"))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(setq lsp-clients-typescript-prefer-use-project-ts-server t)

(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)
