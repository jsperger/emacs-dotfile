;;; config/setup-programming.el --- General programming tools configuration -*- lexical-binding: t -*-

(use-package apheleia
  :disabled
  :custom (apheleia-global-mode nil)
  )

(use-package editorconfig
	:disabled 
  :config
  (editorconfig-mode nil)
	:general
	(tyrant-def "ce" 'editorconfig-mode)
	)

(use-package typespec-ts-mode
	:mode ("\\.tsp\\'" . typespec-ts-mode)
	:config
	(add-to-list
 'treesit-language-source-alist
 '(typespec "https://github.com/happenslol/tree-sitter-typespec"))
	)

(use-package code-cells
	:disabled
	:mode ("\\([ipynb]\\)\\'" . code-cells-mode))

(use-package drepl
	:disabled
	:general
	(trant-def "ci" 'drepl-ipython))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-programming.el ends here
