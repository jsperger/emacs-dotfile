;;; tools-programming.el --- general programming  -*- lexical-binding: t -*-

;;; Commentary:
;; General programming tools excluding bridge-lsp
;; Language-specific packages belong in the relevant lang-{foo}.el file

;;; Code:

;; ====================== Formatting and Linting =====================  

(use-package apheleia
  :disabled
  :custom (apheleia-global-mode nil)
  )

(use-package editorconfig
	;; file format for defining basic coding style for shared projects
	;; see what's included in base emacs with 30.1
	:disabled 
  :config
  (editorconfig-mode nil)
	:general
	(tyrant-def "ce" 'editorconfig-mode)
	)
;; (use-package prettier)

;; (use-package reformatter)

;; ============================= Typespec ============================  

(use-package typespec-ts-mode
	:mode ("\\.tsp\\'" . typespec-ts-mode)
	:config
	(add-to-list
 'treesit-language-source-alist
 '(typespec "https://github.com/happenslol/tree-sitter-typespec"))
	)

;; ================== Literate Programming (not org) =================  
(use-package code-cells
	;; for working with jupyter notebooks
	;; seems to require more configuration to work
	:disabled
	:mode ("\\([ipynb]\\)\\'" . code-cells-mode))

(use-package drepl
	;; fancier repl, suggested by above package
	:disabled
	:general
	(trant-def "ci" 'drepl-ipython))


(provide 'tools-programming)
;;; tools-programming.el ends here
