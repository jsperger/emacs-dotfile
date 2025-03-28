;;; tools-programming.el ---  -*- lexical-binding: t -*-

;;; Commentary: General programming tools excluding bridge-lsp
;; Language-specific packages belong in the relevant lang-{foo}.el file

;;; Code:

(use-package apheleia
  :config (setq apheleia-global-mode nil)
  ;; :custom (apheleia-global-mode nil)
  )

(use-package editorconfig
  :config
  (editorconfig-mode nil)
	:general
	(tyrant-def "ce" 'editorconfig-mode)
	)

(use-package password-menu
	;; was using while debugging authinfo api key stuff
	:disabled
	:general
	(tyrant-def "as" 'password-menu-transient)
	)

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

;; (use-package prettier)

;; (use-package reformatter)


(provide 'tools-programming)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-programming.el ends here
