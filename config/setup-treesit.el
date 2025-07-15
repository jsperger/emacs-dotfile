;;; config/setup-treesit.el --- Treesitter configuration -*- lexical-binding: t -*-

(use-package treesit
  :ensure nil
	:defer t
	:config
	(setq treesit-language-source-alist
				'(
					(r "https://github.com/r-lib/tree-sitter-r" "next")
					(bash "https://github.com/tree-sitter/tree-sitter-bash")
					(cmake "https://github.com/uyha/tree-sitter-cmake")
					(css "https://github.com/tree-sitter/tree-sitter-css")
					(elisp "https://github.com/Wilfred/tree-sitter-elisp")
					(go "https://github.com/tree-sitter/tree-sitter-go")
					(html "https://github.com/tree-sitter/tree-sitter-html")
					(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
					(json "https://github.com/tree-sitter/tree-sitter-json")
					(make "https://github.com/alemuller/tree-sitter-make")
					(markdown "https://github.com/ikatyang/tree-sitter-markdown")
					(python "https://github.com/tree-sitter/tree-sitter-python")
					(toml "https://github.com/tree-sitter/tree-sitter-toml")
					(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
					(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
					(yaml "https://github.com/ikatyang/tree-sitter-yaml")))
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-treesit.el ends here
