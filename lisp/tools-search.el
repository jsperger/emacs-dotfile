;;; tools-search.el --- search tools -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:



(use-package elgrep)

(use-package rg
	:config
	(when (and IS-MAC (daemonp))
  (setopt rg-executable "/opt/homebrew/bin/rg"))
	)

(use-package wgrep)

(use-package manage-minor-mode-table
	:general
	(tyrant-def "am" 'manage-minor-mode-table)
 	)

(provide 'tools-search)
;;; tools-search.el ends here
