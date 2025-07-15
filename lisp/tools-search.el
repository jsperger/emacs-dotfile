;;; tools-search.el --- search tools -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package rg
	:config
	(when (and IS-MAC (daemonp))
  (setopt rg-executable "/opt/homebrew/bin/rg"))
	)

(use-package wgrep
  :disabled)

(use-package manage-minor-mode-table
  :disabled
  :general
  (tyrant-def "am" 'manage-minor-mode-table)
 )

(provide 'tools-search)
;;; tools-search.el ends here
