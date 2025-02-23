;;; tools-search.el ---  -*- lexical-binding: t -*-
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
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;;; tools-search.el ends here
