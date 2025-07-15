;;; config/setup-search.el --- Search tools configuration -*- lexical-binding: t -*-

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

;;; setup-search.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
