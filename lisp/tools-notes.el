;;; tools-notes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package denote)

;;;
;; --- Readling Notes ---
;;;
(use-package citar-denote
	:hook (denote-after-new-note . citar-denote-mode)
	(denote-fontify-links-mode . citar-denote-mode)
	)

(use-package denote-citar-sections
	:disabled)



(use-package obsidian
  :disabled
  :config
  (obsidian-specify-path "~/obsidian")
  :custom
;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  (global-obsidian-mode t)
  )


(use-package orgmdb
;; Tools for managing your watchlist in org-mode and some functions for interacting with OMDb API.
	:disabled
  )


(provide 'tools-notes)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-notes.el ends here
