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

(use-package annotate
	:disabled
	;; TODO make a keymap for the rest of the annotation commands
	;; mode toggle not too useful on its own
	:general
	(tyrant-def "aa" 'annotate-mode)
	)


(use-package obsidian
  :config
	(setopt obsidian-directory "~/obsidian"
					obsidian-inbox-directory "Inbox" ;This directory will be used for `obsidian-capture' if set.
					obsidian-daily-notes-directory "Notes/Time-based Notes/Daily Notes";location for new notes created via obsidian-daily-note
					obsidian-templates-directory "Resources/Templates";location for obsidian.el to find template files
					obsidian-daily-note-template "Resources/Templates/Daily Note Template"
					global-obsidian-mode t
					obsidian-backlinks-mode nil)

	:general
	(tyrant-def "aob" 'obsidian-toggle-backlinks-panel)
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
