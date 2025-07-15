;;; config/setup-notes.el --- Notes configuration -*- lexical-binding: t -*-

(use-package obsidian
  :config
	(setopt obsidian-directory "~/obsidian"
					obsidian-inbox-directory "Inbox"
					obsidian-daily-notes-directory "Notes/Time-based Notes/Daily Notes"
					obsidian-templates-directory "Resources/Templates"
					obsidian-daily-note-template "Resources/Templates/Daily Note Template"
					global-obsidian-mode t
					obsidian-backlinks-mode nil)

	:general
	(tyrant-def "aob" 'obsidian-toggle-backlinks-panel)
  )

(use-package annotate
	:disabled
	:general
	(tyrant-def "aa" 'annotate-mode)
	)

(use-package orgmdb
	:disabled
  )

;;; setup-notes.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
