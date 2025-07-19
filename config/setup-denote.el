;;; config/setup-denote.el --- Denote configuration -*- lexical-binding: t -*-

(use-package denote
	:hook (dired-mode . denote-dired-mode)
	:general
	(tyrant-def "ad" 'denote
		"l"       (cons "denote" (make-sparse-keymap))
		"ld" 'denote
		"ln" 'denote
		"lb" 'denote-backlinks
		"lr" 'denote-rename-file
		"lR" 'denote-rename-file-using-front-matter
		"ll" 'denote-link
    "lL" 'denote-add-links
		"ld" 'denote-dired
		"lg" 'denote-grep
		)
  :config
  (setq denote-directory (expand-file-name "~/obsidian/org/denote/"))
  (denote-rename-buffer-mode 1)
	)
	
(use-package consult-denote
  :general
	(tyrant-def
		"Jd" 'consult-denote-find
		"JD" 'consult-denote-grep)
  :config
  (consult-denote-mode 1)
	)


(use-package denote-org
	:after denote org
	:general
  (despot-def org-mode-map
    "d"     (cons "denote" (make-sparse-keymap))
    "dh"    'denote-org-link-to-heading
    "dH" 'denote-org-backlinks-for-heading
    "de" 'denote-org-extract-org-subtree

    "dc" 'denote-org-convert-links-to-file-type
    "dd" 'denote-org-convert-links-to-denote-type

    "df" 'denote-org-dblock-insert-files
    "dl" 'denote-org-dblock-insert-links
    "db" 'denote-org-dblock-insert-backlinks
    "dm" 'denote-org-dblock-insert-missing-links
    "dF" 'denote-org-dblock-insert-files-as-headings
		)
	)

(use-package denote-journal
	:after denote
	:commands ( denote-journal-new-entry
							denote-journal-new-or-existing-entry
							denote-journal-link-or-create-entry )
	:hook (calendar-mode . denote-journal-calendar-mode)
	:general
	(tyrant-def
		"aj" 'denote-journal-new-or-existing-entry
		"al" 'denote-journal-link-or-create-entry)
	:config
	(setq denote-journal-directory
				(expand-file-name "journal" denote-directory))
	(setq denote-journal-keyword "journal")
	(setq denote-journal-title-format 'day-date-month-year)
	)

(use-package denote-menu
  :general
  (tyrant-def "lm" 'list-denotes)

  (despot-def (denote-menu-mode-map)
		:major-modes '(denote-menu-mode)
		"c" 'dentoe-menu-clear-filters
    "e" 'denote-menu-export-to-dired
    "k" 'denote-menu-filter-by-keyword
		"r" 'denote-menu-filter
    "o" 'denote-menu-filter-out-keyword
    )
  )

(use-package denote-project-notes
	:after denote
	:general
	(tyrant-def "ps" 'denote-project-notes-show
		"pi" 'denote-project-notes-set-identifier)
	)
(use-package citar-denote
	:hook (denote-after-new-note . citar-denote-mode)
	(denote-fontify-links-mode . citar-denote-mode)
	)

(use-package denote-citar-sections
	:disabled)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-denote.el ends here
