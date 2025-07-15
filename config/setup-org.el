;;; config/setup-org.el --- Org-mode configuration -*- lexical-binding: t -*-

(use-package org
	:commands (org-toggle-hidden-emphasis-markers)
  :init
  (setq org-directory "~/obsidian/org/"
        org-inbox-file (concat org-directory "inbox.org")
        org-default-notes-file org-inbox-file
        org-project-file (concat org-directory "projects.org")
        org-confirm-babel-evaluate nil
        )
	:config
	(setopt org-latex-bib-compiler "biber"
					org-latex-compiler "lualatex"
          org-babel-load-languages '(
                                     (emacs-lisp . t)
                                     (R . t)
                                     (shell . t)
                                     )
          )

  (use-package oc
    :ensure nil
    :config
    (setq org-cite-export-processors '((latex biblatex)
                                       (t csl))
          org-cite-global-bibliography '("~/obsidian/obsidian-biblatex.bib")))

	(use-package org-indent
		:ensure nil)
	(use-package org-protocol
		:ensure nil
		)
  :general
  (despot-def org-mode-map
    "'"     'org-edit-special
    ","     'org-ctrl-c-ctrl-c
    "*"     'org-ctrl-c-star
    "-"     'org-ctrl-c-minus
    "#"     'org-update-statistics-cookies
    "RET"   'org-ctrl-c-ret
    "M-RET" 'org-meta-return
    "b"     (cons "babel" (make-sparse-keymap))
    "ba"    'org-babel-sha1-hash
    "bb"    'org-babel-execute-buffer
    "bc"    'org-babel-check-src-block
    "bd"    'org-babel-demarcate-block
    "be"    'org-babel-execute-maybe
    "bf"    'org-babel-tangle-file
    "bg"    'org-babel-goto-named-src-block
    "bh"    'org-babel-describe-bindings
    "bi"    'org-babel-lob-ingest
    "bI"    'org-babel-view-src-block-info
    "bj"    'org-babel-insert-header-arg
    "bl"    'org-babel-load-in-session
    "bn"    'org-babel-next-src-block
    "bo"    'org-babel-open-src-block-result
    "bp"    'org-babel-previous-src-block
    "br"    'org-babel-goto-named-result
    "bs"    'org-babel-execute-subtree
    "bt"    'org-babel-tangle
    "bu"    'org-babel-goto-src-block-head
    "bv"    'org-babel-expand-src-block
    "bx"    'org-babel-do-key-sequence-in-edit-buffer
    "bz"    'org-babel-switch-to-session
    "e"     (cons "export" (make-sparse-keymap))
    "ee"    'org-export-dispatch
    "eb"    'org-beamer-export-to-pdf
    "el"    'org-latex-export-to-latex
    "ep"    'org-latex-export-to-pdf
    "i"     (cons "insert" (make-sparse-keymap))
    "ib"    'org-insert-structure-template
    "ic"    'org-cite-insert
    "id"    'org-insert-drawer
    "if"    'org-footnote-new
    "ih"    'org-insert-heading
    "iH"    'org-insert-heading-after-current
    "ii"    'org-id-get-create
    "iI"    'org-insert-item
    "il"    'org-insert-link
    "in"    'org-add-note
    "ip"    'org-set-property
    "is"    'org-insert-subheading
    "it"    'org-set-tags-command
    "t"     (cons "tables" (make-sparse-keymap))
    "ta"    'org-table-align
    "tb"    'org-table-blank-field
    "tc"    'org-table-convert
    "td"    (cons "delete" (make-sparse-keymap))
    "tdc"   'org-table-delete-column
    "tdr"   'org-table-kill-row
    "te"    'org-table-eval-formula
    "tE"    'org-table-export
    "tf"    'org-table-field-info
    "th"    'org-table-previous-field
    "tH"    'org-table-move-column-left
    "ti"    (cons "insert" (make-sparse-keymap))
    "tic"   'org-table-insert-column
    "tih"   'org-table-insert-hline
    "tiH"   'org-table-hline-and-move
    "tir"   'org-table-insert-row
    "tI"    'org-table-import
    "tj"    'org-table-next-row
    "tJ"    'org-table-move-row-down
    "tK"    'org-table-move-row-up
    "tl"    'org-table-next-field
    "tL"    'org-table-move-column-right
    "tn"    'org-table-create
    "tN"    'org-table-create-with-table.el
    "tp"    'org-plot/gnuplot
    "tr"    'org-table-recalculate
    "ts"    'org-table-sort-lines
    "tt"    (cons "toggles" (make-sparse-keymap))
    "ttf"   'org-table-toggle-formula-debugger
    "tto"   'org-table-toggle-coordinate-overlays
    "tw"    'org-table-wrap-region
    "T"     (cons "toggles" (make-sparse-keymap))
    "Tc"    'org-toggle-checkbox
		"Td"    'org-modern-indent-mode
		"TD"    'org-indent-mode
    "Te"    'org-toggle-pretty-entities
    "Ti"    'org-toggle-inline-images
    "Tl"    'org-toggle-link-display
		"Tm"    'org-toggle-hidden-emphasis-markers
    "Tt"    'org-show-todo-tree
    "Tx"    'org-latex-preview
    "x"     (cons "text" (make-sparse-keymap))
    "xb"    'org-bold
    "xc"    'org-code
    "xi"    'org-italic
    "xo"    'org-open-at-point
    "xr"    'org-clear
    "xs"    'org-strike-through
    "xu"    'org-underline
    "xv"    'org-verbatim)

  (general-def 'normal org-mode-map "RET" 'org-open-at-point)

  (tyrant-def
    "O"      (cons "Org" (make-sparse-keymap))
    "O/"     'org-occur-in-agenda-files
    "Oa"     'org-agenda
    "Oc"     'org-capture
    "OC"     (cons "clock" (make-sparse-keymap))
    "OCc"    'org-clock-cancel
    "OCg"    'org-clock-goto
    "OCi"    'org-clock-in-last
    "OCj"    'org-clock-jump-to-current-clock
    "OCo"    'org-clock-out
    "OCr"    'org-resolve-clocks
    "Od"     'open-org-default-notes-file
    "Ol"     'org-store-link
    "Op"     'open-org-project-file
    "Ot"     'org-transclusion-mode
    )
  )

(use-package org-transclusion
  :after org
  :general
  (despot-def org-mode-map
    "l" 'org-transclusion-add
    "L"     (cons "transclusion" (make-sparse-keymap))
    "L TAB" 'org-cycle
    "Ld" 'org-transclusion-remove
    "LD" 'org-transclusion-detach
    "Lj" 'org-transclusion-demote-subtree
    "Lk" 'org-transclusion-promote-subtree
    "Lm" 'org-transclusion-move-to-source
    "Ls" 'org-transclusion-live-sync-start
    "Lr" 'org-transclusion-refresh
    "Lo" 'org-transclusion-open-source
    )
  )

(use-package org-contrib
  :disabled
  :after org
  )

(use-package org-todoist
  :after gptel ; why would this depend on gptel? oh my api key olol
  :ensure (:host github
           :repo "lillenne/org-todoist"
           :branch "main"
           )
  :init
  (setq org-todoist-api-token (gptel-api-key-from-auth-source "api.todoist.com" "apikey"))
  )

(use-package org-pdftools
	:disabled
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
	:disabled
	:after org-noter
	:config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
	)

(use-package org-make-toc
  :after org
  :config
  (setopt org-make-toc-insert-custom-ids t)
	)

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
	(set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (setopt  org-auto-align-tags nil
           org-tags-column 0
           org-catch-invisible-edits 'show-and-error
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           org-hide-emphasis-markers t
           org-pretty-entities t
           org-ellipsis "…")
  )

(use-package org-side-tree
	:disabled
	:hook org-mode
	:config
	(setopt org-side-tree-persistent nil
					org-side-tree-fontify t
					org-side-tree-enable-folding t)
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-org.el ends here
