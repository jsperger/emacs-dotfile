;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package org
  :init
  (setq org-directory "~/obsidian/org/"
        org-inbox-file (concat org-directory "inbox.org")
        org-default-notes-file org-inbox-file
        org-project-file (concat org-directory "projects.org")
        org-confirm-babel-evaluate nil
        )
	:config
	(setopt org-latex-bib-compiler "biber"
					org-latex-compiler "lualatex")

  (use-package oc
    :ensure nil
    :config
    (setq org-cite-export-processors '((beamer natbib)
                                       (latex biblatex)
                                       (t csl))
          org-cite-global-bibliography '("~/obsidian/obsidian-biblatex.bib")))


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
    "Te"    'org-toggle-pretty-entities
    "Ti"    'org-toggle-inline-images
    "Tl"    'org-toggle-link-display
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

  :general
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
    "Op"     'open-org-project-file)
  )


;;;
;;; Functionality Add-ons
;;;

;; Org-transclusion lets you insert a copy of text content via a file link or ID
;; link within an Org file. It lets you have the same content present in
;; different buffers at the same time without copy-and-pasting it.
(use-package org-transclusion
  :after org)

;; Orphanage - collection of Unmaintained org add-ons
(use-package org-contrib
  :disabled
  :after org)


(use-package org-pdftools
	:disabled
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
	:disabled
	:after org-noter
	:config
	;; Add a function to ensure precise note is inserted
	(defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
		(interactive "P")
		(org-noter--with-valid-session
		 (let ((org-noter-insert-note-no-questions (if toggle-no-questions
																									 (not org-noter-insert-note-no-questions)
																								 org-noter-insert-note-no-questions))
					 (org-pdftools-use-isearch-link t)
					 (org-pdftools-use-freepointer-annot t))
			 (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note))
	)

(use-package org-make-toc
	:hook (org-mode . org-make-toc-mode)
  :after org)

;;;
;; --- Diagramming Tools
;;;

(use-package chatu
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./draws_out")))

(use-package pikchr-mode
		:mode ("\\.pikchr\\'" . pikchr-mode))

(use-package uniline
	:general
(tyrant-def "au" 'uniline-mode)
	)

;;;
;;; Exporters:
;;;

(use-package ox-pandoc
	:after org
	:hook (org-mode . org-pandoc-startup-check))

;; (use-package org-anki
;;   :disabled)

(use-package ob-mermaid
  :after org)

(use-package ox-tufte
	:disabled
	:after org
  )

;;;
;;; Appearance:
;;;

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
	(set-face-attribute 'org-modern-symbol nil :family "Iosevka")
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))

(use-package org-side-tree
	:disabled
	:hook org-mode
	:config
	(setopt org-side-tree-persistent nil
					org-side-tree-fontify t
					org-side-tree-enable-folding t)
	)

(provide 'lang-org)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-org.el ends here
