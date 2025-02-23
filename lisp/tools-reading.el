;;; tools-reading.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;
;; --- File-type handlers ---
;;;

(use-package djvu)

;; epub
(use-package nov
	:mode ("\\.epub\\'" . nov-mode)
	:hook (nov-mode . visual-line-fill-column-mode)
	:init
	(setopt nov-text-width t
					visual-fill-column-center-text t)
	:config
	(setq-local visual-fill-column-width 60)
)

(use-package pdf-tools
  :ensure (:post-build (pdf-tools-install))
	:mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (visual-fill-column-mode -1)))
  :init
  (setopt pdf-view-display-size 'fit-page ; open pdfs scaled to fit page
     ;;     pdf-view-use-scaling t ; hi-dpi support now default https://pdftools.wiki/20ef86be
          pdf-view-resize-factor 1.1   ; more fine-grained zooming
          pdf-annot-activate-created-annotations t ; automatically annotate highlights
					pdf-view-use-unicode-ligther nil
          )
)

;;;
;; --- Readling Notes ---
;;;
(use-package citar-denote
	:disabled)

(use-package denote-citar-sections
	:disabled)

;;;
;; --- Readling List ---
;;;


(use-package zotero
	;; https://gitlab.com/fvdbeek/emacs-zotero
	:disabled
  :commands (zotero-browser)
  :init
  (require 'zotero-browser)
	)


(use-package zotxt
;; https://gitlab.com/egh/zotxt-emacs
	:disabled)

(use-package pocket-reader
	:disabled)

(use-package biblio
	:disabled)

(use-package arxiv-mode
	:disabled)

(use-package calibredb
	:disabled)

;;;
;; --- Documentation ---
;;;

(use-package dash-docs)

(use-package consult-dash
  :after (consult dash-docs))

;; Structure and interpretation of computer programs
;; in package form?
(use-package sicp)

(provide 'tools-reading)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-reading.el ends here
