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

;; (use-package
;;	https://github.com/chenyanming/paw
;;	)

;;;
;; --- Readling List ---
;;;

(use-package wallabag
:disabled
	:load-path "~/.emacs.d/lisp/wallabag/"
  :config
  (setq wallabag-host "https://xx.xx.xx") ;; wallabag server host name
  (setq wallabag-username "xx") ;; username
  (setq wallabag-password "xx") ;; password
  (setq wallabag-clientid "xx") ;; created with API clients management
  (setq wallabag-secret "xx") ;; created with API clients management
  (setq wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date")) ;; control what content should be show in *wallabag-search*
(setq wallabag-search-page-max-rows 32) ;; how many items shown in one page
  ;; (setq wallabag-db-file "~/OneDrive/Org/wallabag.sqlite") ;; optional, default is saved to ~/.emacs.d/.cache/wallabag.sqlite
  )

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

(use-package devdocs
	:general
	(tyrant-def
		"Jm" 'devdocs-lookup
		"JM" 'devdocs-peruse
		"hd" 'devdocs-peruse)
	)

(use-package dash-docs
	:disabled)

(use-package consult-dash
	:disabled
  :after (consult dash-docs)
	:general
	(tyrant-def
		"Jm" 'consult-dash)
  :config
    ;; Use the symbol at point as initial search term
   (consult-customize consult-dash :initial (thing-at-point 'symbol))
	)

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
