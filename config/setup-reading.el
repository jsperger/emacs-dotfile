;;; config/setup-reading.el --- Reading tools configuration -*- lexical-binding: t -*-

(use-package djvu)

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
  (setopt pdf-view-display-size 'fit-page
          pdf-view-resize-factor 1.1
          pdf-annot-activate-created-annotations t
					pdf-view-use-unicode-ligther nil
          )
)

(use-package wallabag
:disabled
	:load-path "~/.emacs.d/lisp/wallabag/"
  :config
  (setq wallabag-host "https://xx.xx.xx")
  (setq wallabag-username "xx")
  (setq wallabag-password "xx")
  (setq wallabag-clientid "xx")
  (setq wallabag-secret "xx")
  (setq wallabag-search-print-items '("title" "domain" "tag" "reading-time" "date"))
(setq wallabag-search-page-max-rows 32)
  )

(use-package zotero
	:disabled
  :commands (zotero-browser)
  :init
  (require 'zotero-browser)
	)


(use-package zotxt
	:disabled)

(use-package pocket-reader
	:disabled)

(use-package biblio
	:disabled)

(use-package arxiv-mode
	:disabled)

(use-package calibredb
	:disabled)

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
   (consult-customize consult-dash :initial (thing-at-point 'symbol))
	)

(use-package sicp)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-reading.el ends here
