;;; tools-reading.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(elpaca (djvu
         :host github
         :repo "emacsmirror/djvu"
         :inherit nil) )

(use-package nov) ; epub

(use-package org-pdftools ; pdf
  :after org pdftools)

(use-package pdf-tools
  :ensure (:post-build (pdf-tools-install))
  :hook (pdf-view-mode . (lambda () (visual-fill-column-mode -1)))
  :config

  (setopt pdf-view-display-size 'fit-page ; open pdfs scaled to fit page
          pdf-view-use-scaling t ; hi-dpi support
          pdf-view-resize-factor 1.1   ; more fine-grained zooming
          pdf-annot-activate-created-annotations t ; automatically annotate highlights
          )
)

;; Structure and interpretation of computer programs
;; in package form?
(use-package sicp)

(provide 'tools-reading)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;;; tools-reading.el ends here
