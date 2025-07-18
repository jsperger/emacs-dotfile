;;; config/setup-reading.el --- Reading tools -*- lexical-binding: t -*-

(use-package djvu)

(use-package nov
	:mode ("\\.epub\\'" . nov-mode)
	:hook (nov-mode . visual-line-fill-column-mode)
	:config
;; Want this only in nov-mode with variable pitch font, not the right way to do        
;;	(setq-local visual-fill-column-width 60)
        (setopt nov-text-width t
                visual-fill-column-center-text t)
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

;; Structure and Interpretation of Computer Programs as info file
(use-package sicp)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-reading.el ends here
