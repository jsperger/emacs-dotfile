;;; tools-diagramming.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package chatu
  :hook ((org-mode markdown-mode) . chatu-mode)
  :commands (chatu-add
             chatu-open)
  :custom ((chatu-input-dir "./draws")
           (chatu-output-dir "./draws_out")))

(use-package d2-mode
	:mode ("\\.d2\\'")
	:general
	(despot-def (d2-mode-map)
		:major-modes '(d2-mode)
		"c" 'd2-compile
		"f" 'd2-compile-file
		"b" 'd2-compile-buffer
		"r" 'd2-compile-region
		"h" 'd2-compile-file-and-browse
		"j" 'd2-compile-buffer-and-browse
		"k" 'd2-compile-region-and-browse
		"o" 'd2-open-browser
		"o" 'd2-view-current-svg
		"d" 'd2-open-doc)
	)

(use-package mermaid-mode
			:mode ("\\.mermaid\\'" . mermaid-mode))

(use-package pikchr-mode
		:mode ("\\.pikchr\\'" . pikchr-mode))

(use-package uniline
	:general
(tyrant-def "au" 'uniline-mode)
	)


(provide 'tools-diagramming)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-diagramming.el ends here
