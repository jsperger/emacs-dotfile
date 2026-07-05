;;; config/setup-diagramming.el --- Diagramming tools configuration -*- lexical-binding: t -*-

;; [[file:../its-lit.org::#diagramming-language-modes][Diagramming Language Modes:1]]
(use-package graphviz-dot-mode
  :mode ("\\.[Dd][Oo][Tt]\\'" . graphviz-dot-mode)
)
;; Diagramming Language Modes:1 ends here

;; [[file:../its-lit.org::#unicode-drawing-tools][Unicode Drawing Tools:1]]
(use-package uniline
	:general
(tyrant-def "au" 'uniline-mode)
	)
;; Unicode Drawing Tools:1 ends here

;; [[file:../its-lit.org::diagramming footer][diagramming footer]]
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-diagramming.el ends here
;; diagramming footer ends here
