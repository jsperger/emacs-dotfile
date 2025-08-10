;;; config/setup-go.el --- Go programming language configuration -*- lexical-binding: t -*-

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :general
  (despot-def (go-mode-map)
    :major-modes '(go-mode go-ts-mode)
    "a" 'gofmt
    "f" (cons "jump" (make-sparse-keymap))
    "fa" 'go-goto-arguments
    "ff" 'go-goto-function
    "fn" 'go-goto-function-name
    "fr" 'go-goto-return-values
    "fm" 'go-goto-method-receiver
    )
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-go.el ends here
