;;; lang-go.el --- Go programming language -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'lang-go)
;;; lang-go.el ends here
