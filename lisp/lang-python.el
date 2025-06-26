;;; lang-python.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;; commentary

;;; Code:

(use-package python-x
;;	:hook python-mode
	)

(use-package live-py-mode
	:general
	  (despot-def (python-mode-map)
    :major-modes '(python-mode python-ts-mode)
    "l" 'live-py-mode
    )
	)

(use-package uv-mode
	:hook (python-mode . uv-mode))

(use-package pyenv-mode
	:disabled)

(provide 'lang-python)
;;; lang-python.el ends here
