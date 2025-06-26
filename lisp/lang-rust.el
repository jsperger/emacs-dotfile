;;; lang-rust.el --- rust and cargo -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package rust-mode
	:mode ("\\.rs\\'" . rust-mode)
  :general
  (despot-def (rust-mode-map)
      :major-modes '(rust-ts-mode rust-mode)
      "c"            'rust-compile
			"k" 'rust-check
			"t" 'rust-test
			"r" 'rust-run
			"F" 'rust-format-buffer)
	)

(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setopt compilation-scroll-output t
					rust-format-on-save t)
	:general
	(despot-def (rust-mode-map)
		:major-modes '(rust-mode rust-ts-mode)
		"e" 'cargo-mode-execute-task
		"t" 'cargo-mode-test
		"l" 'cargo-mode-last-command
		"b" 'cargo-mode-build
		"o" 'cargo-mode-test-current-buffer
		"f" 'cargo-mode-test-current-test)
	)
(provide 'lang-rust)
;;; lang-rust.el ends here
