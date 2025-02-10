;;; lang-r.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package ess
  :mode ("\\([rR]\\)\\'" . R-mode)
  :hook ((R-mode ess-r-mode) . 'eglot-ensure)
	(ess-r-help-mode . evil-mode)
  :init
	(setopt ess-set-style t
					comint-scroll-to-bottom-on-input t
					comint-scroll-to-bottom-on-output t
					ess-indent-offset 2)
	:custom
	(ess-indent-offset 2)
	(ess-own-style-list '((ess-indent-offset . 2)
															(ess-offset-arguments . open-delim)
															(ess-offset-arguments-newline . prev-call)
															(ess-offset-block . prev-line)
															(ess-offset-continued . straight)
															(ess-align-nested-calls "ifelse")
															(ess-align-arguments-in-calls "function[ 	]*(")
															(ess-align-continuations-in-calls . t)
															(ess-align-blocks control-flow)
															(ess-indent-from-lhs arguments fun-decl-opening)
															(ess-indent-from-chain-start . t)
															(ess-indent-with-fancy-comments . t)))
	(ess-style 'OWN)
	:config

  (despot-def (ess-r-mode-map)
    :major-modes 'ess-r-mode
    "b"            'ess-eval-buffer
    "c" 'ess-eval-region-or-function-or-paragraph
    "TAB" 'ess-install-library
    "f" 'ess-eval-function
    "r" 'run-ess-r
    )
  )

(use-package essgd
	;; TODO: add hook
	;; wrong, the mode is for the buffer with the plot
	;; :hook (inferior-ess-r-mode . essgd-mode)
	;; want to call the function essgd-start when
	;; run-ess-r is called or something

	;; TODO: Fix keybinds to work with evil
	)

(use-package ess-view-data)

(use-package poly-R
  :disabled)

(provide 'lang-r)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-r.el ends here
