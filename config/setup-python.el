;;; config/setup-python.el --- Python configuration -*- lexical-binding: t -*-

(use-package python-x
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

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-python.el ends here
