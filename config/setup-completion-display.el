;;; config/setup-completion-display.el --- Completion display -*- lexical-binding: t -*-

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(use-package marginalia
	:custom
	(marginalia-mode t))

(use-package epkg-marginalia
	:after marginalia
	:config
	(setcar (alist-get 'package marginalia-annotator-registry)
          #'epkg-marginalia-annotate-package)
  )

(use-package nerd-icons-completion
  :after marginalia
	:hook (marginalia-mode . nerd-icons-completion-mode)
  :config
  (setopt nerd-icons-completion-mode t)
	)
