;;; config/setup-snippets.el --- Snippets configuration -*- lexical-binding: t -*-

(require 'my-tempel-helpers)

(use-package tempel
  :hook ((text-mode prog-mode) . tempel-setup-capf)
  :init
  (setq tempel-trigger-prefix "<"
        tempel-path "~/.emacs.d/etc/templates/*.eld")
  :config
  (add-to-list 'hippie-expand-try-functions-list #'tempel-hippie-try-expand t)

	;;	:custom
	;;	(tempel-auto-reload nil)
	;; I don't want unexpected changes if I update something from a template, but
	;; I think I should use tempel-done when I'm done with a template
	)

(use-package tempel-collection
  :after tempel)

(use-package eglot-tempel
	:disabled
  :hook (eglot-managed-mode .  eglot-tempel-mode)
	)

(use-package yasnippet
	:hook ((text-mode prog-mode) . yas-minor-mode)
	:config
	(setopt yas-global-mode t)
	)

(use-package yasnippet-snippets
	:after yasnippet)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-snippets.el ends here
