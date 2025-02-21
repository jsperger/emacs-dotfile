;;; completion-snippets.el --- -*- lexical-binding: t; -*-

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package tempel
  :hook ((text-mode prog-mode) . tempel-setup-capf)
  :init
  (setq tempel-trigger-prefix "<"
        tempel-path "~/.emacs.d/etc/templates/*.eld")
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (defun tempel-hippie-try-expand (old)
    "Integrate with hippie expand.
Just put this function in `hippie-expand-try-functions-list'."
    (if (not old)
        (tempel-expand t)
      (undo 1)))
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

(use-package yasnippet-snippets)

(provide 'completion-snippets)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; completion-snippets.el ends here
