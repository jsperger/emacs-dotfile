;;; completion-display.el --- summary -*- lexical-binding: t; -*-

;; Author: John Sperger

;;; Commentary:

;; commentary

;;; Code:

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


(provide 'completion-display)
;;; completion-display.el ends here
