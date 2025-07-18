;;; setup-org-exporters.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ox-pandoc
	:after org
	:hook (org-mode . org-pandoc-startup-check))

(use-package ob-mermaid
  :after org)

(use-package ox-tufte
	:disabled
	:after org
  )

(use-package ox-beamer-lecture
	:after org)
;;; Local Variables:
;;; no-byte-compile: t
;;; no-native-compile: t
;;; no-update-autoloads: t
;;; End:
;;; setup-org-exporters.el ends here
