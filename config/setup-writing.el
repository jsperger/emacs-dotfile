;;; config/setup-writing.el --- Writing tools configuration -*- lexical-binding: t -*-

(use-package binder
	:general
	(tyrant-def
		"t;" 'binder-toggle-sidebar
		"t'"  'binder-reveal-in-sidebar
		"[" 'binder-previous
		"]" 'binder-next)
	)

(use-package flymake-proselint
  :disabled)

(use-package flymake-vale
	:ensure (flymake-vale :host github :repo "tpeacock19/flymake-vale")
	:general
	(tyrant-def
		"as" 'flymake-vale-load))

(use-package freeze-it
	:disabled
	)

(use-package pandoc-mode
	:disabled
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
  "Start pandoc for the buffer and open the menu"
  (interactive)
  (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
  (pandoc-main-hydra/body))

)

(use-package ox-typst
	:disabled
  :after org
	:config
	(defvar org-typst-math-use-typst-syntax t)
	(if org-typst-math-use-typst-syntax
		(setq org-typst-from-latex-environment #'org-typst-from-latex-with-naive
					org-typst-from-latex-fragment #'org-typst-from-latex-with-naive)
(setq org-typst-from-latex-environment #'org-typst-from-latex-with-pandoc
      org-typst-from-latex-fragment #'org-typst-from-latex-with-pandoc)
		)
	)

(use-package writegood-mode
	:disabled
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-writing.el ends here
