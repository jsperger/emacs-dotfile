;;; setup-bib --- bibliography management -*- lexical-binding: t; -*-
(use-package bibtex
  :ensure nil
  )

(use-package citar
  :after bibtex auctex
  :hook ((org-mode LaTeX-mode TeX-latex-mode org-beamer-mode) . citar-capf-setup)
  :init

  (with-eval-after-load 'embark
    (defvar-keymap bibtex-key-embark-map
      :doc "Embark keymap for Zetteldeft links"
      :parent embark-general-map
      "f" #'citar-open
      "n" #'citar-open-notes)
    (add-to-list 'embark-keymap-alist '(bibtex-key . bibtex-key-embark-map))
    )

  :general
  (tyrant-def "aC" 'citar-open)

  :custom
  (citar-at-point-function 'embark-act)
  (citar-bibliography '("~/obsidian/obsidian-biblatex.bib"))
  )


(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode)
  (defun citar-setup-capf ()
  "add `citar-capf' to `completion-at-point-functions'"
  (add-to-list 'completion-at-point-functions #'citar-capf))

)

(use-package ebib
	:general
	(tyrant-def "d" 'ebib))
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-bib.el ends here
