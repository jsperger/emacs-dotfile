;;; lang-bib.el --- bibliography -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bibtex
  :ensure nil
  )

(use-package citar
  :after bibtex auctex
  :hook ((org-mode LaTeX-mode TeX-latex-mode org-beamer-mode) . citar-capf-setup)
  :init
  (with-eval-after-load 'embark
    (defun bibtex-key-embark ()
      (save-excursion
        (bibtex-beginning-of-entry)
        (when (looking-at bibtex-entry-maybe-empty-head)
          (cons 'bibtex-key
                (bibtex-key-in-head)))))
    (defvar-keymap bibtex-key-embark-map
      :doc "Embark keymap for Zetteldeft links"
      :parent embark-general-map
      "f" #'citar-open
      "n" #'citar-open-notes)
    (add-to-list 'embark-keymap-alist '(bibtex-key . bibtex-key-embark-map)))

  :config
  (defun citar-setup-capf ()
    "add `citar-capf' to `completion-at-point-functions'"
    (add-to-list 'completion-at-point-functions #'citar-capf))

  :general
  (tyrant-def "aC" 'citar-open)

  :custom
  (citar-at-point-function 'embark-act)
  (citar-bibliography '("~/obsidian/obsidian-biblatex.bib"))
  )


(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode))

(use-package ebib
	:general
	(tyrant-def "d" 'ebib))



(provide 'lang-bib)
;;; lang-bib.el ends here
