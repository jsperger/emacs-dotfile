;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar bib-file-location "~/obsidian/obsidian-biblatex.bib")

(use-package pdf-tools
  :ensure (:post-build (pdf-tools-install))
  :after tablist
  :config
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t))

(use-package bibtex
  :ensure nil
  )

(use-package citar
  :after bibtex
  :hook ((org-mode latex-mode TeX-latex-mode org-beamer-mode) . citar-capf-setup)
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
  )

(use-package citar-embark
  :config
  (citar-embark-mode))


(provide 'lang-tex)

;;; lang-tex.el ends here
