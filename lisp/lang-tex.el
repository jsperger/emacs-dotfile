;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar bib-file-location "~/obsidian/obsidian-biblatex.bib")

(use-package auctex :defer t
  :ensure (:pre-build (("./autogen.sh")
                       ("./configure"
                        "--without-texmf-dir"
                        "--with-packagelispdir=./"
                        "--with-packagedatadir=./")
                       ("make"))
                      :build (:not elpaca--compile-info) ;; Make will take care of this step
                      :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                      :version (lambda (_) (require 'tex-site) AUCTeX-version))
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . hs-minor-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . TeX-fold-mode)
                                        ; TODO: figure out why these fail but single line declarations are working
  ;;  :hook (LaTeX-mode . (outline-minor-mode hs-minor-mode)) ;fails
  ;;  :hook (LaTeX-mode . (outline-minor-mode hs-minor-mode auto-fill-mode)) also fails
  :general
  (despot-def (TeX-latex-mode-map)
    :major-modes '(TeX-tex-mode LaTeX-mode)
    ","             'TeX-command-master
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                       ;; C-c C-a
    "b"             'TeX-build
    "c"             'TeX-clean
    "h"             'TeX-doc
    "k"             'TeX-kill-job                              ;; C-c C-k
    "l"             'TeX-recenter-output-buffer                ;; C-c C-l
    "n"             'TeX-next-error                            ;; C-c `
    "N"             'TeX-previous-error                        ;; M-g p
    "v"             'TeX-view                                  ;; C-c C-v
    "x"             (cons "text/fonts" (make-sparse-keymap))
    "xb"            'font-bold
    "xc"            'font-code
    "xe"            'font-emphasis
    "xi"            'font-italic
    "xr"            'font-clear
    "xo"            'font-oblique
    "xf"            (cons "fonts" (make-sparse-keymap))
    "xfc"           'font-small-caps
    "xff"           'font-sans-serif
    "xfr"           'font-serif
    "z"             (cons "fold" (make-sparse-keymap))
    "z="            'TeX-fold-math
    "zb"            'TeX-fold-buffer
    "zB"            'TeX-fold-clearout-buffer
    "ze"            'TeX-fold-env
    "zI"            'TeX-fold-clearout-item
    "zm"            'TeX-fold-macro
    "zp"            'TeX-fold-paragraph
    "zP"            'TeX-fold-clearout-paragraph
    "zr"            'TeX-fold-region
    "zR"            'TeX-fold-clearout-region
    "zz"            'TeX-fold-dwim)

  (despot-def LaTeX-mode-map
    :major-modes '(LaTeX-mode)
    "."                'LaTeX-mark-environment              ;; C-c .
    "e"                'LaTeX-environment                   ;; C-c C-e
    "f"                (cons "fill" (make-sparse-keymap))
    "fe"               'LaTeX-fill-environment              ;; C-c C-q C-e
    "fp"               'LaTeX-fill-paragraph                ;; C-c C-q C-p
    "fr"               'LaTeX-fill-region                   ;; C-c C-q C-r
    "fs"               'LaTeX-fill-section                  ;; C-c C-q C-s
    "p"                (cons "preview" (make-sparse-keymap))
    "pb"               'preview-buffer
    "pc"               'preview-clearout
    "pd"               'preview-document
    "pe"               'preview-environment
    "pf"               'preview-cache-preamble
    "pp"               'preview-at-point
    "pr"               'preview-region
    "ps"               'preview-section
    "r"                (cons "reference" (make-sparse-keymap))
    "rr"               'reftex-reference
    "rl"               'reftex-label
    "rj"               'reftex-goto-label
    "s"                'LaTeX-section                       ;; C-c C-s
    "x"                (cons "text/fonts" (make-sparse-keymap))
    "xB"               'font-medium
    "xu"               'font-underline
    "xf"               (cons "fonts" (make-sparse-keymap))
    "xfa"              'font-calligraphic
    "xfn"              'font-normal
    "xfu"              'font-upright
    "xi"               'font-italic
    "xr"               'font-clear
    "xo"               'font-oblique)
  )

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
  )

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode))


(provide 'lang-tex)

;;; lang-tex.el ends here
