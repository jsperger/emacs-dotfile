;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar bib-file-location "~/obsidian/obsidian-biblatex.bib")


(use-package tex
  :elpaca (auctex :pre-build (("./autogen.sh")
                              ("./configure"
                               "--with-texmf-dir=$(dirname $(kpsexpand '$TEXMFHOME'))")
                              ("make")))
  :mode ("\\.tex\\'" . TeX-latex-mode)

  ;;:hook
  ;; ((LaTeX-math-mode TeX-fold-mode TeX-source-correlate-mode TeX-PDF-mode reftex-mode) . TeX-latex-mode)
  ;;(TeX-mode . (outline-minor-mode))

  :config
;;(load "latex.el" nil nil t)
;;(load "preview-latex.el" nil nil t)
(add-to-list 'TeX-expand-list
		 '("%(-PDF)"
		   (lambda ()
		     (if TeX-PDF-mode
		         (cond
		          ((eq TeX-engine 'default) "-pdf")
		          ((eq TeX-engine 'xetex) "-pdfxe")
		          ((eq TeX-engine 'luatex) "-pdflua")) ""))))

    (add-to-list 'TeX-command-list
                 '("LaTeXmk" "latexmk %(-PDF) -%(PDF)%(latex)='%`%l%(mode)%'' %(output-dir) %t"
                   TeX-run-format nil (latex-mode doctex-mode) :help "Run Latexmk"))

    (with-eval-after-load 'latex
      (setq LaTeX-clean-intermediate-suffixes
            (append LaTeX-clean-intermediate-suffixes '("\\.fdb_latexmk" "\\.fls"))))

  (defun TeX-build ()
    (interactive)
    (let* ((master (TeX-master-file))
           (process (and (stringp master) (TeX-process master))))
      (TeX-save-document master)
      (when (and (processp process)
                 (eq (process-status process) 'run))
        (delete-process process))
      (TeX-command TeX-command-default 'TeX-master-file -1)))


  ;; Rebindings for TeX-font
  (with-eval-after-load 'latex
    (add-to-list 'LaTeX-font-list '(?\C-l "\\underline{"     "}")))
  (defun font-bold () (interactive) (TeX-font nil ?\C-b))
  (defun font-medium () (interactive) (TeX-font nil ?\C-m))
  (defun font-code () (interactive) (TeX-font nil ?\C-t))
  (defun font-emphasis () (interactive) (TeX-font nil ?\C-e))
  (defun font-italic () (interactive) (TeX-font nil ?\C-i))
  (defun font-clear () (interactive) (TeX-font nil ?\C-d))
  (defun font-calligraphic () (interactive) (TeX-font nil ?\C-a))
  (defun font-small-caps () (interactive) (TeX-font nil ?\C-c))
  (defun font-sans-serif () (interactive) (TeX-font nil ?\C-f))
  (defun font-normal () (interactive) (TeX-font nil ?\C-n))
  (defun font-serif () (interactive) (TeX-font nil ?\C-r))
  (defun font-oblique () (interactive) (TeX-font nil ?\C-s))
  (defun font-underline () (interactive) (TeX-font nil ?\C-l))
  (defun font-upright () (interactive) (TeX-font nil ?\C-u))

    (general-def TeX-mode-map "<H-S-mouse-1>" 'TeX-view)

    (despot-def (TeX-mode-map LaTeX-mode-map TeX-latex-mode-map)
      :major-modes '(tex-mode latex-mode TeX-latex-mode)
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
      :major-modes '(latex-mode TeX-latex-mode)
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
      "xo"               'font-oblique))

(elpaca-wait)


(use-package pdf-tools
  :elpaca (:post-build (pdf-tools-install))
  :after tablist
  :config
        ;; open pdfs scaled to fit page
        (setq-default pdf-view-display-size 'fit-page)
        ;; more fine-grained zooming
        (setq pdf-view-resize-factor 1.1)
        ;; automatically annotate highlights
        (setq pdf-annot-activate-created-annotations t))

(use-package evil-tex
  :elpaca t
  :after evil
  :hook
  (TeX-mode . evil-tex-mode)
  (TeX-latex-mode . evil-tex-mode))

(use-package cdlatex
  :after auctex
  :hook (TeX-latex-mode . cdlatex-mode)
  (org-beamer-mode . org-cdlatex-mode))

(use-package auctex-latexmk
  :after auctex)

(use-package xenops
  :after auctex)

(use-package bibtex
  :elpaca nil
  :after auctex
  )

(use-package citar
  :elpaca (:files (:defaults))
  :after auctex bibtex
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
    :after citar embark
    :config
    (citar-embark-mode))

(provide 'lang-tex)

;;; lang-tex.el ends here
