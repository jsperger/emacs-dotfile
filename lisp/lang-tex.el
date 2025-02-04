;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(defvar bib-file-location "~/obsidian/obsidian-biblatex.bib")

(use-package auctex
  :ensure
  (:repo "https://git.savannah.gnu.org/git/auctex.git"
         :branch "main"
         :pre-build (("make" "elpa"))
         :build (:not elpaca--compile-info) ;; Make will take care of this step
         :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
         :version (lambda (_) (require 'auctex) AUCTeX-version)
)
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . hs-minor-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . TeX-fold-mode)
                                        ; TODO: figure out why these fail but single line declarations are working
  ;;  :hook (LaTeX-mode . (outline-minor-mode hs-minor-mode)) ;fails
  ;;  :hook (LaTeX-mode . (outline-minor-mode hs-minor-mode auto-fill-mode)) also fails
    (LaTeX-mode . LaTeX-math-mode)
  :general
  (despot-def (TeX-latex-mode-map)
    :major-modes '(TeX-tex-mode LaTeX-mode)
    ","             'TeX-command-master
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                       ;; C-c C-a
    "b"             'TeX-command-menu ;TODO: replace with latexmk
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
    ","             'TeX-command-master
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                       ;; C-c C-a
    "b"             'TeX-build
    "c"             'TeX-clean
    "e"                'LaTeX-environment                   ;; C-c C-e
    "f"                (cons "fill" (make-sparse-keymap))
    "fe"               'LaTeX-fill-environment              ;; C-c C-q C-e
    "fp"               'LaTeX-fill-paragraph                ;; C-c C-q C-p
    "fr"               'LaTeX-fill-region                   ;; C-c C-q C-r
    "fs"               'LaTeX-fill-section                  ;; C-c C-q C-s
    "h"             'TeX-doc
    "k"             'TeX-kill-job                              ;; C-c C-k
    "l"             'TeX-recenter-output-buffer                ;; C-c C-l
    "n"             'TeX-next-error                            ;; C-c `
    "N"             'TeX-previous-error                        ;; M-g p
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
    "xo"               'font-oblique
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

  :config
  (setopt TeX-engine 'luatex)
  )

(use-package latex-extra
  :disabled
  :after auctex
  :hook (LaTeX-mode . latex-extra-mode))

(use-package pdf-tools
  :ensure (:post-build (pdf-tools-install))
  :hook (pdf-view-mode . (lambda () (visual-fill-column-mode -1)))
  :config

  (setopt pdf-view-display-size 'fit-page ; open pdfs scaled to fit page
          pdf-view-use-scaling t ; hi-dpi support
          pdf-view-resize-factor 1.1   ; more fine-grained zooming
          pdf-annot-activate-created-annotations t ; automatically annotate highlights
          )
)

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
  )

(use-package citar-embark
  :after citar embark
  :config
  (citar-embark-mode))

(use-package preview-dvisvgm
  :disabeld
  :after auctex)

(provide 'lang-tex)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-tex.el ends here
