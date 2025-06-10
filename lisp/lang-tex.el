;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

;; commenting rather than deleting for now (2025-02-07)
;; not sure if it's useful to have this as a var so bibtex and citar
;; use a variable rather than individual declarations
;; (defvar bib-file-location "~/obsidian/obsidian-biblatex.bib")

(use-package auctex
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
    ;; TeX-command-run-all runs compile and open the system viewer
    "a"             'TeX-command-run-all      ;; C-c C-a
   ;;  "b"             'TeX-build
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
	;; https://github.com/vedang/pdf-tools/?tab=readme-ov-file#auto-revert
	(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

;; surprised this isn't an option in auctex
(use-package preview-dvisvgm
  :after auctex)

;;;
;; --- Packages to trial
;;;

(use-package xenops
	:disabled)

(use-package cdlatex
	:disabled)

(use-package latex-extra
  :disabled
  :after auctex
  :hook (LaTeX-mode . latex-extra-mode))

(use-package latex-table-wizard
	:disabled)

(use-package evil-tex
	:disabled)

(use-package latex-extra
	:disabled)

(provide 'lang-tex)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-tex.el ends here
