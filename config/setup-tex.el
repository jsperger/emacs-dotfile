;;; config/setup-tex.el --- TeX and LaTeX configuration -*- lexical-binding: t -*-

(use-package auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . hs-minor-mode)
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . TeX-fold-mode)
    (LaTeX-mode . LaTeX-math-mode)
  :general
  (despot-def (TeX-latex-mode-map)
    :major-modes '(TeX-tex-mode LaTeX-mode)
    ","             'TeX-command-master
    "a"             'TeX-command-run-all
    "b"             'TeX-command-menu
    "c"             'TeX-clean
    "h"             'TeX-doc
    "k"             'TeX-kill-job
    "l"             'TeX-recenter-output-buffer
    "n"             'TeX-next-error
    "N"             'TeX-previous-error
    "v"             'TeX-view
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
    "."                'LaTeX-mark-environment
    ","             'TeX-command-master
    "a"             'TeX-command-run-all
    "c"             'TeX-clean
    "e"                'LaTeX-environment
    "f"                (cons "fill" (make-sparse-keymap))
    "fe"               'LaTeX-fill-environment
    "fp"               'LaTeX-fill-paragraph
    "fr"               'LaTeX-fill-region
    "fs"               'LaTeX-fill-section
    "h"             'TeX-doc
    "k"             'TeX-kill-job
    "l"             'TeX-recenter-output-buffer
    "n"             'TeX-next-error
    "N"             'TeX-previous-error
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
    "s"                'LaTeX-section
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
    "v"             'TeX-view
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
	(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

(use-package preview-dvisvgm
  :after auctex)

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

;;; setup-tex.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
