;; The below doesn't work. Only leaving it here temporarily so I remember failed attempts
;; Hacky workaround attempt
;; Issue: after loading a tex file still need to run M-x TeX-latex-mode
;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . Tex-latex-mode))

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
      "xo"               'font-oblique)

(doom-modeline-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(safe-local-variable-values '((TeX-master . t))))


(load-theme 'doom-gruvbox)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

