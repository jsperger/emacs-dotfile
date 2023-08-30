;;; lang-tex.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package tablist
  :elpaca t)

(use-package pdf-tools
  :elpaca t
  :after tablist
  :config
   (pdf-tools-install)

        ;; open pdfs scaled to fit page
        (setq-default pdf-view-display-size 'fit-page)
        ;; more fine-grained zooming
        (setq pdf-view-resize-factor 1.1)
        ;; automatically annotate highlights
        (setq pdf-annot-activate-created-annotations t))

(use-package tex
:elpaca auctex
:defer t
  :mode ("\\.[tT]e[xX]\\'" . TeX-tex-mode)
  :config
  (setq-default TeX-master nil)
  (setq TeX-parse-self t
        TeX-save-query nil
        TeX-source-correlate-start-server t
        TeX-view-program-list '(("Preview.app" "open -a Preview.app %o")
                                ("Skim" "open -a Skim.app %o")
                                ("displayline" "displayline -b %n %o %b")
                                ("open" "open %o"))
        TeX-view-program-selection '((output-dvi "open")
                                     (output-pdf "displayline")
                                     (output-html "open"))
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)

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

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

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

  (despot-def (TeX-mode-map LaTeX-mode-map)
    :major-modes '(tex-mode latex-mode)
    ","             'TeX-command-master
    ;; TeX-command-run-all runs compile and open the viewer
    "a"             'TeX-command-run-all                       ;; C-c C-a
    "b"             'TeX-build
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
    :major-modes '(latex-mode)
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
(use-package evil-tex
  :elpaca t
  :after evil
  :hook (TeX-mode . evil-tex-mode))


(use-package bibtex
  :elpaca nil
  :defer t
  :config
  (setq bibtex-file-path "~/bibliography/"
        bibtex-files '("my-library.bib")
        bibtex-notes-path "~/bibliography/notes/"
        bibtex-align-at-equal-sign t
        bibtex-dialect 'bibtex))

(use-package citar
  :elpaca (:files (:defaults))
  :hook ((org-mode latex-mode) . citar-setup-capf)
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
  (citar-embark-mode)

  (setq citar-at-point-function 'embark-act
        citar-bibliography (mapcar (lambda (file) (concat bibtex-file-path file)) bibtex-files)
        citar-library-paths `(,(concat bibtex-file-path "files/"))
        citar-notes-paths `(,bibtex-notes-path)
        citar-file-open-functions '(("html" . citar-file-open-external)
                                    ("pdf" . citar-file-open-external)
                                    (t . find-file)))
  (defun citar-setup-capf ()
    "add `citar-capf' to `completion-at-point-functions'"
    (add-to-list 'completion-at-point-functions #'citar-capf))
  :general
  (tyrant-def "aC" 'citar-open)
 )

(provide 'lang-tex)
;;; lang-tex.el ends here
