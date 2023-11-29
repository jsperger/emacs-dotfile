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

(load "latex.el" nil nil t)
(load "preview-latex.el" nil nil t)

(setq-default TeX-master nil
              TeX-command "LaTeX"
              TeX-engine 'luatex
  		preview-scale 1.4
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))

(setq preview-auto-cache-preamble nil
 	TeX-parse-self t
     TeX-save-query nil
        TeX-source-correlate-start-server t
        LaTeX-fill-break-at-separators nil)

(setq bibtex-file-path "~/obsidian/"
        bibtex-files '("obsidian-biblatex.bib")
        bibtex-align-at-equal-sign t
        bibtex-dialect 'bibtex)

(setq citar-at-point-function 'embark-act
        citar-bibliography bib-file-location
        citar-library-paths `(,(concat bibtex-file-path "files/"))
        citar-file-open-functions '(("html" . citar-file-open-external)
                                    ("pdf" . citar-file-open-external)
                                    (t . find-file)))
;; After-init hooks packages
(electric-pair-mode)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(winner-mode)
(which-key-mode)
(evil-mode)
(evil-collection-init)
(evil-owl-mode)
(evil-snipe-mode)
(doom-modeline-mode)
(shackle-mode)
(global-hl-todo-mode)
(global-diff-hl-mode)
(global-auto-revert-mode)
(vertico-mode)
(marginalia-mode)
(global-corfu-mode)
(prescient-persist-mode)
(gcmh-mode)
(popper-mode)
(popper-echo-mode)
(global-treesit-auto-mode)
(undohist-initialize)
(winum-mode)
(lambda () (unless (server-running-p)
                              (server-start)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14" "4fdbed4aa8bcb199d7f6a643886bac51178d1705b9b354ef3dd82d4ec48072d2" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(highlight-parentheses-colors '("#689d6a" "#d79921" "#458588" "#b16286" "#98971a"))
 '(package-selected-packages '(doom-themes solarized-theme monokai-theme eglot))
 '(safe-local-variable-values
   '((TeX-master . "../sperger-dissertation")
     (TeX-master . t))))


;; (load-theme 'doom-gruvbox)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil))))

  (setq
    	lacquer-appearance-switch t ;; Switch theme once the system appearance is changed, and distinguish light and dark theme.
    	lacquer-theme-list '((solarized-theme solarized-light)
                          (solarized-theme solarized-dark)
                          (solarized-theme solarized-gruvbox-light)
                          (solarized-theme solarized-gruvbox-dark)
                          (doom-themes doom-nord)
                          (doom-themes doom-gruvbox)
                          (doom-themes doom-gruvbox-light)
                          (doom-themes doom-monokai-pro)
                          (doom-themes doom-monokai-classic)
                          (doom-themes doom-zenburn))
        lacquer-default-theme 'solarized-light
        lacquer-font-list '(Source\ Sans\ 3
                            Fira\ Code
                            Minion\ 3)
        lacquer-default-font-size 115
    )

(lambda () (if (daemonp)
  		  (add-hook 'server-after-make-frame-hook #'lacquer-auto-mode)
        (lacquer-auto-mode)))
(lacquer-auto-mode)
