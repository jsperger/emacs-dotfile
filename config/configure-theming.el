;;; configure-theming.el --- Themes configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; [[file:../its-lit.org::#preserve-appearance-of-collapsed-outline-headings-until-window-edge][Preserve appearance of collapsed outline headings until window edge:1]]
(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))
;; Preserve appearance of collapsed outline headings until window edge:1 ends here

;; [[file:../its-lit.org::#color-theme-packages][Color theme packages:1]]
(use-package doom-themes)

(use-package ef-themes
  :config (setopt ef-themes-mixed-fonts t
                  ef-themes-variable-pitch-ui t)
  ;; wonder why this is fine but modus-themes gets angry if you try setopt instead of setq
  )

(use-package flexoki-themes
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments nil)
  )

;; Development version I think? Also built in
(use-package modus-themes
  :config
  ;; setopt will cause an error unless an :after or an autoload e.g. a hook or  is configured because these variables don't exist until load theme is called? something like that. setq is safe. :after org (this was arbitrary I just knew it was configured to autoload when opening an org file) + setopt worked fine
  ;; (setopt modus-themes-mixed-fonts t
  ;;       modus-themes-variable-pitch-ui t)

  (setq modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)

  ;; Maybe define some palette overrides, such as by using our presets
  ;; (setq modus-themes-common-palette-overrides
  ;;       modus-themes-preset-overrides-intense)
  )
;; Color theme packages:1 ends here

;; [[file:../its-lit.org::#managing-fonts][Managing fonts:1]]
;;;; Font configuration
(use-package fontaine
  :hook (elpaca-after-init . fontaine-mode)
  :config
  (setopt fontaine-presets '(
                             (t  :default-weight normal
                                 :default-height 150
                                 :fixed-pitch-family "JetBrains Mono NL"
                                 :variable-pitch-family "Heliotrope OT"
                                 :variable-pitch-height 1.05
                                 :bold-family nil ; use the face's bold
                                 :bold-weight bold)

;;;;; Presets that keep the same font family but modify its appearance
	                           ;; Presets to change point size only
	                           (10pt :default-height 100)
	                           (11pt :default-height 110)
	                           (12pt :default-height 120)
	                           (13pt :default-height 130)
	                           (14pt :default-height 140)
	                           (15pt :default-height 150)
	                           (16pt :default-height 160)
	                           (17pt :default-height 170)
	                           (18pt :default-height 180)

;; Line spacing is additional line spacing, not total line spacing
;; (i.e. 0 is the default)
;; Meaning differs depending on whether it is an integer or float
;; Integer = additional pixels to add
;; Floating point = additional spacing proportional to point size
	                           (lsd :line-space 0)
	                           ;; line spacing pixels
	                           (lsp1 :line-spacing 1)
	                           (lsp2 :line-spacing 2)
	                           (lsp3 :line-spacing 3)
	                           (lsp4 :line-spacing 4)
	                           (lsp5 :line-spacing 5)
	                           ;; line spacing scaling
	                           (lss05 :line-spacing 0.05)
	                           (lss10 :line-spacing 0.10)
	                           (lss15 :line-spacing 0.15)
	                           (lss20 :line-spacing 0.20)
	                           (lss25 :line-spacing 0.25)
	                           (lss30 :line-spacing 0.30)

;;;;; Font presets

;;;;;; Monospace fonts
	                           (iosevka-comfy
	                            :default-family "Iosevka Comfy"
	                            :default-weight normal)

	                           (fira-mono
	                            :default-family "Fira Mono")

	                           (fira-code
	                            :default-family "FiraCode Nerd Font"
	                            )

	                           (jetbrains-mono
	                            :default-family "JetBrains Mono NL")

	                           (plex-mono
	                            :default-family "IBM Plex Mono")

	                           (dank-mono
	                            :default-family "Dank Mono")

	                           (hack
	                            :default-family "Hack Nerd Font")

	                           ;; think inter is proportional not mono
	                           (inter
	                            :default-family "Inter")

	                           (xenon
	                            :default-family "Monaspace Xenon")

	                           (input-mono
	                            :default-family "Input Mono")
;;;;;; Quasi-monospace fonts
	                           (duospace
	                            :default-family "iA Writer Duospace")

	                           (triplicate-mono-b
	                            :default-family "Triplicate B")

	                           (triplicate-code-b
	                            :default-family "Triplicate B Code")

	                           (triplicate-poly-b
	                            :default-family "Triplicate B Poly")

	                           (triplicate-mono-a
	                            :default-family "Triplicate A")

	                           (triplicate-code-a
	                            :default-family "Triplicate A Code")

	                           (triplicate-poly-a
	                            :default-family "Triplicate A Poly")

	                           (header-triplicate
	                            :header-line-family "Triplicate A Caps"
	                            :header-line-height 1.1)

	                           (triplicate-combo
	                            :default-family "Triplicate A Code"
	                            :variable-pitch-family "Triplicate A Poly"
	                            :header-line-family "Triplicate A Caps"
	                            :header-line-height 1.1
	                            )

                             ;; Not fully variable width but it's not suitable for coding imo.
                             ;; Could be fine for writing in a markup language
                             (quattro
	                            :default-family "iA Writer Quattro V")


;;;;;; Header fonts      
	                           (header-heliotrope
	                            :header-line-family "Heliotrope 4 Caps"
	                            :header-line-height 1.1)

	                           (header-overpass
	                            :header-line-family "Overpass"
	                            :header-line-height 1.1)

                             ;; Don't use a different family for headers 
	                           (header-null
	                            :header-line-family nil
	                            :header-line-height 1.0)

;;;;;; Variable pitch fonts
	                           (serif-input
	                            :variable-pitch-family "Input Serif")

	                           (sans-input
	                            :variable-pitch-family "Input Sans")

	                           (vp-heliotrope
	                            :variable-pitch-family "Heliotrope OT"
	                            :variable-pitch-height 1.05)

	                           (vp-avenir
	                            :variable-pitch-family "Avenir"
	                            :default-weight book)

	                           (vp-minion
	                            :variable-pitch-family "Minion 3")
	                           )
	        )
  :custom (fontaine-mode t)
  )
;; Managing fonts:1 ends here

;; [[file:../its-lit.org::#view-fonts-within-emacs][View fonts within emacs:1]]
(use-package show-font
  :general  (tyrant-def  "hs" 'show-font-select-preview
                         "hT" 'show-font-tabulated
                         )
  :bind
  (("C-c s f" . show-font-select-preview)
   ("C-c s t" . show-font-tabulated))
  )
;; View fonts within emacs:1 ends here

;; [[file:../its-lit.org::#theming-advice-and-hooks][Theming advice and hooks:1]]
(advice-add 'load-theme :after #'load-theme@run-hooks)
(advice-add 'load-theme :before #'load-theme@theme-dont-propagate)

;; (add-hook 'after-load-theme-hook #'bolder-faces)
;; (add-hook 'after-load-theme-hook #'customize-tab-bar)
;; (add-hook 'after-load-theme-hook #'unscale-outlines)
(add-hook 'after-load-theme-hook #'my-disable-flymake-underline)
;; Theming advice and hooks:1 ends here

;; [[file:../its-lit.org::#theming-lisp-footer][Theming lisp footer:1]]
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-theming.el ends here
;; Theming lisp footer:1 ends here
