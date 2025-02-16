;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

;;;;
;; Hooks for after loading a theme
;;

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@run-hooks)

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'load-theme@theme-dont-propagate)

(add-hook 'after-load-theme-hook
          (defun bolder-faces ()
            (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
            (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold)))


(add-hook 'after-load-theme-hook
          (defun customize-tab-bar ()
            "Customize tab-bar faces."
            (set-face-attribute 'tab-bar nil
                                :foreground 'unspecified
                                :background 'unspecified
                                :box `(:line-width (-1 . 4) :color ,(face-background 'default))
                                :inherit 'unspecified)
            (set-face-attribute 'tab-bar-tab nil
                                :weight 'bold
                                :box 'unspecified
                                :foreground 'unspecified
                                :background 'unspecified
                                :inherit 'unspecified)
            (set-face-attribute 'tab-bar-tab-inactive nil
                                :box 'unspecified
                                :foreground 'unspecified
                                :background 'unspecified
                                :inherit 'unspecified)))

;; (add-hook 'after-load-theme-hook
;;           (defun unscale-outlines ()
;;             (dolist (outline-number (number-sequence 1 8))
;;               (let ((outline (intern (format "outline-%d" outline-number))))
;;                 (set-face-attribute outline nil :height 1.0)))))

(add-hook 'after-load-theme-hook
					(defun my-disable-flymake-underline ()
						"Disable underlining for Flymake faces while keeping theme colors."
						(custom-set-faces
						 `(flymake-error ((t (:underline nil :inherit default))))
						 `(flymake-note ((t (:underline nil :inherit default))))
						 `(flymake-warning ((t (:underline nil :inherit default))))))
					)
;;;;
;; Packages
;;
;;;;

(use-package fontaine
  :config
  (setopt fontaine-presets
					'((t
						 :default-weight normal
						 :default-height 150
						 :fixed-pitch-family "JetBrains Mono NL"
						 :variable-pitch-family "Heliotrope OT"
						 :variable-pitch-height 1.05
						 :bold-family nil ; use whatever the underlying face has
						 :bold-weight bold)

					;;;
						;; Universal presets
					;;;

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

					;;;
						;; Font presets
					;;;

						;; Monospace fonts
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
						;; Quasi-monospace fonts
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

						(header-heliotrope
						 :header-line-family "Heliotrope 4 Caps"
						 :header-line-height 1.1)

						(header-overpass
						 :header-line-family "Overpass"
						 :header-line-height 1.1)
						
						(header-null
						 :header-line-family nil
						 :header-line-height 1.0)

						(quattro
						 :default-family "iA Writer Quattro V")

						;; Variable pitch fonts
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
	;;	:hook (elpaca-after-init . (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))
	:hook (elpaca-after-init . fontaine-mode)
	:custom (fontaine-mode t)
  )

;;; Theme packages

;; (use-package nano-theme
;;  :disabled)


(use-package catppuccin-theme
	:disabled ;; slowest theme by a lot
  :init
  (setopt catppuccin-enlarge-headings nil))

(use-package doom-themes)

(use-package ef-themes)

(use-package gruvbox-theme)

(use-package kaolin-themes)

(use-package nordic-night-theme)

(use-package modus-themes)

(use-package solarized-theme)



;;;
;; --- Font Locking / Decoration ---
;;;

(use-package hl-todo
  :custom (global-hl-todo-mode 't))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  ;; (setq highlight-parentheses-colors '("Springgreen3"
  ;;                                      "IndianRed1"
  ;;                                      "IndianRed3"
  ;;                                      "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
	:disabled
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package paren-face
	;; https://github.com/tarsius/paren-face
	;; Parentheses dimming
	;;	:hook (elisp-mode . paren-face-mode)
	:custom (global-paren-face-mode t)
	)

;;;
;; --- Font Locking Utilities ---
;;;

(use-package font-lock-studio)

(use-package highlight-refontification)

(provide 'editor-fonts-themes)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-themes.el ends here
