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

(add-hook 'after-load-theme-hook
          (defun unscale-outlines ()
            (dolist (outline-number (number-sequence 1 8))
              (let ((outline (intern (format "outline-%d" outline-number))))
                (set-face-attribute outline nil :height 1.0)))))

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
  (setq fontaine-presets
        '((t
           :default-weight normal
           :default-height 150
           :fixed-pitch-family "JetBrains Mono NL"
           :variable-pitch-family "Heliotrope OT"
           :variable-pitch-height 1.05
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold)

          (12pt :default-height 120)
          (14pt :default-height 140)
          (15pt :default-height 150)
          (16pt :default-height 160)

          (iosevka-comfy
           :default-family "Iosevka Comfy"
           :default-weight normal)

          (duospace
           :default-family "iA Writer Duospace")

          (quattro
           :default-family "iA Writer Quattro V")

          (fira-mono
           :default-family "Fira Mono")

          (jetbrains-mono
           :default-family "JetBrains Mono NL")

          (plex-mono
           :default-family "IBM Plex Mono"))
        )
  )

;;; Theme packages

;; (use-package nano-theme
;;  :disabled)

(use-package ef-themes)

(use-package catppuccin-theme
  :config
  (setq catppuccin-enlarge-headings nil))

(use-package doom-themes)

(use-package nordic-night-theme
  :disabled)

(use-package modus-themes)

(use-package gruvbox-theme)

(use-package solarized-theme)



;;;
;; --- Font Locking / Decoration ---
;;;

(use-package hl-todo
  :custom (global-hl-todo-mode 't))

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
