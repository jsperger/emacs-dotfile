;;; editor-themes.el --- themes -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

;;;; ============================ Theme packages ===========================

;; (use-package nano-theme
;;  :disabled)


(use-package catppuccin-theme
  :defer t) ; slow loading. Don't think this does anything in my config, defer is default

(use-package doom-themes)

(use-package doric-themes)

(use-package ef-themes
  :config
  (setopt ef-themes-mixed-fonts t)
  )

(use-package flexoki-themes
	:disabled
  :custom
  (flexoki-themes-use-bold-keywords t)
  (flexoki-themes-use-bold-builtins t)
  (flexoki-themes-use-italic-comments nil))

(use-package evenok
  ;;	interesting idea, requires special setup with https://codeberg.org/mekeor/unspecified-theme
	:disabled
	)

(use-package gruvbox-theme)

(use-package kaolin-themes)

(use-package nordic-night-theme)

(use-package modus-themes)

(use-package solarized-theme)


;;;; =================== Hooks for after loading a theme ===================

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

(provide 'editor-themes)
;;; editor-themes.el ends here
