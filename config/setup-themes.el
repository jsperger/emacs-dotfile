;;; config/setup-themes.el --- Themes configuration -*- lexical-binding: t -*-

(require 'my-theme-helpers)

(use-package catppuccin-theme
  :defer t)

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
	:disabled
	)

(use-package gruvbox-theme)

(use-package kaolin-themes)

(use-package nordic-night-theme)

(use-package modus-themes)

(use-package solarized-theme)

(advice-add 'load-theme :after #'load-theme@run-hooks)
(advice-add 'load-theme :before #'load-theme@theme-dont-propagate)

(add-hook 'after-load-theme-hook #'bolder-faces)
(add-hook 'after-load-theme-hook #'customize-tab-bar)
(add-hook 'after-load-theme-hook #'unscale-outlines)
(add-hook 'after-load-theme-hook #'my-disable-flymake-underline)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-themes.el ends here
