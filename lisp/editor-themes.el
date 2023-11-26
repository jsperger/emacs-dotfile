;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

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

;; TODO: Causes an error to be thrown. Maybe outline comes from a package?
;; (add-hook 'after-load-theme-hook
;;           (defun unscale-outlines ()
;;             (dolist (outline-number (number-sequence 1 8))
;;               (let ((outline (intern (format "outline-%d" outline-number))))
;;                 (set-face-attribute outline nil :height 1.0)))))


;;; Theme packages

(use-package solarized-theme)

(use-package doom-themes)

(use-package lacquer
  :after  solarized-themes doom-themes
  :config
    (lacquer-cache "~/.emacs.d/.lacquer.el")
)



(provide 'editor-themes)
;;; editor-themes.el ends here
