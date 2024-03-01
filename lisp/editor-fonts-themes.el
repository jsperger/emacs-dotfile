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

;;;;
;; Font Defaults
;;

(defvar default-font-family "JetBrains Mono NL")
(defvar font-size 12)
(defvar default-font-width 'normal)
(defvar default-font-weight 'regular)

(defvar unicode-font "Noto Sans CJK SC")
(defvar unicode-scale (/ 16.0 font-size))
(defvar emoji-font "Noto Color Emoji")
(defvar symbol-font "Noto Sans Symbols")

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        frame-resize-pixelwise t
        font-size 12)

  (setq unicode-font "Noto Sans CJK SC"
        emoji-font "Apple Color Emoji"
        symbol-font "Apple Symbols"))

(defun setup-font (&rest args)
  (set-face-attribute 'default nil
                      :family default-font-family
                      :width default-font-width
                      :height (* font-size 10)  ; The height in Emacs is usually specified in tenths of a point.
                      :weight default-font-weight)
  (when (fboundp 'set-fontset-font)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset unicode-font))
    (add-to-list 'face-font-rescale-alist `(,unicode-font . ,unicode-scale))
    (set-fontset-font t 'emoji emoji-font nil 'prepend)
    (set-fontset-font t 'symbol symbol-font nil 'prepend))
  )

;;;;
;; Packages
;;
;;;;

(use-package fontaine)


;;; Theme packages

;; (use-package nano-theme
;;  :disabled)

(use-package nano-theme)

(use-package nano-modeline
  :disabled
  :hook
  (prog-mode-hook          . nano-modeline-prog-mode)
  (text-mode-hook          . nano-modeline-text-mode)
  (org-mode-hook           . nano-modeline-org-mode)
  (pdf-view-mode-hook      . nano-modeline-pdf-mode)
  (mu4e-headers-mode-hook  . nano-modeline-mu4e-headers-mode)
  (mu4e-view-mode-hook     . nano-modeline-mu4e-message-mode)
  (elfeed-show-mode-hook   . nano-modeline-elfeed-entry-mode)
  (elfeed-search-mode-hook . nano-modeline-elfeed-search-mode)
  (term-mode-hook          . nano-modeline-term-mode)
  (xwidget-webkit-mode-hook  . nano-modeline-xwidget-mode)
  (messages-buffer-mode-hook . nano-modeline-message-mode)
  (org-capture-mode-hook   . nano-modeline-org-capture-mode)
  (org-agenda-mode-hook    . nano-modeline-org-agenda-mode))

(use-package ef-themes)

(use-package catppuccin-theme)

(use-package doom-themes)

(use-package gruvbox-theme)

(use-package solarized-theme)


(provide 'editor-fonts-themes)
;;; editor-themes.el ends here
