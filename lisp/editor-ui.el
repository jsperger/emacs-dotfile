;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package doom-modeline
  :config
  (setq inhibit-compacting-font-caches t
        doom-modeline-height 0
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project

        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil

        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil))


(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( internal-border-width 10
           header-line-width 4
           mode-line-width 6
           tab-width 4
           right-divider-width 20
           scroll-bar-width 8)
        )
  (setq spacious-padding-subtle-mode-line t)
  )

(use-package nano-modeline
  :disabled)

(use-package shackle
  :config
  (setq shackle-default-size 0.4
        shackle-rules `(
                        (help-mode :select t :align right :size ,fill-column)
                        (helpful-mode :select t :align right :size ,fill-column)
                        ("*Messages*"                    :select t :align t)
                        ("*eldoc*"                       :align t)
                        (special-mode                    :align t)
                        (process-menu-mode               :align t)
                        (compilation-mode                :align t)
                        (flymake-diagnostics-buffer-mode :align t)
                        ("*Shell Command Output*"        :align t)
                        ("*Async Shell Command*"         :align t)
                        ("\\*EGLOT.*" :select t :align right :size ,
                         fill-column :regexp t))))

(use-package writeroom-mode
  :config
  (setq split-width-threshold 120
        writeroom-width 128
        writeroom-fringes-outside-margins nil
        writeroom-global-effects nil
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode backtrace-mode)
        writeroom-maximize-window t
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format)
  :general
  (tyrant-def
    "wc" 'writeroom-mode
    "wC" 'global-writeroom-mode))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-center-text t)
  (setq visual-fill-column-extra-text-width '(4 . 2))
  )

(use-package hl-todo)

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  ;; (setq highlight-parentheses-colors '("Springgreen3"
  ;;                                      "IndianRed1"
  ;;                                      "IndianRed3"
  ;;                                      "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package xterm-color
  :defer t
  :init
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun compilation-filter@around (fn proc string)
    (funcall fn proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'compilation-filter@around))


;; Commented out because it didn't seem to want to respect being disabled
;; (use-package fira-code-mode
;;   :disabled
;;   :if (display-graphic-p)
;;   :custom (fira-code-mode-disabled-ligatures '(
;;             "[]" "#{" "#(" "#_" "#_(" "x" ">>" "<<" "\\" "==" ">>>" "<<<" "!="
;;             ":" ";" ": " ".." "<-" "%%")) ;; List of ligatures to turn off
;;   :hook prog-mode ;; Enables fira-code-mode automatically for programming major modes
;;   )


(use-package bookmark-in-project)

(use-package golden-ratio
  :config (golden-ratio-mode nil))


(provide 'editor-ui)
;;; editor-ui.el ends here
