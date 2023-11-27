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

        doom-modeline-gnus nil
        doom-modeline-icon nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil))

(use-package shackle
  :config
  (setq shackle-default-size 0.4
        shackle-rules `((help-mode                       :select t :align right :size ,fill-column)
                        (helpful-mode                    :select t :align right :size ,fill-column)
                        ("*Messages*"                    :select t :align t)
                        ("*eldoc*"                       :align t)
                        (special-mode                    :align t)
                        (process-menu-mode               :align t)
                        (compilation-mode                :align t)
                        (flymake-diagnostics-buffer-mode :align t)
                        ("*Shell Command Output*"        :align t)
                        ("*Async Shell Command*"         :align t)
                        ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t))))

(use-package writeroom-mode
  :config
  (setq split-width-threshold 120

        writeroom-width 128
        writeroom-fringes-outside-margins nil
        writeroom-global-effects nil
        writeroom-major-modes '(text-mode prog-mode conf-mode special-mode Info-mode dired-mode)
        writeroom-major-modes-exceptions '(process-menu-mode proced-mode backtrace-mode)
        writeroom-maximize-window t
        writeroom-mode-line t
        writeroom-mode-line-toggle-position 'mode-line-format)
  :general
  (tyrant-def
    "wc" 'writeroom-mode
    "wC" 'global-writeroom-mode))

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
  :elpaca t
  :defer t
  :init
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun compilation-filter@around (fn proc string)
    (funcall fn proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'compilation-filter@around))

(use-package fira-code-mode
  :elpaca t
  :if (display-graphic-p)
  :custom (fira-code-mode-disabled-ligatures '(
            "[]" "#{" "#(" "#_" "#_(" "x" ">>" "<<" "\\" "==" ">>>" "<<<" "!="
            ":" ";" ": " ".." "<-" "%%")) ;; List of ligatures to turn off
  :hook prog-mode ;; Enables fira-code-mode automatically for programming major modes
  )

(use-package bookmark-in-project
  :elpaca t)

(provide 'editor-ui)
;;; editor-ui.el ends here
