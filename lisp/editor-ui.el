;;; editor-ui.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:
;; Organized by
;; Buffers
;; Frames / Tabs / Windows
;; Modeline
;; Decoration (not including fonts or themes)
;;;

;;; Code:

;;;
;; --- Buffers ---
;;;
(use-package bicycle
  :hook ((prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode))
  :general
  (tyrant-def
    "t TAB" 'bicycle-cycle
    "t <backtab>" 'bicycle-cycle-global
    )
  )

(use-package indent-control
  :disabled)

(use-package shackle
  :config
  (shackle-mode 1)
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


(use-package visual-fill-column
  :custom
  (global-visual-fill-column-mode 1 "Enable global mode")
  (visual-fill-column-center-text t "Center instead of beginning at margin")
  (visual-fill-column-extra-text-width '(4 . 2))
  )

(use-package writeroom-mode
  :disabled
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



;;;
;; --- Frames / Tabs / Windows ---
;;;
(use-package golden-ratio
  :custom (golden-ratio-mode nil))


(use-package popper
  :config
  (setopt popper-display-control nil
        popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*eldoc\\*"
          "^\\*EGLOT"
          help-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          flymake-diagnostics-buffer-mode))
  :general
  (tyrant-def
    ";" 'popper-toggle
    ":" 'popper-kill-latest-popup)
	:custom (popper-mode t)
	(popper-echo-mode t))

(use-package sideline
  :init
  (use-package sideline-flymake
    :hook (flymake-mode . sideline-mode)
    :init
    (setopt sideline-backends-right '(sideline-flymake))
    (add-hook 'flymake-mode-hook
              (lambda () (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)))))


;;;
;; --- Modeline ---
;;;
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
        doom-modeline-persp-name nil)
  :custom (doom-modeline-mode 1))

;; Moody https://github.com/tarsius/moody
;; Tabs and ribbons for the mode line
(use-package moody
	:disabled
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package nano-modeline
  :disabled)

;;;
;; Decoration (not including fonts or themes)
;;;

(use-package kind-icon
  :after corfu
  :custom
   (kind-icon-blend-background t)
   (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'editor-ui)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-ui.el ends here
