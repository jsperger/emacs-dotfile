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

;;  ------------------------------------
;;  --- Minibuffer ---
;;  ------------------------------------
(use-package mct
	:disabled
	:hook (vertico-mode . mct-mode))

;;  ------------------------------------
;;  --- Buffers ---
;;  ------------------------------------
(use-package bicycle
  :hook ((prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode))
  :general
  (tyrant-def
    "t TAB" 'bicycle-cycle
    "t <backtab>" 'bicycle-cycle-global
    )
  )

;; Make folded outline highlighting go to the end of the line, not the ellipsis
;; https://github.com/tarsius/outline-minor-faces
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))

;; https://github.com/tarsius/backline
(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))


(use-package indent-control
  :disabled)

(use-package nocomments-mode
	:general
	(tyrant-def
		"tc" 'nocomments-mode))

(use-package outline-indent
;;	:hook (yaml-ts-mode yaml-mode . outline-indent-minor-mode)
	:commands outline-indent-minor-mode
	:custom
  (outline-indent-ellipsis " ▼ ")
	:general
	(tyrant-def
		"to" 'outline-indent-minor-mode

		;; Really a general folding repository
    "o"       (cons "outline" (make-sparse-keymap))
		"oa" '("all open" . outline-indent-open-folds)
		"oA" '("all closed" . outline-indent-close-folds)
		"ob" 'bicycle-cycle
		"oB" 'bicycle-cycle-global
		"oc" 'outline-indent-close-fold
		"oj" 'outline-forward-same-level
		"ok" 'outline-backward-same-level
		"oh"      'hs-minor-mode
		"oo" 'outline-indent-open-fold
		"oO"      'outline-minor-mode
		"or" 'outline-indent-open-fold-rec
		"ot" 'bicycle-cycle
		"oT" 'bicycle-cycle-global
		"oz"      'TeX-fold-mode))

(use-package olivetti
	:hook (text-mode . olivetti-mode)
	:config
	(setopt olivetti-style 'fancy))

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
	:hook (elpaca-after-init . global-visual-fill-column-mode)
  :config
	;;  (global-visual-fill-column-mode 1 "Enable global mode")
	(setopt visual-fill-column-center-text t ; "Center instead of beginning at margin"
					visual-fill-column-extra-text-width '(4 . 4))
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
  :config (golden-ratio-mode nil))


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
    ";" '("pop toggle" . popper-toggle)
    ":" '("pop kill" . popper-kill-latest-popup)))

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
	:hook (elpaca-after-init . doom-modeline-mode)
  :config
  (setopt inhibit-compacting-font-caches t
				doom-modeline-buffer-encoding nil
        doom-modeline-height 0
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-project-detection 'project
        doom-modeline-icon t
        doom-modeline-major-mode-icon nil
				doom-modeline-modal-icon nil
        doom-modeline-gnus nil
        doom-modeline-irc nil
        doom-modeline-persp-name nil
;;				doom-modeline-enable-word-count t
;;doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
				)
	;;	(when IS-MAC (setopt doom-modeline-battery t))
)

(use-package nerd-icons
	;;	:hook (doom-modeline-mode)
	:custom (nerd-icon-scale-factor 0.8)
)
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

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
	:config
	(setopt
	 nerd-icons-ibuffer-icon t ; Whether display the icons.
	 nerd-icons-ibuffer-color-icon t ; Whether display the colorful icons.
	 ;; It respects `nerd-icons-color-icons'.
	 nerd-icons-ibuffer-icon-size 1.0
	 ;; The default icon size in ibuffer.
	 nerd-icons-ibuffer-human-readable-size t ; Use human readable file size in ibuffer.
	 ;; A list of ways to display buffer lines with `nerd-icons'.
	 ;; See `ibuffer-formats' for details.
	 ;; nerd-icons-ibuffer-formats

	 ;; Slow Rendering
	 ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
	 ;; you can try setting the following variable
	 inhibit-compacting-font-caches t)
	)

(use-package nerd-icons-corfu
	:init
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;; ;; Optionally:
;; (setq nerd-icons-corfu-mapping
;;       '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
;;         ;; ...
;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
;;         ;; Remember to add an entry for `t', the library uses that as default.
;; The Custom interface is also supported for tuning the variable above.
	)

(use-package all-the-icons
	:disabled
  :if (display-graphic-p))

(use-package all-the-icons-dired
	:disabled
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
	:disabled
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package kind-icon
	:disabled
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
