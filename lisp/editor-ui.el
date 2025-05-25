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

(use-package focus
	;;	:hook text-mode
;;	:hook visual-line-fill-column-mode
	:general
	(tyrant-def "tF" 'focus-mode)
	)

(use-package indent-control
  :disabled)

(use-package nocomments-mode
	;; remove comments from the visual display by replacing them with blank characters
	;; Does not get rid of the space that the comments took up
	:disabled
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
	:hook ((text-mode prog-mode org-mode) . olivetti-mode)
	:config
	(setopt olivetti-style 'fancy)
	)

(use-package shackle
	:config
	(setopt shackle-mode t
					shackle-default-size 0.2
					shackle-rules `(
													(help-mode :select t :align right :size ,fill-column)
													(helpful-mode :select t :align right :size ,fill-column)
													("*Messages*"                    :select t :align t)
													("*eldoc*"                       :align t)
													(special-mode                    :align t)
													(process-menu-mode               :align t)
													(compilation-mode                :align t)
													(flymake-diagnostics-buffer-mode :align t)
													("*Shell Command Output*"        :align t :size 2)
													("*Async Shell Command*"         :align t :size 2)
													("*elpaca-info*" :align t)
													;;("\\*EGLOT.*" :select t :align right :size , fill-column :regexp t)
													))
	)

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

(use-package logos
	;; https://protesilaos.com/emacs/logos
  ;; a simple “focus mode” which can be applied to any buffer for reading, writing, or even doing a presentation. The buffer can be divided in pages using the page-delimiter, outline structure, or any other pattern
	:config
	(setopt olivetti-body-width 0.7
				olivetti-minimum-body-width 80
				olivetti-recall-visual-line-mode-entry-state t)
	)
;; ============================== Frames ============================== 
(use-package calle24
	:disabled
	:hook (compilation-mode . calle24-refresh-appearance)
	:config
	(calle24-refresh-appearance)
)

;; =============================== Tabs =============================== 
(use-package activities
	:disabled
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))


;; ============================== Windows ============================= 
(use-package golden-ratio
  :config (setopt golden-ratio-mode nil)
	)

(use-package eyebrowse
	:disabled)

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

(use-package sideline-flymake
	:disabled
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'point) ; 'point to show errors only on point
                                              ; 'line to show errors on the current line
  (setq sideline-backends-right '(sideline-flymake)))

;; ============================= Modeline ============================= 

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

(provide 'editor-ui)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-ui.el ends here
