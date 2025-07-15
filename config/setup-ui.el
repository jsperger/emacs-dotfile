;;; config/setup-ui.el --- UI configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mct
	:disabled
	:hook (vertico-mode . mct-mode))

(use-package bicycle
  :hook ((prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode))
  :general
  (tyrant-def
    "t TAB" 'bicycle-cycle
    "t <backtab>" 'bicycle-cycle-global
    )
  )

(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode))

(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package focus
  :disabled
	:general
	(tyrant-def "tF" 'focus-mode)
	)

(use-package indent-control
  :disabled)

(use-package nocomments-mode
	:disabled
	:general
	(tyrant-def
		"tc" 'nocomments-mode))

(use-package outline-indent
	:commands outline-indent-minor-mode
	:custom
  (outline-indent-ellipsis " ▼ ")
	:general
	(tyrant-def
		"to" 'outline-indent-minor-mode
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
	(setopt olivetti-style 'fancy
                olivetti-body-width 0.7
		olivetti-minimum-body-width 80
		olivetti-recall-visual-line-mode-entry-state t)
        )
   

(use-package shackle
  :config
  (setopt shackle-mode t
          shackle-default-size 0.2
          shackle-rules
          `((help-mode :select t :align right :size
                       fill-column)
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
  (setopt   visual-fill-column-center-text t
	    visual-fill-column-extra-text-width '(4 . 4)
            fill-column 80
   )
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
  :disabled
	)
(use-package calle24
	:disabled
	:hook (compilation-mode . calle24-refresh-appearance)
	:config
	(calle24-refresh-appearance)
)

(use-package activities
	:disabled
  :init
  (activities-mode)
  (activities-tabs-mode)
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

(use-package golden-ratio
  :disabled
  :config (setopt golden-ratio-mode nil)
	)

(use-package zoom
  :disabled)

(use-package eyebrowse
	:disabled)

(use-package popper
  :config
  (setopt popper-display-control nil
        popper-reference-buffers
        '("\*Messages\*"
          "Output\*$"
          "\*Async Shell Command\*"
          "\*eldoc\*"
          "^\*EGLOT"
          help-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          flymake-diagnostics-buffer-mode))
  :general
  (tyrant-def
    ";" '("pop toggle" . popper-toggle)
    ":" '("pop kill" . popper-kill-latest-popup))
  :custom
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(use-package sideline
  :init
  (use-package sideline-flymake
    :hook (flymake-mode . sideline-mode)
    :init
    (setopt sideline-backends-right '(sideline-flymake))
    (add-hook 'flymake-mode-hook
              (lambda () (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)))))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-ui.el ends here
