;;; config/setup-r.el --- R language configuration -*- lexical-binding: t -*-

(use-package ess
  :mode ("\([rR]\)\'" . R-mode)
  :init
  (setq ess-set-style t
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        ess-indent-offset 2)
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (add-to-list 'mode-line-process '(:eval (nth ess--busy-count ess-busy-strings)))))
  :custom
  (inferior-R-args "--no-save")
  (ess-indent-offset 2)
  (ess-own-style-list '(
                        (ess-indent-offset . 2)
                        (ess-offset-arguments . prev-call)
                        (ess-offset-arguments-newline . prev-line)
                        (ess-offset-block . prev-line)
                        (ess-offset-continued . straight)
                        (ess-align-nested-calls "ifelse")
                        (ess-align-arguments-in-calls "function[ 	]*(")
                        (ess-align-continuations-in-calls . t)
                        (ess-align-blocks control-flow)
                        (ess-indent-from-lhs arguments fun-decl-opening)
                        (ess-indent-from-chain-start . t)
                        (ess-indent-with-fancy-comments . nil))
                      )
  (ess-style 'OWN)
  :config
  :general
  (despot-def (ess-r-mode-map)
    :major-modes '(ess-r-mode R-mode)
    "b" 'ess-eval-buffer
    "c" 'ess-eval-region-or-function-or-paragraph
    "TAB" 'ess-install-library
    "f" 'ess-eval-function
    "F" 'air-format
    "r" 'run-ess-r
    "v" 'oc-set-ess-offset
    )
  )

(use-package r-ts-mode
  :disabled
  :hook (R-mode . r-ts-mode)
  :ensure (r-ts-mode
	   :type git :host github :repo "nverno/r-ts-mode"))

(use-package essgd
	:general
	  (despot-def (ess-r-mode-map)
			:major-modes 'ess-r-mode
			"g" 'essgd-start)
	)

(use-package ess-view-data
	:disabled)

(use-package poly-R
  :mode ("\\.qmd\\'" . poly-markdown+R-mode)
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-r.el ends here
