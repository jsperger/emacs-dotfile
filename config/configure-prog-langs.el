;;; configure-prog-langs.el --- lang-specific -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration that's programming language specific
;; So LSP config doesn't live here
;;; Code:

;; [[file:../its-lit.org::#lisp-based-languages][Lisp-based languages:1]]
;;;; ========================= Lisp languages ==========================
(use-package slime
	:hook (elisp-mode . slime-mode)
  :config
	(setopt inferior-lisp-program "sbcl")
	(add-hook 'slime-mode-hook (setq-local lsp-bridge-mode -1))
  :general
  (despot-def (slime-mode-map)
    :major-modes 'slime-mode
    "b" 'slime-eval-buffer
    "c" 'slime-compile-defun
    "k" 'slime-compile-and-load-file
    "f"  'ad-Advice-slime-eval-defun
    "r" 'slime-eval-region
    )
  )

(use-package elisp-mode
  :ensure nil
  :config
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "'"  'ielm
    "c"  (cons "compile" (make-sparse-keymap))
    "cc" 'emacs-lisp-byte-compile
    "e"  (cons "eval" (make-sparse-keymap))
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "ef" 'eval-defun
    "t"  (cons "tests" (make-sparse-keymap))
    "tb" 'ert-run-tests-buffer
    "tq" 'ert)
  )

(use-package racket-mode
  :mode ("\\.rkt\\'" . racket-mode)
	)
;; Lisp-based languages:1 ends here

;; [[file:../its-lit.org::#python-configuration][Python configuration:1]]
;;;; ============================== Python ==============================
(use-package uv-mode
	:hook (python-mode . uv-mode)
  )
;; Python configuration:1 ends here

;; [[file:../its-lit.org::#r-programming][R programming:1]]
(use-package ess
  :mode ("\\.[rR]\\'" . R-mode)
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
  :config (setopt  ess-ido-flex-matching nil
                   ess-use-R-completion nil
                   ess-use-company nil
                   ess-use-flymake nil
                   ess-use-ido nil
                   ess-indent-offset 2
                   ess-own-style-list '(
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
                        (ess-indent-with-fancy-comments . nil)
                        )
                   ess-style 'OWN
                   )
   (with-eval-after-load 'evil
    (evil-set-initial-state 'ess-r-help-mode 'normal)
    )
   
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

(use-package essgd
  :after ess
	:general
	(despot-def (ess-r-mode-map)
		:major-modes 'ess-r-mode
		"g" 'essgd-start)
	)

(use-package poly-R
  :mode ("\\.qmd\\'" . poly-markdown+R-mode)
  )
;; R programming:1 ends here

;; [[file:../its-lit.org::#rust-programming][Rust programming:1]]
(use-package rust-mode
	:mode ("\\.rs\\'" . rust-mode)
  :general
  (despot-def (rust-mode-map)
    :major-modes '(rust-ts-mode rust-mode)
    "c"            'rust-compile
		"k" 'rust-check
		"t" 'rust-test
		"r" 'rust-run
		"F" 'rust-format-buffer)
	)

(use-package cargo-mode
  :hook (rust-mode . cargo-minor-mode)
  :config  (setopt compilation-scroll-output t
                   rust-format-on-save t)
  :general (despot-def (rust-mode-map)
             :major-modes '(rust-mode rust-ts-mode)
             "e" 'cargo-mode-execute-task
             "t" 'cargo-mode-test
             "l" 'cargo-mode-last-command
             "b" 'cargo-mode-build
             "o" 'cargo-mode-test-current-buffer
             "f" 'cargo-mode-test-current-test
             )
  )
;; Rust programming:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-prog-langs.el ends here
