;;; lang-lisp.el --- -*- lexical-binding: t; -*-
;; Copyright (C) 2024  John Sperger
;; Author: John Sperger
;;; Commentary:
;;; Code:

(use-package slime
	:disabled
;;	:hook (inferior-lisp . slime-mode)
  :config
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
    "tq" 'ert))


(provide 'lang-lisp)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-lisp.el ends here
