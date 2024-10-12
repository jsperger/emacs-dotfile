;;; lang-lisp.el --- -*- lexical-binding: t; -*-
;; Copyright (C) 2024  John Sperger
;; Author: John Sperger
;;; Commentary:
;;; Code:

(use-package slime
  :hook ((r-mode R-mode ess-r-mode) . 'eglot-ensure)
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

(provide 'lang-lisp)
;;; lang-lisp.el ends here
