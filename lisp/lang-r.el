;;; lang-r.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package ess
  :hook ((r-mode R-mode) . 'eglot-ensure)
  :config
  (despot-def (r-mode-map)
    :major-modes '(R-mode r-mode)
    "b"            'ess-eval-buffer
    "c" 'ess-eval-region-or-function-or-paragraph
    "TAB" 'ess-install-library
    "f" 'ess-eval-function
    "r" 'run-ess-r
    )
  )

(use-package ess-view-data)

(provide 'lang-r)
;;; lang-rust.el ends here
