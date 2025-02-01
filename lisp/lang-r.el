;;; lang-r.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package ess
  :hook ((r-mode R-mode ess-r-mode) . 'eglot-ensure)
  :config
  (despot-def (ess-r-mode-map)
    :major-modes 'ess-r-mode
    "b"            'ess-eval-buffer
    "c" 'ess-eval-region-or-function-or-paragraph
    "TAB" 'ess-install-library
    "f" 'ess-eval-function
    "r" 'run-ess-r
    )
  )

(use-package ess-view-data)

(use-package poly-R)

(provide 'lang-r)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-r.el ends here
