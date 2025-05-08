;;; lang-data-formats.el --- -*- lexical-binding: t; -*-


;; Author: Tianshu Wang, John Sperger

;;; Commentary:

;;; Code:

;; -------------------------------------
;; --- Tabular data ---
;; -------------------------------------

(use-package csv-mode
  :mode (".csv" . csv-mode)
  :config
  (add-hook 'csv-mode-hook 'csv-guess-set-separator)
	:general
  (despot-def csv-mode-map
    "s" 'csv-sort-fields
    "n" 'csv-sort-numeric-fields
    "r" 'csv-reverse-region
    "k" 'csv-kill-fields
    "y" 'csv-yank-fields
    "a" 'csv-align-fields
    "A" 'csv-align-mode
    "u" 'csv-unalign-fields
    "t" 'csv-transpose
    )
  )


(use-package emacsql)

(use-package json-navigator
	:disabled
	)

(provide 'lang-data-formats)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-data-formats.el ends here
