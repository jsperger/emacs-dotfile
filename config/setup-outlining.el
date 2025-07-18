;;; setup-outlining.el ---  -*- lexical-binding: t; -*-
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    #'outline-minor-faces-mode)
  )

(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update)
  )

(use-package outline-indent
  :commands outline-indent-minor-mode
  :custom
  (outline-indent-ellipsis " ▼ ")
  :general
  (tyrant-def
    "to" 'outline-indent-minor-mode
    "o"  (cons "outline" (make-sparse-keymap))
    "oa" '("all open" . outline-indent-open-folds)
    "oA" '("all closed" . outline-indent-close-folds)
    "ob" 'bicycle-cycle
    "oB" 'bicycle-cycle-global
    "oc" 'outline-indent-close-fold
    "oj" 'outline-forward-same-level
    "ok" 'outline-backward-same-level
    "oh" 'hs-minor-mode
    "oo" 'outline-indent-open-fold
    "oO" 'outline-minor-mode
    "or" 'outline-indent-open-fold-rec
    "ot" 'bicycle-cycle
    "oT" 'bicycle-cycle-global
    "oz" 'TeX-fold-mode)
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-outlining.el ends here
