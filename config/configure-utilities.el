;;; configure-utilities.el --- editor add-ons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; [[file:../its-lit.org::*Smooth scrolling][Smooth scrolling:1]]
(use-package ultra-scroll
  :ensure (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init  (setq scroll-conservatively 50
               scroll-margin 0) 
  :config (ultra-scroll-mode 1)
  )
;; Smooth scrolling:1 ends here

;; [[file:../its-lit.org::comment tools][comment tools]]
(use-package banner-comment
  :config  (setopt banner-comment-width 72)
  :general  (tyrant-def "ab" 'banner-comment)
  )
;; comment tools ends here

;; [[file:../its-lit.org::#outline-and-fold-text][Outline and fold text:1]]
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
;; Outline and fold text:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-utilities.el ends here
