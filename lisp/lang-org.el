;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package org
  :ensure t
  )

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))

(use-package org-transclusion
  :after org)

(use-package org-contrib
  :after org)

(use-package djvu)

(use-package nov)

(use-package org-pdftools
  :after org pdftools)

(use-package evil-org
  :after org)

(use-package org-noter
  :after org)

(use-package org-noter-pdftools
  :after org-noter pdftools)

;; (use-package fold-dwim-org
;;   :after org)

(provide 'lang-org)
;;; lang-org.el ends here
