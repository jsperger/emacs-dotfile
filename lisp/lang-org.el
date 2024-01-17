;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package org
  :ensure t
  )

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

(use-package djvu)

(use-package nov)

(use-package org-noter
  :after org)

(use-package org-noter-pdftools
  :after org-noter pdftools)

;; (use-package fold-dwim-org
;;   :after org)

(provide 'lang-org)
;;; lang-org.el ends here
