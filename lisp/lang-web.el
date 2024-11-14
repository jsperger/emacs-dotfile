;;; lang-org.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package easy-hugo
  :config
  (setq
   easy-hugo-basedir "~/work/code/websites/jsperger/"
   ))

(use-package mermaid-mode)

(use-package toml)

(provide 'lang-web)
;;; lang-web.el ends here
