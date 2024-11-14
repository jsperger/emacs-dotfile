;;; lang-llm.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Sperger

;; Author: John Sperger

;;; Commentary: For interacting with large language models (LLMs)

;;; Code:

(if (eq system-type 'gnu/linux)
    (use-package ellama
      :config
      (setopt ellama-language "English")))



(provide 'lang-llm)

;;; lang-llm.el ends here
