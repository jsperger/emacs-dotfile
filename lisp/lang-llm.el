;;; lang-llm.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Sperger

;; Author: John Sperger

;;; Commentary: For interacting with large language models (LLMs)

;;; Code:


(use-package ellama
  :config
  (setopt ellama-language "English"))

(use-package gptel
  :config
  ;; Llama.cpp offers an OpenAI compatible API
(gptel-make-openai "llama-cpp"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:8000"                ;Llama.cpp server location
  :models '(test))                    ;Any names, doesn't matter for Llama)
)

(provide 'lang-llm)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-llm.el ends here
