;;; lang-llm.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Sperger

;; Author: John Sperger

;;; Commentary: For interacting with large language models (LLMs)

;;; Code:


(use-package llm
	;; developer-facing package that gptel and others rely on
	;; TODO: figure out how to declare providers here so they are available
	;; to ellama, gptel, or any other front ends
	)

(use-package ellama
  :disabled
  :init
  (require 'llm-openai-compatible)
   (setopt ellama-provider
  	       (make-llm-openai-comptaible
  :url "http://127.0.0.1:1234"))
  :config
  (setopt ellama-language "English")
  )

(use-package gptel
  :config
  ;; Llama.cpp offers an OpenAI compatible API
(gptel-make-openai "llama-cpp"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:1234"                ;Llama.cpp server location
  :models '(test))                    ;Any names, doesn't matter for Llama)
)

(provide 'lang-llm)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-llm.el ends here
