;;; lang-llm.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024  John Sperger

;; Author: John Sperger

;;; Commentary: For interacting with large language models (LLMs)

;;; Code:

(use-package llm
	:config
	(use-package llm-openai
		:ensure nil)
	)

(use-package ellama
	:disabled
  :after llm
	:config
	(setopt ellama-provider (make-llm-openai-compatible
													 :url "http://127.0.0.1:1234"))
	)


(use-package gptel
  :config
	(setopt gptel-temperature 0.8)
  ;; Llama.cpp offers an OpenAI compatible API
(gptel-make-openai "llama-cpp"          ;Any name
  :stream t                             ;Stream responses
  :protocol "http"
  :host "localhost:1234"                ;Llama.cpp server location
  :models '(llama-cpp))                    ;Any names, doesn't matter for Llama)
)

(provide 'lang-llm)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-llm.el ends here
