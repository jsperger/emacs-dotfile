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
	(setopt gptel-model   'llama-cpp
					gptel-backend (gptel-make-openai "llama-cpp"
													:stream t
													:protocol "http"
													:host "localhost:1234"
													:models '(llama-cpp)))

	;; OpenRouter offers an OpenAI compatible API
	(gptel-make-openai "OpenRouter"               
		:host "openrouter.ai"
		:endpoint "/api/v1/chat/completions"
		:stream t
		:key (gptel-api-key-from-auth-source "api.openrouter.ai" "apikey")
		:models '(google/gemini-2.0-flash-001
							anthropic/claude-3.7-sonnet
							mistralai/mistral-small-3.1-24b-instruct:free
							open-r1/olympiccoder-32b:free
							google/gemma-3-27b-it:free
							deepseek/deepseek-r1-zero:free
							qwen/qwq-32b:free)
		)

	(gptel-make-anthropic "Claude"          
		:stream t
		:key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
		)

	(gptel-make-anthropic "Claude-thinking" ;Any name you want
		:key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
		:stream t
		:models '(claude-3-7-sonnet-20250219)
		:header (lambda () (when-let* ((key (gptel--get-api-key)))
												 `(("x-api-key" . ,key)
													 ("anthropic-version" . "2023-06-01")
													 ("anthropic-beta" . "pdfs-2024-09-25")
													 ("anthropic-beta" . "output-128k-2025-02-19")
													 ("anthropic-beta" . "prompt-caching-2024-07-31"))))
		:request-params '(:thinking (:type "enabled" :budget_tokens 8192)
																:max_tokens 65536))
	)

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config

(provide 'lang-llm)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-llm.el ends here
