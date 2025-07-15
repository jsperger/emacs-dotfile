;;; config/setup-llm.el --- LLM configuration -*- lexical-binding: t -*-

(use-package llm
	:config
	(use-package llm-openai
		:ensure nil)

	(use-package llm-claude
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
  :functions gptel--insert-file-string
	:config
	(setopt gptel-model   'llama-cpp
					gptel-backend (gptel-make-openai "llama-cpp"
													:stream t
													:protocol "http"
													:host "localhost:1234"
													:models '(llama-cpp)))

	(gptel-make-openai "OpenRouter"               
		:host "openrouter.ai"
		:endpoint "/api/v1/chat/completions"
		:stream t
		:key (gptel-api-key-from-auth-source "api.openrouter.ai" "apikey")
		:models '(google/gemini-2.5-flash
							google/gemini-2.5-pro
              openrouter/auto
							anthropic/claude-sonnet-4
              deepseek/deepseek-r1-0528:free
              deepseek/deepseek-prover-v2:free
							open-r1/olympiccoder-32b:free
							mistralai/devstral-small:free
              google/gemma-3-27b-it:free
							deepseek/deepseek-r1-zero:free
							qwen/qwq-32b:free)
		)

	(gptel-make-anthropic "Claude"          
		:stream t
		:key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
		)

  (gptel-make-gemini "Gemini-API"
    :stream t
    :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com" "apikey")
    )

	)

(use-package aidermacs
	:general (tyrant-def "aa" 'aidermacs-transient-menu)
  :config
	(setopt aidermacs-backend 'vterm)
  (setenv "GEMINI_API_KEY" (gptel-api-key-from-auth-source "generativelanguage.googleapis.com" "apikey"))
  (setenv "ANTHROPIC_API_KEY" (gptel-api-key-from-auth-source "api.anthropic.com" "apikey"))
  (setenv "OPENROUTER_API_KEY" (gptel-api-key-from-auth-source "api.openrouter.ai" "apikey"))
	(setenv "LM_STUDIO_API_KEY" "dummy-api-key")
	(setenv "LM_STUDIO_API_BASE" "http://localhost:1234/v1") 
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "gemini/gemini-pro-2.5")
  )

(use-package forge-llm
	:disabled
	:after forge llm
	:custom
	(forge-llm-llm-provider
	 (make-llm-claude
		:key (auth-source-pick-first-password
					:host "api.anthropic.com"
					:user "apikey")
		:chat-model "claude-3-7-sonnet-20250219"
		)
	 )
	 :config
	 (forge-llm-setup)
	 :general
	 (tyrant-def
		 "gg" 'forge-llm-generate-pr-description
		 "gp" 'forge-llm-generate-pr-description-at-point
		 "gt" 'forge-llm-insert-template-at-point
		 )
	 )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-llm.el ends here
