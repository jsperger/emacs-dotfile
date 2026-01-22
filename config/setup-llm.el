;;; config/setup-llm.el --- LLM configuration -*- lexical-binding: t -*-

(use-package gptel
  :functions gptel--insert-file-string
  :config (setopt gptel-model   'llama-cpp
                  gptel-backend (gptel-make-openai "llama-cpp"
                                  :stream t
                                  :protocol "http"
                                  :host "localhost:1234"
                                  :models '(llama-cpp))
                  )

  (gptel-make-openai "OpenAI"
    :stream t
    :key (gptel-api-key-from-auth-source "api.openai.com" "apikey"))
  (gptel-make-openai "OpenRouter"               
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (gptel-api-key-from-auth-source "api.openrouter.ai" "apikey")
    :models '(google/gemini-3.0-flash
	            google/gemini-3.0-pro
              openrouter/auto
              )
    )

  (gptel-make-anthropic "Claude"          
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
    )

  (gptel-make-gemini "Gemini-API"
    :stream t
    :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com" "apikey")
    )
  
  :general (tyrant-def "aG" 'gptel-menu)
  
  )

(use-package mcp-server-lib
  :after no-littering
  :custom (mcp-server-lib-install-directory 'no-littering-var-directory)
  )

(use-package elisp-dev-mcp
  :after mcp-server-lib)

(use-package chatgpt-shell
  :config
  (setopt chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")
          chatgpt-shell-anthropic-key (getenv "ANTHROPIC_API_KEY")
          )
  :general (tyrant-def "as" 'chatgpt-shell-transient)
  )

(use-package agent-shell
  :config
  (setopt
   agent-shell-anthropic-claude-environment        (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-anthropic-authentication          (agent-shell-anthropic-make-authentication :login t)
   )
  agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-llm.el ends here
