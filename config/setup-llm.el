;;; config/setup-llm.el --- LLM configuration -*- lexical-binding: t -*-

(use-package gptel
  :functions gptel--insert-file-string
  :config
  (setopt gptel-model   'llama-cpp
          gptel-backend (gptel-make-openai "llama-cpp"
                          :stream t
                          :protocol "http"
                          :host "localhost:1234"
                          :models '(llama-cpp))
          )

  (gptel-make-openai "OpenAI"
    :stream t
    :key (gptel-api-key-from-auth-source "api.openai.com" "apikey")
    )
  
  (gptel-make-openai "OpenRouter"               
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (gptel-api-key-from-auth-source "api.openrouter.ai" "apikey")
    )

  (gptel-make-anthropic "Claude"          
    :stream t
    :key (gptel-api-key-from-auth-source "api.anthropic.com" "apikey")
    )

  ;; (gptel-make-gemini "Gemini-API"
  ;;   :stream t
  ;;   :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com" "apikey")
  ;;   )
  
  :general (tyrant-def "aG" 'gptel-menu) 
  )

(use-package mcp-server-lib
  :after no-littering
  :custom (mcp-server-lib-install-directory no-littering-var-directory)
  :config
  (require 'mcp-server-lib-commands)
  ;; The stdio wrapper agents launch; it talks to this Emacs via emacsclient
  (unless (file-exists-p (mcp-server-lib-installed-script-path))
    (mcp-server-lib-install))
  )

(use-package elisp-dev-mcp
  :after mcp-server-lib)

(use-package agent-shell
  :after mcp-server-lib
  :config
  (require 'my-llm-helpers)
  (unless mcp-server-lib--running
    (mcp-server-lib-start))
  (setopt
   agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir
   agent-shell-file-completion-enabled t
   agent-shell-command-completion-enabled t
   agent-shell-mcp-servers
   `(((name . "elisp-dev")
      (command . ,(mcp-server-lib-installed-script-path))
      (args . ("--init-function=elisp-dev-mcp-enable"
               "--stop-function=elisp-dev-mcp-disable"
               "--server-id=elisp-dev-mcp"))
      (env . ())))
   agent-shell-anthropic-claude-environment
   (agent-shell-make-environment-variables :inherit-env t)
   agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)
   agent-shell-google-authentication
   (agent-shell-google-make-authentication :login t)
   agent-shell-google-gemini-environment (agent-shell-make-environment-variables :inherit-env t)
   )
  ;; Evil state-specific RET behavior: insert mode = newline, normal mode = send
  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map
    (kbd "RET") #'agent-shell-submit
    (kbd "M-RET") #'agent-shell-submit-override)

  ;; Configure *agent-shell-diff* buffers to start in Emacs state
  (add-hook 'diff-mode-hook
  	        (lambda ()
  	          (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
  		          (evil-emacs-state))))
  :general
  (tyrant-def
    "ag" 'agent-shell-google-start-gemini
    "ac" 'agent-shell-anthropic-start-claude-code
    "ai" (cons "agent-shell" (make-sparse-keymap))
    "aii" 'agent-shell
    "ait" 'agent-shell-toggle
    "aib" 'agent-shell-switch-buffer
    "ais" 'agent-shell-send-dwim
    "aif" 'agent-shell-send-current-file
    "aiw" 'agent-shell-new-worktree-shell
    "aih" 'agent-shell-help-menu
    )
  (despot-def agent-shell-mode-map
    "h" 'agent-shell-help-menu
    "r" 'agent-shell-quote-region
    "o" 'agent-shell-other-buffer
    "c" 'agent-shell-prompt-compose
    "f" 'agent-shell-fork
    "v" 'agent-shell-set-session-model
    "m" 'agent-shell-set-session-mode
    "t" 'agent-shell-set-session-thought-level
    "u" 'agent-shell-show-usage
    "i" 'agent-shell-send-clipboard-image
    )
  )

(use-package agent-shell-math-renderer
  :ensure t
  :hook (agent-shell-mode . agent-shell-math-renderer-mode)
  ;; Optional: re-tint equations the instant you switch themes, and
  ;; rescale them when the frame font changes.  See "Usage" below; omit
  ;; if you never change themes or font sizes at runtime.
  :config
  (add-hook 'enable-theme-functions
            #'agent-shell-math-renderer-on-appearance-change)
  (add-hook 'after-setting-font-hook
            #'agent-shell-math-renderer-on-appearance-change)
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-llm.el ends here
