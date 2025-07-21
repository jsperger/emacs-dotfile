;;; config/setup-lsp-bridge.el --- lsp-bridge configuration -*- lexical-binding: t -*-

;; testing out the built-in uv support
;; don't require my helpers for setting up the uv python venv

(use-package lsp-bridge
  :after yasnippet
  :ensure (lsp-bridge
           :type git :host github :repo "manateelazycat/lsp-bridge"
           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not elpaca--byte-compile)
           )
  :config  (setopt toml-indent-offset 2)

;;  (global-lsp-bridge-mode)

   :general
  (tyrant-def "l" (cons "lsp" (make-sparse-keymap))
    "lf" 'lsp-bridge-find-def
    "lF" 'lsp-bridge-find-def-other-window
    "lh" 'lsp-bridge-show-documentation
    "lp" 'lsp-bridge-popup-documentation
    )
  )

(add-hook 'elpaca-after-init-hook #'global-lsp-bridge-mode)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-lsp-bridge.el ends here
