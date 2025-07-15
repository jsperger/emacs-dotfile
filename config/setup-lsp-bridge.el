;;; config/setup-lsp-bridge.el --- lsp-bridge configuration -*- lexical-binding: t -*-

(require 'my-lsp-bridge-helpers)

(use-package lsp-bridge
  :ensure (lsp-bridge
           :type git :host github :repo "manateelazycat/lsp-bridge"
           :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
           :build (:not elpaca--byte-compile)
           )
  :init
  (setq lsp-bridge-python-command lsp-bridge-python-path)

  :hook (elpaca-after-init . global-lsp-bridge-mode)
  :config
  (setopt toml-indent-offset 2)
  :custom (global-lsp-bridge-mode t)

  (use-package acm
    :ensure nil
    )
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-lsp-bridge.el ends here
