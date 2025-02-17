;;; tools-lsp-bridge.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;; commentary

;;; Code:

(use-package yasnippet
	:hook ((text-mode prog-mode) . yas-minor-mode)
	)

(use-package yasnippet-snippets)

(use-package lsp-bridge
	:after yasnippet
  :ensure (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))


(provide 'tools-lsp-bridge)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-lsp-bridge.el ends here
