;;; configure-prog-tools.el --- programming add-ons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; ========================= Language Server packages ========================

;; [[file:../its-lit.org::lsp-bridge block][lsp-bridge block]]
(use-package lsp-bridge
  :after yasnippet

  :ensure (lsp-bridge :type git
                      :host github
                      :repo "manateelazycat/lsp-bridge"
                      :files (:defaults "*.el" "*.py" "acm" "core" "langserver"
                                        "multiserver" "resources")
                      :build (:not elpaca--byte-compile)
                      )

  :config  (setopt toml-indent-offset 2
                   lsp-bridge-enable-hover-diagnostic t
                   lsp-bridge-enable-org-babel t
                   acm-enable-preview t
                   )

  :general  (tyrant-def "l" (cons "lsp" (make-sparse-keymap))
              "lf" 'lsp-bridge-find-def
              "lF" 'lsp-bridge-find-def-other-window
              "lh" 'lsp-bridge-show-documentation
              "lp" 'lsp-bridge-popup-documentation
              )
  )

(add-hook 'elpaca-after-init-hook #'global-lsp-bridge-mode)
;; lsp-bridge block ends here

;; [[file:../its-lit.org::#code-formatting][Code formatting:1]]
(use-package apheleia
  :config
  (add-to-list 'apheleia-formatters '(air . ("air" "format" filepath)))
  (add-to-list 'apheleia-mode-alist '(ess-r-mode . air))
  (apheleia-global-mode +1)
  )
;; Code formatting:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-prog-tools.el ends here
