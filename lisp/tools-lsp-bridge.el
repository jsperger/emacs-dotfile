;;; tools-lsp-bridge.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;; commentary

;;; Code:




;; (add-to-list 'load-path "/Users/jsperger/code/lsp-bridge")
;; (use-package lsp-bridge
;;	:ensure nil
;; :after markdown-mode	
;; :config
;;	(setopt global-lsp-bridge-mode t))

;; (require 'lsp-bridge)
(use-package lsp-bridge
	:after yasnippet markdown-mode
	:ensure (lsp-bridge
					 :type git :host github :repo "manateelazycat/lsp-bridge"
					 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
					 :build (:not elpaca--byte-compile)
					 					 :post-build (eshell-command "pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging -t /Users/jsperger/.emacs.d/elpaca/builds/lsp-bridge"))
	:init
(setq lsp-bridge-python-command "/Users/jsperger/.emacs.d/var/lsp-bridge-env/bin/python")
	:config
(setopt global-lsp-bridge-mode t)
)


(provide 'tools-lsp-bridge)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-lsp-bridge.el ends here
