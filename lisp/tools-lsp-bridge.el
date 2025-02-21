;;; tools-lsp-bridge.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;; commentary

;;; Code:

;; -------------------------------------
;; Path to virtual environment for lsp-bridge to use
;; -------------------------------------

;; assumes the no-littering package is being used
(defvar lsp-bridge-venv-path
	(no-littering-expand-var-file-name "lsp-bridge-env")
  "Path to the virtual environment for lsp-bridge.")

(defvar lsp-bridge-python-path
	(expand-file-name "bin/python" lsp-bridge-venv-path)
  "Path to the python executable in the virtual environment for lsp-bridge.")


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
	(setq lsp-bridge-python-command lsp-bridge-python-path)

	:hook (elpaca-after-init . global-lsp-bridge-mode)
;;	(lsp-bridge-mode . markdown-mode) ; this can't be right. Need to figure out why lsp-bridge requires markdown-mode
	:custom (globla-lsp-bridge-mode t)
)

(provide 'tools-lsp-bridge)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-lsp-bridge.el ends here
