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

;; -------------------------------------
;; Helper function to setup virtual environment
;; -------------------------------------


;; OPTION 2 (dynamic)
;; This is a better option if the `pyenv' executable is discoverable on `exec-path':
(setq lsp-bridge-python-command (string-trim
                                 (shell-command-to-string "pyenv which python3")))


(defun setup-lsp-bridge-env (venv-path)
  "Set up a Python virtual environment at VENV-PATH with required packages."
  (let* ((env-path venv-path))
    ;; Ensure target directory exists
    (unless (file-exists-p (file-name-directory env-path))
      (make-directory (file-name-directory env-path) t))

    ;; Check if the virtual environment already exists, create it if not
    (unless (file-directory-p env-path)
      (message "Creating virtual environment at %s" env-path)
      (shell-command (format "%s -m venv %s"
                             (executable-find "python3")
                             (shell-quote-argument env-path))))

    ;; Activate the virtual environment and install/update packages
    (let ((activate-script (expand-file-name "bin/activate_this.py" env-path)))
      (when (file-exists-p activate-script)
        ;; Use activate_this.py to activate venv in Emacs
        (load activate-script t t)

        ;; Install or update the required Python packages
        (message "Installing/updating required Python packages...")
        (dolist (package '("epc" "orjson" "sexpdata" "six" "setuptools" "paramiko"
                           "rapidfuzz" "watchdog" "packaging"))
          (shell-command (format "%s install --upgrade %s"
                                 (executable-find "pip3")
                                 package)))

        ;; Deactivate the virtual environment
        (deactivate-virtualenv)))))

;; -------------------------------------
;; Helper function to setup virtual environment
;; -------------------------------------

;; When using lsp-bridge, please first disable other completion plugins, such as lsp-mode, eglot, company, corfu, etc. lsp-bridge provides a complete solution from the completion backend, completion frontend to multi-backend integration
(use-package lsp-bridge
	:ensure (lsp-bridge
					 :type git :host github :repo "manateelazycat/lsp-bridge"
					 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
					 :build (:not elpaca--byte-compile)
					 )
	:init
	(setq lsp-bridge-python-command lsp-bridge-python-path)

	:hook (elpaca-after-init . global-lsp-bridge-mode)
	;;	(lsp-bridge-mode . markdown-mode) ; this can't be right. Need to figure out why lsp-bridge requires markdown-mode
	:config
(setopt acm-candidate-match-function 'orderless-flex)
	:custom (globla-lsp-bridge-mode t)
)

(provide 'tools-lsp-bridge)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-lsp-bridge.el ends here
