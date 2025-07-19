;;; my-lsp-bridge-helpers.el --- Helper functions for lsp-bridge -*- lexical-binding: t -*-

(defvar lsp-bridge-venv-path
	(no-littering-expand-var-file-name "lsp-bridge-env")
  "Path to the virtual environment for lsp-bridge.")

(defvar lsp-bridge-python-path
	(expand-file-name "bin/python" lsp-bridge-venv-path)
  "Path to the python executable in the virtual environment for lsp-bridge.")

(defun setup-lsp-bridge-env (venv-path)
  "Set up a Python virtual environment at VENV-PATH with required packages."
  (let* ((env-path venv-path))
    (unless (file-exists-p (file-name-directory env-path))
      (make-directory (file-name-directory env-path) t))

    (unless (file-directory-p env-path)
      (message "Creating virtual environment at %s" env-path)
      (shell-command (format "%s -m venv %s"
                             (executable-find "python3")
                             (shell-quote-argument env-path))))

    (let ((activate-script (expand-file-name "bin/activate_this.py" env-path)))
      (when (file-exists-p activate-script)
        (load activate-script t t)

        (message "Installing/updating required Python packages...")
        (dolist (package '("epc" "orjson" "sexpdata" "six" "setuptools" "paramiko"
                           "rapidfuzz" "watchdog" "packaging"))
          (shell-command (format "%s install --upgrade %s"
                                 (executable-find "pip3")
                                 package)))

        (deactivate-virtualenv)))))

(provide 'my-lsp-bridge-helpers)
;;; my-lsp-bridge-helpers.el ends here
