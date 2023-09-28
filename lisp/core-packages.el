;;; core-packages.el --- -*- lexical-binding: t; -*-
;;; Commentary: Packages
;;;
;;; Code:


(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer t
  :defines exec-path-from-shell-arguments
    exec-path-from-shell-variables
    exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-arguments nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "GNUPGHOME" "SSH_AUTH_SOCK"
                                         "XDG_CACHE_HOME" "XDG_DATA_HOME" "XDG_CONFG_HOME" "XDG_STATE_HOME"))
  (exec-path-from-shell-initialize))

(use-package no-littering
  :defer t)

;; A few more useful configurations...
(use-package emacs
  :elpaca nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
   (setq read-extended-command-predicate
         #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
   (setq enable-recursive-minibuffers t)
)

(use-package pomm
  :commands (pomm pomm-third-time))

(use-package chronometrist
  :elpaca t
  :ensure t
  :init
  (add-hook 'kill-emacs-query-functions 'chronometrist-query-stop))

(use-package ess
  :init (require 'ess-site)
  :mode (("\\.[rR]\\'" . R-mode)
         ("\\.Rnw\\'" . Rnw-mode))
)

(provide 'core-packages)
;;; core-packages.el ends here
