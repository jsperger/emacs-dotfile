;;; core-packages.el --- -*- lexical-binding: t; -*-
;;; Commentary: Packages
;; Note to future self: :defer t is needed to defer loading when elpaca nil is
;; specified.

;;;
;;; Code:


(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x pgtk))
  :defer t
  :defines exec-path-from-shell-arguments
  exec-path-from-shell-variables
  exec-path-from-shell-initialize
  :init
  (setq
        exec-path-from-shell-variables '(
                                         "PATH" "MANPATH" "GNUPGHOME" "SSH_AUTH_SOCK"
                                         "SSH_AGENT_PID" "GPG_AGENT_INFO"
                                         "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"
                                         "XDG_CACHE_HOME" "XDG_DATA_HOME"
                                         "XDG_CONFG_HOME" "XDG_STATE_HOME"))
  (exec-path-from-shell-initialize))

(use-package no-littering)

;; A few more useful configurations...
(use-package emacs
  :ensure nil
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
  (setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setopt read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Emacs 30:
  (setopt text-mode-ispell-word-completion nil)

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t)
  )

(use-package unfill)

(if (eq system-type 'gnu/linux)
    (use-package eat)
  )

(use-package jinx
  :after evil
  :hook (text-mode . jinx-mode)
  :config
  ;; add-to-list notably only accepts one item at a time. It also checks if the
  ;; item is already there, and prepends rather than appends items by default.
  ;; Regular expressions to exclude from jinx results (t for always, otherwise
  ;; by mode)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (add-to-list 'jinx-exclude-regexps '(LaTeX-mode "\\s*\\\\input{[^}]+}\\s*"))

  ;; Modes where camelCase or PascalCase words should be accepted.
  (add-to-list 'jinx-camel-modes 'R-mode)
  (add-to-list 'jinx-camel-modes 'ess-r-mode)

  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))
  (with-eval-after-load 'evil
    (evil-define-motion evil-prev-jinx-error (count)
      "Go to the COUNT'th spelling mistake preceding point."
      :jump t (jinx-previous (or count 1)))
    (evil-define-motion evil-next-jinx-error (count)
      "Go to the COUNT'th spelling mistake after point."
      :jump t (jinx-next (or count 1))))
  :general
  ([remap ispell-word] 'jinx-correct-word
   [remap evil-prev-flyspell-error] 'evil-prev-jinx-error
   [remap evil-next-flyspell-error] 'evil-next-jinx-error))

(use-package flymake-proselint
  :disabled)

(provide 'core-packages)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; core-packages.el ends here
