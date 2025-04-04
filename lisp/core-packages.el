;;; core-packages.el --- -*- lexical-binding: t; -*-
;;; Commentary: Packages
;; Note to future self: :defer t is needed to defer loading when elpaca nil is
;; specified.

;;;
;;; Code:


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

  ;; Enable recursive minibuffers
  (setopt enable-recursive-minibuffers t)

	(set-language-environment 'utf-8)
	(set-default-coding-systems 'utf-8)


	:config
	(setopt user-full-name "John Sperger"
					user-mail-address "josp@duck.com"
					initial-scratch-message nil   ; "make scratch buffer empty"
					inhibit-startup-message t   ; "disable splash screen"
					tab-width 2 ; tab-width default 2 instead of 4
					fill-column 80 ; fill-column default 80 chars
					global-hl-line-mode t ; highlight current line
					ring-bell-function 'ignore ;no beep
					undo-limit 67108864 ; increases undo limit 64mb.
					undo-strong-limit 100663296 ; 96mb.
					undo-outer-limit 1006632960 ; 960mb.
					sentence-end-double-space nil ;single space between sentences
					)

	;;----------------------------------------
	;; OS-specific configuration
  ;;----------------------------------------

	(when IS-WINDOWS (print "How did I get here?"))

	(when IS-MAC
		(setopt ns-pop-up-frames nil
						frame-resize-pixelwise t))
  )

(use-package exec-path-from-shell
	:defines	exec-path-from-shell-arguments
	exec-path-from-shell-variables
	exec-path-from-shell-initialize
	:init
  (setq
   exec-path-from-shell-variables
	 '("PATH"
		 "MANPATH"
		 "GNUPGHOME"
		 "GPG_AGENT_INFO"
		 "HOMEBREW_CELLAR"
		 "HOMEBREW_PREFIX"
		 "HOMEBREW_REPOSITORY"
		 "INFOPATH"
		 "LANG"
		 "LC_CTYPE"
		 "NIX_SSL_CERT_FILE"
		 "NIX_PATH"
		 "SSH_AUTH_SOCK"
		 "SSH_AGENT_PID"
		 "XDG_CACHE_HOME"
		 "XDG_DATA_HOME"
		 "XDG_CONFG_HOME"
		 "XDG_STATE_HOME"))
	:config
	(add-hook 'elpaca-after-init-hook 'exec-path-from-shell-initialize))

(use-package dirvish
	:disabled
	:hook (dired-mode . dirvish-override-dired-mode))

(use-package benchmark-init
	:config
	(add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

(use-package daemons
	:disabled)

(use-package eat
	:disabled)

(use-package surround
	:general
  (tyrant-def
		"Si" 'surround-insert
		"Sd" 'surround-kill
		"Sr" 'surround-replace)
	)

(use-package banner-comment
	:config
	(setopt banner-comment-end "  "
					banner-comment-start ";; "
					banner-comment-width 72)
	:general
	(tyrant-def
		"ab" 'banner-comment))

(provide 'core-packages)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; core-packages.el ends here
