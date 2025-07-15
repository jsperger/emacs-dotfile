;;; config/setup-builtin-packages.el --- Builtin packages -*- lexical-binding: t -*-

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setopt global-auto-revert-non-file-buffers t
          auto-revert-verbose nil
  )
)

;; (use-package dabbrev
;;  :ensure nil
;;  :defer t
;;  :config
;;  (setopt dabbrev-abbrev-char-regexp "[A-Za-z-_]"
;;       dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\'")))

(use-package dired
  :ensure nil
  :defer t
  :config
  (setopt dired-auto-revert-buffer t
          dired-kill-when-opening-new-dired-buffer  t
          dired-create-destination-dirs 'always
          dired-do-revert-buffer t
          dired-dwim-target t
          dired-vc-rename-file t
          )
  )


;;(use-package repeat
;;  :ensure nil
;;  :defer t
;;  )

;;(use-package sqlite
;;  :ensure nil)

(use-package glasses
	:ensure nil
	:hook (ess-r-mode . glasses-mode)
	:config
	(setopt glasses-separate-parentheses-p nil)
)
