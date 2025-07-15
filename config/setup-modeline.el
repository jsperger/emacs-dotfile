;;; config/setup-modeline.el --- Modeline configuration -*- lexical-binding: t -*-

(use-package doom-modeline
  :hook (elpaca-after-init . doom-modeline-mode)
  :config
  (setopt inhibit-compacting-font-caches t
          doom-modeline-buffer-encoding nil
          doom-modeline-height 0
          doom-modeline-buffer-file-name-style 'auto
          doom-modeline-project-detection 'project
          doom-modeline-icon t
          doom-modeline-major-mode-icon nil
          doom-modeline-modal-icon nil
          doom-modeline-gnus nil
          doom-modeline-irc nil
          doom-modeline-persp-name nil
          )
  )

(use-package moody
  :disabled
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  )

(use-package nano-modeline
  :disabled)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-modeline.el ends here
