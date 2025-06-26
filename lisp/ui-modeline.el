;;; ui-modeline.el --- modeline ui elements -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
;;				doom-modeline-enable-word-count t
;;doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode)
				)
	;;	(when IS-MAC (setopt doom-modeline-battery t))
)

;; Moody https://github.com/tarsius/moody
;; Tabs and ribbons for the mode line
(use-package moody
	:disabled
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package nano-modeline
  :disabled)

(provide 'ui-modeline)
;;; ui-modeline.el ends here
