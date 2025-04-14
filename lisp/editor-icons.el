;;; editor-icons.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
	:config
	(setopt
	 nerd-icons-ibuffer-icon t ; Whether display the icons.
	 nerd-icons-ibuffer-color-icon t ; Whether display the colorful icons.
	 ;; It respects `nerd-icons-color-icons'.
	 nerd-icons-ibuffer-icon-size 1.0
	 ;; The default icon size in ibuffer.
	 nerd-icons-ibuffer-human-readable-size t ; Use human readable file size in ibuffer.
	 ;; A list of ways to display buffer lines with `nerd-icons'.
	 ;; See `ibuffer-formats' for details.
	 ;; nerd-icons-ibuffer-formats

	 ;; Slow Rendering
	 ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
	 ;; you can try setting the following variable
	 inhibit-compacting-font-caches t)
	)


(use-package tab-line-nerd-icons
	:hook (tab-line-mode . tab-line-nerd-icons-global-mode))

(use-package nerd-icons-corfu
	:disabled
;;	:if (global-corfu-mode) ; not defined if corfu isn't on. Need to figure out how to safely try this or an alternative condition
	:after corfu
	:init
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
;;	:hook (corfu-mode . nerd-icons-completion)
;; ;; Optionally:
;; (setq nerd-icons-corfu-mapping
;;       '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
;;         (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
;;         ;; ...
;;         (t :style "cod" :icon "code" :face font-lock-warning-face)))
;;         ;; Remember to add an entry for `t', the library uses that as default.
;; The Custom interface is also supported for tuning the variable above.
	)

(use-package all-the-icons
	:disabled
  :if (display-graphic-p))

(use-package all-the-icons-dired
	:disabled
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
	:disabled
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package kind-icon
	:disabled
  :after corfu
  :custom
   (kind-icon-blend-background t)
   (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package nerd-icons
	;;	:hook (doom-modeline-mode)
	:custom (nerd-icon-scale-factor 0.8)
)
(provide 'editor-icons)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-icons.el ends here
