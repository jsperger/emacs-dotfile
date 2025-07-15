;;; config/setup-completion-backends.el --- Completion backends -*- lexical-binding: t -*-

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(require 'my-completion-helpers)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :config
  (setopt orderless-style-dispatchers '(flex-if-twiddle without-if-bang))
)

(use-package prescient
	:disabled
	:config
  (setopt prescient-sort-full-matches-first t
					prescient-sort-length-enable nil)
	)

(use-package corfu-prescient
	:disabled
	:after prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq corfu-prescient-enable-filtering t)
	)


(use-package vertico-prescient
	:disabled
	:after prescient
	:hook (vertico-mode . vertico-prescient-mode)
	:init
	(setq vertico-prescient-enable-filtering t)
	)
