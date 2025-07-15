;;; config/setup-font-locking.el --- Font locking configuration -*- lexical-binding: t -*-

(use-package hl-todo
  :custom (global-hl-todo-mode 't))

(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :config
  ;; (setq highlight-parentheses-colors '("Springgreen3"
  ;;                                      "IndianRed1"
  ;;                                      "IndianRed3"
  ;;                                      "IndianRed4"))
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold))

(use-package rainbow-delimiters
	:disabled
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package paren-face
	;; https://github.com/tarsius/paren-face
	;; Parentheses dimming
	;;	:hook (elisp-mode . paren-face-mode)
	:custom (global-paren-face-mode t)
	)

;;;
;; --- Font Locking Utilities ---
;;;

(use-package font-lock-studio)

(use-package highlight-refontification)

;;; setup-font-locking.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
