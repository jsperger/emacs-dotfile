;;; config/setup-font-locking.el --- Font locking configuration -*- lexical-binding: t -*-

(use-package hl-todo
  :custom (global-hl-todo-mode 't))

(use-package morlock
  :hook emacs-lisp-mode)

(use-package highlight-parentheses
  :hook ((org-mode prog-mode) . highlight-parentheses-mode)
  
  :config
  (defun my-hl-paren-get-theme-colors ()
    "Return a list of colors for highlight-parentheses from the current theme."
    (list
     (face-attribute 'font-lock-keyword-face :foreground nil 'default)
     (face-attribute 'font-lock-function-name-face :foreground nil 'default)
     (face-attribute 'font-lock-string-face :foreground nil 'default)
     (face-attribute 'font-lock-comment-face :foreground nil 'default)))

 ;; (setq highlight-parentheses-colors (function my-hl-paren-get-theme-colors))

;;  (setq highlight-parentheses-attributes '(weight ultra-bold))

  (setopt highlight-parentheses-delay 0.05)
  
  (set-face-attribute 'highlight-parentheses-highlight nil :weight 'ultra-bold)
  )

(use-package rainbow-delimiters
	:disabled
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package paren-face
	;; https://github.com/tarsius/paren-face
	;; Parentheses dimming
	;;	:hook (elisp-mode . paren-face-mode)
  
	:config
  (add-to-list 'paren-face-modes '(r-mode R-mode ess-r-mode))
  (global-paren-face-mode t)
	)

;;;
;; --- Font Locking Utilities ---
;;;

(use-package font-lock-studio
  ;; really useful when you need it, but doesn't need to be enabled most of the time
  :disabled)

(use-package highlight-refontification
  ;; really useful when you need it, but doesn't need to be enabled most of the time
  :disabled)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-font-locking.el ends here
