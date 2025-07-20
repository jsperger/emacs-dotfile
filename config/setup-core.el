;;; config/setup-core.el --- Core configuration -*- lexical-binding: t -*-

(use-package ultra-scroll
  :ensure (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init  (setq scroll-conservatively 50
               scroll-margin 0) 
  :config (ultra-scroll-mode 1)
  )

;; autosave each change
(setopt bookmark-save-flag 1)

;; keep focus while navigating help buffers
(setopt help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setopt use-short-answers t)

;; don't load outdated compiled files.
(setopt load-prefer-newer t)

;; don't save duplicates in kill-ring
(setopt kill-do-not-save-duplicates t)

;; break lines after more characters
(setopt word-wrap-by-category t)

(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-core.el ends here
