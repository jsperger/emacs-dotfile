;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:


(use-package vertico
  :preface (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (if (string-match "\\*\\(.\\)" crm-separator)
                      (match-string 1 crm-separator)
                    "")
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :ensure (:files (:defaults "extensions/*.el"))
  :config
  (setq vertico-cycle t)

  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)


  (use-package vertico-buffer
    :ensure nil
    :after vertico
    :no-require
    :hook (vertico-mode . vertico-buffer-mode)
    :config
    (setq vertico-buffer-display-action `(display-buffer-in-side-window
                                          (window-height . ,(+ 3 vertico-count))
                                          (side . top))))

  (use-package vertico-directory
    :ensure nil
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :general
    (vertico-map "RET"   'vertico-directory-enter
                 "DEL"   'vertico-directory-delete-char
                 "M-DEL" 'vertico-directory-delete-word))

  (use-package vertico-quick
    :ensure nil
    :general
    (vertico-map "C-<return>" 'vertico-quick-exit))

  (use-package vertico-multiform
    :ensure nil
    :hook (vertico-mode . vertico-multiform-mode)
		:config
		(add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))
  )


(use-package corfu
	:disabled
  :ensure (:files (:defaults "extensions/*.el"))
	:hook (text-mode . corfu-mode)
  :init
  (setopt completion-cycle-threshold 3
        tab-always-indent 'complete
        tab-first-completion 'eol
        corfu-auto t
        corfu-auto-prefix 1
        corfu-bar-width 0.5
        corfu-cycle t
        corfu-on-exact-match nil
        corfu-preselect 'prompt)

  :config
   ;; Emacs 30 and newer: Disable Ispell completion function.
   ;; Try `cape-dict' as an alternative.
  (setopt text-mode-ispell-word-completion nil)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (with-eval-after-load 'evil-collection
    (advice-add 'evil-collection-corfu-setup :after
                (defun resert-corfu-esc ()
                  (general-def 'insert corfu-map "<escape>" 'nil))))

  (use-package corfu-history
    :ensure nil
    :hook (corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :ensure nil
    :hook (corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 0.95))

  :general
  (corfu-map
   "RET"    nil
   "M-RET"  'corfu-quick-insert
   "S-SPC"  'corfu-insert-separator)
  )

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'editor-completion)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-completion.el ends here
