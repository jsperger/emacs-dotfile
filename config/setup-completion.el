;;; config/setup-completion.el --- Completion configuration -*- lexical-binding: t -*-

(require 'my-completion-helpers)

(use-package vertico
  :ensure (:files (:defaults "extensions/*.el"))
  :hook (elpaca-after-init . vertico-mode)
  :config
  (setq vertico-cycle t)

  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator))
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)

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
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
    )
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

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))

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
:disabled
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-completion.el ends here
