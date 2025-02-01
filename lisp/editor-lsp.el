;;; editor-lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :commands expand-absolute-name
  :init
  (setq read-process-output-max (* 1024 1024))

  ;; (use-package yasnippet
  ;;   :init
  ;;   (setq yas-minor-mode-map nil))
  ;; :config
  ;; (setq eglot-stay-out-of '(company)
  ;;       eglot-connect-timeout 10
  ;;       eglot-ignored-server-capabilities nil)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  (general-def eglot--managed-mode
    :states '(normal insert motion emacs)
    :keymaps 'override
    :prefix-map 'tyrant-eglot-map
    :definer 'minor-mode
    :prefix "SPC"
    :non-normal-prefix "S-SPC"
    "ce"  (cons "eglot" (make-sparse-keymap))
    "cea" 'eglot-code-actions
    "ceb" 'eglot-events-buffer
    "cer" 'eglot-rename
    "ceR" 'eglot-reconnect
    "cex" 'eglot-shutdown
    "ceX" 'eglot-shutdown-all
    "ce=" 'eglot-format)
  :general
  (tyrant-def "cE" 'eglot))

(use-package treesit
  :ensure nil)

(use-package tree-sitter-langs)

(use-package consult-eglot
  :after consult)

(use-package dape
;;  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setq dape-key-prefix "\C-x\C-a")

;;  :hook
  ;; Save breakpoints on quit
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)

  :config
  ;; Turn on global bindings for setting breakpoints with mouse
  ;; (dape-breakpoint-global-mode)

  ;; Info buffers to the right
  (setopt dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setq dape-buffer-window-arrangement 'gud)
  ;; (setq dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  ;; (setq dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setq dape-cwd-function 'projectile-project-root)
)

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :ensure nil
  :hook (dape-mode . repeat-mode)
)


(provide 'editor-lsp)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-lsp.el ends here
