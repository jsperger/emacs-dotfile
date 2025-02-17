;;; editor-lsp.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Built-in packages

(use-package treesit
  :ensure nil
	:defer t
	:config
	(setq treesit-language-source-alist
				'(
					(r "https://github.com/r-lib/tree-sitter-r" "next")
					(bash "https://github.com/tree-sitter/tree-sitter-bash")
					(cmake "https://github.com/uyha/tree-sitter-cmake")
					(css "https://github.com/tree-sitter/tree-sitter-css")
					(elisp "https://github.com/Wilfred/tree-sitter-elisp")
					(go "https://github.com/tree-sitter/tree-sitter-go")
					(html "https://github.com/tree-sitter/tree-sitter-html")
					(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
					(json "https://github.com/tree-sitter/tree-sitter-json")
					(make "https://github.com/alemuller/tree-sitter-make")
					(markdown "https://github.com/ikatyang/tree-sitter-markdown")
					(python "https://github.com/tree-sitter/tree-sitter-python")
					(toml "https://github.com/tree-sitter/tree-sitter-toml")
					(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
					(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
					(yaml "https://github.com/ikatyang/tree-sitter-yaml")))
	)

(use-package repeat
  :ensure nil
	:defer t
  :hook (dape-mode . repeat-mode)
  ;; Enable repeat mode for more ergonomic `dape' use
  )

;;; Additional Packages

(use-package eglot
;  :commands expand-absolute-name
  :init
  (setq read-process-output-max (* 1024 1024))

  ;; (defun expand-absolute-name (name)
  ;;   (if (file-name-absolute-p name)
  ;;       (tramp-file-local-name
  ;;        (expand-file-name
  ;;         (concat (file-remote-p default-directory) name)))
  ;;     name))

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


(use-package consult-eglot
  :after eglot consult)

(use-package consult-eglot-embark
	:after consult eglot embark
  :hook ('eglot-ensure . consult-eglot-embark-mode))

(use-package dape
	:disabled
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


(provide 'editor-lsp)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-lsp.el ends here
