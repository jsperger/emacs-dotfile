;;; config/setup-evil.el --- Evil configuration -*- lexical-binding: t -*-

(require 'my-evil-helpers)

(use-package evil
	:ensure (:wait t)
	:demand t
	:hook ((elpaca-after-init . evil-mode)
				 (prog-mode . hs-minor-mode)) ;why is this here?
	:init
	(setq evil-want-keybinding nil
					evil-symbol-word-search t
					evil-ex-search-vim-style-regexp t
					evil-search-module 'evil-search
					evil-magic 'very-magic
					evil-want-C-u-delete t
					evil-want-C-u-scroll t
					hs-minor-mode-map nil)
  :config
  (setopt evil-cross-lines t
        evil-kill-on-visual-paste nil
        evil-move-beyond-eol t
        evil-undo-system 'undo-redo
        evil-want-C-i-jump t
        evil-want-fine-undo t
        evil-v$-excludes-newline t
	 )
  (setopt evil-normal-state-cursor  '("DarkGoldenrod2" box)
        evil-insert-state-cursor  '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor   '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor  '("gray" (hbar . 2))
        evil-motion-state-cursor  '("plum3" box))

  (evil-set-undo-system 'undo-redo)
  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  (eldoc-add-command #'evil-normal-state
                     #'evil-insert
                     #'evil-change
                     #'evil-delete
                     #'evil-replace)

  (add-hook 'evil-normal-state-exit-hook #'evil-ex-nohighlight)

	(general-def '(normal motion) "TAB" 'bicycle-cycle)
  (general-def 'normal "zf" 'reposition-window)
  (general-def 'insert [remap evil-complete-previous] 'hippie-expand)
	)

;;; setup-evil.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
