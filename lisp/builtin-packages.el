;;; builtin-packages.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;; Configure built-in packages that should not track updates on package servers.
;; All packages here should have ':ensure nil' set so elpaca doesn't manage them
;; Positive examples: dired 
;; Negative examples: org and jsonrpc do not belong here

;;; Code:

(use-package autorevert
  :ensure nil
	:hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setopt global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package dabbrev
  :ensure nil
  :defer t
  :config
  (setopt dabbrev-abbrev-char-regexp "[A-Za-z-_]"
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package dired
  :ensure nil
  :defer t
  :config
  (when IS-MAC ;on mac use external ls from homebrew gnutils
    (setopt ls-lisp-use-insert-directory-program t
          insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
  (setopt dired-listing-switches "-aBhl --group-directories-first")
  (setopt dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer  t
        dired-create-destination-dirs 'always
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-vc-rename-file t))

;; (use-package casual-dired
;;   :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))


(use-package repeat
  :ensure nil
	:defer t
  )

(use-package sqlite
  :ensure nil)

(use-package glasses
	:ensure nil
	:hook (ess-r-mode . glasses-mode)
	:config
	(setopt glasses-separate-parentheses-p nil)
)

(provide 'builtin-packages)
;;; builtin-packages.el ends here
