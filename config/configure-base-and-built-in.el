;;; configure-base-and-built-in.el --- Built-in packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; [[file:../its-lit.org::*Configuring built-in packages][Configuring built-in packages:1]]
(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setopt global-auto-revert-non-file-buffers t
          auto-revert-verbose nil
  )
)

(use-package dired
  :ensure nil
  :defer t
  :config
  (setopt dired-auto-revert-buffer t
          dired-kill-when-opening-new-dired-buffer  t
          dired-create-destination-dirs 'always
          dired-do-revert-buffer t
          dired-dwim-target t
          dired-vc-rename-file t
          )
  )

(use-package glasses
	:ensure nil
	:hook (ess-r-mode . glasses-mode)
	:config
	(setopt glasses-separate-parentheses-p nil)
)
;; Configuring built-in packages:1 ends here

;; [[file:../its-lit.org::#which-key-config][=which-key= configuration:1]]
(use-package which-key
  :ensure nil
  :hook (elpaca-after-init)
  :config
  (setopt which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t)
  (push '((nil . "tab-bar-select-tab") . t) which-key-replacement-alist))
;; =which-key= configuration:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-builtin-packages.el ends here
