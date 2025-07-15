;;; config/setup-keybindings.el --- Keybindings configuration -*- lexical-binding: t -*-

(require 'my-keybindings)

(use-package which-key
	:hook (elpaca-after-init)
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t)
  (push '((nil . "tab-bar-select-tab") . t) which-key-replacement-alist))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-keybindings.el ends here
