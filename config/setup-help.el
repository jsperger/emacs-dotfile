;;; config/setup-help.el --- helpful annotations -*- lexical-binding: t -*-

(use-package helpful
  :config
  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))
  :general
  ([remap describe-command]  'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key]      'helpful-key
   [remap describe-symbol]   'helpful-symbol
   [remap describe-variable] 'helpful-variable))
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-help.el ends here
