;;; config/setup-completion-display.el --- Completion display -*- lexical-binding: t -*-

(use-package marginalia
  :custom
  (marginalia-mode t))

(use-package epkg-marginalia
  :after marginalia
  :config
    (setcar (alist-get 'package marginalia-annotators)
            #'epkg-marginalia-annotate-package)
  )

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-completion-display.el ends here
