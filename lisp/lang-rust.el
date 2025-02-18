;;; lang-rust.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2023  John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package rust-mode
	:mode ("\\.rs\\'" . rust-ts-mode)
;;  :hook (rust-mode . 'eglot-ensure)
  :config
    ;; (add-to-list 'eglot-server-programs
    ;;          '((rust-ts-mode rust-mode) .
    ;;            ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (despot-def (Rust-mode-map)
      :major-modes '(rust-ts-mode rust-mode)
      "c"            'rust-compile)

)



(provide 'lang-rust)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-rust.el ends here
