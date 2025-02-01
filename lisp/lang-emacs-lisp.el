;;; lang-emacs-lisp.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package elisp-mode
  :ensure nil
  :config
  (despot-def :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    "'"  'ielm
    "c"  (cons "compile" (make-sparse-keymap))
    "cc" 'emacs-lisp-byte-compile
    "e"  (cons "eval" (make-sparse-keymap))
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "er" 'eval-region
    "ef" 'eval-defun
    "t"  (cons "tests" (make-sparse-keymap))
    "tb" 'ert-run-tests-buffer
    "tq" 'ert))

(use-package eval-sexp-fu
  :hook (emacs-lisp-mode . eval-sexp-fu-flash-mode))


(provide 'lang-emacs-lisp)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-emacs-lisp.el ends here
