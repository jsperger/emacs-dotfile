;;; config/setup-eaf.el --- EAF configuration -*- lexical-binding: t -*-

(use-package eaf
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ) 

;;; setup-eaf.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
