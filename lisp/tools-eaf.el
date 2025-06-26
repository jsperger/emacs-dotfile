;;; tools-eaf.el --- external app framework -*- lexical-binding: t; -*-
;;; Commentary:
;; For Emacs application framework packages
;; https://github.com/emacs-eaf/emacs-application-framework
;; A framework for extending Emacs with Python and Qt6.

;;; Code:

(use-package eaf
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser)
  :config
  (defalias 'browse-web #'eaf-open-browser)
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding);; unbind, see more in the Wiki

  ;; (use-package eaf-image-viewer
  ;;   :ensure nil)


  ) 

(provide 'tools-eaf)
;;; tools-eaf.el ends here
