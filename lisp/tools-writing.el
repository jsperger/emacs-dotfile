;;; tools-writing.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package pandoc-mode
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))


(use-package writegood-mode)

(provide 'tools-writing)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;;; tools-writing.el ends here
