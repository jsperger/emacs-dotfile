;;; tools-notes.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package denote)

(use-package obsidian
  :disabled
  :config
  (obsidian-specify-path "~/obsidian")
  :custom
;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  (global-obsidian-mode t)
  )



(provide 'tools-notes)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-notes.el ends here
