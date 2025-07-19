;;; setup-comment-tools.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package banner-comment
  :config  (setopt banner-comment-width 72)
  :general  (tyrant-def "ab" 'banner-comment)
  )


;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-comment-tools.el ends here
