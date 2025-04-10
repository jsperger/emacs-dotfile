;;; tools-media.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package ready-player
	:general
	(tyrant-def "ar" 'ready-player-view-player)
	:config
	(ready-player-mode +1)
	)

(provide 'tools-media)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-media.el ends here
