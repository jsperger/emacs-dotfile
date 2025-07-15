;;; config/setup-media.el --- Media players configuration -*- lexical-binding: t -*-

(use-package ready-player
	:general
	(tyrant-def "ar" 'ready-player-view-player)
	:config
	(ready-player-mode +1)
	)

;;; setup-media.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
