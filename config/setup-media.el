;;; config/setup-media.el --- Media players configuration -*- lexical-binding: t -*-

(use-package ready-player
	:general
	(tyrant-def "ar" 'ready-player-view-player)
	:config
	(ready-player-mode +1)
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-media.el ends here
