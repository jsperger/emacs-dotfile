;;; tools-media.el --- media players -*- lexical-binding: t; -*-

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
;;; tools-media.el ends here
