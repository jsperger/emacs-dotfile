;;; my-keybinding-helpers.el --- Keybinding helper functions -*- lexical-binding: t -*-

(defun my-view-leader-keys ()
  "Display all keybindings defined in the `tyrant-map`.
This provides a consolidated view of all global leader keys."
  (interactive)
  (describe-keymap 'tyrant-map)
  )

(provide 'my-keybinding-helpers)
;;; my-keybinding-helpers.el ends here
