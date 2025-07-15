;;; my-keybind-funs.el --- fun with keybinds -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my-view-leader-keys ()
  "Display all keybindings defined in the `tyrant-map`.
This provides a consolidated view of all global leader keys."
  (interactive)
  (describe-keymap 'tyrant-map))

(provide 'my-keybind-funs)
;;; my-keybind-funs.el ends here
