;;; config/setup-editor-misc.el --- Miscellaneous editor configuration -*- lexical-binding: t -*-

(use-package reveal-in-folder
  :general (tyrant-def "bf" 'reveal-in-folder))

;; Conveniently create scratches in the same mode as the current file
(use-package scratch
  :general (tyrant-def "bS" 'scratch))

(use-package terminal-here
  :init (setq terminal-here-mac-terminal-command 'ghostty)
  :general
  (tyrant-def "'" '("terminal here" . terminal-here-launch)
    "p '" '("terminal project root" . terminal-here-project-launch)
    )
  )

(use-package vterm
  :general (tyrant-def "av" 'vterm
             "aV" 'vterm-other-window
             )
  )

(use-package undo-fu
  :config (setopt evil-undo-system 'undo-fu)
  )

(use-package undo-fu-session
  :custom (undo-fu-session-global-mode t)
  )

(use-package winum
  :hook (elpaca-after-init . winum-mode)
  :init
  (with-eval-after-load 'which-key
    (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
    (push '((nil . "buffer-to-window-[1-9]") . t) which-key-replacement-alist))
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local)

  (dotimes (i 9)
    (let ((n (+ i 1)))
      (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
               ,(format "Move buffer to the window with number %i." n)
               (interactive "P")
               (if arg
                   (move-buffer-to-window ,n t)
                 (swap-buffers-to-window ,n t))))))
  :general
  (tyrant-def
    "1"  '("window 1..9" . winum-select-window-1)
    "2"  'winum-select-window-2
    "3"  'winum-select-window-3
    "4"  'winum-select-window-4
    "5"  'winum-select-window-5
    "6"  'winum-select-window-6
    "7"  'winum-select-window-7
    "8"  'winum-select-window-8
    "9"  'winum-select-window-9
    "b1" '("Move buffer to window 1..9" . buffer-to-window-1)
    "b2" 'buffer-to-window-2
    "b3" 'buffer-to-window-3
    "b4" 'buffer-to-window-4
    "b5" 'buffer-to-window-5
    "b6" 'buffer-to-window-6
    "b7" 'buffer-to-window-7
    "b8" 'buffer-to-window-8
    "b9" 'buffer-to-window-9))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-editor-misc.el ends here
