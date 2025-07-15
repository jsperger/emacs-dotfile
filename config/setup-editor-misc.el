;;; config/setup-editor-misc.el --- Miscellaneous editor configuration -*- lexical-binding: t -*-

(use-package dumb-jump
  :disabled
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read)
  )

(use-package helpful
  :config
  (setq helpful-max-buffers 3
        helpful-switch-buffer-function #'helpful-reuse-window)

  (with-eval-after-load 'ibuffer
    (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode))
  :general
  ([remap describe-command]  'helpful-command
   [remap describe-function] 'helpful-callable
   [remap describe-key]      'helpful-key
   [remap describe-symbol]   'helpful-symbol
   [remap describe-variable] 'helpful-variable))

(use-package link-hint
  :config
  (setq link-hint-restore nil)
  :general
  (general-def
    :keymaps '(compilation-mode-map
               custom-mode-map
               eww-link-keymap
               eww-mode-map
               help-mode-map
               helpful-mode-map
               Info-mode-map
               mu4e-view-mode-map
               xref--xref-buffer-mode-map
               woman-mode-map)
    :states  'normal
    "o"      'link-hint-open-link)

  (tyrant-def
    "Jo" 'link-hint-open-link
    "JO" 'link-hint-open-multiple-links
    "Jy" 'link-hint-copy-link))

(use-package reveal-in-folder
  :general (tyrant-def "bf" 'reveal-in-folder))

(use-package scratch
  :general (tyrant-def "bS" 'scratch))

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command  (lambda (dir)
                                              (list "open" "-a" "kitty" "--args" "--working-directory" (expand-file-name dir)))
        terminal-here-linux-terminal-command 'alacritty
        terminal-here-project-root-function (lambda () (project-root (project-current t))))
  :general
  (tyrant-def
    "'"   '("terminal here" . terminal-here-launch)
    "p '" '("terminal project root" . terminal-here-project-launch)))

(use-package vterm
	:general (tyrant-def
						 "av" 'vterm
						 "aV" 'vterm-other-window)
	)

(use-package undo-fu
	:config
	(setopt evil-undo-system 'undo-fu))

(use-package undo-fu-session
	:custom
	(undo-fu-session-global-mode t)
	)

(use-package password-menu
	;; was using while debugging authinfo api key stuff
	:disabled
	:general
	(tyrant-def "as" 'password-menu-transient)
	)

(use-package substitute
	:general
	(tyrant-def
		"rb" 'substitute-target-in-buffer
		"rd" 'substitute-target-in-defun
		"rj" 'substitute-target-below-point
		"rk" 'substitute-target-above-point)
	:config
	(add-hook 'substitute-post-replace-functions #'substitute-report-operation)
	)

(use-package tmr
	:general
	(tyrant-def "aT" 'tmr-tabulated-view)
	:config
	(setq tmr-sound-file "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
	(setq tmr-notification-urgency 'normal)
	(setq tmr-descriptions-list 'tmr-description-history)
	(define-key global-map "\C-ct" 'tmr-prefix-map)
	)

(use-package xr)

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

(use-package biome
	:disabled
	:general
	(tyrant-def "aw" 'biome)
	:config
	(setq biome-query-coords
      '(("Carrboro" 35.91014 -79.07529)
        ("Philly" 39.95233 -75.16379))
			)
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-editor-misc.el ends here
