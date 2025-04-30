;;; core-keybinds.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package which-key
	:hook (elpaca-after-init)
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t)
  (push '((nil . "tab-bar-select-tab") . t) which-key-replacement-alist))

(use-package general
	:ensure (:wait t)
	:demand t
  :config
  (setq general-emit-autoloads nil)

  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (despot-def "" nil)

  (general-def universal-argument-map
    "SPC u" 'universal-argument-more)

  (tyrant-def
    "SPC"     '("M-x" . execute-extended-command)
    "TAB"     '("last buffer" . alternate-buffer)
		"RET"     '("Switch" . consult-buffer)
    "!"       '("shell cmd" . shell-command)
    "i"       '("insert" . tempel-insert)
		"I"       '("insert" . yas-insert-snippet)
		"M" '("bookmark" . bookmark-set)

		","       (cons "config" (make-sparse-keymap))
		",d"      'describe-face
    ",f"      'fontaine-set-preset
		",F"      'menu-set-font
		",t"      'consult-theme

    ;;================================= applications ===========================
    "a"       (cons "apps" (make-sparse-keymap))
		;;		"ab" 'banner-comment ; defined in package dec
		"ac"      'consult-minor-mode-menu
    "aC"      'calc-dispatch
		"ae"      'embark-act
		"aE"      'embark-act-all
    "ap"      'list-processes
		;;    "ad"      'todoist
    "af"      'fontaine-set-preset
		"aF"      'menu-set-font
		;;		"am"     'manage-minor-mode-table ;in manage-minor-mode-table use-package def
     "ao"      (cons "obsidian" (make-sparse-keymap))
     "aoc"     'obsidian-capture
     "aoj"     'obsidian-jump
     "aos"     'obsidian-search
		"at"      'consult-theme
    "aP"      'proced

		;;================================ buffers =================================
    "b"       (cons "buffers" (make-sparse-keymap))
    "bb"      'switch-to-buffer
    "bB"      'ibuffer
    "bd"      'kill-current-buffer
    "bm"      'switch-to-messages-buffer
    "bs"      'switch-to-scratch-buffer
    "bu"      'reopen-killed-buffer
    "bx"      'kill-buffer-and-window

		;;=================================== code =================================
    "c"       (cons "code" (make-sparse-keymap))
    "cb"      'flymake-show-buffer-diagnostics
    "cc"      'compile
    "cj"      'previous-error
    "ck"      'next-error
		"cn"      'next-error
    "cp"      'previous-error
		"cP"      'check-parens
    "cr"      'recompile
    "cx"      'kill-compilation
    "c="      'indent-region-or-buffer

		;;================================== elpaca ================================
    "e"       (cons "elpaca" (make-sparse-keymap))
    "ef"      'elpaca-fetch-all
    "eF"      'elpaca-fetch
    "el"      'elpaca-log
    "em"      'elpaca-manager
    "eu"      'elpaca-merge ;; update equivalent
    ;; Intentional no bind for merge-all

		;;================================== Ellama ================================
    ;; "E"       (cons "Ellama" (make-sparse-keymap))
    ;; "Ec"      (cons "code" (make-sparse-keymap))
    ;; "Ecc"     'ellama-code-complete
    ;; "Eca"     'ellama-code-add
    ;; "Ece"     'ellama-code-edit
    ;; "Eci"     'ellama-code-improve
    ;; "Ecr"     'ellama-code-review
    ;; "Es"      (cons "summarize & session" (make-sparse-keymap))
    ;; "Ess"    'ellama-summarize
    ;; "Esw"    'ellama-summarize-webpage
    ;; "Esl"    'ellama-load-session
    ;; "Esr"    'ellama-session-rename
    ;; "Esd"    'ellama-session-remove
    ;; "Esa"    'ellama-session-switch
    ;; "Ei"      (cons "improve" (make-sparse-keymap))
    ;; "Eiw"    'ellama-improve-wording
    ;; "Eig"    'ellama-improve-grammar
    ;; "Eic"    'ellama-improve-conciseness
    ;; "Em"      (cons "make" (make-sparse-keymap))
    ;; "Eml"    'ellama-make-list
    ;; "Emt"    'ellama-make-table
    ;; "Emf"    'ellama-make-format
    ;; "Ea"      (cons "ask & chat" (make-sparse-keymap))
    ;; "Eaa"    'ellama-ask-about
    ;; "Eai"    'ellama-chat
    ;; "Eal"    'ellama-ask-line
    ;; "Eas"    'ellama-ask-selection
    ;; "Et"      (cons "translate" (make-sparse-keymap))
    ;; "Ett"    'ellama-translate
    ;; "Etb"    'ellama-translate-buffer
    ;; "Ete"    'ellama-chat-translation-enable
    ;; "Etd"    'ellama-chat-translation-disable
    ;; "Etc"    'ellama-complete
    ;; "Ed"      (cons "define" (make-sparse-keymap))
    ;; "Edw"    'ellama-define-word
    ;; "Ex"      (cons "context" (make-sparse-keymap))
    ;; "Exb"    'ellama-context-add-buffer
    ;; "Exf"    'ellama-context-add-file
    ;; "Exs"    'ellama-context-add-selection
    ;; "Exi"    'ellama-context-add-info-node
    ;; "Ep"      (cons "provider" (make-sparse-keymap))
    ;; "Eps"    'ellama-provider-select

		;;================================== files =================================
    "f"       (cons "files" (make-sparse-keymap))
    "fb"      'rename-current-buffer-file
    "fC"      '("copy-file" . write-file)
    "fD"      'delete-current-buffer-file
    "fe"      'find-library
																				;    "fE"      'sudo-edit
    "ff"      'find-file
    "fj"      'dired-jump
    "fJ"      'dired-jump-other-window
    "fo"      'open-file-or-directory-in-external-app
		"fr"      'rg
    "fR"      'rg-menu
		"fs"      'save-buffer
    "fv"      (cons "variables" (make-sparse-keymap))
    "fvd"     'add-dir-local-variable
    "fvf"     'add-file-local-variable
    "fvp"     'add-file-local-variable-prop-line

		;;================================== Frame =================================
    "F"       (cons "Frame" (make-sparse-keymap))
    "Fd"      'delete-frame
    "FD"      'delete-other-frames
    "Fn"      'make-frame
    "Fo"      'other-frame

		;;=================================== help =================================
    "h"       (cons "help" (make-sparse-keymap))
    "ha"      'apropos
    "hb"      'describe-bindings
    "hc"      'describe-char
    "hf"      'describe-function
    "hF"      'describe-face
    "hi"      'info-emacs-manual
    "hI"      'info-display-manual
    "hk"      'describe-key
    "hK"      'describe-keymap
    "hm"      'describe-mode
    "hM"      'man
    "hp"      'describe-package
    "ht"      'describe-text-properties
    "hv"      'describe-variable
    "hP"      (cons "profiler" (make-sparse-keymap))
    "hPs"     'profiler-start
    "hPk"     'profiler-stop
    "hPr"     'profiler-report

		;;=================================== jump ==================================
    "j"       'consult-buffer
		
    "J"       (cons "jump" (make-sparse-keymap))
    "Jb"      'bookmark-jump
    "Ji"      'imenu
		;;    "jg"      'avy-goto-char-timer
    "Jn"      'denote
		;;    "jo"      'obsidian-jump

		;;=================================== keys ==================================
    "k"       (cons "key" (make-sparse-keymap))
    "km"      'which-key-show-major-mode
    "kt"      'which-key-show-top-level
		"kM"      'which-key-show-minor-mode-keymap
		"ka"      'which-key-show-keymap ; show [a]ny or [a]ll keymap
    "kd"      'describe-key
    "kD"      'describe-keymap

		;;================================== eglot =================================
    ;; "l"  (cons "eglot" (make-sparse-keymap))
    ;; "la" 'eglot-code-actions
    ;; "lb" 'eglot-events-buffer
    ;; "lr" 'eglot-rename
    ;; "lR" 'eglot-reconnect
    ;; "lx" 'eglot-shutdown
    ;; "lX" 'eglot-shutdown-all
    ;; "l=" 'eglot-format

		;;============================ major mode prefix ===========================
    "m"       (cons "major mode" (make-sparse-keymap))

		;;================================= projects ===============================
    "p"       (cons "projects" project-prefix-map)
    "pt"      'project-open-in-tab
    "pb"      'bookmark-in-project-toggle
    "pj"      'bookmark-in-project-jump

		;;=================================== quit =================================
    "q"       (cons "quit" (make-sparse-keymap))
    "qd"      'restart-emacs-debug-init
    "qr"      'restart-emacs
    "qR"      'restart-emacs-without-desktop
    "qf"      'delete-frame
		"qq"      'delete-frame
    "qQ"      'save-buffers-kill-emacs
    "qs"      'server-shutdown
		"qS"      'server-save-buffers-kill-terminal
    "qt"      'save-buffers-kill-terminal

		;;================================= spelling ===============================
    "s"       (cons "spelling" (make-sparse-keymap))
    "sb"      'flyspell-buffer
    "sn"      'flyspell-goto-next-error
    "sr"      'flyspell-region
    "sc"      'jinx-correct
    "sC"      'jinx-correct-nearest
    "sj"      'jinx-next

		;;================================= replace ================================
    "r"       (cons "replace" (make-sparse-keymap))
    "ra"      'query-replace ; Ask = query
    "rs"      'replace-string
    "rr"      'replace-string-in-region

		;;=================================== toggle ===============================
    "t"       (cons "toggle" (make-sparse-keymap))
    "ta"      'auto-fill-mode
		;;    "tb"      'global-obsidian-mode
		;;		"tc"      'nocomments-mode defined-in-package
    "tM"      'consult-minor-mode-menu
    "td"      'toggle-debug-on-error
    "tf"      'display-fill-column-indicator-mode
    "tg"      'golden-ratio-mode
    "th"      'hs-minor-mode
    "tj"      'jinx-mode
    "tl"      'toggle-truncate-lines
    "tm"      'flymake-mode
    "tn"      'display-line-numbers-mode
    "tz"      'outline-minor-mode
		"tO"      'org-modern-mode
		"tp" 'variable-pitch-mode
		;;    "tp"      'pdf-view-mode ;probably don't need it since I fixed the :mode def
		;;    "tr"      'writegood-mode
    "ts"      'flyspell-mode
    "tt"      'LateX-mode
    "tv"      'visual-fill-column-mode
		"tV"      'olivetti-mode
    "tw"      'whitespace-mode
    "tW"      'toggle-word-wrap
    "tz"      'TeX-fold-mode

		;;=================================== Tabs =================================
    "T"       (cons "Tabs" tab-prefix-map)
    "Td"      'tab-bar-close-tab
    "TD"      'tab-bar-close-other-tabs
    "Tg"      'tab-bar-change-tab-group
    "Tm"      'tab-bar-move-tab-to
    "TM"      'tab-bar-move-tab-to-group
    "Tl"      'tab-bar-switch-to-tab
    "TR"      'tab-bar-rename-tab
    "Tt"      'other-tab-prefix
    "Tu"      'tab-bar-undo-close-tab
    "T1"      '("select tab 1..8" . tab-bar-select-tab)
    "T2"      'tab-bar-select-tab
    "T3"      'tab-bar-select-tab
    "T4"      'tab-bar-select-tab
    "T5"      'tab-bar-select-tab
    "T6"      'tab-bar-select-tab
    "T7"      'tab-bar-select-tab
    "T8"      'tab-bar-select-tab
    "T TAB"   'tab-bar-switch-to-last-tab

		;;============================= universal prefix ===========================
    "u"       '("universal" . universal-argument)

		;;================================= windows ================================
    "w"       (cons "windows" (make-sparse-keymap))
    "w TAB"   'alternate-window
    "w+"      'window-layout-toggle
    "wb"      'switch-to-minibuffer-window
    "wd"      'delete-window
    "wD"      'delete-other-windows
    "wm"      'toggle-maximize-buffer
    "wf"      'follow-mode
    "wg"      'golden-ratio
    "wh"      'evil-window-left
    "wH"      'evil-window-move-far-left
    "wj"      'evil-window-down
    "wJ"      'evil-window-move-very-bottom
    "wk"      'evil-window-up
    "wK"      'evil-window-move-very-top
    "wl"      'evil-window-right
    "wL"      'evil-window-move-far-right
    "wr"      'rotate-windows-forward
    "wR"      'rotate-windows-backward
    "wS"      'split-window-vertically
    "ws"      'split-window-vertically-and-focus
    "wt"      'toggle-current-window-dedication
    "wu"      'winner-undo
    "wU"      'winner-redo
    "wV"      'split-window-horizontally
    "wv"      'split-window-horizontally-and-focus
    "w="      'balance-windows
    "w <left>" 'shrink-window-horizontally
    "w <down>" 'shrink-window
    "w <up>"   'enlarge-window
    "w <right>" 'enlarge-window-horizontally
    )

  (general-def
    [remap comment-dwim] 'comment-or-uncomment
    "M-/" 'hippie-expand
    "M-j" (defun scroll-other-window-next-line (&optional arg)
            (interactive "P")
            (scroll-other-window (or arg 1)))
    "M-k" (defun scroll-other-window-previous-line (&optional arg)
            (interactive "P")
            (scroll-other-window (- (or arg 1)))))

  (when IS-MAC
    (general-def
      "s-`"   'other-frame
      "s-a"   'mark-whole-buffer
      "s-c"   'evil-yank
      "s-n"   'make-frame
      "s-m"   'iconify-frame
      "s-q"   'save-buffers-kill-terminal
      "s-v"   'yank
      "s-x"   'kill-region
      "s-w"   'delete-window
      "s-W"   'delete-frame
      "s-z"   'evil-undo
      "s-Z"   'evil-redo
      "s-C-F" 'toggle-frame-fullscreen
      "s-s"   'save-buffer
      "s-<backspace>" (defun delete-line-before-point ()
                        (interactive)
                        (let ((prev-pos (point)))
                          (forward-visible-line 0)
                          (delete-region (point) prev-pos)
                          (indent-according-to-mode))))))

(provide 'core-keybinds)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; core-keybinds.el ends here
