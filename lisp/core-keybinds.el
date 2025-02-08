;;; core-keybinds.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(use-package which-key
  :config
  (setq which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t)

  (push '((nil . "tab-bar-select-tab") . t) which-key-replacement-alist))

(use-package general
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
    "!"       '("shell cmd" . shell-command)
    "i"       '("Tempel insert" . tempel-insert)

    "a"       (cons "apps" (make-sparse-keymap))
    "ac"      'calc-dispatch
    "ap"      'list-processes
    "ad"      'todoist
    "af"      'fontaine-set-preset
    "ao"      (cons "obsidian" (make-sparse-keymap))
    "aoc"     'obsidian-capture
    "aoj"     'obsidian-jump
    "aos"     'obsidian-search
    "at"      'eat
    "aP"      'proced

    "b"       (cons "buffers" (make-sparse-keymap))
    "bb"      'switch-to-buffer
    "bB"      'ibuffer
    "bd"      'kill-current-buffer
    "bm"      'switch-to-messages-buffer
    "bs"      'switch-to-scratch-buffer
    "bu"      'reopen-killed-buffer
    "bx"      'kill-buffer-and-window

    "c"       (cons "code" (make-sparse-keymap))
    "cb"      'flymake-show-buffer-diagnostics
    "cc"      'compile
    "cn"      'next-error
    "cp"      'previous-error
    "cr"      'recompile
    "cx"      'kill-compilation
    "c="      'indent-region-or-buffer

    "e"       (cons "elpaca" (make-sparse-keymap))
    "ef"      'elpaca-fetch-all
    "eF"      'elpaca-fetch
    "el"      'elpaca-log
    "em"      'elpaca-manager
    "eu"      'elpaca-merge ;; update equivalent
    ;; Intentional no bind for merge-all
    "E"       (cons "Ellama" (make-sparse-keymap))
    "Ec"      (cons "code" (make-sparse-keymap))
    "Ecc"     'ellama-code-complete
    "Eca"     'ellama-code-add
    "Ece"     'ellama-code-edit
    "Eci"     'ellama-code-improve
    "Ecr"     'ellama-code-review
    "Es"      (cons "summarize & session" (make-sparse-keymap))
    "Ess"    'ellama-summarize
    "Esw"    'ellama-summarize-webpage
    "Esl"    'ellama-load-session
    "Esr"    'ellama-session-rename
    "Esd"    'ellama-session-remove
    "Esa"    'ellama-session-switch
    "Ei"      (cons "improve" (make-sparse-keymap))
    "Eiw"    'ellama-improve-wording
    "Eig"    'ellama-improve-grammar
    "Eic"    'ellama-improve-conciseness
    "Em"      (cons "make" (make-sparse-keymap))
    "Eml"    'ellama-make-list
    "Emt"    'ellama-make-table
    "Emf"    'ellama-make-format
    "Ea"      (cons "ask & chat" (make-sparse-keymap))
    "Eaa"    'ellama-ask-about
    "Eai"    'ellama-chat
    "Eal"    'ellama-ask-line
    "Eas"    'ellama-ask-selection
    "Et"      (cons "translate" (make-sparse-keymap))
    "Ett"    'ellama-translate
    "Etb"    'ellama-translate-buffer
    "Ete"    'ellama-chat-translation-enable
    "Etd"    'ellama-chat-translation-disable
    "Etc"    'ellama-complete
    "Ed"      (cons "define" (make-sparse-keymap))
    "Edw"    'ellama-define-word
    "Ex"      (cons "context" (make-sparse-keymap))
    "Exb"    'ellama-context-add-buffer
    "Exf"    'ellama-context-add-file
    "Exs"    'ellama-context-add-selection
    "Exi"    'ellama-context-add-info-node
    "Ep"      (cons "provider" (make-sparse-keymap))
    "Eps"    'ellama-provider-select

    "f"       (cons "files" (make-sparse-keymap))
    "fC"      '("copy-file" . write-file)
    "fD"      'delete-current-buffer-file
    "fe"      'find-library
    "fE"      'sudo-edit
    "ff"      'find-file
    "fj"      'dired-jump
    "fJ"      'dired-jump-other-window
    "fo"      'open-file-or-directory-in-external-app
		"fr"      'rg
    "fR"      'rename-current-buffer-file
    "fs"      'save-buffer
    "fv"      (cons "variables" (make-sparse-keymap))
    "fvd"     'add-dir-local-variable
    "fvf"     'add-file-local-variable
    "fvp"     'add-file-local-variable-prop-line

    "F"       (cons "Frame" (make-sparse-keymap))
    "Fd"      'delete-frame
    "FD"      'delete-other-frames
    "Fn"      'make-frame
    "Fo"      'other-frame

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

    "j"       (cons "jump" (make-sparse-keymap))
    "jb"      'bookmark-jump
    "ji"      'imenu
    "jg"      'avy-goto-char-timer
    "jn"      'mindstream-new
    "jo"      'obsidian-jump

    "l"  (cons "eglot" (make-sparse-keymap))
    "la" 'eglot-code-actions
    "lb" 'eglot-events-buffer
    "lr" 'eglot-rename
    "lR" 'eglot-reconnect
    "lx" 'eglot-shutdown
    "lX" 'eglot-shutdown-all
    "l=" 'eglot-format

    "m"       (cons "major mode" (make-sparse-keymap))

    "p"       (cons "projects" project-prefix-map)
    "pt"      'project-open-in-tab
    "pb"      'bookmark-in-project-toggle
    "pj"      'bookmark-in-project-jump

    "q"       (cons "quit" (make-sparse-keymap))
    "qd"      'restart-emacs-debug-init
    "qr"      'restart-emacs
    "qR"      'restart-emacs-without-desktop
    "qf"      'delete-frame
    "qq"      'save-buffers-kill-terminal
    "qQ"      'save-buffers-kill-emacs
    "qs"      'server-shutdown

    "s"       (cons "spelling" (make-sparse-keymap))
    "sb"      'flyspell-buffer
    "sn"      'flyspell-goto-next-error
    "sr"      'flyspell-region
    "sc"      'jinx-correct
    "sC"      'jinx-correct-nearest
    "sj"      'jinx-next

    "s"       (cons "spelling" (make-sparse-keymap))
    "sb"      'flyspell-buffer
    "sn"      'flyspell-goto-next-error
    "sr"      'flyspell-region
    "sc"      'jinx-correct
    "sC" 'jinx-correct-nearest
    "sj" 'jinx-next

    "r"       (cons "replace" (make-sparse-keymap))
    ;; Ask = query
    "ra"      'query-replace
    "rs"      'replace-string
    "rr"      'replace-string-in-region

    ;; "t"       (cons "tempel" (make-sparse-keymap))
    ;; "ti"      'tempel-insert

    "t"       (cons "toggles" (make-sparse-keymap))
    "ta"      'auto-fill-mode
    "tb"      'global-obsidian-mode
    "tc"      'consult-minor-mode-menu
    "td"      'toggle-debug-on-error
    "tf"      'display-fill-column-indicator-mode
    "tg"      'golden-ratio-mode
    "th"      'hs-minor-mode
    "tj"      'jinx-mode
    "tl"      'toggle-truncate-lines
    "tm"      'flymake-mode
    "tn"      'display-line-numbers-mode
    "to"      'outline-minor-mode
    "tO"      'org-modern-mode
    "tp"      'pdf-view-mode
    "tr"      'writegood-mode
    "ts"      'flyspell-mode
    "tt"      'LateX-mode
    "tv"      'visual-fill-column-mode
    "tw"      'whitespace-mode
    "tW"      'toggle-word-wrap
    "tz"      'TeX-fold-mode

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

    "u"       '("universal arg" . universal-argument)

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

(elpaca-wait)

(use-package evil
  :demand t
  :hook ((prog-mode . hs-minor-mode)) ;why is this here?
  :init
	(setopt evil-want-keybinding nil
					evil-symbol-word-search t
					evil-ex-search-vim-style-regexp t
					evil-search-module 'evil-search
					evil-magic 'very-magic
					evil-want-C-u-delete t
					evil-want-C-u-scroll t
					hs-minor-mode-map nil)
  :config
  (setopt evil-cross-lines t
        evil-kill-on-visual-paste nil
        evil-move-beyond-eol t
        evil-want-C-i-jump t
        evil-want-fine-undo t
        evil-v$-excludes-newline t
				evil-mode t)
  (setopt evil-normal-state-cursor  '("DarkGoldenrod2" box)
        evil-insert-state-cursor  '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor   '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor  '("gray" (hbar . 2))
        evil-motion-state-cursor  '("plum3" box))

  (evil-set-undo-system 'undo-redo)

	;; think this is a problem because evil is loaded early and
	;; doesn't seem to be picking up config changes in some mode use-package
	;; declarations e.g. ess-indent-offset
  (progn
    ;; Thanks to `editorconfig-emacs' for many of these
    (defvar evil-indent-variable-alist
      ;; Note that derived modes must come before their sources
      '(((awk-mode c-mode c++-mode java-mode
                   idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
        (groovy-mode . groovy-indent-offset)
        (python-mode . python-indent-offset)
        (cmake-mode . cmake-tab-width)
        (coffee-mode . coffee-tab-width)
        (cperl-mode . cperl-indent-level)
        (css-mode . css-indent-offset)
        (elixir-mode . elixir-smie-indent-basic)
        ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
        (enh-ruby-mode . enh-ruby-indent-level)
        (erlang-mode . erlang-indent-level)
        (ess-mode . ess-indent-offset)
        (js2-mode . js2-basic-offset)
        (js3-mode . js3-indent-level)
        ((js-mode json-mode) . js-indent-level)
        (LaTeX-mode . (LaTeX-indent-level tex-indent-basic))
        (livescript-mode . livescript-tab-width)
        (mustache-mode . mustache-basic-offset)
        (nxml-mode . nxml-child-indent)
        (perl-mode . perl-indent-level)
        (puppet-mode . puppet-indent-level)
        (ruby-mode . ruby-indent-level)
        (rust-mode . rust-indent-offset)
        (scala-mode . scala-indent:step)
        (sgml-mode . sgml-basic-offset)
        (sh-mode . sh-basic-offset)
        (typescript-mode . typescript-indent-level)
        (web-mode . web-mode-markup-indent-offset)
        (yaml-mode . yaml-indent-offset))
      "An alist where each key is either a symbol corresponding
  to a major mode, a list of such symbols, or the symbol t,
  acting as default. The values are either integers, symbols
  or lists of these.")

    (defun set-evil-shift-width ()
      "Set the value of `evil-shift-width' based on the indentation settings of the
  current major mode."
      (let ((shift-width
             (catch 'break
               (dolist (test evil-indent-variable-alist)
                 (let ((mode (car test))
                       (val (cdr test)))
                   (when (or (and (symbolp mode) (derived-mode-p mode))
                             (and (listp mode) (apply 'derived-mode-p mode))
                             (eq 't mode))
                     (when (not (listp val))
                       (setq val (list val)))
                     (dolist (v val)
                       (cond
                        ((integerp v) (throw 'break v))
                        ((and (symbolp v) (boundp v))
                         (throw 'break (symbol-value v))))))))
               (throw 'break (default-value 'evil-shift-width)))))
        (when (and (integerp shift-width)
                   (< 0 shift-width))
          (setq-local evil-shift-width shift-width))))

    ;; after major mode has changed, reset evil-shift-width
    (add-hook 'after-change-major-mode-hook #'set-evil-shift-width 'append))

  (progn
    (evil-define-text-object evil-pasted (count &rest args)
      (list (save-excursion (evil-goto-mark ?\[) (point))
            (save-excursion (evil-goto-mark ?\]) (1+ (point)))))
    (define-key evil-inner-text-objects-map "P" 'evil-pasted)

    ;; define text-object for entire buffer
    (evil-define-text-object evil-inner-buffer (count &optional beg end type)
      (list (point-min) (point-max)))
    (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

  ;; allow eldoc to trigger directly after changing modes
  (eldoc-add-command #'evil-normal-state
                     #'evil-insert
                     #'evil-change
                     #'evil-delete
                     #'evil-replace)

  (add-hook 'evil-normal-state-exit-hook #'evil-ex-nohighlight)

  (general-def 'normal "zf" 'reposition-window)
  (general-def 'insert [remap evil-complete-previous] 'hippie-expand))

(elpaca-wait)

(use-package evil-collection
  :init
  (add-hook 'org-agenda-mode-hook
            (lambda () (evil-collection-unimpaired-mode -1))))

(use-package evil-owl
  :config
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (setq evil-owl-idle-delay 0.5))

(use-package evil-snipe
  :hook ((evil-snipe-mode . evil-snipe-override-mode))
  :config
  (setq evil-snipe-spillover-scope 'whole-buffer)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-surround
  :hook ((text-mode prog-mode conf-mode) . evil-surround-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  ;; `s' for surround instead of `subtitute'
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute))

(provide 'core-keybinds)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; core-keybinds.el ends here
