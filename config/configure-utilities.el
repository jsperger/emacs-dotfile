;;; configure-utilities.el --- editor add-ons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; ========================= Utility packages ========================

;; [[file:../its-lit.org::comment tools][comment tools]]
(use-package banner-comment
  :config  (setopt banner-comment-width 68)
  :general  (tyrant-def "ab" 'banner-comment)
  )
;; comment tools ends here

;; [[file:../its-lit.org::open here utilities][open here utilities]]
(use-package reveal-in-folder
  :general (tyrant-def "bf" 'reveal-in-folder))

(use-package terminal-here
  :init (setq terminal-here-mac-terminal-command 'ghostty)
  :general
  (tyrant-def "'" '("terminal here" . terminal-here-launch)
    "p '" '("terminal project root" . terminal-here-project-launch)
    )
  )
;; open here utilities ends here

;; [[file:../its-lit.org::dwim shell commands][dwim shell commands]]
(use-package dwim-shell-command
  :general (tyrant-def "fm" 'dwim-shell-commands-rename-all)
  )
;; dwim shell commands ends here

;; [[file:../its-lit.org::#scratch-buffer-utilities][Scratch buffer utilities:1]]
;; Conveniently create scratches in the same mode as the current file
(use-package scratch
  :general (tyrant-def "bS" 'scratch))
;; Scratch buffer utilities:1 ends here

;; [[file:../its-lit.org::#search-utilities][Search utilities:1]]
(use-package rg
  :config (when (and IS-MAC (daemonp)) (setopt rg-executable "/opt/homebrew/bin/rg"))
  :general (tyrant-def   "fr" 'rg
                         "fR" 'rg-menu
             )
	)
;; Search utilities:1 ends here

;; [[file:../its-lit.org::#terminal-configuration][Terminal configuration:1]]
(use-package vterm
  :general (tyrant-def "av" 'vterm
                       "aV" 'vterm-other-window
                       )
  )
;; Terminal configuration:1 ends here

;; [[file:../its-lit.org::#data-format-modes][Data format modes:1]]
;;;; ======================= Data file format modes ======================

(use-package csv-mode
  :mode ("\\.[cC][sS][vV]\\'" . csv-mode)
  :config
  (add-hook 'csv-mode-hook 'csv-guess-set-separator)
	(add-hook 'csv-mode-hook (lambda () (visual-fill-column-mode -1)))

:general
  (despot-def csv-mode-map
    "s" 'csv-sort-fields
    "n" 'csv-sort-numeric-fields
    "r" 'csv-reverse-region
    "k" 'csv-kill-fields
    "y" 'csv-yank-fields
    "a" 'csv-align-fields
    "A" 'csv-align-mode
    "u" 'csv-unalign-fields
    "t" 'csv-transpose
    )
  )
;; Data format modes:1 ends here

;;;; ================= Modify core editor functionality ================

;; [[file:../its-lit.org::#smooth-scrolling][Smooth scrolling:1]]
(use-package ultra-scroll
  :ensure (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :init  (setq scroll-conservatively 50
               scroll-margin 0) 
  :config (ultra-scroll-mode 1)
  )
;; Smooth scrolling:1 ends here

;; [[file:../its-lit.org::#setup-help-and-documentation-viewers][Setup help and documentation viewers:1]]
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
;; Setup help and documentation viewers:1 ends here

;; [[file:../its-lit.org::#configuring-how-undo-works][Configuring how undo works:1]]
(use-package undo-fu
  :config (setopt evil-undo-system 'undo-fu)
  )

(use-package undo-fu-session
  :custom (undo-fu-session-global-mode t)
  )
;; Configuring how undo works:1 ends here

;; [[file:../its-lit.org::#windowing][Windowing:1]]
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
    "b9" 'buffer-to-window-9)
  )
;; Windowing:1 ends here

;; [[file:../its-lit.org::#outline-and-fold-text][Outline and fold text:1]]
(use-package outline-indent
  :commands outline-indent-minor-mode
  :hook ((python-mode python-ts-mode yaml-mode yaml-ts-mode) . outline-indent-minor-mode)
  :custom
  (outline-indent-ellipsis " ▼ ")
  :general
  (tyrant-def
    "to" 'outline-indent-minor-mode
    "o"  (cons "outline" (make-sparse-keymap))
    "oa" '("all open" . outline-indent-open-folds)
    "oA" '("all closed" . outline-indent-close-folds)
    "ob" 'bicycle-cycle
    "oB" 'bicycle-cycle-global
    "oc" 'outline-indent-close-fold
    "oj" 'outline-forward-same-level
    "ok" 'outline-backward-same-level
    "oh" 'hs-minor-mode
    "oo" 'outline-indent-open-fold
    "oO" 'outline-minor-mode
    "or" 'outline-indent-open-fold-rec
    "ot" 'bicycle-cycle
    "oT" 'bicycle-cycle-global
    "oz" 'TeX-fold-mode)
  )
;; Outline and fold text:1 ends here

;; [[file:../its-lit.org::#spell-check-with-jinx][Spell check with jinx:1]]
(use-package jinx
  :after evil
  :hook (text-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (add-to-list 'jinx-exclude-regexps '(LaTeX-mode "\\s*\\input{[^}]+}\\s*"))

  (add-to-list 'jinx-camel-modes 'R-mode)
  (add-to-list 'jinx-camel-modes 'ess-r-mode)

  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))
  (with-eval-after-load 'evil
    (evil-define-motion evil-prev-jinx-error (count)
      "Go to the COUNT'th spelling mistake preceding point."
      :jump t (jinx-previous (or count 1)))
    (evil-define-motion evil-next-jinx-error (count)
      "Go to the COUNT'th spelling mistake after point."
      :jump t (jinx-next (or count 1))))
  :general([remap ispell-word] 'jinx-correct-word
           [remap evil-prev-flyspell-error] 'evil-prev-jinx-error
           [remap evil-next-flyspell-error] 'evil-next-jinx-error)
  )
;; Spell check with jinx:1 ends here

;; [[file:../its-lit.org::#text-snippet-insertion-and-collections][Text snippet insertion and collections:1]]
;;;; ========================== Text snippets ==========================
(use-package tempel
  :hook ((text-mode prog-mode) . tempel-setup-capf)
  :init
  (setq tempel-trigger-prefix "<"
        tempel-path "~/.emacs.d/etc/templates/*.eld")
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (defun tempel-hippie-try-expand (old)
    "Integrate with hippie expand. Just put this function in `hippie-expand-try-functions-list'." 
    (if (not old)
        (tempel-expand t)
      (undo 1)))

  (add-to-list 'hippie-expand-try-functions-list #'tempel-hippie-try-expand t)
	)

(use-package tempel-collection
  :after tempel)

(use-package yasnippet
	:hook ((text-mode prog-mode) . yas-minor-mode)
	:config
	(setopt yas-global-mode t)
	)

(use-package yasnippet-snippets
	:after yasnippet)
;; Text snippet insertion and collections:1 ends here

;; [[file:../its-lit.org::non-evil text editing][non-evil text editing]]
(use-package surround
  :general
  (tyrant-def
		"Si" 'surround-insert
		"Sd" 'surround-kill
		"Sr" 'surround-replace)
	)

(use-package unfill
	:general
	(general-def '(normal visual) text-mode-map
		"g=" 'unfill-region
		"g+" 'unfill-paragraph
		"t+" 'unfill-toggle)
	)
;; non-evil text editing ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-utilities.el ends here
