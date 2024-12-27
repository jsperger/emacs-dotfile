;;; editor-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:
(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold #x1200000))

(use-package helpful
  :config
  (defun helpful-reuse-window (buffer-or-name)
    "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
    (if (eq major-mode 'helpful-mode)
        (pop-to-buffer-same-window buffer-or-name)
      (pop-to-buffer buffer-or-name)))

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
    "jl" 'link-hint-open-link
    "jL" 'link-hint-open-multiple-links
    "jy" 'link-hint-copy-link))

(use-package pandoc-mode
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))

(use-package reveal-in-osx-finder
  :if (eq system-type 'darwin)
  :general (tyrant-def "bf" 'reveal-in-osx-finder))

(use-package terminal-here
  :config
  (setq terminal-here-mac-terminal-command  (lambda (dir)
                                              (list "open" "-a" "Alacritty" "--args" "--working-directory" (expand-file-name dir)))
        terminal-here-linux-terminal-command 'alacritty
        terminal-here-project-root-function (lambda () (project-root (project-current t))))
  :general
  (tyrant-def
    "\'"   'terminal-here-launch
    "p \'" 'terminal-here-project-launch))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package tree-sitter-langs)

(use-package undohist
  :config
  (add-to-list 'undohist-ignored-files "EDITMSG")

  (defun undohist-recover-safe@around (fn)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (p) nil)))
      (funcall fn)))
  (advice-add #'undohist-recover-safe :around #'undohist-recover-safe@around))

(use-package xr)  ;; Convert string regexp to rx notation

(use-package winum
  :init
  (with-eval-after-load 'which-key
    (push '((nil . "winum-select-window-[1-9]") . t) which-key-replacement-alist)
    (push '((nil . "buffer-to-window-[1-9]") . t) which-key-replacement-alist))
  :config
  (setq winum-auto-assign-0-to-minibuffer t
        winum-auto-setup-mode-line t
        winum-scope 'frame-local)

  (defun move-buffer-to-window (windownum follow-focus-p)
    "Moves a buffer to a window. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current"
    (interactive)
    (let ((b (current-buffer))
          (w1 (selected-window))
          (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b)))
    (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

  (defun swap-buffers-to-window (windownum follow-focus-p)
    "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
    (interactive)
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum)))

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
    "0"  '("select window 0 or 10" . winum-select-window-0-or-10)
    "1"  '("select window 1..9" . winum-select-window-1)
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

;; (use-package prettier)

;; (use-package reformatter)

(use-package apheleia
  :config (setq apheleia-global-mode nil)
  )

(use-package emacs-elisp-autofmt
  :disabled)

;; (use-package biome)

(provide 'editor-misc)
;;; editor-misc.el ends here
