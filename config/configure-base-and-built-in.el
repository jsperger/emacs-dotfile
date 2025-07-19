;;; configure-base-and-built-in.el --- Built-in packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; [[file:../its-lit.org::*Configure base emacs options][Configure base emacs options:1]]
(use-package emacs
  :ensure nil
  :config
  (setopt enable-recursive-minibuffers t
          user-full-name "John Sperger"
          user-mail-address "josp@duck.com"
          initial-scratch-message nil   ; "make scratch buffer empty"
          tab-width 2 ; tab-width default 2 instead of 4
          fill-column 80 ; fill-column default 80 chars
          ring-bell-function 'ignore ;no beep
          undo-limit 67108864 ; increases undo limit 64mb.
          undo-strong-limit 100663296 ; 96mb.
          undo-outer-limit 1006632960 ; 960mb.
          sentence-end-double-space nil ;single space between sentences
          )
 )
;; Configure base emacs options:1 ends here

;; [[file:../its-lit.org::*Configuring built-in packages][Configuring built-in packages:1]]
(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :config
  (setopt global-auto-revert-non-file-buffers t
          auto-revert-verbose nil
  )
)

(use-package dired
  :ensure nil
  :defer t
  :config
  (setopt dired-auto-revert-buffer t
          dired-kill-when-opening-new-dired-buffer  t
          dired-create-destination-dirs 'always
          dired-do-revert-buffer t
          dired-dwim-target t
          dired-vc-rename-file t
          )
  )
;; Configuring built-in packages:1 ends here

;; [[file:../its-lit.org::*Configure built-in programming utilities][Configure built-in programming utilities:1]]
(use-package display-line-numbers
  :ensure nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (setopt display-line-numbers-type t
          display-line-numbers-width-start 100)
  )

;; (use-package elec-pair
;;   :ensure nil
;; 	:defer t
;; 	:custom (electric-pair-mode 1))


(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
;; no idea what this is from, I don't see the function in help so may be outdated
;; :init (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )

(use-package glasses
	:ensure nil
	:hook (ess-r-mode . glasses-mode)
	:config
	(setopt glasses-separate-parentheses-p nil)
)

(use-package whitespace
  :ensure nil
  :hook (diff-mode . whitespace-mode)
  )
;; Configure built-in programming utilities:1 ends here

;; [[file:../its-lit.org::*Configure ediff for viewing diffs][Configure ediff for viewing diffs:1]]
(use-package ediff
  :ensure nil
  :defer t
  :config
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function 'split-window-horizontally
          ediff-merge-split-window-function 'split-window-horizontally
          )
  )
;; Configure ediff for viewing diffs:1 ends here

;; [[file:../its-lit.org::*Built-in package configuration that were in a separate file for some reason][Built-in package configuration that were in a separate file for some reason:1]]
(use-package doc-view
  :ensure nil
  :defer t
  :config (setopt doc-view-resolution 330)
  )

(use-package files
  :ensure nil
  :defer t
  :config
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq)
  )

(use-package newcomment
  :ensure nil
  :defer t
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment (n)
    (interactive "*p")
    (if (or (region-active-p)
            (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$")))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position n)))))

(use-package project
  :ensure nil
  :defer t
  :config
  (setopt project-vc-merge-submodules nil
          project-switch-commands '((project-switch-to-buffer "Find buffer")
                                    (project-find-file "Find file")
                                    (project-find-regexp "Find regexp")
                                    (project-find-dir "Find directory"))
          project-switch-use-entire-map t
          )
  (add-to-list 'project-find-functions 'project-try-root t)
  )

(use-package recentf
  :ensure nil
  :defer t
  :custom (recentf-mode 1)
  :config (setopt recentf-auto-cleanup 'never
                  recentf-max-saved-items 250)
  )

(use-package savehist
  :ensure nil
  :defer t
  :custom (savehist-mode 1)
  :config
  (setopt enable-recursive-minibuffers t ; allow commands in minibuffers
          history-length 500
          savehist-autosave-interval nil
          savehist-additional-variables '(evil-jumps-history
                                          mark-ring global-mark-ring
                                          search-ring regexp-search-ring
                                          extended-command-history)
          )
  (add-hook 'savehist-save-hook #'savehist-unpropertize-variables-h)
  (add-hook 'savehist-save-hook #'savehist-remove-unprintable-registers-h)
  )


(use-package saveplace
  :ensure nil
  :custom (save-place-mode 1))

(use-package simple
  ;; basic editing commands for emacs
  :ensure nil
  :defer t
  :config
  (setopt column-number-mode t
          delete-trailing-lines nil
          eval-expression-print-length nil
          eval-expression-print-level nil
          next-error-message-highlight t
          ;; save clipboard contents into kill-ring before replace them
          save-interprogram-paste-before-kill t
          )
  )

(use-package winner
  :ensure nil
  :defer t
  :commands (winner-undo winner-redo)
  :init
  (setq winner-dont-bind-my-keys t)
  :custom
  (winner-mode 1)
  :config
  (setopt winner-boring-buffers-regexp "\\*.*\\*")
  )
;; Built-in package configuration that were in a separate file for some reason:1 ends here

;; [[file:../its-lit.org::#which-key-config][=which-key= configuration:1]]
(use-package which-key
  :ensure nil
  :hook (elpaca-after-init)
  :config
  (setopt which-key-idle-delay 0.4
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-allow-evil-operators t)
  (push '((nil . "tab-bar-select-tab") . t) which-key-replacement-alist))
;; =which-key= configuration:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; configure-base-and-built-in ends here
