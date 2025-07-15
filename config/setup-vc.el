;;; config/setup-vc.el --- Version control configuration -*- lexical-binding: t -*-

(require 'my-vc-helpers)

(use-package vc
  :ensure nil
  :config
  (setopt vc-follow-symlinks t))

(use-package magit
  :init
  (setq
	 magit-define-global-key-bindings nil
	 forge-add-default-bindings nil
	 )
	:hook (magit-diff-mode . (lambda () (toggle-truncate-lines -1)))
  :config
  (setopt magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-save-repository-buffers 'dontask)

  (advice-add 'magit-blame-addition           :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-file          :after #'org-reveal-advice)
  (advice-add 'magit-diff-visit-worktree-file :after #'org-reveal-advice)

  :general
  (tyrant-def
    "g"   (cons "git" (make-sparse-keymap))
    "gb"  'magit-blame
    "gc"  'magit-clone
    "gd"  'magit-diff
    "gf"  'magit-file-dispatch
    "gi"  'magit-init
    "gl"  'magit-log-buffer-file
    "gm"  'magit-dispatch
    "gs"  'magit-status
    "gS"  'magit-stage-file
    "gU"  'magit-unstage-file)
	(despot-def '(magit-status-mode-map)
		:major-modes '(magit-status-mode)
    "b"  'magit-blame
    "c"  'magit-commit
    "d"  'magit-diff
		)
	)

(use-package closql)

(use-package ghub)

(use-package forge
  :after magit
  :init
  (setq
	 auth-sources '("~/.authinfo.gpg")
   forge-database-connector 'sqlite-builtin
	 )
	)

(use-package transient
  :after vc magit
  :general
  (general-def transient-base-map   "q" 'transient-quit-one)
  (general-def transient-sticky-map "q" 'transient-quit-seq))

(use-package browse-at-remote
  :after vc magit
  :general
  (tyrant-def "go" 'browse-at-remote))

(use-package diff-hl
  :after vc magit 
	:hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
	(magit-post-refresh . diff-hl-magit-post-refresh)
	:custom (global-diff-hl-mode 1)
  :config
  (setopt diff-hl-side 'right)

  (general-def 'normal
    "[ h" '(diff-hl-previous-hunk :jump t)
    "] h" '(diff-hl-next-hunk :jump t)))

(use-package git-modes
	:mode ("\\.gitignore\\\'" . gitignore-mode)
  :after vc magit)

(use-package git-link
  :after vc magit
  :config
  (setq git-link-open-in-browser t)
  :general
  (tyrant-def
    "gL"  (cons "links" (make-sparse-keymap))
    "gLc" 'git-link-commit
    "gLC" 'git-link-commit-copy-url-only
    "gLl" 'git-link
    "gLL" 'git-link-copy-url-only
    "gLh" 'git-link-homepage))

(use-package git-cliff
	:disabled
	:config
	(with-eval-after-load 'magit-tag
		(transient-append-suffix 'magit-tag
			'(1 0 -1)
			'("c" "changelog" git-cliff-menu)))
	:general
	(despot-def '(magit-status-mode-map)
		:major-modes '(magit-status-mode)
    "C"  'git-cliff-menu
		)
	)

(use-package gitignore-templates
  :after vc magit
  :config
  (setq gitignore-templates-api 'github)
  :general
  (tyrant-def
    "gI"  (cons "gitignore" (make-sparse-keymap))
    "gIn" 'gitignore-templates-new-file
    "gIi" 'gitignore-templates-insert))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

;;; setup-vc.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
