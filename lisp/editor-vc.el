;;; editor-vc.el --- version control -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>
;; edits: John Sperger
;;; Commentary:

;;; Code:

(use-package vc
  :ensure nil
  :config
  (setopt vc-follow-symlinks t))

(use-package magit
	;;	:after evil-collection
;; didn't work, trying to fix keybind issue, see https://github.com/emacs-evil/evil-collection/issues/543
  :init
  (setq
	 magit-define-global-key-bindings nil
	 forge-add-default-bindings nil
	 )
  ;; (with-eval-after-load 'project
  ;;   (define-key project-prefix-map "m" #'magit-project-status)
  ;;   (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

	:hook (magit-diff-mode . (lambda () (toggle-truncate-lines -1)))
  :config
  (setopt magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
        magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-save-repository-buffers 'dontask)

  ;; (add-hook 'magit-process-find-password-functions 'magit-process-password-auth-source)

  (defun org-reveal-advice (&rest _args)
    "Unfold the org headings for a target line.
    This can be used to advice functions that might open .org files.

    For example: To unfold from a magit diff buffer, evaluate the following:
    (advice-add 'magit-diff-visit-file :after #'org-reveal-advice)"
    (when (derived-mode-p 'org-mode) (org-reveal)))

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

(use-package closql
  :disabled)

(use-package ghub)

(use-package forge
  :after magit 
  :init
  (setq
	 auth-sources '("~/.authinfo")
;;  forge-add-default-bindings nil  must be done before magit is loaded not here
   forge-database-connector 'sqlite-builtin
	 )
	)

(use-package transient
  :after vc magit
  :config
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
	:mode ("\\.gitignore\\'" . gitignore-mode)
  :after vc magit)

;; (use-package git-timemachine
;;   :ensure t
;;   :config
;;   (general-def git-timemachine-mode-map
;;     "gt" '(:ignore t :which-key "git-timemachine"))
;;   :general
;;   (tyrant-def "gt" 'git-timemachine))

(use-package git-link
  :after vc magit
  :config
  (setq git-link-open-in-browser t)

  (defun git-link-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link)))

  (defun git-link-commit-copy-url-only ()
    "Only copy the generated link to the kill ring."
    (interactive)
    (let (git-link-open-in-browser)
      (call-interactively 'git-link-commit)))
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
	;; OPTIONAL
	;; Integrate to `magit-tag'
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

(provide 'editor-vc)
;;; editor-vc.el ends here
