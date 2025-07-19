;;; config/setup-projects.el --- Projects configuration -*- lexical-binding: t -*-

(use-package todoist
	:disabled
  :config
  (setq todoist-backing-buffer "~/obsidian/org/todoist")

  (despot-def todoist-mode-map
    "t"     'todoist-task-menu
    "p"     'todoist-project-menu
    "n"     'todoist-new-task
    "c"      'todoist-close-task
    "u"      'todoist-update-task
    "U"      'todoist-update-project)

  )

(use-package bookmark-in-project)

(use-package chronometrist)


(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind ( :map ctl-x-map
          ("p" . disproject-dispatch)))

(use-package perject
	:disabled)

(use-package perpsective
	:disabled)

(use-package projection
  :disabled
  ;; Enable the `projection-hook' feature.
  :hook (elpaca-after-init . global-projection-hook-mode)

  ;; Require projections immediately after project.el.
  :config
  (with-eval-after-load 'project
    (require 'projection))

  :config
  ;; Uncomment if you want to disable prompts for compile commands customized in .dir-locals.el
  ;; (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  ;; (put 'projection-commands-install-project 'safe-local-variable #'stringp)

  ;; Access pre-configured projection commands from a keybinding of your choice.
  ;; Run `M-x describe-keymap projection-map` for a list of available commands.
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :disabled
  ;; Allow interactively selecting available compilation targets from the current
  ;; project type.
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :disabled
  :after embark
  :after projection-multi
  ;; Add the projection set-command bindings to `compile-multi-embark-command-map'.
  :config (projection-multi-embark-setup-command-map))

(use-package tabspaces
  :after consult
  :hook (tab-bar-mode . tabspaces-mode) 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore nil)
  (tab-bar-new-tab-choice "*scratch*")
	:config
	;; Filter Buffers for Consult-Buffer
	(with-eval-after-load 'consult
		;; hide full buffer list (still available with "b" prefix)
		(consult-customize consult--source-buffer :hidden t :default nil)
		;; set consult-workspace buffer list
		(add-to-list 'consult-buffer-sources 'consult--source-workspace)
		)

	(add-hook 'tabspaces-mode-hook #'my--consult-tabspaces)
	(setopt tabspaces-session-project-session-store "~/.emacs.d/var/tabspaces-sessions/")
  )

(use-package activities
  :disabled
	:init
  (activities-mode)
  (activities-tabs-mode)
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list))
	)

(use-package buffler
	:disabled
	)

(use-package perspective
	:disabled
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package burly
	:disabled
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-projects.el ends here
