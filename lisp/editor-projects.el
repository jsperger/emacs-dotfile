;;; editor-projects.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

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
  :hook (tab-bar-mode . tabspaces-mode) 
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-open-or-create-project-and-workspace)
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-initialize-project-with-todo t)
  (tabspaces-todo-file-name "project-todo.org")
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  (tab-bar-new-tab-choice "*scratch*")


	:config
	;; Filter Buffers for Consult-Buffer

	(with-eval-after-load 'consult
		;; hide full buffer list (still available with "b" prefix)
		(consult-customize consult--source-buffer :hidden t :default nil)
		;; set consult-workspace buffer list
		(defvar consult--source-workspace
			(list :name     "Workspace Buffers"
						:narrow   ?w
						:history  'buffer-name-history
						:category 'buffer
						:state    #'consult--buffer-state
						:default  t
						:items    (lambda () (consult--buffer-query
																	:predicate #'tabspaces--local-buffer-p
																	:sort 'visibility
																	:as #'buffer-name)))

			"Set workspace buffer list for consult-buffer.")
		(add-to-list 'consult-buffer-sources 'consult--source-workspace))

	(defun my--consult-tabspaces ()
		"Deactivate isolated buffers when not using tabspaces."
		(require 'consult)
		(cond (tabspaces-mode
					 ;; hide full buffer list (still available with "b")
					 (consult-customize consult--source-buffer :hidden t :default nil)
					 (add-to-list 'consult-buffer-sources 'consult--source-workspace))
					(t
					 ;; reset consult-buffer to show all buffers 
					 (consult-customize consult--source-buffer :hidden nil :default t)
					 (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))

	(add-hook 'tabspaces-mode-hook #'my--consult-tabspaces)

	;; (defvar tabspaces-command-map
	;;   (let ((map (make-sparse-keymap)))
	;;     (define-key map (kbd "C") 'tabspaces-clear-buffers)
	;;     (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
	;;     (define-key map (kbd "d") 'tabspaces-close-workspace)
	;;     (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
	;;     (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
	;;     (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
	;;     (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
	;;     (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
	;;     (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
	;;     map)
  ;;   "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")

	;;   ;; Store in project directories (default)
	;; (setq tabspaces-session-project-session-store 'project)

	;; ;; Store all project sessions in a specific directory
	;; (setq tabspaces-session-project-session-store "~/.emacs.d/tabspaces-sessions/")
  )


(provide 'editor-projects)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-projects.el ends here
