;;; editor-projects.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package binder
  :disabled)
;; Seems to mostly be for plain text files, at least it didn't seem to like
;; when I added tex files.

(use-package todoist
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


(use-package projection
  :disabled
  ;; Enable the `projection-hook' feature.
  :hook (after-init . global-projection-hook-mode)

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


(provide 'editor-projects)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-projects.el ends here
