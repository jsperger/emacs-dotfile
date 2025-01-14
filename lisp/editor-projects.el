;;; editor-projects.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2024 John Sperger

;; Author: John Sperger

;;; Commentary:

;;; Code:

(use-package binder)

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

(provide 'editor-projects)
;;; editor-projects.el ends here
