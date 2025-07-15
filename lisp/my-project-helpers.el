;;; lisp/my-project-helpers.el --- Helper functions for projects -*- lexical-binding: t -*-

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
         (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources))))
  )

(provide 'my-project-helpers)
;;; my-project-helpers.el ends here
