;;; lisp/my-consult-helpers.el --- Helper functions for consult -*- lexical-binding: t -*-

(defun consult-delete-default-contents()
  (remove-hook 'pre-command-hook 'consult-delete-default-contents)
  (cond ((member this-command '(self-insert-command))
         (delete-minibuffer-contents))
        (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))

(defvar consult--source-project-file
  `(:name     "Project File"
              :narrow   ?f
              :category file
              :face     consult-file
              :history  file-name-history
              :state    ,#'consult--file-state
              :enabled  ,(lambda () consult-project-function)
              :items
              ,(lambda ()
                 (when-let (project (project-current t))
                   (let* ((all-files (project-files project))
                          (common-parent-directory
                           (let ((common-prefix (try-completion "" all-files)))
                             (if (> (length common-prefix) 0)
                                 (file-name-directory common-prefix))))
                          (cpd-length (length common-parent-directory))
                          items)
                     (print all-files)
                     (dolist (file all-files items)
                       (let ((part (substring file cpd-length)))
                         (when (equal part "") (setq part "./"))
                         (put-text-property 0 1 'multi-category `(file . ,file) part)
                         (push part items))))))
              "Project file candidate source for `consult-buffer'."))

(defvar consult--source-project-file-hidden
  `(:hidden t :narrow (?f . "Project File") ,@consult--source-project-file)
  "Like `consult--source-project-file' but hidden by default.")

(defvar consult--source-project-recent-file-override
  `(:name "Recent File" :narrow (?r . "Recent File") ,@consult--source-project-file)
  "Like `consult--source-recent-file' but overridden the narrow key.")

(provide 'my-consult-helpers)
;;; my-consult-helpers.el ends here