;;; lisp/my-tempel-helpers.el --- Helper functions for tempel -*- lexical-binding: t -*-

(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))

(defun tempel-hippie-try-expand (old)
  "Integrate with hippie expand.
Just put this function in `hippie-expand-try-functions-list'."
  (if (not old)
      (tempel-expand t)
    (undo 1)))

(provide 'my-tempel-helpers)
;;; my-tempel-helpers.el ends here
