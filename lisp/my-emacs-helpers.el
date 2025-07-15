;;; lisp/my-emacs-helpers.el --- Helper functions for emacs -*- lexical-binding: t -*-

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\" ""
                 crm-separator)
                (car args))
        (cdr args)))

(provide 'my-emacs-helpers)
;;; my-emacs-helpers.el ends here
