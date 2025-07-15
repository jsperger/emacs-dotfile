;;; lisp/my-writing-helpers.el --- Helper functions for writing -*- lexical-binding: t -*-

(defun pandoc ()
  "Start pandoc for the buffer and open the menu"
  (interactive)
  (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
  (pandoc-main-hydra/body))

(provide 'my-writing-helpers)
;;; my-writing-helpers.el ends here
