;;; lisp/my-evil-helpers.el --- Helper functions for evil -*- lexical-binding: t -*-

(evil-define-text-object evil-pasted (count &rest args)
  (list (save-excursion (evil-goto-mark ?\[) (point))
        (save-excursion (evil-goto-mark ?\]) (1+ (point)))))

(evil-define-text-object evil-inner-buffer (count &optional beg end type)
  (list (point-min) (point-max)))

(provide 'my-evil-helpers)
;;; my-evil-helpers.el ends here
