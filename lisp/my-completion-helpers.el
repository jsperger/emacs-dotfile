;;; lisp/my-completion-helpers.el --- Helper functions for completion -*- lexical-binding: t -*-

(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (string-replace "[ \t]*" "" crm-separator)
                (car args))
        (cdr args)))

(provide 'my-completion-helpers)
;;; my-completion-helpers.el ends here
