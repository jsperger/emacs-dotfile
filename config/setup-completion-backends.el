;;; setup-completion-backends.el --- Completion backends -*- lexical-binding: t -*-

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :config
(defun flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun without-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    '(orderless-literal . ""))
   ((string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1)))))
  (setopt orderless-style-dispatchers '(flex-if-twiddle without-if-bang))
)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-completion-backends.el ends here
