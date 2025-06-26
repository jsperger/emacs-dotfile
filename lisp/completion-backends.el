;;; completion-backends.el --- summary -*- lexical-binding: t; -*-

;; Author: John Sperger

;;; Commentary:

;; Sorting and filtering bits of completion

;;; Code:

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

(use-package prescient
	:disabled
	:config
  (setopt prescient-sort-full-matches-first t
					prescient-sort-length-enable nil)
	)

(use-package corfu-prescient
	:disabled
	:after prescient
  :hook (corfu-mode . corfu-prescient-mode)
  :init
  (setq corfu-prescient-enable-filtering t)
	)


(use-package vertico-prescient
	:disabled
	:after prescient
	:hook (vertico-mode . vertico-prescient-mode)
	:init
	(setq vertico-prescient-enable-filtering t)
	)

(provide 'completion-backends)
;;; completion-backends.el ends here
