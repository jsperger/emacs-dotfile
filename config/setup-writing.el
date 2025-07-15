;;; config/setup-writing.el --- Writing tools configuration -*- lexical-binding: t -*-

(require 'my-writing-helpers)

(use-package binder
	:general
	(tyrant-def
		"t;" 'binder-toggle-sidebar
		"t'"  'binder-reveal-in-sidebar
		"[" 'binder-previous
		"]" 'binder-next)
	)

(use-package flymake-proselint
  :disabled)

(use-package flymake-vale
	:ensure (flymake-vale :host github :repo "tpeacock19/flymake-vale")
	:general
	(tyrant-def
		"as" 'flymake-vale-load))

(use-package jinx
  :after evil
  :hook (text-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (add-to-list 'jinx-exclude-regexps '(LaTeX-mode "\\s*\\input{[^}]+}\\s*"))

  (add-to-list 'jinx-camel-modes 'R-mode)
  (add-to-list 'jinx-camel-modes 'ess-r-mode)

  (with-eval-after-load 'vertico-multiform
    (add-to-list 'vertico-multiform-categories '(jinx grid (vertico-grid-annotate . 20))))
  (with-eval-after-load 'evil
    (evil-define-motion evil-prev-jinx-error (count)
      "Go to the COUNT'th spelling mistake preceding point."
      :jump t (jinx-previous (or count 1)))
    (evil-define-motion evil-next-jinx-error (count)
      "Go to the COUNT'th spelling mistake after point."
      :jump t (jinx-next (or count 1))))
  :general
  ([remap ispell-word] 'jinx-correct-word
   [remap evil-prev-flyspell-error] 'evil-prev-jinx-error
   [remap evil-next-flyspell-error] 'evil-next-jinx-error))

(use-package freeze-it
	:disabled
	)

(use-package unfill
	:general
	(general-def '(normal visual) text-mode-map
		"g=" 'unfill-region
		"g+" 'unfill-paragraph
		"t+" 'unfill-toggle)
	)

(use-package pandoc-mode
	:disabled
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc)

(use-package ox-typst
	:disabled
  :after org
	:config
	(defvar org-typst-math-use-typst-syntax t)
	(if org-typst-math-use-typst-syntax
		(setq org-typst-from-latex-environment #'org-typst-from-latex-with-naive
					org-typst-from-latex-fragment #'org-typst-from-latex-with-naive)
(setq org-typst-from-latex-environment #'org-typst-from-latex-with-pandoc
      org-typst-from-latex-fragment #'org-typst-from-latex-with-pandoc)
		)
	)

(use-package writegood-mode
	:disabled
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-writing.el ends here
