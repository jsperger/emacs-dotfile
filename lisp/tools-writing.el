;;; tools-writing.el --- prose writing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package binder
	;; Seems to mostly be for plain text files, at least it didn't seem to like
	;; when I added tex files.
	;; I was wrong: see https://codeberg.org/divyaranjan/binder/issues/1
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
  ;; add-to-list notably only accepts one item at a time. It also checks if the
  ;; item is already there, and prepends rather than appends items by default.
  ;; Regular expressions to exclude from jinx results (t for always, otherwise
  ;; by mode)
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  (add-to-list 'jinx-exclude-regexps '(LaTeX-mode "\\s*\\\\input{[^}]+}\\s*"))

  ;; Modes where camelCase or PascalCase words should be accepted.
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
	;; makes previous writing read-only
	;; https://github.com/rnkn/freeze-it
	;; Text remains read-only until you kill the buffer, so that you can't cheat. This is by design, because the minor mode targets the psychological temptation to revise your writing, rather than just the ability.
	;; MAYBE try out the package
	:disabled
	)

(use-package unfill
	;; TODO Add general keybinds for unfill
	:general
	(general-def '(normal visual) text-mode-map
		"g=" 'unfill-region
		"g+" 'unfill-paragraph
		"t+" 'unfill-toggle)
	)

(use-package pandoc-mode
	:disabled
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))

(use-package ox-typst
	;; https://github.com/jmpunkt/ox-typst
	:disabled
  :after org
	:config
	;; Org defaults to LaTeX math syntax 
	;; Should we use typst or Tex math syntax? TeX entails going through pandoc
	(defvar org-typst-math-use-typst-syntax t)
	;; 
	(if org-typst-math-use-typst-syntax
		(setq org-typst-from-latex-environment #'org-typst-from-latex-with-naive
					org-typst-from-latex-fragment #'org-typst-from-latex-with-naive)
(setq org-typst-from-latex-environment #'org-typst-from-latex-with-pandoc
      org-typst-from-latex-fragment #'org-typst-from-latex-with-pandoc)
		)
	)

(use-package writegood-mode
	:disabled
	;; just does font locking, doesn't plug into flymake or other diagnostics
	;; no easy way to navigate the issues it identifies
	)

(provide 'tools-writing)
;;; tools-writing.el ends here
