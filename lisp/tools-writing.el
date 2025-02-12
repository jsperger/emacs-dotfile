;;; tools-writing.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flymake-proselint
  :disabled)

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

;; TODO Add general keybinds for unfill
(use-package unfill)
  
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


(use-package writegood-mode)

(provide 'tools-writing)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; tools-writing.el ends here
