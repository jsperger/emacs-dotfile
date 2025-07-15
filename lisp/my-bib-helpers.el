;;; lisp/my-bib-helpers.el --- Helper functions for bibliography -*- lexical-binding: t -*-

(defun bibtex-key-embark ()
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (cons 'bibtex-key
            (bibtex-key-in-head)))))

(defun citar-setup-capf ()
  "add `citar-capf' to `completion-at-point-functions'"
  (add-to-list 'completion-at-point-functions #'citar-capf))

(provide 'my-bib-helpers)
;;; my-bib-helpers.el ends here
