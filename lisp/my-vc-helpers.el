;;; lisp/my-vc-helpers.el --- Helper functions for version control -*- lexical-binding: t -*-

(defun org-reveal-advice (&rest _args)
  "Unfold the org headings for a target line.
This can be used to advice functions that might open .org files.

For example: To unfold from a magit diff buffer, evaluate the following:
(advice-add 'magit-diff-visit-file :after #'org-reveal-advice)"
  (when (derived-mode-p 'org-mode) (org-reveal)))

(defun git-link-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link)))

(defun git-link-commit-copy-url-only ()
  "Only copy the generated link to the kill ring."
  (interactive)
  (let (git-link-open-in-browser)
    (call-interactively 'git-link-commit)))

(provide 'my-vc-helpers)
;;; my-vc-helpers.el ends here
