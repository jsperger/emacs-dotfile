;;; lisp/my-org-helpers.el --- Helper functions for org-mode -*- lexical-binding: t -*-

(defun org-toggle-hidden-emphasis-markers ()
  "Toggle whether markup should be hidden in 'org-mode'."
  (interactive)
  (if org-hide-emphasis-markers
      (setopt org-hide-emphasis-markers nil)
    (setopt org-hide-emphasis-markers t)
    )
  (font-lock-update)
  )

(defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
  (interactive "P")
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                 (not org-noter-insert-note-no-questions)
                                               org-noter-insert-note-no-questions))
         (org-pdftools-use-isearch-link t)
         (org-pdftools-use-freepointer-annot t))
     (org-noter-insert-note (org-noter--get-precise-info)))))

(defun org-noter-set-start-location (&optional arg)
  "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
  (interactive "P")
  (org-noter--with-valid-session
   (let ((inhibit-read-only t)
         (ast (org-noter--parse-root))
         (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
     (with-current-buffer (org-noter--session-notes-buffer session)
       (org-with-wide-buffer
        (goto-char (org-element-property :begin ast))
        (if arg
            (org-entry-delete nil org-noter-property-note-location)
          (org-entry-put nil org-noter-property-note-location
                         (org-noter--pretty-print-location location))))))))

(provide 'my-org-helpers)
;;; my-org-helpers.el ends here
