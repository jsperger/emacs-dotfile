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

(defun my/org-import-directory-as-tangled-blocks (dir-name)
  "Import all .el files from a specified directory into Org source blocks.
The target directory DIR-NAME is relative to `user-emacs-directory`.
The generated blocks are inserted into the current buffer. Each
file is placed under its own headline inside a source block with a
corresponding ':tangle' header argument."
  (interactive "sEnter directory name (e.g., lisp, config): ")
  (let* ((target-dir (expand-file-name dir-name user-emacs-directory))
         (el-files (directory-files target-dir t "\\.el$")))
    (if (not el-files)
        (message "No .el files found in %s" target-dir)
      (dolist (file-path (sort el-files #'string-lessp))
        (let* ((file-name (file-name-nondirectory file-path))
               (tangle-path (concat dir-name "/" file-name))
               (file-contents (with-temp-buffer
                                (insert-file-contents file-path)
                                (buffer-string))))
          (insert (format "** %s\n" file-name))
          (insert (format "#+begin_src emacs-lisp :tangle %s\n" tangle-path))
          (insert file-contents)
          (unless (string-suffix-p "\n" file-contents)
            (insert "\n"))
          (insert "#+end_src\n\n"))))))

(with-eval-after-load 'org-noter-pdftools
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
  (interactive "P")
  (org-noter--with-valid-session
   (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                 (not org-noter-insert-note-no-questions)
                                               org-noter-insert-note-no-questions))
         (org-pdftools-use-isearch-link t)
         (org-pdftools-use-freepointer-annot t))
     (org-noter-insert-note (org-noter--get-precise-info)))))
)

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
