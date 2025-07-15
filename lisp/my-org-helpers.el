;;; lisp/my-org-helpers.el --- Helper functions for org-mode -*- lexical-binding: t -*-

(defun my/org-toggle-hidden-emphasis-markers ()
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

(provide 'my-org-helpers)
;;; my-org-helpers.el ends here
