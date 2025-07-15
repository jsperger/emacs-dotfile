;;; lisp/my-core-helpers.el --- Helper functions for core config -*- lexical-binding: t -*-

(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash."
  (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"brew install trash\"")
  (call-process "trash" nil 0 nil "-F"  file))

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(defun desktop-read@inhibit-message (fn)
  "Inhibit `desktop-read' message"
  (let ((inhibit-message t))
    (funcall fn)))

(defun check-large-file ()
  "Check when opening large files - literal file open."
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode
                      '(archive-mode doc-view-mode doc-view-mode-maybe
                                     ebrowse-tree-mode emacs-lisp-mode
                                     fundamental-mode git-commit-mode
                                     image-mode jka-compr pdf-view-mode
                                     tags-table-mode tar-mode)))
           size (> size (* 1024 1024 20))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setopt buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(defun make-directory-maybe ()
  "Create parent directory if not exists while visiting file."
  (let ((dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p dir)
      (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
          (make-directory dir t)
        (keyboard-quit)))))

(defun comment-or-uncomment (n)
  (interactive "*p")
  (if (or (region-active-p)
          (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$")))
      (call-interactively 'comment-dwim)
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position n))))

(defcustom project-root-files '(".project")
  "Files that indicate the root of a project."
  :group 'project
  :type '(repeat string))

(defun project-try-root (dir)
  "Search up the `DIR' for `project-root-files'."
  (when-let* ((root
              (seq-some
               (lambda (n) (locate-dominating-file dir n))
               project-root-files)))
    (cons 'transient (expand-file-name root))))

(defun savehist-unpropertize-variables-h ()
  "Remove text properties from `kill-ring' to reduce savehist cache size."
  (setq kill-ring
        (mapcar #'substring-no-properties
                (cl-remove-if-not #'stringp kill-ring))
        register-alist
        (cl-loop for (reg . item) in register-alist
                 if (stringp item)
                 collect (cons reg (substring-no-properties item))
                 else collect (cons reg item))))

(defun savehist-remove-unprintable-registers-h ()
  "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
  ;; Save new value in the temp buffer savehist is running
  ;; `savehist-save-hook' in. We don't want to actually remove the
  ;; unserializable registers in the current session!
  (setq-local register-alist
              (cl-remove-if-not #'savehist-printable register-alist)))

(provide 'my-core-helpers)
;;; my-core-helpers.el ends here
