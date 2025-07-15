;;; lisp/my-lang-helpers.el --- Language helper functions -*- lexical-binding: t -*-

; ================================== R ================================== ;
(defun oc-set-ess-offset-indent ()
  "Set ess-offset-arguments values to my preferred defaults. Motivated by an Org src block issue (that  is likely due to a config issue on my end)."
  (interactive)
  (setopt
   ess-offset-arguments 'prev-call
   ess-offset-arguments-newline 'prev-line)
  )

(defun air-format-dir ()
  "Run the 'air' formatter in the current directory asynchronously"
  (interactive)
  (async-shell-command "air format .")
  )

(defun air-format-file ()
  "Run the 'air format' shell command on the current buffer's file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          ;; Save the buffer before formatting to ensure changes are read by 'air'
          (save-buffer)
          (async-shell-command
           (format "air format %s" (shell-quote-argument filename)))
          (message "Running 'air format' on %s..." filename))
      (message "Buffer is not visiting a file.")))
  )


; ================================ LaTeX ================================
(defun replace-double-dollar-math ()
  "Replace display math $$...$$ with \[...\] in the current buffer. Double dollar signs should no longer be used in LaTeX."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\$\$\(.*?\)\\$\$" nil t)
    (replace-match "\\[\\1\\]" nil nil))
  )


(provide 'my-lang-helpers)
;;; my-lang-helpers.el ends here
