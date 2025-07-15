;;; lisp/my-editor-helpers.el --- Helper functions for editor -*- lexical-binding: t -*-

(defun helpful-reuse-window (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
  (if (eq major-mode 'helpful-mode)
      (pop-to-buffer-same-window buffer-or-name)
    (pop-to-buffer buffer-or-name)))

(defun move-buffer-to-window (windownum follow-focus-p)
  "Moves a buffer to a window. follow-focus-p controls
whether focus moves to new window (with buffer), or stays on current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))

(defun swap-buffers-to-window (windownum follow-focus-p)
  "Swaps visible buffers between active window and selected window.
follow-focus-p controls whether focus moves to new window (with buffer), or
stays on current"
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (winum-select-window-by-number windownum)))

(provide 'my-editor-helpers)
;;; my-editor-helpers.el ends here
