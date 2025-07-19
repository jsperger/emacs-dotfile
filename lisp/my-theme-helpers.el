;;; my-theme-helpers.el --- Helper functions for themes -*- lexical-binding: t -*-

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(defun bolder-faces ()
  (set-face-attribute 'font-lock-function-name-face nil :weight 'semi-bold)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'semi-bold))

(defun customize-tab-bar ()
  "Customize tab-bar faces."
  (set-face-attribute 'tab-bar nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :box `(:line-width (-1 . 4) :color ,(face-background 'default))
                      :inherit 'unspecified)
  (set-face-attribute 'tab-bar-tab nil
                      :weight 'bold
                      :box 'unspecified
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'unspecified)
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :box 'unspecified
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'unspecified))

(defun unscale-outlines ()
  (dolist (outline-number (number-sequence 1 8))
    (let ((outline (intern (format "outline-%d" outline-number))))
      (set-face-attribute outline nil :height 1.0))))

(defun my-disable-flymake-underline ()
  "Disable underlining for Flymake faces while keeping theme colors."
  (custom-set-faces
   `(flymake-error ((t (:underline nil :inherit default))))
   `(flymake-note ((t (:underline nil :inherit default))))
   `(flymake-warning ((t (:underline nil :inherit default))))))

(provide 'my-theme-helpers)
;;; my-theme-helpers.el ends here
