;;; config/setup-consult.el --- Consult configuration -*- lexical-binding: t -*-

(use-package consult
  :init
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
  (setq consult-project-buffer-sources
        '(consult--source-project-buffer
          consult--source-project-recent-file-override
          consult--source-project-file-hidden)
        )

  :config
  (setopt consult-narrow-key "?"
          consult-preview-key "M-.")

  (defvar consult--source-project-file
    `(:name     "Project File"
                :narrow   ?f
                :category file
                :face     consult-file
                :history  file-name-history
                :state    ,#'consult--file-state
                :enabled  ,(lambda () consult-project-function)
                :items
                ,(lambda ()
                   (when-let (project (project-current t))
                     (let* ((all-files (project-files project))
                            (common-parent-directory
                             (let ((common-prefix (try-completion "" all-files)))
                               (if (> (length common-prefix) 0)
                                   (file-name-directory common-prefix))))
                            (cpd-length (length common-parent-directory))
                            items)
                       (print all-files)
                       (dolist (file all-files items)
                         (let ((part (substring file cpd-length)))
                           (when (equal part "") (setq part "./"))
                           (put-text-property 0 1 'multi-category `(file . ,file) part)
                           (push part items))))))
                "Project file candidate source for `consult-buffer'."))

  (defvar consult--source-project-file-hidden
    `(:hidden t :narrow (?f . "Project File") ,@consult--source-project-file)
    "Like `consult--source-project-file' but hidden by default.")

  (defvar consult--source-project-recent-file-override
    `(:name "Recent File" :narrow (?r . "Recent File") ,@consult--source-project-file)
    "Like `consult--source-recent-file' but overridden the narrow key.")

  (defun consult-delete-default-contents()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-goto-line consult-imenu consult-line
   :preview-key 'any
   consult-line
   :initial (when-let ((string (thing-at-point 'word)))
              (add-hook 'pre-command-hook 'consult-delete-default-contents)
              (propertize string 'face 'shadow)
              )
   )

  :general
  ([remap switch-to-buffer]    'consult-buffer
   [remap goto-line]           'consult-goto-line
   [remap imenu]               'consult-imenu)
  (tyrant-def
    "JI" '("imenu-multi" . consult-imenu-multi)
    "fl" '("locate-files" . consult-find)
    "Jj" '("search lines" . consult-line)
    "JJ" '("search lines a/ buffers" . consult-line-multi)
    )
  (org-mode-map
   [remap consult-imenu]       'consult-org-heading
   [remap consult-imenu-multi] 'consult-org-agenda)
  )

(use-package consult-todo
	;; something seems off, it's slow as hell
	:disabled
	:after consult
	:config
	(defconst consult-todo--narrow
		'((?t . "TODO")
			(?f . "FIXME")
			(?b . "BUG")
			(?h . "HACK"))
		"Default mapping of narrow and keywords.")
	)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-consult.el ends here
