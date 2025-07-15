;;; config/setup-consult.el --- Consult configuration -*- lexical-binding: t -*-

(require 'my-consult-helpers)

(use-package consult
  :init
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key "M-.")

  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any
                     consult-line
                     :initial (when-let ((string (thing-at-point 'word)))
                                (add-hook 'pre-command-hook 'consult-delete-default-contents)
                                (propertize string 'face 'shadow)))

  (setq consult-project-buffer-sources
        '(consult--source-project-buffer
          consult--source-project-recent-file-override
          consult--source-project-file-hidden))
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
