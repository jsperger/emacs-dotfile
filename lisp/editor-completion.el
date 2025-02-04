;;; editor-completion.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang

;; Author: Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:


(use-package vertico
  :preface (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (if (string-match "\\*\\(.\\)" crm-separator)
                      (match-string 1 crm-separator)
                    "")
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  :ensure (:files (:defaults "extensions/*.el"))
  :config
  (setq vertico-cycle t)

  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)


  (use-package vertico-buffer
    :ensure nil
    :after vertico
    :no-require
    :hook (vertico-mode . vertico-buffer-mode)
    :config
    (setq vertico-buffer-display-action `(display-buffer-in-side-window
                                          (window-height . ,(+ 3 vertico-count))
                                          (side . top))))

  (use-package vertico-directory
    :ensure nil
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    :general
    (vertico-map "RET"   'vertico-directory-enter
                 "DEL"   'vertico-directory-delete-char
                 "M-DEL" 'vertico-directory-delete-word))

  (use-package vertico-quick
    :ensure nil
    :general
    (vertico-map "C-<return>" 'vertico-quick-exit))

  (use-package vertico-multiform
    :ensure nil
    :hook (vertico-mode . vertico-multiform-mode))
  )


(elpaca-wait)


(use-package marginalia)

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (setq orderless-style-dispatchers '(flex-if-twiddle without-if-bang))

  (advice-add 'company-capf
              :around
              (lambda (capf-fn &rest args)
                (let ((completion-styles '(basic partial-completion substring)))
                  (apply capf-fn args)))))


(use-package consult
  :init
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key "M-.")

  (defun consult-delete-default-contents()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))

  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any
                     consult-line
                     :initial (when-let ((string (thing-at-point 'word)))
                                (add-hook 'pre-command-hook 'consult-delete-default-contents)
                                (propertize string 'face 'shadow)))

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

  (setq consult-project-buffer-sources
        '(consult--source-project-buffer
          consult--source-project-recent-file-override
          consult--source-project-file-hidden))
  :general
  ([remap switch-to-buffer]    'consult-buffer
   [remap goto-line]           'consult-goto-line
   [remap imenu]               'consult-imenu)
  (tyrant-def
    "jI" '("imenu-multi" . consult-imenu-multi)
    "fl" '("locate-files" . consult-find)
    "jj" '("search lines" . consult-line)
    "jJ" '("search lines a/ buffers" . consult-line-multi)
    )
  (org-mode-map
   [remap consult-imenu]       'consult-org-heading
   [remap consult-imenu-multi] 'consult-org-agenda))

(use-package embark
  :ensure (:files (:defaults *.el))
  :init
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (caar targets) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (when-let ((win (get-buffer-window which-key--buffer
                                         'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
          (apply fn args))))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

  (with-eval-after-load 'vertico
    (defun embark-vertico-indicator ()
      (let ((fr face-remapping-alist))
        (lambda (&optional keymap _targets prefix)
          (when (bound-and-true-p vertico--input)
            (setq-local face-remapping-alist
                        (if keymap
                            (cons '(vertico-current . embark-target) fr)
                          fr))))))

    (add-to-list 'embark-indicators #'embark-vertico-indicator))
  :general
  (:keymaps '(global normal)
            "C-." 'embark-act
            "M-." 'embark-dwim))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep)

(use-package corfu
  :ensure (:files (:defaults "extensions/*.el"))
  :demand t
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent 'complete
        tab-first-completion 'eol
        corfu-auto t
        corfu-auto-prefix 1
        corfu-bar-width 0.5
        corfu-cycle t
        corfu-on-exact-match nil
        corfu-preselect 'prompt)

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

  (with-eval-after-load 'evil-collection
    (advice-add 'evil-collection-corfu-setup :after
                (defun resert-corfu-esc ()
                  (general-def 'insert corfu-map "<escape>" 'nil))))

  (use-package corfu-history
    :ensure nil
    :hook (global-corfu-mode . corfu-history-mode))

  (use-package corfu-popupinfo
    :ensure nil
    :hook (global-corfu-mode . corfu-popupinfo-mode)
    :config
    (set-face-attribute 'corfu-popupinfo nil :height 0.95))

  :general
  (corfu-map
   "RET"    nil
   "M-RET"  'corfu-quick-insert
   "S-SPC"  'corfu-insert-separator)
  )

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package prescient
  :init
  (use-package vertico-prescient
    :hook (vertico-mode . vertico-prescient-mode)
    :init
    (setq vertico-prescient-enable-filtering nil))
  (use-package corfu-prescient
    :hook (corfu-mode . corfu-prescient-mode)
    :init
    (setq corfu-prescient-enable-filtering nil))
  :config
  (setq prescient-sort-full-matches-first t
        prescient-sort-length-enable nil))


(use-package tempel
  :hook ((text-mode prog-mode) . tempel-setup-capf)
  :init
  (setq tempel-trigger-prefix "<"
        tempel-path "~/.emacs.d/etc/templates/*.eld")
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (defun tempel-hippie-try-expand (old)
    "Integrate with hippie expand.
Just put this function in `hippie-expand-try-functions-list'."
    (if (not old)
        (tempel-expand t)
      (undo 1)))
  (add-to-list 'hippie-expand-try-functions-list #'tempel-hippie-try-expand t))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))


(use-package tempel-collection
  :after tempel)

(use-package eglot-tempel
  :hook (eglot-managed-mode .  eglot-tempel-mode)
)



(provide 'editor-completion)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; editor-completion.el ends here
