;;; core-config.el -*- lexical-binding: t; -*-

;; Forked from Tianshu Wang

;; Author: John Sperger forked from Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

;; smooth scrolling
;; (setq scroll-conservatively 101
;;       scroll-margin 2)

(use-package ultra-scroll
	;; :load-path "~/code/emacs/ultra-scroll" ; if you git clone'd instead of using vc
	;; :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; For Emacs>=30
	:ensure (ultra-scroll
					 :type git :host github :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1)
	)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)


;; scroll compilation to first error or end
(setopt compilation-scroll-output 'first-error)

;; Use system trash for file deletion.
(setopt delete-by-moving-to-trash t)

;; autosave each change
(setopt bookmark-save-flag 1)

;; keep focus while navigating help buffers
(setopt help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setopt use-short-answers t)

;; don't load outdated compiled files.
(setopt load-prefer-newer t)

;; don't save duplicates in kill-ring
(setopt kill-do-not-save-duplicates t)

;; break lines after more characters
(setopt word-wrap-by-category t)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(use-package desktop
  :disabled
  :ensure nil
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setopt desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager nil
        desktop-save t)

  (dolist (param '(foreground-color background-color background-mode font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))

  (defun desktop-read@inhibit-message (fn)
    "Inhibit `desktop-read' message"
    (let ((inhibit-message t))
      (funcall fn)))
  (advice-add 'desktop-read :around #'desktop-read@inhibit-message))

(use-package dired
  :ensure nil
  :defer t
  :config
  (when IS-MAC ;on mac use external ls from homebrew gnutils
    (setopt ls-lisp-use-insert-directory-program t
          insert-directory-program "/opt/homebrew/opt/coreutils/libexec/gnubin/ls"))
  (setopt dired-listing-switches "-aBhl --group-directories-first")
  (setopt dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer  t
        dired-create-destination-dirs 'always
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-vc-rename-file t))

;; (use-package casual-dired
;;   :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))

(use-package display-line-numbers
  :ensure nil
	:defer t
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (setopt display-line-numbers-type t
                display-line-numbers-width-start 100))

(use-package doc-view
  :ensure nil
  :defer t
  :config
  (setopt doc-view-resolution 400))

(use-package ediff
  :ensure nil
  :defer t
  :config
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :ensure nil
	:defer t
	:custom (electric-pair-mode 1))

(use-package files
  :ensure nil
	:defer t
;;  :hook (before-save . delete-trailing-whitespace)
  :config
  ;; Prompt to open file literally if large file.
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
  (add-hook 'find-file-hook #'check-large-file)

  ;; see document of `move-file-to-trash'
  (defun system-move-file-to-trash (filename)
    (process-file-shell-command
     (format "trash %S" (file-local-name filename))))

  (defun make-directory-maybe ()
    "Create parent directory if not exists while visiting file."
    (let ((dir (file-name-directory buffer-file-name)))
      (unless (file-exists-p dir)
        (if (y-or-n-p (format "Directory %s does not exist,do you want you create it? " dir))
            (make-directory dir t)
          (keyboard-quit)))))
  (add-to-list 'find-file-not-found-functions 'make-directory-maybe nil #'eq))

(use-package flymake
  :ensure nil
	:defer t
  :hook (prog-mode . flymake-mode)
  :init (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )

(use-package newcomment
  :ensure nil
	:defer t
  :commands comment-or-uncomment
  :config
  (defun comment-or-uncomment (n)
    (interactive "*p")
    (if (or (region-active-p)
            (save-excursion
              (beginning-of-line)
              (looking-at "\\s-*$")))
        (call-interactively 'comment-dwim)
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position n)))))

(use-package project
  :ensure nil
  :defer t
  :config
  (setopt project-vc-merge-submodules nil
        project-switch-commands '((project-switch-to-buffer "Find buffer")
                                  (project-find-file "Find file")
                                  (project-find-regexp "Find regexp")
                                  (project-find-dir "Find directory"))
        project-switch-use-entire-map t)

  (defcustom project-root-files '(".project")
    "Files that indicate the root of a project."
    :group 'project
    :type '(repeat string))

  (defun project-try-root (dir)
    "Search up the `DIR' for `project-root-files'."
    (when-let ((root
                (seq-some
                 (lambda (n) (locate-dominating-file dir n))
                 project-root-files)))
      (cons 'transient (expand-file-name root))))

  (add-to-list 'project-find-functions 'project-try-root t))

(use-package recentf
  :ensure nil
	:defer t
	:custom (recentf-mode 1)
  :config
  (setopt recentf-auto-cleanup 'never
        recentf-max-saved-items 250))

(use-package savehist
  :ensure nil
	:defer t
	:custom (savehist-mode 1)
  :config
  (setopt enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 250
        savehist-autosave-interval nil
        savehist-additional-variables '(evil-jumps-history
                                        mark-ring global-mark-ring
                                        search-ring regexp-search-ring
                                        extended-command-history))

  (add-hook 'savehist-save-hook
            (defun savehist-unpropertize-variables-h ()
              "Remove text properties from `kill-ring' to reduce savehist cache size."
              (setq kill-ring
                    (mapcar #'substring-no-properties
                            (cl-remove-if-not #'stringp kill-ring))
                    register-alist
                    (cl-loop for (reg . item) in register-alist
                             if (stringp item)
                             collect (cons reg (substring-no-properties item))
                             else collect (cons reg item)))))

  (add-hook 'savehist-save-hook
            (defun savehist-remove-unprintable-registers-h ()
              "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
              ;; Save new value in the temp buffer savehist is running
              ;; `savehist-save-hook' in. We don't want to actually remove the
              ;; unserializable registers in the current session!
              (setq-local register-alist
                          (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package saveplace
  :ensure nil
	:custom (save-place-mode 1))

(use-package server
  :ensure nil
  :commands (server-running-p))

(use-package simple
	;; basic editing commands for emacs
  :ensure nil
	:defer t
  :config
  (setopt column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        next-error-message-highlight t
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package whitespace
  :ensure nil
	:defer t
  :hook (;(prog-mode . whitespace-mode)
         (diff-mode . whitespace-mode))
 )

(use-package winner
  :ensure nil
	:defer t
  :commands (winner-undo winner-redo)
  :init
  (setopt winner-dont-bind-my-keys t)
	:custom
	(winner-mode 1)
  :config
  (setopt winner-boring-buffers-regexp "\\*.*\\*"))

(provide 'core-config)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; core-config.el ends here
