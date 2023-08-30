;;; core-config.el --- -*- lexical-binding: t; -*-

;; Forked from Tianshu Wang

;; Author: John Sperger forked from Tianshu Wang <wang@tianshu.me>

;;; Commentary:

;;; Code:

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(setq user-full-name "John Sperger"
      user-mail-address "josp@duck.com")

(defvar default-font "Fira Code")
(defvar font-size 13)
(defvar unicode-font "Noto Sans CJK SC")
(defvar unicode-scale (/ 16.0 font-size))
(defvar emoji-font "Noto Color Emoji")
(defvar symbol-font "Noto Sans Symbols")

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        frame-resize-pixelwise t
        font-size 12)

  (setq unicode-font "Noto Sans CJK SC"
        emoji-font "Apple Color Emoji"
        symbol-font "Apple Symbols"))

(defun setup-font ()
  (set-face-attribute 'default nil :font (font-spec :family default-font :size font-size))

  (when (fboundp 'set-fontset-font)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset unicode-font))
    (add-to-list 'face-font-rescale-alist `(,unicode-font . ,unicode-scale))
    (set-fontset-font t 'emoji emoji-font nil 'prepend)
    (set-fontset-font t 'symbol symbol-font nil 'prepend)))

(add-hook 'after-init-hook #'setup-font)


(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        frame-resize-pixelwise t))

(setq initial-scratch-message nil   ;; "make scratch buffer empty"
      inhibit-startup-message t)    ;; "disable splash screen"

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; change `tab-width' and `fill-column'
(setq-default tab-width 4
              fill-column 80)

;; no beep
(setq ring-bell-function 'ignore)

;; highlight current line
(global-hl-line-mode 1)
; prettify symbols
(global-prettify-symbols-mode 1)

;; Single space between sentences is more widespread than double
(setq sentence-end-double-space nil)

;; smooth scrolling
(setq scroll-conservatively 101
      scroll-margin 2)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook #'bug-reference-prog-mode)
;; enable subword-mode in prog-mode
(add-hook 'prog-mode-hook #'subword-mode)


;; scroll compilation to first error or end
(setq compilation-scroll-output 'first-error)

;; Use system trash for file deletion.
(setq delete-by-moving-to-trash t)

;; autosave each change
(setq bookmark-save-flag 1)

;; keep focus while navigating help buffers
(setq help-window-select t)

;; When emacs asks for "yes" or "no", let "y" or "n" suffice
;; (fset 'yes-or-no-p 'y-or-n-p)
(setq use-short-answers t)

;; don't load outdated compiled files.
(setq load-prefer-newer t)

;; don't save duplicates in kill-ring
(setq kill-do-not-save-duplicates t)

;; break lines after more characters
(setq word-wrap-by-category t)

(defun server-remove-kill-buffer-hook ()
  "Remove prompt if the file is opened in other clients."
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook #'server-remove-kill-buffer-hook)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(use-package autorevert
  :elpaca nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

(use-package dabbrev
  :elpaca nil
  :defer t
  :config
  (setq dabbrev-abbrev-char-regexp "[A-Za-z-_]"
        dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package desktop
  :elpaca nil
  :commands restart-emacs-without-desktop
  :init (desktop-save-mode)
  :config
  ;; inhibit no-loaded prompt
  (setq desktop-file-modtime (file-attribute-modification-time
                              (file-attributes
                               (desktop-full-file-name)))
        desktop-lazy-verbose nil
        desktop-load-locked-desktop t
        desktop-restore-eager 1
        desktop-save t)

  (dolist (param '(foreground-color background-color background-mode font cursor-color mouse-color))
    (push `(,param . :never) frameset-filter-alist))

  (defun desktop-read@inhibit-message (fn)
    "Inhibit `desktop-read' message"
    (let ((inhibit-message t))
      (funcall fn)))
  (advice-add 'desktop-read :around #'desktop-read@inhibit-message))

(use-package dired
  :elpaca nil
  :defer t
  :config
  (setq ls-lisp-use-insert-directory-program t) ; Use external ls
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls") ; Path to GNU ls
  (setq dired-listing-switches "-aBhl --group-directories-first")
  (setq dired-auto-revert-buffer t
        dired-create-destination-dirs 'always
        dired-do-revert-buffer t
        dired-dwim-target t
        dired-vc-rename-file t))

(use-package display-line-numbers
  :elpaca nil
  :hook ((text-mode prog-mode conf-mode) . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative
                display-line-numbers-width-start t))

(use-package doc-view
  :elpaca nil
  :defer t
  :config
  (setq doc-view-resolution 400))

(use-package ediff
  :elpaca nil
  :defer t
  :config
  (setq-default ediff-window-setup-function 'ediff-setup-windows-plain
                ediff-split-window-function 'split-window-horizontally
                ediff-merge-split-window-function 'split-window-horizontally))

(use-package elec-pair
  :elpaca nil
  :hook (after-init . electric-pair-mode))

(use-package files
  :elpaca nil
  :hook (before-save . delete-trailing-whitespace)
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
             size (> size (* 1024 1024 1))
             (y-or-n-p (format (concat "%s is a large file, open literally to "
                                       "avoid performance issues?")
                               filename)))
        (setq buffer-read-only t)
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
  :elpaca nil
  :hook (prog-mode . flymake-mode)
  :init (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
)

(use-package newcomment
  :elpaca nil
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
  :elpaca nil
  :defer t
  :config
  (setq project-vc-merge-submodules nil
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
  :elpaca nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 100))

(use-package savehist
  :elpaca nil
  :hook (after-init . savehist-mode)
  :config
  (setq enable-recursive-minibuffers t ; allow commands in minibuffers
        history-length 100
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
  :elpaca nil
  :hook (after-init . save-place-mode))

(use-package server
  :elpaca nil
  :commands (server-running-p)
  :hook (after-init . (lambda () (unless (server-running-p)
                              (server-start)))))

(use-package simple
  :elpaca nil
  :config
  (setq column-number-mode t
        delete-trailing-lines nil
        eval-expression-print-length nil
        eval-expression-print-level nil
        next-error-message-highlight t
        ;; save clipboard contents into kill-ring before replace them
        save-interprogram-paste-before-kill t))

(use-package whitespace
  :elpaca nil
  :hook ((prog-mode . show-trailing-whitespace)
         (diff-mode . whitespace-mode))
  :config
  (defun show-trailing-whitespace ()
    (set-face-attribute 'trailing-whitespace nil :background
                        (face-attribute 'font-lock-comment-face
                                        :foreground))
    (setq show-trailing-whitespace t)))

(use-package winner
  :elpaca nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init
  (setq winner-dont-bind-my-keys t)
  :config
  (setq winner-boring-buffers-regexp "\\*.*\\*"))

(use-package all-the-icons
  :elpaca t
  :if (display-graphic-p))


(use-package all-the-icons-dired
  :elpaca t
  :after all-the-icons
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :elpaca t
  :after all-the-icons
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(provide 'core-config)
 ;;; core-config.el ends here
