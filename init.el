;;; init.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ===================== Elpaca bootstrap install ====================
;; https://github.com/progfolio/elpaca?tab=readme-ov-file#installer
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; ======================== END: elpaca bootstrap ========================

;; Elpaca use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; ========================= Debugging assistant =========================


(defvar my-debug-mode nil
  "Toggle debugging messages. Set to t to enable, nil to disable.")


;; ==================== (Historical) Problem packages ====================

;; 2025-02-22 Disabled eldoc, use built-in
;; Reason: Error. Not worth fixing at the moment because I was only using elpa versions of
;; eldoc and jsonrpc because of eglot  
;; Error:  something about incf not defined. I'm guessing it should use cl-incf;
;; maybe it should intelligently choose between cl-incf and ts-incf I'm not
;; sure.
;; Eldoc workaround
;; https://github.com/progfolio/elpaca/issues/398
;;(unload-feature 'eldoc t) ;; Unload built-in eldoc
;;(setq custom-delayed-init-variables '())
;;(defvar global-eldoc-mode nil)
;; (elpaca eldoc
;;   (require 'eldoc)
;;   (global-eldoc-mode) ;; This is usually enabled by default by Emacs
;;   )

(use-package jsonrpc :ensure (:wait t) )

(use-package no-littering :ensure (:wait t))

(use-package track-changes)

;; use-package version of eldoc workaround
;; (use-package eldoc
;;   :preface
;;   (unload-feature 'eldoc t)
;;   (setq custom-delayed-init-variables '())
;;   (defvar global-eldoc-mode nil)
;;   :config
;;   (global-eldoc-mode))

(use-package dash)

(use-package queue)

(use-package plz)

;; ============== Define constants for use throughout config =============

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; ============================== Load path ==============================
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.

(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'core-keybinds)
(require 'editor-completion)

(require 'evil-core)
(require 'evil-addons)

(require 'builtin-packages)
(require 'core-config)
(require 'core-funcs)
(require 'core-packages)
(require 'core-treesit)

(require 'completion-actions)
(require 'completion-backends)
(require 'completion-display)
(require 'completion-movement)
(require 'completion-snippets)

;; (require 'editor-lsp)


(require 'editor-icons)
(require 'editor-font-locking)
(require 'editor-fonts-themes)
(require 'editor-misc)
(require 'editor-projects)
(require 'editor-ui)
(require 'editor-vc)

(require 'tools-denote)
(require 'tools-notes)
(require 'tools-reading)
(require 'tools-media)
(require 'tools-programming)
(require 'tools-search)
(require 'tools-web)
(require 'tools-writing)

(require 'lang-data-formats)
(require 'lang-lisp)
(require 'lang-llm)
(require 'lang-markdown)
(require 'lang-org)
(require 'lang-python)
(require 'lang-r)
(require 'lang-rust)
(require 'lang-tex)
(require 'lang-web)

(require 'tools-lsp-bridge)

;; ====================== After-init hooks + custom ======================
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(provide 'init)

;;; DANGER ZONE
;; Respect the no-byte-compile local variable
;; !Config files should not be byte compiled!
;; Emacs will compile package lisp JIT (or when building for packages in base
;; emacs when building emacs with the -native-compile=aot flag)
;;;

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; init.el ends here
