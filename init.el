;;; init.el --- Initialize configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; [[file:config.org::init][init]]
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

(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  )

(elpaca-wait) ; Block until current queue processed.

;; ============== Define constants for use throughout config =============

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))


(defconst my-debug-mode nil
  "Toggle debugging messages. Set to t to enable, nil to disable.")
;; init ends here

;; [[file:config.org::no-littering][no-littering]]
;; ============== Packages that change core functionality =============

(use-package no-littering :ensure (:wait t))

;; no-littering ends here

;; [[file:config.org::load configuration files][load configuration files]]
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

;; (require 'core-packages)
;; (require 'core-treesit)

;; (require 'completion-actions)
;; (require 'completion-backends)
;; (require 'completion-display)
;; (require 'completion-movement)
(require 'completion-snippets)

;; (require 'editor-icons)
;; (require 'editor-fonts)
;; (require 'editor-font-locking)
;; (require 'editor-themes)
;; (require 'editor-misc)
;; (require 'editor-projects)
;; (require 'editor-ui)
(require 'editor-vc)

;; (require 'ui-modeline)

;; (require 'tools-denote)
;; (require 'tools-diagramming)
(require 'tools-llm)
;; (require 'tools-media)
;; (require 'tools-notes)
;; (require 'tools-programming)
;; (require 'tools-reading)
(require 'tools-search)
;; (require 'tools-web)
;; (require 'tools-writing)

;; (require 'lang-bib)
;; (require 'lang-data-formats)
;; (require 'lang-go)
;; (require 'lang-lisp)
;; (require 'lang-markdown)
;; (require 'lang-org)
;; (require 'lang-python)
;; (require 'lang-r)
;; (require 'lang-rust)
;; (require 'lang-tex)
;; (require 'lang-web)
;; (require 'lang-functions)

(require 'tools-lsp-bridge)
;; (require 'tools-org-capture)
;; (require 'org-node)
;; ;; (require 'tools-eaf)


;;;; --- Load use-package configurations manually ---
;; (let ((config-dir (expand-file-name "config" user-emacs-directory)))
;;   (dolist (file
;;            '(;; Core setup (load these first)
;;              "setup-org.el"
;;              "setup-lsp.el"
;;              ;; Major packages and modes
;;              "setup-lang-r.el"
;;              "setup-lang-rust.el"
;;              "setup-tools-notes.el"))
;;     (load-file (expand-file-name file config-dir))))

;; ====================== After-init hooks + custom ======================
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))


(provide 'init)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; init.el ends here
