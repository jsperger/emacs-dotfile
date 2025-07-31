;;; init.el --- Initialize configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; [[file:its-lit.org::init elpaca][init elpaca]]
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
;; init elpaca ends here

;; [[file:its-lit.org::constant flags][constant flags]]
;; ============== Define constants for use throughout config =============
(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-ANDROID (eq system-type 'android))

(defconst my-debug-mode nil
  "Toggle debugging messages. Set to t to enable, nil to disable.")
;; constant flags ends here

;; [[file:its-lit.org::no-littering][no-littering]]
;; ============== Packages that change core functionality =============
(use-package no-littering :ensure (:wait t))
;; no-littering ends here

;; [[file:its-lit.org::general declaration][general declaration]]
(use-package general
  :ensure (:wait t)
  :demand t
  :config  (setopt general-emit-autoloads nil)
  (general-define-key
   :states '(normal insert motion emacs)
   :keymaps 'override
   :prefix-map 'tyrant-map
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (general-create-definer tyrant-def :keymaps 'tyrant-map)
  (tyrant-def "" nil)

  (general-create-definer despot-def
    :states '(normal insert motion emacs)
    :keymaps 'override
    :major-modes t
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m")
  (despot-def "" nil)

  (general-def universal-argument-map
    "SPC u" 'universal-argument-more)

  )
;; general declaration ends here

;; [[file:its-lit.org::benchmark init declaration][benchmark init declaration]]
(use-package benchmark-init
  :config (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate)
  )
;; benchmark init declaration ends here

;; [[file:its-lit.org::load configuration files][load configuration files]]
;;;; =========== Load use-package declarations and configuration =======

;; Declarations to executed immediately. I.e. those with elpaca
;; :ensure (:wait t)
;; :demand t
(load-file (expand-file-name "config/setup-evil.el" user-emacs-directory))

;;;; =========================== Load lisp defuns ======================
;; Add personal `lisp` directory to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; --- Load personal library files ---
(require 'my-core-functions)
(require 'my-core-helpers)
(require 'my-editor-helpers)
(require 'my-keybinding-helpers)
(require 'my-lang-helpers)
(require 'my-lsp-bridge-helpers)
(require 'my-org-helpers)
(require 'my-theme-helpers)
(require 'my-vc-helpers)

;; General delcarations that can be handled by elpaca/use-package queueing
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (dolist (file
           '(
             ;; "setup-bib.el"
             "configure-base-and-built-in.el"
             "configure-keybinding.el"
             "configure-org.el"
             "configure-theming.el"
             "configure-utilities.el"
             "configure-prog-langs.el"
             "configure-prog-tools.el"
             ;; "setup-casual.el"
             ;;              ;; "setup-completion-actions.el"
             "setup-completion-backends.el"
             "setup-completion-display.el"
             "setup-completion.el"
             "setup-consult.el"
             ;; "setup-data-formats.el"
             ;; "setup-denote.el"
             ;; "setup-diagramming.el"
             ;; "setup-eaf.el"
             ;; "setup-emacs.el"
             ;;              ;; "setup-email.el"
             "setup-evil-addons.el"
             "setup-font-locking.el"
             ;;              ;; "setup-go.el"
             ;;              ;; "setup-gopher.el"
             ;;              ;; "setup-icons.el"
             ;;              ;; "setup-lisp.el"
             "setup-llm.el"
             "setup-markdown.el"
             ;;              ;; "setup-media.el"
             "setup-modeline.el"
             ;;              ;; "setup-notes.el"
             ;;              ;; "setup-programming.el"
             ;; "setup-projects.el"
             "setup-reading.el"
             ;; "setup-treesit.el"
             "setup-ui.el"
             "setup-vc.el"
             ;; "setup-web.el"
             ;; "setup-writing.el"
             )
           )
    (load-file (expand-file-name file config-dir))
    )
  )
;; load configuration files ends here

;; [[file:its-lit.org::#load-custom-file-and-run-after-init-hooks][Load custom file and run after-init hooks:1]]
;;;; =============================== Customs ===============================
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))
;; Load custom file and run after-init hooks:1 ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; init.el ends here
