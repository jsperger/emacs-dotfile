;;;; init.el  -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;;;;;;;;;;;;;
;; Auctex + Vertico
;;
;;

;; (use-package jsonrpc)

;;(use-package eldoc
;;  :preface
;;    (unload-feature 'eldoc t)
;;    (setq custom-delayed-init-variables '())
;;    (defvar global-eldoc-mode nil)
;;    :config
;;    (global-eldoc-mode))

(use-package auctex :defer t
  :ensure (:pre-build (("./autogen.sh")
                       ("./configure"
                        "--without-texmf-dir"
                        "--with-packagelispdir=./"
                        "--with-packagedatadir=./")
                       ("make"))
                      :build (:not elpaca--compile-info) ;; Make will take care of this step
                      :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                      :version (lambda (_) (require 'tex-site) AUCTeX-version)))
(elpaca-wait)

;;;;;;;;;;;;;;
;; Key Bindings
;;
;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.

(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'core-keybinds)
(elpaca-wait)


(require 'core-packages)
(require 'core-config)
(require 'core-funcs)
(elpaca-wait)

(require 'editor-completion)
(require 'editor-fonts-themes)
(require 'editor-misc)
(require 'editor-ui)
(elpaca-wait)
(require 'editor-vc)

(require 'lang-misc)
(require 'lang-tex)
(require 'lang-org)
(require 'lang-web)
(require 'lang-emacs-lisp)
(require 'lang-rust)
(require 'lang-r)

;; After-init hooks + custom
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(provide 'init)
;;; init.el ends here
