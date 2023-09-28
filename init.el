;;;; init.el  -*- lexical-binding: t; -*-


(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
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
;; Key Bindings
;;
;; Load path
;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'core-packages)
(require 'core-config)
(require 'core-funcs)
(require 'core-keybinds)
(elpaca-wait)

(require 'editor-ui)
(require 'editor-completion)
(elpaca-wait)
(require 'editor-misc)

(require 'lang-emacs-lisp)
(require 'lang-tex)
(require 'lang-misc)

;; Load markdown mode (in lang-tex) before forge or get a warning.
;; TODO: figure out the right way to deal with it instead of swapping order around.
 (require 'editor-vc)

;; Instead: change definitions in core-keybinds.el so that lowercase switch and follows and uppercase remains
;; (defadvice split-window (after split-window-after activate)
;;  (select-window (get-lru-window)))

(setq-default custom-file (no-littering-expand-var-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;;;;;;;;;;;;;;;;
;; Theme
;;

(use-package solarized-theme
  :elpaca t
)


;;  (setq solarized-high-contrast-mode-line t)

(elpaca-wait)
;; After custom because custom sets the trusted themes
;;(load-theme 'solarized-light-high-contrast)

(load-theme 'doom-gruvbox)

;; (use-package nano-theme
;;   :defer t)
;; (elpaca-wait)

;; (
;; load-theme 'nano t)
(provide 'init)
;;; init.el ends here
