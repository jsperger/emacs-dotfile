;;; customs.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when my-debug-mode
  (message "Checkpoint: %s" "latex el"))
;; Get shell path variables if running as a daemon

(when (daemonp)
  (exec-path-from-shell-initialize))


;; Not working.
;; (defun my-adjust-visual-fill-column-for-variable-pitch ()
;;   "Adjust visual fill column for variable pitch mode."
;;   (when variable-pitch-mode
;;     ;; Set a different visual fill column for variable pitch mode
;;     (setq visual-fill-column-width 60)
;; ))

;; (defun my-disable-visual-fill-column-for-variable-pitch ()
;;   "Disable visual fill column adjustments for variable pitch mode."
;;   (when (and variable-pitch-mode
;;              (bound-and-true-p visual-fill-column-mode))
;;     ;; Disable visual fill column mode when exiting variable pitch
;;     (setq visual-fill-column-width nil)))

;; (add-hook 'variable-pitch-mode-hook #'my-adjust-visual-fill-column-for-variable-pitch)
;; (add-hook 'text-scale-mode-hook 'my-disable-visual-fill-column-for-variable-pitch)
;; (setq-default TeX-master nil
;;               TeX-command "LaTeX"
;;               TeX-engine 'luatex
;;   	      preview-scale 1.0
;;               preview-scale-function
;;               (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))

;; (setq preview-auto-cache-preamble nil
;;       TeX-parse-self t
;;       TeX-save-query nil
;;       TeX-source-correlate-start-server t
;;       LaTeX-fill-break-at-separators nil)

;; (setq bibtex-file-path "~/obsidian/"
;;       bibtex-files '("obsidian-biblatex.bib")
;;       bibtex-align-at-equal-sign t
;;       bibtex-dialect 'bibtex)

;; (setopt citar-at-point-function 'embark-act
;;       citar-bibliography bib-file-location
;;       citar-library-paths `(,(concat bibtex-file-path "files/"))
;;       citar-file-open-functions '(("html" . citar-file-open-external)
;;                                   ("pdf" . citar-file-open-external)
;;                                   (t . find-file)))

(when my-debug-mode (message "Checkpoint: %s" "set q"))


;;; After-init hooks packages

;; general emacs settings

(when my-debug-mode (message "Checkpoint: %s" "hooks: before evil"))
;; evil settings
;; (evil-collection-init)
;; (evil-owl-mode)
;; (evil-snipe-mode)
;;(doom-modeline-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after evil"))
;; (shackle-mode)
;; (global-hl-todo-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: before completion modes"))
(vertico-mode)
(marginalia-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after auto complete"))
(popper-mode)
(popper-echo-mode)
(winum-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: end of hooks"))


(lambda () (unless (server-running-p)
              (server-start)))
(when my-debug-mode (message "Checkpoint: %s" "hooks: before custom set variables"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-indent-close-delimiters "]")
 '(TeX-indent-open-delimiters "[")
 '(auth-source-debug t)
 '(banner-comment-width 72)
 '(custom-safe-themes t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
 '(ess-ido-flex-matching nil)
 '(ess-use-R-completion nil)
 '(ess-use-company nil)
 '(ess-use-flymake nil)
 '(ess-use-ido nil nil (ido))
 '(large-file-warning-threshold 100000000)
 '(logos-olivetti t)
 '(magit-wip-after-apply-mode t)
 '(magit-wip-after-save-mode t)
 '(magit-wip-before-change-mode t)
 '(magit-wip-initial-backup-mode t)
 '(magit-wip-merge-branch t)
 '(magit-wip-mode t)
 '(markdown-list-indent-width 2)
 '(olivetti-style 'fancy)
 '(org-edit-src-content-indentation 0)
 '(org-make-toc-insert-custom-ids t)
 '(package-native-compile t)
 '(safe-local-variable-values '((TeX-master . t)))
 '(standard-indent 2)
 '(trusted-content '("~/.emacs.d/" "~/projects/")))



(when my-debug-mode (message "Checkpoint: %s" "hooks: after custom set variables"))


;; Fonts and Themes
;;
;;

;; (setup-font)
;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
(fontaine-set-preset (or (fontaine-restore-latest-preset) 't))

;; For persisting settings
(fontaine-mode 1)
;; For persisting font after loading theme This might be redundant with the above.
;; (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
;; (load-theme 'ef-dream)
;; (load-theme 'doom-nord)
;; (load-theme 'doom-monokai-pro)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after load theme"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((t (:underline nil :inherit default))))
 '(flymake-note ((t (:underline nil :inherit default))))
 '(flymake-warning ((t (:underline nil :inherit default)))))

(when IS-MAC (toggle-frame-fullscreen))

(provide 'customs)

;;; Local Variables:
;;; no-byte-compile: t
;;; no-native-compile: t
;;; no-update-autoloads: t
;;; End:
;;; customs.el ends here
