;;; customs.el --- Customization-set options -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(when my-debug-mode
  (message "Checkpoint: %s" "latex el" (file-name-base)))

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
 '(indent-tabs-mode nil)
 '(large-file-warning-threshold 100000000)
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
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook #'my/org-update-subtree-modification-info)
     (eval add-hook 'before-save-hook #'org-make-toc nil t)
     (jinx-local-words . "engine-specific model-specific")
     (org-list-indent-offset . 2) (TeX-master . t)))
 '(standard-indent 2)
 '(trusted-content '("~/.emacs.d/" "~/projects/" "~/code/best-analysis")))



(when my-debug-mode (message "Checkpoint: %s" "hooks: after custom set variables"))


;; Fonts and Themes
;;
;;

;; (setup-font)
;; Set the last preset or fall back to desired style from `fontaine-presets'
;; (the `regular' in this case).
;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 't))

;; For persisting settings
;; (fontaine-mode 1)
;; For persisting font after loading theme This might be redundant with the above.
;; (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
;; (load-theme 'ef-dream)
;; (load-theme 'doom-nord)
;; (load-theme 'doom-monokai-pro)
;; (load-theme 'doom-gruvbox)


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
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; customs.el ends here
