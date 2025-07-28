;;; customs.el --- Customization-set options -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(when my-debug-mode
  (message "Checkpoint: %s" "latex el" (file-name-base)))


;; If server package hasn't been loaded then server-running-p won't be defined
;; Problem: if a server is running and can't connect this will error
;; Is that really a problem that should be guarded against or should fixing it when it comes up be encouraged?
;;(unless (and (fboundp 'server-running-p)
;;             (server-running-p))
;;  (server-start))

(when my-debug-mode (message "Checkpoint: %s" "hooks: before custom set variables"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-indent-close-delimiters "]")
 '(TeX-indent-open-delimiters "[")
 '(acm-backend-lsp-show-progress t)
 '(auth-source-debug t)
 '(custom-enabled-themes '(modus-operandi-tinted))
 '(custom-safe-themes t)
 '(eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
 '(ess-ido-flex-matching nil)
 '(ess-style 'OWN)
 '(ess-use-R-completion nil)
 '(ess-use-company nil)
 '(ess-use-flymake nil)
 '(ess-use-ido nil)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold 100000000)
 '(markdown-list-indent-width 2)
 '(mouse-wheel-progressive-speed nil)
 '(next-error-message-highlight t)
 '(org-cite-export-processors '((latex biblatex nil nil) (t csl nil nil)))
 '(org-edit-src-content-indentation 0)
 '(org-make-toc-insert-custom-ids t)
 '(package-native-compile t)
 '(safe-local-variable-values
   '((eval auto-fill-mode t)
     (eval add-hook 'before-save-hook #'my/org-update-subtree-modification-info)
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
;; (load-theme 'modus-operandi-tinted)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after load theme"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((t (:underline nil :inherit default))))
 '(flymake-note ((t (:underline nil :inherit default))))
 '(flymake-warning ((t (:underline nil :inherit default)))))

;; (when IS-MAC (toggle-frame-fullscreen))

(provide 'customs)
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; customs.el ends here
