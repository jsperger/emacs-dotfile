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


;; (lambda () (unless (server-running-p)
;;              (server-start)))
(when my-debug-mode (message "Checkpoint: %s" "hooks: before custom set variables"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-indent-close-delimiters "]")
 '(TeX-indent-open-delimiters "[")
 '(custom-safe-themes
	 '("59c36051a521e3ea68dc530ded1c7be169cd19e8873b7994bfc02a216041bf3b"
		 "4c16a8be2f20a68f0b63979722676a176c4f77e2216cc8fe0ea200f597ceb22e"
		 "2b0fcc7cc9be4c09ec5c75405260a85e41691abb1ee28d29fcd5521e4fca575b"
		 "7fea145741b3ca719ae45e6533ad1f49b2a43bf199d9afaee5b6135fd9e6f9b8"
		 "53b6ea82cb4aa7547d3667b5a007638ff4a2bf877883e440ff3acd82e7bbdabc"
		 "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
		 "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
		 "cee5c56dc8b95b345bfe1c88d82d48f89e0f23008b0c2154ef452b2ce348da37"
		 "046a2b81d13afddae309930ef85d458c4f5d278a69448e5a5261a5c78598e012"
		 "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
		 "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
		 "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c"
		 "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307"
		 "94bed81ca0be98f58d2cfa2676221c492c8fd5f76b40abd9d73ac00c0d0c9711"
		 "ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
		 "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
		 "ffa78fc746f85d1c88a2d1691b1e37d21832e9a44a0eeee114a00816eabcdaf9"
		 "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
		 "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" default))
 '(indent-tabs-mode t)
 '(large-file-warning-threshold 100000000)
 '(markdown-list-indent-width 2)
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
(load-theme 'doom-monokai-pro)

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
