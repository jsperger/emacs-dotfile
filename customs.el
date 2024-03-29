;; These are here because for some reason the hooks don't seem to work
;; when entering LaTeX-mode unless these files are loaded first
;; TODO: Fix latex hook weirdness
(load "latex.el" nil nil t)
(load "preview-latex.el" nil nil t)

;; (message "Checkpoint: %s" "latex el")

(setq-default TeX-master nil
              TeX-command "LaTeX"
              TeX-engine 'luatex
  		      preview-scale 1.1
              preview-scale-function
              (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))

(setq preview-auto-cache-preamble nil
 	  TeX-parse-self t
      TeX-save-query nil
      TeX-source-correlate-start-server t
      LaTeX-fill-break-at-separators nil)

(setq bibtex-file-path "~/obsidian/"
      bibtex-files '("obsidian-biblatex.bib")
      bibtex-align-at-equal-sign t
      bibtex-dialect 'bibtex)

(setq citar-at-point-function 'embark-act
      citar-bibliography bib-file-location
      citar-library-paths `(,(concat bibtex-file-path "files/"))
      citar-file-open-functions '(("html" . citar-file-open-external)
                                  ("pdf" . citar-file-open-external)
                                  (t . find-file)))

;; (message "Checkpoint: %s" "set q")


;; After-init hooks packages
(electric-pair-mode)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(winner-mode)
(which-key-mode)
;; (message "Checkpoint: %s" "hooks: before evil")

(evil-mode)
(evil-collection-init)
(evil-owl-mode)
(evil-snipe-mode)
(doom-modeline-mode)
;; (message "Checkpoint: %s" "hooks: after evil")
(shackle-mode)
(global-hl-todo-mode)
(global-diff-hl-mode)
(global-auto-revert-mode)
;; (message "Checkpoint: %s" "hooks: before auto complete")
(vertico-mode)
(marginalia-mode)
(global-corfu-mode)
(prescient-persist-mode)
;; (message "Checkpoint: %s" "hooks: after auto complete")
(gcmh-mode)
(popper-mode)
(popper-echo-mode)
(global-treesit-auto-mode)
(undohist-initialize)
(winum-mode)
;; (message "Checkpoint: %s" "hooks: end of hooks")


(lambda () (unless (server-running-p)
             (server-start)))
;; (message "Checkpoint: %s" "hooks: before custom set variables")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("418868a2c15787fa29a308b7cbb91458a7bbeb81119795c6954da917b8f1d56f"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "badd1a5e20bd0c29f4fe863f3b480992c65ef1fa63951f59aa5d6b129a3f9c4c"
     "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     "4fdbed4aa8bcb199d7f6a643886bac51178d1705b9b354ef3dd82d4ec48072d2"
     "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a"
     "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307"
     "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2"
     "833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633"
     "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078"
     "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
     "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294"
     "37b6695bae243145fa2dfb41440c204cd22833c25cd1993b0f258905b9e65577"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d"
     "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(golden-ratio-mode nil)
 '(highlight-parentheses-colors '("#009c8f" "#ad8900" "#0072d4" "#8762c6" "#489100"))
 '(package-selected-packages '(doom-themes solarized-theme monokai-theme eglot))
 '(safe-local-variable-values
   '((TeX-master . "../sperger-dissertation") (TeX-master . t))))

;; (message "Checkpoint: %s" "hooks: after custom set variables")

(setup-font)

;; (message "Checkpoint: %s" "hooks: after load theme")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
