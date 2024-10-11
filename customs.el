;;; customs.el --- Summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when my-debug-mode
  (message "Checkpoint: %s" "latex el"))
(add-to-list
 'treesit-language-source-alist
 '(r "https://github.com/r-lib/tree-sitter-r" "next"))

(setq-default TeX-master nil
              TeX-command "LaTeX"
              TeX-engine 'luatex
  	      preview-scale 1.0
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

(when my-debug-mode (message "Checkpoint: %s" "set q"))


;;; After-init hooks packages

;; general emacs settings
(pixel-scroll-precision-mode)
(electric-pair-mode)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(winner-mode)
(which-key-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: before evil"))
;; evil settings
(evil-collection-init)
(evil-owl-mode)
(evil-snipe-mode)
;;(doom-modeline-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after evil"))
;; (shackle-mode)
(global-hl-todo-mode)
(global-diff-hl-mode)
(global-auto-revert-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: before completion modes"))
(vertico-mode)
(marginalia-mode)
(global-corfu-mode)
(prescient-persist-mode)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after auto complete"))
(gcmh-mode)
(popper-mode)
(popper-echo-mode)
(global-treesit-auto-mode)
(undohist-initialize)
(winum-mode)

;; (mindstream-mode)

(setq golden-ratio-mode nil)

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
 '(byte-compile-delete-errors t)
 '(custom-safe-themes
   '("de4382be014e101c135c3d7c638a488e5cb843fa61554008f08e7bec1bc1abb6"
     "f90edafccd777cd3df7b88bb5bb72931d8cbd5ef92423fbca0c3bc344e03f1e2"
     "48bf9307e1468f596789717043cc5a332ce30310cbb747ae0d2ce736ff23538f"
     "0279c1b81b569e46a4ee8e87e5e381568bf931257287191fd091f2948f7e2e8e"
     "077f3e88cee64127903484d97a54dbecc40f4f6b4b7e266b1023991d2c31d25a"
     "6b91ddbc12b0fc9f43fb9e31be8b76b16e387312232324c48e8c666418fe643c"
     "7c7026a406042e060bce2b56c77d715c3a4e608c31579d336cb825b09e60e827"
     "7342266ffff707cc104313c9153342e44a47a9f22ed7157e4893aac74091ad27"
     "28d91e89883df5dd87c7da27f6a15e8e41bb92a0c1341eaa9f397ed67b10b21d"
     "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3"
     "6b839977baf10a65d9d7aed6076712aa2c97145f45abfa3bad1de9d85bc62a0e"
     "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a"
     "0664443859604a53d2257701f034459edf8eab3cc2be50c7d8ae36740fe35578"
     "b5b6396361db4bee9b0c0d7ea678b96b3b55e4217c610038c8d289eb05c426ef"
     "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18"
     "159a29ab0ec5ba4e2811eddd9756aa779b23467723dcbdd223929fbf2dde8954"
     "ca47f7b222eb380e3035fb594d52032acd89dae0a49eac3da722a5cd3f485e3b"
     "063095cf0fe6ed3990546ec77e5d3798a1e2ad5043350063467a71c69518bb24"
     "263e3a9286c7ab0c4f57f5d537033c8a5943e69d142e747723181ab9b12a5855"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "caf6d2a2d0536871f3e0c3427d5f7cb6b6474f65ecf1d825ed74d043f913c277"
     "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
     "d77d6ba33442dd3121b44e20af28f1fae8eeda413b2c3d3b9f1315fbda021992"
     "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3"
     "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307"
     "23841f54afd15fe850da8b37f2bb5813a873f53ed528024b0becb486ad8b01b3" default))
 '(package-native-compile t)
 '(safe-local-variable-values '((TeX-master . t)))
 '(temporary-file-directory "~/.tmp")
 )



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
(add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
(load-theme 'doom-nord)

(when my-debug-mode (message "Checkpoint: %s" "hooks: after load theme"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; customs.el ends here
