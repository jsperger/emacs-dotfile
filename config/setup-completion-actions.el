;;; config/setup-completion-actions.el --- Completion actions -*- lexical-binding: t -*-

(use-package avy
	:general
	(tyrant-def
		"Ja" 'avy-goto-char
		"Jg" 'avy-goto-char-timer
		"Jl" 'avy-goto-line
		"Jw" 'evil-avy-goto-word-1))

(use-package embark
  ;; :init
  ;; (with-eval-after-load 'avy
  ;;   (defun avy-action-embark (pt)
  ;;     (unwind-protect
  ;;         (save-excursion
  ;;           (goto-char pt)
  ;;           (embark-act))
  ;;       (select-window
  ;;        (cdr (ring-ref avy-ring 0))))
  ;;     t)
  ;;   (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  ;; :config
  ;; (with-eval-after-load 'vertico
  ;;   (defun embark-vertico-indicator ()
  ;;     (let ((fr face-remapping-alist))
  ;;       (lambda (&optional keymap _targets prefix)
  ;;         (when (bound-and-true-p vertico--input)
  ;;           (setq-local face-remapping-alist
  ;;                       (if keymap
  ;;                           (cons '(vertico-current . embark-target) fr)
  ;;                         fr))))))

  ;;   (add-to-list 'embark-indicators #'embark-vertico-indicator))
  :config
  (defun bibtex-key-embark ()
  (save-excursion
    (bibtex-beginning-of-entry)
    (when (looking-at bibtex-entry-maybe-empty-head)
      (cons 'bibtex-key
            (bibtex-key-in-head)))))


  :general
  (:keymaps '(global normal)
            "C-." 'embark-act
            "M-." 'embark-dwim))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy-embark-collect
	:disabled)

(use-package consult-dir
	:general
    (vertico-map "C-d"   'consult-dir
                 "C-j"   'consult-dir-jump-file))

(use-package consult-gh-forge
	:disabled
	:after consult-gh
	:custom
	(consult-gh-forge-mode +1))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-completion-actions.el ends here
