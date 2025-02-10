;;; evil-addons.el --- summary -*- lexical-binding: t -*-

;; Author: John Sperger
;;; Commentary:

;; Refactoring evil

;;; Code:
(use-package evil-collection
  :init
  (add-hook 'org-agenda-mode-hook
            (lambda () (evil-collection-unimpaired-mode -1)))
	:hook (elpaca-after-init . evil-collection-init))

(use-package evil-owl
  :init
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (setopt evil-owl-idle-delay 0.5)
	:hook (elpaca-after-init . evil-owl-mode))

(use-package evil-snipe
  :hook ((evil-snipe-mode . evil-snipe-override-mode))
  :config
  (setopt evil-snipe-spillover-scope 'whole-buffer)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-surround
  :hook ((text-mode prog-mode conf-mode) . evil-surround-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  ;; `s' for surround instead of `subtitute'
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute))

;; TODO: make \math*{} commands e.g. \mathbf{} use
;; \sym*{} instead for unicode-math compatibility
(use-package evil-tex
	:hook (LaTeX-mode . evil-tex-mode))

(provide 'evil-addons)
;;; evil-addons.el ends here
