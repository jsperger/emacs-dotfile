;;; config/setup-evil-addons.el --- Evil addons configuration -*- lexical-binding: t -*-

(use-package evil-collection
	:hook (elpaca-after-init . evil-collection-init)
  :init
	(setq evil-collection-magit-want-horizontal-movement t
        evil-collection-unimpaired-want-repeat-mode-integration t)
  (add-hook 'org-agenda-mode-hook
            (lambda () (evil-collection-unimpaired-mode -1))
            )
  )

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :config
  (setopt evil-org-key-theme '(navigation insert textobjects additional todo heading))
  )

(use-package evil-snipe
  :hook (evil-mode . evil-snipe-mode)
	(evil-snipe-mode . evil-snipe-override-mode)
  :config
  (setopt evil-snipe-spillover-scope 'whole-buffer)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-surround
	:disabled
  :hook ((text-mode prog-mode conf-mode) . evil-surround-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
  (general-def 'visual evil-surround-mode-map
    "s" 'evil-surround-region
    "S" 'evil-substitute))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-evil-addons.el ends here
