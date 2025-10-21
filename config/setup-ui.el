;;; config/setup-ui.el --- UI configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package bicycle
  :hook ((prog-mode . outline-minor-mode)
         (prog-mode . hs-minor-mode))
  :general (tyrant-def
             "t TAB" 'bicycle-cycle
             "t <backtab>" 'bicycle-cycle-global
             )
  )

(use-package olivetti
  :hook ((text-mode prog-mode org-mode) . olivetti-mode)
  :config (setopt olivetti-style 'fancy
                  olivetti-body-width 0.7
                  olivetti-minimum-body-width 80
                  olivetti-recall-visual-line-mode-entry-state t
                  )
  )


(use-package shackle
  :config
  (setopt shackle-mode t
          shackle-default-size 0.25
          shackle-rules
          `((help-mode :select t :align right :size 0.4)
            (helpful-mode :select t :align right :size 0.4)
            (inferior-ess-r-mode :select t :alight right :size 0.5)
            (ess-r-info-mode :select t :align right :size 0.4)
            ("*Messages*"                    :select t :align t)
            ("*eldoc*"                       :align t)
            (special-mode                    :align t)
            (process-menu-mode               :align t)
            (compilation-mode                :align t)
            (flymake-diagnostics-buffer-mode :align t)
            ("*Shell Command Output*"        :align t :size 2)
            ("*Async Shell Command*"         :align t :size 2)
            (lsp-bridge-ref-mode             :align t :size 0.2)
            ("*elpaca-info*"                 :align t)
            )
          )
  )

(use-package spacious-padding
  :hook elpaca-after-init
  :config (setopt spacious-padding-subtle-frame-lines t
                  spacious-padding-widths '(:internal-border-width 10
                                            :header-line-width 4
                                            :mode-line-width 4
                                            :tab-width 4
                                            :right-divider-width 0
                                            :scroll-bar-width 4
                                            :fringe-width 8)
                  )
  )

(use-package popper
  :config
  (setopt popper-display-control nil
          popper-reference-buffers
          '("\*Messages\*"
            "Output\*$"
            "\*Async Shell Command\*"
            "\*eldoc\*"
            "^\*EGLOT"
            "^\*lsp-bridge.*"
            "\*lsp-bridge-ref\*"
            compilation-mode
            ess-r-help-mode
            help-mode
            helpful-mode
            process-menu-mode
            special-mode
            flymake-diagnostics-buffer-mode))
  :general
  (tyrant-def
    ";" '("pop toggle" . popper-toggle)
    ":" '("pop kill" . popper-kill-latest-popup))
  :custom
  (popper-mode 1)
  (popper-echo-mode 1)
  )

(use-package visual-fill-column
  :hook (elpaca-after-init . global-visual-fill-column-mode)
  :config (setopt visual-fill-column-center-text t
                  visual-fill-column-extra-text-width '(4 . 4)
                  fill-column 80
                  )
  (add-hook 'visual-line-mode-hook #'visual-fill-column-for-vline)
  )
;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; setup-ui.el ends here
