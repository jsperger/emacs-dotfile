;;; config/setup-org-node.el --- Org-node configuration -*- lexical-binding: t -*-

(use-package org-mem
  :defer
  :functions org-mem-updater
  :config
  (setopt org-mem-do-sync-with-org-id t
          org-mem-watch-dirs (list "~/obsidian/org/node")
          )
  (org-mem-updater-mode)
  )

(use-package org-node
  :after org
  :init
  (keymap-global-set "M-o n" org-node-global-prefix-map)
  (with-eval-after-load 'org
    (keymap-set org-mode-map "M-o n" org-node-org-prefix-map))
  :config
  (org-node-cache-mode)
  :general
  (tyrant-def
    "n"      (cons "node" (make-sparse-keymap))
    "nf" 'org-node-find
    "ni" 'org-node-insert-link
    "nb" 'org-node-context-dwim
    "nd" 'org-node-insert-into-related
    "ng" 'org-node-grep
    "nn" 'org-node-nodeify-entry
    "ns" 'org-node-seq-dispatch
    "nw" 'org-node-refile
    "nr" 'org-node-visit-random
    "nt" 'org-node-insert-transclusion
    "nu" 'org-node-insert-raw-link
    "ns" 'org-node-insert-transclusion-as-subtree
    )
  )

;;; setup-org-node.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
