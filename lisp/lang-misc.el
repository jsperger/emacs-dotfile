;;; lang-misc.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023  Tianshu Wang, (C) 2023-2024  John Sperger

;; Author: Tianshu Wang, John Sperger

;;; Commentary:

;;; Code:

;;; Miscellaneous modes

(use-package markdown-mode
  :init (setq markdown-command "multimarkdown")
  :mode ("README\\.md\\'" . gfm-mode)
  :config
  (setopt markdown-fontify-code-blocks-natively t)

  (despot-def markdown-mode-map
    "RET"   'markdown-do
    ;; rebind this so terminal users can use it
    "M-RET" 'markdown-insert-list-item
    "{"     'markdown-backward-paragraph
    "}"     'markdown-forward-paragraph
    "]"     'markdown-complete
    ">"     'markdown-indent-region
    "<"     'markdown-outdent-region
    "-"     'markdown-insert-hr
    "c"     (cons "command" (make-sparse-keymap))
    "c]"    'markdown-complete-buffer
    "cc"    'markdown-check-refs
    "ce"    'markdown-export
    "cm"    'markdown-other-window
    "cn"    'markdown-cleanup-list-numbers
    "co"    'markdown-open
    "cp"    'markdown-preview
    "cv"    'markdown-export-and-preview
    "cw"    'markdown-kill-ring-save
    "f"     'markdown-follow-thing-at-point
    "h"     (cons "header" (make-sparse-keymap))
    "hi"    'markdown-insert-header-dwim
    "hI"    'markdown-insert-header-setext-dwim
    "h1"    'markdown-insert-header-atx-1
    "h2"    'markdown-insert-header-atx-2
    "h3"    'markdown-insert-header-atx-3
    "h4"    'markdown-insert-header-atx-4
    "h5"    'markdown-insert-header-atx-5
    "h6"    'markdown-insert-header-atx-6
    "h!"    'markdown-insert-header-setext-1
    "h@"    'markdown-insert-header-setext-2
    "i"     (cons "insert" (make-sparse-keymap))
    "if"    'markdown-insert-footnote
    "ii"    'markdown-insert-image
    "il"    'markdown-insert-link
    "iw"    'markdown-insert-wiki-link
    "iu"    'markdown-insert-uri
    "k"     'markdown-kill-thing-at-point
    "N"     'markdown-next-link
    "l"     (cons "lists" (make-sparse-keymap))
    "li"    'markdown-insert-list-item
    "o" (cons "obsidian" (make-sparse-keymap))
    "oo" 'obsidian-follow-link-at-point
    "ob" 'obsidian-backlink-jump
    "ol" 'obsidian-insert-link
    "ow" 'obsidian-insert-wikilink
    "oc" 'obsidian-capture
    "oj" 'obsidian-jump
    "os" 'obsidian-search
    "ot" 'obsidian-tag-find
    "om" 'obsidian-move-file
    "P"     'markdown-previous-link
    "t"     (cons "table" (make-sparse-keymap))
    "tp"    'markdown-table-move-row-up
    "tn"    'markdown-table-move-row-down
    "tf"    'markdown-table-move-column-right
    "tb"    'markdown-table-move-column-left
    "tr"    'markdown-table-insert-row
    "tR"    'markdown-table-delete-row
    "tc"    'markdown-table-insert-column
    "tC"    'markdown-table-delete-column
    "ts"    'markdown-table-sort-lines
    "td"    'markdown-table-convert-region
    "tt"    'markdown-table-transpose
    "T"     (cons "toggles" (make-sparse-keymap))
    "Ti"    'markdown-toggle-inline-images
    "Tl"    'markdown-toggle-url-hiding
    "Tm"    'markdown-toggle-markup-hiding
    "Tt"    'markdown-toggle-gfm-checkbox
    "Tw"    'markdown-toggle-wiki-links
    "x"     (cons "text" (make-sparse-keymap))
    "xb"    'markdown-insert-bold
    "xB"    'markdown-insert-gfm-checkbox
    "xc"    'markdown-insert-code
    "xC"    'markdown-insert-gfm-code-block
    "xi"    'markdown-insert-italic
    "xk"    'markdown-insert-kbd
    "xp"    'markdown-insert-pre
    "xq"    'markdown-insert-blockquote
    "xs"    'markdown-insert-strike-through
    "xQ"    'markdown-blockquote-region
    "xP"    'markdown-pre-region)

  (general-def '(normal insert) markdown-mode-map
    "M-h"      'markdown-promote
    "M-j"      'markdown-move-down
    "M-k"      'markdown-move-up
    "M-l"      'markdown-demote)
  )


(use-package pandoc-mode
  :hook (pandoc-mode . pandoc-load-default-settings)
  :commands pandoc
  :config
  (defun pandoc ()
    "Start pandoc for the buffer and open the menu"
    (interactive)
    ;; only run pandoc-mode if not active, as it resets pandoc--local-settings
    (if (not (bound-and-true-p pandoc-mode)) (pandoc-mode))
    (pandoc-main-hydra/body)))


;; Utilities and misc
;;
;;


(use-package obsidian
  :disabled
  :config
  (obsidian-specify-path "~/obsidian")
  :custom
;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  (global-obsidian-mode t)
  )

(use-package writegood-mode)

(use-package sicp)

(provide 'lang-misc)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
;;; lang-misc.el ends here
