;;; init.el --- Emacs configuration entry point -*- lexical-binding: t -*-

;; Add personal `lisp` directory to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; --- Load personal library files ---
(require 'my-bib-helpers)
(require 'my-completion-helpers)
(require 'my-consult-helpers)
(require 'my-core-functions)
(require 'my-core-helpers)
(require 'my-editor-helpers)
(require 'my-emacs-helpers)
(require 'my-evil-helpers)
(require 'my-kbd-macros)
(require 'my-keybinding-helpers)
(require 'my-keybindings)
(require 'my-lang-helpers)
(require 'my-lsp-bridge-helpers)
(require 'my-org-helpers)
(require 'my-project-helpers)
(require 'my-tempel-helpers)
(require 'my-theme-helpers)
(require 'my-vc-helpers)
(require 'my-writing-helpers)

;; --- Load use-package configurations manually ---
(let ((config-dir (expand-file-name "config" user-emacs-directory)))
  (dolist (file
           '(
             "setup-bib.el"
             "setup-builtin-packages.el"
             "setup-casual.el"
             "setup-completion-actions.el"
             "setup-completion-backends.el"
             "setup-completion-display.el"
             "setup-completion.el"
             "setup-consult.el"
             "setup-core.el"
             "setup-data-formats.el"
             "setup-denote.el"
             "setup-diagramming.el"
             "setup-eaf.el"
             "setup-editor-misc.el"
             "setup-emacs.el"
             "setup-email.el"
             "setup-evil-addons.el"
             "setup-evil.el"
             "setup-font-locking.el"
             "setup-fonts.el"
             "setup-go.el"
             "setup-gopher.el"
             "setup-icons.el"
             "setup-keybindings.el"
             "setup-lisp.el"
             "setup-llm.el"
             "setup-lsp-bridge.el"
             "setup-lsp.el"
             "setup-markdown.el"
             "setup-media.el"
             "setup-modeline.el"
             "setup-notes.el"
             "setup-org-capture.el"
             "setup-org-node.el"
             "setup-org.el"
             "setup-programming.el"
             "setup-projects.el"
             "setup-python.el"
             "setup-r.el"
             "setup-reading.el"
             "setup-rust.el"
             "setup-search.el"
             "setup-snippets.el"
             "setup-tex.el"
             "setup-themes.el"
             "setup-treesit.el"
             "setup-ui.el"
             "setup-vc.el"
             "setup-web.el"
             "setup-writing.el"
            ))
    (load-file (expand-file-name file config-dir))))