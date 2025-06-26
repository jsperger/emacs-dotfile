**Role and Goal:**

You are an expert Emacs Lisp programmer specializing in configuration refactoring. Your task is to refactor an existing Emacs configuration into a modern, maintainable structure based on a specific set of rules.

**Current State:**

The current configuration consists of several `.el` files located in a single `lisp/` directory. These files contain a mix of `use-package` declarations for configuring packages and custom functions defined with `defun`.

**Desired State & Rules:**

You must refactor the entire configuration according to the following rules, creating a new `init.el` and two new directories: `config/` and `lisp/`.

**1. Directory Structure:**

  - **`config/`**: This directory is exclusively for files containing `use-package` declarations, `setq`, `setopt`, and `add-hook` calls. These files are **NOT** to be byte-compiled.
  - **`lisp/`**: This directory is exclusively for personal library files containing `defun`, `defmacro`, and `defvar` definitions. These files **ARE** intended to be byte-compiled.

**2. File Sorting Logic:**

  - Analyze the content of each existing file.
  - If a file contains `use-package` declarations, move that code into a new file in the `config/` directory.
  - If a file contains `defun` or other function/macro definitions, move that code into a new file in the `lisp/` directory.
  - **If a file is mixed**, you must **separate** the contents. The `defun`s go into a new library file in `lisp/`, and the `use-package` declarations go into a new setup file in `config/`.
  - Any use-package declaration that has a defun should have the defun removed and put in a file in the `lisp/` directory while the `use-package` declaration goes in a file in the config directory.
 **3. Granular Splitting & Intelligent Naming:** 
- Do not simply reuse old filenames. 
- Analyze the content of each code block to derive a descriptive new filename
- You can combine files that were separate and split files that were combined
- Rename files more descriptively. The existing files grew over time and some names were from when I knew very little about emacs. 
- Create many files. This is an intermedite step to creating a literate config and so the many files will eventually become org sub-headings.

**4. Naming Conventions:**
  - Files in the `config/` directory should be named with a `setup-` prefix (e.g., `config/setup-org.el`, `config/setup-editing.el`).
  - Files in the `lisp/` directory should be given a descriptive name, prefixed with `my-` (e.g., `lisp/my-org-helpers.el`, `lisp/my-text-utils.el`).
**5. Required File Headers (Local Variables):**

  - **Every file** you create in the **`config/`** directory **MUST** begin with this exact header:
    ```emacs-lisp
    ;;; config/FILENAME.el --- DESCRIPTION -*- lexical-binding: t -*-

    ;; Local Variables:
    ;; no-byte-compile: t
    ;; no-native-compile: t
    ;; no-update-autoloads: t
    ;; End:
    ```
  - Files you create in the **`lisp/`** directory must **NOT** contain these `no-compile` local variables. They should have a standard header like:
    ```emacs-lisp
    ;;; lisp/FILENAME.el --- DESCRIPTION -*- lexical-binding: t -*-
    ```
**6. Required File Footers:**

  - Every file in the `lisp/` directory must end with:
  ```emacs-lisp
  (provide 'my-filename)
  ;;; my-filename.el ends here
  ```
  
**7. The updated `init.el` File:**

  - Update `init.el` to do the following:
    1.  Manually loads the lisp files with `(require 'my-filename)`
    2.  Manually loads each configuration file from the `config/` directory using a `dolist` loop over an explicit list of filenames. This allows the user to easily comment out lines to toggle parts of their configuration.
  - Your generated `init.el` should look like this template:
    ```emacs-lisp
    ;; Add personal `lisp` directory to the load-path
    (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
    (require 'my-filename)
    (require 'my-other-filename)
    ;; --- Load use-package configurations manually ---
    (let ((config-dir (expand-file-name "config" user-emacs-directory)))
      (dolist (file
               '( ;; List all the new setup-*.el files here
                 "setup-core.el"
                 "setup-editing.el"
                 ;; etc.
                ))
        (load-file (expand-file-name file config-dir))))
    ```

-----

**Example of Refactoring a Single File:**

Suppose I provide you with the following input file:

  * **`lisp/old-org-config.el`:**
    ```emacs-lisp
    (defun my/org-get-todo-template ()
      "Return a template string for Org todos."
      "* TODO %?\n  SCHEDULED: %t")

    (use-package org
      :ensure t
      :config
      (setq org-capture-templates
            `(("t" "Todo" entry (file+headline "~/org/todos.org" "Tasks")
               ,(my/org-get-todo-template)))))
    ```

Based on the rules, your output for this single file should be **two new files**:

  * **`lisp/my-org-helpers.el`:**

    ```emacs-lisp
    ;;; lisp/my-org-helpers.el --- Helper functions for Org Mode -*- lexical-binding: t -*-

    (defun my/org-get-todo-template ()
      "Return a template string for Org todos."
      "* TODO %?\n  SCHEDULED: %t")

    (provide 'my-org-helpers)
    ```

  * **`config/setup-org.el`:**

    ```emacs-lisp
    ;;; config/setup-org.el --- Org Mode configuration -*- lexical-binding: t -*-

    ;; Local Variables:
    ;; no-byte-compile: t
    ;; no-native-compile: t
    ;; no-update-autoloads: t
    ;; End:

    (require 'my-org-helpers)

    (use-package org
      :ensure t
      :config
      (setq org-capture-templates
            `(("t" "Todo" entry (file+headline "~/org/todos.org" "Tasks")
               ,(my/org-get-todo-template)))))
    ```
# Notes
This section is for you to add your own notes as you work if you wish. 

# Task management
## Review this file as you work
- Review the instructions in this file periodically as you work. 
- Add notes to yourself as you work if you discover important information that you might want to refer to later.
## TODO List
The following is a list of every file in the `lisp/` directory to start. Update the check boxes as you complete the work
Update the following list while you work. When you check off a box add a one sentence explanation of what you did with the contents of the file e.g. 'split into `config/setup-example.el` and `lisp/my-example-helpers.el`' or 'renamed to `config/setup-foo.el`'
- [ ] builtin-packages.el
- [ ] completion-actions.el
- [ ] completion-backends.el
- [ ] completion-display.el
- [ ] completion-movement.el
- [ ] completion-snippets.el
- [ ] core-config.el
- [ ] core-funcs.el
- [ ] core-keybinds.el
- [ ] core-packages.el
- [ ] core-treesit.el
- [ ] editor-completion.el
- [ ] editor-font-locking.el
- [ ] editor-fonts.el
- [ ] editor-icons.el
- [ ] editor-lsp.el
- [ ] editor-misc.el
- [ ] editor-projects.el
- [ ] editor-themes.el
- [ ] editor-ui.el
- [ ] editor-vc.el
- [ ] evil-addons.el
- [ ] evil-core.el
- [ ] kbd-macros.el
- [ ] lang-bib.el
- [ ] lang-data-formats.el
- [ ] lang-functions.el
- [ ] lang-go.el
- [ ] lang-lisp.el
- [ ] lang-markdown.el
- [ ] lang-org.el
- [ ] lang-python.el
- [ ] lang-r.el
- [ ] lang-rust.el
- [ ] lang-tex.el
- [ ] lang-web.el
- [ ] my-keybind-funs.el
- [ ] org-node.el
- [ ] tools-casual.el
- [ ] tools-denote.el
- [ ] tools-diagramming.el
- [ ] tools-eaf.el
- [ ] tools-email.el
- [ ] tools-llm.el
- [ ] tools-lsp-bridge.el
- [ ] tools-media.el
- [ ] tools-notes.el
- [ ] tools-org-capture.el
- [ ] tools-programming.el
- [ ] tools-reading.el
- [ ] tools-search.el
- [ ] tools-web.el
- [ ] tools-writing.el
- [ ] ui-modeline.el
