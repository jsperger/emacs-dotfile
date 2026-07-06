# Working on this Emacs config

This is a **literate Emacs configuration**. `its-lit.org` is the single source of
truth; all the `.el` files in the repo are *generated* from it.

## The golden rule: edit the org file, not the tangled `.el` files

`its-lit.org` is tangled with `org-babel-tangle` to produce every config file:
`early-init.el`, `init.el`, `config/*.el`, and `lisp/*.el`.

**Do not edit the tangled `.el` files directly** — your changes will be
overwritten the next time the org file is tangled. Make changes in `its-lit.org`,
in the source block for the relevant section, then re-tangle.

You can tell a file is tangled output by its magic comments (from `:comments
noweb`), e.g. `;; [[file:its-lit.org::init elpaca][init elpaca]]`. These link the
code back to its source block in the org file.

- `README.org` is a symlink to `its-lit.org`.
- `#its-lit.org#` (if present) is an Emacs auto-save file; ignore it.

## Tangling

Prefer `emacsclient` over `emacs` for any elisp evaluation (byte-compile,
check-parens, ERT, tangling). To regenerate the config after editing the org file:

```sh
emacsclient --eval '(org-babel-tangle-file "~/.emacs.d/its-lit.org")'
```

Or, from inside the open `its-lit.org` buffer, the org leader binds `SPC m a`
(or `SPC m b t`) to `org-babel-tangle`. There is no custom tangle wrapper or
active post-tangle hook — tangling is stock `org-babel-tangle`, which is why the
per-file headers and footers are written by hand in the org file (with
`:comments no`).

## Org structure → file layout

Headings in the config sections are functional, not just organizational:

| Level    | Role                                                    |
|----------|---------------------------------------------------------|
| H1 `*`   | Groups sections; tangles into a directory               |
| H2 `**`  | Each one tangles to a distinct file                     |
| H3+ `***`| Finer control of header-args, time-stamping, sub-blocks |

Header-args set on a heading (e.g. `:header-args: :tangle config/setup-foo.el
:comments noweb`) control where its blocks tangle. `:comments noweb` enables
detangling; header/footer blocks use `:comments no` so the `lexical-binding`
cookie and local-variable footer aren't wrapped in link comments.

## Two kinds of generated elisp

- **`config/`** — package/option configuration: `use-package` declarations,
  `setq`/`setopt`, `add-hook`. **Not byte-compiled.** Each file ends with a
  local-variables footer (`no-byte-compile: t`, etc.). Filenames use a
  `setup-` or `configure-` prefix.
- **`lisp/`** — personal library code: `defun`, `defmacro`, `defvar`. **Byte-**
  **compiled.** Each file ends with `(provide 'my-NAME)` and files are named with
  a `my-` prefix.

When a section defines a helper function used by a `use-package` block, the
`defun` lives in a `lisp/my-*.el` file and the `use-package` block `require`s it.

## Loading order (`init.el`)

1. Bootstraps **elpaca** (the package manager) and `elpaca-use-package`.
2. Defines platform constants (`IS-MAC`, `IS-LINUX`, …) and `my-debug-mode`.
3. Loads immediate-need config (currently `config/setup-evil.el`).
4. `require`s the `lisp/my-*.el` libraries.
5. Loads `config/*.el` via a `dolist` over an explicit filename list — comment
   out a line there to disable part of the config. **If you add a new
   `config/setup-*.el` section, add its filename to that list** (this is in the
   `load configuration files` block of the org file).
6. Loads `customs.el` (the `custom-file`) last.

## Packages & keybindings

- Package manager is **elpaca**; `use-package` defaults to `:ensure t`. Immediate
  loads use `:ensure (:wait t)`.
- Installed package sources/builds live under `elpaca/`. **Reference them freely
  for how a package works, but never modify them** — they're managed by elpaca and
  gitignored.
- Keybindings use **general.el** with an evil `SPC` leader. `tyrant-def` binds
  under the global `SPC` prefix; `despot-def` binds under `SPC m` per major mode.

## Not tracked in git

`elpaca/`, `elpa/`, `var/`, `eln-cache/`, `tree-sitter/`, and assorted
package-generated state are gitignored (see `.gitignore`). The tracked config is
`its-lit.org` plus the tangled `.el` files, `early-init.el`, and `customs.el`.
