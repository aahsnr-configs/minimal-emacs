# CLAUDE.md

Personal vanilla Emacs 31.1 configuration. Packages are managed by straight.el.

## Ignore

Do not read, search, edit, or index any of the following:

- `eln-cache/`
- `etc/`
- `tests/`
- `var/`
- `LICENSE`
- `NOTICE`

`eln-cache/`, `etc/` and `var/` are generated state; `tests/` is out of scope; `LICENSE` and `NOTICE` are legal boilerplate.

## Layout

- `early-init.el`: runs before the first frame; routes state files into `etc/` and `var/` natively (no no-littering).
- `init.el`: straight.el bootstrap, the package manifest (`ar/packages`), and the ordered module loader. Load order of modules is load-bearing.
- `lisp/*-config.el`: modules (core, ui, editor, completion, keybinds, lang, dev, org, tools, vc-projects).
- `snippets/`: yasnippet snippets.

## Conventions

- Every `.el` file uses `lexical-binding: t` and the standard header/Commentary/Code layout.
- Add new packages to the manifest in `init.el`, not in individual modules.
- Match the surrounding code's comment density and naming (`ar/` prefix for personal symbols).
