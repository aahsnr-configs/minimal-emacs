# A vanilla Emacs 31.1 configuration

> Hand-rolled, Doom-shaped, and entirely local: **straight.el** for packages,
> **Evil** for editing, **Denote** for notes, and no framework to work around.

![Emacs](https://img.shields.io/badge/Emacs-31.1-7F5AB6)
![Packages](https://img.shields.io/badge/packages-187-4E9A06)
![Editing](https://img.shields.io/badge/editing-Evil-6A5ACD)
![Notes](https://img.shields.io/badge/notes-Denote-2E8B57)

This is a personal, single-user Emacs configuration. It is **not** a starter
kit or a distribution: it has no installer script, no module toggles, and no
`doom` command. The repository is the configuration. Read it, fork it, or take
the pieces you want.

It targets **GNU Emacs 31.1** and is developed primarily on GNU/Linux with
PGTK/Wayland. The code keeps a Windows guard for UTF-8 selections, but other
platforms are not regularly tested.

---

## Table of contents

- [What this is](#what-this-is)
- [At a glance](#at-a-glance)
- [Design principles](#design-principles)
- [Requirements](#requirements)
- [Installation](#installation)
- [First run](#first-run)
- [Directory layout](#directory-layout)
- [How startup works](#how-startup-works)
- [Modules](#modules)
- [Keybindings](#keybindings)
- [Packages](#packages)
- [Customization](#customization)
- [Org and Denote](#org-and-denote)
- [Development stack](#development-stack)
- [User interface](#user-interface)
- [Performance](#performance)
- [State and persistence](#state-and-persistence)
- [Testing](#testing)
- [Troubleshooting](#troubleshooting)
- [Security notes](#security-notes)
- [Conventions](#conventions)
- [Known limitations](#known-limitations)
- [Credits and licensing](#credits-and-licensing)

---

## What this is

A complete Emacs configuration built directly on vanilla Emacs:

- **10 local modules** under `lisp/`, loaded in a deliberate order.
- **187 package entries** in one manifest, including **23 explicit git-only
  recipes**.
- Roughly **6,200 lines** of Emacs Lisp, comments included.
- A Doom-inspired user experience: `SPC` leader keys, Evil, a dashboard, a
  rule-based popup manager, workspaces, and a Tokyo Night theme.
- No Doom, Spacemacs, Prelude, `use-package` installer, `package.el`
  activation, or `no-littering`. State routing is done natively.

The package manager is [straight.el](https://github.com/radian-software/straight.el).
Packages are cloned and built from Git, which also makes git-only packages and
explicit commit pins first-class. `init.el` is the only file that installs
anything; every `use-package` form in `lisp/` only configures.

## At a glance

| Area | What you get |
| --- | --- |
| Editing | Evil + Doom's Evil plugin suite, AST text objects, multi-cursor editing, snippets, folding, Jinx spell checking |
| Completion | Vertico, Orderless, Marginalia, Consult, Embark, Corfu, Cape, wgrep |
| Notes | Org mode with lazy Babel loading, org-modern, org-appear, and the full Denote ecosystem |
| IDE | Tree-sitter modes and grammars, lsp-mode + lsp-ui, Flymake, Dape, Apheleia, envrc |
| Version control | Magit, Forge, code-review, orgit, git-link, git-timemachine, blamer, magit-todos, diff-hl |
| Files and projects | project.el backbone, Projectile compatibility, Dired, Dirvish, Treemacs, ibuffer |
| Workspaces | persp-mode, with explicit session save/restore |
| Terminal | Ghostel terminal drawer, Eshell integration, TTY-faithful compilation buffers |
| Tools | EIN/Jupyter, quickrun, eros, dumb-jump, gptel, pdf-tools |
| Appearance | doom-tokyo-night, doom-modeline, Nerd Icons, custom dashboard, popup manager, pixel scrolling |

## Design principles

1. **Vanilla, with Doom ergonomics — not Doom internals.**
   The keybindings and defaults are Doom-like, but the implementation is local
   Emacs Lisp. There is no framework layer to debug and no `doom sync`.

2. **One installer, one manifest.**
   `init.el` holds `ar/packages`. If a package is not in that list, it is not
   installed. If it is in the list, it also needs a configuring `use-package`
   block in a module. One without the other is a bug.

3. **Load order is part of the API.**
   `core-config` defines shared helpers. `editor-config` defines the leader
   macros. `keybinds-config` loads last and binds commands from every module.
   Reordering the `ar-modules` list can silently break startup.

4. **Defer by default; load only first paint eagerly.**
   Theme, modeline, icons, Evil, Vertico/Marginalia, and which-key are needed
   immediately. Nearly everything else is loaded by autoloads, hooks, or
   `with-eval-after-load`.

5. **State has a home.**
   Machine-written files go under `var/`; user-editable files go under `etc/`.
   `user-emacs-directory` itself is moved to `var/`, so straight repos, native
   compilation output, histories, and package state all follow automatically.

6. **Fail loudly, but keep batch runs usable.**
   Package installation collects every failure and raises one error naming all
   of them. Display-only work is guarded by `noninteractive`; idle timers and
   `after-init-hook` do not fire in batch.

7. **Performance is a feature, not a cleanup pass.**
   GC is deferred during startup, expensive modes refuse to activate in huge
   buffers, bytecode is cached and rebuilt in a subprocess, and native
   compilation output is kept out of the config root.

8. **Give git-only packages explicit recipes.**
   Git-only packages carry explicit `:host` and `:repo` recipes. Packages
   available from the configured recipe repositories use the default recipe.
   No package versions are locked.

## Requirements

### Required

| Requirement | Why |
| --- | --- |
| **GNU Emacs 31.1** | The configuration uses Emacs 31 APIs (`treesit`, `show-paren-not-in-comments-or-strings`, newer Org/tree-sitter behavior). Older Emacsen are not supported. |
| **Git** | straight.el clones and builds every package from Git. |
| **Network access on first start** | straight.el bootstraps itself, then clones the manifest. |
| **A C toolchain** | Needed for native compilation, tree-sitter grammars, and packages with native components. |
| **ripgrep (`rg`)** | Consult project search, grep, and the xref fallback. |

Check the basics:

```bash
emacs --version    # must report 31.1
git --version
rg --version
```

### Recommended system packages

| Tool | Used by | Notes |
| --- | --- | --- |
| `fd` | Dirvish's `dirvish-fd` | File jumping inside Dirvish. |
| `zstd` or `gzip` | `undo-fu-session` | Persistent undo history is compressed when either exists. |
| Enchant + a dictionary | Jinx | `jinx-languages` is set to `"en"`. Without the native module, Jinx stays off rather than breaking text buffers. |
| JetBrains Mono | Default, fixed-pitch, variable-pitch faces | The config deliberately uses a monospaced variable-pitch face so Org tables stay aligned. |
| Symbols Nerd Font Mono | Nerd Icons | `M-x nerd-icons-install-fonts` can install it. |
| Noto Sans | Geometric/dingbat fallbacks | Used for `#x25A0–#x25FF`, `#x2600–#x26FF`, and `#x2700–#x27BF`. |
| `emacs-lsp-booster` | Optional LSP performance | If present, lsp-mode is wrapped automatically. |
| `epdfinfo` | pdf-tools | Build once with `M-x pdf-tools-install`. |
| `ty`, `ruff` | Python language servers | lsp-mode runs both in Python buffers (`ty server` for types and navigation, `ruff server` for lint and fixes). Install with `uv tool install ty ruff`. |
| `ruff` | Python formatting | `ruff-isort` then `ruff format`, wired into Apheleia for `python-mode` and `python-ts-mode`. |
| `nixfmt` | Nix formatting | Configured through `nix-mode`. |
| `pytest` | python-pytest | Uses the `pytest` executable by default. |
| `ipython` | Python REPL | Selected automatically when available. |
| `jupyter` + `ipykernel` | emacs-jupyter REPLs and `jupyter-python` Babel blocks | Install `ipykernel` in the project environment (`uv add --dev ipykernel`). The zmq module compiles on first use and needs a C compiler and libzmq. EIN provides notebooks. |
| Ghostel's native dependency | Ghostel terminal | Ghostel is deferred; if its native module is unavailable, the rest of Emacs still starts. |

Language servers, debug adapters, and API credentials are intentionally **not**
bundled. Install the server for each language you use, and configure gptel's
backend in `etc/custom.el` or through `auth-source`.

## Installation

> [!IMPORTANT]
> Back up your current configuration first, and make sure neither `~/.emacs`
> nor `~/.emacs.el` exists. Those files take precedence over an init directory.

### 1. Back up the old configuration

```bash
# Pick whichever location you currently use.
mv ~/.config/emacs ~/.config/emacs.bak 2>/dev/null || true
mv ~/.emacs.d ~/.emacs.d.bak 2>/dev/null || true
mv ~/.emacs ~/.emacs.bak 2>/dev/null || true
mv ~/.emacs.el ~/.emacs.el.bak 2>/dev/null || true
```

### 2. Clone this repository into an Emacs init directory

```bash
# XDG location (recommended on Linux)
git clone https://github.com/aahsnr-configs/emacs-config.git \
  "${XDG_CONFIG_HOME:-$HOME/.config}/emacs"

# Traditional location
# git clone https://github.com/aahsnr-configs/emacs-config.git ~/.emacs.d
```

Any directory works. Emacs finds `early-init.el` and `init.el` there, and
`early-init.el` canonicalizes the path with `file-truename` before deriving
anything from it.

### 3. Start Emacs

```bash
emacs
```

On first start, straight.el bootstraps itself, clones the manifest, and builds
the packages. This can take several minutes. If any package fails, startup
aborts with a single error naming every failure instead of leaving a partially
installed configuration.

### 4. Try it without replacing your setup

Emacs 29+ can point at a configuration directory for one session:

```bash
emacs --init-directory /path/to/this/repository
```

The config still writes its state into `var/` inside that directory, so the
trial is self-contained. Do not run this against a checkout on a different
mount path than the one used to build `var/straight/build/`; straight's build
symlinks are absolute (see [Troubleshooting](#troubleshooting)).

### 5. Update later

There is no wrapper command. Use straight directly:

```elisp
M-x straight-pull-all      ; pull package repositories
M-x straight-rebuild-all   ; rebuild after pulls or compiler changes
M-x straight-prune-build   ; remove orphaned build directories
```

Then restart Emacs. Package versions are not locked: a fresh install and every
`straight-pull-all` take the latest upstream commits.

## First run

After the dashboard appears, a few things may still need one-time setup:

1. **Icons look like empty boxes** — install the Nerd Font:
   `M-x nerd-icons-install-fonts`, then restart the frame.
2. **PDFs do not render** — run `M-x pdf-tools-install` once to build
   `epdfinfo`.
3. **Tree-sitter grammars** — Emacs's own modes register their pinned grammar
   sources; this config adds pinned sources for LaTeX, BibTeX, Nix, Make, Awk
   and Org. Grammars auto-install when a compatible compiler is available. If a grammar fails, install it manually
   with `M-x treesit-install-language-grammar`.
4. **Language servers** — lsp-mode starts on `prog-mode` hooks. Install the
   server for your language (ty and ruff for Python, nixd/nil, texlab, qmlls,
   typescript-language-server, bash-language-server, etc.).
5. **gptel** — set your backend/API key before using `SPC o l`.
6. **Org files** — notes live outside the repository. See
   [Org and Denote](#org-and-denote).

The dashboard reports the manifest's package count, the module count, and how
long startup took. It is also the place to restore the previous workspace
session.

## Directory layout

```text
<config-root>/
├── early-init.el              ; pre-frame: GC, package.el off, state routing
├── init.el                    ; straight bootstrap, manifest, module loader
├── lisp/
│   ├── core-config.el         ; 1  helpers, defaults, GC, persistence
│   ├── editor-config.el       ; 2  Evil, leaders, editing aids
│   ├── ui-config.el           ; 3  theme, modeline, popups, dashboard
│   ├── completion-config.el   ; 4  Vertico/Consult/Embark, Corfu/Cape
│   ├── org-config.el          ; 5  Org + Denote
│   ├── dev-config.el          ; 6  treesit, LSP, Flymake, Dape, Apheleia
│   ├── tools-config.el        ; 7  EIN, quickrun, gptel, PDF
│   ├── lang-config.el         ; 8  language modes and per-language tooling
│   ├── vc-projects-config.el  ; 9  Magit, Dired/Dirvish, Treemacs, persp, Ghostel
│   └── keybinds-config.el     ; 10 escape, leader trees, windows, which-key
├── snippets/                  ; personal YASnippet snippets
├── tests/
│   └── config-regressions.el  ; package-free ERT checks
├── etc/                       ; created at startup; user-editable state
│   ├── custom.el              ; Customize output / personal settings
│   ├── abbrev_defs
│   └── projects.el            ; project.el's known-project list
└── var/                       ; created at startup; machine state (gitignored)
    ├── straight/{repos,build}/
    ├── eln-cache/
    ├── bytecode/
    ├── tree-sitter/
    ├── org/
    ├── persp/
    └── per-package state (undo-fu-session, dirvish, treemacs, dape, …)
```

`etc/` and `var/` are gitignored because they are runtime state. The only
exception in the config root is the `eln-cache` symlink, which points into
`var/eln-cache/` because native compilation writes there through a path that
the directory swap cannot influence.

## How startup works

```text
Emacs starts
│
├── early-init.el
│   ├── raise GC threshold; 4 MiB process reads; load-prefer-newer
│   ├── disable package.el activation
│   ├── set LSP_USE_PLISTS=true
│   ├── move user-emacs-directory to <config-root>/var/
│   ├── create etc/ and var/ subtrees
│   ├── symlink <config-root>/eln-cache -> var/eln-cache/
│   └── strip menu/tool/scroll bars before the first frame
│
├── init.el
│   ├── bootstrap straight.el if needed
│   ├── register bundled libraries as :type built-in
│   ├── install every entry in ar/packages (fail-loud)
│   ├── load etc/custom.el
│   ├── select var/bytecode/ only if every .elc is current
│   └── require the 10 modules in ar-modules order
│
└── first frame
    ├── apply theme, fonts, modeline, icons
    ├── start gcmh, diff-hl, envrc, Super Save
    └── show the *ahsan* dashboard
```

### Load-order contract

| # | Module | Provides | Depends on |
| --- | --- | --- | --- |
| 1 | `core-config` | `too-long-file-p`, `ar/define-large-file-guard`, `with-eval-after-load-all`, `ar/escape-key`, `reload-init-file` | Emacs |
| 2 | `editor-config` | `ar/global-leader`, `ar/local-leader`, Evil stack | core |
| 3 | `ui-config` | theme, modeline, popup manager, dashboard, folding | core |
| 4 | `completion-config` | Vertico/Consult/Embark, Corfu/Cape | core |
| 5 | `org-config` | Org, Denote, note leader tree | core, leaders |
| 6 | `dev-config` | treesit, lsp-mode, Flymake, Dape, Apheleia | core, leaders |
| 7 | `tools-config` | EIN, quickrun, gptel, pdf-tools | core, leaders |
| 8 | `lang-config` | language modes and per-language setup | core |
| 9 | `vc-projects-config` | Magit, Dired/Dirvish, Treemacs, persp, Ghostel | core, leaders |
| 10 | `keybinds-config` | escape/minibuffer maps, leader trees, windows, which-key | all of the above |

`reload-init-file` (`SPC f l`) re-evaluates these files from source in the same
order. It deliberately excludes `early-init.el` and `init.el`: the bootstrap
and module-loading contract must run once per session. Removed hooks, removed
packages, and manifest changes require a restart.

## Modules

### `core-config`

Editor defaults, GC management, and shared helpers. It configures
`gcmh`, recentf, savehist, saveplace, autorevert, subword, pixel scrolling,
electric-pair (including prose emphasis pairs), adaptive fill, whitespace,
line numbers, window dividers, hl-line, and buffer-terminator. It owns the
PGTK/Wayland workarounds, UTF-8 coding defaults, and the `ar` customization
group.

### `editor-config`

The Evil stack: `evil`, `evil-collection`, `evil-escape` (`jk`), undo-fu and
undo-fu-session, `vundo`, the Doom Evil plugins, tree-sitter text objects,
`expreg`, `evil-ts-obj`, Avy/evil-easymotion, evil-snipe, link-hint, anzu,
iedit/evil-multiedit, Jinx, Helpful, YASnippet, auto-yasnippet, sudo-edit,
adaptive-wrap, and visual-fill-column. It also defines the leader macros that
every later module uses.

### `ui-config`

The visual shell: `doom-themes` with `doom-tokyo-night`, solaire-mode, Nerd
Icons, font setup, doom-modeline, a native hide-modeline mode, global text
zoom, colorful-mode, rainbow-delimiters, indent-bars, the folding dispatcher,
hl-todo, a diff-hl fringe gutter, the ported Doom popup manager, and the
`*ahsan*` dashboard.

### `completion-config`

Vertico, Orderless, Marginalia, Consult, Embark, wgrep, Corfu, Cape,
`consult-yasnippet`, `nerd-icons-completion`, `nerd-icons-corfu`, dabbrev, and
ripgrep-backed grep/xref settings. The minibuffer and in-buffer stacks are kept
separate: Corfu is disabled in the minibuffer so Vertico owns it.

### `org-config`

Org mode in Doom-default shape, plus the Denote second brain. Org caches are
routed into `var/org/`; Babel languages are loaded lazily; org-modern and
org-appear handle display. Denote, silos, sequences, journaling, review,
exploration, word clouds, merging, and `consult-denote` are configured. There
is no org-roam.

### `dev-config`

Tree-sitter grammar management, lsp-mode + lsp-ui, Flymake + flymake-popon,
Dape, Apheleia, and envrc. It also wires optional `emacs-lsp-booster` support
and the lsp-mode plist-consistency check.

### `tools-config`

EIN for Jupyter notebooks, quickrun and eros for evaluation, dumb-jump as an
xref fallback, the gptel suite, pdf-tools, PDF scroll restoration, and
org-pdftools links.

### `lang-config`

Per-language modes and tooling for data formats, Emacs Lisp, JSON, LaTeX,
Markdown, Nix, Python, Qt/QML, shell, web templates, and YAML. Tree-sitter
routing itself lives in `dev-config`; LSP auto-start lives in `lsp-mode`'s
`prog-mode` hook.

### `vc-projects-config`

Magit, Forge, code-review, orgit, git-link, git-modes, git-timemachine,
blamer, and magit-todos. It also owns Dired, Dirvish, Dired extensions,
Treemacs, project.el, the Projectile compatibility layer, ibuffer, persp-mode
workspaces, Ediff, woman, and the Ghostel terminal.

### `keybinds-config`

Escape and minibuffer plumbing, the file/search/quit/open leader trees,
global zoom keys, transient, which-key, ace-window, winner-mode, and the
window leader tree.

## Keybindings

The leader is **`SPC`**, with **`C-SPC`** as the global alternative. `SPC`
works in normal and visual states; `C-SPC` works in every state, including
insert and Emacs. `jk` leaves insert state. `<escape>`
clears search highlights, aborts the minibuffer, or quits a pending command
through one shared function. `C-g` also closes the topmost popup when
appropriate.

Which-key shows the next level after one second. The tables below cover the
main trees; `SPC` alone is the authoritative discovery mechanism.

### Top level

| Key | Tree | Examples |
| --- | --- | --- |
| `SPC SPC` | Execute command | `M-x` equivalent |
| `SPC :` | Evaluate expression | `eval-expression` |
| `SPC u` | Universal argument | Prefix argument |
| `SPC !` | Shell command | One-shot shell command |
| `SPC X` | Org capture | `org-capture` |
| `SPC f` | Files and Emacs | Find, recent, save, reload config |
| `SPC s` | Search | Buffer, project, imenu, outline, marks |
| `SPC h` | Help | Emacs manual (`SPC h d h`) |
| `SPC q` | Quit/session | Save workspace, reload last session, quit |
| `SPC o` | Open/applications | Org, projects, terminal, gptel, Dirvish |
| `SPC w` | Windows | Ace-window, split, move, winner |
| `SPC p` | Projects | Switch, find, search, dired, shell |
| `SPC g` | Git | Magit, commit, push, log, hunks |
| `SPC c` | Code | LSP actions, diagnostics, debug, format |
| `SPC n` | Notes | Denote create/search/link/journal/silos |
| `SPC TAB` | Workspaces | Persp switch/create/save/load |

### Files and Emacs (`SPC f`)

| Key | Command |
| --- | --- |
| `SPC f f` | `find-file` |
| `SPC f r` | `consult-recent-file` |
| `SPC f s` / `SPC f S` | Save / save as |
| `SPC f l` | Reload all modules from source |
| `SPC f d` | `consult-dir` |
| `SPC f j` | Jump to a file in a directory |
| `SPC f p` | Find a file in this configuration |
| `SPC f e d` | Open the configuration root |
| `SPC f e i` | Open `init.el` |
| `SPC f P` | Open the configuration root in Dired |
| `SPC f E` | Reopen the current file through sudo/TRAMP |

### Search (`SPC s`)

| Key | Command |
| --- | --- |
| `SPC s s` / `SPC s b` | `consult-line` |
| `SPC s S` | `consult-line-multi` |
| `SPC s p` | `consult-ripgrep` |
| `SPC s i` / `SPC s I` | `consult-imenu` / `consult-imenu-multi` |
| `SPC s o` | `consult-outline` |
| `SPC s m` | `consult-mark` |
| `SPC s e` | `consult-isearch-history` |
| `SPC s r` | `vertico-repeat` |
| `SPC s c` | Clear search highlights |

### Git (`SPC g`)

| Key | Command |
| --- | --- |
| `SPC g s` | Magit status |
| `SPC g h` | Version-control transient dashboard |
| `SPC g c` / `SPC g C` | Commit / amend |
| `SPC g p` / `SPC g P` | Push / pull |
| `SPC g f` | Fetch |
| `SPC g b` / `SPC g l` / `SPC g L` | Branches / file log / branch log |
| `SPC g m` / `SPC g r` | Merge / rebase |
| `SPC g t` | Git timemachine |
| `SPC g n` / `SPC g N` | Next / previous diff hunk |
| `SPC g S` / `SPC g R` / `SPC g H` | Stage / revert / preview hunk |
| `SPC g y` | Copy remote URL (`git-link`) |

### Projects (`SPC p`)

| Key | Command |
| --- | --- |
| `SPC p p` | Switch project |
| `SPC p f` / `SPC p F` | Find file here / in another project |
| `SPC p b` | Project buffers |
| `SPC p s` | Ripgrep in project |
| `SPC p d` / `SPC p e` / `SPC p S` | Dired root / Eshell / shell |
| `SPC p r` | Project query-replace |
| `SPC p k` | Kill all project buffers |
| `SPC p R` | Recent files in project |
| `SPC p i` / `SPC p a` | Forget zombie projects / add project |

### Code (`SPC c`)

| Key | Command |
| --- | --- |
| `SPC c a` | LSP code action |
| `SPC c r` | LSP rename |
| `SPC c i` | Organize imports |
| `SPC c f` | Format buffer with Apheleia |
| `SPC c e` | Diagnostics via `consult-flymake` |
| `SPC c E` / `SPC c B` | Project / buffer diagnostics |
| `SPC c !` | Force a Flymake run |
| `SPC c H` / `SPC c T` | Toggle inlay hints / semantic tokens |
| `SPC c D` / `SPC c A` | References / workspace symbols |
| `SPC c x` / `SPC c y` | Treemacs problems / symbols |
| `SPC c g d` / `SPC c g D` / `SPC c g t` / `SPC c g i` | Definition / declaration / type / implementation |
| `SPC c s` | AST structural refactor transient |
| `SPC c v` | Multi-cursor prefix |
| `SPC c d` | Debug prefix (Dape) |

### Notes (`SPC n`)

| Key | Command |
| --- | --- |
| `SPC n n` / `SPC n N` | New note / new note in subdirectory |
| `SPC n f` / `SPC n s` | Find note / search contents |
| `SPC n l` / `SPC n i` / `SPC n b` | Link / link-or-create / backlinks |
| `SPC n j` / `SPC n J` | Journal today / new journal entry |
| `SPC n q` | Denote sequence prefix |
| `SPC n S` | Denote silo prefix |
| `SPC n e` | Denote explore prefix |
| `SPC n m` | Denote management/merge/review prefix |

### Org (`SPC o`)

| Key | Command |
| --- | --- |
| `SPC X` / `SPC o c` | Org capture |
| `SPC o L` | Store Org link |
| `SPC o a a` | Org agenda dispatcher |
| `SPC o a m` / `SPC o a t` | Tags search / global TODO list |
| `SPC o a v` / `SPC o a h` | Fuzzy heading jump / fuzzy agenda heading |
| `SPC o a d` / `SPC o A` | Agenda dashboard |
| `SPC o a i` / `SPC o a T` | Open inbox / open tasks |
| `SPC o k i` / `o` / `l` / `g` / `x` | Clock in / out / in last / go to / cancel |
| `SPC m` (Org buffer) | todo `t`, schedule `s`, deadline `d`, refile `r`, archive `a`, clock `c i/o/r`, export `e` |
| `SPC m` (Python buffer) | ruff fix `f`, imports `i`, Jupyter REPL `r`, eval `e`/`d`/`b`, kernel `k`/`x` |

### Windows and workspaces

| Key | Command |
| --- | --- |
| `SPC w w` | Ace-window |
| `SPC w d` / `SPC w m` / `SPC w x` | Delete / maximize / swap |
| `SPC w s` / `SPC w v` | Split below / right |
| `SPC w <left>` etc. | Directional window movement |
| `SPC w S-<left>` etc. | Move window to edge |
| `SPC w =` | Balance windows |
| `SPC w u` / `SPC w r` | Winner undo / redo |
| `SPC w t` | Toggle split direction |
| `SPC w p` / `SPC w P` | Toggle / close all popups |
| `SPC TAB TAB` | Switch/create workspace |
| `SPC TAB n` / `SPC TAB p` | Next / previous workspace |
| `SPC TAB s` / `SPC TAB l` | Save / load workspace state |
| `SPC TAB P s` | Switch project in a new workspace |

### Quit and session (`SPC q`)

| Key | Command |
| --- | --- |
| `SPC q q` / `SPC q Q` | Save and quit / quit without saving |
| `SPC q f` | Delete frame |
| `SPC q s` / `SPC q l` | Save / load workspace by name |
| `SPC q L` | Reload the last auto-saved session |
| `SPC q RET` | Jump to bookmark |

### Editing keys outside the leader

| Key | Action |
| --- | --- |
| `jk` (insert) | Leave insert state |
| `<escape>` | Clear highlights / abort minibuffer / quit |
| `C-g` | Quit, or close the topmost popup |
| `M-d` / `M-D` | Add cursor at next match / all matches |
| `C-S-<down>` / `C-S-<up>` | Add cursors down / up in visual state |
| `gs` | Evil-easymotion prefix (`gs s`, `gs l`, `gs SPC`) |
| `gc` / `gcc` | Comment operator / line comment |
| `gx` / `gX` | Exchange two regions / cancel |
| `ys` / `cs` / `ds` | Surround add / change / delete |
| `za` / `zo` / `zc` / `zR` / `zM` | Toggle / open / close / open all / close all folds |
| `zf` / `zF` / `zd` / `zE` | Create fold / line fold / delete / delete all |
| `]f` / `[f` | Next / previous function (tree-sitter) |
| `]C` / `[C` | Next / previous class |
| `] e` / `[ e` | Next / previous Flymake diagnostic |
| `K` | Eldoc / hover documentation |
| `F2` | LSP rename |
| `C-;` | Embark act |
| `C-x M-r` | `vertico-repeat-select` |
| `M-/` / `C-M-/` | Dabbrev completion / expand |
| `C-=` / `C--` / `C-0` | Global zoom in / out / reset |
| `C-\`` | Toggle the last popup |
| `C-c l d` / `C-c l p` | LSP UI doc glance / peek references |
| `C-c n` / `C-c p` / `C-c i` | Next / previous / insert TODO keyword |
| `C-c e` | Macrostep expand |
| `C-c C-p` | JSON path at point |

The text-object layer is deliberately rich: `if/af` (function), `iF/aF`
(call), `iC/aC` (class), `ic/ac` (comment), `iv/av` (conditional), `il/al`
(loop), `ia/aa` (parameter/call), `ib/ab` (any block), `ix/ax` (XML/HTML
attribute), `ii/ai`, `iI/aI`, and `iJ/aJ` (indentation levels).

## Packages

### The manifest

`ar/packages` in `init.el` is grouped by the module that configures each entry:

```elisp
(defvar ar/packages
  '(;; core
    gcmh compat nerd-icons posframe general projectile buffer-terminator
    ;; editor
    evil evil-collection evil-escape ...
    ;; ui
    doom-themes solaire-mode hl-todo ...
    ;; completion
    vertico orderless marginalia consult embark ...
    ;; org
    org-contrib org-modern org-appear ... denote ...
    ;; dev
    lsp-mode lsp-ui consult-lsp ...
    ;; vc-projects
    magit forge code-review ... dirvish ...
    ;; tools
    ein quickrun eros dumb-jump request gptel ...
    ;; lang
    csv-mode json-mode ... auctex ... nix-mode ... python ...
    ;; keybinds & terminal
    ace-window ghostel evil-ghostel))
```

Each entry is either a package symbol (resolved by straight's default recipe
repositories) or an explicit recipe:

```elisp
(evil-ts-obj :host github :repo "dvzubarev/evil-ts-obj"
             :files ("lisp/*.el" "README.org"))
```

Several Codeberg-hosted packages are
redirected to their emacsmirror GitHub mirrors because of Codeberg's
smart-HTTP rate limiting on some networks. `ws-butler` is isolated from the
shared NonGNU ELPA mirror with a dedicated `:local-repo`.

### Bundled libraries

Before installing the manifest, `init.el` registers these libraries as
`:type built-in`:

```elisp
compat eldoc external-completion flymake jsonrpc
org project seq transient xref
```

This prevents straight from rebuilding bundled libraries when a package's
`Package-Requires` mentions them; the Emacs build's own versions stay on
`load-path`.

### Add a package

1. Add the package to the matching group in `ar/packages` in `init.el`.
   For a git-only package, write an explicit recipe with `:host` and `:repo`.
2. Add a `use-package` block in the module that owns the feature. Configure it;
   do not install it.
3. Restart Emacs. Manifest changes are not picked up by `SPC f l`.
4. Run the regression checks if the package touches startup, hooks, or
   cross-module contracts.

A simple deferred addition looks like:

```elisp
(use-package my-package
  :defer t
  :commands (my-package-command)
  :custom
  (my-package-option t))
```

### Remove a package

Remove both the manifest entry and its `use-package` block. Restart, then run
`M-x straight-prune-build` to delete the orphaned build directory. Removing a
package without removing its configuration is how "void-function on startup"
happens.

### Bytecode cache

`init.el` checks every module source against `var/bytecode/`. If **every**
cached `.elc` is current, `var/bytecode/` is added to `load-path`; one stale
file withholds the whole cache and the source files load instead. An idle timer
then runs a batch Emacs subprocess to compile the stale modules. The output goes
to `*ar-byte-compile*`, and the session never blocks.

`SPC f l` always reloads from `lisp/` source, never from the cache. If you
suspect the cache is lying to you, delete `var/bytecode/` and restart.

## Customization

### Where to put personal settings

`etc/custom.el` is the `custom-file` and is loaded after package installation
but before the feature modules. Use it for:

- Customize-written variables,
- personal identity changes (the defaults live in `core-config`),
- one-off package settings that modules do not already own.

Module-specific settings belong in the module that owns the package. The
modules are intentionally the source of truth; `custom.el` is not a second
configuration layer.

### Change the theme or fonts

Theme and typography are first-paint work, so they live in `ui-config`:

```elisp
;; lisp/ui-config.el
(load-theme 'doom-tokyo-night t)
```

Fonts are applied by `ar/setup-fonts`:

```elisp
(set-face-attribute 'default nil :font "JetBrains Mono" :height 125)
```

The function runs at startup and again on `server-after-make-frame-hook`, so
daemon client frames get the same fonts. Keep global zoom (`C-=`, `C--`,
`C-0`) in mind: it uses `global-text-scale-adjust` and deliberately never
resizes frames on PGTK/Wayland.

### Add a keybinding

Use the module that owns the command. The leader macros are defined in
`editor-config` and are available to every later module:

```elisp
(ar/global-leader
  "x"   '(:ignore t :wk "my prefix")
  "x x" '(my-command :wk "Do the thing"))
```

Global, cross-module bindings belong in `keybinds-config`. The local-leader
prefix (`SPC m`) holds buffer-local actions: `org-mode-map` (todo, schedule,
refile, archive, clock, export) and `python-base-mode-map` (ruff fixes and
Jupyter evaluation). Those two blocks use `general-define-key` with the prefix
spelled out, because the package-free test suite stubs it.

### Snippets

Personal snippets go in `snippets/`. YASnippet loads that directory first, so a
personal snippet shadows a Doom snippet with the same name. Saving a file under
`snippets/` reloads the collection automatically.

### Popup rules

The popup manager is rule-based. Add or change entries in
`ar/popup--rules` in `ui-config`:

```elisp
("^\\*My Output\\*$" :size 0.25 :select nil :quit t)
```

Matchers may be regexps or major-mode symbols. Supported properties include
`:size`, `:side`, `:select`, `:quit`, and `:ignore`.
`C-`` toggles the last popup; `C-g` closes the topmost dismissible popup.

### Dashboard

The dashboard is a custom `special-mode` buffer named `*ahsan*`. Its menu is
`ar/dashboard-menu-sections`; its widgets are `ar/dashboard-functions`. The
splash image is `etc/splash-image.png` when that file exists. "Reload last
session" calls `ar/dashboard-load-session`, which loads
`var/persp/persp-auto-save`. Session auto-resume is intentionally off.

### Large-file guard

Expensive minor modes use `ar/define-large-file-guard`. The guard refuses
activation above **500,000 characters** or **10,000 lines**. A negative or zero
prefix argument bypasses it:

```elisp
(ar/define-large-file-guard my-expensive-mode)
```

The guard is applied to whitespace, line numbers, Jinx, colorful-mode,
rainbow-delimiters, hideshow, vimish-fold, treesit-fold, indent-bars, blamer,
org-modern, evil-ts-obj, and LSP inlay hints.

## Org and Denote

### Where notes live

`ar/org-directory` is:

```text
$XDG_DOCUMENTS_DIR/org/     if XDG_DOCUMENTS_DIR is set and exists
~/org/                       otherwise
```

The directory is truenamed. The config ensures:

```text
org/
├── zettelkasten/     ; fallback Denote directory; evergreen concepts
├── journal/          ; Denote journal entries
├── attachments/      ; Org attachments
├── downloads/        ; downloads
└── projects/         ; optional, created per project
```

`SPC n S` manages silos. A silo is a subdirectory with its own
`.dir-locals.el` containing a `denote-directory` and a Denote Org front-matter
template, including the signature field. Because Org export ignores
directory-local variables, the config enables `org-export-allow-bind-keywords`
and writes `#+bind: denote-directory "…"` into each silo. `denote-solo`
provides separate "Zettelkasten" and "Journal" contexts.

Create a project silo with:

```elisp
M-x ar/create-denote-project-silo RET project-name RET
```

The name must be a single path component; escaping names are rejected.

### Org behavior

- Org is deferred until the first Org buffer.
- `org-startup-folded` is `fold` (an overview); inline images and LaTeX previews are off
  at startup.
- `org-modern` and `org-appear` prettify headings, blocks, checkboxes, and
  emphasis markers.
- `toc-org-mode` adds a table of contents.
- `evil-org` and `evil-org-agenda` provide modal bindings.
- Babel languages are loaded on demand by advice around export, source
  editing, and evaluation. `org-babel-do-load-languages` is overridden because
  the lazy loader registers languages itself.
- `org-habit` loads with Org, not at startup.
- Task flow: states `TODO`, `NEXT`, `WAIT`, `DONE`, `CANCEL`; capture lands in
  `inbox.org` and is refiled (`SPC m r`) to `tasks.org` or a project note.
  Agenda files are `inbox.org`, `tasks.org` and every `.org` file under
  `projects/` (scanned at startup).
- Capture templates: `t` task, `T` task with link, `n` Denote note, `j` Denote
  journal entry.
- Agenda: `SPC o A` / `SPC o a d` opens the dashboard (today, next actions,
  waiting, inbox); the dispatcher also has `n`, `w` and `r` (weekly review).
- Clocking persists across restarts (`SPC o k`).

### Denote packages

The second brain is 13 Denote packages plus `consult-denote`:

`denote`, `denote-silo`, `denote-sequence`, `denote-org`, `denote-journal`,
`denote-journal-capture`, `denote-menu`, `denote-review`, `denote-regexp`,
`denote-solo`, `denote-explore`, `denote-merge`, `denote-wordcloud`, and
`consult-denote`.

Denote uses Org files, infers and sorts keywords, renames buffers, and inserts
a minimal atomic-note skeleton after creating a note in a Denote directory:
`* Summary`, `* Connections`, and `* Sources`.

## Development stack

### Tree-sitter

`dev-config` points `treesit-extra-load-path` at `var/tree-sitter/`, enables
grammar auto-installation, and registers commit-pinned grammar sources for
LaTeX, BibTeX, Nix, Make, Awk and Org. Languages Emacs 31's own modes register
(Python, Bash, C, C++, CSS, HTML, JSON, Java, Markdown, ...) are left alone so
their pinned sources apply. Font-lock level is 4.

Folding is routed through `treesit-fold` globally, with hideshow and
vimish-fold as fallbacks. `expreg`, `evil-textobj-tree-sitter`, and
`evil-ts-obj` provide structural selection, text objects, and refactoring.

### LSP

`ar/lsp-ensure-maybe` starts LSP from `prog-mode-hook` for every mode except
Emacs Lisp, Lisp, Makefile, snippet, and ron. Interactive sessions use
`lsp-deferred`; batch sessions import the buffer directory as a workspace and
call `lsp` directly.

`LSP_USE_PLISTS=true` is set in `early-init.el` **before** lsp-mode is built.
lsp-mode records the compile-time value; if it does not match the runtime
value, the config warns. After changing the variable, rebuild the package:

```elisp
M-x straight-rebuild-package RET lsp-mode RET
```

lsp-ui adds hover docs, sideline diagnostics, and peek. `consult-lsp` provides
workspace symbols; `lsp-treemacs` provides problems and symbol views. If
`emacs-lsp-booster` is installed, JSON parsing and server commands are wrapped
automatically.

### Diagnostics and formatting

Flymake replaces Flycheck. `flymake-show-diagnostics-at-end-of-line` is set to
`fancy`, and `flymake-popon` shows the diagnostic at point. Apheleia runs
formatters asynchronously on save; Python uses `ruff-isort` and `ruff`.

### Python and Jupyter

Python buffers start two language servers through lsp-mode: `ty-ls` (types,
navigation, completion) and `ruff` (an add-on client for lint diagnostics, fix
all and organize imports). Rule selection lives in `pyproject.toml`. `SPC m`
in a Python buffer offers ruff fix-all, organize imports and emacs-jupyter:
run or connect a REPL, evaluate line, region, defun or buffer, inspect, restart
or interrupt the kernel. `jupyter-python` Babel blocks default to
`:async yes :session py`; the kernel comes from the installed kernelspec.

### Debugging

Dape is configured for DAP debugging with a GDB-style buffer arrangement and
the project root as its working directory. `SPC c d` exposes start, breakpoint,
continue, stepping, restart, quit, info, and REPL commands.

### Language coverage

| Area | Main pieces |
| --- | --- |
| Data/config | `csv-mode`, `json-mode` + `json-snatcher`, `yaml-mode` + built-in `yaml-ts-mode`, `nix-mode` + `nix-ts-mode` |
| Emacs Lisp | `elisp-def`, `macrostep`, `overseer`, `buttercup`, `package-lint-flymake`, `let-completion` |
| LaTeX | AUCTeX, `auctex-cont-latexmk`, CDLaTeX, `evil-tex`, TeX-fold, RefTeX |
| Markdown | `markdown-mode`/`gfm-mode`, `markdown-toc`, `edit-indirect`, `evil-markdown` |
| Python | built-in `python`/`python-ts-mode`, `pyvenv`, `pipenv`, `conda`, `python-pytest`, IPython |
| Qt/QML | `qml-mode`, `qml-ts-mode`, `qt-pro-mode` |
| Shell | built-in `sh-mode`/`bash-ts-mode`, `bash-completion` |
| Web | `web-mode`, Emmet, Haml/Pug/Slim/Sass/Stylus/SWS modes, `rainbow-mode` |

LSP auto-start is language-agnostic; install the server and it will be picked
up by the corresponding major mode.

## User interface

- **Theme:** `doom-tokyo-night`, with solaire-mode swapping backgrounds in side
  windows and popups.
- **Modeline:** doom-modeline with modal state, major-mode icon, LSP, VCS,
  project, and persp segments.
- **Icons:** Nerd Icons using `Symbols Nerd Font Mono`.
- **Typography:** JetBrains Mono at height 125, with Noto Sans fallbacks for
  geometric shapes and dingbats.
- **Dashboard:** splash image, menu, package/module count, startup time, and a
  footer link.
- **Popups:** a port of Doom's rule-based popup manager. Transient output,
  help, diagnostics, shells, LSP buffers, and VC buffers open as side windows
  with per-rule sizing and dismissal behavior.
- **Folding:** `za`/`zo`/`zc` route to treesit-fold, outline, hideshow, or
  vimish-fold, whichever is active.
- **VC gutter:** diff-hl with thin custom fringe bitmaps.
- **Color tooling:** colorful-mode previews colors in code; rainbow-mode
  colorizes color literals in CSS/HTML; rainbow-delimiters colorizes nesting
  depth.
- **TODO highlights:** hl-todo keywords (`TODO`, `FIXME`, `BUG`, `NOTE`,
  `HACK`, etc.) with Tokyo Night colors and `C-c n`/`C-c p` navigation.
- **Zoom:** `C-=`, `C--`, `C-0`, and `C-wheel-up`/`C-wheel-down` scale text
  globally; frames never resize.

## Performance

| Mechanism | Detail |
| --- | --- |
| Startup GC | `gc-cons-threshold` is `most-positive-fixnum` interactively and 256 MiB in batch; restored to 64 MiB at `emacs-startup-hook`. |
| Steady-state GC | gcmh with a 64 MiB high threshold and an automatic idle delay (`idle-delay 'auto`, factor 10). |
| Process I/O | `read-process-output-max` is 4 MiB for JSON-RPC and ripgrep. |
| Long lines | `long-line-threshold`, `large-hscroll-threshold`, and `syntax-wholeline-max` are 1000; bidi paragraph direction is LTR and BPA is inhibited. |
| Redisplay | `jit-lock-defer-time` is 0.05, `auto-window-vscroll` is nil, pixel scrolling is enabled without synthetic momentum. |
| Large files | Modes gate through the 500k-character / 10k-line guard. |
| Native compilation | Async warning popups are silenced; `var/eln-cache/` is the cache; the root `eln-cache` symlink keeps generated files out of the config directory. |
| Bytecode | All-or-nothing current-cache check; stale modules compile in a batch subprocess after 8 seconds idle. |
| Lazy loading | `use-package` autoloads, hooks, and `with-eval-after-load`; only first-paint packages are eager. |
| LSP | Optional `emacs-lsp-booster`; plist serialization is compiled in. |
| Session hygiene | buffer-terminator kills inactive buried buffers after 30 minutes; recentf and savehist flush periodically for daemon safety. |

No startup benchmark is committed. The dashboard displays the current session's
startup time, which is the number to compare after changes.

## State and persistence

| State | Location | Behavior |
| --- | --- | --- |
| Straight repos/builds | `var/straight/` | Cloned and built on demand. |
| Native compilation | `var/eln-cache/` | Root `eln-cache` is a symlink here. |
| Module bytecode | `var/bytecode/` | Used only if every `.elc` is current. |
| Tree-sitter grammars | `var/tree-sitter/` | Compiled grammar libraries. |
| Recent files | `var/recentf-save.el` | Remote paths and the config/state trees excluded; periodic flush every 5 minutes. |
| Minibuffer history | `var/savehist.el` | 1000 entries; autosave every 5 minutes. |
| Cursor positions | `var/places` | Unreadable/offline files forgotten. |
| Bookmarks | `var/bookmarks` | Standard bookmark file. |
| Undo history | `var/undo-fu-session/` | Persistent, linear, 500 files; zstd/gzip when available. |
| Workspaces | `var/persp/` | Auto-resume off; explicit save/load. |
| Org caches | `var/org/` | persist, LaTeX images, publish timestamps, ID locations. |
| Dape | `var/` | Debugger state. |
| Dirvish | `var/dirvish/` | Preview/cache state. |
| Treemacs | `var/treemacs/` | Persisted tree state. |
| Transient | `var/transient/` | History, values, levels. |
| Eshell | `var/eshell/` | Eshell history and state. |
| TRAMP | `var/tramp`, `var/tramp-autosave/` | Persistency and auto-save. |
| URL/NSM | `var/url/`, `var/network-security.data` | URL cache and network security settings. |
| Custom file | `etc/custom.el` | User-editable. |
| Abbreviations | `etc/abbrev_defs` | User-editable. |
| Project list | `etc/projects.el` | project.el's known projects. |

Backups and auto-save files are disabled (`make-backup-files nil`,
`auto-save-default nil`) in favor of Super Save, which saves after 10 seconds
idle and on window/navigation changes. The routing for backups and auto-saves
still exists in `early-init.el` for anyone who re-enables them.

**Session restore is explicit.** Startup never reopens files. Use the dashboard
button or `SPC q L`; the saved state is `var/persp/persp-auto-save`.

## Testing

The repository contains a package-free ERT suite in
`tests/config-regressions.el`. It stubs `use-package`, `ar/global-leader`, and
`general-define-key`, loads the real modules, and checks contracts that are
easy to break:

- popup rules ignore Org capture buffers;
- comint popups stay writable and restore modelines;
- popup quit advice follows global-mode state;
- transient menus exist on first use;
- Python buffers activate a project `.venv`;
- LSP inlay hints carry the large-file guard;
- Denote silos reject path-escaping names;
- Babel lazy loading caches only successful loads;
- workspace switching prompts for one project root;
- recentf's flush timer is not scheduled in batch.

Run from the repository root:

```bash
emacs -Q --batch \
  -L . \
  -l tests/config-regressions.el \
  -f ert-run-tests-batch-and-exit
```

The suite is intentionally package-free: it does not install the 187-package
manifest and does not need network access. For a full startup check, start
interactive Emacs with `--debug-init` and watch the dashboard's package/module
counts.

When changing a module, also verify that the file byte-compiles without
warnings. The configuration is expected to stay byte-compile clean (no free
variables; cross-module references get `declare-function`/`defvar` first).

## Troubleshooting

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| Startup aborts with `Package bootstrap FAILED (N)` | Network, TLS, or a bad recipe; the error lists every failure | Fix the named package's access, delete its `var/straight/repos/<name>`, restart. Use `emacs --debug-init`. |
| Icons render as boxes | Nerd Font not installed or font family mismatch | `M-x nerd-icons-install-fonts`, restart the frame; verify `Symbols Nerd Font Mono`. |
| LSP warns about `LSP_USE_PLISTS` mismatch | lsp-mode was built before `LSP_USE_PLISTS=true` | `M-x straight-rebuild-package RET lsp-mode RET`, restart. |
| PDFs show blank pages | `epdfinfo` was never built | `M-x pdf-tools-install`, restart. |
| Jinx does not highlight misspellings | Enchant/native module unavailable | Install Enchant + dictionary; test `(jinx-mode 1)` in a text buffer. The config disables Jinx rather than raising on text buffers. |
| Straight build symlinks are broken | The checkout was moved or built from a different mount path | Delete `var/straight/build/`, restart; straight rebuilds. Do not run the config against a foreign mount path. |
| Old code runs after an edit | The bytecode cache is stale | Delete `var/bytecode/`, restart; the idle compiler rebuilds it. `SPC f l` bypasses the cache. |
| The previous session did not reopen | By design | Use the dashboard's "Reload last session" or `SPC q L`. |
| Remote Dired listing is blank | Remote host lacks GNU `ls` | The config strips GNU-only flags over TRAMP; verify the SSH host and `ls`. Only Git is enabled in `vc-handled-backends` to avoid VCS stalls. |
| Daemon client frames use the wrong font | Fonts are per-frame | `ar/setup-fonts` runs on `server-after-make-frame-hook`; ensure the font exists on the daemon host. |
| `ghostel` fails to start | Native terminal dependency missing | It is deferred and should not block startup. Install Ghostel's native dependency or ignore the terminal feature. |
| `code-review` fails on a database class | code-review predates emacsql 4.x | The config aliases the old `emacsql-sqlite-connection` class onto the built-in backend before loading; update packages if the shim no longer applies. |
| `git-timemachine` misbehaves under native compilation | JIT compilation of this package is unreliable | It is already on `native-comp-jit-compilation-deny-list`; restart after changing the list. |
| Agenda is empty | The agenda reads `inbox.org`, `tasks.org` and `projects/**.org` under the org directory | Capture a `TODO` (`SPC X`, `t`) or add files; restart after creating a new project silo. |
| Search/project commands cannot find `rg` | ripgrep not on `exec-path` | Install ripgrep; check `(executable-find "rg")`. GUI Emacs must inherit your PATH. |

### Recovering a broken install

```bash
# Rebuild the package tree (keeps your notes and settings)
rm -rf var/straight
emacs

# Rebuild only the module cache
rm -rf var/bytecode
emacs

# Full machine-state reset (does NOT delete Org/Denote files outside the repo)
rm -rf var
emacs
```

`var/` contains histories, workspaces, persistent undo, and caches — deleting it
is safe but loses those conveniences. `etc/` contains your settings; back it up.

## Security notes

This configuration is optimized for a trusted single-user workstation. A few
settings intentionally trade confirmation for speed:

- **Org Babel evaluates without confirmation.**
  `org-confirm-babel-evaluate` is `nil`. Only open and execute Org files you
  trust.
- **envrc runs direnv.** `envrc-global-mode` is enabled after init; entering a
  directory with an `.envrc` can execute shell code. Trust directories
  explicitly with `direnv allow`.
- **gptel sends data to a provider.** Chat, region, and buffer contents leave
  Emacs according to your backend and directives. Keep secrets out of those
  buffers.
- **sudo-edit uses TRAMP.** `SPC f E` reopens the current file with elevated
  privileges; the usual TRAMP/sudo caveats apply.
- **No version locking.** Package versions follow upstream, so review changes
  after `straight-pull-all`.

## Conventions

These are the rules the configuration follows; keep them when extending it.

- **File headers:** `;;; name.el --- summary -*- lexical-binding: t; -*-`, a
  Commentary section, a Code section, and `(provide 'name)`.
- **Names:** user-facing symbols are prefixed `ar/`; defcustoms join the `ar`
  group (or a subgroup such as `ar/popup` or `ar/dashboard`).
- **Module names:** `<topic>-config.el`, so a module never shadows a bundled
  library on `load-path`.
- **Installation vs configuration:** `init.el` installs; modules configure.
  A package needs both a manifest entry and a `use-package` block.
- **Cross-module references:** add `declare-function` or `defvar` before first
  use, so byte-compilation stays warning-free.
- **Deferral:** prefer `:commands`, `:bind`, `:hook`, `with-eval-after-load`,
  and idle timers. Demand-load only first-paint needs.
- **State routing:** machine-written files go through `ar/var-file`; readable
  user-editable files go through `ar/etc-file`.
- **Batch safety:** guard display-only work with `noninteractive`; do not rely
  on idle timers or `after-init-hook` in batch.
- **Large buffers:** expensive modes get `ar/define-large-file-guard`.
- **Git-only packages:** explicit recipes with `:host` and `:repo`; do not rely
  on an implicit default recipe.
- **Reload vs restart:** `SPC f l` reloads module source; restart for manifest
  changes, removed hooks, or package upgrades.

`CLAUDE.md` is the short operating manual for coding agents working in this
repository.

## Known limitations

- **Agenda project notes load once.** Files under `projects/` join the agenda at
  startup, so a note in a new project silo appears after a restart.
- **No CI configuration is included.** The ERT suite is local; there is no
  workflow file in this checkout.
- **Platform coverage is uneven.** Linux/PGTK/Wayland is the primary target.
  Windows and macOS are not regularly tested.
- **Optional tools are optional.** Missing language servers, formatters,
  debug adapters, API keys, `epdfinfo`, or Ghostel's native library degrade
  individual features, not startup.

## Credits and licensing

This configuration stands on the work of many package authors. In particular:

- [GNU Emacs](https://www.gnu.org/software/emacs/) — the editor.
- [straight.el](https://github.com/radian-software/straight.el) — package
  management and reproducible Git checkouts.
- [Doom Emacs](https://github.com/doomemacs/doomemacs) — the design reference
  for the leader layout, defaults, dashboard, popup manager, and several Evil
  plugins. The popup manager and dashboard here are ports/adaptations of Doom
  code.
- [Evil](https://github.com/emacs-evil/evil) and the Evil ecosystem.
- [Denote](https://protesilaos.com/emacs/denote) — the note-taking model.
- [Org mode](https://orgmode.org/) — documents, agenda, Babel, and export.
- [Vertico](https://github.com/minad/vertico),
  [Consult](https://github.com/minad/consult),
  [Embark](https://github.com/oantolin/embark),
  [Corfu](https://github.com/minad/corfu), and
  [Cape](https://github.com/minad/cape) — completion.
- [Magit](https://magit.vc/) — Git porcelain.
- [Dirvish](https://github.com/alexluigit/dirvish) — Dired replacement.
- [Ghostel](https://github.com/dakra/ghostel) — terminal emulator.
- [nerd-icons](https://github.com/rainstormstudio/nerd-icons.el) — icon set.
- [gptel](https://github.com/karthink/gptel) — LLM integration.

This is a personal configuration, not a distribution.

It is released under the **GNU General Public License, version 3 or later**
(GPL-3.0-or-later). See [`LICENSE`](LICENSE) for the full text.

Portions of the UI code (notably the popup manager and dashboard) are adapted
from Doom Emacs. Doom Emacs is MIT-licensed, not GPL; its copyright and
permission notice is retained in [`NOTICE`](NOTICE). Keep that notice when
redistributing adapted code.
