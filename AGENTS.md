# AGENTS.md — Emacs 31.1 configuration (vanilla · straight.el · Evil · Denote)

Operating manual for coding agents working in this repository. This config is
being **rebuilt from scratch**: the working tree currently contains only the
spec documents; the Elisp files must be (re)created following the rules below.

---

## 1. What this repo is

- A **vanilla GNU Emacs configuration** targeting **Emacs 31.1** (PGTK/Wayland,
  native-comp, tree-sitter built in). It is *not* a Doom Emacs config at
  runtime, but its feature set is specified by Doom: `init.md` holds the
  `doom!` block, and **`PROPERTIES.md` is the authoritative, final merged
  spec** (≈186 packages) — read it before any work.
- Architecture is strictly modular: **all feature code lives in `lisp/`
  modules, sourced by a central `init.el`; `early-init.el` handles everything
  that must precede the first frame.**
- Personal identity (`user-full-name` / `user-mail-address`) is set in the
  core module. Never change it or print it into reports.

## 2. Target layout

```
<config-root>/            ; ar/config-dir
├── early-init.el         ; pre-frame: GC deferral, package.el off, dir routing
├── init.el               ; straight bootstrap, ar/packages manifest, module loader
├── lisp/                 ; ALL feature modules (*-config.el, required in order)
│   ├── core-config.el          ← 1  helpers, defaults, gcmh, persistence
│   ├── editor-config.el        ← 2  evil stack (Doom suite), leaders, editing aids
│   ├── ui-config.el            ← 3  theme, modeline, popup manager, dashboard, folding
│   ├── completion-config.el    ← 4  vertico/consult/embark, corfu/cape, wgrep
│   ├── org-config.el           ← 5  Doom-default org + the Denote ecosystem
│   ├── dev-config.el           ← 6  treesit, lsp-mode + lsp-ui, flymake, dape, apheleia
│   ├── tools-config.el         ← 7  ein, quickrun/eros, lookup, gptel, pdf-tools
│   ├── lang-config.el          ← 8  latex, nix, qt, web, yaml, json, data, sh, python
│   ├── vc-projects-config.el   ← 9  magit/forge, dired/dirvish, treemacs, persp, ghostel
│   └── keybinds-config.el      ← 10 escape plumbing, leaders, windows, which-key
├── snippets/             ; personal yasnippet collection (user content)
├── etc/                  ; user-editable state (custom.el, abbrev_defs, projects.el)
└── var/                  ; machine state — user-emacs-directory points here (gitignored)
    ├── straight/{repos,build}/  eln-cache/  bytecode/  tree-sitter/
    └── per-package state (undo-fu-session, dape, dirvish, treemacs, org, …)
```

## 3. Architecture boundaries (hard rules)

1. **`init.el` is the only installer and the only loader.** Every external
   package is listed in `ar/packages` (grouped by module, fail-loud bootstrap
   that collects all straight failures before erroring). `use-package` forms
   in modules are declarative-only — they configure, never install
   (`straight-use-package-by-default` stays nil; `package-enable-at-startup`
   is nil).
2. **Module load order is load-bearing**: `core-config` → `editor-config` →
   `ui-config` → `completion-config` → `org-config` → `dev-config` →
   `tools-config` → `lang-config` → `vc-projects-config` → `keybinds-config`.
   core defines shared helpers (`too-long-file-p`, `ar/define-large-file-guard`,
   `with-eval-after-load-all`, `ar/escape-key`, `reload-init-file`); editor
   defines the general.el leader definers (`ar/global-leader` = SPC/C-SPC,
   `ar/local-leader` = SPC m); keybinds loads last and binds across modules.
   Do not reorder or create hidden load dependencies.
3. **Module files are named `<topic>-config.el`** so they never shadow a
   bundled library on `load-path` (a module named `org.el` once broke startup).
4. **State routing:** machine-written files go to `var/` (`ar/var-file`),
   user-editable files to `etc/` (`ar/etc-file`). `early-init.el` swaps
   `user-emacs-directory` to `var/` and routes backups/autosave/recentf/
   savehist/bookmarks/transient/tramp/eshell. Never let a package write into
   the config root; the one exception (`eln-cache`) is a symlink into `var/`.
5. **Batch-safe by design:** guard display-only work on `noninteractive`;
   rely on the fact that idle timers and `after-init-hook` never fire in
   `emacs --batch` (smoke tests stay package-free). Every hook target must be
   a defined top-level defun.
6. **Large-file guard:** any expensive minor mode (fontification-heavy, line
   numbers, folds, spell, blame, inlay hints) must gate activation through
   `ar/define-large-file-guard` (>500k chars or >10k lines; negative prefix
   arg bypasses).

## 4. Spec decisions that override `init.md` (from PROPERTIES.md §1)

These are settled — do not "fix" them back toward Doom defaults:

- **Denote ecosystem** is the second brain (13 packages + consult-denote).
  org-roam and the `+roam` flag are **not** honored.
- **jinx** spell checking is kept (Enchant) even though `:checkers spell` is
  disabled in `init.md`.
- **electric-pair + custom prose pairs / delete-pair DWIM** — no smartparens
  (`+smartparens` off).
- **persp-mode** workspaces (+ treemacs-persp) — no tab-bar/bufferlo stack.
- **Doom's rule-based popup manager is ported** into the vanilla config (no
  popper). It is self-contained in upstream `:ui popup` but normally rides on
  Doom core helpers — the port is real work, not a copy-paste.
- **lsp-ui + lsp-treemacs** for LSP hover/panels (treemacs `+lsp` on); no
  eldoc-box.
- **Org capture/agenda use Doom defaults** — the old custom capture dispatcher,
  org-super-agenda views, org-transclusion, universal-sidecar are gone.
- **Projects:** built-in `project.el` backbone + **Projectile** compat layer
  (as upstream Doom: `core/modules/doom/compat/+projectile.el`; arrives
  transitively via `ibuffer-projectile`/`treemacs-projectile`).
- Kept vanilla personality: evil-multiedit/iedit, expreg, evil-ts-obj +
  evil-textobj-tree-sitter, ws-butler, super-save, sudo-edit, ace-window,
  general.el leaders, blamer, magit-todos, colorful-mode, rainbow-delimiters,
  posframe, pixel-scroll animation, hide-modeline, buffer-terminator,
  evil-nerd-commenter + the 7 Doom evil plugins (escape, indent-plus, traces,
  visualstar, textobj-anyblock, exato, quick-diff).

## 5. Emacs conventions (required)

- Every file: `;;; name.el --- summary -*- lexical-binding: t; -*-` header,
  `;;; Commentary:`, `;;; Code:`, and a `(provide 'name)` footer.
- **Byte-compile clean**: no free-variable or "not known to be defined"
  warnings. Cross-module references use `declare-function` / `defvar` placed
  *before* first use. defcustoms join the `ar` customization group; user
  symbols are prefixed `ar/`.
- Defer by default: `:commands`/autoloads, `with-eval-after-load`, mode hooks,
  idle timers. Demand-load (`:demand t`) only what the first paint needs
  (theme, modeline, icons, evil, vertico).
- Prefer Emacs 31.1 built-ins (`project`, `flymake`, `treesit`, `use-package`,
  `which-key`, `editorconfig`, `hideshow`) over new packages. Adding a package
  requires BOTH a manifest entry in `init.el` and a configuring block in the
  owning module — one without the other is a bug.
- Custom faces/colors use the **Tokyo Night** palette (`doom-tokyo-night`);
  keep new faces theme-consistent. Fonts: JetBrains Mono + Symbols Nerd Font
  Mono + Noto Sans fallbacks.
- git-only packages come as explicit straight recipes with pins (evil-ts-obj,
  ws-butler, denote-merge). Respect upstream pins — they are
  deliberate.

## 6. Commands

```bash
emacs --version                                              # must be 31.1 — say so loudly if not

./tests/run.sh                                               # full test harness (distrobox arch-box, Emacs 31.1):
                                                             #   gate 1   bootstrap smoke (fail-loud, non-zero on failure)
                                                             #   gate 1.5 jinx native module compile
                                                             #   gate 2   byte-compile (warning-free)
                                                             #   gate 2.5 deferred force-load (every manifest package; one
                                                             #            documented environment exception: org-pdftools
                                                             #            without a built epdfinfo server)
                                                             #   gate 3   ERT user journeys (38 in-suite + 1 isolated)
```

- Interactive config reload: `SPC f l` (`reload-init-file`) re-evaluates all
  modules from source; **restart Emacs for package-manifest changes**.
- There is no `doom` CLI here — `init.md` is a spec reference, not a build
  system.

## 7. Known gotchas

- **PGTK/Wayland:** GTK IM context disabled per frame; system tooltips and
  dialog boxes off (daemon hangs); GPG pinentry `loopback`; `scroll-margin`
  must stay 0 (pixel-scroll animation); global zoom must never resize frames.
- **Session restore:** persp auto-resume is deliberately OFF — startup must
  never reopen files on its own; the last session is restored explicitly via
  the dashboard's "Reload last session" button (`ar/dashboard-load-session`).
- **Daemon:** the server is started by systemd/emacsclient, never from the
  config; fonts must re-apply on `server-after-make-frame-hook`.
- **Bytecode cache:** `var/bytecode/` is used only when *every* cached `.elc`
  is current; one stale entry withholds the whole cache for the session
  (source loads instead; an idle timer recompiles in a batch subprocess).
- **straight build symlinks are absolute** into the creating Emacs's
  `user-emacs-directory`. A `var/straight/build/` produced under a different
  mount path (e.g. a container running as root) is all broken links and
  kills startup in eager macroexpansion —
  `evil-textobj-tree-sitter-core.el` computes a defconst from
  `(locate-library "evil-textobj-tree-sitter.el")`, which returns nil for
  broken links (`wrong-type-argument stringp nil`). Never run the harness
  against a mount path that differs from the deployment's; `tests/run.sh`
  rebuilds the build dir when it detects a broken symlink.
- **TRAMP:** recentf excludes remote entries; Dired strips GNU `ls` flags over
  SSH; ssh uses direct-async processes. Only `Git` is in `vc-handled-backends`
  (probing other VCSes over TRAMP stalls).
- **Org state** lives under `var/org/` (persist, ltximg, timestamps,
  id-locations); user Org files live under `$XDG_DOCUMENTS_DIR/org/` (fallback
  `~/org/`), with the Zettelkasten silo in `zettelkasten/`.

## 8. Read before touching

- **`PROPERTIES.md`** — the full feature/package spec: framework parity (§2),
  module inventory (§3), per-category capabilities (§4), the package manifest
  (§5), removed-by-decision list (§6), system dependencies (§7).
- **`init.md`** — the Doom `doom!` block the spec was derived from (plus §4
  of this file for the deliberate divergences).
- Verification ethos: evidence beats "looks right" — run the byte-compile
  gate and a batch smoke load before claiming done, and report actual output.
