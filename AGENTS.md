# AGENTS.md — Emacs 31.1 config (vanilla · straight.el · Evil · Denote)

Operating manual for coding agents. `PROPERTIES.md` is the authoritative spec (≈186 packages); `init.md` holds the Doom `doom!` block it was derived from — read both before any work. There is no `doom` CLI.

## Layout

```
<config-root>/            ; ar/config-dir
├── early-init.el         ; pre-frame: GC deferral, package.el off, dir routing
├── init.el               ; straight bootstrap, ar/packages manifest, module loader
├── lisp/*-config.el      ; ALL feature modules (load order below)
│   ├── core-config.el        ← 1  helpers, defaults, gcmh, persistence
│   ├── editor-config.el      ← 2  evil stack, leaders, editing aids
│   ├── ui-config.el          ← 3  theme, modeline, popup manager, dashboard, folding
│   ├── completion-config.el  ← 4  vertico/consult/embark, corfu/cape, wgrep
│   ├── org-config.el         ← 5  Doom-default org + Denote ecosystem
│   ├── dev-config.el         ← 6  treesit, lsp-mode + lsp-ui, flymake, dape, apheleia
│   ├── tools-config.el       ← 7  ein, quickrun/eros, lookup, gptel, pdf-tools
│   ├── lang-config.el        ← 8  latex, nix, qt, web, yaml, json, data, sh, python
│   ├── vc-projects-config.el ← 9  magit/forge, dired/dirvish, treemacs, persp, ghostel
│   └── keybinds-config.el    ← 10 escape plumbing, leaders, windows, which-key
├── snippets/             ; user yasnippets
├── etc/                  ; user-editable state (custom.el, abbrev_defs, projects.el)
└── var/                  ; machine state — user-emacs-directory points here (gitignored)
    ├── straight/{repos,build}/  eln-cache/  bytecode/  tree-sitter/
    └── per-package state (undo-fu-session, dape, dirvish, treemacs, org, …)
```

## Hard rules

1. `init.el` is the **only installer and loader**. Every package is listed in `ar/packages` (fail-loud bootstrap). Module `use-package` forms configure, never install (`straight-use-package-by-default` nil, `package-enable-at-startup` nil).
2. **Load order is load-bearing** (see tree): core defines shared helpers (`too-long-file-p`, `ar/define-large-file-guard`, `with-eval-after-load-all`, `ar/escape-key`, `reload-init-file`); editor defines `ar/global-leader` (SPC/C-SPC) and `ar/local-leader` (SPC m); keybinds loads last and binds across modules. Do not reorder or add hidden load dependencies.
3. Modules are named `<topic>-config.el` so they never shadow a bundled library (a module named `org.el` once broke startup).
4. **State routing:** machine-written files → `var/` (`ar/var-file`), user-editable → `etc/` (`ar/etc-file`); `early-init.el` swaps `user-emacs-directory` to `var/` and routes backups/autosave/recentf/savehist/bookmarks/transient/tramp/eshell. Nothing writes into the config root (exception: the `eln-cache` symlink into `var/`).
5. **Batch-safe:** guard display-only work on `noninteractive`; idle timers and `after-init-hook` never fire in batch. Every hook target is a defined top-level defun.
6. **Large-file guard:** expensive minor modes (fontification, line numbers, folds, spell, blame, inlay hints) gate through `ar/define-large-file-guard` (>500k chars or >10k lines; negative prefix bypasses).

## Settled spec decisions (do not "fix" toward Doom defaults)

- Denote ecosystem is the second brain (13 packages + consult-denote); no org-roam/`+roam`.
- jinx spell checking kept (Enchant) despite `:checkers spell` off in `init.md`.
- electric-pair + prose pairs + delete-pair DWIM; no smartparens.
- persp-mode workspaces (+ treemacs-persp); no tab-bar/bufferlo.
- Doom's popup manager is ported vanilla (no popper).
- lsp-ui + lsp-treemacs; no eldoc-box.
- Org capture/agenda use Doom defaults; no org-super-agenda/org-transclusion.
- project.el backbone + Projectile compat (transitive via ibuffer/treemacs-projectile).
- Kept vanilla extras: evil-multiedit/iedit, expreg, evil-ts-obj, evil-textobj-tree-sitter, ws-butler, super-save, sudo-edit, ace-window, general.el, blamer, magit-todos, colorful-mode, rainbow-delimiters, posframe, pixel-scroll animation, hide-modeline, buffer-terminator, evil-nerd-commenter, 7 Doom evil plugins (escape, indent-plus, traces, visualstar, textobj-anyblock, exato, quick-diff).

## Conventions

- File headers `;;; name.el --- summary -*- lexical-binding: t; -*-` + Commentary/Code + `(provide 'name)`.
- Byte-compile clean: no free-variable warnings; cross-module refs get `declare-function`/`defvar` before first use; user symbols prefixed `ar/`; defcustoms join the `ar` group.
- Defer by default (`:commands`, `with-eval-after-load`, hooks, idle timers); demand-load only first-paint needs (theme, modeline, icons, evil, vertico).
- Prefer Emacs 31.1 built-ins (project, flymake, treesit, use-package, which-key, editorconfig, hideshow). A package needs BOTH a manifest entry and a configuring block — one without the other is a bug.
- Faces/colors follow the Tokyo Night palette (`doom-tokyo-night`). Fonts: JetBrains Mono + Symbols Nerd Font Mono + Noto Sans.
- git-only packages: explicit straight recipes **with `:pin`**; respect upstream pins.
- Personal identity lives in core — never change or print it.

## Commands

```bash
emacs --version    # must be 31.1 — say so loudly if not
./tests/run.sh     # distrobox arch-box harness: gate 1 bootstrap smoke · 1.5 jinx native
                   # module · 2 byte-compile (0 warnings) · 2.5 force-load every manifest
                   # package (one env exception: org-pdftools without built epdfinfo) ·
                   # 3 ERT journeys (38 in-suite + 1 isolated)
```

- `SPC f l` reloads modules from source; restart Emacs for manifest changes.
- Evidence beats "looks right": run the gates, report actual output.

## Gotchas

- **PGTK/Wayland:** GTK IM context off per frame; system tooltips/dialogs off (daemon hangs); GPG pinentry `loopback`; `scroll-margin` stays 0 (pixel-scroll animation); zoom never resizes frames.
- **Session restore:** persp auto-resume is OFF — startup never reopens files; restore via the dashboard's "Reload last session" (`ar/dashboard-load-session`).
- **Daemon:** server comes from systemd/emacsclient, never the config; re-apply fonts on `server-after-make-frame-hook`.
- **Bytecode cache:** `var/bytecode/` is used only when every cached `.elc` is current; one stale entry withholds the whole cache (source loads; idle timer recompiles in a batch subprocess).
- **straight build symlinks are absolute** into the creating Emacs's `user-emacs-directory` — a build dir from a different mount path is all broken links and kills startup (`evil-textobj-tree-sitter-core.el` computes a defconst from `locate-library`). `tests/run.sh` rebuilds the build dir when it detects broken symlinks; never run it against a foreign mount path.
- **TRAMP:** recentf excludes remote; Dired strips GNU `ls` flags over SSH; ssh uses direct-async; only `Git` in `vc-handled-backends` (other VCS probes stall).
- **Org state** in `var/org/`; user Org files in `$XDG_DOCUMENTS_DIR/org/` (fallback `~/org/`), Zettelkasten silo in `zettelkasten/`.
