# AGENTS.md — Emacs 31.1 configuration (straight.el · Evil · lsp-mode · Denote)

Operating manual for any coding agent that tests, debugs or modifies this
configuration. Read this file completely before touching anything. The
reusable procedures (scripts, ERT helpers, runbooks) live in **SKILLS.md**;
this file says *what is true*, *what is forbidden*, and *what "done" means*.

---

## 1. Mission

You must be able to **test and debug every top-level form of every file in
this configuration**, and to **debug the Emacs packages it uses**, both
built-in (`project`, `flymake`, `treesit`, `org`, `transient`, `which-key`,
`use-package`, `editorconfig`, …) and external (everything in `ar/packages`
in `init.el`).

"Every line" is operationalised in §8: each top-level form gets a recorded
status (verified / deferred-with-trigger-verified / blocked-with-reason).
Reading code and judging it "looks right" is **not** verification. Evidence
is a command you ran and the output it produced.

---

## 2. Ground truth about this repository

| Item | Fact |
|---|---|
| Target Emacs | **31.1**, PGTK/Wayland, native-comp, tree-sitter built in. Much of the config depends on 31.1-only behaviour (`treesit-enabled-modes`, built-in `which-key`/`editorconfig`/`use-package`, `show-paren-not-in-comments-or-strings` taking `all`). **Run `emacs --version` first.** If it is not 31.x, say so loudly and treat version-specific results as invalid. |
| Package manager | **straight.el** only. `package-enable-at-startup` is `nil`; package.el never activates. `use-package` is purely declarative (`straight-use-package-by-default` is nil) — it never installs anything. |
| Single install point | `ar/packages` in `init.el`. A package not listed there is not installed, regardless of any `use-package` form. |
| Modal editing | Evil + evil-collection; leaders via general.el: `SPC` / `C-SPC` = `ar/global-leader`, `SPC m` / `C-SPC m` = `ar/local-leader`. |
| IDE | lsp-mode (plist mode, `LSP_USE_PLISTS=true`), flymake, dape, apheleia (ruff for Python), envrc, yasnippet, treesit. Optional `emacs-lsp-booster`. |
| Completion | vertico + orderless + marginalia + consult + embark; corfu + cape for in-buffer. |
| Notes | Org (loaded eagerly) + the Denote family. Files under `$XDG_DOCUMENTS_DIR/org/` or `~/org/`. |
| VC / files / terminal | magit, forge, diff-hl, dirvish, treemacs, project + projection, bufferlo workspaces, ghostel (libghostty-vt) terminal. |
| Platform quirks | PGTK/Wayland workarounds, Arch-style `manpath`, `JetBrains Mono` + `Symbols Nerd Font Mono` + `Noto Sans` fonts. |
| Identity in code | `user-full-name` / `user-mail-address` are set in `core-config.el`. Do not change or print them into reports. |

### 2.1 Expected on-disk layout

The files you are handed may be flat; in the real tree the modules live in
`lisp/`:

```
<config-root>/            ; ar/config-dir (e.g. ~/.config/emacs)
├── early-init.el         ; pre-frame: GC, dirs, env, user-emacs-directory swap
├── init.el               ; straight bootstrap, ar/packages, module loader
├── lisp/
│   ├── core-config.el        ← 1  helpers, defaults, GC, reload-init-file
│   ├── editor-config.el      ← 2  evil stack, leader definers, editing aids
│   ├── ui-config.el          ← 3  themes, fonts, modeline, folding
│   ├── completion-config.el  ← 4  vertico/consult/embark, corfu/cape
│   ├── org-config.el         ← 5  org, capture, agenda, denote
│   ├── dev-config.el         ← 6  treesit, lsp-mode, flymake, dape, apheleia
│   ├── vc-projects-config.el ← 7  magit, dirvish, treemacs, project, ghostel
│   └── keybinds-config.el    ← 8  escape plumbing, windows, which-key, popper
├── etc/                  ; user-editable config (custom.el, abbrev_defs)
├── var/                  ; user-emacs-directory after the swap — machine state
│   ├── straight/{repos,build}/  ├── eln-cache/  ├── bytecode/  ├── tree-sitter/
│   └── backup autosave transient eshell org url undo-fu-session dape …
└── snippets/             ; user content (yasnippet) — NOT machine state
```

The source comments cite design/research documents (`build/design-lsp.md`,
`research2/lsp-facts.md`, `research-packages`, "design §x") and verification
**gates** (**G2** = byte-compile must be warning-clean; **G4** = LSP smoke
test observing `lsp--workspaces`). These documents are not present in this
checkout. If they are added later, read them and prefer them over the
reconstruction here; otherwise use SKILLS.md S1 and S11 for the procedures.

> **Tools and tests are not checked in.** `tools/ar-env.sh`,
> `tools/ar-check-syntax.el`, `tools/ar-inventory.el`, and the ERT suites
> referenced in SKILLS.md are code recipes, not files in this repository.
> Save or generate them before running the commands that reference them.

---

## 3. Boot sequence (memorise; every failure maps to a step)

1. **early-init.el** — GC deferral (`gc-cons-threshold` = `most-positive-fixnum`
   interactive / 256 MB batch); `read-process-output-max` 4 MiB;
   `load-prefer-newer`; `package-enable-at-startup nil`;
   `setenv LSP_USE_PLISTS true`; **`user-emacs-directory` is swapped to
   `<root>/var/`** while `ar/config-dir` keeps the real root; creates `etc/`,
   `var/` subdirs; when native compilation is available, makes
   `<root>/eln-cache` a symlink into `var/eln-cache`;
   routes backup/autosave/recentf/savehist/saveplace/bookmarks/tramp/url/
   transient/nsm/eshell/abbrev paths; sets `custom-file` to `etc/custom.el`;
   `file-name-handler-alist` nil'd until `emacs-startup-hook` (depth 101),
   which also sets the 100 MB steady-state GC; display-only tweaks guarded by
   `(unless noninteractive …)`.
2. **init.el** — sets `straight-check-for-modifications '(check-on-save)`;
   bootstraps straight (network on first run); registers built-ins
   (`compat eldoc external-completion flymake jsonrpc org project seq
   transient xref`) as `:type built-in`; runs `straight-use-package` over
   `ar/packages`, **collecting failures and raising one hard `error`**; loads
   `custom-file`; adds `lisp/` to `load-path`; adds `var/bytecode/` to
   `load-path` **only if every cached .elc is fresh**; `require`s `ar-modules`
   in order; arms an 8-second idle timer (interactive only) that byte-compiles
   stale modules in a subprocess into `var/bytecode/`.
3. **Modules in `ar-modules` order** (load-bearing, see §4.1).
4. **Post-init** — `after-init-hook` users: `super-save-mode`,
   `global-corfu-mode`, `editorconfig-mode`, `envrc-global-mode`,
   `global-diff-hl-mode`. Then `emacs-startup-hook` restores GC and
   file-name handlers.

> **Batch/`-l` caveat that causes most false test results:** `-Q` suppresses
> automatic init-file loading. In `ar_batch`, Emacs processes startup before
> the explicit `-l early-init.el -l init.el`; `after-init-hook` has already
> run, so handlers registered by those files do not run automatically.
> `emacs-startup-hook` is not a reliable batch trigger either. Test hook
> bodies by invoking the relevant hook explicitly after config loading, or
> test actual startup in a TTY/daemon/GUI session. `emacs --batch` alone
> implies `-q` and does not load this config. See SKILLS S0/S3.

---

## 4. Invariants — do not break, and test when anything nearby changes

### 4.1 Structural
1. **Module order** `core → editor → ui → completion → org → dev →
   vc-projects → keybinds`. `core` defines helpers; `editor` defines
   `ar/global-leader` / `ar/local-leader`; `keybinds` binds commands from all.
2. **One definition per symbol across the whole config.** A later-loaded
   module silently shadows an earlier one. (The source records a past
   duplicate of `ar/delete-pair-dwim` and of the zoom commands.) Check with
   SKILLS S2 (duplicate-definition scan) after every edit.
3. **`early-init.el` and `init.el` are not reloaded by `reload-init-file`.**
   Changes there require a fresh process.
4. **Install only in `ar/packages`.** `use-package` never installs.
5. Module filenames must not shadow bundled libraries (`org.el` once did).
6. `gc-cons-threshold` steady-state literal `100000000` in `early-init.el`
   must equal `ar/gc-steady-cons-threshold` in `core-config.el`.

### 4.2 Performance / deferral contract
7. Deferred means deferred. Heavy packages (magit, forge, lsp-mode, dape,
   ghostel, yasnippet, jinx, dirvish internals, …) must not be in `features`
   after a plain startup. Eager by design: evil stack, general, nerd-icons,
   doom-modeline, which-key, vertico family, org (required explicitly).
8. A top-level `setopt` on a variable whose `defcustom` lives in an unloaded
   library **loads that library** (custom-load-symbol). The source documents
   two incidents (`compile.el`, `face-remap`). After any `setopt` edit, check
   `featurep` of the owning library.
9. Idle/timer work must not fire in batch (`run-with-idle-timer` never fires
   there; `run-at-time` can).

### 4.3 The "silently dead config" class (the most common real bug here)
10. A `use-package` block with `:defer t` (or `:after X` without `:demand`)
    whose `:config` / `:custom-face` / `:hook` needs a **trigger**. If nothing
    ever loads the package, the block does nothing and no error is raised.
    Historical examples in this config were `undo-fu-session`
    (`:demand t` since the fix), `evil-textobj-tree-sitter`
    (`:demand t :after evil` since the fix) and an old `popper` shape that
    relied on `after-init-hook` (`:config` now runs eagerly). For every
    currently deferred block, name its trigger (command, hook, key,
    `:after`, timer) and prove it fires (SKILLS S4).
11. Bindings must resolve: every target in a leader tree must be a command or
    an autoload that actually loads and yields a command.

### 4.4 Platform / UX constraints recorded in the source
12. `scroll-margin` **must stay 0** (pixel-scroll on this compositor).
13. `pixel-scroll-precision-use-momentum` stays nil.
14. `use-dialog-box`/`use-file-dialog` nil; `epg-pinentry-mode` `loopback`;
    `x-gtk-use-system-tooltips` nil — daemon freezes otherwise.
15. `vc-handled-backends` is `(Git)` only (TRAMP stalls otherwise).
16. `recentf` must never record TRAMP paths or its own state.
17. Large-file guards go through `ar/define-large-file-guard` /
    `too-long-file-p` with identical thresholds. As written the thresholds
    are: buffer > 500 000 chars, **or** `(car (buffer-line-statistics))` >
    10 000 — and `buffer-line-statistics` returns `(LINES LONGEST MEAN)`, so
    that `car` is the *line count*, not the longest line (see §12, D2).
18. `lsp-mode` is plist-built. Changing `LSP_USE_PLISTS` requires
    `M-x straight-rebuild-package RET lsp-mode`; a mismatch triggers a
    `warn` from the tripwire in `dev-config.el`.
19. `ws-butler` is pinned to a commit in `ar/packages`. Do not unpin or
    bump it without an explicit instruction.

---

## 5. Side-effect map (read before running *anything*)

Loading this config **writes to disk and starts timers**. Running it against
the user's real `$HOME` or real config root is prohibited (§6).

| Trigger | Side effect |
|---|---|
| `early-init.el` | Creates dirs under `<root>/etc`, `<root>/var`; **creates/replaces the symlink `<root>/eln-cache`** (guarded by `native-comp-available-p`; deletes any real directory of that name). |
| `init.el` first run | Clones + builds every package into `var/straight/` (network, minutes, C compilation for some). Failed packages abort startup. |
| `org-config.el` load | Creates `~/org` or `$XDG_DOCUMENTS_DIR/org` and subtrees (`zettelkasten/ projects/ areas/ resources/ archives/ journal/ agenda/ attachments/ downloads/`), **seeds** `agenda/todo.org`, `journal.org`, `habits.org`, and writes `.dir-locals.el` in the zettelkasten silo. |
| `dev-config.el` load | Creates `<root>/snippets/` and `var/tree-sitter/`. |
| Visiting a file whose language has no grammar | `treesit-auto-install-grammar 'always` **downloads and compiles a grammar** (network, `git`, C compiler). In batch this can hang or fail. |
| `super-save-mode` | Saves modified file buffers every 10 idle seconds and on window switches. |
| `recentf` | Timer saves the list every 300 s; idle timer rescans agenda files every 30 s. |
| `bufferlo` | Loads saved workspaces at startup and **saves all at exit** (and every 300 s). |
| `dape` | Saves breakpoints on `kill-emacs-hook`; loads them on first use. |
| `undo-fu-session` | Writes zstd/gzip undo files to `var/undo-fu-session/` (limit 500). |
| `jinx` | First use compiles a C module against enchant (needs compiler + enchant headers + dictionaries). |
| `ar/byte-compile-stale-modules` | Idle timer spawns a batch Emacs writing `.elc` into `var/bytecode/`. |
| `lsp-mode` | Spawns language servers; may modify `lsp-session` state in `var/`. |
| `ghostel` | Loads a native module; spawns shells (strips `DIRENV_*`). |
| `kill-emacs` | Many hooks fire (dape, bufferlo, savehist, recentf, saveplace, undo sessions). |

---

## 6. Operating rules

1. **Never run Emacs with this config against the real `$HOME`, real config
   root or real org directory.** Always use the sandbox from SKILLS S0:
   a throw-away copy of the config root, `HOME`, `XDG_DOCUMENTS_DIR`,
   `XDG_CONFIG_HOME`, `XDG_CACHE_HOME`, `XDG_RUNTIME_DIR` all pointed inside
   one temp directory. Verify no symlink inside the sandbox points back into
   the real tree (S0 includes the check) — straight build dirs are full of
   symlinks.
2. Never run `straight-pull-all`, `straight-pull-package`,
   `straight-freeze-versions`, `package-install`, or anything that changes
   package versions in the real tree. Upgrades are an explicit, separate
   task; in the sandbox they are allowed for diagnosis (S10).
3. Never edit files inside `var/straight/repos/*` in the real tree. To test
   a package patch, do it in the sandbox, and deliver it as a straight recipe
   change, a `:fork`, an advice, or an upstream bug report — not as a silent
   edit of the checkout.
4. Network: first-time straight bootstrap and grammar installs need it. If it
   is unavailable, say which tiers were blocked; do not fake results.
5. Do not run `kill-emacs`, `save-buffers-kill-emacs` or `reload-init-file`
   in a session that is attached to real data.
6. Do not commit, push, rewrite history, or touch git config unless asked.
7. Do not print secrets, tokens, `auth-source` contents, or the identity
   strings in `core-config.el`.
8. If a test mutates user-visible behaviour (e.g. you change a default),
   state that explicitly in the report; do not hide it in a "fix".
9. If you cannot run something (no display, no Emacs 31, no network), report
   it as **BLOCKED** with the exact missing prerequisite. Never mark a form
   verified on the strength of reading it.

---

## 7. Verification ladder and Definition of Done

Run the tiers in order; stop and fix at the first red tier. Procedures are in
SKILLS.md.

| Tier | Name | Pass criterion | Skill |
|---|---|---|---|
| T0 | Syntax | Every file `read`s to EOF; parens balanced; `lexical-binding: t` header present | S1 |
| T1 | Byte-compile gate (**G2**) | `byte-compile-error-on-warn t`, every module compiles with **zero** warnings under Emacs 31.1 | S1 |
| T2 | Inventory | Every top-level form enumerated, classified (eager/deferred/hook/timer/advice/key), no duplicate definitions | S2 |
| T3 | Batch boot | `early-init` + `init` + all modules load with no error and no `*Warnings*` entries; invariants in §4 asserted by ERT | S3 |
| T4 | Trigger tests | Every deferred form's trigger fires and its body takes effect | S4 |
| T5 | Bindings | Every leader/`general-define-key` target resolves; no key shadows another unintentionally | S5 |
| T6 | Interactive | Real keystrokes in a TTY (tmux) and, where possible, a GUI/daemon client frame behave as specified | S6 |
| T7 | Subsystem smoke | LSP (**G4**-style), treesit, dape, apheleia, org/denote/capture/agenda, magit, dirvish/treemacs, ghostel, completion UI | S11 |
| T8 | Reload & idempotence | `reload-init-file` twice leaves hook/advice counts stable; cold start still passes | S12 |
| T9 | Performance | Startup time and eager-feature budget not regressed vs. baseline | S9 |

**Definition of Done for behavior-affecting configuration changes:**
T0–T3 green; T4/T5 green for every form you touched *and* every form that
shares a hook, advice target, keymap or variable with it; a regression test
exists and fails on the unfixed behavior; T8 green; the report (§13) contains
the commands and outputs. The test/ and tools/ files referenced here are not
currently checked in; create them before relying on those procedures. For
**documentation-only** edits to AGENTS.md/SKILLS.md, verify the text against
the source and check the edited commands/snippets that can safely be run;
the full configuration verification ladder is not required.

---

## 8. The "every line" coverage contract

Maintain a ledger (`test/LEDGER.tsv`, generated by SKILLS S2) with one row
per top-level form:

`file · line · head · name · use-package keywords · trigger class · status · evidence`

Status values: `UNVERIFIED` (initial ledger entry, no evidence yet),
`VERIFIED-EAGER`, `VERIFIED-TRIGGER` (body proven to execute after the
named trigger), `VERIFIED-STATIC` (pure declaration: `declare-function`,
`defvar` without value, `provide`), `BLOCKED(reason)`, `FAILED(issue)`.
There is no `ASSUMED`.

Because the S2 generator writes its output with `> test/LEDGER.tsv`, rerunning
it destroys any statuses and evidence you have recorded. Treat the generated
file as a starting point: commit it, or copy it before running the generator
again.

Inside a form, lines are verified by one of: (a) the form executing during a
boot test; (b) an ERT test calling the function with representative and edge
inputs, including the non-interactive/batch branch **and** the interactive
branch (`ar/lsp-ensure-maybe`, `ar/setup-fonts`, `childframe-workable-p` all
branch on `noninteractive`/`display-graphic-p`); (c) `testcover`
instrumentation for branch coverage of the helper functions (SKILLS S7);
(d) for display-only code, a tmux/GUI session (S6) with a screen capture.

Large `use-package` forms are verified **keyword by keyword**: `:init`,
`:custom` (each variable's value after load, via `default-value`),
`:custom-face`, `:hook`, `:bind`, `:config`, `:after`, `:commands`.

---

## 9. Debugging doctrine

1. **Reproduce in the sandbox with the smallest command.** Capture exact
   Emacs version, build features (`system-configuration-features`,
   `(native-comp-available-p)`, `(treesit-available-p)`),
   `(display-graphic-p)`, `(daemonp)`, `noninteractive`, and whether the run
   was batch, TTY, GUI or daemon. `display-type` is not an Emacs Lisp function.
2. **Classify before theorising.** Pick one: load-time error · void
   function/variable · wrong-type · hook/advice ordering · deferred-block
   never fires · keymap shadowing · package/version drift · native-comp or
   .elc staleness · external binary missing · environment (PGTK/Wayland,
   fonts, daemon) · performance.
3. **Bisect mechanically.** Module-level (truncate `ar-modules`), then
   form-level (evaluate forms one by one), then package-level (`emacs -Q`
   with only that package, then straight checkout `git bisect`). SKILLS S8.
4. **Get a backtrace, not a guess**: `--debug-init`, `debug-on-error`,
   `debug-on-signal`, `debug-on-message`, `backtrace-on-error-noninteractive`.
5. **Prove the root cause** by making the failure disappear with the minimal
   change *and* reappear when the change is reverted.
6. **Fix at the right layer**: config bug → this repo; package bug →
   advice/override with a comment and upstream report; environment →
   documentation + guard (`executable-find`, `display-graphic-p`).
7. **Add a regression test** and extend the ledger.

## 10. Package debugging doctrine (built-in and external)

- Identify the **owner** of the behaviour: `(symbol-file 'fn)`,
  `C-h f`/`C-h v` ("defined in", "customized/set by"), `locate-library`,
  `find-function`, `advice-mapc`, `(get 'var 'custom-requests)`.
- Distinguish **source vs build vs loaded**: straight keeps the checkout in
  `var/straight/repos/<pkg>` and symlinks/compiles into
  `var/straight/build/<pkg>`; the running Emacs may be executing a stale
  `.elc`/`.eln`. Compare `load-history`, `load-prefer-newer` and file mtimes.
- Reduce to **`emacs -Q`** with `load-path` of just the package and its deps.
  If it still fails, it is an upstream bug: collect version, commit hash
  (`git -C var/straight/repos/<pkg> rev-parse HEAD`), minimal reproducer.
- If only the full config fails, bisect **config interactions**: advice,
  hooks, `completion-category-defaults`, `display-buffer-alist`,
  `file-name-handler-alist`, global minor modes.
- Built-ins are shipped with Emacs 31.1; their behaviour is fixed by the
  binary. Read the installed source via `find-library`/`C-h f`, not memory.
- Many comments in this config assert version-specific facts ("lsp-mode
  10.0.1 retired `lsp--workspaces`", "dape 0.27 autoloads only …",
  "evil-collection 3.x …"). When a package is upgraded or a test disagrees,
  **re-verify those claims against the installed source** and update the
  comment.

## 11. Conventions for edits

- Every file starts with the lexical-binding header and ends with
  `(provide 'x)` + `;;; x ends here`. Keep the commentary style: explain
  *why*, cite the evidence, note the version.
- Namespaces: `ar/` for config code, `my/` for org/denote/snippet paths.
  Existing un-namespaced helpers (`childframe-workable-p`,
  `icons-displayable-p`, `too-long-file-p`, `with-eval-after-load-all`,
  `window-toggle-split-direction`, `reload-init-file`,
  `lsp-booster--advice-*`) are intentional; do not rename without updating
  every caller (grep first).
- Prefer named functions over lambdas in hooks/advice (lambdas accumulate on
  `reload-init-file`, can't be removed by name, and defeat idempotence tests).
- Use `declare-function` / `(defvar x)` **before** the first use so the
  byte-compile gate stays warning-clean.
- Gate display-only code with `(unless noninteractive …)` or
  `display-graphic-p`; gate external tools with `executable-find`.
- New package ⇒ add to `ar/packages` (grouped by module) **and** a
  `use-package` block **and** a ledger row **and** a trigger test.
- Do not add `no-littering`; state routing is native (early-init.el).
- Never leave debugging code (`debug-on-*`, `lsp-log-io t`, `message`
  spam) in committed files.

## 12. Known sharp edges (verify, don't trust)

### 12.1 Suspected defects found while reading the source (UNVERIFIED — reproduce first)

These findings have different evidence levels. The reproducible Emacs-core
mechanisms below were exercised with Emacs 31.1, but the full configuration
was not booted during this documentation audit because its straight package
tree is absent. Do not treat a core-level reproducer as a full-config smoke
test.

- **D1 — `reload-init-file` vs. the bytecode cache** (`core-config.el`,
  `init.el`) — the underlying `locate-library`/source-path mismatch was
  reproduced on Emacs 31.1 with a temporary `.elc`-first load path. The full
  `reload-init-file` path remains untested. `init.el` prepends
  `var/bytecode/` when all cached `.elc` files are fresh; `reload-init-file`
  strips the extension from `locate-library`'s result and appends `.el`, which
  points to a source file that is not in that bytecode directory. Expected
  symptom: `file-missing` / "Cannot open load file" from `SPC f l`. The
  comment above `reload-init-file` also says stale `.elc` files are
  "removed before reloading", but the function removes nothing. Reproduce the
  full-config case and add a regression test before changing code; see S12.
- **D2 — `too-long-file-p`** (`core-config.el:48-49`) — **reproduced** with
  the current predicate on Emacs 31.1.
  `(car (buffer-line-statistics))` is the number of **lines**; the longest
  line length is the second element (`cadr`). A 200 KB single-line minified
  JSON file passes the guard; a 10 001-line ordinary file trips it. The
  intended threshold must be decided before changing behavior; if long lines
  are the intent, use the second element. See S7 for the reproducer.
- **D3 — anonymous hooks/advice may accumulate on reload** (corfu, cape,
  dape, ghostel, `compilation-mode-hook`, plus the anonymous
  `command-error-function` lambda in core) — **not reproduced**. Compare hook
  and advice state before/after two reloads in a provisioned sandbox (S12).
- **D4 — `ar/undo-fu-session-hash-path`** (`editor-config.el:145-149`) —
  the source formula was exercised with a 300-character basename: the output
  basename was 325 characters. Its docstring promises to bypass the
  filesystem's 255-byte basename limit, but appending the full basename
  defeats that promise. See S7; the full package path remains untested.

### 12.2 Other sharp edges

- Anonymous `lambda` hooks/advice: `corfu` (`save-buffer` advice), `cape`
  (several), `ghostel` (pre-spawn, compilation-mode), `dape` (several),
  `window-divider` etc. → duplicates after `reload-init-file`.
- Hooks registered by config files loaded with `-l` are not automatically
  triggered by startup; invoke them explicitly after loading to test their
  bodies, or use a real TTY/daemon/GUI startup (see §3 and SKILLS S3).
- `treesit-enabled-modes t` + auto-install: first visit to a new language
  triggers network + compile. Pre-install grammars in the sandbox.
- `ar/lsp-ensure-maybe` has distinct batch and interactive branches; the
  interactive branch uses `lsp-deferred`, which only starts when the buffer
  is visible.
- `lsp-booster` advice is conditional on the binary and on `lsp-use-plists`;
  tests with and without the binary on `PATH`.
- Corfu is disabled in the minibuffer and in several modes; completion in
  the minibuffer goes through `consult-completion-in-region`.
- `bufferlo` excludes `".*"` and whitelists two buffers; workspace tests
  must account for that.
- `popper` adds `:before` advice to `keyboard-quit`; it skips an active
  region and terminal buffers, so C-g does not close popups in every context.
- Fonts/icons: missing `JetBrains Mono`/`Symbols Nerd Font Mono`/`Noto Sans`
  produce silent fallbacks, not errors.
- Tests that touch `ghostel` need its native module; treat load failure as
  BLOCKED with the exact message.

## 13. Report format

For each issue or verification batch:

```
ID / severity (blocker | bug | smell | info)
Where: file:line (form name)
Environment: emacs --version; batch|tty|gui|daemon; sandbox path
Reproduce: exact command(s)
Observed: verbatim output (trim, but keep the error + first 15 backtrace lines)
Root cause: one paragraph, with evidence (symbol-file, git hash, load-history…)
Fix: diff
Verify: command(s) + output after the fix
Regression test: path::test-name
Ledger rows updated: n
```

End every report with a table of ledger status counts and an explicit list of
**BLOCKED** items with their missing prerequisites.

## 14. Where to go next

- Build/provision/test/debug procedures: **SKILLS.md** (S0–S13).
- If something here contradicts the code you can run, the code wins — fix
  this file in the same change.
