Emacs Configuration Project State Checkpoint (v7 - July 09, 2026)

CRITICAL INSTRUCTIONS FOR THE NEW AI SESSION
Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read and ingest the attached `editor-architecture.md` file to understand the massive restructuring of the `Editor Behaviour` section and the Non-Lisp AST structural editing paradigm.
5. Read this `project-state.md` file to understand the architectural decisions, the massive "Editor Behaviour" restructuring, and current progress.
6. Acknowledge these rules and the current state.

STRICT GREENLIGHT PROTOCOL: Do not write any code or output any `#+begin_src` blocks until the user explicitly gives the signal to proceed.

Core Architectural Rules & Constraints
The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- Strict Org-Mode Formatting: Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- Vanilla Emacs Paradigm: Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents.
- Keybinding Management: `general.el` is the centralized keybinding manager. Global motions and commands must be routed through `general.el`. Operator-pending text objects must be injected directly into Evil's internal C-level keymaps via `define-key` in a `:config` block guarded by `:after evil`.
- Corfu Confinement: `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- Minibuffer Navigation: Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer and window management to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- Concise Documentation Protocol (Hard Negative Constraint): Documentation inside `config.org` (both Org-mode text descriptions under headers AND Emacs Lisp comments inside `#+begin_src` blocks) must be ruthlessly terse (strictly 1-2 short sentences maximum, passive/objective voice). Never use "I", "we", "you", or "let's".
- Bundled Extensions & `:ensure nil`: Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository MUST explicitly include `:ensure nil`.
- Elpaca Hook Migration: Any `:hook` keyword using `after-init` or `emacs-startup` must be replaced with `elpaca-after-init`.

The "Editor Behaviour" Paradigm Shift & Main Section Naming
In v5, the `Editor Behaviour` concept was massively restructured to become the centralized pillar for how Emacs renders, parses, and manipulates text and buffers. In v7, the 8 Logical Groups have been assigned definitive main section names for physical migration in `config.org`:

1. Visual Chrome & UI Overlays (Group 1 - FULLY FINALIZED)
2. Interactive Buffers & Redisplay Physics (Group 2)
3. AST Parsing & Structural Typing (Group 3)
4. Code Folding & Region Concealment (Group 4)
5. Spatial Alignment & Whitespace Hygiene (Group 5)
6. Frame Chrome & Status Indicators (Group 6)
7. Spatial Traversal & Inline Mutations (Group 7)
8. Lexical Validation & Comparative Workflows (Group 8)

For the exact load-order prerequisites, migration details, and the philosophical/technical justification of the AST stack: Refer to `editor-architecture.md`.

Negative Constraints (Explicitly Rejected Patterns)
The AI must NEVER suggest or implement the following patterns:

- No `smartparens` or `puni`: Regex-based pair tracking causes desyncs in complex strings/templates. Lisp-centric soft-deletion is irrelevant for Non-Lisp AST languages.
- No `combobulate`: Fundamentally clashes with Evil's operator-pending grammar and causes keymap collisions.
- No `completion-in-region-function` overrides (Corfu must remain strictly in-buffer).
- No `which-key` Embark Hacks (use `vertico-multiform` grid).
- No `consult-projectile` (obsolete).
- No Minibuffer Evil Normal State (breaks type-to-filter).
- No custom `RET` intercepts for comment continuation (use native `comment-indent-new-line`).
- No custom window movement Elisp functions (use native `windmove-wrap-around`).
- No Nerd Icons inside `prettify-symbols-alist`: Injecting PUA glyphs into buffer text causes mid-line font-fallback context switches and sub-pixel grid misalignments.

Edge Cases & Deferred Issues

- Dirvish Multi-frame Flicker: Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- lsp-mode Org Element API Crash: Patched via `:around` advice in the `lsp-mode :init` block to prevent `cl-generic` corruption.
- Emacs 31 Unreleased APIs: Features like `grep-edit-mode` or MPS Incremental GC (`igc`) must be wrapped in defensive runtime guards.

Architectural Decisions & Load-Order Physics (Session v7)

- HarfBuzz Delegation: Operator ligatures (`!=`, `>=`, `->`) are delegated to JetBrains Mono's native OpenType GSUB tables via `ligature.el` (`:demand t`). `prettify-symbols-alist` is restricted strictly to `lambda` to prevent cognitive ambiguity and preserve `isearch`/LSP ASCII parsing.
- Daemon-Safe Typography: Font application utilizes `server-after-make-frame-hook` to guarantee GUI frame inheritance in headless `emacsclient` environments. Integer `line-spacing` (2px) is used to prevent sub-pixel blurriness.
- Emacs 30 Built-ins: `which-key` utilizes `:ensure nil` to respect its new Emacs 30 core status, bypassing Elpaca network I/O. `:demand t` is used to guarantee API availability for eager downstream UI packages.
- Theme FOUC Prevention: `doom-themes` uses `:demand t` to guarantee synchronous palette loading before the first frame draw. Downstream injections (Treemacs, Org) are wrapped in `with-eval-after-load` to respect deferred load states.
- Child Frame Safety: `posframe` fringe isolation is guarded by `frame-live-p` to prevent redisplay crashes during daemon teardown or aggressive garbage collection.
- Solaire Race Condition: `solaire-mode` utilizes `:after doom-themes` to guarantee the Tokyo Night palette is fully resident in memory before background face-swapping occurs.

Pending Architectural Decisions & In-Code TODOs

- Project & Workspace Management: `Project Management` and `Workspaces` are empty placeholders. All code referencing `projectile` or `persp-mode` is commented out awaiting this decision.
- Syntax Checking Transition: Transitioning from `flymake` to `flycheck`.
- Debug Adapter Protocol: Evaluating `dape` vs `dap-mode`.
- Emacs 31 Treesitter: `Treesit` grammar installation deferred until Emacs 31 release for automatic grammar installation.
- LaTeX / AUCTeX: Pending integration of `preview-auto.el`, `cdlatex`, and Bibliography Management.
- LSP / Eglot Consolidation: Several language blocks contain TODOs to replace `eglot` with `lsp-mode`.

Overall Configuration Progress

Fully Finalized Main Sections

- Core Emacs: All 13 subsections finalized.
- Visual Chrome & UI Overlays (Editor Behaviour Group 1): All 8 subsections finalized (`Child Frame`, `Nerd Icons`, `Fonts`, `Doom Themes`, `Solaire Mode`, `Transient`, `Which Key`, `Prettify Symbols & Ligatures`).

Partially Finalized Main Sections

- Vim Emulation: 12 subsections finalized. Note: `Evil Surround` and `Evil Matchit` were moved to `Editor Behaviour` (Group 3). Pending: `Smart Comment Continuation`.
- Window: 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.

Untouched / Pending Main Sections (In Document Order)

- Editor Behaviour (Groups 2-8): Group 1 is finalized. Groups 2-8 require physical Org-mode restructuring into their newly established main section names, alongside the creation of the new AST stack placeholders.
- Version Control: Magit, Forge, Diff-hl, Git-Timemachine, Transient Menu.
- Org Mode & Second Brain: Dynamic Directory Structure, Per-Project Context, Denote, Org GTD, Org Agenda, Org Capture, Org Super Agenda.
- Workflow Management: Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
- Completion Framework: Orderless, Vertico, Marginalia, Consult, Embark, Corfu, Cape, Dabbrev. (Code is heavily optimized and present, but main heading and subsections still marked `TODO` pending final sign-off).
- Development Tools: LSP, Formatting, DAP, Syntax Checking, Direnv, Eldoc.
- Highlight TODOs: hl-todo, consult-todo, magit-todos.
- Snippet Engine: Yasnippet, File Templates.
- LaTeX Writing Environment: AUCTeX, TeX Folding, Evil Integration, Bibliography, Preview, Org Integration.
- Languages: Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- Study: PDF Tools, Org Noter.
- Misc: General Keybindings, AI/LLM.

Immediate Structural Action Plan for `config.org.txt`
The Elisp payloads for Group 1 are finalized. The next physical tasks require manual copy-pasting or structural reordering in `config.org`:

1. Create the `* DONE Visual Chrome & UI Overlays` main section and migrate the 8 finalized Group 1 subsections into it.
2. Create the remaining 7 main sections (Groups 2-8) using the established naming convention.
3. Migrate `Terminal` into Group 2.
4. Migrate `Treesit`, `Evil Surround`, `Evil Matchit`, and create empty AST placeholders in Group 3.
5. Migrate `Treesit-Fold` and `Vimish-Fold` into Group 4.
6. Delete the massive, multi-language regex `* TODO Prettify Symbols` main section from the bottom of the file (it has been replaced by the simplified Group 1 block).

Remaining Work & Questions (Checklists)

- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [ ] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle
- [ ] Determine if compile-angel is needed for elpaca?
- [ ] Determine if elpaca byte-compiles and native-compiles all packages. And is it okay to leave these settings to elpaca's default.
- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [ ] Is there a way to execute nerd-icons-install-fonts during intial emacs setup using elpaca? (Note: Answered - manual execution is mandated to prevent startup network I/O blocking).
- [ ] How do I get history evil commands in the echo area like doom emacs? (Note: Solved via `evil-ex-completion-map` arrow key bindings in the `Evil` subsection).
