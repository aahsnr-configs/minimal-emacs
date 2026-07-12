# Emacs Configuration Project State Checkpoint (v15 - July 12, 2026)

## CRITICAL INSTRUCTIONS FOR THE NEW AI SESSION

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` (or `config.org.new.txt`) file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read this `project-state.md` file to understand the architectural decisions, the massive "Editor Behaviour" restructuring, and current progress.
5. Acknowledge these rules and the current state.

**STRICT GREENLIGHT PROTOCOL:** Do not write any code or output any `#+begin_src` blocks until the user explicitly gives the signal to proceed.

## Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

1. **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
2. **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents.
3. **Keybinding Management:** `general.el` is the centralized keybinding manager. Global motions and commands must be routed through `general.el`. Operator-pending text objects must be injected directly into Evil's internal C-level keymaps via `define-key` in a `:config` block guarded by `:after evil`.
4. **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
5. **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer and window management to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
6. **Concise Documentation Protocol (Hard Negative Constraint):** Documentation inside `config.org` (both Org-mode text descriptions under headers AND Emacs Lisp comments inside `#+begin_src` blocks) must be ruthlessly terse (strictly 1-2 short sentences maximum, passive/objective voice). Never use "I", "we", "you", or "let's".
7. **Bundled Extensions & `:ensure nil`:** Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository MUST explicitly include `:ensure nil`.
8. **Elpaca & `use-package` Load-Order Physics:**
   - NEVER use `:hook (elpaca-after-init . mode)` or `:hook (after-init . mode)` to activate global minor modes.
   - For built-in packages, invoke `(mode 1)` directly in `:config` (or `:init` if early interception is required) to eliminate artificial startup latency.
   - Reserve `elpaca-after-init-hook` strictly for cross-package state initialization.
9. **Header Status Keywords:** NEVER add or change a section (`*`) or subsection (`**`) header to `DONE` unless the user explicitly states that the corresponding section or subsection has been finalized. Default to `TODO` or preserve the existing status.
10. **Emacs 31 Targeting & Build Flags:** The configuration explicitly targets **Emacs 31**. The user is currently daily-driving the `emacs-pretest` package (v31.0.90) built with `--with-native-compilation=aot` and `--with-pgtk`. While Emacs 31 is the primary target, defensive guards (`fboundp`/`boundp`/version checks) should still wrap highly experimental APIs that might shift before the final stable release. The MPS incremental/concurrent garbage collector (`igc`) remains deferred out of the Emacs 31 release cycle and must NOT be treated as an Emacs 31 feature.

## The "Editor Behaviour" Paradigm Shift & Main Section Naming

In v5, the `Editor Behaviour` concept was massively restructured to become the centralized pillar for how Emacs renders, parses, and manipulates text and buffers. In v15, the physical migration and finalization of these groups is the primary active objective.

- **Visual Chrome & UI Overlays (Group 1 - FULLY FINALIZED)**
- **Interactive Buffers & Redisplay Physics (Group 2 - FULLY FINALIZED)**
- **AST Parsing & Structural Typing (Group 3 - FULLY FINALIZED)**
- **Code Folding & Region Concealment (Group 4 - FULLY FINALIZED)**
- **Spatial Alignment & Whitespace Hygiene (Group 5 - FULLY FINALIZED)**
- **Frame Chrome & Status Indicators (Group 6 - FULLY FINALIZED)**
- **Spatial Traversal & Inline Mutations (Group 7 - IN PROGRESS)**
- **Lexical Validation & Comparative Workflows (Group 8 - IN PROGRESS)**

## Negative Constraints (Explicitly Rejected Patterns)

The AI must NEVER suggest or implement the following patterns:

- No `smartparens` or `puni`: Regex-based pair tracking causes desyncs in complex strings/templates. Lisp-centric soft-deletion is irrelevant for Non-Lisp AST languages.
- No `combobulate`: Fundamentally clashes with Evil's operator-pending grammar and causes keymap collisions.
- No `tree-edit`: Relies on the legacy `elisp-tree-sitter` (`tsc`) engine and forked grammars, causing dual-engine memory bloat and fatal conflicts with native Emacs 30 `treesit`.
- No `expand-region`: Replaced by `expreg`, which utilizes O(1) AST-aware pre-computation via native `treesit` instead of fragile regex/syntax-table heuristics.
- No `stripspace`: Replaced by `ws-butler` to prevent Git diff pollution via modified-lines-only trimming.
- No `kirigami.el` / `origami.el`: Heavy abstraction layers for folding. A custom transparent dispatcher is preferred for Vanilla Emacs.
- No `completion-in-region-function` overrides (Corfu must remain strictly in-buffer).
- No `which-key` Embark Hacks (use `vertico-multiform` grid).
- No `consult-projectile` (obsolete).
- No Minibuffer Evil Normal State (breaks type-to-filter).
- No custom `RET` intercepts for comment continuation (use native `comment-indent-new-line`).
- No custom window movement Elisp functions (use native `windmove-wrap-around`).
- No Nerd Icons inside `prettify-symbols-alist`: Injecting PUA glyphs into buffer text causes mid-line font-fallback context switches and sub-pixel grid misalignments.
- No `elpaca-after-init` hooks for built-in global minor modes (use direct `:config` invocation).
- No custom ElDoc backends for `show-paren` (use native Emacs 29+ `'overlay` engine).
- No treating the MPS incremental/concurrent garbage collector (`igc`) as an Emacs 31 feature: it was explicitly deferred out of the Emacs 31 release cycle (per `emacs-devel`, May 2026) and is absent from the `31.0.90` pretest. Any `igc`-related code targets a future post-31 release only and must be re-verified via web search before use.
- No Package Merging: NEVER merge multiple distinct packages into a single `#+begin_src` block or a single subsection header. Every package must reside in its own dedicated `**` subsection with its own isolated source block.
- No Scope Creep: NEVER modify, rewrite, or touch subsections, packages, or code blocks that were not explicitly tasked in the current prompt.
- **No Modular Eglot Stack for IDE Parity:** Rejected the modular `eglot` + `flymake` + `dape` ecosystem in favor of the synergistic `lsp-mode` stack. `eglot` lacks native Code Lens and `launch.json` ingestion, requiring fragile custom Elisp bridges that violate the zero-maintenance IDE paradigm.

## Edge Cases & Deferred Issues

- **Dirvish Multi-frame Flicker:** Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- **lsp-mode Org Element API Crash:** Patched via `:around` advice in the `lsp-mode :init` block to prevent `cl-generic` corruption.
- **Emacs 31 Unreleased APIs:** Features like `grep-edit-mode` must be wrapped in defensive runtime guards.
- **[v11 Correction] MPS Incremental GC (`igc`) Removed From Scope:** Prior state incorrectly listed MPS Incremental GC (`igc`) as an Emacs 31 API. Confirmed deferred out of Emacs 31 entirely.
- **`no-littering` API Typo:** Fixed hallucinated `no-littering-expand-var-directory-name` to the correct `no-littering-expand-var-file-name`.
- **`treesit` Block Parenthesis Mismatch:** Fixed an extra closing parenthesis in the `treesit--build-grammar` advice.
- **`treesit-fold` Ellipsis Injection:** Upstream `treesit-fold` natively inherits Emacs' built-in `truncate-string-ellipsis`.
- **[v15 Correction] Structural Nesting Anomaly (Group 7):** In `config.org.txt` (v0.6), `Precision Editing` (Group 7) is incorrectly nested as a `**` subsection under `* TODO Frame Chrome & Status Indicators`. It must be promoted to a top-level `* TODO Precision Editing` header in the next structural rewrite.
- **[v15 Correction] Missing `Move Text` Package:** The `move-text` package and its `ar/move-text-indent-region-advice` are missing from the `Spatial Alignment & Whitespace Hygiene` section in the v0.6 file dump. It must be re-injected.

## Architectural Decisions & Load-Order Physics

### Session v10

- **Non-Lisp AST Stack Finalization (Group 3 Complete):** Standardized on `elec-pair`, `evil-surround` + `evil-embrace`, `delete-pair`, `evil-textobj-tree-sitter`, `evil-ts-obj`, `treesit-navigate-thing`, and `expreg`.
- **`evil-ts-obj` Transient Isolation (Pillar 4):** Operators strictly isolated within an `ar/ast-refactor-transient` menu routed to `SPC c s`.
- **Code Folding Unification (Group 4 Complete):** Unified dispatcher engine routing Evil's `z` prefix to `treesit-fold`, `hideshow`, `outline-minor-mode`, and `vimish-fold`.
- **Whitespace & Indentation Strategy (Group 5 Complete):** Standardized on `ws-butler` and `dtrt-indent`.
- **`general.el` Eager Load-Order Physics:** Synchronously and eagerly loaded to guarantee macro availability.

### Session v11

- **Protocol Fact-Audit (Emacs 31 Version Targeting):** Confirmed `grep-edit-mode` is real. Confirmed MPS GC deferred.

### Session v12

- **Frame Chrome & Status Indicators (Group 6 Complete):** Replaced legacy packages with `colorful-mode`, `ar/hide-modeline-mode`, and native `global-text-scale-adjust`.
- **Non-Lisp AST Stack Guarding:** Injected defensive `derived-mode-p` guards to prevent AST engines in Lisp modes.

### Session v13

- **State Generation & Protocol Audit:** Verified structural parity. Group 6 finalized.

### Session v14

- **Emacs 31 Targeting & Build Flags Consolidation:** Configuration explicitly targets Emacs 31 (`--with-native-compilation=aot` and `--with-pgtk`).
- **Reference Cleanup:** Removed external `editor-architecture.md` references.

### Session v15

- **IDE Stack Consolidation (LSP vs Eglot):** Committed to the synergistic `lsp-mode` ecosystem (`lsp-mode`, `lsp-ui`, `dap-mode`, `flycheck`) over the modular `eglot` stack. This guarantees 1:1 VSCode IDE parity (Code Lens, `launch.json` ingestion, sideline diagnostics) without inventing custom Elisp bridges.
- **Workspace Management Selection:** Selected `bufferlo` over `tabspaces` and `persp-mode`. `bufferlo` provides native frame/tab buffer-list isolation and lightweight bookmark-based persistence, avoiding `desktop.el` bloat while integrating seamlessly with `consult-buffer`.
- **Precision Editing (Group 7) Finalization:** Finalized the Avy/Expreg/Snipe stack. Mapped `M-j` to `avy-isearch`, restricted `avy-keys` to home-row, and enforced the `at-full` visual paradigm.

## Pending Architectural Decisions & In-Code TODOs

- **Project & Workspace Management:** `bufferlo` selected for workspaces. `projection` selected for project management. Code referencing `projectile` or `persp-mode` must be excised and replaced.
- **Syntax Checking Transition:** Transitioning from `flymake` to `flycheck` to integrate with `lsp-mode` diagnostics.
- **Debug Adapter Protocol:** Committed to `dap-mode` (with `transient` menus) over `dape` to maintain `launch.json` parity.
- **LaTeX / AUCTeX:** Pending integration of `preview-auto.el`, `cdlatex`, and Bibliography Management.

## Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** All 13 subsections finalized.
- **Visual Chrome & UI Overlays (Group 1):** All 8 subsections finalized.
- **Interactive Buffers & Redisplay Physics (Group 2):** All 5 subsections finalized.
- **AST Parsing & Structural Typing (Group 3):** All 12 subsections finalized.
- **Code Folding & Region Concealment (Group 4):** Unified dispatcher finalized.
- **Spatial Alignment & Whitespace Hygiene (Group 5):** 4/5 subsections finalized (Missing `Move Text` in v0.6 dump).
- **Frame Chrome & Status Indicators (Group 6):** All 8 subsections finalized.
- **Vim Emulation:** All 12 subsections finalized.
- **General Keybindings:** Core `use-package general` block finalized and eagerly loaded.

### Partially Finalized / In-Progress Main Sections

- **Window:** 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.
- **Precision Editing (Group 7):** Avy, Evil Easymotion, Evil Snipe, Link Hint, Ace Window, Anzu, Iedit, Wgrep, Re-Builder finalized. _Requires structural promotion from `**` to `*`._

### Untouched / Pending Main Sections (In Document Order)

- **Lexical Validation & Comparative Workflows (Group 8):** Jinx, Helpful, Ediff, Woman.
- **Workflow Management:** Dired, Dirvish, Project Management (`projection`), iBuffer, Treemacs, Workspaces (`bufferlo`).
- **Version Control:** Magit, Forge, Diff-hl, Git-Timemachine, Transient Menu.
- **Org Mode & Second Brain:** Dynamic Directory Structure, Per-Project Context, Denote, Org GTD, Org Agenda, Org Capture, Org Super Agenda.
- **Completion Framework:** Orderless, Vertico, Marginalia, Consult, Embark, Corfu, Cape, Dabbrev.
- **Development Tools:** LSP, Formatting, DAP, Syntax Checking, Direnv, Eldoc.
- **Highlight TODOs:** hl-todo, consult-todo, magit-todos.
- **Snippet Engine:** Yasnippet, File Templates.
- **LaTeX Writing Environment:** AUCTeX, TeX Folding, Evil Integration, Bibliography, Preview, Org Integration.
- **Languages:** Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- **Study:** PDF Tools, Org Noter.
- **Misc:** AI/LLM.

## Immediate Structural Action Plan for `config.org.txt`

- **Promote Group 7:** Extract `** TODO Precision Editing` from under `Frame Chrome` and promote it to `* TODO Precision Editing`.
- **Re-inject `Move Text`:** Add the `move-text` package block back into `Spatial Alignment & Whitespace Hygiene`.
- **Excise Projectile/Persp:** Replace all commented-out `projectile` and `persp-mode` glue code with `projection` and `bufferlo` equivalents.
- **Next Steps:** Proceed to Group 8 (Lexical Validation) and Workflow Management.

## Remaining Work & Questions (Checklists)

### Spatial Traversal & Inline Mutations (Group 7)

- [x] Avy: Finalized `at-full` paradigm, home-row routing, and `avy-isearch` (`M-j`) binding.
- [ ] Anzu: Integrate anzu with lsp-mode server only if possible and if needed.
- [ ] Iedit: Determine if iedit needs explicit integration with lsp-mode semantic renaming.

### Lexical Validation & Comparative Workflows (Group 8)

- [ ] Jinx: Migrate exclusions from `dict.txt` to an inline, case-insensitive Elisp list.
- [ ] Helpful: Integrate with the main development tools sections.
- [ ] Ediff: Finalize transient menu and vim-style navigation bindings.

### Workflow Management

- [ ] Project Management: Implement `projection` package.
- [ ] Workspaces: Implement `bufferlo` package and wire `consult-buffer` sources.

### Development Tools

- [ ] Language Server Protocol: Disable and remove `lsp-org` at all costs.
- [ ] Debug Adapter Protocol: Replace Dape with `dap-mode` and wrap in `transient`.
- [ ] Syntax Checking: Replace flymake with `flycheck` and chain with `lsp-mode` diagnostics.
- [ ] Eldoc: Determine Doom Emacs eldoc scenarios and implement.

### Global Remaining Work

- [ ] Fix `Precision Editing` header nesting anomaly in `config.org`.
- [ ] Re-inject `Move Text` into `Spatial Alignment`.
- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`.
- [ ] Customize org-agenda-finalize to customize org-agenda buffer.
- [x] Setup automatic pair generation for '**', '==', etc (Solved via O(1) multi-char prose formatter in Electric Pair).
- [ ] Determine if compile-angel is needed for elpaca.
- [ ] Pressing enter on a header should move a task from TODO to DONE like doom emacs does.

### Global Questions

- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [x] Is there a way to execute nerd-icons-install-fonts during intial emacs setup using elpaca? (Answered: manual execution mandated).
- [x] How do I get history evil commands in the echo area like doom emacs? (Answered: Solved via `evil-ex-completion-map` arrow keys).
- [ ] How can I make the echo area useful? Do this at the very end of the configuration.
- [ ] How to open links from org buffers by pressing enter and using the correct browser?
