# Emacs Configuration Project State Checkpoint (v14 - July 11, 2026)

## CRITICAL INSTRUCTIONS FOR THE NEW AI SESSION

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` (or `config.org.new.txt`) file. The `config.org.new.txt` will be added later after several prompts.
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

In v5, the `Editor Behaviour` concept was massively restructured to become the centralized pillar for how Emacs renders, parses, and manipulates text and buffers. In v14, the physical migration and finalization of these groups is the primary active objective.

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

## Edge Cases & Deferred Issues

- **Dirvish Multi-frame Flicker:** Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- **lsp-mode Org Element API Crash:** Patched via `:around` advice in the `lsp-mode :init` block to prevent `cl-generic` corruption.
- **Emacs 31 Unreleased APIs:** Features like `grep-edit-mode` must be wrapped in defensive runtime guards.
- **[v11 Correction] MPS Incremental GC (`igc`) Removed From Scope:** Prior state (v10 and earlier) incorrectly listed MPS Incremental GC (`igc`) alongside `grep-edit-mode` as an Emacs 31 API requiring defensive guards. A protocol audit (July 2026) against `emacs-devel` confirmed the Emacs 31 release manager announced in May 2026 that the new garbage collector would not ship in Emacs 31 — the `emacs-31` branch was cut without it, and the subsequent `31.0.90` pretest (June 2026) confirms its absence. `igc` is therefore struck from all Emacs-31-targeting guards and negative-constraint language going forward; it is deferred to an unspecified future (post-31) release and must not be assumed present without a fresh, dated web search.
- **`no-littering` API Typo:** Fixed hallucinated `no-littering-expand-var-directory-name` to the correct `no-littering-expand-var-file-name` (with trailing slash for directories) in `treesit` and `undo-fu-session` blocks.
- **`treesit` Block Parenthesis Mismatch:** Fixed an extra closing parenthesis in the `treesit--build-grammar` advice that caused an `Invalid read syntax: )` crash during tangling.
- **`treesit-fold` Ellipsis Injection:** Upstream `treesit-fold` (Jan 2025 refactor) natively inherits Emacs' built-in `truncate-string-ellipsis`. Custom overlay advice for ellipses is no longer required; global `setq` is sufficient.

## Architectural Decisions & Load-Order Physics

### Session v10

- **Non-Lisp AST Stack Finalization (Group 3 Complete):** Rejected `smartparens`/`puni`, `combobulate`, `tree-edit`, and `expand-region`. Standardized on `elec-pair`, `evil-surround` + `evil-embrace`, `delete-pair`, `evil-textobj-tree-sitter`, `evil-ts-obj`, `treesit-navigate-thing`, and `expreg` for structural editing.
- **`evil-ts-obj` Transient Isolation (Pillar 4):** Fulfills structural manipulation (slurp, barf, raise, extract). Default global bindings (`M-j`, `M-k`, etc.) are aggressively purged in `:config` to prevent collisions with `general.el` and `move-text`. Operators are strictly isolated within an `ar/ast-refactor-transient` menu routed to `SPC c s`.
- **Code Folding Unification (Group 4 Complete):** Replaced fragmented folding with a unified dispatcher engine routing Evil's `z` prefix to `treesit-fold` (AST), `hideshow` (Regex/Fold-markers), `outline-minor-mode` (Prose fallback), and `vimish-fold` (Visual).
- **Whitespace & Indentation Strategy (Group 5 Complete):** Rejected `stripspace` in favor of `ws-butler` (unobtrusive, modified-lines-only trimming to protect Git diffs). `dtrt-indent` is routed to `change-major-mode-after-body-hook` to prevent LSP race conditions.
- **`general.el` Eager Load-Order Physics:** Verified that `general.el` is synchronously and eagerly loaded (`:ensure (:wait t)`, `:demand t`) at the end of the Vim Emulation section. This mathematically guarantees that `general-define-key`, `ar/global-leader`, and `ar/local-leader` are globally available for all downstream deferred packages without requiring `with-eval-after-load` wrappers.

### Session v11

- **Protocol Fact-Audit (Emacs 31 Version Targeting):** Conducted a web-search-backed audit of `system-prompt-protocol.md`'s Emacs 31 claims against current (July 2026) upstream sources. Confirmed `grep-edit-mode` is a real, correctly named Emacs 31 feature. Confirmed the MPS incremental/concurrent garbage collector was deferred out of Emacs 31 entirely. All prior guidance treating `igc` as an in-scope, guard-wrapped Emacs 31 feature is superseded.

### Session v12

- **Frame Chrome & Status Indicators (Group 6 Complete):** Replaced legacy `rainbow-mode` with `colorful-mode`. Replaced unmaintained `hide-mode-line` with a native buffer-local minor mode (`ar/hide-modeline-mode`). Replaced `default-text-scale` with native Emacs 29+ `global-text-scale-adjust` face-remapping engine. Consolidated `doom-modeline` inheritance physics.
- **Non-Lisp AST Stack Guarding:** Injected defensive `derived-mode-p` guards into `treesit-fold`, `indent-bars`, and `evil-ts-obj` to strictly prevent AST engines from activating in `emacs-lisp-mode` and `lisp-interaction-mode`, eliminating missing grammar warnings and strictly enforcing the architectural boundary.

### Session v13

- **State Generation & Protocol Audit:** Executed unconditional state generation command. Verified structural parity between `config.org.txt` and `project-state.md`. Group 6 (Frame Chrome & Status Indicators) remains fully finalized. Group 7 (Spatial Traversal & Inline Mutations / Precision Editing) and Group 8 (Lexical Validation & Comparative Workflows) remain the active pending frontiers. Next immediate target: `Avy`.

### Session v14

- **Emacs 31 Targeting & Build Flags Consolidation:** The configuration now explicitly targets **Emacs 31** as the primary environment. The user is daily-driving the `emacs-pretest` package (v31.0.90) compiled with `--with-native-compilation=aot` and `--with-pgtk`. Defensive guards remain for highly experimental APIs, but Emacs 31 is treated as the baseline.
- **Reference Cleanup:** Removed all references to the external `editor-architecture.md` file. The architectural paradigm is now fully self-contained within `project-state.md` and `system-prompt-protocol.md`.

## Pending Architectural Decisions & In-Code TODOs

- **Project & Workspace Management:** `Project Management` and `Workspaces` are empty placeholders. All code referencing `projectile` or `persp-mode` is commented out awaiting this decision.
- **Syntax Checking Transition:** Transitioning from `flymake` to `flycheck`.
- **Debug Adapter Protocol:** Evaluating `dape` vs `dap-mode`.
- **LaTeX / AUCTeX:** Pending integration of `preview-auto.el`, `cdlatex`, and Bibliography Management.
- **LSP / Eglot Consolidation:** Several language blocks contain TODOs to replace `eglot` with `lsp-mode`.

## Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** All 13 subsections finalized.
- **Visual Chrome & UI Overlays (Editor Behaviour Group 1):** All 8 subsections finalized.
- **Interactive Buffers & Redisplay Physics (Group 2):** All 5 subsections finalized.
- **AST Parsing & Structural Typing (Group 3):** All 12 subsections finalized.
- **Code Folding & Region Concealment (Group 4):** Unified dispatcher finalized.
- **Spatial Alignment & Whitespace Hygiene (Group 5):** All 4 subsections finalized.
- **Frame Chrome & Status Indicators (Group 6):** All 8 subsections finalized (`Line Numbers`, `Fill Column Indicator`, `Display Dividers`, `Active Line Highlight`, `Color Visualization`, `Modeline`, `Hide Modeline`, `Text Scaling`).
- **Vim Emulation:** All 12 subsections finalized.
- **General Keybindings:** Core `use-package general` block finalized and eagerly loaded.

### Partially Finalized / In-Progress Main Sections

- **Window:** 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.

### Untouched / Pending Main Sections (In Document Order)

- **Editor Behaviour (Groups 7-8):** `Spatial Traversal & Inline Mutations` (Avy, Anzu, Iedit) and `Lexical Validation & Comparative Workflows` (Jinx, Helpful, Ediff).
- **Workflow Management:** Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
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

- The physical reorganization of the 8 Logical Groups defined in the architecture is COMPLETE.
- The legacy `* TODO Prettify Symbols` bloat has been DELETED and replaced by the simplified Group 1 block.
- Group 6 (Frame Chrome & Status Indicators) is FULLY FINALIZED.
- Next Steps: Proceed to Group 7 (Spatial Traversal & Inline Mutations / Precision Editing) and Group 8 (Lexical Validation & Comparative Workflows).

## Remaining Work & Questions (Checklists)

### Spatial Traversal & Inline Mutations (Group 7)

- [ ] Avy: Issue is avy replaces characters when it is active, so the original position to go to is difficult to select using a letter when the letter is blocking the view of the letter to go to.
- [ ] Anzu: Do I need to anything with other isearch functionalities in the rest of the emacs config to work with anzu?
- [ ] Anzu: Integrate anzu with lsp-mode server only if possible and if needed
- [ ] Iedit: Is the current evil-multiedit need to be configured together with iedit?
- [ ] Iedit: Is iedit needed to integrated with lsp-mode

### Lexical Validation & Comparative Workflows (Group 8)

- [ ] Jinx: Instead of a dict.txt file, I prefer to have every exclusion written here and it should be case-insensitive
- [ ] Helpful: How does doom emacs configure the helpful package and what keybinding does doom emacs have?
- [ ] Helpful: It must integrate with the main development tools sections
- [ ] Helpful: Revisit it again after writing development tools
- [ ] Ediff: List all the features and functionalities of ediff

### Workflow Management

- [ ] Project Management: Determine what SPC f p does in doom emacs. Create a similar function for my config
- [ ] Project Management: Determine what SPC p p does in doom emacs. Create a similar function for my config
- [ ] Project Management: Use projection package and read the readme carefully for this package: https://raw.githubusercontent.com/mohkale/projection/refs/heads/master/README.org

### Org Mode & Second Brain

- [ ] Transient Template System: Review transient code beforehand
- [ ] Org Modern: Use this updated org-modern config instead

### Development Tools

- [ ] Language Server Protocol: Must disable and remove lsp-org at all costs
- [ ] Debug Adapter Protocol: Replace Dape with dap-mode
- [ ] Debug Adapter Protocol: Use Transient for dap-mode instead of the built-in hydra for dap-mode. Determine if installation of hydra can be skipped.
- [ ] Debug Adapter Protocol: dap-mode must integrate properly with lsp-mode
- [ ] Syntax Checking: Replace flymake with flycheck (flycheck and lsp-mode diagnostics must work together)
- [ ] Eldoc: What scenarios does the doom emacs project use eldoc for? And how does it do them? Search the web thoroughly.

### Highlight TODOs

- [ ] flycheck-todo: Uncomment when setting up flycheck

### LaTeX Writing Environment

- [ ] General: This section should have extra math ligatures for org-mode in source code blocks and latex environments
- [ ] Core AUCTeX: add preview-auto.el from https://github.com/ultronozm/preview-auto.el
- [ ] Flymake Integration: Replace with flycheck
- [ ] Bibliography Management: Uncomment when Project Management is setup
- [ ] Fast LaTeX Input: Use doom emacs configuration
- [ ] Preview LaTeX: Determine the difference between this preview package and preview-auto from https://github.com/ultronozm/preview-auto.el
- [ ] Completions: LSP + AUCTeX + Company Integration: Determine

### Languages

- [ ] Nix: Deal with later / Replace eglot with lsp-mode / Add flycheck
- [ ] Bash: Replace eglot with lsp-mode
- [ ] PlantUML: Deal with later
- [ ] C/C++: Deal with later
- [ ] Python: Add flycheck
- [ ] kdl: Deal with later
- [ ] yaml: Replace eglot with lsp-mode
- [ ] KBD: Deal with later

### Global Remaining Work

- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [x] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle (Solved via O(1) multi-char prose formatter in Electric Pair)
- [ ] Determine if compile-angel is needed for elpaca?
- [ ] Determine if elpaca byte-compiles and native-compiles all packages. And is it okay to leave these settings to elpaca's default.
- [ ] Pressing enter on a header should move a task from TODO to DONE like doom emacs does

### Global Questions

- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [x] Is there a way to execute nerd-icons-install-fonts during intial emacs setup using elpaca? (Note: Answered - manual execution is mandated to prevent startup network I/O blocking).
- [x] How do I get history evil commands in the echo area like doom emacs? (Note: Solved via `evil-ex-completion-map` arrow key bindings in the `Evil` subsection).
- [ ] How can I make the echo area useful? Do this at the very end of the configuration
- [ ] How to open links from org buffers by pressing enter and using the correct browser?
