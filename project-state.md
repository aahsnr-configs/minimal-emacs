# Emacs Configuration Project State Checkpoint (v9 - July 10, 2026)

## CRITICAL INSTRUCTIONS FOR THE NEW AI SESSION

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read and ingest the attached `editor-architecture.md` file to understand the massive restructuring of the `Editor Behaviour` section and the Non-Lisp AST structural editing paradigm.
5. Read this `project-state.md` file to understand the architectural decisions, the massive "Editor Behaviour" restructuring, and current progress.
6. Acknowledge these rules and the current state.

**STRICT GREENLIGHT PROTOCOL:** Do not write any code or output any `#+begin_src` blocks until the user explicitly gives the signal to proceed.

## Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents.
- **Keybinding Management:** `general.el` is the centralized keybinding manager. Global motions and commands must be routed through `general.el`. Operator-pending text objects must be injected directly into Evil's internal C-level keymaps via `define-key` in a `:config` block guarded by `:after evil`.
- **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer and window management to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- **Concise Documentation Protocol (Hard Negative Constraint):** Documentation inside `config.org` (both Org-mode text descriptions under headers AND Emacs Lisp comments inside `#+begin_src` blocks) must be ruthlessly terse (strictly 1-2 short sentences maximum, passive/objective voice). Never use "I", "we", "you", or "let's".
- **Bundled Extensions & `:ensure nil`:** Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository MUST explicitly include `:ensure nil`.
- **Elpaca & `use-package` Load-Order Physics:**
  - NEVER use `:hook (elpaca-after-init . mode)` or `:hook (after-init . mode)` to activate global minor modes.
  - For built-in packages, invoke `(mode 1)` directly in `:config` (or `:init` if early interception is required) to eliminate artificial startup latency.
  - Reserve `elpaca-after-init-hook` strictly for cross-package state initialization.

## The "Editor Behaviour" Paradigm Shift & Main Section Naming

In v5, the `Editor Behaviour` concept was massively restructured to become the centralized pillar for how Emacs renders, parses, and manipulates text and buffers. In v9, the physical migration and finalization of these groups is the primary active objective.

1. **Visual Chrome & UI Overlays (Group 1 - FULLY FINALIZED)**
2. **Interactive Buffers & Redisplay Physics (Group 2 - FULLY FINALIZED)**
3. **AST Parsing & Structural Typing (Group 3 - FULLY FINALIZED)**
4. **Code Folding & Region Concealment (Group 4)**
5. **Spatial Alignment & Whitespace Hygiene (Group 5)**
6. **Frame Chrome & Status Indicators (Group 6)**
7. **Spatial Traversal & Inline Mutations (Group 7)**
8. **Lexical Validation & Comparative Workflows (Group 8)**

_For the exact load-order prerequisites, migration details, and the philosophical/technical justification of the AST stack: Refer to `editor-architecture.md`._

## Negative Constraints (Explicitly Rejected Patterns)

The AI must NEVER suggest or implement the following patterns:

- **No `smartparens` or `puni`:** Regex-based pair tracking causes desyncs in complex strings/templates. Lisp-centric soft-deletion is irrelevant for Non-Lisp AST languages.
- **No `combobulate`:** Fundamentally clashes with Evil's operator-pending grammar and causes keymap collisions.
- **No `tree-edit`:** Relies on the legacy `elisp-tree-sitter` (`tsc`) engine and forked grammars, causing dual-engine memory bloat and fatal conflicts with native Emacs 30 `treesit`.
- **No `expand-region`:** Replaced by `expreg`, which utilizes O(1) AST-aware pre-computation via native `treesit` instead of fragile regex/syntax-table heuristics.
- **No `completion-in-region-function` overrides** (Corfu must remain strictly in-buffer).
- **No `which-key` Embark Hacks** (use `vertico-multiform` grid).
- **No `consult-projectile`** (obsolete).
- **No Minibuffer Evil Normal State** (breaks type-to-filter).
- **No custom `RET` intercepts for comment continuation** (use native `comment-indent-new-line`).
- **No custom window movement Elisp functions** (use native `windmove-wrap-around`).
- **No Nerd Icons inside `prettify-symbols-alist`:** Injecting PUA glyphs into buffer text causes mid-line font-fallback context switches and sub-pixel grid misalignments.
- **No `elpaca-after-init` hooks for built-in global minor modes** (use direct `:config` invocation).
- **No custom ElDoc backends for `show-paren`** (use native Emacs 29+ `'overlay` engine).

## Edge Cases & Deferred Issues

- **Dirvish Multi-frame Flicker:** Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- **lsp-mode Org Element API Crash:** Patched via `:around` advice in the `lsp-mode :init` block to prevent `cl-generic` corruption.
- **Emacs 31 Unreleased APIs:** Features like `grep-edit-mode` or MPS Incremental GC (`igc`) must be wrapped in defensive runtime guards.
- **`no-littering` API Typo:** Fixed hallucinated `no-littering-expand-var-directory-name` to the correct `no-littering-expand-var-file-name` (with trailing slash for directories) in `treesit` and `undo-fu-session` blocks.
- **`treesit` Block Parenthesis Mismatch:** Fixed an extra closing parenthesis in the `treesit--build-grammar` advice that caused an `Invalid read syntax: )` crash during tangling.

## Architectural Decisions & Load-Order Physics (Session v9)

- **Non-Lisp AST Stack Finalization (Group 3 Complete):** Rejected `smartparens`/`puni`, `combobulate`, `tree-edit`, and `expand-region`. Standardized on `elec-pair`, `evil-surround` + `evil-embrace`, `delete-pair`, `evil-textobj-tree-sitter`, `evil-ts-obj`, `treesit-navigate-thing`, and `expreg` for structural editing.
- **`evil-ts-obj` Transient Isolation (Pillar 4):** Fulfills structural manipulation (slurp, barf, raise, extract). Default global bindings (`M-j`, `M-k`, etc.) are aggressively purged in `:config` to prevent collisions with `general.el` and `move-text`. Operators are strictly isolated within an `ar/ast-refactor-transient` menu routed to `SPC c s`.
- **`expreg` & `treesit-thing` Native Integration:** Leveraged Emacs 30's native C-level `treesit` and `treesit-thing` APIs for O(1) region expansion and sibling hopping (`]s`, `[s`, `]x`, `[x`), entirely bypassing heavy query-based overhead.
- **`general.el` Eager Load-Order Physics:** Verified that `general.el` is synchronously and eagerly loaded (`:ensure (:wait t)`, `:demand t`) at the end of the Vim Emulation section. This mathematically guarantees that `general-define-key`, `ar/global-leader`, and `ar/local-leader` are globally available for all downstream deferred packages without requiring `with-eval-after-load` wrappers.

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
- **AST Parsing & Structural Typing (Group 3):** All 12 subsections finalized (`Treesit`, `Electric Pair`, `Evil Surround`, `Delete Pair`, `Show Paren Mode`, `Blink Matching Paren`, `Rainbow Delimiters`, `Evil Matchit`, `Evil Textobj Tree-Sitter`, `Evil TS Obj`, `Treesit Navigate Thing`, `Expreg`).
- **Vim Emulation:** All 12 subsections finalized.

### Partially Finalized Main Sections

- **Window:** 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.
- **General Keybindings:** Elisp payload is heavily optimized and fully written, but main heading remains `TODO` pending final sign-off and integration testing.
- **Completion Framework:** Elisp payload is heavily optimized and present, but main heading and subsections still marked `TODO` pending final sign-off.

### Untouched / Pending Main Sections (In Document Order)

- **Editor Behaviour (Groups 4-8):** Require physical Org-mode restructuring and placeholder creation.
- **Version Control:** Magit, Forge, Diff-hl, Git-Timemachine, Transient Menu.
- **Org Mode & Second Brain:** Dynamic Directory Structure, Per-Project Context, Denote, Org GTD, Org Agenda, Org Capture, Org Super Agenda.
- **Workflow Management:** Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
- **Development Tools:** LSP, Formatting, DAP, Syntax Checking, Direnv, Eldoc.
- **Highlight TODOs:** hl-todo, consult-todo, magit-todos.
- **Snippet Engine:** Yasnippet, File Templates.
- **LaTeX Writing Environment:** AUCTeX, TeX Folding, Evil Integration, Bibliography, Preview, Org Integration.
- **Languages:** Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- **Study:** PDF Tools, Org Noter.
- **Misc:** AI/LLM.

## Immediate Structural Action Plan for `config.org.txt`

The Elisp payloads for Groups 1, 2, and 3 are fully finalized. The immediate next physical task requires structural reordering in `config.org`:

1. **REORGANIZE ENTIRE CONFIG:** Physically reorganize the whole `config.org` file to match the 8 Logical Groups defined in `editor-architecture.md`. Create the missing main section headers (`* Code Folding & Region Concealment`, etc.) and move the existing `** DONE` and `** TODO` subsections into their correct architectural pillars.
2. **Delete Legacy Bloat:** Delete the massive, multi-language regex `* TODO Prettify Symbols` main section from the bottom of the file (it has been replaced by the simplified Group 1 block).

## Remaining Work & Questions (Checklists)

- [x] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle (Solved via O(1) multi-char prose formatter in Electric Pair)
- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [ ] Determine if compile-angel is needed for elpaca?
- [ ] Determine if elpaca byte-compiles and native-compiles all packages. And is it okay to leave these settings to elpaca's default.
- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [x] Is there a way to execute nerd-icons-install-fonts during intial emacs setup using elpaca? (Note: Answered - manual execution is mandated to prevent startup network I/O blocking).
- [x] How do I get history evil commands in the echo area like doom emacs? (Note: Solved via `evil-ex-completion-map` arrow key bindings in the `Evil` subsection).
