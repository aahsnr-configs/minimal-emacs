# Emacs Configuration Project State Checkpoint (v6 - July 08, 2026)

## CRITICAL INSTRUCTIONS FOR THE NEW AI SESSION

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read and ingest the attached `editor-architecture.md` file to understand the massive restructuring of the `Editor Behaviour` section and the Non-Lisp AST structural editing paradigm.
5. Read this `project-state.md` file to understand the architectural decisions, the massive "Editor Behaviour" restructuring, and current progress.
6. Acknowledge these rules and the current state.
7. **STRICT GREENLIGHT PROTOCOL:** Do not write any code or output any `#+begin_src` blocks until the user explicitly gives the signal to proceed.

## Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents.
- **Keybinding Management:** `general.el` is the centralized keybinding manager. Global motions and commands must be routed through `general.el`. Operator-pending text objects must be injected directly into Evil's internal C-level keymaps via `define-key` in a `:config` block guarded by `:after evil`.
- **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer and window management to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- **Concise Documentation Protocol (Hard Negative Constraint):** Documentation inside `config.org` (both Org-mode text descriptions under headers AND Emacs Lisp comments inside `#+begin_src` blocks) must be ruthlessly terse (strictly 1-2 short sentences maximum, passive/objective voice). Never use "I", "we", "you", or "let's".
- **Bundled Extensions & `:ensure nil`:** Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository MUST explicitly include `:ensure nil`.
- **Elpaca Hook Migration:** Any `:hook` keyword using `after-init` or `emacs-startup` must be replaced with `elpaca-after-init`.

## The "Editor Behaviour" Paradigm Shift (v5 Architecture)

In v5, the `Editor Behaviour` main section was massively restructured to become the undisputed, centralized pillar for **how Emacs renders, parses, and manipulates text and buffers**. It absorbed multiple sections from other parts of the config and adopted a strict Non-Lisp AST (Abstract Syntax Tree) paradigm.

- **For the exact 8 Logical Groups, load-order prerequisites, migration details, and the philosophical/technical justification of the AST stack:** Refer to `editor-architecture.md`.

## Negative Constraints (Explicitly Rejected Patterns)

The AI must NEVER suggest or implement the following patterns:

- **No `smartparens` or `puni`:** Regex-based pair tracking causes desyncs in complex strings/templates. Lisp-centric soft-deletion is irrelevant for Non-Lisp AST languages.
- **No `combobulate`:** Fundamentally clashes with Evil's operator-pending grammar and causes keymap collisions.
- No `completion-in-region-function` overrides (Corfu must remain strictly in-buffer).
- No `which-key` Embark Hacks (use `vertico-multiform` grid).
- No `consult-projectile` (obsolete).
- No Minibuffer Evil Normal State (breaks type-to-filter).
- No custom `RET` intercepts for comment continuation (use native `comment-indent-new-line`).
- No custom window movement Elisp functions (use native `windmove-wrap-around`).

## Edge Cases & Deferred Issues

- **Dirvish Multi-frame Flicker:** Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- **lsp-mode Org Element API Crash:** Patched via `:around` advice in the `lsp-mode :init` block to prevent `cl-generic` corruption.
- **Emacs 31 Unreleased APIs:** Features like `grep-edit-mode` or MPS Incremental GC (`igc`) must be wrapped in defensive runtime guards.

## Pending Architectural Decisions & In-Code TODOs

- **Project & Workspace Management:** `Project Management` and `Workspaces` are empty placeholders. All code referencing `projectile` or `persp-mode` is commented out awaiting this decision.
- **Syntax Checking Transition:** Transitioning from `flymake` to `flycheck`.
- **Debug Adapter Protocol:** Evaluating `dape` vs `dap-mode`.
- **Emacs 31 Treesitter:** `Treesit` grammar installation deferred until Emacs 31 release for automatic grammar installation.
- **LaTeX / AUCTeX:** Pending integration of `preview-auto.el`, `cdlatex`, and Bibliography Management.
- **LSP / Eglot Consolidation:** Several language blocks contain TODOs to replace `eglot` with `lsp-mode`.

## Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** All 13 subsections finalized.

### Partially Finalized Main Sections

- **Vim Emulation:** 12 subsections finalized (`Undo Fu`, `Goto Chg`, `Evil`, `Evil Collection`, `Evil Commentary`, `Evil Multiedit`, `Evil Args`, `Evil Numbers`, `Evil Exchange`, `Evil Goggles`, `Evil Lion`).
  - _Note:_ `Evil Surround` and `Evil Matchit` were **moved** to `Editor Behaviour` (Group 3).
  - _Pending:_ `Smart Comment Continuation`.
- **Window:** 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.

### Untouched / Pending Main Sections (In Document Order)

- **Editor Behaviour:** _Massively restructured in v5._ The headers exist in `config.org` but require strict reordering, migration of external sections, and the creation of the new AST stack (Group 3).
- **Version Control:** Magit, Forge, Diff-hl, Git-Timemachine, Transient Menu.
- **Org Mode & Second Brain:** Dynamic Directory Structure, Per-Project Context, Denote, Org GTD, Org Agenda, Org Capture, Org Super Agenda.
- **Workflow Management:** Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
- **Completion Framework:** Orderless, Vertico, Marginalia, Consult, Embark, Corfu, Cape, Dabbrev. (Code is heavily optimized and present, but main heading and subsections still marked `TODO` pending final sign-off).
- **Development Tools:** LSP, Formatting, DAP, Syntax Checking, Direnv, Eldoc. _(Note: Treesit, Treesit-Fold, and Vimish-Fold were **moved** to Editor Behaviour)._
- **Highlight TODOs:** hl-todo, consult-todo, magit-todos.
- **Snippet Engine:** Yasnippet, File Templates.
- **LaTeX Writing Environment:** AUCTeX, TeX Folding, Evil Integration, Bibliography, Preview, Org Integration.
- **Languages:** Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- **Study:** PDF Tools, Org Noter.
- **Misc:** General Keybindings, AI/LLM. _(Note: Terminal, Which-Key, and Prettify Symbols were **moved** to Editor Behaviour)._

## Immediate Structural Action Plan for `config.org.txt`

The next task for the AI is to execute the physical restructuring of the `config.org.txt` file to match the v5 "Editor Behaviour" paradigm. **No Elisp code should be written until these Org-mode structural edits are complete.**

1.  **Reorder `Editor Behaviour`:** Reorder the existing subsections under `* TODO Editor Behaviour` to strictly follow the 8 Logical Groups defined in `editor-architecture.md`.
2.  **Migrate from `Development Tools`:** Move the `** TODO Treesit`, `** TODO Treesit-Fold`, and `** TODO Vimish Fold` blocks into `Editor Behaviour` (Groups 3 and 4).
3.  **Migrate from `Misc`:** Move the `** TODO Terminal`, `** TODO Which Key` blocks into `Editor Behaviour` (Groups 2 and 1).
4.  **Migrate from `Vim Emulation`:** Move the `** TODO Evil Surround` and `** DONE Evil Matchit` blocks into `Editor Behaviour` (Group 3).
5.  **Delete & Replace `Prettify Symbols`:** Delete the massive, multi-language regex `* TODO Prettify Symbols` main section from the bottom of the file. Create a new, highly simplified `** TODO Prettify Symbols` subsection inside `Editor Behaviour` (Group 1) utilizing a basic global `prettify-symbols-mode` alist.
6.  **Create New AST Subsections:** Inside `Editor Behaviour` (Group 3), create empty `** TODO` placeholder headers for the new Non-Lisp stack: `delete-pair`, `blink-matching-paren`, `evil-textobj-tree-sitter`, `treesit-navigate-thing`, `expreg`, and `tree-edit`.

## Remaining Work & Questions (Checklists)

- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [ ] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle
- [ ] Determine if compile-angel is needed for elpaca?
- [ ] Determine if elpaca byte-compiles and native-compiles all packages. And is it okay to leave these settings to elpaca's default.
- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [ ] Is there a way to execute nerd-icons-install-fonts during intial emacs setup using elpaca?
- [ ] How do I get history evil commands in the echo area like doom emacs? (Note: Solved via `evil-ex-completion-map` arrow key bindings in the `Evil` subsection).
