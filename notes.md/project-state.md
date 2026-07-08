# Emacs Configuration Project State Checkpoint (v4 - July 08, 2026)

## CRITICAL INSTRUCTIONS FOR THE AI

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` (or `config.org.new.txt`) file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read this `project-state.md` file to understand the architectural decisions and current progress.
5. Acknowledge these rules and the current state, and identify the exact next subsection to tackle based on the `DONE` keywords in the config file.
6. Do not write any code until the user explicitly gives the signal to proceed.

## Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents.
- **Keybinding Management:** `general.el` is the centralized keybinding manager. Global motions and commands must be routed through `general.el`. Operator-pending text objects (e.g., `evil-args`) must be injected directly into Evil's internal C-level keymaps via `define-key` in a `:config` block guarded by `:after evil`.
- **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer and window management to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- **Concise Documentation Protocol (Hard Negative Constraint):** Documentation inside `config.org` (both Org-mode text descriptions under headers AND Emacs Lisp comments inside `#+begin_src` blocks) must be ruthlessly terse (strictly 1-2 short sentences maximum, passive/objective voice). Never use "I", "we", "you", or "let's". Do not bleed conversational depth into the configuration file.
- **Bundled Extensions & `:ensure nil`:** Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository MUST explicitly include `:ensure nil`.
- **Elpaca Hook Migration:** Any `:hook` keyword using `after-init` or `emacs-startup` must be replaced with `elpaca-after-init`.

## Negative Constraints (Explicitly Rejected Patterns)

The AI must NEVER suggest or implement the following patterns:

- No `completion-in-region-function` overrides (Corfu must remain strictly in-buffer).
- No `which-key` Embark Hacks (use `vertico-multiform` grid).
- No `consult-projectile` (obsolete).
- No Minibuffer Evil Normal State (breaks type-to-filter).
- No `smartparens` or `puni` for non-Lisp structural editing (causes pair desyncs in complex strings/templates).
- No `combobulate` (fundamentally clashes with Evil's operator-pending grammar).
- No custom `RET` intercepts for comment continuation (use native `comment-indent-new-line`).
- No custom window movement Elisp functions (use native `windmove-wrap-around`).

## Edge Cases & Deferred Issues

- **Dirvish Multi-frame Flicker:** Patched via `define-advice` on `dirvish-pre-redisplay-h` to debounce redisplay hooks in Emacs 30+.
- **lsp-mode Org Element API Crash:** Patched via `:around` advice in the `lsp-mode` `:init` block to prevent `cl-generic` corruption.
- **Emacs 31 Unreleased APIs:** Features like `grep-edit-mode` or MPS Incremental GC (`igc`) must be wrapped in defensive runtime guards.

## Pending Architectural Decisions & In-Code TODOs

- **Project & Workspace Management:** `Project Management` and `Workspaces` are empty placeholders. All code referencing `projectile` or `persp-mode` is commented out awaiting this decision.
- **Syntax Checking Transition:** Transitioning from `flymake` to `flycheck`.
- **Debug Adapter Protocol:** Evaluating `dape` vs `dap-mode`.
- **Emacs 31 Treesitter:** `Treesit` configuration deferred until Emacs 31 release for automatic grammar installation.
- **LaTeX / AUCTeX:** Pending integration of `preview-auto.el`, `cdlatex`, and Bibliography Management.
- **LSP / Eglot Consolidation:** Several language blocks contain TODOs to replace `eglot` with `lsp-mode`.

## Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** All 13 subsections finalized.

### Partially Finalized Main Sections

- **Vim Emulation:** 12 subsections finalized (`Undo Fu`, `Goto Chg`, `Evil`, `Evil Collection`, `Evil Matchit`, `Evil Commentary`, `Evil Multiedit`, `Evil Args`, `Evil Numbers`, `Evil Exchange`, `Evil Goggles`, `Evil Lion`). Pending: `Evil Surround`, `Smart Comment Continuation`. (Note: `Evil Window Movement Enhancements` and `Native Commenting Operator` were deleted and replaced by `Windmove` and `Evil Commentary` respectively).
- **Window:** 1 subsection finalized (`Windmove`). Pending: `Winner`, `Popper`.

### Untouched / Pending Subsections & Main Sections (In Document Order)

- **Editor Behaviour:** Child Frame, Nerd Icons, Fonts, Themes, Transient, Avy, Autorevert, Anzu, Parenthesis pairing, show-paren, iedit, Sudo edit, Subword, Buffer Terminator, Helpful, Jinx, Stripspace, Dirt Indent, Indent Bars, Rainbow Mode, Modeline, Hide Modeline, Line Numbers, Dividers, Text Scaling, Scrolling, Hl-line, Ediff, Rainbow-delimiters.
- **Version Control:** Magit, Forge, Diff-hl, Git-Timemachine, Transient Menu.
- **Org Mode & Second Brain:** Dynamic Directory Structure, Per-Project Context, Denote, Org GTD, Org Agenda, Org Capture, Org Super Agenda.
- **Workflow Management:** Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
- **Completion Framework:** Orderless, Vertico, Marginalia, Consult, Embark, Corfu, Cape, Dabbrev. (Code is heavily optimized and present, but main heading and subsections still marked `TODO` pending final sign-off).
- **Development Tools:** LSP, Formatting, DAP, Syntax Checking, Direnv, Treesit, Treesit-Fold, Vimish Fold.
- **Highlight TODOs:** hl-todo, consult-todo, magit-todos.
- **Snippet Engine:** Yasnippet, File Templates.
- **LaTeX Writing Environment:** AUCTeX, TeX Folding, Evil Integration, Bibliography, Preview, Org Integration.
- **Languages:** Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- **Study:** PDF Tools, Org Noter.
- **Misc:** Which Key, Terminal, Prettify Symbols, General Keybindings, AI/LLM.

## Remaining Work & Questions (Checklists)

- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [ ] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle
- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow? (Note: `evil-matchit`, `evil-commentary`, `evil-multiedit` added. `evil-textobj-tree-sitter` evaluated).
- [ ] How do I get history evil commands in the echo area like doom emacs? (Note: Solved via `evil-ex-completion-map` arrow key bindings in the `Evil` subsection).
