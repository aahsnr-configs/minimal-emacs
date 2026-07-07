# Emacs Configuration Project State Checkpoint (v3 - July 08, 2026)

## CRITICAL INSTRUCTIONS FOR THE AI

Before generating any code, making suggestions, or answering questions, you MUST:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read this `project-state.md` file to understand the architectural decisions and current progress.
5. Acknowledge these rules and the current state, and identify the exact next subsection to tackle based on the `DONE` keywords in `config.org.txt`.
6. Do not write any code until the user explicitly gives the signal to proceed.

## Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents (e.g., `use-package`, `general-def`, `define-advice`, `with-eval-after-load`).
- **Keybinding Management:** `general.el` is the centralized keybinding manager. However, for core package bindings (like Embark or Vertico), native `use-package :bind` or `:commands` keywords are preferred to guarantee safe deferred autoloading.
- **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- **Concise Documentation Protocol:** Documentation inside `config.org` must be LLM-agnostic, objective, and follow a programmer's language (terse, technical, passive voice or objective present tense). Never use "I", "we", "you", or "let's". This protocol applies universally to BOTH the Org-mode text descriptions AND the Emacs Lisp comments inside the `#+begin_src` blocks.
- **Bundled Extensions & `:ensure nil`:** Because `use-package-always-ensure` is set to `t` globally (via Elpaca's `elpaca-use-package-by-default`), any `use-package` declaration for an extension bundled within a parent package's repository (e.g., `corfu-quick` inside `corfu`, `vertico-repeat` inside `vertico`) MUST explicitly include `:ensure nil` to prevent package manager startup errors.
- **Elpaca Hook Migration:** When utilizing the `elpaca` package manager, any `:hook` keyword using `after-init` or `emacs-startup` must be replaced with `elpaca-after-init`. Similarly, any `add-hook` call targeting `after-init-hook` or `emacs-startup-hook` must be replaced with `elpaca-after-init-hook` to ensure execution occurs only after Elpaca has activated all queued packages. _(Exception: The official Elpaca bootstrap snippet's internal `after-init-hook` for `elpaca-process-queues` must remain untouched)._

## Negative Constraints (Explicitly Rejected Patterns)

The AI must NEVER suggest or implement the following patterns, as they have been explicitly evaluated and rejected for this specific configuration:

- **No `completion-in-region-function` overrides:** Do not route `completion-in-region-function` to `consult-completion-in-region`. Corfu must remain strictly in-buffer.
- **No `which-key` Embark Hacks:** Do not use the legacy `embark-which-key-indicator` wiki hack. Embark actions must be displayed using the native `vertico-multiform` grid (`embark-keybinding` category).
- **No `consult-projectile`:** This package is obsolete and unmaintained.
- **No Minibuffer Evil Normal State:** Do not force `evil-normal-state` in the minibuffer to allow `j`/`k` scrolling. It breaks the type-to-filter paradigm.

## Edge Cases & Deferred Issues

This section tracks known architectural flaws, edge cases, or bugs that have been identified but are explicitly deferred for future sessions.

- _None currently deferred._ (Previous issues regarding Recentf Daemon Data Loss and Custom Functions Elpaca Adaptation have been successfully resolved and integrated into the `Session Management` and `Custom Functions` subsections, respectively).

## Pending Architectural Decisions & In-Code TODOs

This section tracks TODOs found in normal Org text and source code comments (types 2 and 3) that represent deferred architectural decisions, pending package replacements, or empty placeholders.

- **Project & Workspace Management:** The `Project Management` and `Workspaces` subsections under `Workflow Management` are currently empty placeholders. The choice of project manager and workspace manager is pending. All code referencing `projectile` or `persp-mode` (in Consult, Consult Dir, General Keybindings, Per-Project Org Context, Dirvish, and LaTeX Project Awareness) is currently commented out or marked with TODOs awaiting this decision.
- **Syntax Checking Transition:** Transitioning from `flymake` to `flycheck`. Multiple language configurations (LaTeX, Nix, Python, Markdown) have `flycheck` integrations commented out or marked TODO pending this switch. A core mandate notes that `flycheck and lsp-mode diagnostics must work together`.
- **Debug Adapter Protocol:** Text TODO indicates replacing `dape` with `dap-mode` (or finalizing the choice between them).
- **Emacs 31 Treesitter:** `Treesit` configuration is deferred until Emacs 31 release for automatic grammar installation. `treesit-fold` and `vimish-fold` require cross-referencing with Doom Emacs modules.
- **LaTeX / AUCTeX:** Pending integration of `preview-auto.el`, determining differences between preview packages, using Doom's `cdlatex` config, determining LSP + AUCTeX + Company integration, and uncommenting Bibliography Management (`citar`, `reftex`) once Project Management is decided.
- **LSP / Eglot Consolidation:** Several language blocks (Nix, Bash, YAML) contain TODOs to replace `eglot` with `lsp-mode`.
- **Terminal:** Text TODO under `* Terminal` to replace `eshell-mode` with `term` from Doom Emacs modules.
- **Vim Emulation / Evil:** Open question on how to get history evil commands in the echo area like Doom Emacs, and whether there are other useful Evil-related packages.
- **Editor Behaviour:** `Stripspace` requires looking at Doom Emacs modules for inspiration.
- **Languages:** Nix, PlantUML, C/C++, kdl, and KBD are marked as "Deal with later". Python and Nix require `flycheck` additions.
- **Miscellaneous Code/Text TODOs:** `Completions: LSP + AUCTeX` (Determine integration), and the `* Remaining Work` / `* Questions` checklists at the end of `config.org`.

## Foundational Files Status

- **`early-init.el` (Finalized):** Handles maximum GC deferral, `read-process-output-max` (4MB for LSP/ripgrep), lexical capture of `file-name-handler-alist` for startup speed, redisplay/frame optimizations, native-comp `eln-cache` redirection, and UI stripping. Restores GC and handlers via `emacs-startup-hook`. Disables `package-enable-at-startup` to prepare for Elpaca.
- **`config.org` (In Progress):** The main literate configuration file. Uses the `DONE` keyword to track progress.
  - A main section (e.g., `* DONE Core Emacs`) is only marked `DONE` if all its subsections are finalized.
  - Subsections (e.g., `** DONE Package Management`) are marked `DONE` when their code is reviewed, debugged, and finalized.
  - _Note on Second Brain & Productivity:_ The main heading `* Second Brain & Productivity` is missing the `TODO` keyword in the raw text file, but all of its subsections are marked `** TODO`. It remains functionally pending.

## Completion Framework Status

- **Current State:** The code blocks for the Completion Framework (Orderless, Vertico, Marginalia, Consult, Embark, Corfu, Cape, Dabbrev) contain extensive, highly optimized implementations and architectural documentation.
- **Pending Action:** In the current `config.org` file, these subsections are explicitly marked with the `TODO` keyword (e.g., `** TODO Orderless`). They require final review and the application of the `DONE` keyword to be considered officially finalized in the tracking system.

## Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** All 13 subsections finalized and main heading officially marked `* DONE Core Emacs`.

### Partially Finalized Main Sections

- None.

### Untouched / Pending Subsections & Main Sections (In Document Order)

- **Vim Emulation:** Undo Fu, Goto Chg, Evil, Evil Collection, Evil Surround, Evil Args, Evil Numbers, Evil Exchange, Evil Goggles, Evil Lion, Evil Window Movement Enhancements, Native Commenting Operator, Smart Comment Continuation.
- **Editor Behaviour:** Child Frame, Nerd Icons, Transient, Autorevert, Anzu, Automatic parenthesis pairing, Edit multiple regions simultaneously, Sudo edit, Subword, Buffer Terminator, Helpful, Jinx, Stripspace, Fonts, Doom Themes, Solaire Mode, Modeline, Hide Modeline, Line Numbers, Display dividers, Text Scaling, Scrolling, Highlight current line, Ediff.
- **Version Control:** Magit, Forge, Diff-hl, Git-Timemachine, Magit Custom Functions, Transient Menu.
- **Org Mode:** Dynamic Directory Structure, Per-Project Org Context, Register Projects, Better Font Faces, Core Configuration, Hooks, Keywords, Tags & Priorities, Org Structure Templates, Transient Template System, Org Modern, Org Src Buffer Naming, Org Habit, Eldoc for Org Mode, Org Appear.
- **Second Brain & Productivity:** Denote Directory, Denote, Denote Journal, Denote Org, Consult Denote, Citar Denote, Org GTD, Weekly Review, Org Agenda, Org Capture, Org Super Agenda, Org Agenda Custom Commands.
- **Workflow Management:** Dired, Dired Extensions, Dirvish, Project Management, iBuffer, Treemacs, Workspaces.
- **Completion Framework:** Orderless, Vertico, Marginalia, Nerd Icons Completion, Consult, Consult Dir, Embark, Embark Consult, Embark Org, Corfu, Basic Completion, Nerd Icons Corfu, Cape, Dabbrev. _(Code is present, awaiting final sign-off/DONE keyword)_.
- **Development Tools:** Language Server Protocol, Transient Menu, Formatting, Debug Adapter Protocol, Syntax Checking, Direnv.
- **Treesitter/Folding:** Treesit, Treesit-Fold, Vimish Fold.
- **Highlight TODOs:** hl-todo, flycheck-todo, consult-todo, magit-todos.
- **Rainbow Mode**
- **Delimiters:** show-paren-mode, rainbow-delimiters.
- **Indentations:** Indent Bars, Dirt Indent.
- **Snippet Engine:** Yasnippet, File Templates.
- **LaTeX Writing Environment:** Utility Functions, Project Awareness, Adaptive Wrap, Core AUCTeX, Flymake Integration, TeX Site, Misc LaTeX, TeX Folding, Electric Pair Disabling, Evil Integration, Bibliography Management, Fast LaTeX Input, Preview LaTeX, Org Mode Integration, Org LaTeX Classes, Completions, LaTeX Keybindings, Formatting.
- **Which Key**
- **Terminal**
- **Prettify Symbols:** Common, Python, JavaScript/TypeScript, Emacs Lisp, Common Lisp, C/C++ modes, TypeScript, Shell Script, Markdown Mode, LaTeX, YAML, JSON, Hooks.
- **Languages:** Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- **Study:** PDF Tools, Save PDF View, Org Noter, Org PDF Tools, Org Noter PDF Tools.
- **Window:** Winner, Popper.
- **AI/LLM**
- **General Keybindings**

## Remaining Work & Questions (Checklists)

These checklists are tracked directly from the end of `config.org` to ensure no actionable items or architectural questions are lost during context truncation.

### Remaining Work

- [ ] Add a doom emacs style keybinding with SPC as leader in general.el for `org-babel-remove-result-one-or-many`
- [ ] Remove flycheck for now since messing with org-mode editing
- [ ] Customize org-agenda-finalize to customize org-agenda buffer
- [ ] Setup automatic pair generation for '**', '==', etc and make sure the cursor in insert mode is placed in the middle

### Questions

- [ ] Can org-level variation can only work for certain file and/or org buffers?
- [ ] Are there any other evil related packages that can useful for my workflow?
- [ ] How do I get history evil commands in the echo area like doom emacs?
