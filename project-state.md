Emacs Configuration Project State Checkpoint (v3 - July 07, 2026)

1. CRITICAL INSTRUCTIONS FOR THE AI
   Before generating any code, making suggestions, or answering questions, you MUST:

- Read and ingest the attached `early-init.el.txt` file.
- Read and ingest the attached `config.org.txt` file.
- Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
- Read this `project-state.md` file to understand the architectural decisions and current progress.
- Acknowledge these rules and the current state, and identify the exact next subsection to tackle based on the `DONE` keywords in `config.org.txt`.
- Do not write any code until the user explicitly gives the signal to proceed.

2. Core Architectural Rules & Constraints
   The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- Strict Org-Mode Formatting: Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- Vanilla Emacs Paradigm: Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents (e.g., `use-package`, `general-def`, `define-advice`, `with-eval-after-load`).
- Keybinding Management: `general.el` is the centralized keybinding manager. However, for core package bindings (like Embark or Vertico), native `use-package :bind` or `:commands` keywords are preferred to guarantee safe deferred autoloading.
- Corfu Confinement: `corfu` is strictly confined to buffer editing. It must never be enabled in the minibuffer, as it conflicts with `vertico`.
- Minibuffer Navigation: Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- Impersonal Documentation: Documentation inside `config.org` must be LLM-agnostic and objective. Never use "I", "we", "you", or "let's".
- Bundled Extensions & `:ensure nil`: Because `use-package-always-ensure` is set to `t` globally, any `use-package` declaration for an extension bundled within a parent package's repository (e.g., `corfu-quick` inside `corfu`, `vertico-repeat` inside `vertico`) MUST explicitly include `:ensure nil` to prevent package.el startup errors.

3. Negative Constraints (Explicitly Rejected Patterns)
   The AI must NEVER suggest or implement the following patterns, as they have been explicitly evaluated and rejected for this specific configuration:

- No `completion-in-region-function` overrides: Do not route `completion-in-region-function` to `consult-completion-in-region`. Corfu must remain strictly in-buffer; stealing completions into the minibuffer breaks the established workflow.
- No `which-key` Embark Hacks: Do not use the legacy `embark-which-key-indicator` wiki hack. Embark actions must be displayed using the native `vertico-multiform` grid (`embark-keybinding` category).
- No Minibuffer Evil Normal State: Do not force `evil-normal-state` in the minibuffer to allow `j`/`k` scrolling. It breaks the type-to-filter paradigm.

4. Edge Cases & Deferred Issues
   This section tracks known architectural flaws, edge cases, or bugs that have been identified but are explicitly deferred for future sessions.

- Recentf Daemon Data Loss:
  - The Issue: `recentf-mode` relies on `kill-emacs-hook` to save its cache. When running Emacs as a daemon, closing `emacsclient` does not trigger this hook. If the daemon crashes or the OS reboots abruptly, the recent files list is lost.
  - The Fix Required: Implement a periodic `run-at-time` background timer to force `recentf-save-list` every 5 minutes.
  - Status: Deferred. Currently, `recentf` is configured in `Core Emacs -> Session Management`, but the daemon-safe timer needs to be injected in a future session.

5. Pending Architectural Decisions & In-Code TODOs
   This section tracks TODOs found in normal Org text and source code comments (types 2 and 3) that represent deferred architectural decisions, pending package replacements, or empty placeholders.

- Project & Workspace Management: The `Project Management` and `Workspaces` subsections under `Workflow Management` are currently empty placeholders. The choice of project manager and workspace manager is pending. All code referencing `projectile` or `persp-mode` (in Consult, Consult Dir, General Keybindings, Per-Project Org Context, and Bibliography Management) is currently commented out or marked with TODOs awaiting this decision.
- Package Manager Migration: `package.el` is currently in use, but text TODOs in `Package Management` and `Custom Functions` mandate a future replacement with `elpaca`.
- Syntax Checking Transition: Transitioning from `flymake` to `flycheck`. Multiple language configurations (LaTeX, Nix, Python, Markdown) have `flycheck` integrations commented out or marked TODO pending this switch.
- Debug Adapter Protocol: Text TODO indicates replacing `dape` with `dap-mode` (or finalizing the choice between them).
- Emacs 31 Treesitter: `Treesit` configuration is deferred until Emacs 31 release for automatic grammar installation. `treesit-fold` and `vimish-fold` require cross-referencing with Doom Emacs modules.
- LaTeX / AUCTeX: Pending integration of `preview-auto.el`, determining differences between preview packages, using Doom's `cdlatex` config, and uncommenting Bibliography Management (`citar`, `reftex`) once Project Management is decided.
- LSP / Eglot Consolidation: Several language blocks (Nix, Bash, YAML) contain TODOs to replace `eglot` with `lsp-mode`.
- Terminal: Text TODO under `* Terminal` to replace `eshell-mode` with `term` from Doom Emacs modules.
- Miscellaneous Code/Text TODOs: `Stripspace` (Doom inspiration), `Completions: LSP + AUCTeX` (Determine integration), and the `* Remaining Work` / `* Questions` checklists at the end of `config.org`.

6. Foundational Files Status

- early-init.el (Finalized): Handles maximum GC deferral, `read-process-output-max` (4MB for LSP/ripgrep), lexical capture of `file-name-handler-alist` for startup speed, redisplay/frame optimizations, native-comp `eln-cache` redirection, and UI stripping. Restores GC and handlers via `emacs-startup-hook`.
- config.org (In Progress): The main literate configuration file. Uses the `DONE` keyword to track progress.
  - A main section (e.g., `* DONE Core Emacs`) is only marked `DONE` if all its subsections are finalized.
  - Subsections (e.g., `** DONE Orderless`) are marked `DONE` when their code is reviewed, debugged, and finalized.

7. Completion Framework Status (FULLY FINALIZED)
   The entire Completion Framework has been rigorously reviewed, optimized for load-order physics, and finalized.

- Finalized Subsections: Orderless, Vertico, Marginalia, Nerd Icons Completion, Consult, Consult Dir, Embark, Embark Consult, Embark Org, Corfu, Basic Completion, Nerd Icons Corfu, Cape, Dabbrev.

8. Overall Configuration Progress

Fully Finalized Main Sections

- Completion Framework: All subsections finalized.

Partially Finalized Main Sections

- Core Emacs: Lexical Binding, Constants, Performance Tuning, Small Configs, Auto Save, Garbage Collector, UTF-8 Coding System, Session Management, Misc, Setup User, Title. (Pending: Package Management, Custom Functions).
- Editor Behaviour: Child Frame, Nerd Icons, Transient, Autorevert, Anzu, Automatic parenthesis pairing, Edit multiple regions simultaneously, Sudo edit, Subword, Buffer Terminator, Helpful, Jinx, Fonts, Doom Themes, Solaire Mode, Modeline, Hide Modeline, Line Numbers, Display dividers, Text Scaling, Scrolling, Highlight current line, Ediff. (Pending: Stripspace).
- Workflow Management: Dired, Dired Extensions, Dirvish, iBuffer, Treemacs. (Pending: Project Management, Workspaces).
- Org Mode: Dynamic Directory Structure, Better Font Faces, Core Configuration, Hooks, Keywords, Tags & Priorities, Org Structure Templates, Transient Template System, Org Modern, Org Src Buffer Naming, Org Habit, Eldoc for Org Mode. (Pending: Per-Project Org Context, Org Appear).
- Second Brain & Productivity: Denote Directory, Denote, Denote Journal, Denote Org, Consult Denote, Citar Denote, Org GTD, Weekly Review, Org Agenda, Org Capture, Org Super Agenda, Org Agenda Custom Commands.
- Version Control: Magit, Forge, Diff-hl, Git-Timemachine, Magit Custom Functions, Transient Menu.
- Development Tools: Direnv, Language Server Protocol, Transient Menu, Formatting. (Pending: Debug Adapter Protocol, Syntax Checking).

Untouched / Pending Subsections & Main Sections (In Document Order)

- Vim Emulation: Undo Fu, Goto Chg, Evil, Evil Collection, Evil Surround, Evil Args, Evil Numbers, Evil Exchange, Evil Goggles, Evil Lion, Evil Window Movement Enhancements, Native Commenting Operator, Smart Comment Continuation.
- LaTeX Writing Environment: Utility Functions, Project Awareness, Adaptive Wrap, Core AUCTeX, Flymake Integration, TeX Site, Misc LaTeX, TeX Folding, Electric Pair Disabling, Evil Integration, Bibliography Management, Fast LaTeX Input, Preview LaTeX, Org Mode Integration, Org LaTeX Classes, Completions, LaTeX Keybindings, Formatting.
- Treesitter/Folding: Treesit, Treesit-Fold, Vimish Fold.
- Highlight TODOs: hl-todo, flycheck-todo, consult-todo, magit-todos.
- Rainbow Mode
- Delimiters: show-paren-mode, rainbow-delimiters.
- Indentations: Indent Bars, Dirt Indent.
- Snippet Engine: Yasnippet, File Templates.
- Which Key
- Terminal
- Prettify Symbols: Common, Python, JavaScript/TypeScript, Emacs Lisp, Common Lisp, C/C++ modes, TypeScript, Shell Script, Markdown Mode, LaTeX, YAML, JSON, Hooks.
- Languages: Nix, Bash, PlantUML, C/C++, Python, kdl, yaml, KBD, Markdown.
- Study: PDF Tools, Save PDF View, Org Noter, Org PDF Tools, Org Noter PDF Tools.
- Window: Winner, Popper.
- AI/LLM
- General Keybindings (Contains commented-out project/workspace blocks pending architectural decision).
- Remaining Work & Questions (Checklists at the end of config.org).
