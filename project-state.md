# Emacs Configuration Project State Checkpoint (v2 - July 05, 2026)

## 1. CRITICAL INSTRUCTIONS FOR THE AI

Before generating any code, making suggestions, or answering questions, you **MUST**:

1. Read and ingest the attached `early-init.el.txt` file.
2. Read and ingest the attached `config.org.txt` file.
3. Read and ingest the attached `system-prompt-protocol.md` file to understand your strict operational boundaries, research mandates, and formatting rules.
4. Read this `project-state.md` file to understand the architectural decisions and current progress.
5. Acknowledge these rules and the current state, and identify the exact next subsection to tackle based on the `DONE` keywords in `config.org.txt`.
6. **Do not write any code** until the user explicitly gives the signal to proceed.

## 2. Core Architectural Rules & Constraints

The AI must strictly adhere to the following rules when generating or modifying Emacs Lisp code:

- **Strict Org-Mode Formatting:** Zero Markdown syntax is allowed to bleed into Org-mode text or source blocks inside the `config.org` file. Use `=code=` or `~code~` for inline code, `*bold*` for bold, `/italics/` for italics, and standard Org headings (`*`, `**`, `***`).
- **Vanilla Emacs Paradigm:** Do not use Doom Emacs proprietary macros (e.g., `map!`, `defadvice!`, `use-package!`). Translate Doom-inspired logic into native Vanilla Emacs equivalents (e.g., `use-package`, `general-def`, `define-advice`, `with-eval-after-load`).
- **Keybinding Management:** `general.el` is the centralized keybinding manager. However, for core package bindings (like Embark or Vertico), native `use-package` `:bind` or `:commands` keywords are preferred to guarantee safe deferred autoloading.
- **Corfu Confinement:** `corfu` is strictly confined to buffer editing. It must _never_ be enabled in the minibuffer, as it conflicts with `vertico`.
- **Projectile Reliance:** `consult` and `consult-dir` must strictly rely on `projectile-project-root`. Native `project.el` fallbacks are explicitly disabled.
- **Workspace Isolation:** `persp-mode` is used for strict buffer isolation. Any code querying perspective buffers must include a safe `let` guard to prevent `wrong-type-argument` crashes when querying the global `nil` ("main") perspective.
- **Minibuffer Navigation:** Arrow keys (`<up>`, `<down>`, `C-<up>`, `C-<down>`) are preferred over `hjkl` in the minibuffer to preserve the "type-to-filter" paradigm and prevent Evil state conflicts.
- **Impersonal Documentation:** Documentation inside `config.org` must be LLM-agnostic and objective. Never use "I", "we", "you", or "let's".

## 3. Negative Constraints (Explicitly Rejected Patterns)

The AI must **NEVER** suggest or implement the following patterns, as they have been explicitly evaluated and rejected for this specific configuration:

- **No `completion-in-region-function` overrides:** Do not route `completion-in-region-function` to `consult-completion-in-region`. Corfu must remain strictly in-buffer; stealing completions into the minibuffer breaks the established workflow.
- **No `which-key` Embark Hacks:** Do not use the legacy `embark-which-key-indicator` wiki hack. Embark actions must be displayed using the native `vertico-multiform` grid (`embark-keybinding` category).
- **No `consult-projectile`:** This package is obsolete and unmaintained. Projectile integration must be handled natively via `consult-dir` and `consult-project-function`.
- **No Minibuffer Evil Normal State:** Do not force `evil-normal-state` in the minibuffer to allow `j`/`k` scrolling. It breaks the type-to-filter paradigm.

## 4. Edge Cases & Deferred Issues

This section tracks known architectural flaws, edge cases, or bugs that have been identified but are explicitly deferred for future sessions.

- **Recentf Daemon Data Loss:**
  - _The Issue:_ `recentf-mode` relies on `kill-emacs-hook` to save its cache. When running Emacs as a daemon, closing `emacsclient` does not trigger this hook. If the daemon crashes or the OS reboots abruptly, the recent files list is lost.
  - _The Fix Required:_ Implement a periodic `run-at-time` background timer to force `recentf-save-list` every 5 minutes.
  - _Status:_ **Deferred.** Currently, `recentf` is configured in `Core Emacs -> Session Management`, but the daemon-safe timer needs to be injected in a future session.

## 5. Foundational Files Status

- **early-init.el (Finalized):** Handles maximum GC deferral, `read-process-output-max` (4MB for LSP/ripgrep), lexical capture of `file-name-handler-alist` for startup speed, redisplay/frame optimizations, native-comp `eln-cache` redirection, and UI stripping. Restores GC and handlers via `emacs-startup-hook`.
- **config.org (In Progress):** The main literate configuration file. Uses the `DONE` keyword to track progress.
  - A main section (e.g., `* DONE Core Emacs`) is only marked `DONE` if _all_ its subsections are finalized.
  - Subsections (e.g., `** DONE Orderless`) are marked `DONE` when their code is reviewed, debugged, and finalized.

## 6. Completion Framework Status (Current Focus)

The Completion Framework is partially finalized. The minibuffer stack and context-action stack are complete; the in-buffer completion stack is pending.

### Finalized Subsections (Do Not Modify)

- **Orderless:** Doom-inspired affix dispatchers, O(1) string parsing, Consult "tofu" regex injection.
- **Vertico:** Multiform traffic controller, `vertico-quick`, `vertico-repeat`, visual transforms (Doom-inspired), Embark grid synergy, safe load-order guarantees, arrow key navigation.
- **Marginalia:** Absolute timestamps, right-edge alignment.
- **Nerd Icons Completion:** Hooked into Marginalia.
- **Consult:** Strict Projectile reliance, `hl-todo` preview, hidden file/git exclusion, Vim-style line search, safe `persp-mode` buffer isolation.
- **Consult Dir:** Native Projectile and `fd` integration (obsolete `consult-projectile` removed).
- **Embark:** Modern Vertico grid synergy (removed legacy `which-key` hack), Doom-style native bindings, safe deferred autoloading.
- **Embark Consult:** Live preview in collect buffers, `grep-mode` exports.
- **Embark Org:** Bundled GNU ELPA extension for Org AST targets.

### Pending Subsections (Next Immediate Tasks)

These subsections exist in `config.org` but lack the `DONE` keyword and require review, optimization, and finalization:

1. **Corfu:** In-buffer completion. Needs review of `lsp-capf` overrides, `corfu-history`, and Evil state exit hooks.
2. **Basic Completion:** `tab-always-indent` and `read-extended-command-predicate`.
3. **Nerd Icons Corfu:** Margin formatter integration.
4. **Cape:** `ar/org-elisp-capf`, `lsp-mode` advices, and general Capf hooks.
5. **Dabbrev:** Custom buffer scanning size limits and ignored modes/regexps.

## 7. Overall Configuration Progress

### Fully Finalized Main Sections

- **Core Emacs:** Bootstrap, Package Mgmt, Compile Angel, Performance, Session Management, etc.
- **Workflow Management:** Dired, Dirvish (with multi-frame flicker patch), Projectile, iBuffer, Treemacs, Perspective (with `uniquify` patch and emacsclient frame isolation).

### Untouched / Pending Main Sections

- Vim Emulation (Evil, Evil Collection, Surround, etc.)
- Editor Behaviour (Child Frame, Transient, Autorevert, Anzu, etc.)
- Development Tools (LSP, Flycheck, Apheleia, DAP)
- Treesitter/Folding
- Highlight TODOs
- Rainbow Mode, Delimiters, Indentations
- Snippet Engine (Yasnippet, File Templates)
- LaTeX Writing Environment
- Which Key, Terminal, Prettify Symbols, Languages, Study, Window, AI/LLM, General Keybindings.
