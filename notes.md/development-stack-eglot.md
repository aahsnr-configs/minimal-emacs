# Development Stack (Native-First Emacs 30/31 Architecture)

This document serves as the exhaustive, definitive 1:1 feature parity mapping for transitioning from the heavy `lsp-mode`/`lsp-ui`/`dap-mode`/`flycheck` ecosystem to the modern, native-first Emacs 30/31 stack utilizing `eglot`, `flymake`, `dape`, and `apheleia`.

This architecture leverages C-level Emacs primitives, `project.el`, and `xref` to eliminate dependency bloat, eradicate main-thread blocking, and achieve superior performance on PGTK/Wayland builds.

---

## Eglot (Emacs 30/31 Built-in) + Ecosystem

### Core protocol/architecture

- Emacs built-in client/library implementing the Language Server Protocol (LSP), fully featured for LSP v3.17+ (including semantic tokens and code lens).
- Non-blocking, fully asynchronous JSON-RPC communication utilizing native Emacs asynchronous processes.
- **`eglot-booster` integration**: Routes Eglot's JSON-RPC through the `emacs-lsp-booster` Rust binary, parsing JSON off the main thread and returning native Emacs Lisp bytecode/objects to prevent main-thread blocking on massive payloads .
- Client-server architecture; Emacs acts purely as the LSP client, a separate language server binary does the analysis.
- Supports well over 100 languages/language-servers natively via the `eglot-server-programs` plist, eliminating the need for dedicated `lsp-<language>.el` client files.
- Auto-detects and integrates with native Emacs frameworks (`xref`, `flymake`, `eldoc`, `corfu`, `project.el`) without heavy auto-configuration scripts (replaces `lsp-auto-configure`).
- `eglot-ensure` command to defer server startup/DidOpen notifications until the buffer becomes visible (replaces `lsp-deferred`).
- Multi-root / multi-folder workspace support natively handled by `project.el` grouping buffers under the same project root (add/remove/blocklist handled via `project-ignored-directories`).
- Disconnect a buffer from its language server (`eglot-shutdown`) while keeping the server running.
- Restart or shut down a language server session on demand (`eglot-reconnect`, `eglot-shutdown`).
- Describe/inspect the current LSP session state via `eglot-describe-server` and `eglot-events-buffer`.
- Client/server protocol logging toggle for debugging via `eglot-events-buffer`.
- File-watcher integration handled via Emacs' native `file-notify-add-watch` to notify servers of external file system changes (`workspace/didChangeWatchedFiles`).
- Remote development support (e.g., via TRAMP) is native and first-class; Eglot automatically spawns the language server on the remote host.
- Performance tuning guidance: Relies on `read-process-output-max` (set to 4MB in `early-init.el`) and `eglot-booster` rather than aggressive `gc-cons-threshold` or plist vs hash-table manipulation.
- Docker integration via TRAMP's native Docker methods (`/docker:container:/path`) or by wrapping the server binary in `eglot-server-programs` (replaces `lsp-docker`).

### Completion

- Completion-at-point integration exposing LSP-provided completion candidates via `eglot-completion-at-point`.
- Works seamlessly with `corfu` and `cape` for a modern, lightweight completion popup (replaces `company-mode` / `company-capf`).
- Recommended tuning: `corfu-auto-prefix 1`, `corfu-auto-delay 0.0` (mirrors `company-minimum-prefix-length 1`, `company-idle-delay 0.0`).
- (Legacy `company-lsp` and `company-capf` backends explicitly rejected in favor of the native `completion-at-point-functions` pipeline).
- `cape-wrap-noninterruptible` shields Eglot network requests from `quit` signals, and `cape-wrap-nonexclusive` merges LSP candidates with local Dabbrev/File candidates.

### Code navigation

- `xref-find-definitions` (`M-.`) — jump to symbol definition (replaces `lsp-find-definition`).
- `xref-find-references` (`M-?`) — list/jump to all references of a symbol (replaces `lsp-find-references`).
- `eglot-find-implementation` — find implementations of a symbol.
- `eglot-find-typeDefinition` — find type definitions of a symbol.
- `eglot-find-declaration` — find declarations of a symbol.
- `consult-eglot-symbols` — find all workspace symbols matching a pattern via Vertico/Orderless with async live-preview (replaces `helm-lsp` / `lsp-ivy`).
- Incoming call hierarchy view via `eglot-show-call-hierarchy` (Emacs 30+ `xref` integration, replaces `lsp-treemacs` call hierarchy).
- Builtin `xref` integration (Eglot acts as a native `xref` backend).
- Tree-based navigation via `consult-imenu`, `breadcrumb`, and Tree-sitter AST parsing (replaces `lsp-treemacs` tree views).
- Peek-style navigation via `consult-xref` (routes `xref` through Vertico with live buffer previews, replacing `lsp-ui-peek`).
- `consult-imenu` / builtin `imenu` code-outline browsing (replaces `helm-imenu`).

### Diagnostics/linting

- Real-time on-the-fly diagnostics rendered natively through `flymake` via the `eglot--flymake-backend` (replaces `flycheck`).
- Project-wide/workspace error statistics shown directly in the modeline via native Emacs 30+ Flymake modeline counters (replaces `lsp-modeline-diagnostics-mode`).
- Configurable diagnostics scope: `flymake-show-buffer-diagnostics` (`:file`) vs `flymake-show-project-diagnostics` (`:workspace`/`:global`).
- Aggregated project-wide error list via `flymake-show-project-diagnostics` + `consult-flymake` (replaces `lsp-treemacs-errors-list`).
- Diagnostics settings group covers severity display, filtering, and rendering behavior natively in `flymake`.

### Code actions

- Execute code actions on demand via `eglot-code-actions` (replaces `lsp-execute-code-action`).
- Code actions surfaced directly in the modeline via `doom-modeline` or `eglot-code-action-indications` (Emacs 31) — recommended UI (replaces `lsp-modeline-code-actions-mode`).
- Configurable modeline segments: count, icon, name handled via `doom-modeline` customization.
- Code actions also surfaced via `sideline-eldoc` or `eldoc-box` (replaces `lsp-ui` sideline).
- "Organize imports" code action shortcut via `eglot-code-action-organize-imports` (replaces `source.organizeImports`).
- `fixAll` code action support, auto-applied on save via `apheleia` or `before-save-hook` (replaces `lsp-fix-all-on-save-list`).

### Code lens

- Inline code lens rendering natively supported in Emacs 31 via `eglot-code-lens-mode` (e.g., reference counts, run/test links).
- Toggle code-lens overlays on/off via `eglot-code-lens-mode`.
- Click a lens using standard Emacs mouse clicks or `avy` on overlays.

### Headerline / breadcrumb

- `breadcrumb` package (written by Eglot's author, João Távora) shows a breadcrumb bar at the top of the window (replaces `lsp-headerline-breadcrumb-mode`).
- Configurable breadcrumb segments: `breadcrumb-imenu-crumbs` (symbols), `breadcrumb-project-crumbs` (path-up-to-project, file, project).
- Optional numeric labeling of breadcrumb symbol entries via custom `breadcrumb` formatting functions.

### Symbol highlighting

- Eglot intentionally omits `textDocument/documentHighlight` by default to save JSON-RPC bandwidth.
- Replaced by `symbol-overlay` or Emacs 31's Tree-sitter semantic highlighting to highlight read/write occurrences of the symbol at point using C-level AST queries (zero-network-cost).
- Toggleable symbol highlighting minor mode (`symbol-overlay-mode`).
- Dedicated faces for "read," "write," and generic textual occurrences via `symbol-overlay` faces.

### Hover / documentation

- Built-in `eldoc-mode` integration showing one-line signature info in the minibuffer.
- `eldoc-doc-buffer` (Emacs 29+) for full hover documentation in a dedicated, searchable Markdown buffer (replaces `lsp-describe-thing-at-point`).
- Signature help popup natively handled by `eglot-signature-eldoc-function`, with `M-n`/`M-p` cycling between overloaded signatures via `eldoc` navigation.
- Richer hover/documentation rendering via `eldoc-box` child frames (replaces `lsp-ui-doc`).
- Clickable in-buffer links when the server supports `documentLink`, handled by `goto-address-mode` and `browse-url-button` buttonization in `eldoc-doc-buffer`.

### Formatting

- Whole-buffer formatting requested from the language server via `eglot-format-buffer` or `apheleia`.
- Region (or current-line) formatting via `eglot-format`.
- On-type formatting triggered by specific characters: Eglot intentionally avoids this to prevent UI stutter; replaced by `electric-pair-mode` and `apheleia` (replaces `lsp-enable-on-type-formatting`).
- Format-on-save option, scoped per major mode via `apheleia-global-mode` and `apheleia-mode-alist` (replaces `lsp-format-buffer-on-save-list`).
- Indent-size/tabs-vs-spaces control handled by `dtrt-indent` and `editorconfig` (deeper formatter config is server-specific).

### Renaming / editing

- `eglot-rename` — rename a symbol and all its references atomically across the workspace using `xref`.
- Dedicated highlighting face for the identifier being renamed achieved via `symbol-overlay` or `highlight` integration during the `xref` query.
- `iedit` / `evil-multiedit` integration for interactive multi-cursor-style editing of symbols, respecting `xref` boundaries.

### Semantic tokens

- Implements LSP 3.17 semantic tokens natively in Emacs 30/31 via `eglot-semantic-tokens-mode` for richer, server-driven syntax highlighting (supported by clangd, rust-analyzer, pyright) .
- Dedicated semantic-tokens settings group via `eglot-semantic-tokens` customization.

### Folding, imenu, indentation

- Code folding support handled natively by `treesit-fold` (AST-aware) and `outline-minor-mode` (toggle, configurable line limits, max folding ranges).
- Automatic `imenu` integration via Eglot's `xref-backend-definitions` and Tree-sitter AST parsing (replaces `textDocument/documentSymbol`).
- Region indentation driven by `apheleia` formatters or `eglot-format`.

### Debugger integration

- Deep integration decoupled from the LSP client, handled entirely by `dape` (Debug Adapter Protocol) for a lightweight, native debugging experience (replaces `dap-mode`).

### Editor/ecosystem integrations

- Treemacs integration via `project.el` and `treemacs`.
- Consult integration (`consult-eglot`).
- `which-key` integration for keybinding discovery (per-mode or global).
- `dired` and `project.el` integration.
- `ido` integration explicitly rejected in favor of Vertico/Consult.
- Mouse support for LSP commands via Emacs 30 context menus (`mouse-3`).
- Almost-complete mnemonic keybinding scheme routed via `general.el` and `transient` menus (replaces `s-l` prefix).
- `eglot-describe-server` provides auto-generated documentation for the connected language server (replaces `lsp-clients.json`/`lsp-doc.el`).

### Language coverage

- Client definitions for well over 150 languages/tools configured via simple plist entries in `eglot-server-programs`, each independently configurable without heavy client files.

---

## Eglot UI Ecosystem (`eldoc-box`, `sideline`, `consult-xref`, `breadcrumb`)

### General

- UI companion ecosystem for `eglot` providing visual modules: end-of-line diagnostics, xref previews, doc popovers, and breadcrumbs (replaces `lsp-ui`).
- Automatically activated by hooking into `eglot-managed-mode-hook` — zero extra config required in the common case.
- Installable via MELPA/`elpaca`/`use-package`.

### Sideline / End-of-line Diagnostics (replaces `lsp-ui-sideline`)

- **Emacs 30 Native Error Lens**: `flymake-show-diagnostics-at-end-of-line` renders diagnostic ghost-text directly at the end of the line via C-level display properties .
- **`sideline` ecosystem**: `sideline-flymake` and `sideline-eldoc` show contextual information, code actions, and hover information in the right margin.
- Displays flymake diagnostics inline in the sideline.
- Displays available Eglot code actions inline in the sideline.
- Displays hover information inline in the sideline.
- `sideline-flymake` toggle for diagnostics display.
- `sideline-eldoc` toggle for hover display.
- `sideline-code-actions` toggle for code-action display.
- `sideline-update-mode` — choose whether sideline updates per-line or per-point-movement.
- `sideline-delay` — configurable delay (seconds) before the sideline appears.
- `sideline-diagnostic-max-lines` — control verbosity of multi-line diagnostic messages (helps prevent flicker).

### Peek / Xref Previews (replaces `lsp-ui-peek`)

- **`consult-xref`**: VSCode-style "peek" UI routed through Vertico for inline, non-intrusive browsing of cross-references.
- `consult-xref` — peek at definition(s) without leaving the current buffer context.
- `consult-xref` — peek at all references.
- `consult-xref` — peek at implementations.
- `consult-eglot-symbols` — peek results for a workspace symbol search pattern.
- Custom cross-reference requests handled via Eglot's native `xref` backend extensions.
- Remappable over the standard `xref-find-definitions`/`xref-find-references` bindings (`M-.`/`M-?`).
- Window-local jump list dedicated to cross-references handled natively by `xref-go-back` (`M-,`) and `xref-go-forward`.
- `consult-xref` enable toggle via `xref-show-xrefs-function`.
- `consult-xref` optionally shows each result's containing directory via Marginalia annotations.

### Doc / Hover Popovers (replaces `lsp-ui-doc`)

- **`eldoc-box`**: Shows object/symbol documentation at point in a child frame (posframe-style popover).
- **`eldoc-doc-buffer`**: Opens a dedicated, fully interactive Emacs buffer for deep-reading massive Markdown payloads (replaces WebKit-rendered widget).
- Ability to focus/interact directly inside the `eldoc-doc-buffer` using standard Evil/Emacs motions.
- `eldoc-box-help-at-point` toggle.
- `eldoc-box-position` — display doc at top, bottom, or at-point.
- `eldoc-box-side` — display doc on the left or right.
- `eldoc-idle-delay` — seconds before the doc popover appears.
- `eldoc-box-show-with-cursor` — trigger doc display by moving the cursor over a symbol.
- `eldoc-box-show-with-mouse` — trigger doc display by hovering the mouse (via `eldoc-box-hover-mode`).
- `eldoc-box-include-signature` — include function/method signature in the popover.
- `eldoc-box-use-childframe` — toggle child-frame rendering vs. regular buffer.
- Configurable border/alignment and `winum-ignore` behavior for the doc frame via `posframe` parameters.

### Imenu / Outline (replaces `lsp-ui-imenu`)

- **`consult-imenu`** and **`breadcrumb`**: Dedicated, navigable UI listing all AST symbols for the current buffer.
- `consult-imenu` groups entries by "kind" (function, variable, class, etc.) natively via Tree-sitter/Eglot.
- `breadcrumb` placement in the header-line.
- `consult-imenu` window width handled by Vertico/Embark buffer layouts.
- `consult-imenu` prevents window resizing via `display-buffer-alist`.
- Customizable mode-line format for the imenu buffer via `embark-collect`.
- `consult-imenu` automatically refreshes the list as the buffer changes (via `imenu` auto-rescan).
- `consult-imenu` refresh delay handled by `imenu-auto-rescan` and `idle-change` hooks.

### Flymake integration

- Native `eglot--flymake-backend` auto-loaded when `eglot-ensure` connects, tying LSP diagnostics directly into Flymake's C-level rendering engine (replaces `lsp-ui-flycheck`).

---

## Dape (Debug Adapter Protocol)

### Core capabilities

- Full, lightweight client implementation of Microsoft's DAP for Emacs 30+. Vastly superior to `dap-mode` in memory footprint, utilizing native Emacs APIs and `plz` for async HTTP .
- Works independently of `eglot`, utilizing `project.el` and `dape-configs`.
- Launch and Attach debugging modes.
- Breakpoints (set, toggle, delete, add at line).
- Conditional breakpoints (`dape-breakpoint-condition`).
- Hit-count/hit-condition breakpoints (`dape-breakpoint-hit-condition`).
- Logpoints — breakpoints that log a message instead of stopping (`dape-breakpoint-log`).
- Exception handling/breakpoints.
- Pause and Continue execution control.
- Step In / Step Over (Next) / Step Out execution control.
- Call stack inspection and navigation (`dape-select-stack`).
- Restart the current stack frame (`dape-restart-frame`) .
- Thread inspection and switching between threads (`dape-select-thread`).
- Stop a specific thread (`dape-thread-stop`).
- Multiple simultaneous debug sessions, with session switching (`dape-session-switch`).
- Switch the active stack frame (`dape-select-stack`).
- Evaluate arbitrary expressions (`dape-eval`).
- Evaluate a selected region as an expression (`dape-eval` with active region).
- Evaluate the symbol/thing at point (`dape-eval-thing-at-point`).
- Disconnect from/cancel the current debug session (`dape-disconnect-quit`).
- Debug/run configuration templates, editable before running via `dape-configs` (Elisp plist) or `.dir-locals.el` (replaces `dap-debug-edit-template`).
- `dape` — pick and start a registered debug configuration.
- `dape-debug-last` — re-run the previous debug configuration.
- `dape-debug-recent` — choose from recently used debug configurations.
- Jump to the debug output buffer (`dape-repl`).

### `launch.json` support (The Paradigm Shift)

- **Architectural Divergence**: `dape` explicitly **rejects** VSCode-style `launch.json` files to avoid JSON parsing overhead and lock-in.
- **Parity Solution**: Replaced by `dape-configs` (Elisp plist) or `.dir-locals.el` for per-project configuration. For strict `launch.json` parity, the community package `dape-vscode` or `dap-mode`'s legacy JSON parser can be bridged, but native Elisp configs are mathematically safer and faster.
- Supports variable substitution via Elisp `format` and `expand-file-name` within `dape-configs`.

### Pre-debug build/compile support

- `dape-compile` or custom Elisp middleware hooks run a shell command that must succeed before the debug session starts (replaces `:dap-compilation`).
- `default-directory` controls the working directory for that compilation step (replaces `:dap-compilation-dir`).

### REPL / debug shell

- Interactive DAP debug REPL (`dape-repl`) for executing commands while stopped at a breakpoint.
- Standard Emacs Comint-backed shell ergonomics: command history, `C-p`/`C-n` history navigation.
- `corfu` / `cape` autocompletion inside the REPL (replaces `company-mode` in REPL).

### UI windows (replaces `dap-ui`)

- Sessions view (`dape-info-sessions`) listing active/terminated debug sessions.
- Locals view (`dape-info-scope`) showing local variables in the current frame .
- Expressions/watch view (`dape-info-watch`), with add/remove watch expressions.
- Breakpoints view (`dape-info-breakpoints`) with its own keymap: jump to breakpoint (RET), delete under cursor (d), delete selected (D), mark/unmark/unmark-all (m/u/U) .
- Loaded-sources view (`dape-info-sources`).
- REPL window (`dape-repl`).
- `dape-mouse-mode` — on-screen controls/mouse-hover support to manage the debug session (replaces `dap-ui-controls-mode` and `dap-tooltip-mode`).

### Transient integration (replaces Hydra)

- Replaces `dap-hydra`. A custom `transient-define-prefix` (`ar/toggles-dape`) provides a grid-aligned, Nerd-Icon-accented menu for quick access to common debugger actions.
- Can auto-trigger the transient menu whenever a breakpoint is hit via `dape-stopped-hook`.

### Docker / containerized debugging

- Supported via adapter configurations that wrap the debug server binary in a Docker execution string or utilize TRAMP Docker methods.
- Configuration via `.dir-locals.el` (replaces `.lsp-docker.yml`).
- Network-based/remote debug-server connections supported via TRAMP and `plz` async HTTP.

### Language/debugger coverage

- Tested against and supports Java, Python (debugpy), Ruby, Elixir, and LLDB (C/C++/Objective-C/Swift), Node.js, Go (Delve), PHP, Firefox, Chrome/Edge, GDB/LLDB.
- **Paradigm Shift**: Dape explicitly rejects `dap-install` auto-install helpers (e.g., `dap-ruby-setup`). It relies on system package managers (Nix, Mason, OS packages) to install debug adapters, ensuring environment isolation and security.
- Custom cross-reference/debug provider registration API via `dape-configs` plist extensions.

---

## Flymake (+ `flymake-collection`, `consult-flymake`)

### Core purpose

- Modern, built-in on-the-fly syntax checking extension for GNU Emacs (Emacs 30+), positioned as the native, C-level alternative to `flycheck`.
- Delegates actual checking to Eglot (semantic) or external syntax-checking programs via `flymake-collection` (lexical).
- `flymake-collection` ships with support for 100+ different syntax-checking tools out of the box, providing 1:1 parity with `flycheck`'s checker ecosystem.
- Simple, documented interface for defining custom syntax checkers via `flymake-define-diagnostic`.

### Enabling / activation

- `flymake-mode` — enable automatic syntax checking in the current buffer.
- `global-flymake-mode` — enable it in all buffers where checking is possible.
- Global mode intentionally skips remote (TRAMP) files and encrypted files by default (for speed/security reasons), though it can be force-enabled manually.
- `flymake-global-modes` — include/exclude specific major modes from global activation.

### Automatic checking triggers

- `save` — check immediately after the buffer is saved.
- `new-line` — Flymake relies on `idle-change`; to strictly emulate `new-line`, a `post-self-insert-hook` checking for `?\n` can trigger `flymake-start`.
- `idle-change` — check a short, configurable delay after the last edit (`flymake-idle-change-delay`).
- `idle-buffer-switch` — check a short, configurable delay after switching into the buffer (Emacs 30+).
- `mode-enabled` — check immediately when `flymake-mode` is turned on.
- Fully customizable trigger set via `flymake-start-on-...` variables.
- Manual, on-demand checking via `flymake-start` (`C-c ! c`).

### Syntax checker selection and chaining

- Automatic selection of the best-matching backend(s) for the current major mode from `flymake-diagnostic-functions`.
- Checker chains — multiple backends can run in sequence or parallel for one buffer (e.g., Eglot for semantic errors + `flymake-collection-ruff` for Python styling).
- Manual backend selection for the current buffer, with ability to revert to automatic selection.
- Per-project checker enforcement via directory-local variables.
- Disable specific checkers per buffer/project via `remove-hook` on `flymake-diagnostic-functions`.
- Automatic self-disabling of checkers that fail to run (e.g., tool not installed), visible in the `*Flymake log*` buffer.
- Custom chaining order defined by the sequence of functions added to `flymake-diagnostic-functions` (replaces `:next-checkers` / `flycheck-add-next-checker`).
- In-Emacs documentation for any checker via `flymake-collection` docstrings and `describe-function`.

### Checker configuration

- Command-line flags exposed as ordinary customizable Emacs user options per `flymake-collection` checker.
- Support for reading external configuration files per checker, with a configurable file-lookup strategy (absolute path → nearest ancestor directory → `$HOME`).
- Override the executable path used for any checker — useful for project-local tool installs (e.g., a linter inside `node_modules`).
- Recommended workflow of using Directory Variables to scope checker configuration per project.

### In-buffer error display

- Three built-in severity levels: `error`, `warning`, `note` (info).
- Configurable in-buffer highlighting extent via `flymake` diagnostic faces (none, whole line, column-only, symbol).
- Configurable highlighting style: native C-level underline faces (replaces Flycheck's custom delimiter bracketing and conditional styles).
- Dedicated customizable faces per severity level (`flymake-error`, `flymake-warning`, `flymake-note`).
- Fringe indicators (small arrow bitmaps) in the left or right fringe, colored by severity.
- Margin indicators as an alternative to fringe icons.
- Multi-line error indication handled by Emacs 30's native `flymake-show-diagnostics-at-end-of-line` ghost text (replaces Flycheck's hatch patterns/vertical dots).
- Customizable fringe bitmaps/margin symbols via `flymake` face and bitmap overrides.
- Mode-line indicator showing Flymake's current state and error/warning counts natively, including "all clear" success indicators via `doom-modeline`.
- Error-count threshold protection that discards and disables an overly noisy checker (`flymake-collection` threshold guards).
- `flymake-clear` to wipe all highlighting/indicators from the buffer.

### Error list

- Dedicated, auto-updating error list buffer via `flymake-show-buffer-diagnostics` and `flymake-show-project-diagnostics` (workspace-wide).
- `consult-flymake` provides a Vertico-backed, fuzzy-searchable error list that follows the currently active source buffer.
- Highlights the entry corresponding to the error at point in the source buffer.
- Keybindings inside the list: jump to error (RET), next/previous error (n/p), filter by level, sort, re-check and refresh (g), quit (q).
- Filter the list to hide errors below a chosen severity.
- Sort by line, level, ID, or message/checker column, toggling ascending/descending.
- Customizable window placement/sizing via `display-buffer-alist` (e.g., IDE-style bottom panel).

### Error navigation and interaction

- Integrates with Emacs' standard `next-error`/`previous-error` (`M-g n` / `M-g p`).
- Independent navigation commands: `flymake-goto-next-error` / `flymake-goto-prev-error`.
- Minimum-severity filter for navigation.
- Automatic display of the error(s) at point after a configurable delay via `eldoc-box` or `flymake-popon`.
- Customizable error-display function/backend (echo area, child frame, or sideline).
- GUI mouse-hover tooltips showing error message(s) via `eldoc-box` or `flymake-popon`.
- Errors from other files (e.g., a header included via `gcc`) surface in the project diagnostics buffer and are natively handled by `project.el` and `xref` (replaces Flycheck's line-1 anchoring).
- "Explain error" support via `consult-flymake` and LSP diagnostic payloads.
- Copy error message(s) at point to the kill ring via custom Elisp wrappers around `flymake-diagnostic` (replicates `flycheck-copy-errors-as-kill` with IDs).

### Ecosystem / extensibility

- `flymake-collection` serves as the definitive successor to `flycheck`'s checker ecosystem, providing modern, maintained definitions for hundreds of linters.
- Detailed "Flycheck versus Flymake" parity is achieved via `flymake-collection` and Emacs 30's native C-level rendering optimizations.
- Officially built into GNU Emacs 26+; reaches full IDE parity in Emacs 30/31.
