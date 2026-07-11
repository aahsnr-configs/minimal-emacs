# lsp-mode (v10.0.0, latest release Apr 3, 2026)

**Core protocol/architecture**

- Emacs client/library implementing the Language Server Protocol (LSP), fully featured for LSP v3.14+ (with 3.17 extensions like semantic tokens)
- Non-blocking, fully asynchronous JSON-RPC communication with language servers
- Client-server architecture; Emacs acts purely as the LSP client, a separate language server binary does the analysis
- Supports well over 100 languages/language-servers via dedicated `lsp-<language>.el` client files
- Auto-detects and auto-configures companion packages (`lsp-ui`, `company-mode`, etc.) unless `lsp-auto-configure` is set to nil
- `lsp-deferred` command to defer server startup/DidOpen notifications until the buffer becomes visible
- Multi-root / multi-folder workspace support (add/remove/blocklist project roots)
- Disconnect a buffer from its language server while keeping the server running
- Restart or shut down a language server session on demand
- Describe/inspect the current LSP session state
- Client/server protocol logging toggle for debugging
- File-watcher integration so servers are notified of external file system changes (configurable threshold)
- Remote development support (e.g., via TRAMP)
- Performance tuning guidance (increasing `gc-cons-threshold`, `read-process-output-max`, plist vs hash-table JSON parsing, etc.)
- Docker integration via the companion `lsp-docker` project for containerized language servers

**Completion**

- Completion-at-point integration exposing LSP-provided completion candidates
- Works with `company-mode` (`company-capf`) for a traditional completion popup
- Recommended tuning: `company-minimum-prefix-length 1`, `company-idle-delay 0.0`
- (Legacy `company-lsp` backend explicitly no longer supported/maintained)

**Code navigation**

- `lsp-find-definition` — jump to symbol definition
- `lsp-find-references` — list/jump to all references of a symbol
- Find implementations of a symbol
- Find type definitions of a symbol
- Find declarations of a symbol
- Find all workspace symbols matching a pattern
- Incoming call hierarchy view (via `lsp-treemacs`)
- Builtin `xref` integration (`M-.` / `xref-find-definitions`, `xref-find-references`)
- Tree-based navigation via `lsp-treemacs`
- Peek-style navigation via `lsp-ui` (definitions, references, implementations, workspace symbols)
- `helm-imenu`/builtin `imenu` code-outline browsing

**Diagnostics/linting**

- Real-time on-the-fly diagnostics rendered through `flycheck` (recommended) or `flymake` (Emacs >26, flymake ≥1.0.5)
- Project-wide/workspace error statistics shown directly in the modeline (`lsp-modeline-diagnostics-mode`)
- Configurable diagnostics scope: `:global`, `:workspace`, or `:file`
- Aggregated project-wide error list via `lsp-treemacs-errors-list`
- Diagnostics settings group covers severity display, filtering, and rendering behavior

**Code actions**

- Execute code actions on demand via `lsp-execute-code-action`
- Code actions surfaced directly in the modeline (`lsp-modeline-code-actions-mode`) — recommended UI
- Configurable modeline segments: count, icon, name in any combination
- Code actions also surfaced via `lsp-ui` sideline
- "Organize imports" code action shortcut (`source.organizeImports`)
- `fixAll` code action support, optionally auto-applied on save (`lsp-fix-all-on-save-list` scoping)

**Code lens**

- Inline code lens rendering when the server supports it (e.g., reference counts, run/test links)
- Toggle code-lens overlays on/off
- Click a lens using the `avy` package for quick activation

**Headerline / breadcrumb**

- `lsp-headerline-breadcrumb-mode` shows a breadcrumb bar at the top of the window
- Configurable breadcrumb segments: path-up-to-project, file, project, symbols (in any combination)
- Optional numeric labeling of breadcrumb symbol entries

**Symbol highlighting**

- Automatic highlighting of all occurrences of the symbol at point (read vs. write occurrences use distinct faces)
- Toggleable symbol highlighting minor mode
- Dedicated faces for "read," "write," and generic textual occurrences

**Hover / documentation**

- Built-in `eldoc-mode` integration showing one-line signature info in the minibuffer
- `lsp-describe-thing-at-point` for full hover documentation
- Signature help popup (triggered manually or by trigger characters like `(`), with `M-n`/`M-p` cycling between overloaded signatures
- Richer hover/documentation rendering via `lsp-ui-doc` child frames
- Clickable in-buffer links when the server supports `documentLink`

**Formatting**

- Whole-buffer formatting requested from the language server
- Region (or current-line) formatting
- On-type formatting triggered by specific characters (e.g., `}`, RET) — `lsp-enable-on-type-formatting`
- Format-on-save option, scoped per major mode via `lsp-format-buffer-on-save-list`
- Indent-size/tabs-vs-spaces control at the `lsp-mode` level (deeper formatter config is server-specific)

**Renaming / editing**

- `lsp-rename` — rename a symbol and all its references, with a dedicated highlighting face for the identifier being renamed
- `iedit` integration for interactive multi-cursor-style editing of symbols

**Semantic tokens**

- Implements LSP 3.17 semantic tokens for richer, server-driven syntax highlighting (supported by servers such as clangd and rust-analyzer)
- Dedicated semantic-tokens settings group

**Folding, imenu, indentation**

- Code folding support (toggle, configurable line limits, max folding ranges)
- Automatic `imenu` integration when the server provides `textDocument/documentSymbol`
- Region indentation driven by the server's formatting capability

**Debugger integration**

- Deep integration with `dap-mode` (Debug Adapter Protocol) for a full debugging experience from within `lsp-mode` workflows

**Editor/ecosystem integrations**

- Treemacs integration (`lsp-treemacs`) — tree views, error lists, call hierarchies
- Helm integration (`helm-lsp`) — workspace symbol search
- Ivy integration (`lsp-ivy`) — workspace symbol search
- Consult integration (`consult-lsp`)
- `which-key` integration for keybinding discovery (per-mode or global)
- `dired` integration
- `ido` integration
- Mouse support for LSP commands
- Almost-complete mnemonic keybinding scheme under a configurable prefix (default `s-l`), covering server lifecycle, navigation, hover, rename, code actions, peeking, and toggles
- `lsp-clients.json`/`lsp-doc.el`-driven auto-generated documentation for each language client

**Language coverage**

- Client definitions for well over 150 languages/tools (C/C++ via clangd/ccls, Python via pyright/pylsp/ruff/jedi, Rust via rust-analyzer, Go via gopls, Java via Eclipse JDT, TypeScript/JavaScript, Haskell, Scala/Metals, Elixir, Clojure, Dart/Flutter, Swift, Terraform, YAML, JSON, Docker, GraphQL, and dozens more), each independently configurable

# lsp-ui

**General**

- UI companion package for `lsp-mode` providing "higher level" visual modules: sideline, peek, doc popovers, imenu, flycheck integration, and code lenses
- Automatically activated by `lsp-mode` out of the box unless `lsp-auto-configure` is nil — zero extra config required in the common case
- Installable via MELPA/`use-package`/`package-install`

**lsp-ui-sideline**

- Shows contextual information for the symbol(s) on the current line in the right margin/sideline
- Displays flycheck diagnostics inline in the sideline
- Displays available LSP code actions inline in the sideline
- Displays hover information inline in the sideline
- `lsp-ui-sideline-show-diagnostics` toggle for diagnostics display
- `lsp-ui-sideline-show-hover` toggle for hover display
- `lsp-ui-sideline-show-code-actions` toggle for code-action display
- `lsp-ui-sideline-update-mode` — choose whether sideline updates per-line or per-point-movement
- `lsp-ui-sideline-delay` — configurable delay (seconds) before the sideline appears
- `lsp-ui-sideline-diagnostic-max-lines` — control verbosity of multi-line diagnostic messages (helps prevent flicker)

**lsp-ui-peek**

- VSCode-style "peek" UI for inline, non-intrusive browsing of cross-references
- `lsp-ui-peek-find-definitions` — peek at definition(s) without leaving the current buffer
- `lsp-ui-peek-find-references` — peek at all references
- `lsp-ui-peek-find-implementation` — peek at implementations
- `lsp-ui-peek-find-workspace-symbol` — peek results for a workspace symbol search pattern
- `lsp-ui-peek-find-custom` — support for server-specific custom cross-reference requests (e.g., ccls's `$cquery/base`)
- Remappable over the standard `xref-find-definitions`/`xref-find-references` bindings (`M-.`/`M-?`)
- Window-local jump list dedicated to cross-references (`lsp-ui-peek-jump-backward` / `lsp-ui-peek-jump-forward`)
- `lsp-ui-peek-enable` toggle
- `lsp-ui-peek-show-directory` — optionally show each result's containing directory

**lsp-ui-doc**

- Shows object/symbol documentation at point in a child frame (posframe-style popover)
- Optional WebKit-rendered documentation widget for richer formatting
- Ability to focus/interact directly inside the doc child frame
- `lsp-ui-doc-enable` toggle
- `lsp-ui-doc-position` — display doc at top, bottom, or at-point
- `lsp-ui-doc-side` — display doc on the left or right
- `lsp-ui-doc-delay` — seconds before the doc popover appears
- `lsp-ui-doc-show-with-cursor` — trigger doc display by moving the cursor over a symbol
- `lsp-ui-doc-show-with-mouse` — trigger doc display by hovering the mouse over a symbol
- `lsp-ui-doc-include-signature` — include function/method signature in the popover
- `lsp-ui-doc-use-childframe` — toggle child-frame rendering vs. regular buffer
- Configurable border/alignment and winum-ignore behavior for the doc frame

**lsp-ui-imenu**

- Dedicated, navigable window listing all `imenu` entries (symbols) for the current buffer
- `lsp-ui-imenu-kind-position` — where to display each entry's "kind" (function, variable, class, etc.)
- `lsp-ui-imenu-buffer-position` — placement of the imenu window
- `lsp-ui-imenu-window-width` — set a fixed window width
- `lsp-ui-imenu-window-fix-width` — prevent the window from being resized by `balance-windows`
- Customizable mode-line format for the imenu buffer
- `lsp-ui-imenu-auto-refresh` — automatically refresh the list as the buffer changes
- `lsp-ui-imenu-refresh-delay` — delay before auto-refreshing

**Flycheck integration**

- Dedicated `lsp-ui-flycheck` module auto-loaded when `flycheck` is present, tying LSP diagnostics into flycheck's UI (list position, live reporting, etc.)

# dap-mode (Emacs client/library for the Debug Adapter Protocol)

**Core capabilities**

- Full client implementation of Microsoft's Debug Adapter Protocol (DAP) — the debugging counterpart to LSP
- Works on top of `lsp-mode`'s `lsp.el` interface
- Launch and Attach debugging modes
- Breakpoints (set, toggle, delete, add at line)
- Conditional breakpoints (`dap-breakpoint-condition`)
- Hit-count/hit-condition breakpoints (`dap-breakpoint-hit-condition`)
- Logpoints — breakpoints that log a message instead of stopping (`dap-breakpoint-log-message`)
- Exception handling/breakpoints
- Pause and Continue execution control
- Step In / Step Over (Next) / Step Out execution control
- Call stack inspection and navigation
- Restart the current stack frame
- Thread inspection and switching between threads (`dap-switch-thread`)
- Stop a specific thread (`dap-stop-thread`)
- Multiple simultaneous debug sessions, with session switching (`dap-switch-session`)
- Switch the active stack frame (`dap-switch-stack-frame`)
- Evaluate arbitrary expressions (`dap-eval`)
- Evaluate a selected region as an expression (`dap-eval-region`)
- Evaluate the symbol/thing at point (`dap-eval-thing-at-point`)
- Disconnect from/cancel the current debug session
- Debug/run configuration templates, editable before running (`dap-debug-edit-template`)
- `dap-debug` — pick and start a registered debug configuration
- `dap-debug-last` — re-run the previous debug configuration
- `dap-debug-recent` — choose from recently used debug configurations
- Jump to the debug output buffer (`dap-go-to-output-buffer`)

**launch.json support**

- Native support for VSCode-style `launch.json` files, placed at the project root or `.vscode` directory
- Automatically populates the configuration picker from `launch.json` entries
- Supports `launch.json` variable substitution (VSCode-compatible variables reference)
- No extra setup required beyond dropping the file in place and running `dap-debug`

**Pre-debug build/compile support**

- `:dap-compilation` property (or `"dap-compilation"` in `launch.json`) runs a shell command that must succeed before the debug session starts
- `:dap-compilation-dir` controls the working directory for that compilation step

**REPL / debug shell**

- Interactive DAP debug REPL for executing commands while stopped at a breakpoint
- Standard Emacs shell ergonomics: command history, `C-p`/`C-n` history navigation
- Optional `company-mode` autocompletion inside the REPL

**UI windows (dap-ui)**

- Sessions view (`dap-ui-sessions`) listing active/terminated debug sessions
- Locals view (`dap-ui-locals`) showing local variables in the current frame
- Expressions/watch view (`dap-ui-expressions`), with add/remove watch expressions (`dap-ui-expressions-add` / `dap-ui-expressions-remove`)
- Breakpoints view (`dap-ui-breakpoints`) with its own keymap: jump to breakpoint (RET), delete under cursor (d), delete selected (D), mark/unmark/unmark-all (m/u/U)
- Loaded-sources view (`dap-ui-loaded-sources`)
- REPL window (`dap-ui-repl`)
- `dap-ui-controls-mode` — on-screen controls to manage the debug session
- `dap-tooltip-mode` — mouse-hover support while debugging

**Hydra integration**

- Built-in `dap-hydra` command console for quick access to common debugger actions
- Can auto-trigger the hydra whenever a breakpoint is hit (`dap-stopped-hook`)

**Docker / containerized debugging**

- `dap-docker-register` support for debugging inside Docker containers/images
- Configuration via `.dir-locals.el` or a dedicated `.lsp-docker.yml` file
- Shares configuration mechanics with the companion `lsp-docker` project
- (Network-based/remote debug-server connections are explicitly not yet supported by this Docker mechanism)

**Language/debugger coverage**

- Tested against and supports Java (via LSP Java auto-discovery), Python (debugpy), Ruby, Elixir, and LLDB (C/C++/Objective-C/Swift)
- Additional community-documented integrations for Node.js, Go (Delve), PHP (xdebug via vscode-php-debug), Firefox, Chrome/Edge, GDB/LLDB via `dap-gdb-lldb`, and more
- Automatic extension/debug-server installation helpers for several languages (e.g., `dap-ruby-setup`, `dap-go-setup`, `dap-chrome-setup`, `dap-firefox-setup`, `dap-gdb-lldb-setup`)
- Custom cross-reference/debug provider registration API for adding new debug servers (`dap-register-debug-provider`)

# Flycheck (v36.0, current stable)

**Core purpose**

- Modern on-the-fly syntax checking extension for GNU Emacs, positioned as the successor/alternative to the built-in Flymake
- Delegates actual checking to external syntax-checking/linting programs or services (does not check code itself)
- Ships with support for 60+ programming languages and 100+ different syntax-checking tools out of the box
- Simple, documented interface for defining custom syntax checkers

**Enabling / activation**

- `flycheck-mode` — enable automatic syntax checking in the current buffer
- `global-flycheck-mode` — enable it in all buffers where checking is possible
- Global mode intentionally skips remote (TRAMP) files and encrypted files by default (for speed/security reasons), though it can be force-enabled manually
- `flycheck-global-modes` — include/exclude specific major modes from global activation

**Automatic checking triggers**

- `save` — check immediately after the buffer is saved
- `new-line` — check immediately after a newline is inserted
- `idle-change` — check a short, configurable delay after the last edit (`flycheck-idle-change-delay`)
- `idle-buffer-switch` — check a short, configurable delay after switching into the buffer (`flycheck-idle-buffer-switch-delay`), with control over intermediate-buffer checking behavior
- `mode-enabled` — check immediately when `flycheck-mode` is turned on
- Fully customizable trigger set via `flycheck-check-syntax-automatically`
- Manual, on-demand checking via `flycheck-buffer` (`C-c ! c`)

**Syntax checker selection and chaining**

- Automatic selection of the best-matching checker(s) for the current major mode from `flycheck-checkers`
- Checker chains — multiple checkers can run in sequence for one buffer (e.g., `emacs-lisp` → `emacs-lisp-checkdoc`; `python-mypy` → `python-flake8`)
- Manual checker selection for the current buffer (`flycheck-select-checker`, `C-c ! s`), with ability to revert to automatic selection
- Per-project checker enforcement via directory-local variables
- Disable specific checkers per buffer/project (`flycheck-disable-checker`, `C-c ! x`) and re-enable them
- Automatic self-disabling of checkers that fail to run (e.g., tool not installed), visible in the verification buffer
- Custom `:next-checkers` property (and `flycheck-add-next-checker` API) to define chain order, optionally gated by minimum error level
- In-Emacs documentation for any checker via `flycheck-describe-checker` (`C-c ! ?`)

**Checker configuration**

- Command-line flags exposed as ordinary customizable Emacs user options per checker
- Support for reading external configuration files per checker, with a configurable file-lookup strategy (absolute path → nearest ancestor directory → `$HOME`)
- Override the executable path used for any checker (`flycheck-set-checker-executable`) — useful for project-local tool installs (e.g., a linter inside `node_modules`)
- Recommended workflow of using Directory Variables to scope checker configuration per project

**In-buffer error display**

- Three built-in severity levels: `error`, `warning`, `info`
- Configurable in-buffer highlighting extent (`flycheck-highlighting-mode`): none, whole line, column-only, symbol (default), or full s-expression
- Configurable highlighting style (`flycheck-highlighting-style`): none, colored "level-face" underline, custom delimiter bracketing, or conditional style based on error span length
- Dedicated customizable faces per severity level for both underline and delimiter styles
- Fringe indicators (small arrow bitmaps) in the left or right fringe, colored by severity
- Margin indicators (using scalable `»` glyphs) as an alternative to fringe icons, with helper functions to auto-configure fringe/margin widths
- Multi-line error indication via hatch patterns (fringe) or vertical dots (margin)
- Customizable fringe bitmaps/margin symbols via `flycheck-redefine-standard-error-levels`
- Mode-line indicator showing Flycheck's current state and error/warning counts, fully customizable, including a custom "all clear" success indicator
- Error-count threshold protection (`flycheck-checker-error-threshold`) that discards and disables an overly noisy checker
- `flycheck-clear` (`C-c ! C`) to wipe all highlighting/indicators from the buffer, with a variant that also interrupts an in-progress check

**Error list**

- Dedicated, auto-updating error list buffer (`flycheck-list-errors` / `list-flycheck-errors`, `C-c ! l`) that follows the currently active "source buffer"
- Highlights the entry corresponding to the error at point in the source buffer
- Keybindings inside the list: jump to error (RET), next/previous error (n/p), explain error (e), filter by level (f), clear filter (F), sort by column (S), re-check and refresh (g), quit (q)
- Filter the list to hide errors below a chosen severity
- Sort by line, level, ID, or message/checker column, toggling ascending/descending
- Customizable window placement/sizing via `display-buffer-alist` (e.g., IDE-style bottom panel)

**Error navigation and interaction**

- Integrates with Emacs' standard `next-error`/`previous-error` (`M-g n` / `M-g p`), toggleable via `flycheck-standard-error-navigation`
- Independent navigation commands regardless of compilation-buffer precedence: `flycheck-next-error` / `flycheck-previous-error` (`C-c ! n` / `C-c ! p`), both accepting numeric prefix arguments, plus `flycheck-first-error`
- Minimum-severity filter for navigation (`flycheck-navigation-minimum-level`)
- Automatic display of the error(s) at point after a configurable delay (`flycheck-display-errors-delay`), toggleable (`flycheck-auto-display-errors-after-checking`)
- Customizable error-display function/backend (default shows in the echo area or a popped-up buffer; swappable, e.g., to only show when the error list is hidden)
- GUI mouse-hover tooltips showing error message(s)/IDs at a location, with a customizable tooltip-content function
- Errors from other files (e.g., a header included via `gcc`) surface in the error list and are anchored to the first line of the current buffer, with a configurable minimum severity for inclusion and an option to disable them entirely
- "Explain error" support (`flycheck-explain-error-at-point`, `C-c ! e`) for checkers that provide rich explanations (e.g., ESLint, stylelint, Rust/Cargo/Clippy, Pylint, Ruff, ShellCheck, markdownlint, Perl::Critic, rpmlint)
- Copy error message(s) at point to the kill ring (`flycheck-copy-errors-as-kill`, `C-c ! C-w`), with variants to copy including IDs or IDs-only

**Ecosystem / extensibility**

- Documented Developer's Guide for writing and registering new syntax checkers, including complex multi-step checkers
- Large recommended-extensions ecosystem for alternative error UIs (e.g., popup tooltips), per-language checker add-ons, and mode-line enhancements
- Detailed "Flycheck versus Flymake" comparison covering relation to core Emacs, automatic checking, checker definition/customization, executable handling, multiple-checkers-per-buffer support, error identifiers/explanations, indicator style, error parsing, message display, and current support status under both Eglot and `lsp-mode`
- Officially requires GNU Emacs 27.1+; works best on Unix-like systems (Windows is unofficially supported)
