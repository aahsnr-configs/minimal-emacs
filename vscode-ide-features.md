# VS Code IDE Features — Exhaustive List

Scope: language-intelligence and debugging features only (no AI/agentic
features, no remote development). Current as of July 2026.

VS Code's IDE capabilities are built on two protocols it originated as
an editor client: **LSP** (Language Server Protocol) for code
intelligence, and **DAP** (Debug Adapter Protocol) for debugging. Not
every feature is available for every language — it depends on whether
the specific language server or debug adapter implements that
capability — but this is the full set VS Code itself supports as a
client.

---

## Part 1 — Language Intelligence (LSP-backed)

| # | Feature | LSP method | What it does in VS Code |
|---|---|---|---|
| 1 | IntelliSense / code completion | `textDocument/completion`, `completionItem/resolve` | Context-aware autocomplete popup, ghost text (inline suggestions), auto-import on accept |
| 2 | Hover info | `textDocument/hover` | Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut |
| 3 | Signature help | `textDocument/signatureHelp` | Parameter hints shown while typing inside a function call |
| 4 | Go to Definition | `textDocument/definition` | Jump to (or peek) where a symbol is defined |
| 5 | Go to Declaration | `textDocument/declaration` | Jump to a symbol's declaration (distinct from definition in some languages, e.g. C/C++ headers) |
| 6 | Go to Type Definition | `textDocument/typeDefinition` | Jump to the type definition of a variable/expression |
| 7 | Go to Implementation | `textDocument/implementation` | Jump to concrete implementations of an interface/abstract method |
| 8 | Find All References | `textDocument/references` | Lists every usage of a symbol across the project in the References panel |
| 9 | Document Highlight | `textDocument/documentHighlight` | Highlights all occurrences of the symbol under the cursor within the current file |
| 10 | Document Symbols / Outline view | `textDocument/documentSymbol` | Tree view of classes/functions/variables in the current file; also powers the breadcrumb dropdown |
| 11 | Workspace Symbol search | `workspace/symbol` | Ctrl+T "Go to Symbol in Workspace" — fuzzy search symbols across the whole project |
| 12 | Code Actions (quick fixes & refactorings) | `textDocument/codeAction`, `codeAction/resolve` | Lightbulb menu: quick fixes, extract method/variable, add missing import, etc. |
| 13 | Code Lens | `textDocument/codeLens`, `codeLens/resolve` | Inline actionable annotations above symbols (e.g. "1 reference \| Run Test \| Debug Test") |
| 14 | Document Formatting (whole file) | `textDocument/formatting` | "Format Document" command |
| 15 | Range Formatting | `textDocument/rangeFormatting` | "Format Selection" command |
| 16 | On-type Formatting | `textDocument/onTypeFormatting` | Auto-reformats as you type trigger characters like `}` or `;` |
| 17 | Rename Symbol | `textDocument/rename`, `textDocument/prepareRename` | F2 — project-wide safe rename of a symbol and all its references |
| 18 | Folding Ranges | `textDocument/foldingRange` | Gutter arrows to collapse/expand code blocks, functions, imports, regions |
| 19 | Selection Range (smart expand/shrink) | `textDocument/selectionRange` | Shift+Alt+Right / Shift+Alt+Left grows/shrinks selection by syntactic scope |
| 20 | Linked Editing Range | `textDocument/linkedEditingRange` | Editing one HTML/JSX tag name automatically updates its matching pair |
| 21 | Call Hierarchy | `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls` | Tree view of what calls a function and what it calls |
| 22 | Type Hierarchy | `textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`, `typeHierarchy/subtypes` | Tree view of a type's supertypes and subtypes |
| 23 | Semantic Tokens (semantic highlighting) | `textDocument/semanticTokens/full`, `.../delta`, `.../range` | Type-aware syntax coloring beyond what static TextMate grammars can do |
| 24 | Inlay Hints | `textDocument/inlayHint`, `inlayHint/resolve` | Inline grey annotations showing inferred types, parameter names, etc. |
| 25 | Inline Values | `textDocument/inlineValue` | Shows variable values inline at end-of-line while paused in the debugger |
| 26 | Document Links | `textDocument/documentLink`, `documentLink/resolve` | Ctrl+click-able URLs, file paths, or import references inside source/comments |
| 27 | Document Color | `textDocument/documentColor`, `textDocument/colorPresentation` | Inline color swatches next to CSS-like color values, with a picker |
| 28 | Diagnostics (push model) | `textDocument/publishDiagnostics` | Server pushes errors/warnings; shown as squiggles and in the Problems panel |
| 29 | Diagnostics (pull model) | `textDocument/diagnostic`, `workspace/diagnostic` | Client requests diagnostics on demand (LSP 3.17+), including workspace-wide refresh |
| 30 | Moniker | `textDocument/moniker` | Cross-repository symbol identity, mainly used for large-scale/indexed code navigation (e.g. GitHub-style code search) |
| 31 | Workspace file-operation hooks | `workspace/willRenameFiles`, `didRenameFiles`, `willDeleteFiles`, `didDeleteFiles`, `willCreateFiles`, `didCreateFiles` | Automatically fixes up imports/references when files are renamed, moved, or deleted in the Explorer |
| 32 | Execute Command | `workspace/executeCommand` | Server-defined custom commands surfaced in the Command Palette (e.g. "Organize Imports") |
| 33 | Notebook Document support | Various `notebook/*` methods (LSP 3.17+) | Language-server-aware editing inside Jupyter-style notebook cells |

### Editor-chrome features layered on top of LSP results
These are VS Code UI conventions built around the LSP data above —
some are direct UI treatments of an LSP response, others are pure
editor polish with no protocol request behind them at all:

| Feature | Backed by an LSP request? | What it does |
|---|---|---|
| Peek Definition / Peek References | Yes (`definition`/`references`) | Shows the target inline in an expandable panel without switching editor tabs |
| Breadcrumbs bar | Yes (`documentSymbol`) | Path › file › enclosing symbol navigation bar above the editor |
| Problems panel | Yes (diagnostics) | Centralized, filterable list of all errors/warnings across the project |
| Sticky Scroll | No | Pins the current enclosing function/class/block header at the top of the viewport while scrolling |
| Bracket Pair Colorization | No | Matches bracket pairs by color |
| Minimap | No | Miniature file overview on the right edge of the editor |
| Multi-cursor editing | No | Place and edit from multiple cursors simultaneously |
| Quick Fix lightbulb | Yes (`codeAction`) | Visual indicator that code actions are available at the cursor |

---

## Part 2 — Debugging (DAP-backed)

| # | Feature | VS Code UI |
|---|---|---|
| 1 | Launch/Attach configurations (`launch.json`) | Run and Debug view, configuration dropdown |
| 2 | Line breakpoints | Click in the gutter next to a line number |
| 3 | Conditional breakpoints (expression-based) | Right-click a breakpoint → Edit Breakpoint → expression condition |
| 4 | Hit-count breakpoints | Right-click a breakpoint → Edit Breakpoint → hit count condition |
| 5 | Triggered breakpoints (armed only after another breakpoint fires) | Right-click gutter → Add Triggered Breakpoint |
| 6 | Function breakpoints (break by function name, no source line needed) | "+" button in the Breakpoints panel |
| 7 | Inline breakpoints (column-specific, for multi-statement lines) | Shift+F9, useful for minified/dense code |
| 8 | Data breakpoints (break on variable value change/read/access) | Right-click a variable in the Variables view → Break on Value Change/Read/Access |
| 9 | Logpoints (log a message without pausing execution) | Right-click gutter → Add Logpoint |
| 10 | Exception breakpoints | Checkboxes in the Breakpoints panel |
| 11 | Continue / Pause | Debug toolbar |
| 12 | Step Over | F10 |
| 13 | Step Into | F11 |
| 14 | Step Out | Shift+F11 |
| 15 | Restart Frame | Right-click a frame in the Call Stack view |
| 16 | Stop / Disconnect | Debug toolbar |
| 17 | Call Stack view with multi-frame navigation | CALL STACK panel |
| 18 | Threads, multi-thread stepping | Threads shown under each session in the Call Stack view |
| 19 | Multiple simultaneous debug sessions / multi-target debugging | Sessions stacked in the Call Stack view, switchable |
| 20 | Variables (Locals) view, including expanding nested objects/arrays | VARIABLES panel |
| 21 | Set Value on a variable mid-session | Right-click a variable → Set Value |
| 22 | Copy Value / Copy as Expression | Right-click a variable in the Variables view |
| 23 | Watch expressions | WATCH panel, add via "+" |
| 24 | Debug Console with interactive evaluation and autocomplete | DEBUG CONSOLE panel |
| 25 | Loaded Sources view | LOADED SCRIPTS panel |
| 26 | Disassembly View (step through assembly when source isn't available) | Opens as an editor tab, automatically or via command |
| 27 | Inline variable value decorations while stepping | End-of-line ghost text in the editor while execution is paused |
| 28 | preLaunchTask / compile-before-debug | `preLaunchTask` property in `launch.json`, tied to `tasks.json` |
| 29 | Hot code replace / Edit and Continue (runtime-dependent) | Automatic for supported runtimes without restarting the session |
| 30 | Run without debugging | "Run" button alongside "Debug" in the Run and Debug view |
| 31 | Breakpoints view (centralized list across all files) | BREAKPOINTS panel |
| 32 | Multi-target debugging via compound configurations | `compounds` array in `launch.json`, launches several configs together |

---
*Sources: VS Code Language Server Protocol documentation
(code.visualstudio.com/api/language-extensions), LSP specification
3.16–3.18 (microsoft/language-server-protocol), and VS Code debugging
documentation (code.visualstudio.com/docs/debugtest/debugging), all
fetched July 2026.*
