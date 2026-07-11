Yes, absolutely. Because Eglot’s maintainer (João Távora) has explicitly rejected adding Code Lens to Eglot core to avoid UI clutter and maintain minimalism, achieving a true 1:1 visual and functional replica of `lsp-lens` requires building a dedicated, external `eglot-codelens` extension.

To do this correctly, we must completely bypass the blocking network calls of early community attempts (like Gavinok's `eglot-codelens.el` ) and instead leverage Emacs 30/31's mature asynchronous engine. By reverse-engineering `lsp-lens.el`'s viewport culling and mapping it to `jsonrpc.el`'s deferred requests, we can build a high-performance Code Lens engine that feels native to Eglot.

### The Architectural Paradigm Shift: `lsp-lens.el` vs. Native Eglot Async

Before diving into the implementation plan, it is crucial to understand the network architectures at play:

- **`lsp-lens.el` (The Heavyweight):** Modifies the buffer extensively, hooks into every keystroke, and historically relied on synchronous or poorly debounced network requests that can cause UI stuttering and "stutter-and-vanish" overlays on large files .
- **`eglot` + `jsonrpc.el` (The Modern Engine):** Eglot relies on `jsonrpc.el` for non-blocking, asynchronous communication. `jsonrpc.el` features a powerful `:deferred` mechanism that automatically cancels stale network requests if the user continues typing or the buffer state changes . To achieve `lsp-lens` aesthetics without the performance penalty, our `eglot-codelens` wrapper must hook directly into `jsonrpc-async-request` and utilize Emacs 30's optimized overlay priority systems.

---

### Comprehensive Plan: Replicating `lsp-lens` in `eglot`

#### 1. Capability Negotiation & The "Push" Protocol (Replacing `lsp-mode` auto-config)

- **The `lsp-lens` way:** `lsp-auto-configure` blindly injects all capabilities and relies on heavy polling or workspace-wide refreshes.
- **The `eglot` plan:** We must use `cl-defmethod` to advise `eglot-client-capabilities` and inject `:codeLensProvider` into the `:textDocument` plist, and `workspace/codeLens/refresh` into the `:workspace` plist. We will also implement a handler for `eglot-handle-notification` to listen for the server's push refresh events, ensuring lenses update instantly when the AST changes without manual polling.

#### 2. The Async Network Layer & Viewport Culling (Replacing `lsp-lens` blocking requests)

- **The `lsp-lens` way:** Often requests lenses for the entire buffer or uses heavy timers that block the main thread .
- **The `eglot` plan:** We will implement strict **Viewport Culling**. By calculating `window-start` and `window-end`, we will only request and render lenses for lines currently visible on the screen . Crucially, we will use `jsonrpc-async-request` with the `:deferred` keyword . This tells `jsonrpc.el` to automatically discard the server's response if the `buffer-modified-tick` changes before the payload arrives, completely eliminating the overlay misalignment bug that plagues older implementations.

#### 3. The "Resolve" Pipeline (Handling Server Optimizations)

- **The `lsp-lens` way:** Handles resolution opaquely, sometimes blocking the UI while waiting for heavy servers like `gopls` or `rust-analyzer`.
- **The `eglot` plan:** Many modern LSP servers return "unresolved" lenses (providing only the `range` and `data` payload) to save bandwidth. We will inspect `eglot--server-capabilities` for `:resolveProvider`. If true, we will render a placeholder overlay immediately, then fire a secondary, batched `jsonrpc-async-request` to `codeLens/resolve` to fetch the actual command titles asynchronously, updating the overlay in place once the data arrives.

#### 4. Overlay Rendering & Collision Avoidance (Using Emacs 30/31 Native Overlays)

- **The `lsp-lens` way:** Uses complex `lsp-ui` or `overlay` stacks that often conflict with `flycheck` or `corfu`.
- **The `eglot` plan:** We will use Emacs' native `before-string` overlays anchored to the `line-beginning-position`. To prevent collisions with `flyover` (diagnostics) or `eglot-inlay-hints-mode`, we will assign strict overlay `priority` levels. If multiple lenses target the same line (e.g., "Run Test | 2 References"), we will group them into a single concatenated string with distinct text properties and local keymaps for each substring, ensuring mouse clicks route to the correct command.

#### 5. Command Execution & Server Quirks (Replacing `lsp-execute-code-action`)

- **The `lsp-lens` way:** Routes everything through `lsp-execute-code-action`, which sometimes fails on non-standard server commands.
- **The `eglot` plan:** We will route executions through Eglot's native `eglot-execute-command`. However, because servers like `rust-analyzer` or `jdtls` often emit custom, non-standard commands (e.g., `rust-analyzer.runSingle`), we will provide a `cl-defmethod` hook allowing users to define custom Elisp execution logic for these specific server payloads.

#### 6. Lifecycle & Debouncing

- **The `eglot` plan:** Hook into `after-change-functions` and `window-scroll-functions`, but wrap the trigger in a `run-with-idle-timer` (e.g., 0.3s). This ensures we only fetch lenses when the user pauses typing or finishes scrolling, respecting the minimalist resource footprint that Eglot users expect.

---

### Required Source Code Files

To implement this plan with 100% accuracy, I need to analyze the following source files.

#### 1. The Target & Reference Implementations (Reverse-Engineering)

- **`eglot.el` (Emacs 30/31 Master):** To inspect `eglot-client-capabilities`, `eglot--server-capabilities`, and `eglot-handle-notification` for injecting protocol support and reading server state.
- **`lsp-lens.el` (from `emacs-lsp/lsp-mode`):** **Crucial.** I need to reverse-engineer its viewport culling algorithm (`window-start`/`window-end` tracking) and its overlay collision handling to ensure our `eglot` wrapper is mathematically sound and performant .
- **`eglot-codelens.el`

  (Community Gists/Packages by Gavinok/zsxh):** To analyze early community attempts and explicitly avoid their pitfalls, such as the use of blocking `jsonrpc-request` instead of async callbacks [[11], [14]].

#### 2. The Network & Protocol Frameworks

- **`jsonrpc.el`

  (Built-in Emacs Core):** **Absolutely required.** This is the backbone of Eglot's async communication. I need to analyze its implementation of `jsonrpc-async-request`, specifically the `:deferred` keyword argument which automatically cancels stale requests [[17], [18]], and its internal endpoint queueing system to ensure our Code Lens requests don't starve critical LSP features like autocomplete or diagnostics.

- **LSP Specification 3.17 (Microsoft):** To verify the exact JSON schema for `CodeLensParams`, `CodeLens`, `CodeLensOptions` (specifically `resolveProvider`), and the `workspace/codeLens/refresh` notification.

#### 3. Aesthetics & Utilities

- **`nerd-icons.el` / `all-the-icons.el`:** To inject aesthetic glyphs (e.g., a "play" icon for run lenses, or an "eye" icon for references) directly into the `before-string` overlay text properties.
- **`pulse.el` (Built-in Emacs):** To provide subtle visual feedback (a momentary pulse) when a Code Lens command is successfully executed.
- **Emacs Overlay API (`elisp` core):** To master `overlay-put`, `priority`, `evaporate`, and `keymap` text properties to ensure our lenses never break Evil-mode motions or native Emacs cursor tracking.

#### 4. Eglot CodeLens Source Files

To build the high-performance, non-blocking Code Lens engine, you need the core Eglot networking files, the `lsp-mode` reference implementation, and the community attempts to avoid their pitfalls.

**The Target & Reference Implementations**

- **`eglot.el` (Emacs 30/31 Master):**
  [https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/eglot.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/eglot.el)
- **`lsp-lens.el` (from `emacs-lsp/lsp-mode`):**
  [https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-lens.el](https://github.com/emacs-lsp/lsp-mode/blob/master/lsp-lens.el)
- **`eglot-codelens.el` (Gavinok's Community Gist):**
  [https://gist.github.com/Gavinok/fa23bbea7f44725eb633c002a8aa803a](https://gist.github.com/Gavinok/fa23bbea7f44725eb633c002a8aa803a)
- **`eglot-codelens` (zsxh's Community Repo):**
  [https://github.com/zsxh/eglot-codelens](https://github.com/zsxh/eglot-codelens)

**The Network & Protocol Frameworks**

- **`jsonrpc.el` (Built-in Emacs Core):**
  [https://github.com/emacs-mirror/emacs/blob/master/lisp/jsonrpc.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/jsonrpc.el)
  _(Yes, this is absolutely required. It contains the `jsonrpc-async-request` and `:deferred` logic necessary to prevent UI blocking)._
- **LSP Specification 3.17 (Microsoft):**
  [https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)

**Aesthetics & Utilities**

- **`nerd-icons.el`:**
  [https://github.com/rainstormstudio/nerd-icons.el](https://github.com/rainstormstudio/nerd-icons.el)
- **`pulse.el` (Built-in Emacs):**
  [https://github.com/emacs-mirror/emacs/blob/master/lisp/pulse.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/pulse.el)

---

### 2. Dape UI (Treemacs) Source Files

To build the 1:1 `dap-ui` visual replica, you need the modern `dape` backend, the legacy `dap-mode` files to reverse-engineer the UX, and the UI framework extensions.

**The Target & Reference Implementations**

- **`dape.el` (and related `dape-*.el` files):**
  [https://github.com/svaante/dape](https://github.com/svaante/dape)
- **`dap-mode` Repository (Contains all legacy UI files):**
  [https://github.com/emacs-lsp/dap-mode](https://github.com/emacs-lsp/dap-mode)
  - `dap-ui.el`: [Link](https://github.com/emacs-lsp/dap-mode/blob/master/dap-ui.el)
  - `dap-treemacs.el`: [Link](https://github.com/emacs-lsp/dap-mode/blob/master/dap-treemacs.el)
  - `dap-controls.el`: [Link](https://github.com/emacs-lsp/dap-mode/blob/master/dap-controls.el)
  - `dap-tooltip.el`: [Link](https://github.com/emacs-lsp/dap-mode/blob/master/dap-tooltip.el)
  - `dap-hydra.el`: [Link](https://github.com/emacs-lsp/dap-mode/blob/master/dap-hydra.el)

**The UI Frameworks**

- **`treemacs` (core, extensions, icons):**
  [https://github.com/Alexander-Miller/treemacs](https://github.com/Alexander-Miller/treemacs)
  _(Specifically look inside the `/src/elisp/` directory for `treemacs-extensions.el` and `treemacs-icons.el`)_.
- **`posframe.el`:**
  [https://github.com/tumashu/posframe](https://github.com/tumashu/posframe)
- **`eldoc-box.el`:**
  [https://github.com/casouri/eldoc-box](https://github.com/casouri/eldoc-box)

**Aesthetics & Utilities**

- **`nerd-icons.el`:**
  [https://github.com/rainstormstudio/nerd-icons.el](https://github.com/rainstormstudio/nerd-icons.el)
- **`transient.el` (Magit's Transient library):**
  [https://github.com/magit/transient](https://github.com/magit/transient)
- **`pulse.el` (Built-in Emacs):**
  [https://github.com/emacs-mirror/emacs/blob/master/lisp/pulse.el](https://github.com/emacs-mirror/emacs/blob/master/lisp/pulse.el)

---

### How to use these sources:

If you are using an LLM or a code-parsing tool to write the final Elisp packages, I recommend feeding it the **Raw** versions of these files (just add `raw.githubusercontent.com` to the GitHub blob URLs) so the AI isn't confused by the GitHub web UI HTML.

Let me know if you would like me to extract and summarize the specific functions from any of these links to begin writing the actual Elisp code for either `eglot-codelens.el` or `dape-treemacs.el`!
