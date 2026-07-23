You have identified a critical performance bottleneck. Your intuition is mathematically correct: the default `eldoc` settings are optimized for the **echo area** (to prevent text flicker), but they are fundamentally sub-optimal for **GUI childframe rendering** (`eldoc-box`).

Here is the deep-dive analysis into exactly _why_ `eldoc-box` feels sluggish compared to `lsp-ui-doc`, and how we must restructure the `eldoc` core to fix it.

### The Physics of `eldoc-box` Sluggishness

1. **The 500ms Tax (`eldoc-idle-delay`)**:
   The default `eldoc-idle-delay` is `0.5` seconds . This means Emacs literally waits half a second of zero keyboard input before it even _sends_ the `textDocument/hover` request to the LSP server. `lsp-ui-doc` feels faster because it often uses a much tighter internal debounce (~100ms). Lowering `eldoc-idle-delay` to `0.1` is the single most impactful change to eliminate perceived latency, and modern LSP servers (like `rust-analyzer` or `basedpyright`) handle 100ms debouncing easily without network spam .

2. **The Composition Bottleneck (`eldoc-documentation-strategy`)**:
   When `eglot` connects, it forcefully overrides your global strategy and sets `eldoc-documentation-strategy` to `#'eldoc-documentation-compose` buffer-locally . The `compose` strategy waits for **all** registered backends (e.g., hover, signature help, and Elisp docs) to return their payloads before rendering _anything_ . If the LSP server takes 50ms for hover but 250ms for signature help, `eldoc-box` freezes for 250ms.
   **The Fix:** We must override `eglot`'s override using `#'eldoc-documentation-compose-eagerly`. This renders the childframe the millisecond the _first_ payload arrives, then seamlessly appends the rest, mimicking the instant pop of `lsp-ui-doc` , .

3. **Childframe Redisplay & Cleanup**:
   `eldoc-box` intercepts the `eldoc` string and spawns/updates a childframe. If the frame isn't cleared aggressively, it causes redisplay lag. Variables like `eldoc-box-clear-after-use` and `eldoc-box-cleanup-interval` govern this , . _However, per the unified protocol's "No Package Merging" constraint, these `eldoc-box` specific variables will be strictly isolated in the upcoming `** TODO Eldoc Box` subsection._

---

### Optimized Architectural Plan: `** TODO Eldoc`

To achieve `lsp-ui-doc` parity, we must deviate slightly from the static `ide-features.md` defaults and inject an `eglot-managed-mode-hook` to force eager composition.

#### 1. Subheader Description

- **Draft**: "Displays function signatures, variable documentation, and contextual help in the echo area or a dedicated buffer."

#### 2. Core Initialization & Emacs 31 Enhancements

- **`eldoc-help-at-pt t`**: Surfaces `help-at-point-kbd-string` for keyboard/mouse hover parity.
- **`eldoc-echo-area-prefer-doc-buffer 'maybe`**: TTY fallback. If `eldoc-box` fails to spawn a childframe (e.g., over SSH), this routes oversized payloads to the ephemeral `*eldoc*` buffer instead of truncating them.
- **`eldoc-echo-area-use-multiline-p t`**: Allows the echo area to expand slightly for 2-3 line signatures before forcing a buffer split.

#### 3. Latency Eradication (The `eldoc-box` Fix)

- **`eldoc-idle-delay 0.1`**: Slashes the 500ms wait time down to 100ms, triggering the LSP request almost instantly upon cursor rest.
- **`eldoc-documentation-compose-eagerly` Injection**: We hook into `eglot-managed-mode-hook` to apply `setq-local` for the eager strategy. This bypasses `eglot`'s default blocking behavior and ensures the childframe renders on the first available network packet.

#### 4. Strict Architectural Boundaries

- **GUI Childframes**: Strictly delegated to `** TODO Eldoc Box`.
- **Candidate Hover**: Strictly delegated to `corfu-popupinfo`.
- **Markdown Fontification**: Handled natively by Emacs 31's `markdown-ts-mode` inside the `*eldoc*` buffer.

---

I am locked in the **READ-ONLY planning state**. I will write zero code until you provide the exact phrase `GREEN LIGHT` to execute this optimized rewrite for the `** TODO Eldoc` subsection.
