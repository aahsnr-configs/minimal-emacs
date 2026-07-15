---
---

---

# IntelliSense / Code Completion — Emacs 31 Implementation

> **VS Code feature:** Context-aware autocomplete popup, ghost text (inline suggestions), auto-import on accept.

## Feature Overview

| Attribute              | Value                                                                       |
| ---------------------- | --------------------------------------------------------------------------- |
| **Feature**            | IntelliSense / Code Completion                                              |
| **VS Code equivalent** | Autocomplete popup, inline ghost text, auto-imports, fuzzy filtering        |
| **Status**             | 🟢 `working` · `eglot` + `corfu` + `cape` + `orderless`                     |
| **Category**           | Completion & Intelligence                                                   |
| **LSP methods**        | `textDocument/completion`, `completionItem/resolve`                         |
| **Emacs routing**      | `eglot` → `completion-at-point-functions` → `cape` (merging) → `corfu` (UI) |

## Implementation Stack

| Layer              | Component                    | Role                                                                                                                                                                          |
| ------------------ | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **LSP Client**     | `eglot` (built-in, Emacs 31) | Drives `textDocument/completion`, injects candidates into `completion-at-point-functions`, and handles `completionItem/resolve` for just-in-time docstrings and auto-imports. |
| **UI Engine**      | `corfu`                      | Renders the minimal, high-performance child-frame popup using native Emacs completion APIs.                                                                                   |
| **Ghost Text**     | `corfu-candidate-overlay`    | Draws an inline, dimmed preview of the currently selected candidate directly in the buffer.                                                                                   |
| **Backend Merger** | `cape`                       | Merges LSP candidates with local Dabbrev, File, and Snippet candidates via non-exclusive wrappers without blocking the main thread.                                           |
| **Filtering**      | `orderless`                  | Provides space-separated, out-of-order fuzzy filtering for the candidate list.                                                                                                |
| **Icons**          | `nerd-icons-corfu`           | Injects LSP symbol icons (variables, functions, classes) into the Corfu margin for VS Code visual parity.                                                                     |
| **Documentation**  | `corfu-popupinfo`            | Renders `completionItem/resolve` docstrings in a floating child frame adjacent to the popup.                                                                                  |

## Commands & Keybindings

| Action                  | Command                  | Keybinding      | Notes                                                                           |
| ----------------------- | ------------------------ | --------------- | ------------------------------------------------------------------------------- |
| Trigger completion      | `completion-at-point`    | `TAB` / `C-SPC` | Indents if unaligned, triggers popup if aligned (`tab-always-indent 'complete`) |
| Next candidate          | `corfu-next`             | `TAB` / `C-n`   | Cycles forward through the candidate list                                       |
| Previous candidate      | `corfu-previous`         | `S-TAB` / `C-p` | Cycles backward through the candidate list                                      |
| Insert candidate        | `corfu-insert`           | `RET`           | Commits the candidate, triggering `additionalTextEdits` (auto-imports)          |
| Toggle documentation    | `corfu-popupinfo-toggle` | `M-h`           | Shows/hides the `completionItem/resolve` docstring                              |
| Quick select (insert)   | `corfu-quick-insert`     | `M-q`           | Avy-style 1-2 char jump to insert candidate                                     |
| Quick select (complete) | `corfu-quick-complete`   | `M-Q`           | Avy-style jump to insert and close popup                                        |
| Exit completion         | `corfu-quit`             | `ESC` / `C-g`   | Cancels the popup without inserting                                             |

## Configuration

```emacs-lisp
;; ==========================================
;; 1. EGLOT (LSP Completion Backend)
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Disable eglot's default auto-completion to let corfu handle the UI.
  ;; eglot still injects `eglot-completion-at-point` into the Capf list.
  (eglot-completion-at-point t))

;; ==========================================
;; 2. CORFU (In-Buffer Completion UI)
;; ==========================================
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  ;; PROTOCOL COMPLIANCE: Strictly confine Corfu to buffer editing.
  ;; MUST NEVER be enabled in the minibuffer (conflicts with Vertico).
  (global-corfu-minibuffer nil)
  ;; Auto-popup configuration
  (corfu-auto t)
  (corfu-auto-delay 0.2)     ;; 0.2s delay prevents GC stutter with Orderless
  (corfu-auto-prefix 2)      ;; Trigger after 2 characters
  ;; UI dimensions and behavior
  (corfu-count 16)
  (corfu-max-width 120)
  (corfu-cycle t)            ;; Allow cycling from bottom to top
  ;; TAB-and-Go safety net: Preselect the prompt so accidental RET inserts
  ;; exactly what was typed, rather than blindly committing the first LSP candidate.
  (corfu-preselect 'prompt)
  :bind (:map corfu-map
         ("TAB"       . corfu-next)
         ("<tab>"     . corfu-next)
         ("S-TAB"     . corfu-previous)
         ("<backtab>" . corfu-previous)
         ("RET"       . corfu-insert)
         ("<escape>"  . corfu-quit))
  :config
  ;; Ensure Orderless filters LSP candidates correctly
  (add-to-list 'completion-category-overrides
               `(lsp-capf (styles ,@completion-styles)))
  ;; Always close the popup on leaving insert state or saving
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  (add-hook 'before-save-hook #'corfu-quit))

;; ==========================================
;; 3. CORFU EXTENSIONS (Ghost Text & Docs)
;; ==========================================
(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :config
  (corfu-candidate-overlay-mode +1))

(use-package corfu-popupinfo
  :ensure nil  ;; Bundled with corfu
  :after corfu
  :bind (:map corfu-map
         ("M-h" . corfu-popupinfo-toggle))
  :config
  (corfu-popupinfo-mode 1)
  ;; Documentation is ONLY shown on manual trigger (M-h), never automatically.
  ;; Prevents UI flicker and LSP network spam via `completionItem/resolve`.
  (setq corfu-popupinfo-delay nil))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  ;; Register formatter in :init to prevent missing-icon flicker on first popup.
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
```

## Why This Approach (vs. `company-mode` / `lsp-mode`)

| Consideration           | `corfu` + `eglot` (chosen)                                              | `company-mode` + `lsp-mode` (rejected)                                             |
| ----------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| **Architecture**        | Uses native Emacs `completion-at-point` APIs and child-frames.          | Custom overlay engine and heavy workspace management.                              |
| **Performance**         | Exponentially lighter; zero main-thread blocking during typing.         | Prone to micro-stutters and "stutter-and-vanish" popup bugs on slow networks.      |
| **Protocol Compliance** | Honors the `eglot`-only stack mandate.                                  | Requires the forbidden `lsp-mode` ecosystem.                                       |
| **Filtering**           | Integrates seamlessly with `orderless` for out-of-order fuzzy matching. | Requires `company-flx` or custom matchers; struggles with space-separated queries. |
| **Emacs 31 Synergy**    | Leverages native TTY child-frames and PGTK Wayland fixes.               | Legacy rendering engine lacks modern child-frame optimizations.                    |

## Behavioral Parity Matrix

| VS Code behavior                 | Emacs 31 equivalent                                                |
| -------------------------------- | ------------------------------------------------------------------ |
| Autocomplete popup on typing     | `corfu-auto t` with `corfu-auto-delay 0.2`                         |
| Ghost text (inline preview)      | `corfu-candidate-overlay-mode` renders dimmed text in-buffer       |
| Auto-import on accept            | `eglot` processes `additionalTextEdits` natively on `corfu-insert` |
| Fuzzy / Substring filtering      | `orderless` matching styles (space-separated components)           |
| Snippet integration              | `yasnippet-capf` merged into the pipeline via `cape`               |
| Documentation on hover/selection | `corfu-popupinfo` (`M-h`) triggers `completionItem/resolve`        |
| Icons in autocomplete list       | `nerd-icons-corfu` injects glyphs into the Corfu margin            |
| Dismiss on Escape                | `corfu-quit` bound to `<escape>` in `corfu-map`                    |
| TAB to indent, TAB to complete   | `tab-always-indent 'complete` (native Emacs setting)               |

## Emacs 31 Specific Enhancements

- **TTY Child-Frame Support:** Emacs 31 introduces `tty-tip-mode` and native TTY child-frame capabilities. This allows `corfu` to render perfectly in terminal emulators (like Ghostty or Kitty), bringing IDE-grade autocomplete to the CLI without falling back to the legacy `*Completions*` buffer.
- **PGTK Child-Frame Fixes:** Child-frame positioning on Wayland (PGTK builds) is now pixel-accurate. This eliminates the "drifting popup" bug that plagued GNOME/mutter users in Emacs 29/30, ensuring the Corfu popup anchors exactly to the cursor baseline.
- **Eager Display API:** The new `completion-eager-display` variable ensures that if Corfu is bypassed or disabled, the native `*Completions*` fallback appears immediately and updates predictably as you type.

## Integration with Existing Stack

The completion stack integrates deeply with the broader Emacs 31 IDE surface:

- **`cape` (Completion At Point Extensions):** Merges `eglot-completion-at-point` with local backends (`cape-dabbrev`, `cape-file`, `yasnippet-capf`). Using `cape-wrap-nonexclusive` ensures LSP candidates don't shadow local buffer words.
- **`orderless`:** Provides the filtering engine. Typing `get usr` will match `get_current_user` because `orderless` matches space-separated components anywhere in the candidate string.
- **`vertico`:** While `corfu` handles in-buffer completion, `vertico` handles the minibuffer. The strict architectural boundary (`global-corfu-minibuffer nil`) ensures they never fight for control of the completion UI.
- **`yasnippet`:** Snippets are injected into the Corfu popup via `yasnippet-capf`, allowing you to tab-complete LSP functions and local snippets from the exact same menu.

---

---

---

# Hover Info — Emacs 31 Implementation

> **VS Code feature:** Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut.

## Feature Overview

| Attribute              | Value                                                                                  |
| ---------------------- | -------------------------------------------------------------------------------------- |
| **Feature**            | Hover info                                                                             |
| **VS Code equivalent** | Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut       |
| **Status**             | 🟢 `emacs 31` · native `eldoc` + `eglot` · no `eldoc-box` required                     |
| **Category**           | Completion & Intelligence                                                              |
| **LSP methods**        | `textDocument/hover`                                                                   |
| **Emacs routing**      | `eglot` → `eldoc` (echo area / ephemeral buffer) + `corfu-popupinfo` (candidate-level) |

## Implementation Stack

| Layer                     | Component                               | Role                                                                       |
| ------------------------- | --------------------------------------- | -------------------------------------------------------------------------- |
| **LSP Client**            | `eglot` (built-in, Emacs 31)            | Drives `textDocument/hover`, returns Markdown payloads natively            |
| **Documentation Engine**  | `eldoc` (built-in)                      | Renders hover payloads in the echo area or ephemeral `*eldoc*` buffer      |
| **Markdown Rendering**    | `markdown-ts-mode` (built-in, Emacs 31) | Provides rich markdown fontification inside the `*eldoc*` ephemeral buffer |
| **Candidate-Level Hover** | `corfu-popupinfo` (bundled with corfu)  | Shows `completionItem/resolve` docs adjacent to the completion popup       |
| **Mouse Integration**     | `help-at-point-kbd-string` (built-in)   | Surfaces hover info on cursor hover via `eldoc-help-at-pt`                 |

## Commands & Keybindings

| Action                    | Command                     | Keybinding             | Notes                                                             |
| ------------------------- | --------------------------- | ---------------------- | ----------------------------------------------------------------- |
| Hover at point (keyboard) | `eldoc`                     | `K` (Evil normal)      | Shows type info / docstring for symbol under cursor               |
| Help at point             | `help-at-point`             | `C-h .`                | Native Emacs help surfacing via `eldoc-help-at-pt`                |
| Toggle candidate docs     | `corfu-popupinfo-toggle`    | `M-h` (in `corfu-map`) | Shows/hides doc popup for selected completion candidate           |
| Scroll doc buffer         | `scroll-other-window`       | `C-M-v`                | Scrolls the `*eldoc*` ephemeral buffer when doc exceeds echo area |
| Scroll doc buffer (back)  | `scroll-other-window-down`  | `C-M-S-v`              | Reverse scroll for long documentation                             |
| Force hover refresh       | `eglot-signature-eldoc-bar` | —                      | Re-queries `textDocument/hover` on demand                         |

## Configuration

```emacs-lisp
;; ==========================================
;; 1. EGLOT HOVER INTEGRATION (Built-in)
;; ==========================================
;; eglot automatically registers `eglot-hover-eldoc-function` into
;; `eldoc-documentation-functions` when a buffer is managed.
;; No explicit configuration required — just ensure eglot is connected.

;; ==========================================
;; 2. ELDOC CORE (Emacs 31 Native Rendering)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  ;; Enables hover-style info on cursor movement without third-party packages.
  (eldoc-help-at-point t)
  ;; Emacs 31 NEW: Prefer the ephemeral `*eldoc*` buffer over the echo area
  ;; when documentation exceeds a single line. Replaces `eldoc-box` entirely —
  ;; Emacs 31's ephemeral buffer renders rich markdown via `markdown-ts-mode`.
  (eldoc-echo-area-prefer-ephemeral-buffer t)
  ;; Truncate long echo-area messages to prevent modeline clobbering.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering hover query (prevents LSP network spam).
  (eldoc-idle-delay 0.5)
  :config
  ;; Enable eldoc globally — eglot-managed buffers inherit the documentation function.
  (global-eldoc-mode 1))

;; ==========================================
;; 3. CORFU-POPUPINFO (Candidate-Level Hover)
;; ==========================================
(use-package corfu-popupinfo
  :ensure nil  ;; Bundled with corfu
  :after corfu
  :bind (:map corfu-map
         ;; Manual toggle only — prevents UI flicker and LSP network spam
         ;; via `completionItem/resolve` on every candidate scroll.
         ("M-h" . corfu-popupinfo-toggle))
  :config
  (corfu-popupinfo-mode 1)
  ;; Documentation is ONLY shown on manual trigger (M-h), never automatically.
  ;; Setting the delay to nil prevents the automatic hover trigger.
  (setq corfu-popupinfo-delay nil))

;; ==========================================
;; 4. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(general-define-key
  :states 'normal
  "K" #'eldoc)  ;; Evil-standard hover binding
```

## Why This Approach (vs. `lsp-ui-doc` / `eldoc-box`)

| Consideration            | `eldoc` native (chosen)                                   | `lsp-ui-doc` (rejected)                         | `eldoc-box` (rejected)                      |
| ------------------------ | --------------------------------------------------------- | ----------------------------------------------- | ------------------------------------------- |
| **LSP client coupling**  | Works with _any_ eldoc backend (eglot, native elisp)      | Hard-bound to `lsp-mode` ecosystem              | Requires child-frame overhead               |
| **Protocol compliance**  | Honors the `eglot`-only stack mandate                     | Requires forbidden `lsp-mode` ecosystem         | Third-party child-frame dependency          |
| **Emacs 31 integration** | Native ephemeral buffer with `markdown-ts-mode` rendering | No integration with Emacs 31 eldoc enhancements | Superseded by native ephemeral buffer       |
| **Performance**          | Zero additional packages, echo-area fast path             | Child-frame latency on every hover              | Child-frame rendering overhead              |
| **Maintenance**          | Maintained by GNU Emacs core team                         | Stale — tracks lsp-mode lifecycle               | Community-maintained, redundant in Emacs 31 |

## Behavioral Parity Matrix

| VS Code behavior               | Emacs 31 equivalent                                                    |
| ------------------------------ | ---------------------------------------------------------------------- |
| Hover tooltip on cursor idle   | `eldoc` with `eldoc-idle-delay 0.5` triggers `textDocument/hover`      |
| Mouse hover shows tooltip      | `eldoc-help-at-point t` surfaces info on cursor movement               |
| `K` key shows hover (Vim)      | `K` bound to `eldoc` in Evil normal state                              |
| Rich markdown rendering        | `markdown-ts-mode` fontifies `*eldoc*` ephemeral buffer                |
| Long docs in side panel        | `eldoc-echo-area-prefer-ephemeral-buffer t` routes to `*eldoc*` buffer |
| Scroll long documentation      | `C-M-v` / `C-M-S-v` scroll the `*eldoc*` buffer                        |
| Hover on completion candidate  | `corfu-popupinfo-toggle` (`M-h`) shows `completionItem/resolve` docs   |
| Signature help in tooltip      | `eldoc` natively merges `textDocument/signatureHelp` with hover        |
| Type info + docstring combined | eglot merges both payloads into single eldoc response                  |

## Emacs 31 Specific Enhancements

- **`eldoc-help-at-pt` (NEW)**: Surfaces `help-at-point-kbd-string` through the eldoc pipeline, enabling hover-style information on cursor movement without third-party packages. Bridges the gap between keyboard-driven and mouse-driven hover paradigms.
- **`eldoc-echo-area-prefer-ephemeral-buffer` (NEW)**: When documentation exceeds the echo area, Emacs 31 automatically routes the payload to an ephemeral `*eldoc*` buffer rendered with `markdown-ts-mode`. This provides the same "floating documentation window" experience as `eldoc-box` or `lsp-ui-doc` with zero additional dependencies.
- **`markdown-ts-mode` integration**: Emacs 31's native tree-sitter markdown mode fontifies the ephemeral `*eldoc*` buffer, providing rich code blocks, syntax highlighting, and links inside hover documentation — matching VS Code's markdown rendering fidelity.
- **Ephemeral buffer lifecycle**: The `*eldoc*` buffer is automatically managed by Emacs core — it appears when needed, hides when the cursor moves to a non-documentable position, and never pollutes the buffer list like persistent third-party frames.
- **TTY-safe rendering**: Unlike child-frame-based solutions (`eldoc-box`, `lsp-ui-doc`), the ephemeral buffer approach degrades gracefully to TTY frames, ensuring documentation is accessible over SSH and in terminal emulators.

## Integration with Existing Stack

The hover info surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`**: Automatically injects `eglot-hover-eldoc-function` into `eldoc-documentation-functions` when a buffer is LSP-managed.
- **`eldoc`**: Serves as the unified documentation router — aggregating LSP hover, Elisp docstrings, and `help-at-point` into a single echo-area / ephemeral-buffer pipeline.
- **`corfu-popupinfo`**: Handles completion-candidate-level documentation (via `completionItem/resolve`), keeping candidate hover separate from symbol hover to prevent UI conflicts.
- **`treesit`**: Enables `markdown-ts-mode` fontification inside the `*eldoc*` buffer, providing syntax-highlighted code blocks in hover documentation.
- **`which-key`**: Echo-area priority is preserved — `eldoc` yields to `which-key` popups to prevent documentation from clobbering keybinding hints.

## Known Issues & Workarounds

### Echo Area Clobbering

If `eldoc` messages occasionally overwrite active minibuffer prompts or `which-key` hints:

```emacs-lisp
;; Reduce eldoc priority to prevent echo-area conflicts
(setq eldoc-idle-delay 0.5
      eldoc-message-function #'message)  ;; Use standard message routing
```

### Long Documentation Overflow

For language servers returning extremely long hover payloads (e.g., rust-analyzer type expansions):

```emacs-lisp
;; Force ephemeral buffer for all hover (never use echo area)
(setq eldoc-echo-area-prefer-ephemeral-buffer 'always)
```

### Mouse Hover Sensitivity

If cursor-movement-triggered hover feels too aggressive:

```emacs-lisp
;; Disable help-at-point, rely only on keyboard `K`
(setq eldoc-help-at-point nil)
```

`eldoc-box` was created to solve severe architectural limitations in older Emacs versions (Emacs 26 through 29) regarding how the native `eldoc` engine handled Language Server Protocol (LSP) hover payloads.

Before Emacs 31, `eldoc` was fundamentally designed for simple, single-line Elisp function signatures. When modern LSP servers began returning massive, multi-paragraph Markdown payloads (e.g., Rust type expansions, TypeScript generic signatures, or C++ template documentation), the native Emacs UI failed in three critical ways:

### 1. The Echo Area Bottleneck (Truncation)

Historically, `eldoc` was hardcoded to print documentation to the echo area (the minibuffer at the bottom of the frame). The echo area is strictly limited to 1 or 2 lines. Long LSP docstrings were brutally truncated, forcing users to either open the `*Messages*` buffer to read the full text or rely on third-party packages to intercept the string.

### 2. Markdown Illiteracy (No Fontification)

LSP servers return documentation formatted in Markdown. Older versions of Emacs `eldoc` treated these payloads as plain text. The echo area would display raw Markdown syntax (e.g., `**bold**`, `` `code` ``, `### Headers`) without any syntax highlighting, font-lock, or structural rendering, making complex documentation nearly unreadable.

### 3. Lack of Scrollability

Because the echo area is not a standard interactive buffer, users could not scroll through long documentation blocks or copy text directly from the hover payload.

---

### The `eldoc-box` Solution

To bypass these limitations, `eldoc-box` (and its `lsp-mode` equivalent, `lsp-ui-doc`) intercepted the `eldoc` string and rendered it inside a **GUI child frame** (a floating popup window) anchored directly to the cursor coordinates.

- It piped the payload through `markdown-mode` to render rich text, tables, and syntax-highlighted code blocks.
- It provided a fully scrollable viewport for massive type signatures.
- Because it used child frames, it floated _above_ the text without triggering Emacs' window redisplay engine, preventing the editor layout from shifting or resizing.

### Why it is Rejected in the Emacs 31 Stack

In the context of the Emacs 31 `eglot`-only stack documented in this project, `eldoc-box` is classified as obsolete and redundant due to native core enhancements:

1. **Native Ephemeral Buffers:** Emacs 31 introduces `eldoc-echo-area-prefer-ephemeral-buffer`. When a docstring exceeds the echo area, Emacs natively routes it to a dedicated `*eldoc*` buffer.
2. **Native Tree-Sitter Markdown:** Emacs 31 natively integrates `markdown-ts-mode`, allowing the ephemeral `*eldoc*` buffer to fontify LSP Markdown payloads at C-speed without requiring the heavy `markdown-mode` dependency.
3. **Protocol Compliance:** The architectural mandate of this stack prioritizes native Emacs primitives over third-party child-frame managers to reduce memory overhead and Wayland/PGTK rendering glitches.

**The One Remaining UI Distinction:**
The only technical reason a user might still seek out `eldoc-box` on Emacs 31 is **UI physics**. Emacs 31's native ephemeral buffer relies on standard window management (`display-buffer`), which can cause window splits or layout shifts when displaying long documentation. `eldoc-box` uses child frames, which overlay the screen without altering the window tree. However, for a strictly minimal, native-first configuration, Emacs 31's native ephemeral routing is the mathematically correct choice.

### Analysis: `peek` vs. `eldoc-box` for Hover Documentation

They do **not** serve the same UI function. They rely on fundamentally different Emacs rendering engines, which dictates their use case:

1.  **`eldoc-box` (Childframes):** Spawns a true GUI **childframe** (a separate, floating OS-level window managed by Emacs). It hovers _above_ the text without altering the buffer's layout, shifting lines, or causing redisplay jitter. This perfectly mimics VS Code's floating hover tooltip. It requires a GUI environment (PGTK/Wayland/X11).
2.  **`peek` (Overlays):** As explicitly noted in the `peek` source repository's "Future Plan" section: _"Child frame. (Currently Peek only support overlay.)"_ It renders **inline** within the current buffer using `before-string`/`after-string` overlays or by physically shifting buffer text downward. This mimics VS Code's "Peek Definition" inline panel (Alt+F12), which expands _inside_ the editor viewport.

**Conclusion:** Using `peek` for LSP Hover Info would cause severe visual jitter, text-shifting, and main-thread redisplay overhead on every cursor movement. `eldoc-box` is the mathematically correct package for **Hover Info** (floating tooltips), while `peek` is strictly reserved for **Peek Definition / Peek References** (inline structural panels).

---

# Hover Info — Emacs 31 Implementation (Childframe Parity)

> **VS Code feature:** Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut.

## Feature Overview

| Attribute              | Value                                                                       |
| ---------------------- | --------------------------------------------------------------------------- |
| **Feature**            | Hover info                                                                  |
| **VS Code equivalent** | Floating tooltip with rich markdown, type info, and signatures              |
| **Status**             | 🟢 `working` · `eldoc-box` (GUI childframe) + native `eldoc` (TTY fallback) |
| **Category**           | Completion & Intelligence                                                   |
| **LSP methods**        | `textDocument/hover`                                                        |
| **Emacs routing**      | `eglot` → `eldoc` → `eldoc-box` (childframe) OR `*eldoc*` buffer (TTY)      |

## Implementation Stack

| Layer                      | Component                     | Role                                                                                                                            |
| -------------------------- | ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| **LSP Client**             | `eglot` (built-in, Emacs 31)  | Drives `textDocument/hover`, returning Markdown payloads natively via `eglot-hover-eldoc-function`.                             |
| **Documentation Router**   | `eldoc` (built-in)            | Aggregates hover payloads and routes them to the active display backend.                                                        |
| **GUI Rendering Engine**   | `eldoc-box`                   | Spawns a floating **childframe** anchored to the cursor, rendering rich markdown without shifting buffer text (VS Code parity). |
| **TTY Fallback Engine**    | `eldoc` (Emacs 31 native)     | Routes payloads to an ephemeral `*eldoc*` buffer or echo area when childframes are unavailable (e.g., over SSH/TTY).            |
| **Markdown Fontification** | `markdown-ts-mode` (built-in) | Provides C-level tree-sitter syntax highlighting for code blocks inside the hover tooltip.                                      |

## Commands & Keybindings

| Action                    | Command                        | Keybinding             | Notes                                                            |
| ------------------------- | ------------------------------ | ---------------------- | ---------------------------------------------------------------- |
| Hover at point (keyboard) | `eldoc`                        | `K` (Evil normal)      | Triggers `textDocument/hover` and spawns the childframe.         |
| Help at point             | `help-at-point`                | `C-h .`                | Native Emacs help surfacing via `eldoc-help-at-pt`.              |
| Scroll hover tooltip      | `eldoc-box-scroll-up` / `down` | `C-M-v` / `C-M-S-v`    | Scrolls the childframe when docstrings exceed the viewport.      |
| Toggle candidate docs     | `corfu-popupinfo-toggle`       | `M-h` (in `corfu-map`) | Shows/hides childframe docs for the active completion candidate. |

## Configuration

This configuration enforces a strict boundary: GUI frames utilize `eldoc-box` for floating childframes, while TTY/daemon frames gracefully degrade to Emacs 31's native ephemeral buffers.

```emacs-lisp
;; ==========================================
;; 1. ELDOC-BOX (GUI Childframe Hover)
;; ==========================================
(use-package eldoc-box
  :ensure t
  :after eglot
  :custom
  ;; Clear the childframe immediately when the cursor moves off the symbol.
  (eldoc-box-clear-after-use t)
  ;; Only spawn the childframe for multi-line payloads. Single-line signatures
  ;; remain in the echo area to prevent UI flicker and childframe spam.
  (eldoc-box-only-multi-line t)
  ;; Position the childframe slightly offset from the cursor to prevent occlusion.
  (eldoc-box-offset '(10 10 10))
  :custom-face
  ;; Tokyo Night synergy: Match the childframe background and border to the theme.
  (eldoc-box-border ((t (:background "#292e42"))))
  (eldoc-box-default-face ((t (:background "#1a1b26" :foreground "#c0caf5"))))
  :config
  ;; Enable hover-at-point tracking. The childframe appears automatically
  ;; when the cursor rests on a documentable symbol.
  (eldoc-box-hover-at-point-mode 1))

;; ==========================================
;; 2. ELDOC CORE (Emacs 31 TTY Fallback & Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  (eldoc-help-at-point t)
  ;; Emacs 31 NEW: TTY Fallback. When `eldoc-box` cannot spawn a childframe
  ;; (e.g., over SSH or in a terminal), route long docs to the ephemeral
  ;; `*eldoc*` buffer instead of truncating them in the echo area.
  (eldoc-echo-area-prefer-ephemeral-buffer t)
  ;; Truncate echo-area messages to prevent modeline clobbering.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering hover query (prevents LSP network spam).
  (eldoc-idle-delay 0.5)
  :config
  (global-eldoc-mode 1))

;; ==========================================
;; 3. CORFU-POPUPINFO (Candidate-Level Hover)
;; ==========================================
(use-package corfu-popupinfo
  :ensure nil  ;; Bundled with corfu
  :after corfu
  :bind (:map corfu-map
         ;; Manual toggle only — prevents UI flicker and LSP network spam
         ;; via `completionItem/resolve` on every candidate scroll.
         ("M-h" . corfu-popupinfo-toggle))
  :config
  (corfu-popupinfo-mode 1)
  ;; Documentation is ONLY shown on manual trigger (M-h), never automatically.
  (setq corfu-popupinfo-delay nil))
```

## Why This Approach (vs. `peek` / `lsp-ui-doc`)

| Consideration           | `eldoc-box` (chosen)                               | `peek` (rejected for hover)                            | `lsp-ui-doc` (rejected)                |
| ----------------------- | -------------------------------------------------- | ------------------------------------------------------ | -------------------------------------- |
| **Rendering Engine**    | **Childframe** (Floating GUI window)               | **Overlay** (Inline buffer shift)                      | Childframe (Heavy lsp-mode dependency) |
| **UI Physics**          | Floats _above_ text; zero layout shift.            | Shifts buffer text down; causes redisplay jitter.      | Floats above text.                     |
| **Protocol Compliance** | Works with _any_ eldoc backend (eglot).            | Works with xref/eldoc, but designed for inline panels. | Hard-bound to forbidden `lsp-mode`.    |
| **Use Case Parity**     | **Hover Info** (VS Code Tooltip).                  | **Peek Definition** (VS Code Alt+F12 panel).           | Hover Info.                            |
| **Performance**         | Lightweight, respects `eldoc-box-only-multi-line`. | High redisplay overhead if used for idle hover.        | Heavy child-frame pipeline.            |

## Behavioral Parity Matrix

| VS Code behavior                  | Emacs 31 equivalent                                                                     |
| --------------------------------- | --------------------------------------------------------------------------------------- |
| Floating tooltip on cursor idle   | `eldoc-box-hover-at-point-mode` spawns childframe after `eldoc-idle-delay`.             |
| Rich markdown rendering           | `markdown-ts-mode` fontifies code blocks inside the `eldoc-box` childframe.             |
| Tooltip disappears on cursor move | `eldoc-box-clear-after-use t` destroys the childframe instantly.                        |
| Single-line hints in status bar   | `eldoc-box-only-multi-line t` keeps 1-liners in the echo area.                          |
| Scroll long documentation         | `C-M-v` / `C-M-S-v` scrolls the `eldoc-box` childframe window.                          |
| Hover on completion candidate     | `corfu-popupinfo-toggle` (`M-h`) spawns a childframe for `completionItem/resolve`.      |
| Works over SSH / Terminal         | Emacs 31 `eldoc-echo-area-prefer-ephemeral-buffer` routes to `*eldoc*` buffer natively. |

## Emacs 31 Specific Enhancements

- **PGTK Child-Frame Pixel Accuracy:** Emacs 31 fixes severe child-frame positioning bugs on Wayland (PGTK builds). `eldoc-box` tooltips now anchor perfectly to the cursor baseline without drifting or clipping off-screen under GNOME/mutter.
- **Native TTY Degradation:** If `eldoc-box` detects a TTY frame (where childframes are unsupported), Emacs 31's native `eldoc-echo-area-prefer-ephemeral-buffer` seamlessly intercepts the payload and routes it to a split `*eldoc*` buffer, ensuring hover info is never lost over SSH.
- **`markdown-ts-mode` Integration:** Emacs 31's native tree-sitter markdown mode fontifies the childframe buffer at C-speed, providing syntax-highlighted code blocks inside the hover tooltip without requiring the heavy `markdown-mode` package.

---

---

---

# Signature Help — Emacs 31 Implementation

> **VS Code feature:** Parameter hints shown while typing inside a function call.

## Feature Overview

| Attribute              | Value                                                                               |
| ---------------------- | ----------------------------------------------------------------------------------- |
| **Feature**            | Signature help                                                                      |
| **VS Code equivalent** | Floating parameter hints highlighting the active argument while typing `(` or `,`   |
| **Status**             | 🟢 `native eldoc` · `eglot` + `eldoc` · no third-party UI required                  |
| **Category**           | Completion & Intelligence                                                           |
| **LSP methods**        | `textDocument/signatureHelp`                                                        |
| **Emacs routing**      | `eglot` → `eglot-signature-eldoc-function` → `eldoc` (echo area / ephemeral buffer) |

## Implementation Stack

| Layer                    | Component                       | Role                                                                                                                                           |
| ------------------------ | ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| **LSP Client**           | `eglot` (built-in, Emacs 31)    | Intercepts trigger characters (`(`, `,`), queries `textDocument/signatureHelp`, and parses the active parameter index.                         |
| **Documentation Router** | `eldoc` (built-in)              | Aggregates the signature payload and routes it to the echo area or ephemeral buffer.                                                           |
| **Rendering Engine**     | `eldoc` + `markdown-ts-mode`    | Highlights the active parameter using the `eldoc-highlight-function-argument` face and fontifies code blocks in the `*eldoc*` buffer.          |
| **Trigger Mechanism**    | `eldoc-documentation-functions` | `eglot` injects `eglot-signature-eldoc-function` into this hook, triggering automatically on `post-command-hook` when inside a callable scope. |

## Commands & Keybindings

| Action                   | Command                    | Keybinding    | Notes                                                                            |
| ------------------------ | -------------------------- | ------------- | -------------------------------------------------------------------------------- |
| Manual signature trigger | `eldoc`                    | `C-h .` / `K` | Forces a `textDocument/signatureHelp` query if the automatic trigger was missed. |
| Scroll long signature    | `scroll-other-window`      | `C-M-v`       | Scrolls the `*eldoc*` ephemeral buffer when a signature exceeds the echo area.   |
| Scroll signature (back)  | `scroll-other-window-down` | `C-M-S-v`     | Reverse scroll for massive C++/Rust generic signatures.                          |
| Help at point            | `help-at-point`            | `C-h .`       | Native Emacs help surfacing that integrates with eldoc payloads.                 |

## Configuration

Signature help requires zero additional packages. It relies entirely on `eglot`'s native integration with Emacs' built-in `eldoc` engine. The configuration focuses on optimizing how Emacs 31 handles long signatures that would otherwise clobber the echo area.

```emacs-lisp
;; ==========================================
;; 1. ELDOC CORE (Signature & Hover Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: When a signature (or hover doc) exceeds the echo area,
  ;; automatically route it to the ephemeral `*eldoc*` buffer instead of
  ;; truncating it or expanding the echo area to 10 lines (which causes UI jitter).
  (eldoc-echo-area-prefer-ephemeral-buffer t)
  ;; Allow multi-line signatures in the echo area if they fit within 3 lines.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering signature/hover queries (prevents LSP network spam).
  (eldoc-idle-delay 0.5)
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  (eldoc-help-at-point t)
  :config
  ;; Enable eldoc globally. eglot-managed buffers automatically inject
  ;; `eglot-signature-eldoc-function` into `eldoc-documentation-functions`.
  (global-eldoc-mode 1))

;; ==========================================
;; 2. EGLOT SIGNATURE INTEGRATION (Built-in)
;; ==========================================
;; eglot automatically registers `eglot-signature-eldoc-function` when a buffer
;; is managed. No explicit hook registration is required.
;; eglot natively parses the `activeParameter` index from the LSP payload and
;; applies the `eldoc-highlight-function-argument` face to the correct argument.
```

## Why This Approach (vs. `lsp-ui-sideline` / `lsp-signature`)

| Consideration           | `eldoc` native (chosen)                                 | `lsp-ui-sideline` (rejected)                         | `lsp-signature` (rejected)                  |
| ----------------------- | ------------------------------------------------------- | ---------------------------------------------------- | ------------------------------------------- |
| **LSP client coupling** | Works with _any_ eldoc backend (eglot, native elisp).   | Hard-bound to `lsp-mode` ecosystem.                  | Hard-bound to `lsp-mode` ecosystem.         |
| **Protocol compliance** | Honors the `eglot`-only stack mandate.                  | Requires forbidden `lsp-mode` ecosystem.             | Requires forbidden `lsp-mode` ecosystem.    |
| **UI Physics**          | Echo area (fast) or ephemeral buffer (no layout shift). | Renders in margins/sidelines (causes text shifting). | Child-frame overlays (heavy, Wayland bugs). |
| **Performance**         | Zero additional packages, native C-level echo area.     | High redisplay overhead on every keystroke.          | Child-frame rendering latency.              |
| **Maintenance**         | Maintained by GNU Emacs core team.                      | Stale — tracks lsp-mode lifecycle.                   | Stale — tracks lsp-mode lifecycle.          |

## Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                                                                                                  |
| ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| Auto-trigger on `(` or `,`           | `eglot` registers trigger characters via `textDocument/signatureHelp` capabilities; `eldoc` fires on `post-command-hook`.            |
| Highlights active parameter          | `eglot` applies `eldoc-highlight-function-argument` face to the active parameter index.                                              |
| Cycles through overloads             | `eldoc` natively supports multiple signatures; `C-h .` or arrow keys can cycle if the server returns an array of signatures.         |
| Floating tooltip for long signatures | Emacs 31 `eldoc-echo-area-prefer-ephemeral-buffer t` routes long signatures to the `*eldoc*` buffer without shifting window layouts. |
| Manual trigger shortcut              | `C-h .` (`help-at-point`) or `K` (`eldoc`).                                                                                          |
| Dismiss on cursor move               | `eldoc` automatically clears the echo area or hides the ephemeral buffer when the cursor leaves the callable scope.                  |

## Emacs 31 Specific Enhancements

- **`eldoc-echo-area-prefer-ephemeral-buffer` (NEW)**: In older Emacs versions, long C++ template signatures or Rust generic bounds would either truncate in the echo area or force the echo area to expand to 5+ lines, causing severe UI jitter and modeline clobbering. Emacs 31 introduces this variable to automatically route oversized signatures to a dedicated, scrollable `*eldoc*` buffer, perfectly mirroring VS Code's floating signature widget without requiring third-party child-frame packages.
- **`markdown-ts-mode` Integration**: When signatures are routed to the `*eldoc*` buffer, Emacs 31's native tree-sitter markdown mode fontifies any code blocks or type annotations embedded in the signature documentation, providing rich syntax highlighting at C-speed.
- **`elisp-eldoc-funcall-with-docstring`**: For Emacs Lisp buffers, Emacs 31's native eldoc engine now merges the function signature with its docstring in a single, highly optimized payload, reducing the need for separate hover queries when inspecting Elisp functions.
- **TTY-Safe Degradation**: Unlike child-frame-based signature widgets (`lsp-signature`), `eldoc` degrades gracefully to the echo area or standard window splits over SSH/TTY, ensuring signature help is always accessible in terminal environments.

---

---

---

# Go to Definition — Emacs 31 Implementation

> **VS Code feature:** Jump to (or peek) where a symbol is defined (F12 / Ctrl+Click).

## Feature Overview

| Attribute              | Value                                                                                          |
| ---------------------- | ---------------------------------------------------------------------------------------------- |
| **Feature**            | Go to Definition                                                                               |
| **VS Code equivalent** | F12 to jump, Ctrl+Click to jump, Alt+F12 to peek inline                                        |
| **Status**             | 🟢 `working` · `eglot` + `xref` + `consult-xref` + Emacs 31 `xref-mouse-mode`                  |
| **Category**           | Navigation & Code Jumping                                                                      |
| **LSP methods**        | `textDocument/definition`                                                                      |
| **Emacs routing**      | `eglot` → `xref-find-definitions` → `consult-xref` (preview dropdown) OR `peek` (inline panel) |

## Implementation Stack

| Layer                    | Component                             | Role                                                                                                                                                                     |
| ------------------------ | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| **LSP Client**           | `eglot` (built-in, Emacs 31)          | Drives `textDocument/definition` and injects the location payload into the native `xref` framework.                                                                      |
| **Navigation Framework** | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                             |
| **Preview Engine**       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when multiple definitions exist (e.g., overloaded functions). |
| **Mouse Integration**    | `xref-mouse-mode` (Emacs 31 NEW)      | Binds `C-<mouse-1>` to `xref-find-definitions-at-mouse`, enabling native Ctrl+Click jumps.                                                                               |
| **Inline Peek**          | `peek` (sr.ht/~meow_king/peek)        | Renders the definition inline below the cursor using overlays without switching tabs (Alt+F12 parity).                                                                   |

## Commands & Keybindings

| Action                          | Command                              | Keybinding    | Notes                                                              |
| ------------------------------- | ------------------------------------ | ------------- | ------------------------------------------------------------------ |
| Jump to definition              | `xref-find-definitions`              | `M-.` / `g d` | Jumps to target; opens `consult-xref` dropdown if ambiguous.       |
| Jump to definition (mouse)      | `xref-find-definitions-at-mouse`     | `C-<mouse-1>` | **Emacs 31 NEW** — Ctrl+Click parity via `global-xref-mouse-mode`. |
| Peek definition (inline)        | `peek-xref-definition`               | `SPC c p d`   | Shows target in an inline overlay panel (requires `peek` package). |
| Go back (history)               | `xref-go-back`                       | `M-,` / `g ,` | Returns to the exact cursor position before the jump.              |
| Go forward (history)            | `xref-go-forward`                    | —             | Reverses `xref-go-back`.                                           |
| Find definitions (other window) | `xref-find-definitions-other-window` | `C-x 4 .`     | Opens definition in a horizontal split.                            |
| Find definitions (other frame)  | `xref-find-definitions-other-frame`  | `C-x 5 .`     | Opens definition in a new frame.                                   |

## Configuration

```emacs-lisp
;; ==========================================
;; 1. XREF & CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  ;; Binds `C-<down-mouse-1>` to `xref-find-definitions-at-mouse`.
  :config
  (global-xref-mouse-mode 1)
  ;; Route xref location prompts through Consult for live previews.
  ;; When a symbol has multiple definitions (e.g., overloaded functions,
  ;; interface implementations), Consult renders a Vertico dropdown with
  ;; live buffer previews instead of a static *xref* buffer.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; ==========================================
;; 2. PEEK PACKAGE (Inline Definition Panel)
;; ==========================================
;; Provides VS Code's Alt+F12 "Peek Definition" parity by rendering the
;; target inline below the cursor using overlays, without switching tabs.
(use-package peek
  :ensure (peek :host sourcehut :repo "~meow_king/peek")
  :commands (peek-xref-definition peek-overlay-dwim)
  :custom
  (peek-mode-enable-eldoc t)        ;; show eldoc inside peek panel
  (peek-definition-function #'xref-find-definitions)
  :config
  (global-peek-mode 1))

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(general-define-key
  :states 'motion
  "g d" #'xref-find-definitions
  "g ," #'xref-go-back)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c p" '(:ignore t :wk "peek")
  "c p d" '(peek-xref-definition :wk "Peek definition"))
```

## Why This Approach (vs. `lsp-ui-peek` / `lsp-mode`)

| Consideration           | `xref` + `consult-xref` (chosen)                                          | `lsp-ui-peek` / `lsp-mode` (rejected)                      |
| ----------------------- | ------------------------------------------------------------------------- | ---------------------------------------------------------- |
| **LSP client coupling** | Works with _any_ xref backend (eglot, dumb-jump, etags, tags).            | Hard-bound to the `lsp-mode` ecosystem.                    |
| **Protocol compliance** | Honors the `eglot`-only stack mandate.                                    | Requires the forbidden `lsp-mode` ecosystem.               |
| **Preview engine**      | `consult-xref` leverages `vertico` for fuzzy filtering and live previews. | Custom child-frame pipeline with heavy rendering overhead. |
| **Mouse integration**   | Native Emacs 31 `xref-mouse-mode` (zero dependencies).                    | Requires custom mouse-click advice.                        |
| **History tracking**    | Native `xref` history ring (shared across all backends).                  | Fragmented history management.                             |

## Behavioral Parity Matrix

| VS Code behavior                  | Emacs 31 equivalent                                        |
| --------------------------------- | ---------------------------------------------------------- |
| `F12` jumps to definition         | `M-.` or `g d` (`xref-find-definitions`)                   |
| `Ctrl+Click` jumps to definition  | `C-<mouse-1>` via Emacs 31 `global-xref-mouse-mode`        |
| `Alt+F12` peeks definition inline | `SPC c p d` (`peek-xref-definition`) via `peek` package    |
| Dropdown for multiple definitions | `consult-xref` intercepts `xref-show-definitions-function` |
| Live preview in dropdown          | `consult-xref` + `vertico` live buffer previews            |
| `Alt+Left` returns to origin      | `M-,` or `g ,` (`xref-go-back`)                            |
| Open in split window              | `C-x 4 .` (`xref-find-definitions-other-window`)           |

## Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW)**: Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to `xref-find-definitions-at-mouse`, perfectly mirroring VS Code's Ctrl+Click convention without requiring third-party mouse-click advice.
- **Editable Xref Buffers**: Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns reference/definition lists into writable surfaces. While primarily used for "Find All References", it can be used to bulk-edit multiple definition sites if a language server returns them.
- **`consult-xref` synergy**: The integration of `consult-xref` with Emacs 31's refined `xref` API ensures that ambiguous definitions (e.g., C++ overloaded functions, TypeScript union types) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews.
- **`peek` package integration**: The `peek` package hooks directly into the `xref` framework via `peek-definition-function`, allowing it to intercept `textDocument/definition` payloads from `eglot` and render them as inline overlays. This provides true "Peek Definition" parity without the heavy child-frame overhead of `lsp-ui-peek`.

## Integration with Existing Stack

The Go to Definition surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`**: Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `M-.` to `textDocument/definition`.
- **`consult`**: `consult-xref` intercepts the xref display functions to provide vertico-powered previews.
- **`treesit`**: For non-LSP buffers (or when eglot is disconnected), `treesit` modes can provide fallback definition jumping via `treesit-thing` navigation or `imenu` integration.
- **`evil-collection`**: Standardizes `g d` and `g ,` across all major modes, ensuring Vim muscle memory is preserved.
- **`peek`**: Provides the inline overlay engine for "Peek Definition" workflows, keeping the user's spatial context intact.

---

---

---

# Go to Declaration — Emacs 31 Implementation

> **VS Code feature:** Jump to a symbol's declaration (distinct from definition in some languages, e.g. C/C++ headers).

## Feature Overview

| Attribute              | Value                                                                  |
| ---------------------- | ---------------------------------------------------------------------- |
| **Feature**            | Go to Declaration                                                      |
| **VS Code equivalent** | Jump to header/declaration site (distinct from F12 "Go to Definition") |
| **Status**             | 🟢 `working` · `eglot` + `xref`                                        |
| **Category**           | Navigation & Code Jumping                                              |
| **LSP methods**        | `textDocument/declaration`                                             |
| **Emacs routing**      | `eglot` → `eglot-find-declaration` → `xref` framework                  |

## Implementation Stack

| Layer                    | Component                             | Role                                                                                                                                                                          |
| ------------------------ | ------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **LSP Client**           | `eglot` (built-in, Emacs 31)          | Queries `textDocument/declaration` and injects the location payload into the native `xref` framework.                                                                         |
| **Navigation Framework** | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                                  |
| **Preview Engine**       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when multiple declarations exist (e.g., C++ forward declarations). |
| **Mouse Integration**    | `xref-mouse-mode` (Emacs 31 NEW)      | Enables native Ctrl+Click routing for xref payloads.                                                                                                                          |

## Commands & Keybindings

| Action                    | Command                  | Keybinding      | Notes                                                                      |
| ------------------------- | ------------------------ | --------------- | -------------------------------------------------------------------------- |
| Jump to declaration       | `eglot-find-declaration` | `C-c d` / `g D` | Jumps to the header/declaration site; distinct from `M-.` (definition).    |
| Jump to definition        | `xref-find-definitions`  | `M-.` / `g d`   | Jumps to the actual implementation/definition site.                        |
| Go back (history)         | `xref-go-back`           | `M-,` / `g ,`   | Returns to the exact cursor position before the jump.                      |
| Go forward (history)      | `xref-go-forward`        | —               | Reverses `xref-go-back`.                                                   |
| Peek declaration (inline) | `peek-xref-definition`   | `SPC c p d`     | Requires `peek` package; renders the header inline without switching tabs. |

## Configuration

`eglot` natively registers `eglot-find-declaration` when a buffer is LSP-managed. No explicit hook registration is required for the command itself. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```emacs-lisp
;; ==========================================
;; 1. XREF & CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Route xref location prompts through Consult for live previews.
  ;; When a symbol has multiple declarations (e.g., C++ forward declarations),
  ;; Consult renders a Vertico dropdown with live buffer previews.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  (global-xref-mouse-mode 1))

;; ==========================================
;; 2. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(general-define-key
  :states 'motion
  "g d" #'xref-find-definitions   ;; Go to Definition (F12)
  "g D" #'eglot-find-declaration  ;; Go to Declaration (Header jump)
  "g ," #'xref-go-back)           ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c g" '(:ignore t :wk "goto")
  "c g d" '(xref-find-definitions :wk "Definition")
  "c g D" '(eglot-find-declaration :wk "Declaration")
  "c g i" '(eglot-find-implementation :wk "Implementation")
  "c g t" '(eglot-find-typeDefinition :wk "Type Definition"))
```

## Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration           | `eglot` + `xref` (chosen)                                                                               | `lsp-mode` (rejected)                                |
| ----------------------- | ------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| **LSP client coupling** | Works exclusively with `eglot` and native `xref`.                                                       | Hard-bound to the `lsp-mode` ecosystem.              |
| **Protocol compliance** | Honors the `eglot`-only stack mandate.                                                                  | Requires forbidden `lsp-mode` ecosystem.             |
| **History tracking**    | Native `xref` history ring (shared across all backends, including `dumb-jump` and `etags`).             | Fragmented history management.                       |
| **Emacs 31 synergy**    | Leverages `xref-mouse-mode` and editable xref buffers.                                                  | No integration with Emacs 31 core xref enhancements. |
| **Fallback physics**    | `eglot` gracefully falls back to `textDocument/definition` if the server returns empty for declaration. | Custom fallback logic required.                      |

## Behavioral Parity Matrix

| VS Code behavior                              | Emacs 31 equivalent                                                                               |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| Jump to `.h` header file (C/C++)              | `g D` (`eglot-find-declaration`) routes via `textDocument/declaration`.                           |
| Jump to `.cpp` implementation file            | `g d` (`xref-find-definitions`) routes via `textDocument/definition`.                             |
| Distinct commands for Decl vs Def             | `eglot-find-declaration` vs `xref-find-definitions`.                                              |
| Fallback to definition if declaration missing | `eglot` natively falls back to definition if the LSP server returns an empty declaration payload. |
| Dropdown for multiple declarations            | `consult-xref` intercepts the xref display functions for vertico-powered previews.                |
| `Alt+Left` returns to origin                  | `M-,` (`xref-go-back`) via the native xref history ring.                                          |
| `Ctrl+Click` on symbol                        | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`).                                                |

## Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW)**: Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention without requiring third-party mouse-click advice.
- **Editable Xref Buffers**: Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns declaration lists into writable surfaces. If a symbol has multiple forward declarations across headers, you can press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating header files natively.
- **`consult-xref` synergy**: Ambiguous declarations (e.g., C++ overloaded forward declarations or TypeScript interface merges) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.

---

---

---

# Go to Type Definition — Emacs 31 Implementation

> **VS Code feature:** Jump to the type definition of a variable/expression (Ctrl+Shift+F12 in some bindings, or right-click → Go to Type Definition).

## Feature Overview

| Attribute              | Value                                                                                                                                    |
| ---------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- |
| **Feature**            | Go to Type Definition                                                                                                                    |
| **VS Code equivalent** | Jump to where the _type_ of a symbol is defined (class, interface, struct, type alias) — distinct from the symbol's value/implementation |
| **Status**             | 🟢 `working` · `eglot` + `xref` + `consult-xref`                                                                                         |
| **Category**           | Navigation & Code Jumping                                                                                                                |
| **LSP methods**        | `textDocument/typeDefinition`                                                                                                            |
| **Emacs routing**      | `eglot` → `eglot-find-typeDefinition` → `xref` framework                                                                                 |

## Implementation Stack

| Layer                    | Component                             | Role                                                                                                                                                                                                       |
| ------------------------ | ------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **LSP Client**           | `eglot` (built-in, Emacs 31)          | Queries `textDocument/typeDefinition` and injects the location payload into the native `xref` framework.                                                                                                   |
| **Navigation Framework** | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                                                               |
| **Preview Engine**       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when a symbol resolves to multiple candidate types (e.g., union types, generic instantiations). |
| **Mouse Integration**    | `xref-mouse-mode` (Emacs 31 NEW)      | Enables native Ctrl+Click routing for xref payloads, including type definitions.                                                                                                                           |

## Commands & Keybindings

| Action                         | Command                     | Keybinding      | Notes                                                                    |
| ------------------------------ | --------------------------- | --------------- | ------------------------------------------------------------------------ |
| Jump to type definition        | `eglot-find-typeDefinition` | `C-c t` / `g t` | Jumps to the type (class/interface/struct) of the symbol at point.       |
| Jump to definition (contrast)  | `xref-find-definitions`     | `M-.` / `g d`   | Jumps to the _value/implementation_ of the symbol.                       |
| Jump to declaration (contrast) | `eglot-find-declaration`    | `C-c d` / `g D` | Jumps to the _header/forward declaration_ of the symbol.                 |
| Go back (history)              | `xref-go-back`              | `M-,` / `g ,`   | Returns to the exact cursor position before the jump.                    |
| Peek type definition (inline)  | `peek-xref-definition`      | `SPC c p d`     | Requires `peek` package; renders the type inline without switching tabs. |

## Configuration

`eglot` natively registers `eglot-find-typeDefinition` when a buffer is LSP-managed. No explicit hook registration is required for the command itself. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```emacs-lisp
;; ==========================================
;; 1. XREF & CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Route xref location prompts through Consult for live previews.
  ;; When a symbol's type resolves to multiple candidates (e.g., TypeScript
  ;; union types, C++ template instantiations), Consult renders a Vertico
  ;; dropdown with live buffer previews.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  (global-xref-mouse-mode 1))

;; ==========================================
;; 2. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(general-define-key
  :states 'motion
  "g d" #'xref-find-definitions        ;; Go to Definition (value/impl)
  "g D" #'eglot-find-declaration       ;; Go to Declaration (header)
  "g t" #'eglot-find-typeDefinition    ;; Go to Type Definition (class/struct)
  "g i" #'eglot-find-implementation    ;; Go to Implementation (concrete)
  "g ," #'xref-go-back)               ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c g" '(:ignore t :wk "goto")
  "c g d" '(xref-find-definitions :wk "Definition")
  "c g D" '(eglot-find-declaration :wk "Declaration")
  "c g t" '(eglot-find-typeDefinition :wk "Type Definition")
  "c g i" '(eglot-find-implementation :wk "Implementation"))
```

## Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration           | `eglot` + `xref` (chosen)                                                                                   | `lsp-mode` (rejected)                                |
| ----------------------- | ----------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| **LSP client coupling** | Works exclusively with `eglot` and native `xref`.                                                           | Hard-bound to the `lsp-mode` ecosystem.              |
| **Protocol compliance** | Honors the `eglot`-only stack mandate.                                                                      | Requires forbidden `lsp-mode` ecosystem.             |
| **History tracking**    | Native `xref` history ring (shared across all backends, including `dumb-jump` and `etags`).                 | Fragmented history management.                       |
| **Emacs 31 synergy**    | Leverages `xref-mouse-mode` and editable xref buffers.                                                      | No integration with Emacs 31 core xref enhancements. |
| **Graceful fallback**   | `eglot` gracefully falls back to `textDocument/definition` if the server returns empty for type definition. | Custom fallback logic required.                      |

## Semantic Distinction: Type vs. Definition vs. Declaration

Understanding when to use each command is critical for efficient navigation in strongly-typed languages:

| Command                           | Jumps To                              | Example in TypeScript                                   | Example in Rust                       |
| --------------------------------- | ------------------------------------- | ------------------------------------------------------- | ------------------------------------- |
| **Go to Definition** (`g d`)      | The value/implementation site         | `const user = new User()` → the `new User()` expression | `let x: MyStruct` → the `let` binding |
| **Go to Declaration** (`g D`)     | The header/forward declaration        | Interface declaration in `.d.ts`                        | `mod` declaration in `lib.rs`         |
| **Go to Type Definition** (`g t`) | The type/class/struct definition      | `class User { ... }`                                    | `struct MyStruct { ... }`             |
| **Go to Implementation** (`g i`)  | Concrete implementations of interface | Classes implementing an interface                       | `impl Trait for Type` blocks          |

**When to use `g t` specifically:**

- Inspecting the shape of a variable whose type is non-obvious (e.g., deeply nested generics)
- Navigating from a function parameter to its struct/class definition
- Exploring type hierarchies in TypeScript/Go/Rust codebases
- Understanding return types of factory functions or complex expressions

## Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                                             |
| -------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| Right-click → Go to Type Definition    | `g t` (`eglot-find-typeDefinition`) routes via `textDocument/typeDefinition`.                                   |
| Jump to class/interface of variable    | `g t` on a variable jumps to its type's class/struct definition.                                                |
| Jump to type of function return        | `g t` on a function call jumps to the return type's definition.                                                 |
| Dropdown for ambiguous types           | `consult-xref` intercepts the xref display functions for vertico-powered previews.                              |
| `Alt+Left` returns to origin           | `M-,` (`xref-go-back`) via the native xref history ring.                                                        |
| `Ctrl+Click` on type reference         | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`).                                                              |
| Fallback to definition if type missing | `eglot` natively falls back to definition if the LSP server returns an empty type definition payload.           |
| Works across language boundaries       | eglot routes through xref, which is backend-agnostic — works identically for TypeScript, Rust, Go, C++, Python. |

## Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW)**: Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention for type navigation without requiring third-party mouse-click advice.
- **Editable Xref Buffers**: Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns type-definition lists into writable surfaces. When a symbol's type has multiple candidate definitions (e.g., TypeScript discriminated unions, C++ template specializations), you can press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating files natively.
- **`consult-xref` synergy**: Ambiguous type definitions (e.g., TypeScript union types like `string | number | User`, Go interface implementations, Rust trait objects) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.
- **Unified xref history**: `xref-go-back` (`M-,`) treats type-definition jumps identically to definition and reference jumps — the entire navigation chain is preserved in a single ring, allowing seamless backtracking through complex type exploration sessions.

## Language-Specific Behavior

The utility of `eglot-find-typeDefinition` varies significantly by language server implementation:

| Language       | Server                                 | Behavior                                                                                                            |
| -------------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| **TypeScript** | `typescript-language-server` / `ts_ls` | Excellent — jumps to `interface`, `type`, `class` definitions. Critical for navigating complex generic types.       |
| **Rust**       | `rust-analyzer`                        | Excellent — jumps to `struct`, `enum`, `trait` definitions. Essential for understanding ownership and trait bounds. |
| **Go**         | `gopls`                                | Excellent — jumps to `type` declarations, `struct` definitions, and `interface` specifications.                     |
| **C++**        | `clangd`                               | Excellent — jumps to class/struct definitions, distinct from forward declarations in headers.                       |
| **Python**     | `pyright` / `pylsp`                    | Good — jumps to class definitions; less useful for dynamically-typed code where types are inferred.                 |
| **Java**       | `jdtls`                                | Excellent — jumps to class/interface definitions across modules.                                                    |
| **Emacs Lisp** | N/A                                    | Not applicable — Elisp has no static type system; `xref-find-definitions` is the canonical navigation.              |

## Integration with Existing Stack

The Go to Type Definition surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`**: Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `g t` to `textDocument/typeDefinition`.
- **`consult`**: `consult-xref` intercepts the xref display functions to provide vertico-powered previews for ambiguous type targets.
- **`treesit`**: For non-LSP buffers (or when eglot is disconnected), `treesit` modes can provide fallback type-navigation via `evil-textobj-tree-sitter` text objects (e.g., `vC` to select a class node) combined with `imenu`.
- **`evil-collection`**: Standardizes `g t` across all major modes, ensuring Vim muscle memory is preserved.
- **`peek`**: Provides the inline overlay engine for "Peek Type Definition" workflows, keeping the user's spatial context intact while inspecting type shapes.
- **`eglot-header-line-mode`**: After jumping to a type definition, the breadcrumb bar immediately reflects the new enclosing class/struct context, maintaining spatial orientation.
