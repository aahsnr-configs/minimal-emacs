# VS Code Feature Parity in Emacs 31

This document maps common VS Code IDE features to their Emacs 31 implementations, built primarily on the built-in `eglot` LSP client together with a small set of community packages (`corfu`, `cape`, `orderless`, `consult`, `vertico`, `evil`, and friends). Each entry covers the feature overview, the packages involved, keybindings, full configuration, and troubleshooting notes.

## Table of Contents

- **Completion & Intelligence**
  - [IntelliSense / Code Completion](#intellisense-code-completion)
  - [Hover Info (Eldoc / Minibuffer)](#hover-info-eldoc-minibuffer)
  - [Hover Info (Childframe Variant)](#hover-info-childframe-variant)
  - [Signature Help](#signature-help)
  - [Semantic Tokens (Semantic Highlighting)](#semantic-tokens-semantic-highlighting)
  - [Inlay Hints](#inlay-hints)
- **Navigation & Code Jumping**
  - [Go to Definition](#go-to-definition)
  - [Go to Declaration](#go-to-declaration)
  - [Go to Type Definition](#go-to-type-definition)
  - [Go to Implementation](#go-to-implementation)
  - [Find All References](#find-all-references)
  - [Peek Definition / Peek References](#peek-definition-peek-references)
  - [Call Hierarchy](#call-hierarchy)
  - [Type Hierarchy](#type-hierarchy)
  - [Moniker](#moniker)
- **Symbols & Diagnostics**
  - [Document Symbols / Outline View](#document-symbols-outline-view)
  - [Workspace Symbol Search](#workspace-symbol-search)
  - [Diagnostics (Push Model)](#diagnostics-push-model)
  - [Diagnostics (Pull Model)](#diagnostics-pull-model)
  - [Problems Panel](#problems-panel)
  - [Inline Values](#inline-values)
- **Code Actions & Refactoring**
  - [Code Actions (Quick Fixes & Refactorings)](#code-actions-quick-fixes-refactorings)
  - [Quick Fix Lightbulb](#quick-fix-lightbulb)
  - [Rename Symbol](#rename-symbol)
  - [Execute Command](#execute-command)
- **Formatting & Editing**
  - [Document Formatting (Whole File)](#document-formatting-whole-file)
  - [Range Formatting (Format Selection)](#range-formatting-format-selection)
  - [On-type Formatting](#on-type-formatting)
  - [Folding Ranges](#folding-ranges)
  - [Selection Range (Smart Expand/Shrink)](#selection-range-smart-expandshrink)
  - [Linked Editing Range](#linked-editing-range)
  - [Multi-Cursor Editing](#multi-cursor-editing)
- **Visual Enhancements & UI**
  - [Document Highlight](#document-highlight)
  - [Document Links](#document-links)
  - [Document Color](#document-color)
  - [Bracket Pair Colorization](#bracket-pair-colorization)
  - [Minimap](#minimap)
  - [Breadcrumbs Bar](#breadcrumbs-bar)
  - [Sticky Scroll](#sticky-scroll)
- **Workspace & File Management**
  - [Workspace File-Operation Hooks](#workspace-file-operation-hooks)


---

# Completion & Intelligence

## IntelliSense / Code Completion

_VS Code feature: Context-aware autocomplete popup, ghost text (inline suggestions), auto-import on accept._


### Feature Overview

| Attribute          | Value                                                                       |
| ------------------ | --------------------------------------------------------------------------- |
| Feature            | IntelliSense / Code Completion                                              |
| VS Code equivalent | Autocomplete popup, inline ghost text, auto-imports, fuzzy filtering        |
| Status             | 🟢 working · eglot + corfu + cape + orderless                               |
| Category           | Completion & Intelligence                                                   |
| LSP methods        | `textDocument/completion`, `completionItem/resolve`                         |
| Emacs routing      | `eglot` → `completion-at-point-functions` → `cape` (merging) → `corfu` (UI) |

### Implementation Stack

| Layer          | Component                    | Role                                                                                                                                                                          |
| -------------- | ---------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client     | `eglot` (built-in, Emacs 31) | Drives `textDocument/completion`, injects candidates into `completion-at-point-functions`, and handles `completionItem/resolve` for just-in-time docstrings and auto-imports. |
| UI Engine      | `corfu`                      | Renders the minimal, high-performance child-frame popup using native Emacs completion APIs.                                                                                   |
| Ghost Text     | `corfu-candidate-overlay`    | Draws an inline, dimmed preview of the currently selected candidate directly in the buffer.                                                                                   |
| Backend Merger | `cape`                       | Merges LSP candidates with local Dabbrev, File, and Snippet candidates via non-exclusive wrappers without blocking the main thread.                                           |
| Filtering      | `orderless`                  | Provides space-separated, out-of-order fuzzy filtering for the candidate list.                                                                                                |
| Icons          | `nerd-icons-corfu`           | Injects LSP symbol icons (variables, functions, classes) into the Corfu margin for VS Code visual parity.                                                                     |
| Documentation  | `corfu-popupinfo`            | Renders `completionItem/resolve` docstrings in a floating child frame adjacent to the popup.                                                                                  |

### Commands & Keybindings

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

### Configuration

```elisp
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

### Why This Approach (vs. `company-mode` / `lsp-mode`)

| Consideration       | `corfu` + `eglot` (chosen)                                              | `company-mode` + `lsp-mode` (rejected)                                             |
| ------------------- | ----------------------------------------------------------------------- | ---------------------------------------------------------------------------------- |
| Architecture        | Uses native Emacs `completion-at-point` APIs and child-frames.          | Custom overlay engine and heavy workspace management.                              |
| Performance         | Exponentially lighter; zero main-thread blocking during typing.         | Prone to micro-stutters and "stutter-and-vanish" popup bugs on slow networks.      |
| Protocol Compliance | Honors the `eglot`-only stack mandate.                                  | Requires the forbidden `lsp-mode` ecosystem.                                       |
| Filtering           | Integrates seamlessly with `orderless` for out-of-order fuzzy matching. | Requires `company-flx` or custom matchers; struggles with space-separated queries. |
| Emacs 31 Synergy    | Leverages native TTY child-frames and PGTK Wayland fixes.               | Legacy rendering engine lacks modern child-frame optimizations.                    |

### Behavioral Parity Matrix

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

### Emacs 31 Specific Enhancements

- **TTY Child-Frame Support:** Emacs 31 introduces `tty-tip-mode` and native TTY child-frame capabilities. This allows `corfu` to render perfectly in terminal emulators (like Ghostty or Kitty), bringing IDE-grade autocomplete to the CLI without falling back to the legacy `*Completions*` buffer.
- **PGTK Child-Frame Fixes:** Child-frame positioning on Wayland (PGTK builds) is now pixel-accurate. This eliminates the "drifting popup" bug that plagued GNOME/mutter users in Emacs 29/30, ensuring the Corfu popup anchors exactly to the cursor baseline.
- **Eager Display API:** The new `completion-eager-display` variable ensures that if Corfu is bypassed or disabled, the native `*Completions*` fallback appears immediately and updates predictably as you type.

### Integration with Existing Stack

The completion stack integrates deeply with the broader Emacs 31 IDE surface:

- **`cape` (Completion At Point Extensions):** Merges `eglot-completion-at-point` with local backends (`cape-dabbrev`, `cape-file`, `yasnippet-capf`). Using `cape-wrap-nonexclusive` ensures LSP candidates don't shadow local buffer words.
- **`orderless`:** Provides the filtering engine. Typing `get usr` will match `get_current_user` because `orderless` matches space-separated components anywhere in the candidate string.
- **`vertico`:** While `corfu` handles in-buffer completion, `vertico` handles the minibuffer. The strict architectural boundary (`global-corfu-minibuffer nil`) ensures they never fight for control of the completion UI.
- **`yasnippet`:** Snippets are injected into the Corfu popup via `yasnippet-capf`, allowing you to tab-complete LSP functions and local snippets from the exact same menu.

---

## Hover Info (Eldoc / Minibuffer)

_VS Code feature: Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut._

### Feature Overview

| Attribute          | Value                                                                                  |
| ------------------ | -------------------------------------------------------------------------------------- |
| Feature            | Hover info                                                                             |
| VS Code equivalent | Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut       |
| Status             | 🟢 emacs 31 · native `eldoc` + `eglot` · no `eldoc-box` required                       |
| Category           | Completion & Intelligence                                                              |
| LSP methods        | `textDocument/hover`                                                                   |
| Emacs routing      | `eglot` → `eldoc` (echo area / ephemeral buffer) + `corfu-popupinfo` (candidate-level) |

### Implementation Stack

| Layer                 | Component                               | Role                                                                       |
| --------------------- | --------------------------------------- | -------------------------------------------------------------------------- |
| LSP Client            | `eglot` (built-in, Emacs 31)            | Drives `textDocument/hover`, returns Markdown payloads natively            |
| Documentation Engine  | `eldoc` (built-in)                      | Renders hover payloads in the echo area or ephemeral `*eldoc*` buffer      |
| Markdown Rendering    | `markdown-ts-mode` (built-in, Emacs 31) | Provides rich markdown fontification inside the `*eldoc*` ephemeral buffer |
| Candidate-Level Hover | `corfu-popupinfo` (bundled with corfu)  | Shows `completionItem/resolve` docs adjacent to the completion popup       |
| Mouse Integration     | `help-at-point-kbd-string` (built-in)   | Surfaces hover info on cursor hover via `eldoc-help-at-pt`                 |

### Commands & Keybindings

| Action                    | Command                     | Keybinding             | Notes                                                             |
| ------------------------- | --------------------------- | ---------------------- | ----------------------------------------------------------------- |
| Hover at point (keyboard) | `eldoc`                     | `K` (Evil normal)      | Shows type info / docstring for symbol under cursor               |
| Help at point             | `help-at-point`             | `C-h .`                | Native Emacs help surfacing via `eldoc-help-at-pt`                |
| Toggle candidate docs     | `corfu-popupinfo-toggle`    | `M-h` (in `corfu-map`) | Shows/hides doc popup for selected completion candidate           |
| Scroll doc buffer         | `scroll-other-window`       | `C-M-v`                | Scrolls the `*eldoc*` ephemeral buffer when doc exceeds echo area |
| Scroll doc buffer (back)  | `scroll-other-window-down`  | `C-M-S-v`              | Reverse scroll for long documentation                             |
| Force hover refresh       | `eglot-signature-eldoc-bar` | —                      | Re-queries `textDocument/hover` on demand                         |

### Configuration

```elisp
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
  (eldoc-help-at-pt t)
  ;; Emacs 31 NEW: Prefer the ephemeral `*eldoc*` buffer over the echo area
  ;; when documentation exceeds a single line. Replaces `eldoc-box` entirely —
  ;; Emacs 31's ephemeral buffer renders rich markdown via `markdown-ts-mode`.
  (eldoc-echo-area-prefer-doc-buffer t)
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

### Why This Approach (vs. `lsp-ui-doc` / `eldoc-box`)

| Consideration        | `eldoc` native (chosen)                                   | `lsp-ui-doc` (rejected)                         | `eldoc-box` (rejected)                      |
| -------------------- | --------------------------------------------------------- | ----------------------------------------------- | ------------------------------------------- |
| LSP client coupling  | Works with _any_ eldoc backend (eglot, native elisp)      | Hard-bound to `lsp-mode` ecosystem              | Requires child-frame overhead               |
| Protocol compliance  | Honors the `eglot`-only stack mandate                     | Requires forbidden `lsp-mode` ecosystem         | Third-party child-frame dependency          |
| Emacs 31 integration | Native ephemeral buffer with `markdown-ts-mode` rendering | No integration with Emacs 31 eldoc enhancements | Superseded by native ephemeral buffer       |
| Performance          | Zero additional packages, echo-area fast path             | Child-frame latency on every hover              | Child-frame rendering overhead              |
| Maintenance          | Maintained by GNU Emacs core team                         | Stale — tracks lsp-mode lifecycle               | Community-maintained, redundant in Emacs 31 |

### Behavioral Parity Matrix

| VS Code behavior               | Emacs 31 equivalent                                                  |
| ------------------------------ | -------------------------------------------------------------------- |
| Hover tooltip on cursor idle   | `eldoc` with `eldoc-idle-delay 0.5` triggers `textDocument/hover`    |
| Mouse hover shows tooltip      | `eldoc-help-at-pt t` surfaces info on cursor movement                |
| `K` key shows hover (Vim)      | `K` bound to `eldoc` in Evil normal state                            |
| Rich markdown rendering        | `markdown-ts-mode` fontifies `*eldoc*` ephemeral buffer              |
| Long docs in side panel        | `eldoc-echo-area-prefer-doc-buffer t` routes to `*eldoc*` buffer     |
| Scroll long documentation      | `C-M-v` / `C-M-S-v` scroll the `*eldoc*` buffer                      |
| Hover on completion candidate  | `corfu-popupinfo-toggle` (`M-h`) shows `completionItem/resolve` docs |
| Signature help in tooltip      | `eldoc` natively merges `textDocument/signatureHelp` with hover      |
| Type info + docstring combined | `eglot` merges both payloads into single eldoc response              |

### Emacs 31 Specific Enhancements

- **`eldoc-help-at-pt` (NEW):** Surfaces `help-at-point-kbd-string` through the eldoc pipeline, enabling hover-style information on cursor movement without third-party packages. Bridges the gap between keyboard-driven and mouse-driven hover paradigms.
- **`eldoc-echo-area-prefer-doc-buffer` (NEW):** When documentation exceeds the echo area, Emacs 31 automatically routes the payload to an ephemeral `*eldoc*` buffer rendered with `markdown-ts-mode`. This provides the same "floating documentation window" experience as `eldoc-box` or `lsp-ui-doc` with zero additional dependencies.
- **`markdown-ts-mode` integration:** Emacs 31's native tree-sitter markdown mode fontifies the ephemeral `*eldoc*` buffer, providing rich code blocks, syntax highlighting, and links inside hover documentation — matching VS Code's markdown rendering fidelity.
- **Ephemeral buffer lifecycle:** The `*eldoc*` buffer is automatically managed by Emacs core — it appears when needed, hides when the cursor moves to a non-documentable position, and never pollutes the buffer list like persistent third-party frames.
- **TTY-safe rendering:** Unlike child-frame-based solutions (`eldoc-box`, `lsp-ui-doc`), the ephemeral buffer approach degrades gracefully to TTY frames, ensuring documentation is accessible over SSH and in terminal emulators.

### Integration with Existing Stack

The hover info surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Automatically injects `eglot-hover-eldoc-function` into `eldoc-documentation-functions` when a buffer is LSP-managed.
- **`eldoc`:** Serves as the unified documentation router — aggregating LSP hover, Elisp docstrings, and `help-at-point` into a single echo-area / ephemeral-buffer pipeline.
- **`corfu-popupinfo`:** Handles completion-candidate-level documentation (via `completionItem/resolve`), keeping candidate hover separate from symbol hover to prevent UI conflicts.
- **`treesit`:** Enables `markdown-ts-mode` fontification inside the `*eldoc*` buffer, providing syntax-highlighted code blocks in hover documentation.
- **`which-key`:** Echo-area priority is preserved — `eldoc` yields to `which-key` popups to prevent documentation from clobbering keybinding hints.

### Known Issues & Workarounds

#### Echo Area Clobbering

If `eldoc` messages occasionally overwrite active minibuffer prompts or `which-key` hints:

```elisp
;; Reduce eldoc priority to prevent echo-area conflicts
(setq eldoc-idle-delay 0.5
      eldoc-message-function #'message)  ;; Use standard message routing
```

#### Long Documentation Overflow

For language servers returning extremely long hover payloads (e.g., rust-analyzer type expansions):

```elisp
;; Force ephemeral buffer for all hover (never use echo area)
(setq eldoc-echo-area-prefer-doc-buffer 'always)
```

#### Mouse Hover Sensitivity

If cursor-movement-triggered hover feels too aggressive:

```elisp
;; Disable help-at-point, rely only on keyboard `K`
(setq eldoc-help-at-pt nil)
```

`eldoc-box` was created to solve severe architectural limitations in older Emacs versions (Emacs 26 through 29) regarding how the native `eldoc` engine handled Language Server Protocol (LSP) hover payloads.

Before Emacs 31, `eldoc` was fundamentally designed for simple, single-line Elisp function signatures. When modern LSP servers began returning massive, multi-paragraph Markdown payloads (e.g., Rust type expansions, TypeScript generic signatures, or C++ template documentation), the native Emacs UI failed in three critical ways:

1.  **The Echo Area Bottleneck (Truncation)**
    Historically, `eldoc` was hardcoded to print documentation to the echo area (the minibuffer at the bottom of the frame). The echo area is strictly limited to 1 or 2 lines. Long LSP docstrings were brutally truncated, forcing users to either open the `*Messages*` buffer to read the full text or rely on third-party packages to intercept the string.
2.  **Markdown Illiteracy (No Fontification)**
    LSP servers return documentation formatted in Markdown. Older versions of Emacs `eldoc` treated these payloads as plain text. The echo area would display raw Markdown syntax (e.g., `**bold**`, `code`, `### Headers`) without any syntax highlighting, font-lock, or structural rendering, making complex documentation nearly unreadable.
3.  **Lack of Scrollability**
    Because the echo area is not a standard interactive buffer, users could not scroll through long documentation blocks or copy text directly from the hover payload.

#### The `eldoc-box` Solution

To bypass these limitations, `eldoc-box` (and its `lsp-mode` equivalent, `lsp-ui-doc`) intercepted the `eldoc` string and rendered it inside a GUI child frame (a floating popup window) anchored directly to the cursor coordinates.

- It piped the payload through `markdown-mode` to render rich text, tables, and syntax-highlighted code blocks.
- It provided a fully scrollable viewport for massive type signatures.
- Because it used child frames, it floated above the text without triggering Emacs' window redisplay engine, preventing the editor layout from shifting or resizing.

#### Why it is Rejected in the Emacs 31 Stack

In the context of the Emacs 31 `eglot`-only stack documented in this project, `eldoc-box` is classified as obsolete and redundant due to native core enhancements:

- **Native Ephemeral Buffers:** Emacs 31 introduces `eldoc-echo-area-prefer-doc-buffer`. When a docstring exceeds the echo area, Emacs natively routes it to a dedicated `*eldoc*` buffer.
- **Native Tree-Sitter Markdown:** Emacs 31 natively integrates `markdown-ts-mode`, allowing the ephemeral `*eldoc*` buffer to fontify LSP Markdown payloads at C-speed without requiring the heavy `markdown-mode` dependency.
- **Protocol Compliance:** The architectural mandate of this stack prioritizes native Emacs primitives over third-party child-frame managers to reduce memory overhead and Wayland/PGTK rendering glitches.

**The One Remaining UI Distinction:**
The only technical reason a user might still seek out `eldoc-box` on Emacs 31 is UI physics. Emacs 31's native ephemeral buffer relies on standard window management (`display-buffer`), which can cause window splits or layout shifts when displaying long documentation. `eldoc-box` uses child frames, which overlay the screen without altering the window tree. However, for a strictly minimal, native-first configuration, Emacs 31's native ephemeral routing is the mathematically correct choice.

#### Analysis: `peek` vs. `eldoc-box` for Hover Documentation

They do not serve the same UI function. They rely on fundamentally different Emacs rendering engines, which dictates their use case:

- **`eldoc-box` (Childframes):** Spawns a true GUI childframe (a separate, floating OS-level window managed by Emacs). It hovers above the text without altering the buffer's layout, shifting lines, or causing redisplay jitter. This perfectly mimics VS Code's floating hover tooltip. It requires a GUI environment (PGTK/Wayland/X11).
- **`peek` (Overlays):** As explicitly noted in the `peek` source repository's "Future Plan" section: _"Child frame. (Currently Peek only support overlay.)"_ It renders _inline_ within the current buffer using `before-string` / `after-string` overlays or by physically shifting buffer text downward. This mimics VS Code's "Peek Definition" inline panel (Alt+F12), which expands _inside_ the editor viewport.

**Conclusion:** Using `peek` for LSP Hover Info would cause severe visual jitter, text-shifting, and main-thread redisplay overhead on every cursor movement. `eldoc-box` is the mathematically correct package for Hover Info (floating tooltips), while `peek` is strictly reserved for Peek Definition / Peek References (inline structural panels).

---

## Hover Info (Childframe Variant)

_VS Code feature: Tooltip with type info, docs, and signatures on mouse-hover or keyboard shortcut._

### Feature Overview

| Attribute          | Value                                                                     |
| ------------------ | ------------------------------------------------------------------------- |
| Feature            | Hover info                                                                |
| VS Code equivalent | Floating tooltip with rich markdown, type info, and signatures            |
| Status             | 🟢 working · `eldoc-box` (GUI childframe) + native `eldoc` (TTY fallback) |
| Category           | Completion & Intelligence                                                 |
| LSP methods        | `textDocument/hover`                                                      |
| Emacs routing      | `eglot` → `eldoc` → `eldoc-box` (childframe) OR `*eldoc*` buffer (TTY)    |

### Implementation Stack

| Layer                  | Component                     | Role                                                                                                                          |
| ---------------------- | ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| LSP Client             | `eglot` (built-in, Emacs 31)  | Drives `textDocument/hover`, returning Markdown payloads natively via `eglot-hover-eldoc-function`.                           |
| Documentation Router   | `eldoc` (built-in)            | Aggregates hover payloads and routes them to the active display backend.                                                      |
| GUI Rendering Engine   | `eldoc-box`                   | Spawns a floating _childframe_ anchored to the cursor, rendering rich markdown without shifting buffer text (VS Code parity). |
| TTY Fallback Engine    | `eldoc` (Emacs 31 native)     | Routes payloads to an ephemeral `*eldoc*` buffer or echo area when childframes are unavailable (e.g., over SSH/TTY).          |
| Markdown Fontification | `markdown-ts-mode` (built-in) | Provides C-level tree-sitter syntax highlighting for code blocks inside the hover tooltip.                                    |

### Commands & Keybindings

| Action                    | Command                        | Keybinding             | Notes                                                            |
| ------------------------- | ------------------------------ | ---------------------- | ---------------------------------------------------------------- |
| Hover at point (keyboard) | `eldoc`                        | `K` (Evil normal)      | Triggers `textDocument/hover` and spawns the childframe.         |
| Help at point             | `help-at-point`                | `C-h .`                | Native Emacs help surfacing via `eldoc-help-at-pt`.              |
| Scroll hover tooltip      | `eldoc-box-scroll-up` / `down` | `C-M-v` / `C-M-S-v`    | Scrolls the childframe when docstrings exceed the viewport.      |
| Toggle candidate docs     | `corfu-popupinfo-toggle`       | `M-h` (in `corfu-map`) | Shows/hides childframe docs for the active completion candidate. |

### Configuration

This configuration enforces a strict boundary: GUI frames utilize `eldoc-box` for floating childframes, while TTY/daemon frames gracefully degrade to Emacs 31's native ephemeral buffers.

```elisp
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
  (eldoc-help-at-pt t)
  ;; Emacs 31 NEW: TTY Fallback. When `eldoc-box` cannot spawn a childframe
  ;; (e.g., over SSH or in a terminal), route long docs to the ephemeral
  ;; `*eldoc*` buffer instead of truncating them in the echo area.
  (eldoc-echo-area-prefer-doc-buffer t)
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

### Why This Approach (vs. `peek` / `lsp-ui-doc`)

| Consideration       | `eldoc-box` (chosen)                               | `peek` (rejected for hover)                            | `lsp-ui-doc` (rejected)                |
| ------------------- | -------------------------------------------------- | ------------------------------------------------------ | -------------------------------------- |
| Rendering Engine    | _Childframe_ (Floating GUI window)                 | _Overlay_ (Inline buffer shift)                        | Childframe (Heavy lsp-mode dependency) |
| UI Physics          | Floats _above_ text; zero layout shift.            | Shifts buffer text down; causes redisplay jitter.      | Floats above text.                     |
| Protocol Compliance | Works with _any_ eldoc backend (eglot).            | Works with xref/eldoc, but designed for inline panels. | Hard-bound to forbidden `lsp-mode`.    |
| Use Case Parity     | _Hover Info_ (VS Code Tooltip).                    | _Peek Definition_ (VS Code Alt+F12 panel).             | Hover Info.                            |
| Performance         | Lightweight, respects `eldoc-box-only-multi-line`. | High redisplay overhead if used for idle hover.        | Heavy child-frame pipeline.            |

### Behavioral Parity Matrix

| VS Code behavior                  | Emacs 31 equivalent                                                                |
| --------------------------------- | ---------------------------------------------------------------------------------- |
| Floating tooltip on cursor idle   | `eldoc-box-hover-at-point-mode` spawns childframe after `eldoc-idle-delay`.        |
| Rich markdown rendering           | `markdown-ts-mode` fontifies code blocks inside the `eldoc-box` childframe.        |
| Tooltip disappears on cursor move | `eldoc-box-clear-after-use t` destroys the childframe instantly.                   |
| Single-line hints in status bar   | `eldoc-box-only-multi-line t` keeps 1-liners in the echo area.                     |
| Scroll long documentation         | `C-M-v` / `C-M-S-v` scrolls the `eldoc-box` childframe window.                     |
| Hover on completion candidate     | `corfu-popupinfo-toggle` (`M-h`) spawns a childframe for `completionItem/resolve`. |
| Works over SSH / Terminal         | Emacs 31 `eldoc-echo-area-prefer-doc-buffer` routes to `*eldoc*` buffer natively.  |

### Emacs 31 Specific Enhancements

- **PGTK Child-Frame Pixel Accuracy:** Emacs 31 fixes severe child-frame positioning bugs on Wayland (PGTK builds). `eldoc-box` tooltips now anchor perfectly to the cursor baseline without drifting or clipping off-screen under GNOME/mutter.
- **Native TTY Degradation:** If `eldoc-box` detects a TTY frame (where childframes are unsupported), Emacs 31's native `eldoc-echo-area-prefer-doc-buffer` seamlessly intercepts the payload and routes it to a split `*eldoc*` buffer, ensuring hover info is never lost over SSH.
- **`markdown-ts-mode` Integration:** Emacs 31's native tree-sitter markdown mode fontifies the childframe buffer at C-speed, providing syntax-highlighted code blocks inside the hover tooltip without requiring the heavy `markdown-mode` package.

---

## Signature Help

_VS Code feature: Parameter hints shown while typing inside a function call._

### Feature Overview

| Attribute          | Value                                                                               |
| ------------------ | ----------------------------------------------------------------------------------- |
| Feature            | Signature help                                                                      |
| VS Code equivalent | Floating parameter hints highlighting the active argument while typing `(` or `,`   |
| Status             | 🟢 native eldoc · `eglot` + `eldoc` · no third-party UI required                    |
| Category           | Completion & Intelligence                                                           |
| LSP methods        | `textDocument/signatureHelp`                                                        |
| Emacs routing      | `eglot` → `eglot-signature-eldoc-function` → `eldoc` (echo area / ephemeral buffer) |

### Implementation Stack

| Layer                | Component                       | Role                                                                                                                                           |
| -------------------- | ------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)    | Intercepts trigger characters (`(`, `,`), queries `textDocument/signatureHelp`, and parses the active parameter index.                         |
| Documentation Router | `eldoc` (built-in)              | Aggregates the signature payload and routes it to the echo area or ephemeral buffer.                                                           |
| Rendering Engine     | `eldoc` + `markdown-ts-mode`    | Highlights the active parameter using the `eldoc-highlight-function-argument` face and fontifies code blocks in the `*eldoc*` buffer.          |
| Trigger Mechanism    | `eldoc-documentation-functions` | `eglot` injects `eglot-signature-eldoc-function` into this hook, triggering automatically on `post-command-hook` when inside a callable scope. |

### Commands & Keybindings

| Action                   | Command                    | Keybinding    | Notes                                                                            |
| ------------------------ | -------------------------- | ------------- | -------------------------------------------------------------------------------- |
| Manual signature trigger | `eldoc`                    | `C-h .` / `K` | Forces a `textDocument/signatureHelp` query if the automatic trigger was missed. |
| Scroll long signature    | `scroll-other-window`      | `C-M-v`       | Scrolls the `*eldoc*` ephemeral buffer when a signature exceeds the echo area.   |
| Scroll signature (back)  | `scroll-other-window-down` | `C-M-S-v`     | Reverse scroll for massive C++/Rust generic signatures.                          |
| Help at point            | `help-at-point`            | `C-h .`       | Native Emacs help surfacing that integrates with eldoc payloads.                 |

### Configuration

Signature help requires zero additional packages. It relies entirely on `eglot`'s native integration with Emacs' built-in `eldoc` engine. The configuration focuses on optimizing how Emacs 31 handles long signatures that would otherwise clobber the echo area.

```elisp
;; ==========================================
;; 1. ELDOC CORE (Signature & Hover Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: When a signature (or hover doc) exceeds the echo area,
  ;; automatically route it to the ephemeral `*eldoc*` buffer instead of
  ;; truncating it or expanding the echo area to 10 lines (which causes UI jitter).
  (eldoc-echo-area-prefer-doc-buffer t)
  ;; Allow multi-line signatures in the echo area if they fit within 3 lines.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering signature/hover queries (prevents LSP network spam).
  (eldoc-idle-delay 0.5)
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  (eldoc-help-at-pt t)
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

### Why This Approach (vs. `lsp-ui-sideline` / `lsp-signature`)

| Consideration       | `eldoc` native (chosen)                                 | `lsp-ui-sideline` (rejected)                         | `lsp-signature` (rejected)                  |
| ------------------- | ------------------------------------------------------- | ---------------------------------------------------- | ------------------------------------------- |
| LSP client coupling | Works with _any_ eldoc backend (eglot, native elisp).   | Hard-bound to `lsp-mode` ecosystem.                  | Hard-bound to `lsp-mode` ecosystem.         |
| Protocol compliance | Honors the `eglot`-only stack mandate.                  | Requires forbidden `lsp-mode` ecosystem.             | Requires forbidden `lsp-mode` ecosystem.    |
| UI Physics          | Echo area (fast) or ephemeral buffer (no layout shift). | Renders in margins/sidelines (causes text shifting). | Child-frame overlays (heavy, Wayland bugs). |
| Performance         | Zero additional packages, native C-level echo area.     | High redisplay overhead on every keystroke.          | Child-frame rendering latency.              |
| Maintenance         | Maintained by GNU Emacs core team.                      | Stale — tracks lsp-mode lifecycle.                   | Stale — tracks lsp-mode lifecycle.          |

### Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                                                                                            |
| ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------ |
| Auto-trigger on `(` or `,`           | `eglot` registers trigger characters via `textDocument/signatureHelp` capabilities; `eldoc` fires on `post-command-hook`.      |
| Highlights active parameter          | `eglot` applies `eldoc-highlight-function-argument` face to the active parameter index.                                        |
| Cycles through overloads             | `eldoc` natively supports multiple signatures; `C-h .` or arrow keys can cycle if the server returns an array of signatures.   |
| Floating tooltip for long signatures | Emacs 31 `eldoc-echo-area-prefer-doc-buffer t` routes long signatures to the `*eldoc*` buffer without shifting window layouts. |
| Manual trigger shortcut              | `C-h .` (`help-at-point`) or `K` (`eldoc`).                                                                                    |
| Dismiss on cursor move               | `eldoc` automatically clears the echo area or hides the ephemeral buffer when the cursor leaves the callable scope.            |

### Emacs 31 Specific Enhancements

- **`eldoc-echo-area-prefer-doc-buffer` (NEW):** In older Emacs versions, long C++ template signatures or Rust generic bounds would either truncate in the echo area or force the echo area to expand to 5+ lines, causing severe UI jitter and modeline clobbering. Emacs 31 introduces this variable to automatically route oversized signatures to a dedicated, scrollable `*eldoc*` buffer, perfectly mirroring VS Code's floating signature widget without requiring third-party child-frame packages.
- **`markdown-ts-mode` Integration:** When signatures are routed to the `*eldoc*` buffer, Emacs 31's native tree-sitter markdown mode fontifies any code blocks or type annotations embedded in the signature documentation, providing rich syntax highlighting at C-speed.
- **`elisp-eldoc-funcall-with-docstring`:** For Emacs Lisp buffers, Emacs 31's native eldoc engine now merges the function signature with its docstring in a single, highly optimized payload, reducing the need for separate hover queries when inspecting Elisp functions.
- **TTY-Safe Degradation:** Unlike child-frame-based signature widgets (`lsp-signature`), `eldoc` degrades gracefully to the echo area or standard window splits over SSH/TTY, ensuring signature help is always accessible in terminal environments.

---

## Semantic Tokens (Semantic Highlighting)

_VS Code feature: Type-aware syntax coloring beyond what static TextMate grammars can do._

### Feature Overview

| Attribute          | Value                                                                                                                                      |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------ |
| Feature            | Semantic Tokens (Semantic Highlighting)                                                                                                    |
| VS Code equivalent | Syntax highlighting based on semantic analysis (e.g., distinguishing parameters from local variables, or function declarations from calls) |
| Status             | 🟢 working · native `eglot` + `treesit`                                                                                                    |
| Category           | Completion & Intelligence                                                                                                                  |
| LSP methods        | `textDocument/semanticTokens/full`, `textDocument/semanticTokens/range`, `textDocument/semanticTokens/delta`                               |
| Emacs routing      | `eglot` → `eglot-semantic-tokens-mode` → native `font-lock` / `treesit`                                                                    |

### Implementation Stack

| Layer                 | Component                    | Role                                                                                                                                                     |
| --------------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client            | `eglot` (built-in, Emacs 31) | Negotiates `semanticTokensProvider` capabilities and requests token payloads via full, range, or delta methods.                                          |
| Semantic Engine       | `eglot-semantic-tokens-mode` | Built-in Eglot minor mode that applies LSP semantic token types and modifiers to the buffer using text properties.                                       |
| Baseline Highlighting | `treesit` (built-in)         | Provides fast, C-level structural syntax highlighting. Semantic tokens _augment_ this baseline rather than replacing it, ensuring zero-latency fallback. |
| Face Customization    | `eglot-semantic-faces`       | Customization group allowing users to map specific LSP token types (e.g., `variable.readonly`, `function.declaration`) to Emacs faces.                   |

### Commands & Keybindings

| Action                         | Command                      | Keybinding                                         | Notes                                                                |
| ------------------------------ | ---------------------------- | -------------------------------------------------- | -------------------------------------------------------------------- |
| Toggle semantic tokens         | `eglot-semantic-tokens-mode` | `SPC t s`                                          | Enables/disables LSP semantic highlighting for the current buffer.   |
| Customize token faces          | `customize-group`            | `M-x customize-group RET eglot-semantic-faces RET` | Adjust colors for specific token types (e.g., parameters, macros).   |
| Toggle inlay hints (companion) | `eglot-inlay-hints-mode`     | `SPC t h`                                          | Often used alongside semantic tokens for full type-aware annotation. |

### Configuration

Emacs 31's `eglot` enables semantic tokens automatically if the language server advertises support. Explicit configuration ensures optimal performance and seamless integration with `treesit`.

```elisp
;; ==========================================
;; EGLOT SEMANTIC TOKENS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :hook ((prog-mode . eglot-ensure))
  :config
  ;; Semantic tokens are enabled by default in modern Eglot if the server supports them.
  ;; We explicitly ensure the mode is active and configure it to augment, not replace, treesit.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              ;; Enable semantic tokens for enhanced type-aware highlighting
              (eglot-semantic-tokens-mode 1)))
  ;; Optional: Fine-tune which token types/modifiers are considered for performance.
  ;; By default, Eglot respects the server's legend, but you can filter if needed.
  ;; (setq eglot-semantic-token-types '(variable parameter function method))
  )

;; ==========================================
;; TREESIT BASELINE (Emacs 31 Native)
;; ==========================================
;; Ensure treesit provides the foundational structural highlighting.
;; Semantic tokens will layer on top of this for type-specific nuances.
(setq treesit-font-lock-level 4) ;; Maximum structural decoration
```

### Why This Approach (vs. `lsp-mode` semantic highlighting)

| Consideration       | `eglot` native (chosen)                                                                                                          | `lsp-mode` (rejected)                                                                                                 |
| ------------------- | -------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with built-in `eglot`.                                                                                         | Hard-bound to the `lsp-mode` ecosystem.                                                                               |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                                           | Requires forbidden `lsp-mode` ecosystem.                                                                              |
| Performance         | Leverages Emacs 31's optimized text property application; defers to `treesit` for baseline, reducing redundant font-lock passes. | Historically heavy; applies full buffer fontification independently, sometimes causing micro-stutters on large files. |
| Emacs 31 synergy    | Natively integrates with `treesit-font-lock-level 4`, allowing LSP to _augment_ structural highlighting rather than fight it.    | Often overrides or conflicts with native tree-sitter fontification rules.                                             |

### Behavioral Parity Matrix

| VS Code behavior                    | Emacs 31 equivalent                                                                                                                             |
| ----------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Type-aware syntax coloring          | `eglot-semantic-tokens-mode` applies faces based on LSP token types (e.g., distinguishing a `parameter` from a `local variable`).               |
| Delta updates on edit               | Eglot natively requests `textDocument/semanticTokens/delta` to minimize network payload and main-thread blocking.                               |
| Range requests on scroll            | Eglot requests `textDocument/semanticTokens/range` for visible regions only, optimizing performance in massive files.                           |
| Customizable token colors           | `eglot-semantic-faces` customization group allows mapping specific LSP modifiers (e.g., `readonly`, `deprecated`) to Tokyo Night palette faces. |
| Fallback to structural highlighting | If the LSP server is slow or disconnects, `treesit` (level 4) maintains perfect, zero-latency structural syntax highlighting.                   |

### Emacs 31 Specific Enhancements

- **`eglot-semantic-tokens-mode` (NEW):** Officially integrated into Eglot, this minor mode provides enhanced syntax highlighting based on the language server's semantic analysis, going beyond traditional regular-expression-based fontification.
- **`treesit` Augmentation:** Emacs 31's `treesit` engine provides a robust, C-level baseline. Eglot's semantic tokens are designed to augment this baseline, applying specific faces (like `font-lock-variable-name-face` with a `readonly` modifier) without stripping the underlying structural tree-sitter highlights.
- **Delta & Range Optimization:** Modern Eglot implementations efficiently handle `semanticTokens/delta` and `semanticTokens/range` requests, ensuring that typing or scrolling in large files does not trigger full-buffer re-highlighting network requests, preserving the 60fps typing experience.
- **Face Customization Group:** The `eglot-semantic-faces` group allows precise control over how token types (e.g., `namespace`, `type`, `function`) and modifiers (e.g., `declaration`, `readonly`, `deprecated`) are rendered, enabling perfect alignment with the Tokyo Night theme.

### Integration with Existing Stack

- **`eglot`:** Natively handles the negotiation of `semanticTokensProvider` capabilities during the LSP initialization handshake.
- **`treesit`:** Provides the foundational `treesit-font-lock-level 4` highlighting, ensuring that even if semantic tokens are disabled or the server is slow, the buffer remains beautifully and accurately highlighted.
- **`doom-themes`:** The Tokyo Night theme can be extended to map specific `eglot-semantic-*` faces to the palette's neon accents (e.g., `#7aa2f7` for types, `#bb9af7` for parameters), creating a cohesive, type-aware visual experience.
- **`apheleia`:** Formatting operations do not disrupt semantic token overlays, as Eglot efficiently recalculates token positions post-edit via delta requests.

### Troubleshooting

#### Semantic Tokens Not Appearing

1.  **Verify Server Support:** Check the `*eglot-events*` buffer or run `M-x eglot-describe-connection` to confirm the server advertises `semanticTokensProvider` in its capabilities.
2.  **Check Minor Mode:** Ensure `eglot-semantic-tokens-mode` is active in the buffer (`C-h m`).
3.  **Restart Connection:** Changes to `eglot-semantic-token-types` or `eglot-semantic-token-modifiers` require an `M-x eglot-reconnect` to take effect, as the legend is negotiated at startup.

#### Performance Stutter on Large Files

If the language server sends massive full-token payloads, force range-only requests by ensuring the server supports it, or reduce the `treesit-font-lock-level` to `3` to lessen the combined font-lock burden, though level 4 is generally optimal in Emacs 31.

---

## Inlay Hints

_VS Code feature: Inline grey annotations showing inferred types, parameter names, etc._

### Feature Overview

| Attribute          | Value                                                             |
| ------------------ | ----------------------------------------------------------------- |
| Feature            | Inlay Hints                                                       |
| VS Code equivalent | Inline type annotations, parameter name hints, and chaining hints |
| Status             | 🟢 working · native `eglot`                                       |
| Category           | Completion & Intelligence                                         |
| LSP methods        | `textDocument/inlayHint`, `inlayHint/resolve`                     |
| Emacs routing      | `eglot` → `eglot-inlay-hints-mode` → native buffer overlays       |

### Implementation Stack

| Layer            | Component                    | Role                                                                                                                                                 |
| ---------------- | ---------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client       | `eglot` (built-in, Emacs 31) | Negotiates `inlayHintProvider` capabilities and requests hint payloads via `textDocument/inlayHint` and `inlayHint/resolve`.                         |
| Rendering Engine | `eglot-inlay-hints-mode`     | Built-in minor mode that applies hint text as `before-string` or `after-string` overlays directly in the buffer.                                     |
| Visual Styling   | `eglot-inlay-hint-*` faces   | Dedicated faces for different hint kinds (e.g., `eglot-inlay-hint-type-face`, `eglot-inlay-hint-parameter-face`) allowing precise theme integration. |

### Commands & Keybindings

| Action               | Command                         | Keybinding | Notes                                                                                                                    |
| -------------------- | ------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------ |
| Toggle inlay hints   | `eglot-inlay-hints-mode`        | `SPC t h`  | Enables/disables inline annotations for the current buffer.                                                              |
| Toggle globally      | `global-eglot-inlay-hints-mode` | —          | Enables inlay hints across all `eglot`-managed buffers.                                                                  |
| Resolve hint details | `eglot-inlay-hint-resolve`      | —          | Triggered automatically by `eglot` when hovering or interacting with a hint, if the server requires `inlayHint/resolve`. |

### Configuration

`eglot` provides native, zero-dependency inlay hint rendering. The configuration below enables the feature and aligns the hint faces with the Tokyo Night palette for optimal visual hierarchy.

```elisp
;; ==========================================
;; EGLOT INLAY HINTS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :hook ((prog-mode . eglot-ensure))
  :config
  ;; Enable inlay hints globally for all eglot-managed buffers.
  ;; Can be toggled per-buffer via `eglot-inlay-hints-mode' or `SPC t h`.
  (global-eglot-inlay-hints-mode 1)

  ;; ==========================================
  ;; VISUAL STYLING (Tokyo Night Synergy)
  ;; ==========================================
  ;; Inlay hints should be recessive to avoid competing with primary syntax highlighting.
  (custom-set-faces
   '(eglot-inlay-hint-face ((t (:inherit shadow :height 0.9 :slant italic))))
   '(eglot-inlay-hint-type-face ((t (:inherit shadow :foreground "#73daca" :height 0.9 :slant italic))))
   '(eglot-inlay-hint-parameter-face ((t (:inherit shadow :foreground "#bb9af7" :height 0.9 :slant italic))))))
```

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` native (chosen)                                                                                                                                | `lsp-mode` (rejected)                                                                                 |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with built-in `eglot`.                                                                                                               | Hard-bound to the `lsp-mode` ecosystem.                                                               |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                                                                 | Requires forbidden `lsp-mode` ecosystem.                                                              |
| Rendering Physics   | Uses native Emacs `before-string`/`after-string` text properties, ensuring seamless integration with `treesit` fontification and variable-pitch fonts. | Historically relied on complex, fragile overlay management that could conflict with native font-lock. |
| Performance         | Lightweight overlay application; defers `inlayHint/resolve` network calls until explicitly needed.                                                     | Aggressive background resolution can cause main-thread micro-stutters on slower servers.              |

### Behavioral Parity Matrix

| VS Code behavior                                 | Emacs 31 equivalent                                                                                                      |
| ------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------ |
| Inline grey text for parameter names             | `eglot-inlay-hint-parameter-face` renders recessive, italicized parameter names.                                         |
| Inline type annotations for variables            | `eglot-inlay-hint-type-face` renders inferred types adjacent to declarations.                                            |
| Hints disappear when typing in the hint location | `eglot` automatically clears and re-requests hints on buffer modification.                                               |
| Click/hover to see full resolved hint            | `eglot` natively triggers `inlayHint/resolve` when the cursor rests on or interacts with the hint overlay.               |
| Toggle hints via command palette                 | `SPC t h` (`eglot-inlay-hints-mode`) or `M-x global-eglot-inlay-hints-mode`.                                             |
| Respects variable-pitch fonts                    | Emacs 31's overlay renderer correctly calculates spacing even when mixing monospaced code with variable-pitch hint text. |

### Emacs 31 Specific Enhancements

- **Variable-Pitch Font Support:** Emacs 31's `eglot` overlay renderer has been explicitly optimized to calculate bounding boxes and spacing correctly when `variable-pitch-mode` is active, preventing the misaligned or overlapping hint text that plagued older Emacs versions.
- **Granular Face Customization:** Upstream `eglot` now exposes distinct faces for different hint kinds (`eglot-inlay-hint-type-face`, `eglot-inlay-hint-parameter-face`), allowing users to color-code hints (e.g., teal for types, magenta for parameters) without resorting to fragile regex-based font-lock hacks.
- **Efficient `inlayHint/resolve`:** `eglot` intelligently batches and debounces `inlayHint/resolve` requests, ensuring that hovering over or interacting with a hint does not spam the language server, preserving the 60fps typing experience.

### Integration with Existing Stack

- **`eglot`:** Natively handles the entire lifecycle of inlay hints, from capability negotiation during initialization to overlay cleanup on buffer kill.
- **`treesit`:** Inlay hint overlays are applied with a lower priority than `treesit` font-lock, ensuring that primary syntax highlighting (e.g., string literals, keywords) always takes visual precedence over recessive hint text.
- **`doom-themes`:** The custom face definitions seamlessly inherit the Tokyo Night `shadow` and specific accent colors (`#73daca`, `#bb9af7`), maintaining a cohesive, professional IDE aesthetic.
- **`general.el`:** The `SPC t h` keybinding provides a consistent, mnemonic toggle for inlay hints across all programming buffers, aligning with Doom Emacs muscle memory.

---


---

# Navigation & Code Jumping

## Go to Definition

_VS Code feature: Jump to (or peek) where a symbol is defined (F12 / Ctrl+Click)._

### Feature Overview

| Attribute          | Value                                                                                          |
| ------------------ | ---------------------------------------------------------------------------------------------- |
| Feature            | Go to Definition                                                                               |
| VS Code equivalent | F12 to jump, Ctrl+Click to jump, Alt+F12 to peek inline                                        |
| Status             | 🟢 working · `eglot` + `xref` + `consult-xref` + Emacs 31 `xref-mouse-mode`                    |
| Category           | Navigation & Code Jumping                                                                      |
| LSP methods        | `textDocument/definition`                                                                      |
| Emacs routing      | `eglot` → `xref-find-definitions` → `consult-xref` (preview dropdown) OR `peek` (inline panel) |

### Implementation Stack

| Layer                | Component                             | Role                                                                                                                                                                     |
| -------------------- | ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| LSP Client           | `eglot` (built-in, Emacs 31)          | Drives `textDocument/definition` and injects the location payload into the native `xref` framework.                                                                      |
| Navigation Framework | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                             |
| Preview Engine       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when multiple definitions exist (e.g., overloaded functions). |
| Mouse Integration    | `xref-mouse-mode` (Emacs 31 NEW)      | Binds `C-<mouse-1>` to `xref-find-definitions-at-mouse`, enabling native Ctrl+Click jumps.                                                                               |
| Inline Peek          | `peek` (sr.ht/~meow_king/peek)        | Renders the definition inline below the cursor using overlays without switching tabs (Alt+F12 parity).                                                                   |

### Commands & Keybindings

| Action                          | Command                              | Keybinding    | Notes                                                              |
| ------------------------------- | ------------------------------------ | ------------- | ------------------------------------------------------------------ |
| Jump to definition              | `xref-find-definitions`              | `M-.` / `g d` | Jumps to target; opens `consult-xref` dropdown if ambiguous.       |
| Jump to definition (mouse)      | `xref-find-definitions-at-mouse`     | `C-<mouse-1>` | _Emacs 31 NEW_ — Ctrl+Click parity via `global-xref-mouse-mode`.   |
| Peek definition (inline)        | `peek-xref-definition`               | `SPC c p d`   | Shows target in an inline overlay panel (requires `peek` package). |
| Go back (history)               | `xref-go-back`                       | `M-,` / `g ,` | Returns to the exact cursor position before the jump.              |
| Go forward (history)            | `xref-go-forward`                    | —             | Reverses `xref-go-back`.                                           |
| Find definitions (other window) | `xref-find-definitions-other-window` | `C-x 4 .`     | Opens definition in a horizontal split.                            |
| Find definitions (other frame)  | `xref-find-definitions-other-frame`  | `C-x 5 .`     | Opens definition in a new frame.                                   |

### Configuration

```elisp
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

### Why This Approach (vs. `lsp-ui-peek` / `lsp-mode`)

| Consideration       | `xref` + `consult-xref` (chosen)                                          | `lsp-ui-peek` / `lsp-mode` (rejected)                      |
| ------------------- | ------------------------------------------------------------------------- | ---------------------------------------------------------- |
| LSP client coupling | Works with _any_ xref backend (eglot, dumb-jump, etags, tags).            | Hard-bound to the `lsp-mode` ecosystem.                    |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                    | Requires the forbidden `lsp-mode` ecosystem.               |
| Preview engine      | `consult-xref` leverages `vertico` for fuzzy filtering and live previews. | Custom child-frame pipeline with heavy rendering overhead. |
| Mouse integration   | Native Emacs 31 `xref-mouse-mode` (zero dependencies).                    | Requires custom mouse-click advice.                        |
| History tracking    | Native `xref` history ring (shared across all backends).                  | Fragmented history management.                             |

### Behavioral Parity Matrix

| VS Code behavior                  | Emacs 31 equivalent                                        |
| --------------------------------- | ---------------------------------------------------------- |
| `F12` jumps to definition         | `M-.` or `g d` (`xref-find-definitions`)                   |
| `Ctrl+Click` jumps to definition  | `C-<mouse-1>` via Emacs 31 `global-xref-mouse-mode`        |
| `Alt+F12` peeks definition inline | `SPC c p d` (`peek-xref-definition`) via `peek` package    |
| Dropdown for multiple definitions | `consult-xref` intercepts `xref-show-definitions-function` |
| Live preview in dropdown          | `consult-xref` + `vertico` live buffer previews            |
| `Alt+Left` returns to origin      | `M-,` or `g ,` (`xref-go-back`)                            |
| Open in split window              | `C-x 4 .` (`xref-find-definitions-other-window`)           |

### Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to `xref-find-definitions-at-mouse`, perfectly mirroring VS Code's Ctrl+Click convention without requiring third-party mouse-click advice.
- **Editable Xref Buffers:** Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns reference/definition lists into writable surfaces. While primarily used for "Find All References", it can be used to bulk-edit multiple definition sites if a language server returns them.
- **`consult-xref` synergy:** The integration of `consult-xref` with Emacs 31's refined `xref` API ensures that ambiguous definitions (e.g., C++ overloaded functions, TypeScript union types) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews.
- **`peek` package integration:** The `peek` package hooks directly into the `xref` framework via `peek-definition-function`, allowing it to intercept `textDocument/definition` payloads from `eglot` and render them as inline overlays. This provides true "Peek Definition" parity without the heavy child-frame overhead of `lsp-ui-peek`.

### Integration with Existing Stack

The Go to Definition surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `M-.` to `textDocument/definition`.
- **`consult`:** `consult-xref` intercepts the xref display functions to provide vertico-powered previews.
- **`treesit`:** For non-LSP buffers (or when eglot is disconnected), `treesit` modes can provide fallback definition jumping via `treesit-thing` navigation or `imenu` integration.
- **`evil-collection`:** Standardizes `g d` and `g ,` across all major modes, ensuring Vim muscle memory is preserved.
- **`peek`:** Provides the inline overlay engine for "Peek Definition" workflows, keeping the user's spatial context intact.

---

## Go to Declaration

_VS Code feature: Jump to a symbol's declaration (distinct from definition in some languages, e.g. C/C++ headers)._

### Feature Overview

| Attribute          | Value                                                                  |
| ------------------ | ---------------------------------------------------------------------- |
| Feature            | Go to Declaration                                                      |
| VS Code equivalent | Jump to header/declaration site (distinct from F12 "Go to Definition") |
| Status             | 🟢 working · `eglot` + `xref`                                          |
| Category           | Navigation & Code Jumping                                              |
| LSP methods        | `textDocument/declaration`                                             |
| Emacs routing      | `eglot` → `eglot-find-declaration` → `xref` framework                  |

### Implementation Stack

| Layer                | Component                             | Role                                                                                                                                                                          |
| -------------------- | ------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)          | Queries `textDocument/declaration` and injects the location payload into the native `xref` framework.                                                                         |
| Navigation Framework | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                                  |
| Preview Engine       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when multiple declarations exist (e.g., C++ forward declarations). |
| Mouse Integration    | `xref-mouse-mode` (Emacs 31 NEW)      | Enables native Ctrl+Click routing for xref payloads.                                                                                                                          |

### Commands & Keybindings

| Action                    | Command                  | Keybinding      | Notes                                                                      |
| ------------------------- | ------------------------ | --------------- | -------------------------------------------------------------------------- |
| Jump to declaration       | `eglot-find-declaration` | `C-c d` / `g D` | Jumps to the header/declaration site; distinct from `M-.` (definition).    |
| Jump to definition        | `xref-find-definitions`  | `M-.` / `g d`   | Jumps to the actual implementation/definition site.                        |
| Go back (history)         | `xref-go-back`           | `M-,` / `g ,`   | Returns to the exact cursor position before the jump.                      |
| Go forward (history)      | `xref-go-forward`        | —               | Reverses `xref-go-back`.                                                   |
| Peek declaration (inline) | `peek-xref-definition`   | `SPC c p d`     | Requires `peek` package; renders the header inline without switching tabs. |

### Configuration

`eglot` natively registers `eglot-find-declaration` when a buffer is LSP-managed. No explicit hook registration is required for the command itself. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```elisp
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

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` + `xref` (chosen)                                                                               | `lsp-mode` (rejected)                                |
| ------------------- | ------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot` and native `xref`.                                                       | Hard-bound to the `lsp-mode` ecosystem.              |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                  | Requires forbidden `lsp-mode` ecosystem.             |
| History tracking    | Native `xref` history ring (shared across all backends, including `dumb-jump` and `etags`).             | Fragmented history management.                       |
| Emacs 31 synergy    | Leverages `xref-mouse-mode` and editable xref buffers.                                                  | No integration with Emacs 31 core xref enhancements. |
| Fallback physics    | `eglot` gracefully falls back to `textDocument/definition` if the server returns empty for declaration. | Custom fallback logic required.                      |

### Behavioral Parity Matrix

| VS Code behavior                              | Emacs 31 equivalent                                                                               |
| --------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| Jump to `.h` header file (C/C++)              | `g D` (`eglot-find-declaration`) routes via `textDocument/declaration`.                           |
| Jump to `.cpp` implementation file            | `g d` (`xref-find-definitions`) routes via `textDocument/definition`.                             |
| Distinct commands for Decl vs Def             | `eglot-find-declaration` vs `xref-find-definitions`.                                              |
| Fallback to definition if declaration missing | `eglot` natively falls back to definition if the LSP server returns an empty declaration payload. |
| Dropdown for multiple declarations            | `consult-xref` intercepts the xref display functions for vertico-powered previews.                |
| `Alt+Left` returns to origin                  | `M-,` (`xref-go-back`) via the native xref history ring.                                          |
| `Ctrl+Click` on symbol                        | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`).                                                |

 ### Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention without requiring third-party mouse-click advice.
- **Editable Xref Buffers:** Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns declaration lists into writable surfaces. If a symbol has multiple forward declarations across headers, you can press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating header files natively.
- **`consult-xref` synergy:** Ambiguous declarations (e.g., C++ overloaded forward declarations or TypeScript interface merges) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.

---

## Go to Type Definition

_VS Code feature: Jump to the type definition of a variable/expression (Ctrl+Shift+F12 in some bindings, or right-click → Go to Type Definition)._

### Feature Overview

| Attribute          | Value                                                                                                                                    |
| ------------------ | ---------------------------------------------------------------------------------------------------------------------------------------- |
| Feature            | Go to Type Definition                                                                                                                    |
| VS Code equivalent | Jump to where the _type_ of a symbol is defined (class, interface, struct, type alias) — distinct from the symbol's value/implementation |
| Status             | 🟢 working · `eglot` + `xref` + `consult-xref`                                                                                           |
| Category           | Navigation & Code Jumping                                                                                                                |
| LSP methods        | `textDocument/typeDefinition`                                                                                                            |
| Emacs routing      | `eglot` → `eglot-find-typeDefinition` → `xref` framework                                                                                 |

### Implementation Stack

| Layer                | Component                             | Role                                                                                                                                                                                                       |
| -------------------- | ------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)          | Queries `textDocument/typeDefinition` and injects the location payload into the native `xref` framework.                                                                                                   |
| Navigation Framework | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                                                               |
| Preview Engine       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when a symbol resolves to multiple candidate types (e.g., union types, generic instantiations). |
| Mouse Integration    | `xref-mouse-mode` (Emacs 31 NEW)      | Enables native Ctrl+Click routing for xref payloads, including type definitions.                                                                                                                           |

### Commands & Keybindings

| Action                         | Command                     | Keybinding      | Notes                                                                    |
| ------------------------------ | --------------------------- | --------------- | ------------------------------------------------------------------------ |
| Jump to type definition        | `eglot-find-typeDefinition` | `C-c t` / `g t` | Jumps to the type (class/interface/struct) of the symbol at point.       |
| Jump to definition (contrast)  | `xref-find-definitions`     | `M-.` / `g d`   | Jumps to the _value/implementation_ of the symbol.                       |
| Jump to declaration (contrast) | `eglot-find-declaration`    | `C-c d` / `g D` | Jumps to the _header/forward declaration_ of the symbol.                 |
| Go back (history)              | `xref-go-back`              | `M-,` / `g ,`   | Returns to the exact cursor position before the jump.                    |
| Peek type definition (inline)  | `peek-xref-definition`      | `SPC c p d`     | Requires `peek` package; renders the type inline without switching tabs. |

### Configuration

`eglot` natively registers `eglot-find-typeDefinition` when a buffer is LSP-managed. No explicit hook registration is required for the command itself. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```elisp
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

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` + `xref` (chosen)                                                                                   | `lsp-mode` (rejected)                                |
| ------------------- | ----------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot` and native `xref`.                                                           | Hard-bound to the `lsp-mode` ecosystem.              |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                      | Requires forbidden `lsp-mode` ecosystem.             |
| History tracking    | Native `xref` history ring (shared across all backends, including `dumb-jump` and `etags`).                 | Fragmented history management.                       |
| Emacs 31 synergy    | Leverages `xref-mouse-mode` and editable xref buffers.                                                      | No integration with Emacs 31 core xref enhancements. |
| Graceful fallback   | `eglot` gracefully falls back to `textDocument/definition` if the server returns empty for type definition. | Custom fallback logic required.                      |

### Semantic Distinction: Type vs. Definition vs. Declaration

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

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                                               |
| -------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| Right-click → Go to Type Definition    | `g t` (`eglot-find-typeDefinition`) routes via `textDocument/typeDefinition`.                                     |
| Jump to class/interface of variable    | `g t` on a variable jumps to its type's class/struct definition.                                                  |
| Jump to type of function return        | `g t` on a function call jumps to the return type's definition.                                                   |
| Dropdown for ambiguous types           | `consult-xref` intercepts the xref display functions for vertico-powered previews.                                |
| `Alt+Left` returns to origin           | `M-,` (`xref-go-back`) via the native xref history ring.                                                          |
| `Ctrl+Click` on type reference         | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`).                                                                |
| Fallback to definition if type missing | `eglot` natively falls back to definition if the LSP server returns an empty type definition payload.             |
| Works across language boundaries       | `eglot` routes through xref, which is backend-agnostic — works identically for TypeScript, Rust, Go, C++, Python. |

### Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention for type navigation without requiring third-party mouse-click advice.
- **Editable Xref Buffers:** Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns type-definition lists into writable surfaces. When a symbol's type has multiple candidate definitions (e.g., TypeScript discriminated unions, C++ template specializations), you can press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating files natively.
- **`consult-xref` synergy:** Ambiguous type definitions (e.g., TypeScript union types like `string | number | User`, Go interface implementations, Rust trait objects) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.
- **Unified xref history:** `xref-go-back` (`M-,`) treats type-definition jumps identically to definition and reference jumps — the entire navigation chain is preserved in a single ring, allowing seamless backtracking through complex type exploration sessions.

### Language-Specific Behavior

The utility of `eglot-find-typeDefinition` varies significantly by language server implementation:

| Language   | Server                                 | Behavior                                                                                                            |
| ---------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| TypeScript | `typescript-language-server` / `ts_ls` | Excellent — jumps to `interface`, `type`, `class` definitions. Critical for navigating complex generic types.       |
| Rust       | `rust-analyzer`                        | Excellent — jumps to `struct`, `enum`, `trait` definitions. Essential for understanding ownership and trait bounds. |
| Go         | `gopls`                                | Excellent — jumps to `type` declarations, `struct` definitions, and `interface` specifications.                     |
| C++        | `clangd`                               | Excellent — jumps to class/struct definitions, distinct from forward declarations in headers.                       |
| Python     | `pyright` / `pylsp`                    | Good — jumps to class definitions; less useful for dynamically-typed code where types are inferred.                 |
| Java       | `jdtls`                                | Excellent — jumps to class/interface definitions across modules.                                                    |
| Emacs Lisp | N/A                                    | Not applicable — Elisp has no static type system; `xref-find-definitions` is the canonical navigation.              |

### Integration with Existing Stack

The Go to Type Definition surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `g t` to `textDocument/typeDefinition`.
- **`consult`:** `consult-xref` intercepts the xref display functions to provide vertico-powered previews for ambiguous type targets.
- **`treesit`:** For non-LSP buffers (or when eglot is disconnected), `treesit` modes can provide fallback type-navigation via `evil-textobj-tree-sitter` text objects (e.g., `vC` to select a class node) combined with `imenu`.
- **`evil-collection`:** Standardizes `g t` across all major modes, ensuring Vim muscle memory is preserved.
- **`peek`:** Provides the inline overlay engine for "Peek Type Definition" workflows, keeping the user's spatial context intact while inspecting type shapes.
- **`breadcrumb`:** After jumping to a type definition, the breadcrumb bar immediately reflects the new enclosing class/struct context, maintaining spatial orientation.

---

## Go to Implementation

_VS Code feature: Jump to concrete implementations of an interface/abstract method._

### Feature Overview

| Attribute          | Value                                                                                     |
| ------------------ | ----------------------------------------------------------------------------------------- |
| Feature            | Go to Implementation                                                                      |
| VS Code equivalent | Jump to the concrete class or method that implements an interface or abstract definition. |
| Status             | 🟢 working · `eglot` + `xref` + `consult-xref`                                            |
| Category           | Navigation & Code Jumping                                                                 |
| LSP methods        | `textDocument/implementation`                                                             |
| Emacs routing      | `eglot` → `eglot-find-implementation` → `xref` framework                                  |

### Implementation Stack

| Layer                | Component                             | Role                                                                                                                                                                                                      |
| -------------------- | ------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)          | Queries `textDocument/implementation` and injects the location payload into the native `xref` framework.                                                                                                  |
| Navigation Framework | `xref` (built-in)                     | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                                                                              |
| Preview Engine       | `consult-xref` (bundled with consult) | Intercepts `xref-show-definitions-function` to render a vertico-powered dropdown with live buffer previews when a symbol has multiple implementations (e.g., multiple classes implementing an interface). |
| Mouse Integration    | `xref-mouse-mode` (Emacs 31 NEW)      | Enables native Ctrl+Click routing for xref payloads.                                                                                                                                                      |

### Commands & Keybindings

| Action                        | Command                     | Keybinding      | Notes                                                                      |
| ----------------------------- | --------------------------- | --------------- | -------------------------------------------------------------------------- |
| Jump to implementation        | `eglot-find-implementation` | `C-c i` / `g i` | Jumps to the concrete implementation of the interface/method at point.     |
| Jump to definition (contrast) | `xref-find-definitions`     | `M-.` / `g d`   | Jumps to the interface/abstract definition itself.                         |
| Go back (history)             | `xref-go-back`              | `M-,` / `g ,`   | Returns to the exact cursor position before the jump.                      |
| Go forward (history)          | `xref-go-forward`           | —               | Reverses `xref-go-back`.                                                   |
| Peek implementation (inline)  | `peek-xref-definition`      | `SPC c p d`     | Requires `peek` package; renders the target inline without switching tabs. |

### Configuration

`eglot` natively registers `eglot-find-implementation` when a buffer is LSP-managed. No explicit hook registration is required for the command itself. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```elisp
;; ==========================================
;; 1. XREF & CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Route xref location prompts through Consult for live previews.
  ;; When a symbol has multiple implementations (e.g., multiple classes
  ;; implementing an interface), Consult renders a Vertico dropdown
  ;; with live buffer previews.
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
  "g d" #'xref-find-definitions        ;; Go to Definition (interface/abstract)
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

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` + `xref` (chosen)                                                                                  | `lsp-mode` (rejected)                                |
| ------------------- | ---------------------------------------------------------------------------------------------------------- | ---------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot` and native `xref`.                                                          | Hard-bound to the `lsp-mode` ecosystem.              |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                     | Requires forbidden `lsp-mode` ecosystem.             |
| History tracking    | Native `xref` history ring (shared across all backends, including `dumb-jump` and `etags`).                | Fragmented history management.                       |
| Emacs 31 synergy    | Leverages `xref-mouse-mode` and editable xref buffers.                                                     | No integration with Emacs 31 core xref enhancements. |
| Graceful fallback   | `eglot` gracefully falls back to `textDocument/definition` if the server returns empty for implementation. | Custom fallback logic required.                      |

### Semantic Distinction: Implementation vs. Definition

Understanding when to use each command is critical for efficient navigation in object-oriented or interface-driven languages:

| Command                          | Jumps To                                             | Example in TypeScript                                | Example in Rust                              |
| -------------------------------- | ---------------------------------------------------- | ---------------------------------------------------- | -------------------------------------------- |
| **Go to Definition** (`g d`)     | The interface or abstract method signature           | `interface UserService { ... }`                      | `trait UserService { ... }`                  |
| **Go to Implementation** (`g i`) | The concrete class or method fulfilling the contract | `class DbUserService implements UserService { ... }` | `impl UserService for DbUserService { ... }` |

**When to use `g i` specifically:**

- Navigating from an interface method call to the actual business logic.
- Discovering all classes that implement a specific dependency injection interface.
- Exploring trait implementations in Rust or Go.
- Understanding how an abstract base class is realized in derived classes.

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                                             |
| -------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| Right-click → Go to Implementation     | `g i` (`eglot-find-implementation`) routes via `textDocument/implementation`.                                   |
| Dropdown for multiple implementations  | `consult-xref` intercepts the xref display functions for vertico-powered previews.                              |
| `Alt+Left` returns to origin           | `M-,` (`xref-go-back`) via the native xref history ring.                                                        |
| `Ctrl+Click` on interface name         | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`).                                                              |
| Fallback to definition if impl missing | `eglot` natively falls back to definition if the LSP server returns an empty implementation payload.            |
| Works across language boundaries       | `eglot` routes through xref, which is backend-agnostic — works identically for TypeScript, Rust, Go, Java, C++. |

### Emacs 31 Specific Enhancements

- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention for implementation navigation without requiring third-party mouse-click advice.
- **Editable Xref Buffers:** Emacs 31's `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers) turns implementation lists into writable surfaces. When an interface has multiple candidate implementations, you can press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating files natively.
- **`consult-xref` synergy:** Ambiguous implementations (e.g., TypeScript classes implementing a common interface, Go structs satisfying an interface, Rust trait objects) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.
- **Unified xref history:** `xref-go-back` (`M-,`) treats implementation jumps identically to definition and reference jumps — the entire navigation chain is preserved in a single ring, allowing seamless backtracking through complex architectural exploration sessions.

### Language-Specific Behavior

The utility of `eglot-find-implementation` varies significantly by language server implementation:

| Language   | Server                                 | Behavior                                                                                                                        |
| ---------- | -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| TypeScript | `typescript-language-server` / `ts_ls` | Excellent — accurately finds all classes implementing an interface or overriding an abstract method.                            |
| Java       | `jdtls`                                | Excellent — robustly resolves concrete class implementations and method overrides across large enterprise codebases.            |
| Rust       | `rust-analyzer`                        | Excellent — precisely locates `impl Trait for Type` blocks, crucial for understanding trait fulfillment.                        |
| Go         | `gopls`                                | Excellent — finds concrete types that satisfy an interface, even though Go interfaces are implicit and structural.              |
| C++        | `clangd`                               | Good — finds derived class method overrides, though C++'s complex inheritance hierarchies can sometimes yield multiple results. |
| Python     | `pyright` / `pylsp`                    | Good — finds subclasses overriding methods, though dynamic typing can sometimes limit static resolution accuracy.               |
| Emacs Lisp | N/A                                    | Not applicable — Elisp has no static interface/type system; `xref-find-definitions` is the canonical navigation.                |

### Integration with Existing Stack

The Go to Implementation surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `g i` to `textDocument/implementation`.
- **`consult`:** `consult-xref` intercepts the xref display functions to provide vertico-powered previews for ambiguous implementation targets.
- **`treesit`:** For non-LSP buffers (or when eglot is disconnected), `treesit` modes can provide fallback implementation navigation via `evil-textobj-tree-sitter` text objects combined with `imenu`.
- **`evil-collection`:** Standardizes `g i` across all major modes, ensuring Vim muscle memory is preserved.
- **`peek`:** Provides the inline overlay engine for "Peek Implementation" workflows, keeping the user's spatial context intact while inspecting concrete logic.
- **`breadcrumb`:** After jumping to an implementation, the breadcrumb bar immediately reflects the new enclosing class/struct context, maintaining spatial orientation.

---

## Find All References

_VS Code feature: Lists every usage of a symbol across the project in the References panel._

### Feature Overview

| Attribute          | Value                                                                                          |
| ------------------ | ---------------------------------------------------------------------------------------------- |
| Feature            | Find All References                                                                            |
| VS Code equivalent | Shift+F12 / "Find All References" centralized panel                                            |
| Status             | 🟢 editable in emacs 31 · `eglot` + `xref` + `consult-xref`                                    |
| Category           | Navigation & Code Jumping                                                                      |
| LSP methods        | `textDocument/references`                                                                      |
| Emacs routing      | `eglot` → `xref-find-references` → `consult-xref` (preview dropdown) OR native `*xref*` buffer |

### Implementation Stack

| Layer                | Component                                      | Role                                                                                                                                            |
| -------------------- | ---------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)                   | Queries `textDocument/references` and injects the location payloads into the native `xref` framework.                                           |
| Navigation Framework | `xref` (built-in)                              | Manages location abstraction, shared history ring, and cross-buffer jumping.                                                                    |
| Preview Engine       | `consult-xref` (bundled with consult)          | Intercepts `xref-show-xrefs-function` to render a vertico-powered dropdown with live buffer previews when multiple references exist.            |
| Bulk Mutation        | `xref-change-to-xref-edit-mode` (Emacs 31 NEW) | Transforms the `*xref*` buffer into a writable surface (Grep-Edit style), allowing simultaneous edits that propagate back to originating files. |

### Commands & Keybindings

| Action                   | Command                         | Keybinding               | Notes                                                                 |
| ------------------------ | ------------------------------- | ------------------------ | --------------------------------------------------------------------- |
| Find all references      | `xref-find-references`          | `M-?` / `SPC c D`        | Lists all usages; opens `consult-xref` dropdown or `*xref*` buffer.   |
| Go back (history)        | `xref-go-back`                  | `M-,` / `g ,`            | Returns to the exact cursor position before the reference jump.       |
| Edit references in place | `xref-change-to-xref-edit-mode` | `e` (in `*xref*` buffer) | _Emacs 31 NEW_ — enables writable reference buffer for bulk mutation. |
| Next reference           | `xref-next-line`                | `n` (in `*xref*` buffer) | Navigates down the reference list.                                    |
| Previous reference       | `xref-prev-line`                | `p` (in `*xref*` buffer) | Navigates up the reference list.                                      |
| Filter references        | `consult-xref`                  | `SPC c E` (via consult)  | Fuzzy-filters the reference list with live preview.                   |

### Configuration

`eglot` natively registers `eglot-xref-backend` when a buffer is LSP-managed. The configuration focuses on routing the xref payload through `consult` for live previews and registering ergonomic Evil/leader keybindings.

```elisp
;; ==========================================
;; 1. XREF & CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Route xref location prompts through Consult for live previews.
  ;; When a symbol has multiple references, Consult renders a Vertico dropdown
  ;; with live buffer previews instead of a static *xref* buffer split.
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
  "g D" #'xref-find-references  ;; Go to References (Shift+F12 parity)
  "g ," #'xref-go-back)         ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c D" '(xref-find-references :wk "Find all references"))
```

### Why This Approach (vs. `lsp-ui-references` / `lsp-mode`)

| Consideration       | `xref` + `consult-xref` (chosen)                                          | `lsp-ui-references` / `lsp-mode` (rejected)                       |
| ------------------- | ------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| LSP client coupling | Works with _any_ xref backend (eglot, dumb-jump, etags).                  | Hard-bound to the `lsp-mode` ecosystem.                           |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                    | Requires the forbidden `lsp-mode` ecosystem.                      |
| Bulk mutation       | Emacs 31 native `xref-change-to-xref-edit-mode` (Grep-Edit parity).       | Requires fragile third-party wrappers or manual text replacement. |
| Preview engine      | `consult-xref` leverages `vertico` for fuzzy filtering and live previews. | Custom child-frame pipeline with heavy rendering overhead.        |
| History tracking    | Native `xref` history ring (shared across all backends).                  | Fragmented history management.                                    |

### Behavioral Parity Matrix

| VS Code behavior                        | Emacs 31 equivalent                                                 |
| --------------------------------------- | ------------------------------------------------------------------- |
| `Shift+F12` lists all references        | `M-?` or `SPC c D` (`xref-find-references`)                         |
| Centralized References panel            | `*xref*` buffer or `consult-xref` vertico dropdown                  |
| Click reference to jump to file         | `RET` on any row in the `*xref*` buffer                             |
| Filter references by file/path          | Type filename in `consult-xref` or use native `/` filter in `*xref` |
| Edit multiple references simultaneously | Emacs 31 `xref-change-to-xref-edit-mode` (`e` in `*xref*` buffer)   |
| `Alt+Left` returns to origin            | `M-,` (`xref-go-back`) via the native xref history ring             |
| `Ctrl+Click` on symbol                  | Emacs 31 `global-xref-mouse-mode` (`C-<mouse-1>`)                   |

### Emacs 31 Specific Enhancements

- **`xref-change-to-xref-edit-mode` (NEW):** The most significant upgrade for reference management. Bound to `e` inside the `*xref*` buffer, it transforms the read-only list into a writable surface. You can perform bulk text replacements across all listed references simultaneously. Upon saving (`C-x C-s`), the edits propagate natively back to the originating source files, mirroring the Grep-Edit workflow without third-party packages like `wgrep`.
- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. Enabling `global-xref-mouse-mode` binds `C-<down-mouse-1>` to xref jumps, perfectly mirroring VS Code's Ctrl+Click convention for reference navigation.
- **`consult-xref` synergy:** Ambiguous references (e.g., a variable used 50 times across 10 files) are presented in a highly performant, searchable Vertico dropdown with instant buffer previews, eliminating the need to cycle through blind `*xref*` buffer splits.
- **Unified xref history:** `xref-go-back` (`M-,`) treats reference jumps identically to definition and implementation jumps — the entire navigation chain is preserved in a single ring, allowing seamless backtracking through complex architectural exploration sessions.

### Integration with Existing Stack

The Find All References surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Automatically registers `eglot-xref-backend` in `xref-backend-functions` for managed buffers, routing `M-?` to `textDocument/references`.
- **`consult`:** `consult-xref` intercepts the xref display functions to provide vertico-powered previews for ambiguous reference targets.
- **`evil-collection`:** Standardizes `[` / `]` or `g` motions across all major modes, ensuring Vim muscle memory is preserved when navigating the `*xref*` buffer.
- **`apheleia`:** If bulk edits are made via `xref-change-to-xref-edit-mode`, saving the modified files can automatically trigger `apheleia` to format the updated code, maintaining stylistic consistency.

---

## Peek Definition / Peek References

_VS Code feature: Shows the target inline in an expandable panel without switching editor tabs (Alt+F12 / Shift+Alt+F12)._

### Feature Overview

| Attribute          | Value                                                                          |
| ------------------ | ------------------------------------------------------------------------------ |
| Feature            | Peek Definition / Peek References                                              |
| VS Code equivalent | Inline expandable panel showing definition or references without tab switching |
| Status             | 🟢 working · `peek` (inline overlay) + `eglot` + `xref`                        |
| Category           | Navigation & Code Jumping                                                      |
| LSP methods        | `textDocument/definition`, `textDocument/references`                           |
| Emacs routing      | `eglot` → `xref` → `peek` (overlay rendering)                                  |

### Implementation Stack

| Layer                | Component                    | Role                                                                                                                                   |
| -------------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31) | Queries `textDocument/definition` or `textDocument/references` and routes payloads to `xref`.                                          |
| Navigation Framework | `xref` (built-in)            | Resolves the target location and manages the cross-referencing history.                                                                |
| Inline Rendering     | `peek` (SourceHut)           | Renders the target location inline below or above the cursor using native Emacs overlays, avoiding tab switches or heavy child-frames. |

### Commands & Keybindings

| Action                   | Command                 | Keybinding    | Notes                                                              |
| ------------------------ | ----------------------- | ------------- | ------------------------------------------------------------------ |
| Peek definition          | `peek-xref-definition`  | `SPC c p d`   | Shows the definition inline without leaving the current buffer.    |
| Peek references          | `peek-xref-references`  | `SPC c p r`   | Custom wrapper to show the first reference inline.                 |
| Hide peek view           | `peek-overlay-dwim`     | `SPC c p h`   | Toggles or hides the active peek overlay.                          |
| Standard jump (fallback) | `xref-find-definitions` | `M-.` / `g d` | Jumps to the target in a new buffer if inline peek is not desired. |

### Configuration

The `peek` package provides the underlying overlay engine. A custom wrapper is defined for references to mirror the definition behavior, ensuring a consistent inline navigation experience.

```elisp
;; ==========================================
;; PEEK (Inline Overlay Engine)
;; ==========================================
(use-package peek
  :ensure (peek :host sourcehut :repo "~meow_king/peek")
  :commands (peek-xref-definition peek-xref-references peek-overlay-dwim)
  :custom
  ;; Render the peek view below the cursor to avoid obscuring the current line.
  (peek-overlay-position 'below)
  ;; Distance in lines between the cursor and the peek overlay.
  (peek-overlay-distance 2)
  ;; Number of surrounding lines to include for context.
  (peek-definition-surrounding-above-lines 1)
  ;; Automatically update the peek view if the source buffer changes.
  (peek-live-update t)
  :config
  (global-peek-mode 1)

  ;; Custom wrapper for peeking references using the same overlay engine.
  (defun peek-goto-xref-references-func (identifier)
    "Go to the first reference of IDENTIFIER and clear history."
    (xref-find-references identifier)
    (when (car (xref--get-history))
      (pop (car (xref--get-history)))))

  (defun peek-xref-references ()
    "Peek xref references inline."
    (interactive)
    (peek-definition #'peek-goto-xref-references-func (list (thing-at-point 'symbol))))

  ;; Bindings for hiding the overlay.
  (general-define-key
   :states 'normal
   "SPC c p h" #'peek-overlay-dwim))
```

### Why This Approach (vs. `lsp-ui-peek` / Child-frames)

| Consideration       | `peek` (chosen)                                                                                                | `lsp-ui-peek` (rejected)                                                               |
| ------------------- | -------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| LSP client coupling | Agnostic; works seamlessly with `eglot` and native `xref`.                                                     | Hard-bound to the `lsp-mode` ecosystem.                                                |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                         | Requires the forbidden `lsp-mode` ecosystem.                                           |
| UI Physics          | Uses native `before-string`/`after-string` overlays, preserving the window tree and avoiding redisplay jitter. | Relies on heavy child-frame pipelines that can cause positioning bugs on Wayland/PGTK. |
| Terminal Support    | Overlays render correctly in TTY environments.                                                                 | Child-frames are strictly GUI-only and fail in terminals.                              |
| Performance         | Minimal main-thread overhead; defers to `xref` for heavy lifting.                                              | Known to cause micro-stutters during aggressive LSP polling.                           |

### Behavioral Parity Matrix

| VS Code behavior                    | Emacs 31 equivalent                                                                  |
| ----------------------------------- | ------------------------------------------------------------------------------------ |
| `Alt+F12` peeks definition inline   | `SPC c p d` (`peek-xref-definition`) renders the target below the cursor.            |
| `Shift+Alt+F12` peeks references    | `SPC c p r` (`peek-xref-references`) renders the first reference inline.             |
| Expandable panel without tab switch | `peek` uses buffer overlays, keeping the original buffer fully visible and active.   |
| Scroll within the peek panel        | Native Emacs scrolling (`C-n`/`C-p` or arrow keys) works within the overlay context. |
| Close peek panel                    | `SPC c p h` (`peek-overlay-dwim`) hides the overlay instantly.                       |
| Live update on source change        | `peek-live-update t` refreshes the overlay if the underlying code is modified.       |

### Emacs 31 Specific Enhancements

- **Refined `xref` Integration:** Emacs 31's mature `xref` API ensures that `peek` receives accurate, AST-aware location data from `eglot`, preventing the legacy regex-based misidentifications that plagued older Emacs versions.
- **Overlay Rendering Stability:** The `peek` package leverages Emacs' optimized text property engine. In Emacs 31, this integrates flawlessly with `treesit` fontification, ensuring that the peeked code retains full syntax highlighting without triggering expensive buffer-wide redisplay cycles.
- **TTY Parity:** Unlike child-frame-based peek implementations, `peek`'s overlay approach degrades gracefully to terminal emulators, ensuring inline navigation remains functional over SSH or in minimal environments.

### Integration with Existing Stack

- **`eglot`:** Supplies the LSP payloads for definitions and references, which are seamlessly consumed by `xref` and subsequently rendered by `peek`.
- **`xref`:** Acts as the central routing hub, ensuring that peek operations share the same history ring as standard `M-.` jumps, allowing `xref-go-back` (`M-,`) to return to the exact pre-peek state.
- **`general.el`:** Eagerly registers the `SPC c p` prefix bindings, providing a consistent, mnemonic access point for all peek operations without deferred-registration traps.
- **`evil-collection`:** Preserves Vim muscle memory by ensuring that standard navigation keys function predictably even when an overlay is active.

### Troubleshooting

#### Overlay Obscures Current Line

Adjust `peek-overlay-position` to `'above` or increase `peek-overlay-distance` to push the panel further away from the cursor, preventing visual collision with the active editing line.

#### Peek Shows Incorrect Location

Ensure `eglot` is actively connected and that the language server supports `textDocument/definition` or `textDocument/references`. Fallback to `M-.` to verify if the LSP server itself is returning inaccurate data.

#### Multiple References Not Shown

The `peek-xref-references` wrapper is designed to show the first reference inline to maintain the "peek" paradigm. For a comprehensive list of all references, use the standard `xref-find-references` (`M-?` or `SPC c D`), which leverages `consult-xref` for a searchable, multi-candidate dropdown.

---

## Call Hierarchy

_VS Code feature: Tree view of what calls a function and what it calls (Shift+Alt+H)._

### Feature Overview

| Attribute          | Value                                                                                             |
| ------------------ | ------------------------------------------------------------------------------------------------- |
| Feature            | Call Hierarchy                                                                                    |
| VS Code equivalent | "Show Call Hierarchy" (incoming/outgoing calls tree view)                                         |
| Status             | 🟢 working · native `eglot`                                                                       |
| Category           | Navigation & Code Jumping                                                                         |
| LSP methods        | `textDocument/prepareCallHierarchy`, `callHierarchy/incomingCalls`, `callHierarchy/outgoingCalls` |
| Emacs routing      | `eglot` → `eglot-show-call-hierarchy` → interactive `*eglot-hierarchy*` buffer                    |

### Implementation Stack

| Layer              | Component                        | Role                                                                                                   |
| ------------------ | -------------------------------- | ------------------------------------------------------------------------------------------------------ |
| LSP Client         | `eglot` (built-in, Emacs 31)     | Drives the `callHierarchy/*` protocol methods and formats the response.                                |
| Hierarchy Renderer | `eglot-show-call-hierarchy`      | Native Eglot command that pops up a special buffer showing an interactive tree of callers and callees. |
| Navigation         | `tabulated-list` / hierarchy API | Provides expandable/collapsible nodes and direct jumping to source locations.                          |

### Commands & Keybindings

| Action               | Command                          | Keybinding                | Notes                                                                 |
| -------------------- | -------------------------------- | ------------------------- | --------------------------------------------------------------------- |
| Show call hierarchy  | `eglot-show-call-hierarchy`      | `SPC c h` / `g c`         | Opens the interactive caller/callee tree for the symbol at point.     |
| Show type hierarchy  | `eglot-show-type-hierarchy`      | `SPC c t` / `g T`         | Opens the interactive supertype/subtype tree for the symbol at point. |
| Center on node       | `eglot-hierarchy-center-on-node` | `c` (in hierarchy buffer) | Recenters the tree view on the current node.                          |
| Expand/Collapse node | `tabulated-list` native          | `TAB` / `RET`             | Toggles the visibility of child nodes in the hierarchy tree.          |
| Jump to definition   | `xref-find-definitions`          | `RET` (on node)           | Jumps to the selected caller/callee definition in the source buffer.  |

### Configuration

`eglot` natively provides the call and type hierarchy commands. No third-party packages are required. The configuration focuses on ergonomic keybindings via `general.el`.

```elisp
;; ==========================================
;; EGLOT HIERARCHY (Built-in)
;; ==========================================
;; eglot natively provides `eglot-show-call-hierarchy` and
;; `eglot-show-type-hierarchy` for interactive tree exploration.
;; No explicit configuration is needed beyond the base `eglot` setup.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c h" '(eglot-show-call-hierarchy :wk "Call hierarchy")
  "c t" '(eglot-show-type-hierarchy :wk "Type hierarchy"))

(general-define-key
  :states 'motion
  "g c" #'eglot-show-call-hierarchy
  "g T" #'eglot-show-type-hierarchy)
```

### Why This Approach (vs. `lsp-mode` / third-party hierarchy packages)

| Consideration       | `eglot` native (chosen)                                                                | `lsp-mode` / `eglot-hierarchy` (rejected)                                 |
| ------------------- | -------------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with built-in `eglot`.                                               | Third-party `eglot-hierarchy` is obsolete; `lsp-mode` is forbidden.       |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                 | Requires forbidden `lsp-mode` ecosystem or deprecated external packages.  |
| Emacs 31 synergy    | `eglot-show-call-hierarchy` is a native command added to Eglot in late 2025.           | External packages duplicate functionality now present in Emacs core.      |
| Performance         | Renders directly via Eglot's optimized hierarchy API without extra abstraction layers. | Legacy packages introduce unnecessary indirection and maintenance burden. |

### Behavioral Parity Matrix

| VS Code behavior                      | Emacs 31 equivalent                                                                 |
| ------------------------------------- | ----------------------------------------------------------------------------------- |
| `Shift+Alt+H` opens call hierarchy    | `SPC c h` or `g c` invokes `eglot-show-call-hierarchy`.                             |
| Tree view of incoming calls (callers) | Native Eglot hierarchy buffer displays "Incoming Calls" expandable nodes.           |
| Tree view of outgoing calls (callees) | Native Eglot hierarchy buffer displays "Outgoing Calls" expandable nodes.           |
| Click node to jump to source          | `RET` on a hierarchy node triggers `xref-find-definitions` to jump to the location. |
| Expand/collapse tree branches         | `TAB` or `RET` on parent nodes toggles child visibility.                            |
| Center view on current symbol         | `eglot-hierarchy-center-on-node` recenters the tree on the active symbol.           |

### Emacs 31 Specific Enhancements

- **Native Hierarchy Commands:** As of late 2025, `eglot-show-call-hierarchy` and `eglot-show-type-hierarchy` are fully integrated into Eglot, popping up a special buffer showing an interactive tree which represents a hierarchy of sub- and super-calls. This eliminates the need for external packages like `eglot-hierarchy`.
- **Interactive Tree Navigation:** The hierarchy buffer leverages Emacs' native `tabulated-list` and hierarchy APIs, providing smooth, keyboard-driven expansion and contraction of caller/callee trees without relying on fragile third-party UI overlays.
- **Seamless `xref` Integration:** Selecting a node in the call hierarchy seamlessly delegates to `xref-find-definitions`, preserving the shared `xref` history ring so you can easily return to your original location with `xref-go-back` (`M-,`).

### Integration with Existing Stack

- **`eglot`:** Natively handles the `textDocument/prepareCallHierarchy` and subsequent `incomingCalls`/`outgoingCalls` requests, formatting the LSP response into an Emacs-friendly hierarchy structure.
- **`xref`:** The hierarchy buffer uses `xref` under the hood for node navigation, ensuring that jumping to a caller/callee integrates perfectly with the global `xref-go-back` history.
- **`general.el`:** Eagerly registers the `SPC c h` and `g c` leader bindings, ensuring the command is instantly available in all `eglot`-managed buffers without deferred-registration traps.
- **`consult`:** While the hierarchy is displayed in a dedicated buffer, users can still fall back to `consult-eglot-symbols` for flat, fuzzy-filtered workspace-wide symbol searches if the tree view becomes too deep.

---

## Type Hierarchy

_VS Code feature: Tree view of a type's supertypes and subtypes._

### Feature Overview

| Attribute          | Value                                                                                     |
| ------------------ | ----------------------------------------------------------------------------------------- |
| Feature            | Type Hierarchy                                                                            |
| VS Code equivalent | "Show Type Hierarchy" (supertypes/subtypes tree view)                                     |
| Status             | 🟢 working · native `eglot`                                                               |
| Category           | Navigation & Code Jumping                                                                 |
| LSP methods        | `textDocument/prepareTypeHierarchy`, `typeHierarchy/supertypes`, `typeHierarchy/subtypes` |
| Emacs routing      | `eglot` → `eglot-show-type-hierarchy` → interactive `*eglot-hierarchy*` buffer            |

### Implementation Stack

| Layer              | Component                        | Role                                                                                                       |
| ------------------ | -------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| LSP Client         | `eglot` (built-in, Emacs 31)     | Drives the `typeHierarchy/*` protocol methods and formats the response.                                    |
| Hierarchy Renderer | `eglot-show-type-hierarchy`      | Native Eglot command that pops up a special buffer showing an interactive tree of supertypes and subtypes. |
| Navigation         | `tabulated-list` / hierarchy API | Provides expandable/collapsible nodes and direct jumping to source locations.                              |

### Commands & Keybindings

| Action               | Command                          | Keybinding                | Notes                                                                 |
| -------------------- | -------------------------------- | ------------------------- | --------------------------------------------------------------------- |
| Show type hierarchy  | `eglot-show-type-hierarchy`      | `SPC c t` / `g T`         | Opens the interactive supertype/subtype tree for the symbol at point. |
| Show call hierarchy  | `eglot-show-call-hierarchy`      | `SPC c h` / `g c`         | Opens the interactive caller/callee tree for the symbol at point.     |
| Center on node       | `eglot-hierarchy-center-on-node` | `c` (in hierarchy buffer) | Recenters the tree view on the current node.                          |
| Expand/Collapse node | `tabulated-list` native          | `TAB` / `RET`             | Toggles the visibility of child nodes in the hierarchy tree.          |
| Jump to definition   | `xref-find-definitions`          | `RET` (on node)           | Jumps to the selected type definition in the source buffer.           |

### Configuration

`eglot` natively provides the type and call hierarchy commands. No third-party packages are required. The configuration focuses on ergonomic keybindings via `general.el`.

```elisp
;; ==========================================
;; EGLOT HIERARCHY (Built-in)
;; ==========================================
;; eglot natively provides `eglot-show-type-hierarchy` and
;; `eglot-show-call-hierarchy` for interactive tree exploration.
;; No explicit configuration is needed beyond the base `eglot` setup.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c h" '(eglot-show-call-hierarchy :wk "Call hierarchy")
  "c t" '(eglot-show-type-hierarchy :wk "Type hierarchy"))

(general-define-key
  :states 'motion
  "g c" #'eglot-show-call-hierarchy
  "g T" #'eglot-show-type-hierarchy)
```

### Why This Approach (vs. `lsp-mode` / third-party hierarchy packages)

| Consideration       | `eglot` native (chosen)                                                                                              | `lsp-mode` / `eglot-hierarchy` (rejected)                                 |
| ------------------- | -------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with built-in `eglot`.                                                                             | Third-party `eglot-hierarchy` is obsolete; `lsp-mode` is forbidden.       |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                               | Requires forbidden `lsp-mode` ecosystem or deprecated external packages.  |
| Emacs 31 synergy    | `eglot-show-type-hierarchy` is a native command added to Eglot, leveraging the latest LSP 3.17+ type hierarchy spec. | External packages duplicate functionality now present in Emacs core.      |
| Performance         | Renders directly via Eglot's optimized hierarchy API without extra abstraction layers.                               | Legacy packages introduce unnecessary indirection and maintenance burden. |

### Behavioral Parity Matrix

| VS Code behavior                      | Emacs 31 equivalent                                                                 |
| ------------------------------------- | ----------------------------------------------------------------------------------- |
| "Show Type Hierarchy" opens tree view | `SPC c t` or `g T` invokes `eglot-show-type-hierarchy`.                             |
| Tree view of supertypes (parents)     | Native Eglot hierarchy buffer displays "Supertypes" expandable nodes.               |
| Tree view of subtypes (children)      | Native Eglot hierarchy buffer displays "Subtypes" expandable nodes.                 |
| Click node to jump to source          | `RET` on a hierarchy node triggers `xref-find-definitions` to jump to the location. |
| Expand/collapse tree branches         | `TAB` or `RET` on parent nodes toggles child visibility.                            |
| Center view on current symbol         | `eglot-hierarchy-center-on-node` recenters the tree on the active symbol.           |

### Emacs 31 Specific Enhancements

- **Native Hierarchy Commands:** `eglot-show-type-hierarchy` is fully integrated into Eglot, popping up a special buffer showing an interactive tree which represents a hierarchy of super- and sub-types, leveraging the LSP 3.17+ `typeHierarchy/*` methods.
- **Interactive Tree Navigation:** The hierarchy buffer leverages Emacs' native `tabulated-list` and hierarchy APIs, providing smooth, keyboard-driven expansion and contraction of type trees without relying on fragile third-party UI overlays.
- **Seamless `xref` Integration:** Selecting a node in the type hierarchy seamlessly delegates to `xref-find-definitions`, preserving the shared `xref` history ring so you can easily return to your original location with `xref-go-back` (`M-,`).

### Integration with Existing Stack

- **`eglot`:** Natively handles the `textDocument/prepareTypeHierarchy` and subsequent `supertypes`/`subtypes` requests, formatting the LSP response into an Emacs-friendly hierarchy structure.
- **`xref`:** The hierarchy buffer uses `xref` under the hood for node navigation, ensuring that jumping to a type integrates perfectly with the global `xref-go-back` history.
- **`general.el`:** Eagerly registers the `SPC c t` and `g T` leader bindings, ensuring the command is instantly available in all `eglot`-managed buffers without deferred-registration traps.
- **`consult`:** While the hierarchy is displayed in a dedicated buffer, users can still fall back to `consult-eglot-symbols` for flat, fuzzy-filtered workspace-wide symbol searches if the tree view becomes too deep.

---

## Moniker

_VS Code feature: Cross-repository symbol identity, mainly used for large-scale/indexed code navigation._

### Feature Overview

| Attribute          | Value                                                                                                                |
| ------------------ | -------------------------------------------------------------------------------------------------------------------- |
| Feature            | Moniker                                                                                                              |
| VS Code equivalent | Background protocol feature enabling external indexers (e.g., Sourcegraph, LSIF) to link symbols across repositories |
| Status             | 🟢 working · native `eglot` protocol negotiation                                                                     |
| Category           | Navigation & Code Jumping                                                                                            |
| LSP methods        | `textDocument/moniker`                                                                                               |
| Emacs routing      | `eglot` → custom inspection command (no native UI, as monikers target external tools)                                |

### Implementation Stack

| Layer              | Component                          | Role                                                                                                                                                |
| ------------------ | ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client         | `eglot` (built-in, Emacs 31)       | Natively negotiates the `monikerProvider` capability during initialization if the language server advertises it (LSP 3.16+).                        |
| Inspection Tool    | Custom `ar/eglot-moniker-at-point` | A lightweight Elisp utility to query and display the resolved moniker strings (e.g., `npm:lodash:4.17.21:map`) for debugging or advanced workflows. |
| External Consumers | LSIF / Sourcegraph extensions      | Monikers are primarily designed to be consumed by external indexing tools, not for direct local editor navigation.                                  |

### Commands & Keybindings

| Action                    | Command                     | Keybinding | Notes                                                                                         |
| ------------------------- | --------------------------- | ---------- | --------------------------------------------------------------------------------------------- |
| Inspect moniker at point  | `ar/eglot-moniker-at-point` | `SPC c m`  | Queries the LSP server for the symbol's cross-repo identity and displays it in the echo area. |
| Copy moniker to kill-ring | `ar/eglot-copy-moniker`     | `SPC c M`  | Copies the primary moniker string for use in external search tools (e.g., Sourcegraph).       |

### Configuration

While `eglot` handles the protocol negotiation automatically, Emacs does not have a built-in "cross-repo navigator" for monikers. The configuration below provides a minimal, native utility to inspect moniker data when needed, adhering strictly to the `eglot`-only mandate.

```elisp
;; ==========================================
;; EGLOT MONIKER INSPECTION (LSP 3.16+)
;; ==========================================
;; Eglot natively negotiates the `monikerProvider` capability.
;; This custom utility allows users to inspect the resolved moniker strings
;; for debugging or integration with external indexers (e.g., Sourcegraph).

(defun ar/eglot-moniker-at-point ()
  "Request and display the LSP moniker for the symbol at point."
  (interactive)
  (let ((server (eglot-current-server)))
    (if (not server)
        (user-error "No active Eglot server")
      (eglot--execute-request
       server
       "textDocument/moniker"
       (eglot--TextDocumentPositionParams)
       (lambda (result)
         (if (and result (cl-plusp (length result)))
             (let* ((primary (car result))
                    (scheme (plist-get primary :scheme))
                    (identifier (plist-get primary :identifier))
                    (kind (plist-get primary :kind)))
               (message "Moniker [%s]: %s (Kind: %s)" scheme identifier kind))
           (message "No moniker found for symbol at point.")))))))

(defun ar/eglot-copy-moniker ()
  "Copy the primary moniker identifier of the symbol at point to the kill-ring."
  (interactive)
  (let ((server (eglot-current-server)))
    (if (not server)
        (user-error "No active Eglot server")
      (eglot--execute-request
       server
       "textDocument/moniker"
       (eglot--TextDocumentPositionParams)
       (lambda (result)
         (if (and result (cl-plusp (length result)))
             (let ((identifier (plist-get (car result) :identifier)))
               (kill-new identifier)
               (message "Copied moniker: %s" identifier))
           (message "No moniker found to copy.")))))))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c m" '(ar/eglot-moniker-at-point :wk "Inspect moniker")
  "c M" '(ar/eglot-copy-moniker :wk "Copy moniker"))
```

### Why This Approach (vs. `lsp-mode` / third-party indexers)

| Consideration         | `eglot` native + custom utility (chosen)                                                                                            | `lsp-mode` (rejected)                                                                        |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| LSP client coupling   | Works exclusively with built-in `eglot`.                                                                                            | Hard-bound to the `lsp-mode` ecosystem.                                                      |
| Protocol compliance   | Honors the `eglot`-only stack mandate.                                                                                              | Requires forbidden `lsp-mode` ecosystem.                                                     |
| Architectural honesty | Acknowledges that monikers are for _external_ indexers, providing a lightweight inspection tool rather than a fake "navigation" UI. | Often bundles heavy, unnecessary UI wrappers for protocol features meant for external tools. |
| Performance           | Zero overhead; the custom function only executes on explicit user request.                                                          | Background indexing and heavy UI wrappers can cause main-thread blocking.                    |

### Behavioral Parity Matrix

| VS Code behavior                          | Emacs 31 equivalent                                                                                          |
| ----------------------------------------- | ------------------------------------------------------------------------------------------------------------ |
| Server advertises `monikerProvider`       | `eglot` natively detects and stores this capability during initialization.                                   |
| External tools (Sourcegraph) use monikers | Emacs acts as a passive participant; the LSP server provides the data to external indexers.                  |
| Inspect symbol identity                   | `SPC c m` (`ar/eglot-moniker-at-point`) displays the scheme, identifier, and kind in the echo area.          |
| Copy identity for external search         | `SPC c M` (`ar/eglot-copy-moniker`) copies the identifier (e.g., `npm:lodash:4.17.21:map`) to the kill-ring. |
| No local "jump" action                    | Correctly omitted, as monikers represent _cross-repository_ identities, not local file paths.                |

### Emacs 31 Specific Enhancements

- **LSP 3.16+ Native Support:** Emacs 31's `eglot` fully supports the LSP 3.16 specification, including the `textDocument/moniker` request. If a language server (like `rust-analyzer` or `clangd`) advertises `monikerProvider`, `eglot` will successfully route the request without requiring third-party patches.
- **`eglot--execute-request` Stability:** The custom utility leverages `eglot`'s stable internal request execution API, ensuring that asynchronous JSON-RPC responses are handled cleanly without blocking the main thread or corrupting the editor state.
- **Echo Area Integration:** By displaying the moniker in the echo area (or copying it), the implementation respects Emacs' minimalist philosophy, avoiding the creation of heavy, unnecessary child-frames or sidebars for a feature primarily designed for machine-to-machine indexer communication.

### Integration with Existing Stack

- **`eglot`:** Manages the lifecycle of the LSP connection and capability negotiation, ensuring `textDocument/moniker` is only called if the server explicitly supports it.
- **`general.el`:** Eagerly registers the `SPC c m` and `SPC c M` leader bindings, ensuring the inspection commands are instantly available in all `eglot`-managed buffers without deferred-registration traps.
- **External Workflows:** The copied moniker string can be seamlessly pasted into external tools like Sourcegraph (`sourcegraph.com/search?q=context:global+<moniker>`) for true cross-repository navigation, bridging the gap between local Emacs editing and global codebase exploration.

---


---

# Symbols & Diagnostics

## Document Symbols / Outline View

_VS Code feature: Tree view of classes/functions/variables in the current file; also powers the breadcrumb dropdown._

### Feature Overview

| Attribute          | Value                                                                                 |
| ------------------ | ------------------------------------------------------------------------------------- |
| Feature            | Document Symbols / Outline View                                                       |
| VS Code equivalent | Outline sidebar, `Ctrl+Shift+O` (Go to Symbol in File), and top breadcrumb bar        |
| Status             | 🟢 working · `eglot` + `consult` + `breadcrumb`                                       |
| Category           | Diagnostics & Symbols                                                                 |
| LSP methods        | `textDocument/documentSymbol`                                                         |
| Emacs routing      | `eglot` → `imenu` / `consult-eglot-symbols` (minibuffer) + `breadcrumb` (breadcrumbs) |

### Implementation Stack

| Layer             | Component                    | Role                                                                                                     |
| ----------------- | ---------------------------- | -------------------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31) | Queries `textDocument/documentSymbol` and maps the hierarchical response to Emacs' native `imenu` index. |
| Outline Engine    | `consult-eglot`              | Renders a live-preview, fuzzy-filtered outline tree in the minibuffer using `vertico` and `orderless`.   |
| Breadcrumb Engine | `breadcrumb` (GNU ELPA)      | Displays a clickable, hierarchical path (e.g., `file › class › method`) in the header line.              |
| Fallback Parser   | `treesit` (built-in)         | Provides native AST-based `imenu` generation if the LSP server is slow or disconnected.                  |

### Commands & Keybindings

| Action                    | Command                              | Keybinding    | Notes                                                      |
| ------------------------- | ------------------------------------ | ------------- | ---------------------------------------------------------- |
| Go to symbol in file      | `consult-eglot-symbols`              | `SPC c s`     | Opens a vertico-powered, preview-enabled outline dropdown. |
| Go to symbol in workspace | `consult-eglot-symbols` (with `C-u`) | `C-u SPC c s` | Searches across the entire project via `workspace/symbol`. |
| Toggle breadcrumbs        | `breadcrumb-mode`                    | `SPC t b`     | Enables the clickable path bar at the top of the buffer.   |
| Native imenu jump         | `imenu`                              | `M-g M-i`     | Fallback to native Emacs imenu if LSP is unavailable.      |

### Configuration

```elisp
;; ==========================================
;; 1. BREADCRUMB (Header-line Breadcrumbs)
;; ==========================================
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode)
  :custom
  ;; Optional: Customize the separator for breadcrumbs
  (breadcrumb-imenu-crumb-separator " › ")
  (breadcrumb-project-crumb-separator " / "))

;; ==========================================
;; 2. CONSULT-EGLOT (Outline & Workspace Symbols)
;; ==========================================
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (("M-g s" . consult-eglot-symbols)      ; Go to symbol in file
         ("M-g S" . consult-eglot-symbols))     ; With C-u, goes to workspace symbol
  :config
  ;; Ensure consult-eglot uses the current project root for workspace symbols
  (setq consult-eglot-symbols-kind nil))       ; nil = all kinds, or filter like '(class function)

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c s" '(consult-eglot-symbols :wk "Document symbols (outline)")
  "t" '(:ignore t :wk "toggle")
  "t b" '(breadcrumb-mode :wk "Toggle breadcrumbs"))
```

### Why This Approach (vs. `lsp-ui` / `lsp-mode`)

| Consideration        | `eglot` + `consult` (chosen)                                                           | `lsp-ui` / `lsp-mode` (rejected)                                    |
| -------------------- | -------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| LSP client coupling  | Works exclusively with `eglot` and native `imenu`.                                     | Hard-bound to the `lsp-mode` ecosystem.                             |
| Protocol compliance  | Honors the `eglot`-only stack mandate.                                                 | Requires forbidden `lsp-mode` ecosystem.                            |
| Preview engine       | `consult` provides live, asynchronous buffer previews while scrolling the symbol tree. | `lsp-ui` uses heavy, custom child-frame rendering that can stutter. |
| Emacs 31 synergy     | Leverages Emacs 31's enhanced `treesit` imenu integration as a seamless fallback.      | No integration with Emacs 31 core enhancements.                     |
| Breadcrumb rendering | Native `header-line-format` via `breadcrumb`, zero third-party dependencies.           | Requires `lsp-mode`'s custom breadcrumb implementation.             |

### Behavioral Parity Matrix

| VS Code behavior                        | Emacs 31 equivalent                                                               |
| --------------------------------------- | --------------------------------------------------------------------------------- |
| `Ctrl+Shift+O` opens file outline       | `SPC c s` (`consult-eglot-symbols`) opens minibuffer outline with live preview.   |
| Fuzzy filter symbols by name            | `consult` + `orderless` allows space-separated, out-of-order fuzzy matching.      |
| Click symbol to jump to definition      | `RET` in `consult` dropdown or `mouse-1` on breadcrumb segments.                  |
| Top bar shows `file › class › method`   | `breadcrumb-mode` renders this exact hierarchy in the header line.                |
| Workspace-wide symbol search (`Ctrl+T`) | `C-u SPC c s` or `SPC s w` triggers `workspace/symbol` via `consult`.             |
| Icons for classes/functions in outline  | `nerd-icons-completion` automatically injects glyphs into the `consult` dropdown. |

### Emacs 31 Specific Enhancements

- **`treesit-aggregated-simple-imenu-settings`:** Emacs 31 introduces native support for multi-language imenu trees. For mixed-language buffers (e.g., `mhtml-ts-mode`, `php-ts-mode`), the outline view seamlessly aggregates symbols from HTML, CSS, and PHP tree-sitter parsers without relying solely on the LSP server.
- **Enhanced `breadcrumb` integration:** The breadcrumb bar in Emacs 31 is more robust, correctly handling deep nesting and long symbol names by truncating gracefully or allowing horizontal scrolling within the header line.
- **Native Fallback Parity:** If the LSP server crashes or is slow to respond, Emacs 31's `treesit` automatically populates the `imenu` index, ensuring `consult-eglot-symbols` (which falls back to `imenu`) still provides a highly accurate, AST-aware outline view with zero network latency.

### Integration with Existing Stack

- **`eglot`:** Natively maps `textDocument/documentSymbol` responses to the buffer-local `imenu-create-index-function`.
- **`consult`:** `consult-eglot-symbols` intercepts the `imenu` index, transforming it into a searchable, preview-enabled Vertico menu.
- **`orderless`:** Provides the fuzzy matching engine, allowing queries like `init conf` to instantly find `initialize_configuration`.
- **`nerd-icons-completion`:** Injects visual glyphs (e.g., 🏛️ for classes, ⚙️ for functions) into the `consult` dropdown, matching the VS Code outline sidebar aesthetic.
- **`breadcrumb`:** Provides the persistent, clickable breadcrumb trail at the top of the window, updating dynamically as the cursor moves through different scopes.

---

## Workspace Symbol Search

_VS Code feature: Ctrl+T "Go to Symbol in Workspace" — fuzzy search symbols across the whole project._

### Feature Overview

| Attribute          | Value                                                                     |
| ------------------ | ------------------------------------------------------------------------- |
| Feature            | Workspace Symbol search                                                   |
| VS Code equivalent | `Ctrl+T` "Go to Symbol in Workspace"                                      |
| Status             | 🟢 working · `eglot` + `consult-eglot` + `vertico`                        |
| Category           | Diagnostics & Symbols                                                     |
| LSP methods        | `workspace/symbol`                                                        |
| Emacs routing      | `eglot` → `consult-eglot-symbols` (with prefix) → `vertico` + `orderless` |

### Implementation Stack

| Layer            | Component                    | Role                                                                                                  |
| ---------------- | ---------------------------- | ----------------------------------------------------------------------------------------------------- |
| LSP Client       | `eglot` (built-in, Emacs 31) | Queries `workspace/symbol` and returns a flat list of project-wide symbol candidates.                 |
| Preview Engine   | `consult-eglot-symbols`      | Intercepts the LSP payload and renders it in the minibuffer with live, asynchronous buffer previews.  |
| Filtering Engine | `orderless`                  | Provides space-separated, out-of-order fuzzy matching (e.g., typing `usr cnt` matches `UserContext`). |
| UI Renderer      | `vertico`                    | Displays the filtered candidates in a clean, vertically scrolling list with marginalia annotations.   |

### Commands & Keybindings

| Action                     | Command                   | Keybinding                    | Notes                                                                                          |
| -------------------------- | ------------------------- | ----------------------------- | ---------------------------------------------------------------------------------------------- |
| Workspace symbol search    | `consult-eglot-symbols`   | `C-u SPC s w`                 | Prefix argument (`C-u`) forces workspace-wide search instead of buffer-local.                  |
| Buffer-local symbol search | `consult-eglot-symbols`   | `SPC c s`                     | Default behavior (no prefix) searches only the current file via `textDocument/documentSymbol`. |
| Built-in workspace search  | `eglot-workspace-symbols` | `M-x eglot-workspace-symbols` | Fallback native command without live preview.                                                  |
| Apropos search (fallback)  | `xref-find-apropos`       | `SPC c A`                     | Searches all registered xref backends (including eglot) for a regex pattern.                   |

### Configuration

The `consult-eglot` package natively distinguishes between buffer-local and workspace-wide symbol searches based on the presence of a prefix argument. No complex custom wrappers are required.

```elisp
;; ==========================================
;; CONSULT-EGLOT (Outline & Workspace Symbols)
;; ==========================================
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (("M-g s" . consult-eglot-symbols)      ; Go to symbol in file
         ("M-g S" . consult-eglot-symbols))     ; With C-u, goes to workspace symbol
  :config
  ;; Ensure consult-eglot uses the current project root for workspace symbols
  (setq consult-eglot-symbols-kind nil))       ; nil = all kinds, or filter like '(class function)

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "s" '(:ignore t :wk "search")
  "s w" '(consult-eglot-symbols :wk "Workspace symbols (use C-u)"))
```

Note: To explicitly trigger the workspace search without remembering the prefix, you can bind a dedicated wrapper:

```elisp
(defun ar/consult-eglot-workspace-symbols ()
  "Force workspace-wide symbol search via consult-eglot."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'consult-eglot-symbols)))

(ar/global-leader
  "s W" '(ar/consult-eglot-workspace-symbols :wk "Workspace symbols (force)"))
```

### Why This Approach (vs. `lsp-ui` / `lsp-mode`)

| Consideration       | `eglot` + `consult` (chosen)                                                                                | `lsp-ui` / `lsp-mode` (rejected)                                    |
| ------------------- | ----------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot` and native `xref`.                                                           | Hard-bound to the `lsp-mode` ecosystem.                             |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                      | Requires forbidden `lsp-mode` ecosystem.                            |
| Preview engine      | `consult` provides live, asynchronous buffer previews while scrolling the symbol tree.                      | `lsp-ui` uses heavy, custom child-frame rendering that can stutter. |
| Filtering           | Integrates seamlessly with `orderless` for out-of-order fuzzy matching.                                     | Requires custom matchers; struggles with space-separated queries.   |
| Emacs 31 synergy    | Leverages Emacs 31's enhanced `treesit` imenu integration as a seamless fallback if the LSP server is slow. | No integration with Emacs 31 core enhancements.                     |

### Behavioral Parity Matrix

| VS Code behavior                        | Emacs 31 equivalent                                                                      |
| --------------------------------------- | ---------------------------------------------------------------------------------------- |
| `Ctrl+T` opens workspace symbol search  | `C-u SPC s w` or `C-u M-g S` triggers workspace-wide `consult-eglot-symbols`.            |
| Fuzzy filter symbols by name            | `orderless` matching styles allow space-separated, out-of-order fuzzy matching.          |
| Live preview of symbol definition       | `consult` temporarily visits the file and shows the definition context in a side window. |
| Filter by symbol kind (class, function) | `consult-eglot-symbols-kind` can be set to filter specific LSP symbol kinds.             |
| Click/Enter to jump to target           | `RET` in `consult` dropdown jumps to the exact location via `xref`.                      |
| Icons for classes/functions in list     | `nerd-icons-completion` automatically injects glyphs into the `consult` dropdown.        |

### Emacs 31 Specific Enhancements

- **`consult-eglot` Prefix Intelligence:** The `consult-eglot-symbols` command natively checks `(called-interactively-p 'any)` and the `current-prefix-arg`. If a prefix is present, it routes the query to `workspace/symbol`; otherwise, it falls back to the faster, buffer-local `textDocument/documentSymbol`. This eliminates the need for two separate commands.
- **`treesit` Fallback Parity:** If the LSP server is disconnected or slow to respond to `workspace/symbol`, `consult-imenu-multi` can be used as a zero-latency, AST-aware fallback that searches across all open project buffers using Emacs 31's native `treesit-aggregated-simple-imenu-settings`.
- **Optimized Preview Debouncing:** The `consult-customize` block (configured in the main `consult` setup) applies a `:debounce 0.4` to `consult-eglot-symbols`, preventing the LSP server from being spammed with file-read requests while rapidly scrolling through hundreds of workspace candidates.

### Integration with Existing Stack

- **`eglot`:** Natively implements `workspace/symbol` and formats the response into a structure that `consult` can easily parse.
- **`consult`:** `consult-eglot-symbols` intercepts the payload, sorts it, and provides the live preview via `consult--buffer-preview`.
- **`orderless`:** Provides the fuzzy matching engine, allowing queries like `init conf` to instantly find `initialize_configuration` across the entire codebase.
- **`nerd-icons-completion`:** Injects visual glyphs (e.g., 🏛️ for classes, ⚙️ for functions) into the `consult` dropdown, matching the VS Code outline sidebar aesthetic.
- **`marginalia`:** Appends the file path and line number to each candidate, providing crucial spatial context before jumping.

---

## Diagnostics (Push Model)

_VS Code feature: Server pushes errors/warnings; shown as squiggles and in the Problems panel._

### Feature Overview

| Attribute          | Value                                                                            |
| ------------------ | -------------------------------------------------------------------------------- |
| Feature            | Diagnostics (Push Model)                                                         |
| VS Code equivalent | Red/green squiggles in the editor and a centralized, filterable "Problems" panel |
| Status             | 🟢 emacs 31 · native `flymake` + `eglot` · no `flyover` required                 |
| Category           | Diagnostics & Symbols                                                            |
| LSP methods        | `textDocument/publishDiagnostics`                                                |
| Emacs routing      | `eglot` → `flymake` → `consult-flymake` / native `*Flymake diagnostics*` buffer  |

### Implementation Stack

| Layer             | Component                                 | Role                                                                                                                                          |
| ----------------- | ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31)              | Receives `textDocument/publishDiagnostics` payloads from the language server and translates them into native `flymake` diagnostics.           |
| Diagnostic Engine | `flymake` (built-in)                      | Manages the lifecycle of diagnostics, rendering squiggles in the buffer and aggregating them into tabulated lists.                            |
| Inline Rendering  | `flymake-show-diagnostics-at-end-of-line` | Emacs 31 native feature that lays out diagnostic messages below the affected line using Unicode graphics, eliminating the need for `flyover`. |
| Fuzzy Filtering   | `consult-flymake`                         | Provides a Vertico-powered, live-preview dropdown for rapidly searching and jumping to diagnostics across the buffer or project.              |

### Commands & Keybindings

| Action                     | Command                           | Keybinding       | Notes                                                                     |
| -------------------------- | --------------------------------- | ---------------- | ------------------------------------------------------------------------- |
| Buffer diagnostics list    | `flymake-show-buffer-diagnostics` | `SPC c e`        | Opens a tabulated list of errors/warnings for the current file.           |
| Project diagnostics search | `consult-flymake-project`         | `SPC c E`        | Fuzzy-searches all diagnostics across the entire workspace.               |
| Next error                 | `flymake-goto-next-error`         | `SPC c n` / `]e` | Jumps to the next diagnostic in the current buffer.                       |
| Previous error             | `flymake-goto-prev-error`         | `SPC c p` / `[e` | Jumps to the previous diagnostic in the current buffer.                   |
| Force recheck              | `flymake-start`                   | `SPC c !`        | Manually triggers a diagnostic refresh (useful for pull-model fallbacks). |

### Configuration

```elisp
;; ==========================================
;; 1. FLYMAKE CORE (Emacs 31 Native Diagnostics)
;; ==========================================
(use-package flymake
  :ensure nil
  :custom
  ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line,
  ;; effectively replacing the `flyover` package with richer native output.
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  ;; Emacs 31 NEW: 'auto prefers fringes on GUI frames, falls back to margins on TTY.
  (flymake-indicator-type 'auto)
  ;; Suppress the legacy echo-area summary to keep the minibuffer clean for eldoc.
  (flymake-suppress-zero-count-warnings t)
  :config
  ;; Enable flymake globally in programming buffers.
  (add-hook 'prog-mode-hook #'flymake-mode))

;; ==========================================
;; 2. CONSULT-FLYMAKE (Vertico-powered filtering)
;; ==========================================
(use-package consult-flymake
  :ensure nil  ;; Bundled with consult
  :after (consult flymake)
  :config
  ;; `consult-flymake-project` is not a native command; it requires a wrapper
  ;; to pass the prefix argument for project-wide searching.
  (defun consult-flymake-project ()
    "Invoke consult-flymake across all project buffers."
    (interactive)
    (consult-flymake t)))

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c e" '(consult-flymake :wk "Search errors (buffer)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c n" '(flymake-goto-next-error :wk "Next error")
  "c p" '(flymake-goto-prev-error :wk "Prev error")
  "c !" '(flymake-start :wk "Force recheck"))

(general-define-key
  :states 'motion
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)
```

### Why This Approach (vs. `flycheck` / `lsp-ui`)

| Consideration       | `flymake` native (chosen)                                                  | `flycheck` (rejected)                                                 | `lsp-ui` (rejected)                                    |
| ------------------- | -------------------------------------------------------------------------- | --------------------------------------------------------------------- | ------------------------------------------------------ |
| LSP client coupling | Works seamlessly with `eglot` out of the box.                              | Requires explicit checker definitions and parallel engine management. | Hard-bound to the `lsp-mode` ecosystem.                |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                     | Adds redundant diagnostic engine overhead.                            | Requires forbidden `lsp-mode` ecosystem.               |
| Emacs 31 synergy    | Leverages new `'fancy` end-of-line rendering and enhanced tabulated lists. | Lacks integration with Emacs 31 core UI enhancements.                 | Relies on heavy, custom child-frame pipelines.         |
| Performance         | Built into Emacs core; zero additional packages or background processes.   | Spawns independent checker processes, increasing memory footprint.    | Child-frame overhead on every hover/diagnostic update. |

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                             |
| -------------------------------------- | ------------------------------------------------------------------------------- |
| Squiggles under erroneous code         | `flymake` renders squiggles via `flymake-error` / `flymake-warning` faces.      |
| Centralized Problems panel             | `flymake-show-buffer-diagnostics` or `consult-flymake-project`.                 |
| Filter by severity (Errors / Warnings) | Native `/` filter in tabulated-list + `consult-flymake` for fuzzy matching.     |
| Click error to jump to location        | `RET` on any row in the diagnostics buffer, or fringe/margin clicks (Emacs 31). |
| Inline message on hover                | Emacs 31 `'fancy` end-of-line rendering displays the message contextually.      |
| "Quick Fix" from panel                 | `SPC c a` (`eglot-code-actions`) at the diagnostic location.                    |

### Emacs 31 Specific Enhancements

- **`flymake-show-diagnostics-at-end-of-line 'fancy`:** A game-changer for inline diagnostics. Instead of truncating messages in the echo area or requiring a floating child-frame (`flyover`), Emacs 31 lays out diagnostics below the affected line using Unicode graphics that point back to the exact locus of the error.
- **`flymake-show-buffer-diagnostics` Enhanced:** The native diagnostics buffer now highlights the diagnostic nearest to the current point in the listing and responds to fringe/margin mouse clicks, making buffer-local navigation instantaneous. Column widths also dynamically adjust to content.
- **`flymake-indicator-type 'auto`:** Intelligently prefers fringes on GUI frames for a cleaner look, but gracefully falls back to margins on TTY frames, maintaining visual consistency across all environments.
- **`flymake-make-diagnostic` API Extensions:** Accepts new `origin` and `code` attributes, and `flymake-diagnostic-format-alist` provides granular, per-context control over how diagnostics are formatted.

### Integration with Existing Stack

- **`eglot`:** Natively intercepts `textDocument/publishDiagnostics` and routes the payload directly into `flymake`'s API, requiring zero custom translation layers.
- **`consult`:** `consult-flymake` leverages `vertico` and `orderless` to provide instant, space-separated fuzzy filtering of diagnostic messages, complete with live buffer previews.
- **`doom-modeline`:** The `doom-modeline-lsp` segment automatically displays live error and warning counts in the mode line, mirroring the VS Code status bar badge.
- **`evil-collection`:** Provides Unimpaired-style `[e` / `]e` bracket navigation for rapidly cycling through errors without leaving normal state.

---

## Diagnostics (Pull Model)

_VS Code feature: Client requests diagnostics on demand (LSP 3.17+), including workspace-wide refresh._

### Feature Overview

| Attribute          | Value                                                                |
| ------------------ | -------------------------------------------------------------------- |
| Feature            | Diagnostics (Pull Model)                                             |
| VS Code equivalent | "Re-run document diagnostics" or workspace-wide diagnostic refresh   |
| Status             | 🟢 working · native `eglot` + `flymake` (LSP 3.17+)                  |
| Category           | Diagnostics & Symbols                                                |
| LSP methods        | `textDocument/diagnostic`, `workspace/diagnostic`                    |
| Emacs routing      | `flymake-start` → `eglot` → LSP pull request → `flymake` aggregation |

### Implementation Stack

| Layer                 | Component                          | Role                                                                                                                                                              |
| --------------------- | ---------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client            | `eglot` (built-in, Emacs 31)       | Natively implements LSP 3.17+ pull diagnostic capabilities, routing `textDocument/diagnostic` requests when triggered.                                            |
| Diagnostic Engine     | `flymake` (built-in)               | Acts as the central aggregator. When `flymake-start` is invoked, it signals `eglot` to pull fresh diagnostics rather than waiting for the server's push interval. |
| Workspace Aggregation | `flymake-show-project-diagnostics` | Emacs 31 native command that lists all pulled diagnostics across the entire workspace in a single, filterable tabulated buffer.                                   |
| Fuzzy Filtering       | `consult-flymake-project`          | Provides a Vertico-powered, live-preview dropdown for rapidly searching and jumping to pulled workspace diagnostics.                                              |

### Commands & Keybindings

| Action                     | Command                            | Keybinding | Notes                                                                                              |
| -------------------------- | ---------------------------------- | ---------- | -------------------------------------------------------------------------------------------------- |
| Force document re-check    | `flymake-start`                    | `SPC c !`  | Explicitly triggers a pull-model `textDocument/diagnostic` request for the current buffer.         |
| Workspace diagnostics list | `flymake-show-project-diagnostics` | `SPC c P`  | **Emacs 31 NEW** — pulls and lists all workspace diagnostics in a centralized tabulated buffer.    |
| Project diagnostics search | `consult-flymake-project`          | `SPC c E`  | Fuzzy-searches all pulled diagnostics across the entire workspace with live preview.               |
| Toggle auto-check          | `flymake-mode`                     | —          | Can be toggled off to rely _exclusively_ on manual pull-model requests, saving network/CPU cycles. |

### Configuration

`eglot` automatically negotiates LSP 3.17+ pull diagnostic capabilities during the initialization handshake if the language server advertises `diagnosticProvider`. No explicit configuration is required to enable the pull model; it seamlessly integrates with the existing `flymake` setup.

```elisp
;; ==========================================
;; FLYMAKE CORE (Emacs 31 Native Diagnostics)
;; ==========================================
(use-package flymake
  :ensure nil
  :custom
  ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line.
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  (flymake-indicator-type 'auto)
  (flymake-suppress-zero-count-warnings t)
  :config
  ;; Enable flymake globally in programming buffers.
  (add-hook 'prog-mode-hook #'flymake-mode))

;; ==========================================
;; CONSULT-FLYMAKE (Vertico-powered filtering)
;; ==========================================
(use-package consult-flymake
  :ensure nil  ;; Bundled with consult
  :after (consult flymake)
  :config
  ;; Wrapper for project-wide diagnostic searching.
  (defun consult-flymake-project ()
    "Invoke consult-flymake across all project buffers."
    (interactive)
    (consult-flymake t)))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c !" '(flymake-start :wk "Force recheck (pull diagnostics)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics panel"))
```

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` + `flymake` native (chosen)                                                                                                      | `lsp-mode` (rejected)                                        |
| ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ |
| LSP client coupling | Works seamlessly with built-in `eglot`.                                                                                                  | Hard-bound to the `lsp-mode` ecosystem.                      |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                                                   | Requires forbidden `lsp-mode` ecosystem.                     |
| Emacs 31 synergy    | Leverages Emacs 31's new `flymake-show-project-diagnostics` for native workspace pull aggregation.                                       | Relies on legacy, third-party workspace diagnostic buffers.  |
| Performance         | Built into Emacs core; zero additional packages or background processes.                                                                 | Heavy workspace management overhead.                         |
| Flexibility         | Allows users to disable `flymake-mode` entirely and rely _only_ on manual `flymake-start` pulls for maximum performance on slow servers. | Tightly couples auto-checking with the LSP client lifecycle. |

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                             |
| -------------------------------------- | ----------------------------------------------------------------------------------------------- |
| "Re-run document diagnostics" command  | `SPC c !` (`flymake-start`) triggers an immediate pull request.                                 |
| Workspace-wide diagnostic refresh      | `SPC c P` (`flymake-show-project-diagnostics`) pulls and aggregates all workspace errors.       |
| Centralized Problems panel             | `flymake-show-project-diagnostics` or `consult-flymake-project`.                                |
| Filter by severity (Errors / Warnings) | Native `/` filter in tabulated-list + `consult-flymake` for fuzzy matching.                     |
| Click error to jump to location        | `RET` on any row in the diagnostics buffer, or fringe/margin clicks.                            |
| On-demand checking (no auto-check)     | Disable `flymake-mode` globally, and bind `flymake-start` to a convenient key for manual pulls. |

### Emacs 31 Specific Enhancements

- **LSP 3.17+ Native Support:** `eglot` in Emacs 31 fully implements the `textDocument/diagnostic` and `workspace/diagnostic` methods. When `flymake-start` is called, `eglot` intelligently routes this as a pull request rather than waiting for the server's push interval, providing instant feedback on demand.
- **`flymake-show-project-diagnostics` (NEW):** A game-changer for workspace pull diagnostics. Instead of querying files individually, this Emacs 31 command requests and lists every diagnostic across the entire workspace in one `*Flymake diagnostics*` tabulated buffer, perfectly mirroring VS Code's Problems panel.
- **Dynamic Column Widths:** The tabulated list dynamically adjusts column widths to fit content, preventing truncation of long file paths or verbose LSP pull diagnostic messages in monorepos.

### Integration with Existing Stack

- **`eglot`:** Natively intercepts `flymake-start` and translates it into an LSP 3.17+ `textDocument/diagnostic` request, respecting the server's `interFileDependencies` and `workspaceDiagnostics` capabilities.
- **`consult`:** `consult-flymake-project` leverages `vertico` and `orderless` to provide instant, space-separated fuzzy filtering of the pulled workspace diagnostics, complete with live buffer previews.
- **`doom-modeline`:** The `doom-modeline-lsp` segment automatically displays live error and warning counts in the mode line, updating instantly when pull diagnostics are refreshed.
- **`evil-collection`:** Provides Unimpaired-style `[e` / `]e` bracket navigation for rapidly cycling through the newly pulled errors without leaving normal state.

### Troubleshooting

#### Pull Diagnostics Not Triggering

1.  **Verify Server Support:** Check the `*eglot-events*` buffer or run `M-x eglot-describe-connection` to confirm the server advertises `diagnosticProvider` in its capabilities.
2.  **Check Flymake State:** Ensure `flymake-mode` is active in the buffer. Even for manual pulls, `flymake` must be enabled to receive and render the `eglot` payload.
3.  **Force Reconnect:** If the server updated its capabilities mid-session, run `M-x eglot-reconnect` to re-negotiate the LSP 3.17+ pull diagnostic features.

#### Workspace Pull is Slow

If `workspace/diagnostic` requests cause noticeable lag on massive projects, rely on buffer-local pulls (`SPC c !`) for immediate feedback, and reserve `consult-flymake-project` (`SPC c E`) for targeted, fuzzy-filtered searches rather than full workspace refreshes.

---

## Problems Panel

_VS Code feature: Centralized, filterable list of all errors/warnings across the project._

### Feature Overview

| Attribute          | Value                                                                        |
| ------------------ | ---------------------------------------------------------------------------- |
| Feature            | Problems Panel                                                               |
| VS Code equivalent | "Problems" tab aggregating workspace diagnostics with severity filtering     |
| Status             | 🟢 emacs 31 · native `flymake` + `consult-flymake`                           |
| Category           | Diagnostics & Symbols                                                        |
| LSP methods        | `textDocument/publishDiagnostics`                                            |
| Emacs routing      | `eglot` → `flymake` → `consult-flymake` / `flymake-show-project-diagnostics` |

### Implementation Stack

| Layer                | Component                                         | Role                                                                                                                             |
| -------------------- | ------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client           | `eglot` (built-in, Emacs 31)                      | Receives `textDocument/publishDiagnostics` payloads and translates them into native `flymake` diagnostics.                       |
| Diagnostic Engine    | `flymake` (built-in)                              | Manages the lifecycle of diagnostics, rendering squiggles and aggregating them into tabulated lists.                             |
| Workspace Aggregator | `flymake-show-project-diagnostics` (Emacs 31 NEW) | Natively lists all pulled diagnostics across the entire workspace in a single, filterable tabulated buffer.                      |
| Fuzzy Filtering      | `consult-flymake`                                 | Provides a Vertico-powered, live-preview dropdown for rapidly searching and jumping to diagnostics across the buffer or project. |

### Commands & Keybindings

| Action                     | Command                            | Keybinding        | Notes                                                                                    |
| -------------------------- | ---------------------------------- | ----------------- | ---------------------------------------------------------------------------------------- |
| Buffer diagnostics search  | `consult-flymake`                  | `SPC c e`         | Fuzzy-searches diagnostics in the current buffer with live preview.                      |
| Project diagnostics search | `consult-flymake-project`          | `SPC c E`         | Fuzzy-searches all diagnostics across the entire workspace.                              |
| Project diagnostics panel  | `flymake-show-project-diagnostics` | `SPC c P`         | **Emacs 31 NEW** — opens a centralized, filterable tabulated list of all project errors. |
| Next error                 | `flymake-goto-next-error`          | `SPC c n` / `] e` | Jumps to the next diagnostic in the current buffer.                                      |
| Previous error             | `flymake-goto-prev-error`          | `SPC c p` / `[ e` | Jumps to the previous diagnostic in the current buffer.                                  |
| Force recheck              | `flymake-start`                    | `SPC c !`         | Manually triggers a diagnostic refresh.                                                  |

### Configuration

```elisp
;; ==========================================
;; FLYMAKE CORE (Emacs 31 Native Diagnostics)
;; ==========================================
(use-package flymake
  :ensure nil
  :custom
  ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line.
  (flymake-show-diagnostics-at-end-of-line 'fancy)
  ;; Emacs 31 NEW: 'auto prefers fringes on GUI frames, falls back to margins on TTY.
  (flymake-indicator-type 'auto)
  ;; Suppress the legacy echo-area summary to keep the minibuffer clean.
  (flymake-suppress-zero-count-warnings t)
  :config
  ;; Enable flymake globally in programming buffers.
  (add-hook 'prog-mode-hook #'flymake-mode))

;; ==========================================
;; CONSULT-FLYMAKE (Vertico-powered filtering)
;; ==========================================
(use-package consult-flymake
  :ensure nil  ;; Bundled with consult
  :after (consult flymake)
  :config
  ;; Wrapper for project-wide diagnostic searching.
  (defun consult-flymake-project ()
    "Invoke consult-flymake across all project buffers."
    (interactive)
    (consult-flymake t)))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c e" '(consult-flymake :wk "Search errors (buffer)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c n" '(flymake-goto-next-error :wk "Next error")
  "c p" '(flymake-goto-prev-error :wk "Prev error")
  "c !" '(flymake-start :wk "Force recheck")
  "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics panel"))

(general-define-key
  :states 'motion
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)
```

### Why This Approach (vs. `lsp-ui` / `lsp-mode`)

| Consideration       | `flymake` native (chosen)                                                          | `lsp-ui` (rejected)                                         |
| ------------------- | ---------------------------------------------------------------------------------- | ----------------------------------------------------------- |
| LSP client coupling | Works seamlessly with `eglot` out of the box.                                      | Hard-bound to the `lsp-mode` ecosystem.                     |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                             | Requires the forbidden `lsp-mode` ecosystem.                |
| Emacs 31 synergy    | Leverages new `flymake-show-project-diagnostics` for native workspace aggregation. | Relies on legacy, third-party workspace diagnostic buffers. |
| Performance         | Built into Emacs core; zero additional packages or background processes.           | Heavy child-frame overhead on every diagnostic update.      |

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                      |
| -------------------------------------- | ---------------------------------------------------------------------------------------- |
| Centralized Problems panel             | `SPC c P` (`flymake-show-project-diagnostics`) or `SPC c E` (`consult-flymake-project`). |
| Filter by severity (Errors / Warnings) | Native `/` filter in tabulated-list + `consult-flymake` for fuzzy matching.              |
| Click error to jump to location        | `RET` on any row in the diagnostics buffer, or fringe/margin clicks.                     |
| "Quick Fix" from panel                 | `SPC c a` (`eglot-code-actions`) at the diagnostic location.                             |
| Squiggles under erroneous code         | `flymake` renders squiggles via `flymake-error` / `flymake-warning` faces.               |
| Inline message on hover                | Emacs 31 `'fancy` end-of-line rendering displays the message contextually.               |

### Emacs 31 Specific Enhancements

- **`flymake-show-project-diagnostics` (NEW):** A game-changer for workspace diagnostics. Instead of querying files individually, this Emacs 31 command requests and lists every diagnostic across the entire workspace in one `*Flymake diagnostics*` tabulated buffer, perfectly mirroring VS Code's Problems panel.
- **`flymake-show-diagnostics-at-end-of-line 'fancy`:** Instead of truncating messages in the echo area, Emacs 31 lays out diagnostics below the affected line using Unicode graphics that point back to the exact locus of the error.
- **Dynamic Column Widths:** The tabulated list dynamically adjusts column widths to fit content, preventing truncation of long file paths or verbose LSP diagnostic messages in monorepos.

### Integration with Existing Stack

- **`eglot`:** Natively intercepts `textDocument/publishDiagnostics` and routes the payload directly into `flymake`'s API, requiring zero custom translation layers.
- **`consult`:** `consult-flymake` leverages `vertico` and `orderless` to provide instant, space-separated fuzzy filtering of diagnostic messages, complete with live buffer previews.
- **`doom-modeline`:** The `doom-modeline-lsp` segment automatically displays live error and warning counts in the mode line, mirroring the VS Code status bar badge.
- **`evil-collection`:** Provides Unimpaired-style `[ e` / `] e` bracket navigation for rapidly cycling through errors without leaving normal state.

### Troubleshooting

#### Project Diagnostics Panel is Empty

1.  **Verify Project Context:** `flymake-show-project-diagnostics` requires an active `project-current`. Ensure your project has a `.git` directory or a `pyproject.toml`/`package.json` at the root.
2.  **Check Eglot Connection:** Ensure `eglot` is actively connected. Run `M-x eglot-describe-connection` to verify the server is running and providing `diagnosticProvider` capabilities.

#### Diagnostics Feel Laggy

If workspace diagnostic requests cause noticeable lag on massive projects, rely on buffer-local pulls (`SPC c !`) for immediate feedback, and reserve `consult-flymake-project` (`SPC c E`) for targeted, fuzzy-filtered searches rather than full workspace refreshes.

---

## Inline Values

_VS Code feature: Shows variable values inline at end-of-line while paused in the debugger._

### Feature Overview

| Attribute          | Value                                                                                                                                                       |
| ------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Feature            | Inline Values (Debugger)                                                                                                                                    |
| VS Code equivalent | Inline variable value overlays displayed at the end of the line when execution is paused                                                                    |
| Status             | 🟢 working · `dape` (Debug Adapter Protocol for Emacs)                                                                                                      |
| Category           | Diagnostics & Debugging                                                                                                                                     |
| Protocol           | Debug Adapter Protocol (DAP) — _Note: While LSP 3.17 introduced `textDocument/inlineValue`, debugger-specific inline values are exclusively a DAP feature._ |
| Emacs routing      | `dape` → `dape-inline-variables` → native buffer overlays                                                                                                   |

### Implementation Stack

| Layer          | Component                 | Role                                                                                                                                                |
| -------------- | ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| Debug Client   | `dape` (GNU ELPA / MELPA) | The modern, lightweight Debug Adapter Protocol client for Emacs, explicitly designed as a minimalist alternative to the heavy `dap-mode` ecosystem. |
| Overlay Engine | `dape-inline-variables`   | Built-in `dape` feature that renders variable values as `after-string` overlays at the end of the line when the debugger is paused.                 |
| Visual Styling | `dape-inline-value-face`  | Customizable face allowing the inline values to blend seamlessly with the editor theme (e.g., Tokyo Night).                                         |

### Commands & Keybindings

| Action                  | Command                        | Keybinding | Notes                                                               |
| ----------------------- | ------------------------------ | ---------- | ------------------------------------------------------------------- |
| Toggle inline variables | `dape-toggle-inline-variables` | —          | Toggles the overlay visibility dynamically.                         |
| Continue execution      | `dape-continue`                | `F5`       | Resumes execution; inline overlays automatically clear.             |
| Step over               | `dape-next`                    | `F10`      | Steps to the next line; overlays update with new variable states.   |
| Step into               | `dape-step-in`                 | `F11`      | Steps into the current function call.                               |
| Evaluate expression     | `dape-evaluate`                | `SPC d e`  | Opens minibuffer to evaluate and temporarily display an expression. |

### Configuration

The canonical Emacs 31 approach uses `dape`, which provides a minimalist, highly performant debugging experience without the bloated, legacy `dap-mode` ecosystem.

```elisp
;; ==========================================
;; DAPE (Lightweight Debug Adapter Protocol)
;; ==========================================
(use-package dape
  :ensure t
  :defer t
  :commands (dape dape-breakpoint-toggle dape-continue dape-next dape-step-in)
  :custom
  ;; Enable inline variable overlays while paused in the debugger.
  (dape-inline-variables t)
  ;; Optional: Configure the window layout for the REPL/buffers
  (dape-buffer-window-arrangement 'right)
  :config
  ;; Enable global breakpoint fringe indicators
  (dape-breakpoint-global-mode 1)

  ;; ==========================================
  ;; VISUAL STYLING (Tokyo Night Synergy)
  ;; ==========================================
  (custom-set-faces
   '(dape-inline-value-face
     ((t (:inherit shadow
                   :foreground "#73daca"
                   :height 0.9
                   :slant italic
                   :box (:line-width 1 :color "#292e42" :style nil)))))))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "d" '(:ignore t :wk "debug")
  "d c" '(dape-continue :wk "Continue")
  "d n" '(dape-next :wk "Step over")
  "d i" '(dape-step-in :wk "Step into")
  "d o" '(dape-step-out :wk "Step out")
  "d b" '(dape-breakpoint-toggle :wk "Toggle breakpoint")
  "d d" '(dape :wk "Start debugging"))
```

### Why This Approach (vs. `dap-mode` / LSP `inlineValue`)

| Consideration       | `dape` (chosen)                                                                     | `dap-mode` (rejected)                                                          | LSP `textDocument/inlineValue`                                                                   |
| ------------------- | ----------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------ |
| Ecosystem Weight    | Minimalist, modern, actively maintained.                                            | Heavy, complex, and increasingly deprecated in favor of `dape`.                | N/A for debugger states; LSP inline values are for non-debugger inferred values (e.g., Jupyter). |
| Protocol Compliance | Honors the "no `dap-mode`" constraint while providing full DAP parity.              | Violates the strict architectural constraint against the `dap-mode` ecosystem. | Incorrect protocol for _paused debugger_ variable inspection.                                    |
| Performance         | Lightweight overlay rendering with minimal main-thread impact.                      | Known for UI stutter and heavy buffer management during debug sessions.        | N/A                                                                                              |
| Emacs 31 Synergy    | Integrates cleanly with modern Emacs window management and `eglot`-managed buffers. | Legacy architecture with outdated window splitting logic.                      | N/A                                                                                              |

### Behavioral Parity Matrix

| VS Code behavior                                     | Emacs 31 equivalent                                                                                   |
| ---------------------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| Inline grey text showing variable values when paused | `dape-inline-variables t` renders `dape-inline-value-face` overlays at the end of the line.           |
| Overlays update automatically on step                | `dape-next` / `dape-step-in` triggers an overlay refresh with the new scope's variable state.         |
| Overlays clear when continuing                       | `dape-continue` automatically removes all inline value overlays.                                      |
| Click to evaluate or expand                          | `dape` overlays can be configured to trigger `dape-evaluate` on mouse interaction (or via `SPC d e`). |
| Respects editor theme                                | `dape-inline-value-face` is customized to inherit `shadow` with Tokyo Night accent colors.            |

### Emacs 31 Specific Enhancements

- **Minimalist Overlay Architecture:** `dape` was explicitly designed to avoid the heavy UI overhead of legacy debuggers. Its inline variable implementation uses efficient `after-string` text properties that do not interfere with `treesit` fontification or `eglot` semantic tokens.
- **Seamless `eglot` Coexistence:** Because `dape` handles only the Debug Adapter Protocol, it coexists perfectly with `eglot` (which handles the Language Server Protocol). There is no protocol overlap or resource contention, allowing both to run simultaneously in the same buffer without conflict.
- **Native Breakpoint Indicators:** `dape-breakpoint-global-mode` provides clean, non-intrusive fringe indicators for breakpoints, complementing the inline value overlays without cluttering the margin.

### Integration with Existing Stack

- **`eglot`:** Manages all LSP features (completion, diagnostics, semantic tokens), while `dape` exclusively handles the debugging lifecycle. This strict separation of concerns is the gold standard for modern Emacs configurations.
- **`treesit`:** The inline value overlays are rendered with a lower priority than `treesit` font-lock, ensuring that primary syntax highlighting always takes visual precedence.
- **`general.el`:** Eagerly registers the `SPC d` prefix for all debugging commands, ensuring consistent, mnemonic access regardless of the major mode.
- **`doom-themes`:** The custom `dape-inline-value-face` inherits the Tokyo Night `shadow` and `#73daca` (teal) accents, maintaining a cohesive, professional IDE aesthetic that clearly distinguishes debug values from standard code.

### Troubleshooting

#### Inline Values Not Appearing

1.  **Verify DAP Configuration:** Ensure your `dape-configs` are correctly set up for your language (e.g., `python` using `debugpy`, `go` using `dlv`).
2.  **Check Variable Scope:** Inline values only appear for variables in the current stack frame's scope. If the cursor is on a line where a variable is not yet defined or is out of scope, the overlay will not render.
3.  **Confirm Toggle State:** Run `M-x dape-toggle-inline-variables` to ensure the feature is actively enabled.

#### Overlays Clash with Syntax Highlighting

If the inline values are too bright or distracting, adjust the `dape-inline-value-face` in your configuration to use a more recessive color (e.g., `:foreground "#565f89"`) and ensure `:inherit shadow` is set.

---


---

# Code Actions & Refactoring

## Code Actions (Quick Fixes & Refactorings)

_VS Code feature: Lightbulb menu: quick fixes, extract method/variable, add missing import, etc._

### Feature Overview

| Attribute          | Value                                                                |
| ------------------ | -------------------------------------------------------------------- |
| Feature            | Code Actions (quick fixes & refactorings)                            |
| VS Code equivalent | `Ctrl+.` lightbulb menu, inline quick fixes, and refactoring options |
| Status             | 🟢 working · native `eglot` + `vertico` / `consult`                  |
| Category           | Code Actions & Refactoring                                           |
| LSP methods        | `textDocument/codeAction`, `codeAction/resolve`                      |
| Emacs routing      | `eglot` → `eglot-code-actions` → `completing-read` (Vertico/Consult) |

### Implementation Stack

| Layer            | Component                                      | Role                                                                                                        |
| ---------------- | ---------------------------------------------- | ----------------------------------------------------------------------------------------------------------- |
| LSP Client       | `eglot` (built-in, Emacs 31)                   | Queries `textDocument/codeAction` at point and resolves complex actions via `codeAction/resolve`.           |
| Action Router    | `eglot-code-actions`                           | Presents available actions (quick fixes, refactors, source actions) in a searchable `completing-read` menu. |
| UI Engine        | `vertico` / `consult`                          | Provides fuzzy filtering, candidate grouping, and live preview of the code action context.                  |
| Visual Indicator | `eglot-code-action-indications` (Emacs 31 NEW) | Renders the "lightbulb" indicator in the left margin or inline near the cursor when actions are available.  |

### Commands & Keybindings

| Action                     | Command                              | Keybinding        | Notes                                                                             |
| -------------------------- | ------------------------------------ | ----------------- | --------------------------------------------------------------------------------- |
| Invoke code actions        | `eglot-code-actions`                 | `SPC c a` / `C-.` | Opens the searchable menu of all available actions at point.                      |
| Organize imports           | `eglot-code-action-organize-imports` | `SPC c i`         | Direct shortcut for the most common source action.                                |
| Quick fix (diagnostic)     | `eglot-code-action-quickfix`         | —                 | Filters the code action menu to show only quick fixes for the current diagnostic. |
| Execute server command     | `eglot-execute-command`              | `SPC c x`         | Runs workspace-level commands surfaced by the server (e.g., "Restart Server").    |
| Toggle lightbulb indicator | `eglot-code-action-indications`      | —                 | Configured via `setq` to show in `margin`, `eldoc-hint`, or both.                 |

### Configuration

`eglot` natively handles code actions. The configuration focuses on optimizing the Emacs 31 lightbulb indicator and ensuring seamless integration with the `vertico`/`consult` completion stack.

```elisp
;; ==========================================
;; EGLOT CODE ACTIONS (Built-in)
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Control where the lightbulb indicator appears.
  ;; - 'margin: Renders indicator in the left margin (VS Code parity)
  ;; - 'eldoc-hint: Renders indicator inline near the point via ElDoc
  ;; Both can be enabled simultaneously for maximum visibility.
  (eglot-code-action-indications '(margin eldoc-hint))
  ;; The actual glyph/string used as the indicator.
  ;; Defaults to a lightbulb emoji, but can be changed to a simpler Unicode
  ;; character (e.g., "⚡" or "✦") if the emoji causes rendering glitches
  ;; in specific tree-sitter modes or terminal emulators.
  (eglot-code-action-indicator "💡")
  :config
  ;; Ensure eglot's code actions integrate cleanly with consult/vertico
  ;; for live preview and fuzzy filtering.
  (when (boundp 'eglot-extend-to-xref)
    (setq eglot-extend-to-xref t)))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c a" '(eglot-code-actions :wk "Code actions (lightbulb)")
  "c i" '(eglot-code-action-organize-imports :wk "Organize imports")
  "c x" '(eglot-execute-command :wk "Execute server command"))
```

### Why This Approach (vs. `lsp-ui-sideline` / `lsp-mode`)

| Consideration       | `eglot` native (chosen)                                      | `lsp-ui-sideline` (rejected)                                                  |
| ------------------- | ------------------------------------------------------------ | ----------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot`.                              | Hard-bound to the `lsp-mode` ecosystem.                                       |
| Protocol compliance | Honors the `eglot`-only stack mandate.                       | Requires the forbidden `lsp-mode` ecosystem.                                  |
| UI Physics          | Native `completing-read` menu + Emacs 31 margin indicators.  | Heavy sideline rendering engine that shifts text and causes redisplay jitter. |
| Performance         | Zero additional packages; leverages native Emacs completion. | Child-frame overhead and complex sideline management.                         |
| Maintenance         | Maintained by GNU Emacs core team (João Távora).             | Stale — tracks `lsp-mode` lifecycle.                                          |

### Behavioral Parity Matrix

| VS Code behavior                                  | Emacs 31 equivalent                                                          |
| ------------------------------------------------- | ---------------------------------------------------------------------------- |
| `Ctrl+.` opens quick fix menu                     | `SPC c a` (`eglot-code-actions`) opens Vertico-powered action menu.          |
| Lightbulb icon in gutter                          | Emacs 31 `eglot-code-action-indications` renders 💡 in the left margin.      |
| "Organize Imports" shortcut                       | `SPC c i` (`eglot-code-action-organize-imports`).                            |
| Action menu filters by type (Quick Fix, Refactor) | `vertico` allows instant fuzzy filtering (e.g., typing `fix` or `refactor`). |
| Live preview of the proposed change               | `consult` integration provides live buffer preview of the action's effect.   |
| Click lightbulb to open actions                   | Emacs 31 `eglot-code-actions-at-mouse` (click diagnostic with `mouse-2`).    |
| Workspace-level commands                          | `SPC c x` (`eglot-execute-command`) surfaces server-defined palette entries. |

### Emacs 31 Specific Enhancements

- **`eglot-code-action-indications` (NEW):** Emacs 31 introduces native visual indication of available code actions directly within `eglot`. The variable accepts a list of symbols: `margin` (renders in the left margin), `eldoc-hint` (renders via ElDoc), or `mode-line`. This completely eliminates the need for third-party sideline packages.
- **`eglot-code-action-indicator` (NEW):** Customizable string/glyph used as the visual indicator. While the default is a lightbulb emoji, it can be safely swapped for a simpler Unicode character (like `⚡`) to prevent rendering glitches in specific tree-sitter modes or TTY environments.
- **Enhanced `completing-read` Integration:** Emacs 31's refined `xref` and `eglot` pipelines ensure that complex code actions (which require `codeAction/resolve` network calls) are handled asynchronously, preventing main-thread blocking while the action menu populates.
- **Mouse Integration:** `eglot-code-actions-at-mouse` allows invoking the code action menu directly by clicking on a diagnostic squiggle with `mouse-2`, matching VS Code's click-to-fix paradigm.

### Integration with Existing Stack

The Code Actions surface integrates seamlessly with the eglot + treesit stack:

- **`eglot`:** Natively queries `textDocument/codeAction` and handles the `codeAction/resolve` lifecycle, applying the resulting `WorkspaceEdit` safely.
- **`vertico` / `consult`:** Intercepts the `completing-read` prompt to provide a vertically scrolling, fuzzy-filterable list with live buffer previews, making it easy to distinguish between similar refactoring options.
- **`apheleia`:** Often used in tandem; while `eglot` handles semantic refactoring (e.g., "Extract Method"), `apheleia` ensures the resulting code is instantly formatted on save without blocking the editor.
- **`general.el`:** Eagerly registers the `SPC c a` and `SPC c i` leader bindings, ensuring the commands are instantly available without waiting for package lazy-loading.

---

## Quick Fix Lightbulb

_VS Code feature: Visual indicator (lightbulb) in the gutter showing that code actions are available at the cursor._

### Feature Overview

| Attribute          | Value                                                                   |
| ------------------ | ----------------------------------------------------------------------- |
| Feature            | Quick Fix Lightbulb                                                     |
| VS Code equivalent | Gutter lightbulb icon indicating available code actions or quick fixes  |
| Status             | 🟢 working · native `eglot`                                             |
| Category           | Code Actions & Refactoring                                              |
| LSP methods        | `textDocument/codeAction`                                               |
| Emacs routing      | `eglot` → `eglot-code-action-indications` → margin/eldoc-hint rendering |

### Implementation Stack

| Layer             | Component                       | Role                                                                                          |
| ----------------- | ------------------------------- | --------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31)    | Queries `textDocument/codeAction` on cursor idle and evaluates available actions.             |
| Indicator Engine  | `eglot-code-action-indications` | Natively renders visual cues in the left margin or via ElDoc hints when actions are present.  |
| Visual Glyph      | `eglot-code-action-indicator`   | Customizable string or glyph (e.g., `"💡"` or `"⚡"`) used as the visual marker.              |
| Performance Guard | Idle delay integration          | Debounces the code action query to prevent main-thread blocking during rapid cursor movement. |

### Commands & Keybindings

| Action                  | Command                         | Keybinding        | Notes                                                             |
| ----------------------- | ------------------------------- | ----------------- | ----------------------------------------------------------------- |
| Invoke code actions     | `eglot-code-actions`            | `SPC c a` / `C-.` | Opens the searchable menu of all available actions at point.      |
| Toggle margin indicator | `eglot-code-action-indications` | —                 | Configured via `setq` to show in `margin`, `eldoc-hint`, or both. |

### Configuration

`eglot` natively handles code action indications in Emacs 31. The configuration focuses on optimizing the visual indicator and ensuring seamless integration without terminal rendering glitches.

```elisp
;; ==========================================
;; QUICK FIX LIGHTBULB (Built-in Eglot)
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Control where the lightbulb indicator appears.
  ;; 'margin renders in the left gutter; 'eldoc-hint shows in the echo area.
  (eglot-code-action-indications '(margin eldoc-hint))
  ;; The actual glyph used as the indicator.
  ;; Defaults to a lightbulb emoji, but a simpler Unicode character (e.g., "⚡")
  ;; prevents rendering glitches in specific terminals or tree-sitter modes.
  (eglot-code-action-indicator "💡")
  :config
  ;; Ensure code actions integrate cleanly with consult/vertico for live preview.
  (when (boundp 'eglot-extend-to-xref)
    (setq eglot-extend-to-xref t)))
```

### Why This Approach (vs. `lsp-ui-sideline` / `lsp-mode`)

| Consideration       | `eglot` native (chosen)                                                      | `lsp-ui-sideline` (rejected)                                                  |
| ------------------- | ---------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot`.                                              | Hard-bound to the `lsp-mode` ecosystem.                                       |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                       | Requires the forbidden `lsp-mode` ecosystem.                                  |
| UI Physics          | Native margin or ElDoc rendering with zero layout shift.                     | Heavy sideline rendering engine that shifts text and causes redisplay jitter. |
| Performance         | Zero additional packages; leverages native Emacs completion and margin APIs. | Child-frame overhead and complex sideline management.                         |
| Emacs 31 Synergy    | Leverages Emacs 31's refined margin and ElDoc integration natively.          | No integration with Emacs 31 core enhancements.                               |

### Behavioral Parity Matrix

| VS Code behavior                           | Emacs 31 equivalent                                                                                      |
| ------------------------------------------ | -------------------------------------------------------------------------------------------------------- |
| Lightbulb icon appears in gutter           | `eglot-code-action-indications` set to `margin` renders the indicator in the left fringe.                |
| Indicator disappears when no actions exist | `eglot` automatically clears the margin indicator when `textDocument/codeAction` returns empty.          |
| Click indicator to open actions            | Emacs 31 `eglot-code-actions-at-mouse` allows invoking the menu by clicking the diagnostic or indicator. |
| Fallback hint in status area               | `eldoc-hint` in `eglot-code-action-indications` surfaces a subtle hint in the echo area.                 |
| No lag or stutter while typing             | Idle delay and debouncing prevent the LSP server from being spammed on every keystroke.                  |

### Emacs 31 Specific Enhancements

- **`eglot-code-action-indications` (NEW):** Emacs 31 introduces native visual indication of available code actions directly within `eglot`. The variable accepts a list of valid symbols: `margin` (renders in the left margin), `eldoc-hint` (renders via ElDoc), or `mode-line`.
- **`eglot-code-action-indicator` (NEW):** Customizable string or glyph used as the visual indicator. While the default is a lightbulb emoji, it can be safely swapped for a simpler Unicode character (like `⚡`) to prevent rendering glitches in specific tree-sitter modes or TTY environments.
- **Enhanced `completing-read` Integration:** Emacs 31's refined `eglot` pipelines ensure that complex code actions (which require `codeAction/resolve` network calls) are handled asynchronously, preventing main-thread blocking while the action menu populates.

### Integration with Existing Stack

- **`eglot`:** Natively queries `textDocument/codeAction` and handles the `codeAction/resolve` lifecycle, applying the resulting `WorkspaceEdit` safely.
- **`vertico` / `consult`:** Intercepts the `completing-read` prompt to provide a vertically scrolling, fuzzy-filterable list with live buffer previews, making it easy to distinguish between similar refactoring options.
- **`apheleia`:** Often used in tandem; while `eglot` handles semantic refactoring (e.g., "Extract Method"), `apheleia` ensures the resulting code is instantly formatted on save without blocking the editor.
- **`general.el`:** Eagerly registers the `SPC c a` leader binding, ensuring the command is instantly available without waiting for package lazy-loading.

### Troubleshooting

#### Indicator Causes Terminal Rendering Glitches

If the default lightbulb emoji causes display corruption in a TTY or specific terminal emulator, change `eglot-code-action-indicator` to a standard ASCII or simple Unicode character like `"*"` or `"⚡"`.

#### Indicator Feels Laggy

If the indicator appears slowly, the language server might be slow to respond to `textDocument/codeAction`. Ensure `eglot`'s idle delay is not set too low, or consider disabling the `margin` indication and relying solely on `eldoc-hint` to reduce rendering overhead.

#### Indicator Persists After Actions Are Resolved

Ensure `eglot` is actively managing the buffer. If the indicator gets stuck, manually trigger `M-x eglot-code-actions` to force a state refresh, or restart the server via `M-x eglot-reconnect`.

## Rename Symbol

_VS Code feature: F2 — project-wide safe rename of a symbol and all its references._

### Feature Overview

| Attribute          | Value                                               |
| ------------------ | --------------------------------------------------- |
| Feature            | Rename Symbol                                       |
| VS Code equivalent | F2 (Rename) across the entire workspace             |
| Status             | 🟢 working · native `eglot`                         |
| Category           | Code Actions & Refactoring                          |
| LSP methods        | `textDocument/prepareRename`, `textDocument/rename` |
| Emacs routing      | `eglot` → `eglot-rename` → `workspace/applyEdit`    |

### Implementation Stack

| Layer                 | Component                          | Role                                                                                                                                   |
| --------------------- | ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client            | `eglot` (built-in, Emacs 31)       | Drives `textDocument/prepareRename` (to validate the rename target) and `textDocument/rename` (to apply the AST-aware workspace edit). |
| Workspace Edit Engine | `lsp-workspace-edit` (via `eglot`) | Safely applies multi-file text edits, preserving undo history and file state.                                                          |
| User Prompt           | `read-string`                      | Prompts for the new symbol name, defaulting to the current symbol at point.                                                            |

### Commands & Keybindings

| Action                    | Command                 | Keybinding       | Notes                                                                                 |
| ------------------------- | ----------------------- | ---------------- | ------------------------------------------------------------------------------------- |
| Rename symbol             | `eglot-rename`          | `F2` / `SPC c r` | Prompts for new name and applies project-wide rename.                                 |
| Prepare rename (internal) | `eglot--prepare-rename` | —                | Automatically invoked by `eglot-rename` to check if the symbol is valid for renaming. |

### Configuration

`eglot` handles renaming natively. No additional packages are required. The configuration focuses on ergonomic keybindings via `general.el`.

```elisp
;; ==========================================
;; EGLOT RENAME (Built-in)
;; ==========================================
;; eglot natively provides `eglot-rename`, which safely renames symbols
;; across the entire project using LSP `textDocument/rename`.
;; No explicit configuration is needed beyond the base `eglot` setup.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c r" '(eglot-rename :wk "Rename symbol"))

;; Standard F2 binding for universal rename parity
(general-define-key
  :states '(normal visual)
  "F2" #'eglot-rename)
```

### Why This Approach (vs. `lsp-mode` / `lsp-ui`)

| Consideration       | `eglot` native (chosen)                                                             | `lsp-mode` (rejected)                                     |
| ------------------- | ----------------------------------------------------------------------------------- | --------------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot`.                                                     | Hard-bound to the `lsp-mode` ecosystem.                   |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                              | Requires forbidden `lsp-mode` ecosystem.                  |
| Safety              | Relies on the language server's AST to guarantee only valid references are renamed. | Same, but carries the heavy `lsp-mode` overhead.          |
| Dependencies        | Zero. Built directly into `eglot.el`.                                               | Requires `lsp-mode` and its complex workspace management. |

### Behavioral Parity Matrix

| VS Code behavior                    | Emacs 31 equivalent                                                                                        |
| ----------------------------------- | ---------------------------------------------------------------------------------------------------------- |
| `F2` prompts for new name           | `F2` or `SPC c r` invokes `eglot-rename` with `read-string`.                                               |
| Validates rename target first       | `eglot` automatically sends `textDocument/prepareRename` before prompting.                                 |
| Renames across all project files    | `eglot` processes the `workspace/applyEdit` payload, opening and modifying files as needed.                |
| Preserves undo history              | Native Emacs undo handles the multi-file edits cleanly.                                                    |
| Fails gracefully on invalid targets | `eglot` displays a user-friendly error if `prepareRename` rejects the location (e.g., renaming a keyword). |

### Emacs 31 Specific Enhancements

- **Robust Workspace Edits:** Emacs 31's refined `eglot` implementation handles complex `WorkspaceEdit` payloads (including file creation, deletion, and renaming alongside text edits) with improved stability and fewer edge-case crashes compared to older versions.
- **Seamless `xref` Integration:** While rename is a write operation, any subsequent navigation (like jumping to a renamed file) benefits from Emacs 31's enhanced `xref` history and `xref-mouse-mode`.

### Integration with Existing Stack

- **`eglot`:** Natively manages the entire rename lifecycle, from validation to multi-file application.
- **`general.el`:** Eagerly registers the `F2` and `SPC c r` leader bindings, ensuring the command is instantly available in all `eglot`-managed buffers without deferred-registration traps.
- **`apheleia`:** If the rename operation modifies files, saving those files will naturally trigger `apheleia` to format them, ensuring the refactored code adheres to project styling rules.

---

## Execute Command

_VS Code feature: Server-defined custom commands surfaced in the Command Palette (e.g., "Organize Imports", "Restart Server", "Expand Macro")._

### Feature Overview

| Attribute          | Value                                                                                                    |
| ------------------ | -------------------------------------------------------------------------------------------------------- |
| Feature            | Execute Command                                                                                          |
| VS Code equivalent | Command Palette entries triggered by the language server (e.g., `@workspace` or server-specific actions) |
| Status             | 🟢 working · native `eglot`                                                                              |
| Category           | Code Actions & Refactoring                                                                               |
| LSP methods        | `workspace/executeCommand`                                                                               |
| Emacs routing      | `eglot` → `eglot-execute-command` → `completing-read` (Vertico/Consult)                                  |

### Implementation Stack

| Layer                  | Component                     | Role                                                                                                                                         |
| ---------------------- | ----------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client             | `eglot` (built-in, Emacs 31)  | Queries the server's `executeCommandProvider` capabilities during initialization and routes the `workspace/executeCommand` JSON-RPC request. |
| UI Engine              | `completing-read` / `vertico` | Presents the list of available server-defined commands as a searchable, fuzzy-filtered dropdown.                                             |
| Workspace Edit Handler | `eglot--apply-workspace-edit` | Natively processes any `WorkspaceEdit` payloads returned by the executed command (e.g., multi-file refactoring).                             |

### Commands & Keybindings

| Action                 | Command                              | Keybinding | Notes                                                                                                                         |
| ---------------------- | ------------------------------------ | ---------- | ----------------------------------------------------------------------------------------------------------------------------- |
| Execute server command | `eglot-execute-command`              | `SPC c x`  | Prompts for a command ID registered by the active LSP server.                                                                 |
| Organize imports (LSP) | `eglot-code-action-organize-imports` | `SPC c i`  | Built-in wrapper for the standard LSP organize imports command.                                                               |
| Code actions menu      | `eglot-code-actions`                 | `SPC c a`  | Often surfaces commands like "Fix All" or "Organize Imports" as actionable items, bypassing the need to type raw command IDs. |

### Configuration

`eglot` handles command execution natively. The configuration focuses on ergonomic leader keybindings and leveraging built-in wrappers for common tasks, while deferring pure formatting to `apheleia`.

```elisp
;; ==========================================
;; EGLOT EXECUTE COMMAND (Built-in)
;; ==========================================
;; eglot natively provides `eglot-execute-command` to surface
;; server-defined commands via `completing-read`.

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c x" '(eglot-execute-command :wk "Execute server command")
  "c i" '(eglot-code-action-organize-imports :wk "Organize imports (LSP)"))
```

### Why This Approach (vs. `lsp-mode` or Custom Wrappers)

| Consideration       | `eglot` native (chosen)                                                                          | `lsp-mode` (rejected)                                                    |
| ------------------- | ------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------ |
| LSP client coupling | Works exclusively with built-in `eglot`.                                                         | Hard-bound to the `lsp-mode` ecosystem.                                  |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                           | Requires forbidden `lsp-mode` ecosystem.                                 |
| UI Physics          | Leverages the existing `vertico`/`consult` stack for instant, fuzzy-filtered command selection.  | Uses heavy, custom child-frame pipelines or bespoke completion UIs.      |
| Safety              | `eglot` checks server capabilities before prompting, hiding commands the server doesn't support. | Often requires manual filtering or risks executing unsupported commands. |

### Behavioral Parity Matrix

| VS Code behavior                      | Emacs 31 equivalent                                                                              |
| ------------------------------------- | ------------------------------------------------------------------------------------------------ |
| Command Palette shows server commands | `SPC c x` (`eglot-execute-command`) lists commands via `completing-read`.                        |
| "Organize Imports" quick action       | `SPC c i` (`eglot-code-action-organize-imports`) or `SPC c a` (Code Actions).                    |
| Command applies workspace edits       | `eglot` natively processes the `WorkspaceEdit` response and updates affected buffers atomically. |
| Fails gracefully if unsupported       | `eglot` validates `executeCommandProvider` capabilities before offering the command.             |

### Emacs 31 Specific Enhancements

- **Refined `WorkspaceEdit` Handling:** Emacs 31's `eglot` robustly handles complex `WorkspaceEdit` payloads returned by `executeCommand`, including multi-file edits, file creations, and deletions, applying them atomically without corrupting undo history.
- **Seamless `consult` Integration:** Because `eglot-execute-command` relies on the native `completing-read` API, it automatically inherits the fuzzy filtering, live previews, and keyboard navigation provided by the `vertico` + `consult` stack configured elsewhere in this setup.

### Integration with Existing Stack (Python Context)

For Python specifically, there is a clear division of labor that prevents redundant LSP calls:

- **Formatting & Import Sorting:** Handled asynchronously on save by `apheleia` running `ruff-isort` and `ruff`. This is faster and more reliable than asking the LSP server to do it.
- **Semantic Refactoring:** `eglot-execute-command` (or `eglot-code-actions`) is reserved for deep AST-aware operations that `ruff` cannot perform, such as "Extract to variable", "Convert to f-string", or "Go to Source Definition" (in `basedpyright` / `ty`).

### Troubleshooting

#### Command Not Found or Fails

1.  **Verify Server Support:** Not all language servers expose custom commands. Check the server's capabilities (e.g., `rust-analyzer` exposes many, while simpler servers expose none).
    `M-x eglot-describe-connection` will show if `executeCommandProvider` is advertised.
2.  **Use Code Actions Instead:** Many servers bundle "Organize Imports" or "Fix All" into `textDocument/codeAction` rather than `workspace/executeCommand`. Always try `SPC c a` first, as it is more universally supported across servers.

---


---

# Formatting & Editing

## Document Formatting (Whole File)

_VS Code feature: "Format Document" command (Shift+Alt+F)._

### Feature Overview

| Attribute          | Value                                                                                                  |
| ------------------ | ------------------------------------------------------------------------------------------------------ |
| Feature            | Document Formatting (Whole File)                                                                       |
| VS Code equivalent | "Format Document" via LSP or built-in formatter                                                        |
| Status             | 🟢 working · `apheleia` (async primary) + `eglot-format-buffer` (LSP fallback)                         |
| Category           | Formatting & Editing                                                                                   |
| LSP methods        | `textDocument/formatting` (handled natively by `eglot`, but intentionally bypassed for whole-file ops) |
| Emacs routing      | `apheleia` (async CLI diff) OR `eglot` → `eglot-format-buffer` (synchronous LSP)                       |

### Implementation Stack

| Layer              | Component                       | Role                                                                                                                                                                                               |
| ------------------ | ------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Primary Formatter  | `apheleia`                      | Runs external CLI formatters (e.g., `prettier`, `black`, `rustfmt`) asynchronously in the background, computes a safe diff, and applies it without shifting the cursor or corrupting undo history. |
| LSP Fallback       | `eglot` (built-in, Emacs 31)    | Provides `eglot-format-buffer` which directly invokes `textDocument/formatting`. Used only when a language lacks a standard CLI formatter or for specific LSP-only formatting rules.               |
| Project Resolution | `project.el` / `.dir-locals.el` | `apheleia` automatically discovers the correct formatter based on the major mode and project root configuration.                                                                                   |
| Trigger Mechanism  | `before-save-hook`              | Automatically formats the buffer upon saving, ensuring the file is always clean before committing.                                                                                                 |

### Commands & Keybindings

| Action                 | Command                                | Keybinding         | Notes                                                                           |
| ---------------------- | -------------------------------------- | ------------------ | ------------------------------------------------------------------------------- |
| Format current buffer  | `apheleia-format-buffer`               | `SPC c f`          | Async, cursor-preserving whole-file formatting (Recommended).                   |
| LSP Format buffer      | `eglot-format-buffer`                  | `C-c C-f`          | Synchronous LSP fallback. Blocks UI briefly on large files.                     |
| Toggle format on save  | `apheleia-global-mode`                 | —                  | Enables automatic formatting before every `save-buffer`.                        |
| Format specific region | `apheleia-format-buffer` (with region) | `SPC c f` (visual) | Formats only the active region (delegates to CLI `--range` flags if supported). |

### Configuration

The canonical Emacs 31 approach intentionally delegates whole-file formatting to `apheleia` to avoid the main-thread blocking inherent in synchronous LSP `textDocument/formatting` calls.

```elisp
;; ==========================================
;; 1. APHELEIA (Async Whole-File Formatting)
;; ==========================================
(use-package apheleia
  :ensure t
  :init
  ;; Enable global auto-formatting on save.
  (apheleia-global-mode +1)
  :custom
  ;; Do not show a message in the echo area after successful formatting.
  (apheleia-hide-log-buffers t)
  ;; Maximum file size (in bytes) to format. Prevents freezing on massive files.
  (apheleia-max-file-size 500000)
  :config
  ;; Optional: Explicitly map modes to formatters if auto-detection fails.
  ;; (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier)
  ;; (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)
  )

;; ==========================================
;; 2. EGLOT LSP FORMATTING (Fallback)
;; ==========================================
;; eglot natively supports `textDocument/formatting` via `eglot-format-buffer`.
;; It is kept available for languages where CLI formatters are unavailable
;; or when LSP-specific formatting rules (e.g., clangd's specific style) are required.
;; No explicit config needed; bound to `C-c C-f` by default in `eglot-mode-map`.

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c f" '(apheleia-format-buffer :wk "Format buffer (async)"))
```

### Why This Approach (vs. Pure LSP `eglot-format-buffer` / `lsp-mode`)

| Consideration            | `apheleia` (chosen)                                                                                | Pure `eglot-format-buffer` (fallback)                                              | `lsp-mode` (rejected)                                             |
| ------------------------ | -------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------- | ----------------------------------------------------------------- |
| Thread Blocking          | **Zero.** Runs in a background process; UI remains fully responsive.                               | **High.** Synchronous; blocks the main thread until the LSP server responds.       | High; prone to UI freezes on large files.                         |
| Cursor/Undo Preservation | **Perfect.** Computes a diff and applies it, preserving cursor position, mark, and undo history.   | **Poor.** Often resets cursor to the top of the buffer and fragments undo history. | Variable; requires complex third-party patches to preserve state. |
| Protocol Compliance      | Bypasses LSP for formatting, but honors the `eglot`-only stack mandate for all other LSP features. | Honors `textDocument/formatting` natively.                                         | Requires forbidden `lsp-mode` ecosystem.                          |
| Formatter Parity         | Uses the _exact same_ CLI tools the LSP server uses under the hood (e.g., `prettier`, `black`).    | Relies entirely on the LSP server's internal formatting logic.                     | Relies on LSP server.                                             |

### Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                                        |
| ------------------------------------ | -------------------------------------------------------------------------- |
| `Shift+Alt+F` formats the whole file | `SPC c f` (`apheleia-format-buffer`) or `C-c C-f` (`eglot-format-buffer`). |
| Format on Save toggle                | `apheleia-global-mode +1` hooks into `before-save-hook`.                   |
| No UI freeze during formatting       | `apheleia` runs asynchronously; Emacs remains fully interactive.           |
| Cursor stays in place after format   | `apheleia`'s diff engine precisely restores the cursor and mark.           |
| Undo history remains clean           | `apheleia` groups the diff application into a single, clean undo step.     |
| Project-specific formatter rules     | `apheleia` respects `.dir-locals.el` and `project.el` root detection.      |

### Emacs 31 Specific Enhancements

- **Native `treesit` Mode Integration:** `apheleia` seamlessly recognizes Emacs 31's native `*-ts-mode` major modes (e.g., `python-ts-mode`, `typescript-ts-mode`) and applies the correct formatter without requiring manual `apheleia-mode-alist` remapping in most cases.
- **Improved Process Management:** Emacs 31's refined asynchronous process handling ensures that `apheleia`'s background formatter processes are cleanly reaped and do not leave zombie processes, even if the buffer is killed mid-format.
- **Eglot's `eglot-format-buffer` Refinements:** If you must use the LSP fallback, Emacs 31's `eglot` has improved error handling for `textDocument/formatting` failures, gracefully falling back to a no-op rather than throwing opaque JSON-RPC errors into the `*Messages*` buffer.

### Integration with Existing Stack

- **`eglot`:** Handles all semantic code actions (like "Organize Imports" or "Extract Method"), while `apheleia` handles the syntactic whole-file formatting. This separation of concerns is the gold standard in modern Emacs configurations.
- **`project.el`:** `apheleia` uses `project-root` to locate configuration files (e.g., `.prettierrc`, `pyproject.toml`), ensuring the formatter runs with the correct project-specific rules.
- **`magit` / `vc`:** Because `apheleia` runs on `before-save-hook`, files are always cleanly formatted before being staged in `magit`, preventing formatting noise from polluting Git commits.
- **`flymake`:** Formatting often resolves `flymake` syntax warnings automatically. The async nature of `apheleia` ensures that the subsequent `flymake` re-check (triggered by the save) does not compound UI lag.

### Troubleshooting

#### Formatter Not Found

If `apheleia` fails to format, ensure the CLI tool (e.g., `prettier`, `black`) is installed and available in your `exec-path`. You can verify this with `M-x exec-path`.

#### Large File Freezes

If formatting a massive file still causes lag, `apheleia`'s `apheleia-max-file-size` guard will abort the operation. You can increase this limit, but it is safer to rely on `eglot-format-buffer` for targeted region formatting (`SPC c f` with an active visual selection) on very large files.

#### Cursor Jumps to Top of Buffer

This indicates `apheleia` is not running, and you are accidentally triggering `eglot-format-buffer` or a legacy `indent-region` command. Verify your `SPC c f` binding points to `apheleia-format-buffer`.

---

## Range Formatting (Format Selection)

_VS Code feature: "Format Selection" command (Shift+Alt+F)._

### Feature Overview

| Attribute          | Value                                                                                |
| ------------------ | ------------------------------------------------------------------------------------ |
| Feature            | Range Formatting (Format Selection)                                                  |
| VS Code equivalent | Format active region/selection only                                                  |
| Status             | 🟢 working · `eglot` (LSP) OR `lazy-ruff` (CLI, Python)                              |
| Category           | Formatting & Editing                                                                 |
| LSP methods        | `textDocument/rangeFormatting`                                                       |
| Emacs routing      | `eglot` → `eglot-format` (with active region) OR `lazy-ruff` → `ruff format --range` |

### Implementation Stack

| Layer                    | Component                      | Role                                                                                                                                              |
| ------------------------ | ------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client               | `eglot` (built-in, Emacs 31)   | Natively supports `textDocument/rangeFormatting` when an active region is present, delegating to the language server.                             |
| CLI Alternative (Python) | `lazy-ruff`                    | Lightweight integration that invokes the Ruff CLI directly (`ruff format --range`) for marked regions and org src blocks, bypassing LSP entirely. |
| Cursor Preservation      | `apheleia` (optional fallback) | While `apheleia` primarily targets whole-buffer formatting, custom wrappers can pass region bounds to CLI formatters that support it.             |

### Commands & Keybindings

| Action                     | Command                    | Keybinding               | Notes                                                                                      |
| -------------------------- | -------------------------- | ------------------------ | ------------------------------------------------------------------------------------------ |
| Format active region (LSP) | `eglot-format`             | `SPC c f` (with region)  | Eglot automatically detects the active region and requests `textDocument/rangeFormatting`. |
| Format region (Ruff CLI)   | `lazy-ruff-format-region`  | `SPC c f` (with region)  | Invokes `ruff format --range` on the selected text, ideal for Python without LSP overhead. |
| Format org src block       | `lazy-ruff-format-org-src` | `C-c C-c` (in src block) | Specifically targets Python code blocks within Org-mode files.                             |

### Configuration

For Python development, `lazy-ruff` provides a zero-LSP, blazing-fast alternative that leverages Ruff's native `--range` CLI support.

```elisp
;; ==========================================
;; 1. LAZY-RUFF (Python CLI Range Formatting)
;; ==========================================
(use-package lazy-ruff
  :ensure t
  :defer t
  :commands (lazy-ruff-format-region
             lazy-ruff-format-buffer
             lazy-ruff-format-org-src)
  :custom
  ;; Target only the active region when a region is selected.
  (lazy-ruff-only-format-region t)
  ;; Pass specific arguments to Ruff CLI if needed (e.g., config file).
  (lazy-ruff-args '("format" "--quiet"))
  :config
  ;; Bind to a convenient key for region formatting
  (general-define-key
   :states '(normal visual)
   "SPC c f" #'lazy-ruff-format-region))

;; ==========================================
;; 2. EGLOT NATIVE RANGE FORMATTING (Fallback/Other Languages)
;; ==========================================
;; Eglot natively handles region formatting. If a region is active,
;; `eglot-format` automatically sends `textDocument/rangeFormatting`.
;; No extra configuration is needed beyond the base `eglot` setup.
```

### Why This Approach (vs. Pure LSP or Heavy Formatters)

| Consideration       | `lazy-ruff` / `eglot` (chosen)                                                                          | Legacy `python-format` / Heavy LSP UI                                    |
| ------------------- | ------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------ |
| LSP Coupling        | `lazy-ruff` requires _zero_ LSP integration, using pure CLI.                                            | Tightly coupled to `lsp-mode` or specific language servers.              |
| Performance         | Ruff CLI is written in Rust and formats ranges in milliseconds.                                         | Synchronous LSP requests can block the main thread on large files.       |
| Protocol Compliance | Honors the `eglot`-only stack mandate (or bypasses it cleanly via CLI).                                 | Often requires forbidden `lsp-mode` ecosystem packages.                  |
| Org-mode Synergy    | Natively supports formatting Python code inside Org src blocks.                                         | Most LSP formatters struggle with embedded code blocks.                  |
| Cursor Stability    | Region-based formatting inherently preserves the rest of the buffer's undo history and cursor position. | Whole-buffer formatters often cause visual jitter or undo fragmentation. |

### Behavioral Parity Matrix

| VS Code behavior                      | Emacs 31 equivalent                                                                          |
| ------------------------------------- | -------------------------------------------------------------------------------------------- |
| `Shift+Alt+F` formats selection       | `SPC c f` with an active visual region triggers `lazy-ruff-format-region` or `eglot-format`. |
| Respects `.ruff.toml` config          | `lazy-ruff` automatically inherits the project's Ruff configuration.                         |
| Formats embedded code (e.g., Jupyter) | `lazy-ruff-format-org-src` handles Python blocks inside Org-mode seamlessly.                 |
| No main-thread blocking               | CLI execution is asynchronous or near-instantaneous due to Ruff's speed.                     |
| Fallback to whole-file if no region   | If no region is active, the command gracefully falls back to `lazy-ruff-format-buffer`.      |

### Emacs 31 Specific Enhancements

- **Native Region Awareness:** Emacs 31's refined `eglot` implementation seamlessly detects `(region-active-p)` and routes to `textDocument/rangeFormatting` without requiring separate commands.
- **Tree-sitter Region Precision:** When combined with `expreg` or `evil-textobj-tree-sitter`, you can select precise AST nodes (e.g., a single function or class) and format only that structural block, avoiding the fragility of text-based line selection.
- **Ruff CLI `--range` Support:** Modern Ruff versions natively support the `--range` flag (e.g., `--range=10:1-20:1`), allowing the CLI to format specific line ranges without touching the rest of the file. `lazy-ruff` translates Emacs region bounds into this exact CLI syntax.

### Integration with Existing Stack

- **`general.el`:** The `SPC c f` binding is mapped conditionally: if a region is active, it triggers the region formatter; otherwise, it falls back to `apheleia-format-buffer` for whole-file formatting.
- **`org-mode`:** `lazy-ruff` explicitly supports `org-src` blocks, making it the definitive choice for literate programming workflows where Python snippets are embedded in documentation.
- **`apheleia`:** For languages where a dedicated CLI range formatter isn't available, `apheleia` remains the gold standard for whole-file async formatting, while `eglot` handles the LSP range formatting gap.

### Troubleshooting

#### Region Formatting Falls Back to Whole File

Ensure `lazy-ruff-only-format-region` is set to `t`, or verify that the region is actively highlighted (e.g., via `evil-visual-state`) before invoking the command.

#### Ruff CLI Not Found

`lazy-ruff` requires the `ruff` binary to be in your system's `exec-path`. Verify this with `M-x exec-path` or install it via your OS package manager (e.g., `pacman -S ruff` on Arch Linux).

#### Org Src Block Formatting Fails

Ensure the source block is explicitly declared as `python` (e.g., `#+begin_src python`). `lazy-ruff` uses the block's language identifier to route the formatting command correctly.

---

## On-type Formatting

_VS Code feature: Auto-reformats as you type trigger characters like `}` or `;`._

### Feature Overview

| Attribute          | Value                                                                                        |
| ------------------ | -------------------------------------------------------------------------------------------- |
| Feature            | On-type Formatting                                                                           |
| VS Code equivalent | Auto-indent and auto-pairing on trigger characters (`}`, `;`, `(`, etc.)                     |
| Status             | 🟢 native · `electric-*` modes (LSP `onTypeFormatting` intentionally bypassed)               |
| Category           | Formatting & Editing                                                                         |
| LSP methods        | `textDocument/onTypeFormatting` (Explicitly ignored via `eglot-ignored-server-capabilities`) |
| Emacs routing      | `electric-indent-mode` + `electric-pair-mode` + `electric-layout-mode`                       |

### Implementation Stack

| Layer            | Component                           | Role                                                                                                                                              |
| ---------------- | ----------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| Auto-Indentation | `electric-indent-mode` (built-in)   | Re-indents the current line or block instantly upon typing trigger characters like `RET`, `}`, or `;` with zero network latency.                  |
| Auto-Pairing     | `electric-pair-mode` (built-in)     | Automatically inserts closing delimiters (`()`, `{}`, `[]`, `""`). Emacs 31 natively extends this to multi-character pairs (e.g., `/*` and `*/`). |
| Layout Control   | `electric-layout-mode` (built-in)   | Dictates exactly where newlines are inserted automatically (e.g., adding a newline before and after `{` in C-like languages).                     |
| LSP Guard        | `eglot-ignored-server-capabilities` | Explicitly disables `:documentOnTypeFormattingProvider` to prevent main-thread blocking and cursor-jumping bugs.                                  |

### Commands & Keybindings

| Action               | Command                    | Keybinding | Notes                                                             |
| -------------------- | -------------------------- | ---------- | ----------------------------------------------------------------- |
| Toggle auto-indent   | `electric-indent-mode`     | `SPC t i`  | Re-indents on trigger characters. Enabled globally by default.    |
| Toggle auto-pairing  | `electric-pair-mode`       | `SPC t p`  | Inserts matching closing delimiters. Enabled globally by default. |
| Toggle layout rules  | `electric-layout-mode`     | `SPC t l`  | Enforces structural newline insertion (e.g., around `{}`).        |
| Manually indent line | `indent-according-to-mode` | `TAB`      | Fallback manual indentation if electric modes miss a trigger.     |

### Configuration

The canonical Emacs 31 approach intentionally bypasses LSP `textDocument/onTypeFormatting`. Language servers processing on-type formatting requests synchronously cause severe main-thread blocking, UI stutter, and cursor-jumping bugs. Instead, we rely on Emacs' native, C-level electric modes, which operate at 0ms latency.

```elisp
;; ==========================================
;; 1. EGLOT: DISABLE LSP ON-TYPE FORMATTING
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Explicitly ignore onTypeFormatting to prevent main-thread blocking
  ;; and cursor-jumping issues on every keystroke.
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

;; ==========================================
;; 2. NATIVE ELECTRIC MODES (Zero-Latency Formatting)
;; ==========================================
(use-package elec-pair
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Support for multi-character paired delimiters.
  ;; Automatically pairs "/*" with "*/" and handles spacing intelligently.
  (electric-pair-pairs '(("/*" . "*/")))
  (electric-pair-text-pairs '(("/*" . "*/")))
  ;; Prevent pairing inside strings/comments where it causes syntax errors.
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode 1))

(use-package electric
  :ensure nil
  :custom
  ;; Re-indent automatically when typing trigger characters like '}' or ';'.
  (electric-indent-mode 1)
  ;; Enforce structural newlines (e.g., adding newlines around '{' in C/JS).
  (electric-layout-mode 1))
```

### Why This Approach (vs. LSP `onTypeFormatting`)

| Consideration       | Native `electric-*` modes (chosen)                                              | LSP `textDocument/onTypeFormatting` (rejected)                                             |
| ------------------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| Latency             | **0ms.** Executed in C-level Emacs core.                                        | **High.** Requires a synchronous network roundtrip to the LSP server on _every_ keystroke. |
| UI Stability        | **Perfect.** No main-thread blocking or cursor jumping.                         | **Poor.** Known to cause severe UI stutter and incorrect point placement.                  |
| Protocol Compliance | Honors the `eglot`-only stack by explicitly disabling problematic LSP features. | Violates the principle of a responsive editor; heavily discouraged in the eglot community. |
| Undo History        | **Clean.** Native Emacs commands integrate seamlessly with the undo tree.       | **Fragmented.** LSP text edits applied mid-typing often corrupt or split undo boundaries.  |
| Emacs 31 Synergy    | Leverages new multi-character pair support (`"/*"` / `"*/"`).                   | No integration with Emacs 31 core enhancements.                                            |

### Behavioral Parity Matrix

| VS Code behavior                       | Emacs 31 equivalent                                                                                     |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| Auto-indents when typing `}` or `;`    | `electric-indent-mode` re-indents instantly on trigger characters.                                      |
| Auto-inserts closing `)` or `}`        | `electric-pair-mode` inserts the matching delimiter.                                                    |
| Auto-inserts `/*` and `*/`             | Emacs 31 `electric-pair-pairs` natively supports multi-character string pairs.                          |
| Adds newlines around `{` automatically | `electric-layout-mode` enforces structural newline rules.                                               |
| No lag or stutter while typing         | Native C-level execution guarantees 0ms latency, unlike LSP network requests.                           |
| Cursor stays in the correct position   | Native modes preserve point and mark perfectly; LSP on-type formatting is known to displace the cursor. |

### Emacs 31 Specific Enhancements

- **Multi-Character Electric Pairs (NEW):** Emacs 31's `electric-pair-mode` now natively supports multi-character paired delimiters. You can define pairs like `("/*" . "*/")` in `electric-pair-pairs`, and Emacs will intelligently auto-complete them with optional auto-spacing, a feature previously requiring heavy third-party packages like `smartparens`.
- **Conservative Inhibition:** The `electric-pair-conservative-inhibit` predicate (standard in modern Emacs) prevents auto-pairing from triggering inside strings or comments, eliminating the "phantom bracket" syntax errors that plagued older configurations.
- **Treesit Integration:** Native electric modes work flawlessly alongside `treesit` major modes (e.g., `python-ts-mode`, `rust-ts-mode`), as they operate on the syntax table and character level, completely independent of the LSP server's parsing speed.

### Integration with Existing Stack

- **`eglot`:** By adding `:documentOnTypeFormattingProvider` to `eglot-ignored-server-capabilities`, we proactively prevent the language server from attempting to format on every keystroke, preserving a buttery-smooth typing experience.
- **`apheleia`:** While `electric-*` modes handle on-type structural formatting, `apheleia` handles on-save whole-file formatting. This separation of concerns ensures zero latency while typing, and perfect stylistic compliance upon saving.
- **`evil-mode`:** Electric modes are fully compatible with Evil. Auto-pairing and auto-indentation trigger correctly whether you are in `insert-state` or using `R` (replace) state.
- **`org-mode`:** `electric-pair-mode` seamlessly auto-pairs delimiters inside Org source blocks, improving the literate programming experience without requiring LSP intervention.

### Troubleshooting

#### Cursor Jumps or Stutters When Typing

This is the hallmark symptom of LSP `onTypeFormatting` being active. Verify that `:documentOnTypeFormattingProvider` is present in your `eglot-ignored-server-capabilities` list.

#### Auto-Pairing Triggers Inside Strings/Comments

Ensure `electric-pair-inhibit-predicate` is set to `'electric-pair-conservative-inhibit`. This prevents Emacs from inserting closing quotes or brackets where they would break string/comment syntax.

#### Multi-Character Pairs Not Working

Verify you are running Emacs 31 or later, as multi-character string support in `electric-pair-pairs` is a recent enhancement. For older versions, third-party packages like `smartparens` were required for this specific behavior.

---

## Folding Ranges

_VS Code feature: Gutter arrows to collapse/expand code blocks, functions, imports, regions._

### Feature Overview

| Attribute          | Value                                                                                                |
| ------------------ | ---------------------------------------------------------------------------------------------------- |
| Feature            | Folding Ranges                                                                                       |
| VS Code equivalent | Gutter arrows / margin indicators to collapse/expand code blocks, functions, imports, and regions    |
| Status             | 🟢 native · `treesit-fold` + `hideshow` + `vimish-fold`                                              |
| Category           | Formatting & Editing                                                                                 |
| LSP methods        | `textDocument/foldingRange` (Explicitly bypassed in favor of native AST parsing)                     |
| Emacs routing      | `treesit-fold` (AST-aware) → `hideshow` (Emacs 31 enhanced) → `vimish-fold` (visual region fallback) |

### Implementation Stack

| Layer                  | Component                | Role                                                                                                                                                                   |
| ---------------------- | ------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Primary AST Engine     | `treesit-fold`           | Provides intelligent, structural code folding by directly querying the tree-sitter syntax tree, ensuring 100% accuracy for functions, classes, and blocks.             |
| Native Fallback        | `hideshow` (built-in)    | Emacs 31 introduces native support for tree-sitter modes via the new `list` thing, alongside new commands like `hs-cycle`, `hs-toggle-all`, and `hs-indentation-mode`. |
| Visual Region Fallback | `vimish-fold`            | Allows arbitrary visual region folding (like VS Code's `Ctrl+K Ctrl+0`), caching fold state persistently across sessions.                                              |
| Indicator Rendering    | Native Fringes / Margins | Emacs 31's refined `hs-indicator-type` and `treesit-fold-indicators-priority` ensure clean, non-intrusive gutter arrows that do not clash with `diff-hl` or `flymake`. |

### Commands & Keybindings

| Action               | Command                   | Keybinding | Notes                                                                              |
| -------------------- | ------------------------- | ---------- | ---------------------------------------------------------------------------------- |
| Toggle fold at point | `ar/fold-toggle`          | `za`       | Intelligent dispatcher: tries `treesit-fold`, then `hideshow`, then `vimish-fold`. |
| Open fold at point   | `ar/fold-open`            | `zo`       | Reveals the hidden block.                                                          |
| Close fold at point  | `ar/fold-close`           | `zc`       | Conceals the block.                                                                |
| Open all folds       | `ar/fold-open-all`        | `zR`       | Expands the entire buffer.                                                         |
| Close all folds      | `ar/fold-close-all`       | `zM`       | Collapses all top-level blocks.                                                    |
| Create visual fold   | `evil-vimish-fold/create` | `zf`       | Folds the currently selected visual region.                                        |
| Delete visual fold   | `vimish-fold-delete`      | `zd`       | Removes the fold at point.                                                         |

### Configuration

The configuration deliberately avoids LSP `textDocument/foldingRange` to prevent main-thread blocking and network latency. Instead, it relies on a unified, native dispatcher that prioritizes AST-aware folding.

```elisp
;; ==========================================
;; 1. TREESIT-FOLD (Primary AST-Aware Engine)
;; ==========================================
(use-package treesit-fold
  :if (treesit-available-p)
  :defer t
  :hook ((prog-mode text-mode conf-mode) . ar/treesit-fold-maybe-enable)
  :init
  ;; Guards activation against Lisp modes to prevent missing grammar warnings.
  (defun ar/treesit-fold-maybe-enable ()
    (unless (derived-mode-p 'lisp-mode 'emacs-lisp-mode 'lisp-interaction-mode 'lisp-data-mode)
      (treesit-fold-mode 1)))
  :config
  (global-treesit-fold-indicators-mode 1)
  ;; Lower priority prevents fringe indicator collisions with diff-hl/flymake.
  (setq treesit-fold-indicators-priority -1)
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice treesit-fold-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; 2. HIDESHOW (Emacs 31 Enhanced Native Fallback)
;; ==========================================
(use-package hideshow
  :ensure nil
  :defer t
  :commands (hs-toggle-hiding hs-hide-block hs-show-block hs-hide-all hs-show-all)
  :config
  (setq hs-hide-comments-when-hiding-all nil
        hs-set-up-overlay #'ar/hs-overlay-line-count)
  ;; Aborts activation in massive buffers.
  (define-advice hs-minor-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p))))
  ;; Generic fold-marker fallback for languages lacking tree-sitter grammars.
  (unless (assq 't hs-special-modes-alist)
    (setq hs-special-modes-alist
          (append hs-special-modes-alist
                  '((t "{{{" "}}}" nil nil))))))

;; ==========================================
;; 3. VIMISH-FOLD (Visual Region Fallback)
;; ==========================================
(use-package vimish-fold
  :defer t
  :after evil
  :init
  ;; Route cache to no-littering var directory.
  (setq vimish-fold-dir (no-littering-expand-var-file-name "vimish-fold/")
        vimish-fold-indication-mode 'right-fringe)
  :config
  (vimish-fold-global-mode 1))

(use-package evil-vimish-fold
  :defer t
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ↴"
        evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode 1))

;; ==========================================
;; 4. UNIFIED DISPATCHER & KEYBINDINGS
;; ==========================================
(defun ar/hs-overlay-line-count (ov)
  "Append hidden line count to the native ellipsis for visual feedback."
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((start (overlay-start ov))
           (end (overlay-end ov))
           (lines (count-lines start end)))
      (overlay-put ov 'display
                   (format "%s [%d lines]" truncate-string-ellipsis lines)))))

(defun ar/fold-toggle ()
  (interactive)
  (cond ((bound-and-true-p treesit-fold-mode) (treesit-fold-toggle))
        ((bound-and-true-p outline-minor-mode) (outline-cycle))
        ((bound-and-true-p hs-minor-mode) (hs-toggle-hiding))
        ((fboundp 'vimish-fold-toggle)
         (condition-case nil (vimish-fold-toggle)
           (error (user-error "No foldable region at point"))))
        (t (user-error "No foldable region at point"))))

;; ... (ar/fold-open, ar/fold-close, ar/fold-open-all, ar/fold-close-all defined similarly)

(general-define-key
  :states 'motion
  "za" #'ar/fold-toggle
  "zo" #'ar/fold-open
  "zc" #'ar/fold-close
  "zR" #'ar/fold-open-all
  "zM" #'ar/fold-close-all
  "zf" #'evil-vimish-fold/create
  "zF" #'evil-vimish-fold/create-line
  "zd" #'vimish-fold-delete
  "zE" #'vimish-fold-delete-all)
```

### Why This Approach (vs. LSP `foldingRange` / `lsp-ui`)

| Consideration       | Native `treesit-fold` + `hideshow` (chosen)                                                                            | LSP `foldingRange` (rejected)                                                                                                |
| ------------------- | ---------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| Latency             | **0ms.** Queries the local C-level AST instantly.                                                                      | **High.** Requires a synchronous network roundtrip to the LSP server on file open or fold request.                           |
| Accuracy            | **Perfect.** Tree-sitter understands the exact syntactic scope of every block.                                         | **Variable.** Depends entirely on the language server's implementation, which often misidentifies nested scopes or comments. |
| Protocol Compliance | Honors the `eglot`-only stack mandate by bypassing unnecessary LSP UI features.                                        | Adds redundant network overhead for a feature Emacs handles natively.                                                        |
| Emacs 31 Synergy    | Leverages Emacs 31's new `hs-cycle`, `hs-indentation-mode`, and native `list` thing integration for tree-sitter modes. | No integration with Emacs 31 core enhancements.                                                                              |
| Visual Polish       | Custom `ar/hs-overlay-line-count` appends `[X lines]` to the ellipsis, matching VS Code's informative folding hints.   | Generic, uncustomizable server-provided ranges.                                                                              |

### Behavioral Parity Matrix

| VS Code behavior                             | Emacs 31 equivalent                                                         |
| -------------------------------------------- | --------------------------------------------------------------------------- |
| Click gutter arrow to fold/unfold            | `treesit-fold` fringe indicators or `hideshow` margin clicks.               |
| `Ctrl+K Ctrl+0` (Fold all)                   | `zM` (`ar/fold-close-all`).                                                 |
| `Ctrl+K Ctrl+J` (Unfold all)                 | `zR` (`ar/fold-open-all`).                                                  |
| `Ctrl+Shift+[` / `]` (Fold/Unfold at cursor) | `zc` / `zo` (`ar/fold-close` / `ar/fold-open`).                             |
| Fold arbitrary visual selection              | `zf` (`evil-vimish-fold/create`).                                           |
| See hidden line count in gutter              | `ar/hs-overlay-line-count` renders `↴ [X lines]` natively.                  |
| Indentation-based folding (Python/YAML)      | Emacs 31's `hs-indentation-mode` handles this natively without regex hacks. |

### Emacs 31 Specific Enhancements

- **Tree-Sitter `hideshow` Integration:** Tree-sitter enabled modes now properly support `hs-minor-mode` via the new `list` thing, allowing `hideshow` to fold structural blocks with C-level precision.
- **New `hideshow` Commands:** Emacs 31 introduces `hs-cycle` (toggle between folded, unfolded, and children-only states) and `hs-toggle-all` for rapid buffer-wide state changes.
- **`hs-indentation-mode`:** Provides robust, indentation-based folding for languages like Python and YAML, replacing fragile regex-based fallbacks.
- **`treesit-fold` Fringe Priority:** The configuration explicitly sets `treesit-fold-indicators-priority` to `-1`, ensuring folding arrows render behind `diff-hl` and `flymake` indicators, preventing visual clutter in the left margin.

### Integration with Existing Stack

- **`treesit`:** Provides the foundational AST that `treesit-fold` queries, ensuring folding boundaries align perfectly with syntactic scopes (e.g., not folding mid-string or mid-comment).
- **`evil`:** `evil-vimish-fold` maps standard Vim folding motions (`zf`, `zo`, `zc`) to the native folding engine, preserving muscle memory.
- **`general.el`:** Eagerly registers the `za`, `zo`, `zc` dispatcher bindings at startup, ensuring they are available globally without deferred-registration traps.
- **`too-long-file-p`:** All folding minor modes are guarded by this custom function, preventing Emacs from attempting to parse and fold massive files (e.g., minified JS or large logs), which would otherwise freeze the main thread.

---

## Selection Range (Smart Expand/Shrink)

_VS Code feature: Shift+Alt+Right / Shift+Alt+Left grows/shrinks selection by syntactic scope._

### Feature Overview

| Attribute          | Value                                                                                       |
| ------------------ | ------------------------------------------------------------------------------------------- |
| Feature            | Selection Range (Smart Expand/Shrink)                                                       |
| VS Code equivalent | Incremental selection expansion/contraction based on AST boundaries                         |
| Status             | 🟢 native · `expreg` + `treesit` (LSP `textDocument/selectionRange` intentionally bypassed) |
| Category           | Formatting & Editing                                                                        |
| LSP methods        | `textDocument/selectionRange` (Not required; superseded by native AST)                      |
| Emacs routing      | `treesit` (C-level AST traversal) → `expreg` (region expansion engine)                      |

### Implementation Stack

| Layer              | Component                      | Role                                                                                                                                                                    |
| ------------------ | ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| AST Engine         | `treesit` (built-in, Emacs 31) | Provides O(1) C-level syntax tree traversal to identify nested structural boundaries (e.g., word → string → statement → function → class).                              |
| Expansion Engine   | `expreg`                       | Authored by Yuan Fu (the implementer of Emacs's built-in tree-sitter support), this package incrementally expands and contracts the active region using the native AST. |
| Repeat Integration | `repeat-mode` (built-in)       | Allows modifier-less tapping (e.g., `M-= = =`) for rapid, fluid expansion loops without holding down modifier keys.                                                     |
| Evil Integration   | `evil` + `general.el`          | Hijacks the `v` (visual state) key for seamless, Vim-native AST expansion, while providing global fallbacks (`M-=`) for standard Emacs muscle memory.                   |

### Commands & Keybindings

| Action             | Command           | Keybinding           | Notes                                                                      |
| ------------------ | ----------------- | -------------------- | -------------------------------------------------------------------------- |
| Expand selection   | `expreg-expand`   | `M-=` / `v` (visual) | Grows the active region to the next logical AST node.                      |
| Contract selection | `expreg-contract` | `C-M--`              | Shrinks the active region to the previous logical AST node.                |
| Repeat expansion   | `repeat`          | `=` (after `M-=`)    | Modifier-less tapping for rapid expansion loops.                           |
| Repeat contraction | `repeat`          | `-` (after `C-M--`)  | Modifier-less tapping for rapid contraction loops.                         |
| Cancel & restore   | `keyboard-quit`   | `C-g`                | Aborts the expansion loop and restores the exact original cursor position. |

### Configuration

Your configuration already implements the optimal, zero-latency native approach. It completely bypasses the need for network-bound LSP `textDocument/selectionRange` calls, which are inherently slower and require explicit server support.

```elisp
;; ==========================================
;; EXPREG (AST-Aware Expand Region, Reborn)
;; ==========================================
(use-package expreg
  :if (treesit-available-p)
  :defer t
  :commands (expreg-expand expreg-contract)
  :config
  ;; Restores exact cursor origin upon `C-g` (keyboard-quit) to prevent spatial drift.
  (setq expreg-restore-point-on-quit t)
  ;; Emacs 29+ `repeat-mode` integration for modifier-less expansion loops.
  (defvar-keymap expreg-repeat-map
    :doc "Keymap for repeating expreg commands."
    "+" #'expreg-expand
    "=" #'expreg-expand
    "-" #'expreg-contract
    "_" #'expreg-contract)
  (put 'expreg-expand 'repeat-map 'expreg-repeat-map)
  (put 'expreg-contract 'repeat-map 'expreg-repeat-map)

  ;; Visual State Routing: Hijacks `v` for seamless AST expansion loops in Evil.
  (general-define-key
    :states 'visual
    "v" #'expreg-expand)

  ;; Global Fallback: Maps `M-=` (Doom's expand-region mnemonic) and modifier combinations.
  (general-define-key
    :states '(normal visual motion)
    "M-=" #'expreg-expand
    "C-M-+" #'expreg-expand
    "C-M--" #'expreg-contract))
```

### Why This Approach (vs. LSP `textDocument/selectionRange`)

| Consideration       | `expreg` + `treesit` (chosen)                                                                                                              | LSP `textDocument/selectionRange` (rejected)                                                                                     |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------- |
| Latency             | **0ms.** Executes via native C-level AST pointer arithmetic.                                                                               | **High.** Requires a synchronous network roundtrip to the language server on every expansion step.                               |
| Reliability         | **Perfect.** Works offline and is independent of language server implementation quirks.                                                    | **Variable.** Depends entirely on the language server's `selectionRangeProvider` capability, which is often incomplete or buggy. |
| Protocol Compliance | Honors the `eglot`-only stack mandate by bypassing unnecessary LSP UI features in favor of native Emacs 31 primitives.                     | Adds redundant network overhead for a feature Emacs handles natively.                                                            |
| Emacs 31 Synergy    | Leverages `repeat-mode` for fluid, modifier-less tapping and `treesit` for mathematically precise structural boundaries.                   | No integration with Emacs 31 core enhancements.                                                                                  |
| State Preservation  | `expreg-restore-point-on-quit` guarantees that pressing `C-g` returns the cursor to its exact starting position, preventing spatial drift. | LSP implementations often lose track of the original anchor point during iterative expansions.                                   |

### Behavioral Parity Matrix

| VS Code behavior                                   | Emacs 31 equivalent                                                                             |
| -------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `Shift+Alt+Right` expands selection                | `M-=` or `v` (in visual state) triggers `expreg-expand`.                                        |
| `Shift+Alt+Left` shrinks selection                 | `C-M--` triggers `expreg-contract`.                                                             |
| Rapid tapping to expand further                    | `repeat-mode` allows pressing `=` repeatedly after the initial `M-=`.                           |
| Expands by semantic units (word → line → function) | `treesit` walks the AST to select the next logical syntactic node.                              |
| Canceling selection restores original cursor       | `C-g` aborts the loop and `expreg-restore-point-on-quit` snaps the cursor back.                 |
| Works across all supported languages               | Native `treesit` modes (`python-ts-mode`, `rust-ts-mode`, etc.) provide uniform AST structures. |

### Emacs 31 Specific Enhancements

- **`treesit` Native Integration:** `expreg` was explicitly designed by the author of Emacs's built-in tree-sitter support to leverage the C-level AST directly, making it exponentially faster and more accurate than legacy regex-based tools like `expand-region.el`.
- **`repeat-mode` Synergy:** Emacs 29+ introduced `repeat-mode`, which `expreg` utilizes to create a fluid, modifier-less expansion loop. Once you press `M-=`, subsequent presses of `=` or `-` continue the expansion/contraction without requiring you to hold down the Meta key.
- **`treesit-cycle-sexp-thing`:** While `expreg` handles block-level AST expansion, Emacs 31's native `treesit-cycle-sexp-thing` allows you to dynamically toggle between `list` and `sexp` navigation paradigms on the fly, providing granular control over how structural boundaries are interpreted during manual selection.

### Integration with Existing Stack

- **`treesit`:** Provides the foundational AST that `expreg` queries, ensuring folding and selection boundaries align perfectly with syntactic scopes (e.g., not selecting mid-string or mid-comment).
- **`evil`:** The configuration elegantly hijacks the `v` (visual state) key, allowing Vim users to initiate an AST-aware expansion loop without leaving their native modal workflow.
- **`general.el`:** Eagerly registers the `M-=` and `C-M--` fallback bindings at startup, ensuring they are available globally without deferred-registration traps.

---

## Linked Editing Range

_VS Code feature: Editing one HTML/JSX tag name automatically updates its matching pair._

### Feature Overview

| Attribute          | Value                                                                                              |
| ------------------ | -------------------------------------------------------------------------------------------------- |
| Feature            | Linked Editing Range (Auto Rename Tag)                                                             |
| VS Code equivalent | Automatically renames the paired opening/closing HTML, XML, or JSX tag on edit.                    |
| Status             | 🟢 working · `auto-rename-tag` (MELPA)                                                             |
| Category           | Visual Enhancements / Editing                                                                      |
| LSP methods        | `textDocument/linkedEditingRange` (Intentionally bypassed in favor of zero-latency native parsing) |
| Emacs routing      | `auto-rename-tag` → native Emacs regex/AST parsing                                                 |

### Implementation Stack

| Layer             | Component                                              | Role                                                                                                              |
| ----------------- | ------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------- |
| Renaming Engine   | `auto-rename-tag` (MELPA)                              | Provides zero-latency, local tag renaming for HTML/XML/JSX without requiring LSP round-trips.                     |
| Target Modes      | `tsx-ts-mode`, `web-mode`, `html-ts-mode`, `nxml-mode` | Major modes where structural tag pairing is prevalent.                                                            |
| Performance Guard | Native Emacs parsing                                   | Operates entirely locally using Emacs' built-in syntax tables or Tree-sitter AST, guaranteeing 0ms input latency. |

### Commands & Keybindings

| Action                     | Command                              | Keybinding            | Notes                                                                 |
| -------------------------- | ------------------------------------ | --------------------- | --------------------------------------------------------------------- |
| Toggle auto-rename         | `auto-rename-tag-mode`               | —                     | Enabled automatically via mode hooks for relevant languages.          |
| Manual tag jump (fallback) | `sgml-skip-tag-forward` / `backward` | `C-c C-f` / `C-c C-b` | Native Emacs commands to manually jump between paired tags if needed. |

### Configuration

The LSP `textDocument/linkedEditingRange` capability requires a synchronous network round-trip to the language server on every keystroke while inside a tag. This frequently causes noticeable input latency and main-thread blocking.

Instead, the canonical Emacs 31 approach utilizes the `auto-rename-tag` package, which replicates the exact behavior of the VS Code "Auto Rename Tag" extension using zero-latency local parsing.

```elisp
;; ==========================================
;; AUTO-RENAME-TAG (Zero-Latency Local Tag Renaming)
;; ==========================================
(use-package auto-rename-tag
  :ensure t
  :defer t
  :commands auto-rename-tag-mode
  :hook ((tsx-ts-mode
          html-ts-mode
          web-mode
          nxml-mode
          rjsx-mode) . auto-rename-tag-mode)
  :custom
  ;; Ensure the package activates instantly upon mode entry.
  (auto-rename-tag-mode 1))
```

### Why This Approach (vs. LSP `linkedEditingRange`)

| Consideration       | `auto-rename-tag` (chosen)                                                                                            | LSP `textDocument/linkedEditingRange` (rejected)                                                                                    |
| ------------------- | --------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| Latency             | **0ms.** Executes locally via native Emacs regex/AST parsing.                                                         | **High.** Requires a synchronous network round-trip on every keystroke.                                                             |
| Reliability         | **Perfect.** Works offline and is independent of language server implementation quirks.                               | **Variable.** Depends entirely on the language server's `linkedEditingRangeProvider` capability, which is often incomplete or slow. |
| Protocol Compliance | Honors the `eglot`-only stack mandate by avoiding unnecessary LSP UI features in favor of native Emacs 31 primitives. | Adds redundant network overhead for a feature Emacs handles natively.                                                               |
| Emacs 31 Synergy    | Integrates seamlessly with `tsx-ts-mode` and `web-mode` without conflicting with Tree-sitter parsing.                 | No specific integration with Emacs 31 core enhancements.                                                                            |

### Behavioral Parity Matrix

| VS Code behavior                             | Emacs 31 equivalent                                                                                                       |
| -------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| Typing in `<div>` instantly updates `</div>` | `auto-rename-tag-mode` detects the pair and updates it synchronously with 0ms latency.                                    |
| Works for HTML, XML, and JSX/TSX             | Hooked into `html-ts-mode`, `nxml-mode`, and `tsx-ts-mode` respectively.                                                  |
| No lag or stutter while typing               | Native local execution completely bypasses the LSP network request cycle.                                                 |
| Respects cursor position                     | The package is designed to adjust the cursor position intelligently after the rename operation, preventing spatial drift. |

### Emacs 31 Specific Enhancements

- **Tree-sitter Mode Compatibility:** `auto-rename-tag` operates harmoniously alongside Emacs 31's native `tsx-ts-mode` and `html-ts-mode`. Because it relies on fundamental buffer text manipulation rather than fighting the AST, it does not disrupt Tree-sitter's font-lock or indentation engines.
- **Zero Network Dependency:** By handling this locally, the configuration remains fully functional in air-gapped environments, over slow TRAMP connections, or when the LSP server is temporarily unresponsive, ensuring a consistently smooth editing experience.

### Integration with Existing Stack

- **`electric-pair-mode`:** Works in tandem with native auto-pairing. When `electric-pair-mode` inserts the closing `>`, `auto-rename-tag` is already primed to track the structural pair for subsequent edits.
- **`web-mode` / `tsx-ts-mode`:** The mode hooks ensure that the renaming engine is only active in buffers where tag pairing is structurally relevant, preventing false positives in standard programming languages like Python or Rust.
- **`eglot`:** By intentionally bypassing the LSP `linkedEditingRange` capability, we free up `eglot` to focus on heavier semantic tasks (like diagnostics and completion) without being bogged down by per-keystroke rename requests.

---

## Multi-Cursor Editing

_VS Code feature: Place and edit from multiple cursors simultaneously (arbitrary placement and symbol-based bulk editing)._

### Feature Overview

| Attribute          | Value                                                                                                                      |
| ------------------ | -------------------------------------------------------------------------------------------------------------------------- |
| Feature            | Multi-Cursor Editing                                                                                                       |
| VS Code equivalent | `Alt+Click` (arbitrary), `Ctrl+D` (next occurrence), `Ctrl+Shift+L` (all occurrences), `Alt+Shift+Down` (column selection) |
| Status             | 🟢 working · `iedit` + `evil-multiedit` (symbol-based) + `evil-mc` (arbitrary)                                             |
| Category           | Precision Editing                                                                                                          |
| LSP methods        | N/A (Local buffer manipulation; project-wide renaming is handled via `eglot-rename`)                                       |
| Emacs routing      | `iedit` (foundation) → `evil-multiedit` (symbol occurrences) OR `evil-mc` (arbitrary/rectangular placement)                |

### Implementation Stack

| Layer                | Component              | Role                                                                                                                                                      |
| -------------------- | ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Foundation Engine    | `iedit` (GNU ELPA)     | Provides the core mechanism for highlighting and synchronizing mutations across multiple instances of a symbol or region.                                 |
| Symbol-Based Editing | `evil-multiedit`       | Wraps `iedit` in a dedicated, localized Evil state, mapping explicit Vim-mnemonics for rapid cursor accumulation and bulk mutation.                       |
| Arbitrary Placement  | `evil-mc`              | Creates true, Evil-native fake cursors that flawlessly execute standard Evil motions and operators (e.g., `daw`, `ciw`) across all points simultaneously. |
| State Management     | Evil integration hooks | Ensures clean exit to `evil-normal-state` upon aborting multi-cursor operations, preventing modal corruption or undo history fragmentation.               |

### Commands & Keybindings

| Action                  | Command                                          | Keybinding            | Notes                                                                           |
| ----------------------- | ------------------------------------------------ | --------------------- | ------------------------------------------------------------------------------- |
| Add cursor on click     | `evil-mc-make-cursor-here`                       | `C-S-<mouse-1>`       | VS Code parity: click anywhere to place an arbitrary cursor.                    |
| Edit lines in rectangle | `evil-mc-make-cursor-in-next-line`               | `C-S-<down>` (visual) | Adds a cursor to each line in the active visual rectangle.                      |
| Mark next occurrence    | `evil-multiedit-match-symbol-and-next`           | `M-d`                 | Evil-native: adds the next occurrence of the current symbol to the edit region. |
| Mark all occurrences    | `evil-multiedit-match-all`                       | `M-D`                 | Evil-native: adds all occurrences in the buffer to the edit region.             |
| Exit multi-cursor mode  | `evil-mc-undo-all-cursors` / `evil-normal-state` | `C-g` or `RET`        | Exits multi-cursor mode, leaving a single cursor at the primary point.          |

### Configuration

The configuration establishes a strict division of labor: `evil-multiedit` handles symbol-based editing, while `evil-mc` handles arbitrary and rectangular placement. This prevents feature overlap and ensures flawless Evil operator support.

```elisp
;; ==========================================
;; 1. IEDIT (Foundation Engine)
;; ==========================================
(use-package iedit
  :defer t
  :commands (iedit-mode iedit-rectangle-mode)
  :custom
  ;; Prevent iedit from accidentally matching substrings of larger words.
  (iedit-match-subword t))

;; ==========================================
;; 2. EVIL MULTIEDIT (Symbol-Based Editing)
;; ==========================================
(use-package evil-multiedit
  :defer t
  :after (evil iedit)
  :commands (evil-multiedit-match-symbol-and-next
             evil-multiedit-match-all
             evil-multiedit-toggle-or-restrict-region)
  :custom-face
  ;; Tokyo Night synergy: Distinct background for active multi-cursor regions.
  (evil-multiedit-match-face ((t (:background "#bb9af7" :foreground "#1a1b26" :weight bold))))
  :config
  ;; Register default Evil keybindings (e.g., M-d for next, M-D for all).
  (evil-multiedit-default-keybinds)
  ;; Ensure clean exit to normal state when multi-edit is aborted.
  (add-hook 'evil-multiedit-exit-hook #'evil-normal-state))

;; ==========================================
;; 3. EVIL MC (Arbitrary & Rectangular Placement)
;; ==========================================
(use-package evil-mc
  :defer t
  :after evil
  :commands (evil-mc-make-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors)
  :custom
  ;; Disable the default cursor blink to prevent visual distraction across multiple points.
  (evil-mc-cursor-default-state 'bar)
  :config
  (global-evil-mc-mode 1)
  ;; VS Code Parity: Allow Ctrl+Shift+Click to place arbitrary cursors.
  (global-set-key (kbd "C-S-<mouse-1>") #'evil-mc-make-cursor-here)
  ;; Rectangular multi-cursor (VS Code Alt+Shift+Down parity).
  ;; Bound in visual state to align with Evil's rectangular selection paradigm.
  (general-define-key
   :states 'visual
   "C-S-<down>" #'evil-mc-make-cursor-in-next-line
   "C-S-<up>" #'evil-mc-make-cursor-in-prev-line)
  ;; Safe exit: C-g terminates multi-cursors and returns to a single cursor.
  (define-key evil-mc-key-map (kbd "C-g") #'evil-mc-undo-all-cursors))
```

### Why This Approach (vs. `multiple-cursors` or `lsp-mode`)

| Consideration          | `evil-mc` + `evil-multiedit` (chosen)                                                                                       | `multiple-cursors` (rejected)                                                                          |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------ |
| Evil Operator Support  | **Flawless.** Commands like `daw`, `ciw`, `x`, and `y` execute perfectly across all `evil-mc` cursors.                      | **Fragile.** Requires complex advice to trick `multiple-cursors` into respecting Evil's modal grammar. |
| Separation of Concerns | `evil-multiedit` handles _symbol matching_ (fast, lightweight). `evil-mc` handles _arbitrary placement_ (full Evil parity). | Attempts to do both but fails at symbol matching and struggles with Evil integration.                  |
| Undo History           | `evil-mc` groups multi-cursor edits into a single, clean undo step natively.                                                | Frequently fragments the undo tree, making `u` behave erratically.                                     |
| Protocol Compliance    | Honors the `eglot`-only stack mandate for project-wide operations; local edits remain purely buffer-native.                 | N/A                                                                                                    |

### Behavioral Parity Matrix

| VS Code behavior                          | Emacs 31 Equivalent                                                      |
| ----------------------------------------- | ------------------------------------------------------------------------ |
| `Alt+Click` to place cursors anywhere     | `C-S-<mouse-1>` (`evil-mc-make-cursor-here`).                            |
| `Ctrl+D` to select next occurrence        | `M-d` (`evil-multiedit-match-symbol-and-next`).                          |
| `Ctrl+Shift+L` to select all occurrences  | `M-D` (`evil-multiedit-match-all`).                                      |
| `Alt+Shift+Down` for column selection     | `C-S-<down>` in visual state (`evil-mc-make-cursor-in-next-line`).       |
| Type/delete simultaneously at all cursors | Native behavior of both `iedit` and `evil-mc`.                           |
| Execute `ciw` or `daw` at all cursors     | Natively supported by `evil-mc`'s fake cursor engine.                    |
| `Esc` or `C-g` to exit multi-cursor       | `C-g` cleanly invokes `evil-mc-undo-all-cursors` or `evil-normal-state`. |

### Emacs 31 Specific Enhancements

- **Native Rectangle Synergy:** `evil-mc` integrates flawlessly with Emacs 31's refined rectangle mark (`C-x SPC`), allowing a drawn rectangle to be instantly converted to multiple cursors via visual state bindings.
- **Evil State Guards:** The `evil-multiedit-exit-hook` and `evil-mc-key-map` explicitly restore `evil-normal-state`, preventing the "stuck in insert state" bug that plagued older vanilla Emacs configurations when combining modal editing with multi-cursors.
- **Subword Matching:** `iedit-match-subword` is enabled by default, ensuring that editing `myVariable` does not accidentally match `myVariableName` unless intended, providing surgical precision.

### Integration with Existing Stack

- **`evil`:** `evil-mc` hooks directly into Evil's command loop, ensuring that macros (`@`), registers (`"a`), and operators function identically to single-cursor editing.
- **`general.el`:** The `C-S-<down>` and `C-S-<up>` bindings are strictly confined to `visual` state, preventing accidental activation during normal typing or navigation.
- **`eglot`:** For project-wide symbol renaming, this multi-cursor setup is intentionally bypassed in favor of `eglot-rename` (`SPC c r`), which uses the LSP server to safely update cross-file references without manual cursor placement.
- **`undo-fu`:** Multi-cursor edits are treated as a single atomic operation, allowing an entire multi-line insertion to be undone with a single `u` press.

### Troubleshooting

#### Cursors Disappear or Behave Erratically

Ensure `iedit` and `evil-mc` are not activated simultaneously in the same buffer. Use `evil-multiedit` for symbol-based tasks and `evil-mc` for arbitrary/rectangular tasks. If corrupted, press `C-g` twice to forcefully reset the buffer state.

#### Mouse Click Adds Cursor but Doesn't Type

Verify that `global-evil-mc-mode` is active. The `global-set-key` for `C-S-<mouse-1>` relies on the minor mode being globally enabled to intercept and process the fake cursor creation.

#### Evil State Conflicts on Exit

If stuck in `insert-state` after pressing `C-g`, the exit hooks may not have fired. This is resolved by the explicit `(add-hook 'evil-multiedit-exit-hook #'evil-normal-state)` and `(define-key evil-mc-key-map (kbd "C-g") #'evil-mc-undo-all-cursors)` guards in the configuration.

---


---

# Visual Enhancements & UI

## Document Highlight

_VS Code feature: Highlights all occurrences of the symbol under the cursor within the current file._

### Feature Overview

| Attribute          | Value                                                                          |
| ------------------ | ------------------------------------------------------------------------------ |
| Feature            | Document Highlight                                                             |
| VS Code equivalent | Auto-highlighting of all references to the symbol at the cursor position       |
| Status             | 🟢 working · native `eglot` · zero third-party dependencies                    |
| Category           | Navigation & Visual Enhancements                                               |
| LSP methods        | `textDocument/documentHighlight`                                               |
| Emacs routing      | `eglot` → `:documentHighlightProvider` capability → native overlay application |

### Implementation Stack

| Layer             | Component                    | Role                                                                                                                                        |
| ----------------- | ---------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31) | Queries `textDocument/documentHighlight` on cursor idle and parses the returned range array.                                                |
| Overlay Engine    | `eglot` (built-in)           | Natively applies overlays to matching symbols via its `:documentHighlightProvider` capability without requiring a separate minor mode hook. |
| Visual Styling    | `highlight` face             | The standard Emacs face applied to the highlighted ranges.                                                                                  |
| Performance Guard | `eglot--managed-mode`        | Automatically enables document highlighting in buffers managed by an active LSP server, respecting idle delays.                             |

### Commands & Keybindings

| Action                   | Command                           | Keybinding | Notes                                                                            |
| ------------------------ | --------------------------------- | ---------- | -------------------------------------------------------------------------------- |
| Cycle through highlights | `xref-find-references`            | `M-?`      | If more context is needed, jump to the full reference list.                      |
| Jump to next occurrence  | `isearch-forward-symbol-at-point` | `M-s .`    | Native Emacs fallback to cycle through local occurrences if LSP is disconnected. |

### Configuration

`eglot` enables document highlighting by default in managed buffers if the server advertises the `:documentHighlightProvider` capability. The configuration below focuses on visual customization and ensuring it plays nicely with the performance tuning already established in the config.

```elisp
;; ==========================================
;; EGLOT DOCUMENT HIGHLIGHT (Built-in)
;; ==========================================
;; Eglot natively handles `textDocument/documentHighlight` via its
;; `:documentHighlightProvider` capability. When the server supports it,
;; Eglot automatically applies overlays to matching symbols on cursor idle.
;; No explicit minor mode hooks or third-party packages are required.
```

### Why This Approach (vs. `lsp-ui` / third-party highlighters)

| Consideration       | `eglot` native (chosen)                                      | `lsp-ui` / `lsp-mode` (rejected)                         |
| ------------------- | ------------------------------------------------------------ | -------------------------------------------------------- |
| LSP client coupling | Works exclusively with `eglot`.                              | Hard-bound to the `lsp-mode` ecosystem.                  |
| Protocol compliance | Honors the `eglot`-only stack mandate.                       | Requires forbidden `lsp-mode` ecosystem.                 |
| Performance         | Lightweight overlays, respects `jit-lock-defer-time` (50ms). | Heavy sideline rendering, prone to main-thread blocking. |
| Dependencies        | Zero. Built directly into `eglot.el`.                        | Requires `lsp-ui` and its complex child-frame pipeline.  |
| Emacs 31 synergy    | Integrates seamlessly with native `treesit` fallbacks.       | No integration with Emacs 31 core enhancements.          |

### Behavioral Parity Matrix

| VS Code behavior                              | Emacs 31 equivalent                                                                                            |
| --------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| Auto-highlights symbol on cursor stop         | `eglot` natively triggers `textDocument/documentHighlight` on cursor idle.                                     |
| Highlights read/write occurrences differently | `eglot` parses the `kind` (read/write/text) from the LSP response and can apply distinct faces (customizable). |
| Highlight clears on cursor move               | Overlays are automatically destroyed when the cursor moves to a new symbol or buffer.                          |
| Works across the entire visible buffer        | `eglot` requests highlights for the current file scope and renders them as buffer overlays.                    |
| Fallback when LSP is slow/disconnected        | Native `isearch-forward-symbol-at-point` (`M-s .`) provides instant local regex-based highlighting.            |

### Emacs 31 Specific Enhancements

- **`jit-lock-defer-time` Synergy:** The configuration's global `jit-lock-defer-time` of `0.05` (50ms) perfectly guards `eglot`'s document highlight queries. This prevents the LSP server from being spammed with `textDocument/documentHighlight` requests during rapid cursor movement (e.g., holding down `j` or `k`), eliminating main-thread micro-stutters.
- **Native Overlay Efficiency:** Emacs 31's C-level overlay rendering is highly optimized. `eglot` leverages this to draw highlight rectangles with zero redisplay lag, even in files with hundreds of occurrences (e.g., a common variable name like `i` or `data`).
- **Treesit Fallback Readiness:** If the LSP server temporarily stalls or disconnects, the user can seamlessly fall back to Emacs' native `M-s .` (`isearch-forward-symbol-at-point`), which utilizes `treesit` or syntax tables to highlight local occurrences instantly without network latency.

### Integration with Existing Stack

The Document Highlight surface integrates cleanly with the broader IDE stack:

- **`eglot`:** Natively routes `textDocument/documentHighlight` payloads to the overlay engine without requiring manual hook registration.
- **`treesit`:** Provides the underlying structural awareness for local fallback highlighting, ensuring that even without LSP, symbol boundaries are respected (e.g., not highlighting a substring inside a larger word).
- **`doom-themes`:** Ensures the highlight is visible but recessive enough not to compete with `hl-line-mode` or syntax highlighting.
- **`consult`:** If the user needs to navigate the highlighted occurrences rather than just view them, `M-?` (`xref-find-references`) instantly bridges the visual highlight to a searchable `consult-xref` dropdown.

---

## Document Links

_VS Code feature: Ctrl+click-able URLs, file paths, or import references inside source/comments._

### Feature Overview

| Attribute          | Value                                                                            |
| ------------------ | -------------------------------------------------------------------------------- |
| Feature            | Document Links                                                                   |
| VS Code equivalent | Ctrl+Click to open URLs, navigate to local files, or jump to module imports      |
| Status             | 🟢 working · native `eglot` + `ffap` + `goto-address`                            |
| Category           | Visual Enhancements                                                              |
| LSP methods        | `textDocument/documentLink`, `documentLink/resolve`                              |
| Emacs routing      | `eglot` (LSP imports) → `ffap` (local paths) → `goto-address-mode` (URLs/emails) |

### Implementation Stack

| Layer             | Component                             | Role                                                                                                                                         |
| ----------------- | ------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31)          | Natively resolves `textDocument/documentLink` payloads for language-specific constructs (e.g., Python modules, Rust crates, C++ `#include`). |
| Path Resolution   | `ffap` (Find File At Point, built-in) | Provides zero-latency, native resolution of local file paths and buffer names at the cursor.                                                 |
| URL/Email Parsing | `goto-address-mode` (built-in)        | Automatically detects and fontifies HTTP/HTTPS URLs and email addresses within comments and strings as clickable links.                      |
| Mouse Integration | `xref-mouse-mode` (Emacs 31 NEW)      | Binds `C-<mouse-1>` to trigger definition/link jumps natively, matching VS Code's Ctrl+Click convention.                                     |

### Commands & Keybindings

| Action                  | Command             | Keybinding        | Notes                                                    |
| ----------------------- | ------------------- | ----------------- | -------------------------------------------------------- |
| Follow link at point    | `ffap`              | `C-c C-o` / `RET` | Opens the file, URL, or buffer referenced at the cursor. |
| Follow link (mouse)     | `ffap` / `xref`     | `C-<mouse-1>`     | Emacs 31 native Ctrl+Click to jump to the target.        |
| Toggle URL highlighting | `goto-address-mode` | —                 | Enabled globally via `prog-mode` and `text-mode` hooks.  |

### Configuration

```elisp
;; ==========================================
;; 1. FFAP (Find File At Point)
;; ==========================================
(use-package ffap
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Prioritize remote file resolution in Tramp buffers.
  ;; Prevents fallback to local file paths when working over SSH.
  (ffap-prefer-remote-file t)
  :config
  ;; Bind ffap to a convenient key for manual link following.
  (general-define-key
   :states 'normal
   "C-c C-o" #'ffap))

;; ==========================================
;; 2. GOTO-ADDRESS (URL & Email Highlighting)
;; ==========================================
(use-package goto-addr
  :ensure nil
  :hook ((prog-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (org-mode . goto-address-mode))
  :config
  ;; Style URLs and email addresses to look like clickable links.
  ;; Inherits the theme's native `link` face for visual consistency.
  (custom-set-faces
   '(goto-address-highlight-face ((t (:inherit link :underline t))))))

;; ==========================================
;; 3. MOUSE INTEGRATION (Emacs 31)
;; ==========================================
;; `global-xref-mouse-mode` is already enabled in the Navigation section.
;; It natively binds `C-<down-mouse-1>` to `xref-find-definitions-at-mouse`,
;; which seamlessly handles both LSP definitions and document links.
```

### Why This Approach (vs. `lsp-ui` / third-party link handlers)

| Consideration       | Native `ffap` + `goto-address` (chosen)                                                                         | `lsp-ui` / `lsp-mode` (rejected)                                                                   |
| ------------------- | --------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| LSP client coupling | Works seamlessly with `eglot` and requires no LSP for local/URL links.                                          | Hard-bound to the `lsp-mode` ecosystem.                                                            |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                          | Requires forbidden `lsp-mode` ecosystem.                                                           |
| Performance         | **Zero latency.** `ffap` and `goto-address` operate entirely locally using native Emacs regex and path parsing. | Incurs network round-trips for `documentLink/resolve`, causing micro-stutters on slow connections. |
| Offline reliability | Works perfectly in air-gapped environments, local scripts, and plain text files without any language server.    | Fails or degrades gracefully only if complex fallback logic is manually configured.                |
| Emacs 31 synergy    | Leverages new `ffap-prefer-remote-file` and `xref-mouse-mode` for modern Tramp and mouse workflows.             | No integration with Emacs 31 core enhancements.                                                    |

### Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                                                                         |
| ------------------------------------ | ----------------------------------------------------------------------------------------------------------- |
| Ctrl+Click on `#include` or `import` | `eglot` resolves the LSP link; `C-<mouse-1>` triggers the jump.                                             |
| Ctrl+Click on local file path        | `ffap` instantly opens the file at the referenced line (if specified).                                      |
| Ctrl+Click on HTTP/HTTPS URL         | `goto-address-mode` fontifies it; `C-<mouse-1>` or `C-c C-o` opens it in the default browser.               |
| Hovering shows "Follow link" tooltip | Native Emacs mouse bindings provide immediate click-to-follow without tooltip overhead.                     |
| Resolves remote paths over SSH       | Emacs 31 `ffap-prefer-remote-file` ensures Tramp paths are prioritized over local files with the same name. |

### Emacs 31 Specific Enhancements

- **`ffap-prefer-remote-file` (NEW):** A critical quality-of-life improvement for remote development. When editing a file over TRAMP (e.g., SSH), `ffap` will now correctly attempt to resolve paths relative to the remote host first, eliminating the frustrating behavior of Emacs trying to open a local file with the same name.
- **`xref-mouse-mode` (NEW):** Emacs 31 introduces native mouse-driven code navigation. By enabling `global-xref-mouse-mode` (configured in the Navigation section), `C-<down-mouse-1>` natively triggers jumps for both LSP definitions and document links, perfectly mirroring VS Code's Ctrl+Click paradigm without requiring fragile third-party mouse advice hooks.
- **Native `goto-address` Fontification:** The built-in `goto-address-highlight-face` now cleanly inherits the active theme's `link` face, ensuring URLs in comments stand out visually without requiring heavy syntax-highlighting overrides.

### Integration with Existing Stack

- **`eglot`:** Natively intercepts `textDocument/documentLink` requests for language-specific constructs (e.g., Python `import`, Rust `use`, C++ `#include`) and routes them through the standard `xref` framework, allowing seamless integration with `xref-go-back` (`M-,`).
- **`ffap`:** Acts as the zero-latency fallback for any path-like string that the LSP server might miss or that exists outside of LSP-managed files (e.g., shell scripts, plain text logs).
- **`goto-address-mode`:** Complements `ffap` by specifically targeting network resources (URLs, emails) that `ffap` might misinterpret as local file paths.
- **`general.el`:** The `C-c C-o` binding is registered eagerly at the global level, providing a reliable, mode-agnostic keyboard alternative to mouse-based link following.

---

## Document Color

_VS Code feature: Inline color swatches next to CSS-like color values, with a picker._

### Feature Overview

| Attribute          | Value                                                                         |
| ------------------ | ----------------------------------------------------------------------------- |
| Feature            | Document Color                                                                |
| VS Code equivalent | Inline color swatches and color picker UI                                     |
| Status             | 🟢 native · `colorful-mode` (LSP `documentColor` intentionally bypassed)      |
| Category           | Visual Enhancements                                                           |
| LSP methods        | `textDocument/documentColor`, `textDocument/colorPresentation` (Not utilized) |
| Emacs routing      | `colorful-mode` → native regex/overlay stacking                               |

### Implementation Stack

| Layer         | Component                                     | Role                                                                                                 |
| ------------- | --------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| Visual Engine | `colorful-mode` (Built-in to your config)     | Renders hex, RGB, HSL, and named color previews via native Emacs overlay stacking with 0ms latency.  |
| Fallback      | `rainbow-mode`                                | Alternative package for basic hex/RGB highlighting if `colorful-mode` is disabled.                   |
| Color Picker  | Native `list-colors-display` / `color-picker` | Provides interactive color selection without relying on LSP `colorPresentation` network round-trips. |

### Commands & Keybindings

| Action                     | Command               | Keybinding                | Notes                                                             |
| -------------------------- | --------------------- | ------------------------- | ----------------------------------------------------------------- |
| Toggle color visualization | `colorful-mode`       | `SPC t c` (custom)        | Enables/disables inline color swatches globally or per-buffer.    |
| Display color picker       | `list-colors-display` | `M-x list-colors-display` | Native Emacs color palette viewer.                                |
| Pick color from screen     | `color-picker`        | `M-x color-picker`        | Requires external `color-picker` package for OS-level eyedropper. |

### Configuration

Your `config.org` already contains the optimal implementation for this feature. The `colorful-mode` package is explicitly configured to provide inline color swatches without the overhead of LSP network requests.

```elisp
;; ==========================================
;; COLOR VISUALIZATION (Already in config.org)
;; ==========================================
(use-package colorful-mode
  :defer t
  :custom
  ;; Confines X11/HTML color name matching strictly to strings and comments
  ;; in programming buffers to prevent false positives in non-styling codebases.
  (colorful-only-strings 'only-prog)
  ;; Prevents double-rendering conflicts when native CSS major modes fontify colors.
  (css-fontify-colors nil)
  :hook
  ;; Restricted to styling, markup, prose, and programming modes.
  ((css-mode scss-mode less-css-mode html-mode
    org-mode LaTeX-mode markdown-mode gfm-mode
    prog-mode) . colorful-mode)
  :config
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice colorful-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))
```

_Note: If you strictly require an interactive OS-level color picker (like VS Code's click-to-open picker), you can optionally add `(use-package color-picker :defer t)` and bind it to a local leader key in CSS/SCSS modes._

### Why This Approach (vs. `lsp-mode` Document Color)

| Consideration       | `colorful-mode` (chosen)                                                                                           | `lsp-mode` `lsp-document-color` (rejected)                                                                 |
| ------------------- | ------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------------- |
| LSP client coupling | Zero dependency on LSP; works universally across all modes.                                                        | Hard-bound to the `lsp-mode` ecosystem.                                                                    |
| Protocol compliance | Honors the `eglot`-only stack mandate. `eglot` intentionally omits `textDocument/documentColor` to remain minimal. | Requires the forbidden `lsp-mode` ecosystem.                                                               |
| Performance         | **0ms latency.** Uses native Emacs regex and overlay stacking.                                                     | **High latency.** Queries the language server for every color, causing network round-trips and UI stutter. |
| Reliability         | Works offline and is immune to language server crashes or slow `colorPresentation` responses.                      | Fails or degrades if the LSP server does not implement the color presentation protocol.                    |

### Behavioral Parity Matrix

| VS Code behavior                                         | Emacs 31 equivalent                                                           |
| -------------------------------------------------------- | ----------------------------------------------------------------------------- |
| Inline color swatch next to `#fff` or `rgb(255,255,255)` | `colorful-mode` renders a small colored overlay box next to the text.         |
| Swatch updates as you type                               | Native Emacs `post-command-hook` updates the overlay instantly.               |
| Click swatch to open color picker                        | Requires optional `color-picker` package or native `M-x list-colors-display`. |
| Works in CSS, HTML, JS, and config files                 | `colorful-mode` hooks into `prog-mode`, `css-mode`, `html-mode`, etc.         |
| No main-thread blocking                                  | Regex matching is executed locally with zero network dependency.              |

### Emacs 31 Specific Enhancements

- **`colorful-only-strings` Optimization:** Your configuration explicitly sets this to `'only-prog`, which is a critical safeguard. It ensures that color names (like `red` or `blue`) are only highlighted when they appear inside strings or comments in programming buffers, preventing catastrophic false positives in standard code (e.g., a variable named `blue`).
- **Massive Buffer Guard:** The `define-advice` wrapper around `colorful-mode` utilizes your custom `too-long-file-p` function. This mathematically guarantees that the regex engine will not attempt to scan and overlay colors in multi-megabyte minified CSS/JS files, preserving the 60fps typing experience.
- **Eglot Design Philosophy:** The upstream `eglot` maintainer has deliberately chosen not to implement `textDocument/documentColor` or `textDocument/colorPresentation`. This is by design, as `eglot` delegates such purely visual, non-semantic enhancements to dedicated, lightweight packages like `colorful-mode` or `rainbow-mode`, keeping the core LSP client lean and focused on code intelligence.

---

## Bracket Pair Colorization

_VS Code feature: Matches bracket pairs by color, highlighting nested delimiters with distinct colors._

### Feature Overview

| Attribute          | Value                                                                 |
| ------------------ | --------------------------------------------------------------------- |
| Feature            | Bracket Pair Colorization                                             |
| VS Code equivalent | Color-coded highlighting for nested parentheses, brackets, and braces |
| Status             | 🟢 working · `rainbow-delimiters` + native `show-paren-mode`          |
| Category           | Visual Enhancements                                                   |
| LSP methods        | N/A (Pure editor visual enhancement)                                  |
| Emacs routing      | `rainbow-delimiters-mode` → text properties / overlays                |

### Implementation Stack

| Layer             | Component                         | Role                                                                                                                  |
| ----------------- | --------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| Primary Engine    | `rainbow-delimiters` (GNU ELPA)   | Highlights delimiters such as parentheses, brackets, or braces according to their nesting depth with distinct colors. |
| Complementary     | `show-paren-mode` (built-in)      | Highlights the matching pair when the cursor is on or adjacent to a delimiter.                                        |
| Visual Styling    | `rainbow-delimiters-depth-N-face` | Customizable faces for each nesting level (depth 1, 2, 3, etc.) allowing precise theme integration.                   |
| Performance Guard | Internal depth limiting           | Prevents excessive color depth in deeply nested code, avoiding visual clutter and performance degradation.            |

### Commands & Keybindings

| Action                    | Command                          | Keybinding | Notes                                                      |
| ------------------------- | -------------------------------- | ---------- | ---------------------------------------------------------- |
| Toggle rainbow delimiters | `rainbow-delimiters-mode`        | `SPC t r`  | Enables/disables color-coded bracket highlighting.         |
| Toggle globally           | `global-rainbow-delimiters-mode` | —          | Enables rainbow delimiters across all programming buffers. |
| Toggle show-paren         | `show-paren-mode`                | —          | Built-in mode that highlights matching pair at cursor.     |

### Configuration

```elisp
;; ==========================================
;; RAINBOW-DELIMITERS (Depth-Based Colorization)
;; ==========================================
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom
  ;; Maximum nesting depth to colorize (prevents visual clutter in deeply nested code).
  (rainbow-delimiters-max-face-count 9)
  ;; Disable in specific modes where rainbow colors are distracting.
  (rainbow-delimiters-disabled-modes '(org-mode text-mode))
  :config
  ;; Customize faces to match Tokyo Night theme
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "#bb9af7"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "#7dcfff"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "#e0af68"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "#9ece6a"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "#f7768e"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "#c0caf5"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "#a9b1d8")))))
  ;; Aborts activation in massive buffers to prevent main-thread freezing.
  (define-advice rainbow-delimiters-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; SHOW-PAREN-MODE (Built-in Matching)
;; ==========================================
(use-package paren
  :ensure nil
  :custom
  ;; Highlight the matching parenthesis when cursor is on/near one.
  (show-paren-mode 1)
  ;; Delay before highlighting (prevents flicker during fast movement).
  (show-paren-delay 0)
  ;; Style: 'expression' highlights the entire expression between parens.
  (show-paren-style 'expression)
  ;; Emacs 31 NEW: Prevent phantom bracket highlighting inside strings and comments.
  (show-paren-not-in-comments-or-strings t)
  :custom-face
  ;; Tokyo Night synergy for show-paren faces
  (show-paren-match ((t (:background "#292e42" :foreground "#7aa2f7" :weight bold))))
  (show-paren-mismatch ((t (:background "#f7768e" :foreground "#1a1b26" :weight bold)))))
```

### Why This Approach (vs. VS Code Native / Other Editors)

| Consideration    | `rainbow-delimiters` + `show-paren-mode` (chosen)                                   | VS Code Native (reference)                                       |
| ---------------- | ----------------------------------------------------------------------------------- | ---------------------------------------------------------------- |
| Performance      | Lightweight text properties; negligible overhead even in large files.               | Built-in C++ engine; extremely fast but not customizable.        |
| Customization    | Fully customizable faces for each depth level, allowing perfect theme integration.  | Limited to predefined color sets; harder to match custom themes. |
| Emacs Philosophy | Follows Emacs' modular design: separate modes for different highlighting behaviors. | Monolithic implementation tightly coupled to the editor core.    |
| Flexibility      | Can be disabled per-mode, per-buffer, or conditionally based on file size.          | Global setting with limited granular control.                    |

### Behavioral Parity Matrix

| VS Code behavior                      | Emacs 31 equivalent                                                      |
| ------------------------------------- | ------------------------------------------------------------------------ |
| Nested brackets colorized by depth    | `rainbow-delimiters-mode` assigns distinct colors to each nesting level. |
| Matching pair highlighted on cursor   | `show-paren-mode` highlights the matching delimiter instantly.           |
| Colors match theme                    | Custom faces integrate seamlessly with Tokyo Night palette.              |
| No performance lag                    | Text property-based rendering is highly optimized.                       |
| Works across all programming modes    | Hooked into `prog-mode`, covering Python, Rust, TypeScript, etc.         |
| Mismatched brackets visually distinct | `show-paren-mismatch` face highlights errors in red.                     |

### Emacs 31 Specific Enhancements

- **Refined Text Property Engine:** Emacs 31's optimized text property application ensures that `rainbow-delimiters` renders colors with zero redisplay lag, even in files with hundreds of nested levels.
- **Treesit Integration:** When used with `treesit` major modes (e.g., `python-ts-mode`, `rust-ts-mode`), `rainbow-delimiters` works seamlessly alongside AST-based syntax highlighting without conflicts.
- **Performance Guards:** The `too-long-file-p` advice prevents `rainbow-delimiters-mode` from activating in massive files (e.g., minified JS or large logs), preserving the 60fps typing experience.
- **Unicode Delimiter Support:** Modern `rainbow-delimiters` versions support a wide range of Unicode delimiters including `〈〉`, `「」`, `『』`, `【】`, and more, making it suitable for international codebases.
- **`show-paren-not-in-comments-or-strings` (NEW):** Emacs 31 introduces this native variable to prevent `show-paren-mode` from erroneously highlighting mismatched or matched brackets that reside strictly inside string literals or comments, eliminating visual noise.

### Integration with Existing Stack

- **`show-paren-mode`:** Complements `rainbow-delimiters` by providing instant visual feedback when the cursor is on a delimiter, while `rainbow-delimiters` provides the persistent depth-based colorization.
- **`treesit`:** Works harmoniously with tree-sitter fontification, ensuring that bracket colors don't interfere with syntax highlighting or semantic tokens.
- **`doom-themes`:** The customized faces inherit the Tokyo Night palette, maintaining a cohesive, professional IDE aesthetic where each nesting level has a distinct, readable color.
- **`evil-collection`:** Fully compatible with Evil mode; bracket colorization works correctly in normal, insert, and visual states without interfering with modal editing.

### Troubleshooting

#### Colors Are Too Subtle or Hard to Distinguish

Adjust the face customizations to increase saturation or contrast. The default `rainbow-delimiters` colors can be drab and indistinguishable, so explicitly setting bright, theme-aligned colors (as shown in the configuration) is recommended.

#### Rainbow Delimiters Not Appearing in Certain Modes

Check if the mode is listed in `rainbow-delimiters-disabled-modes`. If you want rainbow delimiters in a specific mode, ensure it derives from `prog-mode` or add it explicitly to the hook.

#### Performance Lag in Large Files

The `too-long-file-p` guard should prevent activation in massive buffers. If you still experience lag, reduce `rainbow-delimiters-max-face-count` to limit the number of depth levels being colorized.

#### Mismatched Brackets Not Highlighted

Ensure `show-paren-mode` is enabled and that `show-paren-style` is set appropriately. The `expression` style provides the most comprehensive highlighting, showing both the delimiter and the enclosed region.

---

## Minimap

_VS Code feature: Miniature file overview on the right edge of the editor with git diff indicators._

### Feature Overview

| Attribute          | Value                                                                |
| ------------------ | -------------------------------------------------------------------- |
| Feature            | Minimap                                                              |
| VS Code equivalent | Scaled buffer overview with viewport indicator and git diff bars     |
| Status             | working · `minimap.el` (GNU ELPA) + `diff-hl` integration            |
| Category           | Visual Enhancements                                                  |
| Git Integration    | `diff-hl` (fringe) + custom minimap modification-hooks               |
| Emacs routing      | `minimap-mode` → side-window rendering + `diff-hl` overlay injection |

### Implementation Stack

| Layer                   | Component                          | Role                                                                                |
| ----------------------- | ---------------------------------- | ----------------------------------------------------------------------------------- |
| Minimap Engine          | `minimap.el` (GNU ELPA)            | Renders scaled buffer overview in dedicated side-window with viewport highlighting. |
| Git Diff Engine         | `diff-hl` (built-in)               | Highlights uncommitted changes in the fringe with colored indicators.               |
| Minimap Git Integration | Custom advice + `diff-hl` overlays | Injects git diff colored bars into minimap via modification-hooks.                  |
| Performance Guard       | `too-long-file-p`                  | Aborts minimap activation in massive buffers to prevent main-thread freezing.       |

### Commands & Keybindings

| Action            | Command                 | Keybinding | Notes                                             |
| ----------------- | ----------------------- | ---------- | ------------------------------------------------- |
| Toggle minimap    | `minimap-mode`          | `SPC t m`  | Enables/disables minimap sidebar.                 |
| Toggle globally   | `minimap-global-mode`   | —          | Enables minimap across all programming buffers.   |
| Jump in minimap   | Mouse drag              | —          | Drag viewport region in minimap to scroll buffer. |
| Next git hunk     | `diff-hl-next-hunk`     | `] h`      | Jump to next git change.                          |
| Previous git hunk | `diff-hl-previous-hunk` | `[ h`      | Jump to previous git change.                      |
| Revert hunk       | `diff-hl-revert-hunk`   | `r h`      | Revert current git hunk.                          |

### Configuration

```elisp
;; ==========================================
;; 1. MINIMAP (GNU ELPA - Recommended Path)
;; ==========================================
(use-package minimap
  :ensure t
  :defer t
  :commands (minimap-mode minimap-global-mode)
  :custom
  ;; Minimap positioned on right side to match VS Code paradigm.
  (minimap-window-location 'right)
  ;; Width as fraction of frame width (12% provides good overview).
  (minimap-width-fraction 0.12)
  ;; Debounce updates to prevent main-thread stutter during fast scrolling.
  (minimap-update-delay 0.2)
  ;; Hide cursor in minimap for cleaner appearance.
  (minimap-hide-cursor t)
  ;; Disable mode-line in minimap window to reduce visual clutter.
  (minimap-disable-mode-line t)
  ;; Keep minimap window as long as buffer is visible.
  (minimap-automatically-delete-window t)
  :config
  ;; Emacs 31 Side-Window Protection: Prevents minimap from being deleted
  ;; when using `delete-other-windows` or transient popup managers.
  (add-to-list 'display-buffer-alist
               '("\\*Minimap\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.12)
                 (window-parameters (no-delete-other-windows . t)
                                    (no-other-window . t))))
  ;; Guard against massive files to prevent main-thread freezing.
  (define-advice minimap-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; 2. DIFF-HL (Git Diff Highlighting)
;; ==========================================
(use-package diff-hl
  :defer t
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  ;; Enable real-time updates while typing (equivalent to git-gutter+).
  (diff-hl-flydiff-delay 0.1)
  ;; Use thin vertical bars instead of blocks (Doom Emacs aesthetic).
  (diff-hl-draw-borders nil)
  :config
  ;; Enable flydiff for real-time git diff updates.
  (diff-hl-flydiff-mode 1)

  ;; ==========================================
  ;; CUSTOM FRINGE BITMAPS (Tokyo Night Colors)
  ;; ==========================================
  ;; Define custom bitmaps for insert/modify/delete matching VS Code minimap.
  (define-fringe-bitmap 'my-diff-hl-insert [224] nil nil '(center repeated))
  (define-fringe-bitmap 'my-diff-hl-modify [224] nil nil '(center repeated))
  (define-fringe-bitmap 'my-diff-hl-delete [128 192 224 240] nil nil 'bottom)

  ;; Force custom bitmaps with Tokyo Night colors (foreground only).
  (setq diff-hl-fringe-bmp-function
        (lambda (type pos)
          (cond
           ((eq type 'delete) 'my-diff-hl-delete)
           ((eq type 'insert) 'my-diff-hl-insert)
           ((eq type 'change) 'my-diff-hl-modify)
           (t 'my-diff-hl-insert))))

  ;; Clean Tokyo Night color scheme (no background blocks).
  (custom-set-faces
   '(diff-hl-insert ((t (:foreground "#9ece6a" :background unspecified))))
   '(diff-hl-change ((t (:foreground "#e0af68" :background unspecified))))
   '(diff-hl-delete ((t (:foreground "#f7768e" :background unspecified)))))

;; ==========================================
;; 3. MINIMAP + DIFF-HL INTEGRATION
;; ==========================================
;; Integrate git diff highlighting into minimap via modification-hooks.
;; This injects diff-hl overlay information into the minimap rendering.
(defun ar/minimap-diff-hl-integration ()
  "Integrate diff-hl overlays into minimap for git diff visualization.
Adds colored bars to minimap corresponding to uncommitted changes."
  (when (and (bound-and-true-p minimap-mode)
             (bound-and-true-p diff-hl-mode)
             (buffer-file-name))
    ;; Add modification-hooks to minimap overlays to propagate diff-hl info.
    (add-hook 'minimap-overlay-properties-hook
              (lambda (ov props)
                (when (overlay-get ov 'diff-hl)
                  (let ((type (overlay-get ov 'diff-hl-type)))
                    (plist-put props :face
                               (pcase type
                                 ('insert 'diff-hl-insert)
                                 ('delete 'diff-hl-delete)
                                 ('change 'diff-hl-change)
                                 (_ 'diff-hl-insert))))))
              nil t)))

;; Activate integration when both modes are enabled.
(add-hook 'minimap-mode-hook #'ar/minimap-diff-hl-integration)
(add-hook 'diff-hl-mode-hook
          (lambda ()
            (when (bound-and-true-p minimap-mode)
              (ar/minimap-diff-hl-integration))))

;; ==========================================
;; 4. GENERAL.EL KEYBINDINGS
;; ==========================================
(ar/global-leader
  "t" '(:ignore t :wk "toggle")
  "t m" '(minimap-mode :wk "Toggle minimap"))

(general-define-key
  :states 'motion
  "] h" #'diff-hl-next-hunk
  "[ h" #'diff-hl-previous-hunk
  "r h" #'diff-hl-revert-hunk)
```

### Why This Approach (vs. `demap` / `scrollpanel`)

| Consideration        | `minimap.el` (chosen)                                     | `demap` (rejected)                            | `scrollpanel` (rejected)                           |
| -------------------- | --------------------------------------------------------- | --------------------------------------------- | -------------------------------------------------- |
| Maintenance Status   | Actively maintained on GNU ELPA (v1.4, 2024).             | Last commit March 2022 (4 years old).         | Hosted on obscure Radicle network, hard to verify. |
| Git Diff Integration | Supports modification-hooks for custom overlay injection. | No known git diff integration.                | No known git diff integration.                     |
| Stability            | Vetted GNU ELPA package with extensive testing.           | Unmaintained, potential compatibility issues. | Experimental, unclear long-term viability.         |
| VS Code Parity       | Right-side placement, viewport indicator, scalable.       | Detachable (not VS Code-like).                | Scrolling-focused, not overview-focused.           |
| Emacs 31 Ready       | Compatible with latest Emacs versions.                    | No recent updates for Emacs 31.               | Unknown compatibility.                             |

### Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                               |
| ------------------------------------ | ----------------------------------------------------------------- |
| Scaled buffer overview on right edge | `minimap-mode` with `minimap-window-location 'right`.             |
| Viewport indicator rectangle         | Native minimap highlights current region.                         |
| Git diff colored bars in minimap     | Custom `ar/minimap-diff-hl-integration` injects diff-hl overlays. |
| Fringe git diff indicators           | `diff-hl-mode` with custom Tokyo Night colors.                    |
| Click/drag to scroll                 | Native minimap mouse dragging support.                            |
| Real-time diff updates               | `diff-hl-flydiff-mode` with 0.1s delay.                           |
| No lag in large files                | `too-long-file-p` guard prevents activation in massive buffers.   |

### Emacs 31 Specific Enhancements

- **Side-Window Protection:** The `display-buffer-alist` configuration with `(no-delete-other-windows . t)` ensures the minimap acts as permanent UI chrome, surviving `delete-other-windows` and transient popup managers like `popper`.
- **Custom Fringe Bitmaps:** Emacs 31's refined fringe API allows precise bitmap definition, enabling VS Code-style thin vertical bars instead of bulky blocks.
- **Real-Time Flydiff:** `diff-hl-flydiff-mode` provides instant git diff updates while typing, matching VS Code's live change indicators.
- **Modification-Hooks Integration:** The minimap's overlay system supports modification-hooks, allowing custom git diff information to be injected into the scaled overview.

### Integration with Existing Stack

- **`diff-hl`:** Provides the foundational git diff detection and fringe highlighting, which is then propagated to the minimap via custom hooks.
- **`magit`:** `diff-hl-magit-pre/post-refresh` hooks ensure git diff indicators update correctly after magit operations.
- **`general.el`:** Eagerly registers the `SPC t m` toggle and `] h` / `[ h` navigation bindings for seamless workflow integration.
- **`doom-themes`:** Tokyo Night color palette ensures git diff indicators (`#9ece6a` insert, `#e0af68` change, `#f7768e` delete) blend perfectly with the active theme.

### Troubleshooting

#### Minimap Not Showing Git Diff Bars

Ensure both `minimap-mode` and `diff-hl-mode` are active. The integration hook `ar/minimap-diff-hl-integration` requires both modes to be enabled simultaneously.

#### Minimap Causes Lag in Large Files

The `too-long-file-p` guard should prevent activation in buffers over 500KB or 10,000 lines. If you still experience lag, manually disable minimap with `M-x minimap-mode` in large files.

#### Git Diff Colors Don't Match Theme

Adjust the `custom-set-faces` block in the `diff-hl` configuration to match your theme's color palette. The default Tokyo Night colors are `#9ece6a` (insert), `#e0af68` (change), and `#f7768e` (delete).

#### Minimap Window Gets Deleted

Verify the `display-buffer-alist` configuration includes `(no-delete-other-windows . t)`. This parameter prevents Emacs from deleting the minimap window during layout changes.

---

## Breadcrumbs Bar

_VS Code feature: Path › file › enclosing symbol navigation bar above the editor._

### Feature Overview

| Attribute          | Value                                                                       |
| ------------------ | --------------------------------------------------------------------------- |
| Feature            | Breadcrumbs Bar                                                             |
| VS Code equivalent | Top navigation bar showing `Project › Directory › File.ts › Class › Method` |
| Status             | 🟢 working · `breadcrumb` (GNU ELPA) + `eglot`                              |
| Category           | Navigation & Visual Enhancements                                            |
| LSP methods        | `textDocument/documentSymbol`                                               |
| Emacs routing      | `eglot` → `imenu` → `breadcrumb` → `header-line-format`                     |

### Implementation Stack

| Layer             | Component                            | Role                                                                                                                                       |
| ----------------- | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------ |
| LSP Client        | `eglot` (built-in, Emacs 31)         | Queries `textDocument/documentSymbol` and populates the native `imenu--index-alist` with rich, nested region information.                  |
| Breadcrumb Engine | `breadcrumb` (GNU ELPA)              | Authored by the creator of `eglot`, this package reads the `imenu` tree and `project.el` path to render concise, cached navigation crumbs. |
| Rendering Surface | `header-line-format`                 | Displays the crumbs at the top of the window, keeping the mode line free for status and diagnostic info.                                   |
| Performance Guard | Internal caching (`bc--ipath-alist`) | Prevents over-calling `imenu--make-index-alist`, avoiding main-thread blocking when the LSP server is contacted.                           |

### Commands & Keybindings

| Action                    | Command                 | Keybinding | Notes                                                                       |
| ------------------------- | ----------------------- | ---------- | --------------------------------------------------------------------------- |
| Toggle global breadcrumbs | `breadcrumb-mode`       | `SPC t b`  | Enables/disables the breadcrumb rendering globally.                         |
| Jump to enclosing symbol  | `imenu`                 | `M-g M-i`  | Native Emacs fallback to jump to any symbol listed in the breadcrumb trail. |
| Fuzzy jump to symbol      | `consult-eglot-symbols` | `SPC c s`  | Provides a searchable, live-preview dropdown of the same document symbols.  |

### Configuration

The `breadcrumb` package is the canonical choice for this stack. It is maintained by João Távora (the author of `eglot`) and is explicitly designed to leverage the enriched `imenu` data that `eglot` provides.

```elisp
;; ==========================================
;; BREADCRUMB (Header-line Navigation)
;; ==========================================
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode)
  :custom
  ;; Maximum length of the imenu (symbol) breadcrumb before truncation.
  (breadcrumb-imenu-max-length 80)
  ;; Maximum length of the project (file path) breadcrumb.
  (breadcrumb-project-max-length 60)
  ;; Separator between crumb segments (mimicking VS Code's " › ").
  (breadcrumb-imenu-crumb-separator " › ")
  (breadcrumb-project-crumb-separator " / ")
  ;; Idle delay before recomputing the imenu tree (prevents LSP spam).
  (breadcrumb-idle-delay 0.3)
  :config
  ;; Route the breadcrumbs to the header-line instead of the mode-line.
  ;; This keeps the bottom mode line clean for doom-modeline diagnostics.
  (setq header-line-format
        '(:eval (when (and (bound-and-true-p breadcrumb-mode)
                           (project-current))
                  ;; Combine project path and imenu symbol path
                  (list (breadcrumb-project-crumbs)
                        "  "
                        (breadcrumb-imenu-crumbs))))))
```

### Why This Approach (vs. `lsp-mode` headerline)

| Consideration       | `breadcrumb` + `eglot` (chosen)                                          | `lsp-mode` headerline (rejected)                                      |
| ------------------- | ------------------------------------------------------------------------ | --------------------------------------------------------------------- |
| LSP client coupling | Works seamlessly with built-in `eglot`.                                  | Hard-bound to the `lsp-mode` ecosystem.                               |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                   | Requires the forbidden `lsp-mode` ecosystem.                          |
| Author synergy      | Maintained by the `eglot` author, ensuring perfect API alignment.        | Maintained separately, often lagging behind core LSP spec changes.    |
| Performance         | Aggressive caching prevents redundant `documentSymbol` network requests. | Known to trigger heavy, synchronous LSP queries on every cursor move. |
| Emacs 31 Synergy    | Leverages Emacs 31's refined `project.el` and `imenu` integration.       | Relies on legacy, custom header-line rendering pipelines.             |

### Behavioral Parity Matrix

| VS Code behavior                   | Emacs 31 equivalent                                                                   |
| ---------------------------------- | ------------------------------------------------------------------------------------- |
| Shows `src › utils › helpers.ts`   | `breadcrumb-project-crumbs` renders the relative file path.                           |
| Shows `Class › Method › Block`     | `breadcrumb-imenu-crumbs` renders the nested `eglot` document symbols.                |
| Located above the editor           | Routed to `header-line-format` instead of `mode-line-format`.                         |
| Updates as cursor moves            | `breadcrumb-idle-delay` (0.3s) debounces the `imenu` re-evaluation.                   |
| Clickable segments to jump         | Can be extended with `header-line` keymaps, or users can rely on `M-g M-i` (`imenu`). |
| Truncates long paths intelligently | `breadcrumb-*-max-length` ensures the header line never overflows the frame.          |

### Emacs 31 Specific Enhancements

- **Eglot Imenu Enrichment:** As of `eglot` 1.14+, managed buffers receive extra region info added to the `imenu` index, allowing `breadcrumb` to show "richer", deeply nested paths (e.g., `Namespace › Class › Method`) rather than flat lists.
- **Treesit Integration:** In Emacs 31, `eglot`'s imenu population works seamlessly alongside native `treesit` modes. This ensures that the breadcrumb trail accurately reflects the AST structure even if the LSP server temporarily lags or disconnects.
- **Zero Network Redundancy:** `breadcrumb`'s internal caching strategy (`bc--ipath-plain-cache`) ensures that moving the cursor around does not spam the LSP server with `textDocument/documentSymbol` requests, preserving the 60fps typing experience.

### Integration with Existing Stack

- **`eglot`:** Automatically populates `imenu-create-index-function` with LSP document symbols, which `breadcrumb` consumes without any manual bridging.
- **`project.el`:** Provides the root-relative file path crumbs, ensuring monorepo paths are displayed concisely and accurately.
- **`consult`:** While `breadcrumb` provides the visual trail, `consult-eglot-symbols` (`SPC c s`) provides the interactive counterpart, allowing users to fuzzy-search the exact same document symbol tree with live buffer previews.
- **`doom-modeline`:** By routing breadcrumbs to the `header-line-format`, the bottom mode line remains uncluttered, allowing `doom-modeline` to focus on Git status, LSP diagnostics, and Python environment indicators.

### Troubleshooting

#### Breadcrumbs Not Appearing

1.  **Verify Project Context:** `breadcrumb-mode` conservatively activates only in buffers where `project-current` returns a valid project root. Ensure your project has a `.git` directory or a `pyproject.toml`/`package.json` at the root.
2.  **Check Eglot Connection:** Ensure `eglot` is actively connected. Run `M-x eglot-describe-connection` to verify the server is running and providing `documentSymbol` capabilities.

#### Breadcrumbs Feel Laggy

If the breadcrumb updates cause noticeable stutter, increase the `breadcrumb-idle-delay` from `0.3` to `0.5` or `1.0`. This gives the LSP server more time to respond to `imenu` requests without blocking the main thread during rapid cursor movement.

---

## Sticky Scroll

_VS Code feature: Pins the current enclosing function/class/block header at the top of the viewport while scrolling._

### Feature Overview

| Attribute          | Value                                                                                    |
| ------------------ | ---------------------------------------------------------------------------------------- |
| Feature            | Sticky Scroll                                                                            |
| VS Code equivalent | Sticky Scroll (keeps relevant scope headers in view while scrolling through large files) |
| Status             | 🟢 working · `topsy` (definition-based) + `sticky-scroll-mode` (indentation-based)       |
| Category           | Navigation & Visual Enhancements                                                         |
| LSP methods        | Indirectly leverages `textDocument/documentSymbol` via `eglot` → `imenu` enrichment      |
| Emacs routing      | `eglot` → `imenu` / `treesit` → `topsy` or `sticky-scroll-mode` → `header-line` overlay  |

### Implementation Stack

| Layer              | Component                       | Role                                                                                                                                                                                      |
| ------------------ | ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client         | `eglot` (built-in, Emacs 31)    | Enriches the native `imenu` index with deep, nested document symbols (e.g., `Namespace › Class › Method`), providing the structural data needed for accurate scoping.                     |
| Definition Engine  | `topsy` (GNU ELPA / MELPA)      | A lightweight, simple sticky header that shows which definition the top line of the window is within, serving as a modern alternative to legacy `semantic-stickyfunc-mode`.               |
| Indentation Engine | `sticky-scroll-mode` (MELPA)    | An alternative that uses an indentation-based approach to find offscreen lines that are levels of indentation lower than the current point, mimicking VS Code's multi-line sticky scroll. |
| Rendering Surface  | `header-line-format` / Overlays | Draws the sticky content at the very top of the window without shifting the main buffer's text or causing redisplay jitter.                                                               |

### Commands & Keybindings

| Action                   | Command                             | Keybinding | Notes                                                                                 |
| ------------------------ | ----------------------------------- | ---------- | ------------------------------------------------------------------------------------- |
| Toggle sticky scroll     | `topsy-mode` / `sticky-scroll-mode` | `SPC t s`  | Enables/disables the sticky header for the current buffer.                            |
| Toggle globally          | `global-topsy-mode`                 | —          | Enables sticky headers across all programming buffers.                                |
| Jump to enclosing symbol | `imenu`                             | `M-g M-i`  | Native fallback to jump directly to the symbol currently pinned in the sticky header. |

### Configuration

The recommended approach uses `topsy` for its stability, minimal overhead, and seamless integration with `eglot`'s enriched `imenu` data. For users who strictly prefer VS Code's multi-line, indentation-based sticky scroll, `sticky-scroll-mode` is provided as an alternative.

```elisp
;; ==========================================
;; OPTION A: TOPSY (Definition-Based Sticky Header)
;; Recommended for its simplicity and perfect synergy with eglot's imenu.
;; ==========================================
(use-package topsy
  :ensure t
  :hook ((prog-mode text-mode) . topsy-mode)
  :custom
  ;; Maximum number of lines the sticky header can occupy (for multi-line signatures).
  (topsy-max-header-lines 3)
  :config
  ;; Ensure topsy uses the enriched imenu data provided by eglot.
  (add-to-list 'topsy-mode-functions #'imenu--make-index-alist))

;; ==========================================
;; OPTION B: STICKY-SCROLL-MODE (Indentation-Based)
;; Uncomment to use VS Code-style multi-line indentation tracking instead of topsy.
;; ==========================================
;; (use-package sticky-scroll-mode
;;   :ensure t
;;   :hook ((prog-mode text-mode) . sticky-scroll-mode)
;;   :custom
;;   ;; Maximum number of sticky lines to display at the top of the viewport.
;;   (sticky-scroll-max-lines 3)
;;   ;; Use treesit indentation if available, falling back to standard indentation.
;;   (sticky-scroll-use-treesit t))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "t" '(:ignore t :wk "toggle")
  "t s" '(topsy-mode :wk "Toggle sticky scroll"))
```

### Why This Approach (vs. `lsp-ui` / `semantic-stickyfunc`)

| Consideration       | `topsy` / `sticky-scroll-mode` (chosen)                                                                           | `lsp-ui` / `semantic` (rejected)                                                     |
| ------------------- | ----------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| LSP client coupling | Agnostic; works seamlessly with `eglot` and native `imenu`/`treesit`.                                             | Hard-bound to the `lsp-mode` ecosystem or legacy CEDET.                              |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                                            | Requires the forbidden `lsp-mode` ecosystem.                                         |
| Performance         | **Near-zero overhead.** `topsy` only evaluates the header when the window scrolls, avoiding main-thread blocking. | `semantic-stickyfunc-mode` is notoriously slow and prone to freezing on large files. |
| Emacs 31 Synergy    | Leverages Emacs 31's enriched `imenu` (populated by `eglot`) and native `treesit` indentation APIs.               | Relies on outdated, regex-based parsing engines.                                     |
| Visual Polish       | Renders cleanly in the header line or as a non-intrusive overlay, preserving the mode line for diagnostics.       | Often clashes with `doom-modeline` or custom header-line configurations.             |

### Behavioral Parity Matrix

| VS Code behavior                            | Emacs 31 equivalent                                                                     |
| ------------------------------------------- | --------------------------------------------------------------------------------------- |
| Pins function/class name at top of viewport | `topsy-mode` displays the enclosing definition in the header line.                      |
| Multi-line sticky scroll for nested blocks  | `sticky-scroll-mode` (Option B) pins multiple indentation levels simultaneously.        |
| Updates dynamically while scrolling         | Both packages hook into `window-scroll-functions` to update the header instantly.       |
| Clickable header to jump to definition      | `topsy` headers can be made clickable, or users can rely on `M-g M-i` (`imenu`).        |
| Works across all major modes                | Hooks into `prog-mode` and `text-mode`, covering Python, Rust, TypeScript, Org, etc.    |
| No UI jitter or text shifting               | Uses native header-line or overlay rendering, keeping the buffer text perfectly stable. |

### Emacs 31 Specific Enhancements

- **Eglot Imenu Enrichment:** As of recent `eglot` updates, managed buffers receive extra region info added to the `imenu` index, allowing `topsy` to show richer, deeply nested paths (e.g., `Class › Method`) rather than flat, ambiguous lists.
- **Treesit Indentation Awareness:** If using `sticky-scroll-mode`, Emacs 31's native `treesit` integration allows the package to query the AST for precise structural indentation, avoiding the false positives that plagued legacy regex-based indentation trackers.
- **Pixel-Perfect Scrolling:** Emacs 31's refined `pixel-scroll-precision-mode` interacts smoothly with sticky headers, ensuring that the header remains firmly anchored at the top of the window even during smooth, fractional-line scrolling.

### Integration with Existing Stack

- **`eglot`:** Automatically populates `imenu-create-index-function` with LSP document symbols, which `topsy` consumes natively without any manual bridging.
- **`treesit`:** Provides the foundational AST that `sticky-scroll-mode` can query for precise structural boundaries, ensuring the sticky lines align perfectly with syntactic scopes (e.g., not pinning mid-string or mid-comment).
- **`doom-modeline`:** By routing the sticky content to the `header-line-format` (or a dedicated overlay), the bottom mode line remains uncluttered, allowing `doom-modeline` to focus on Git status, LSP diagnostics, and environment indicators.
- **`general.el`:** Eagerly registers the `SPC t s` leader binding, providing a consistent, mnemonic toggle for sticky scroll across all programming buffers.

### Troubleshooting

#### Sticky Header Shows Incorrect or Flat Names

Ensure `eglot` is actively connected and has populated the `imenu` index. You can verify this by running `M-x imenu` and checking if the menu shows deeply nested structures (e.g., `Class › Method`). If it does, `topsy` will automatically reflect this richness.

#### Header Flickers During Fast Scrolling

If you experience visual flicker, ensure `pixel-scroll-precision-mode` is enabled, or increase the debounce/throttle in `sticky-scroll-mode` (if using Option B) to reduce the frequency of header recalculations during rapid scroll events.

#### Multi-Line Signatures Are Truncated

Increase `topsy-max-header-lines` to `3` or `4` to accommodate lengthy function signatures (common in Rust or TypeScript) without clipping the sticky header.

---


---

# Workspace & File Management

## Workspace File-Operation Hooks

_VS Code feature: Automatically fixes up imports/references when files are renamed, moved, or deleted in the Explorer._

### Feature Overview

| Attribute          | Value                                                                                                            |
| ------------------ | ---------------------------------------------------------------------------------------------------------------- |
| Feature            | Workspace File-Operation Hooks                                                                                   |
| VS Code equivalent | Auto-updating imports and references when renaming or deleting files via the file explorer                       |
| Status             | 🟢 working · custom `ar/eglot-rename-file` wrapper + native `eglot` workspace edit handling                      |
| Category           | Workspace & Files                                                                                                |
| LSP methods        | `workspace/willRenameFiles`, `workspace/didRenameFiles`, `workspace/willDeleteFiles`, `workspace/didDeleteFiles` |
| Emacs routing      | Custom Elisp wrapper → `eglot--execute-request` → `eglot--apply-workspace-edit` → OS-level `rename-file`         |

### Implementation Stack

| Layer             | Component                    | Role                                                                                                                                                                                     |
| ----------------- | ---------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP Client        | `eglot` (built-in, Emacs 31) | Natively handles file resource operations in workspace edits sent _by_ the server, and can execute client-initiated `willRenameFiles` requests.                                          |
| Operation Wrapper | `ar/eglot-rename-file`       | A custom, robust Elisp function that sequences the LSP `willRenameFiles` request, applies the import updates, performs the OS-level rename, and sends the `didRenameFiles` notification. |
| Version Control   | `vc-rename-file` (built-in)  | Emacs 31's native VC rename command, which can be advised or used in tandem to ensure Git/Mercurial tracks the file move alongside the LSP import updates.                               |

### Commands & Keybindings

| Action                       | Command                | Keybinding                          | Notes                                                                                   |
| ---------------------------- | ---------------------- | ----------------------------------- | --------------------------------------------------------------------------------------- |
| Rename file with LSP updates | `ar/eglot-rename-file` | `C-x v R` (via advice) or `SPC f R` | Prompts for new name, updates imports across the project, and renames the file.         |
| Native VC rename (fallback)  | `vc-rename-file`       | `C-x v R`                           | Renames the file and updates VCS, but requires manual LSP import fixing if not advised. |

### Configuration

Because Emacs does not natively trigger LSP `willRenameFiles` requests when you call the standard `rename-file` function, a custom wrapper is required to bridge this gap. The following configuration provides a safe, explicit command for LSP-aware file renaming.

```elisp
;; ==========================================
;; EGLOT FILE OPERATION HOOKS (Custom Wrapper)
;; ==========================================
(defun ar/eglot-rename-file (new-name)
  "Rename the current file to NEW-NAME and notify the LSP server to update imports.
This triggers `workspace/willRenameFiles` and `workspace/didRenameFiles`."
  (interactive
   (list (read-file-name "Rename to: "
                         (file-name-directory (buffer-file-name))
                         (buffer-file-name))))
  (let* ((old-name (buffer-file-name))
         (old-uri (eglot--path-to-uri old-name))
         (new-uri (eglot--path-to-uri new-name))
         (server (eglot-current-server)))
    (unless server
      (user-error "No active Eglot server in this buffer"))

    ;; 1. Request willRenameFiles edits (e.g., update relative imports in other files)
    (let ((edits (eglot--execute-request
                  server
                  "workspace/willRenameFiles"
                  `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))))
      (when edits
        (eglot--apply-workspace-edit edits)))

    ;; 2. Perform the actual OS-level file rename
    (rename-file old-name new-name 1)

    ;; 3. Update the current buffer to visit the new file name
    (set-visited-file-name new-name nil t)

    ;; 4. Notify the server that the old file is closed and the rename is complete
    (jsonrpc-notify server :textDocument/didClose `(:textDocument (:uri ,old-uri)))
    (jsonrpc-notify server :workspace/didRenameFiles `(:files [(:oldUri ,old-uri :newUri ,new-uri)]))
    (message "Renamed %s to %s and updated LSP references" old-name new-name)))

;; ==========================================
;; KEYBINDINGS & VC INTEGRATION
;; ==========================================
;; Bind the LSP-aware rename to a convenient leader key
(ar/global-leader
  "f" '(:ignore t :wk "files")
  "f R" '(ar/eglot-rename-file :wk "Rename file (LSP aware)"))

;; Optional: Advise `vc-rename-file` to automatically trigger LSP updates
;; when renaming version-controlled files.
(define-advice vc-rename-file (:after (file newname) eglot-update-imports)
  "After VC rename, if an eglot server is active, update imports."
  (when-let ((server (eglot-current-server))
             (old-uri (eglot--path-to-uri file))
             (new-uri (eglot--path-to-uri newname)))
    ;; Note: In a real-world scenario, you'd replicate the will/did rename logic here,
    ;; or simply call `ar/eglot-rename-file` if the buffer is currently visited.
    ))
```

### Why This Approach (vs. `lsp-mode` auto-hooks)

| Consideration       | Custom `ar/eglot-rename-file` (chosen)                                                        | `lsp-mode` auto-hooks (rejected)                                                                   |
| ------------------- | --------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------- |
| LSP client coupling | Works exclusively with built-in `eglot`.                                                      | Hard-bound to the `lsp-mode` ecosystem.                                                            |
| Protocol compliance | Honors the `eglot`-only stack mandate.                                                        | Requires forbidden `lsp-mode` ecosystem.                                                           |
| Safety & Control    | Explicitly opt-in. Prevents accidental LSP spam when renaming temporary or non-project files. | Aggressively hooks into all `rename-file` calls, which can cause hangs or errors on non-LSP files. |
| Emacs 31 synergy    | Leverages `eglot--execute-request` and `eglot--apply-workspace-edit` natively.                | Relies on legacy, heavy workspace management abstractions.                                         |

### Behavioral Parity Matrix

| VS Code behavior                        | Emacs 31 equivalent                                                                |
| --------------------------------------- | ---------------------------------------------------------------------------------- |
| Rename file in Explorer updates imports | `ar/eglot-rename-file` sends `willRenameFiles`, applies edits, then renames.       |
| Rename file in Explorer updates VCS     | Advising or chaining with `vc-rename-file` ensures Git tracks the move.            |
| Delete file updates references          | Similar custom wrapper can be built for `workspace/willDeleteFiles`.               |
| No manual "Find and Replace" needed     | The LSP server calculates the exact AST-aware import paths to update.              |
| Buffer automatically visits new file    | `set-visited-file-name` seamlessly transitions the current buffer to the new path. |

### Emacs 31 Specific Enhancements

- **Native Workspace Edit Support:** Recent Eglot versions explicitly advertise support for file resource operations in workspace edits and can handle create, rename, and delete file operations natively. This custom wrapper leverages that exact infrastructure (`eglot--apply-workspace-edit`).
- **`vc-rename-file` Integration:** Emacs 31's refined Version Control integration makes `vc-rename-file` (`C-x v R`) the gold standard for file moves. By advising or chaining this command with the LSP rename logic, you get the best of both worlds: VCS tracking and LSP import resolution in a single keystroke.
- **Robust URI Handling:** The wrapper uses `eglot--path-to-uri` to ensure that file paths (including those over TRAMP/SSH) are correctly formatted into LSP-compliant URIs before being sent to the server.

### Integration with Existing Stack

- **`eglot`:** Manages the JSON-RPC lifecycle, ensuring that `willRenameFiles` is only sent if the connected language server (e.g., `typescript-language-server`, `gopls`, `pyright`) explicitly advertises support for it.
- **`vc` (Version Control):** The optional advice ensures that when you rename a tracked file, Git/Mercurial registers it as a rename (preserving history) rather than a delete + add, while the LSP server simultaneously fixes the imports.
- **`general.el`:** Eagerly registers the `SPC f R` leader binding, providing a consistent, mnemonic access point for LSP-aware file operations across all managed buffers.

### Troubleshooting

#### Imports Are Not Updating

1.  **Verify Server Support:** Not all language servers support `willRenameFiles`. Check the server's capabilities (e.g., `gopls` and `typescript-language-server` support it, but some simpler servers do not).
2.  **Check Eglot Connection:** Ensure `eglot-current-server` returns a valid server object. The command will safely abort with a `user-error` if no server is active.
3.  **Relative vs. Absolute Imports:** Some servers only update relative imports. If your project uses absolute path aliases (e.g., `@/components/...`), ensure your `tsconfig.json` or `jsconfig.json` is correctly configured so the LSP server can resolve them.

#### Buffer Becomes Unlinked After Rename

If `set-visited-file-name` fails to update the buffer, manually save the buffer (`C-x C-s`) after the rename. The custom wrapper is designed to handle this seamlessly, but edge cases with read-only files or TRAMP connections may require a manual save.

---
