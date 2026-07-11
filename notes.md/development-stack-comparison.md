# The Definitive 1:1 Stack Translation Guide (Emacs 30/31)

**`lsp-mode` / `lsp-ui` / `dap-mode` / `flycheck` $\longleftrightarrow$ `eglot` / `flymake` / `dape` / `apheleia` + Modern Overlay Ecosystem**

This document serves as the exhaustive, forensic 1:1 feature parity mapping for transitioning from the heavy "IDE" stack to the modern, native-first Emacs 30/31 architecture. It explicitly incorporates the latest 2025/2026 overlay ecosystems, specifically **`flyover`** for aesthetic inline diagnostics and **`corfu-popupinfo`** for completion documentation, ensuring every single inline overlay window from `lsp-ui` has a direct, mathematically sound counterpart.

---

## 1. Core Protocol & Architecture (`lsp-mode` $\rightarrow$ `eglot`)

| Legacy Stack (`lsp-mode`)                                                             | Native-First Stack (`eglot` + Emacs 30/31)                                                                                                                   | Translation Notes                                                                                         |
| :------------------------------------------------------------------------------------ | :----------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------- |
| **JSON-RPC Engine**: Custom Elisp JSON parser.                                        | **`eglot` + `eglot-booster`**: Native async processes + Rust binary offloading.                                                                              | `eglot-booster` parses JSON off the main thread, returning native Elisp bytecode to prevent UI blocking . |
| **Client Files**: Dedicated `lsp-<language>.el` files.                                | **`eglot-server-programs`**: A single, unified plist mapping major modes to server binaries.                                                                 | Eliminates hundreds of heavy client dependencies.                                                         |
| **Auto-configure**: `lsp-auto-configure` hooks into `company`, `flycheck`, `lsp-ui`.  | **Native Integration**: Automatically registers with `xref`, `flymake`, `eldoc`, `corfu`, and `project.el`.                                                  | Zero configuration required; respects Emacs boundaries.                                                   |
| **Deferred Startup**: `lsp-deferred`.                                                 | **`eglot-ensure`**: Hooks into `prog-mode` but only contacts the server when the buffer is fully initialized and visible.                                    |
| **Multi-root Workspace**: Custom workspace management.                                | **`project.el`**: Eglot natively groups buffers under the exact same `project.el` root into a single LSP workspace.                                          |
| **File Watchers**: Aggressive `workspace/didChangeWatchedFiles`.                      | **`auto-revert-mode` + `file-notify-add-watch`**: Eglot intentionally avoids aggressive file-watching to save resources, relying on native OS notifications. |
| **Docker Integration**: `lsp-docker` companion package.                               | **TRAMP Docker**: Handled natively via TRAMP methods (`/docker:container:/path`) or wrapping the binary in `eglot-server-programs`.                          |
| **Performance Tuning**: `gc-cons-threshold` manipulation, plist vs hash-table tweaks. | **`read-process-output-max`**: Bumping this to 4MB (in `early-init.el`) is the _only_ required tuning to prevent stuttering on massive LSP payloads.         |
| **Client Documentation**: `lsp-clients.json` / `lsp-doc.el`.                          | **`eglot-describe-server`**: Eglot 1.24+ provides auto-generated documentation for the connected language server.                                            |

---

## 2. Completion & Documentation Overlays (`company` + `lsp-ui-doc` $\rightarrow$ `corfu` + `corfu-popupinfo`)

| Legacy Stack (`lsp-mode` + `lsp-ui`)                                                           | Native-First Stack (`corfu` + `cape` + `corfu-popupinfo`)                                                 | Translation Notes                                                                                                         |
| :--------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------ |
| **Backend**: `company-capf` / `company-lsp` (deprecated).                                      | **`completion-at-point-functions`**: Eglot natively registers `eglot-completion-at-point`.                | Legacy `company` backends are explicitly rejected.                                                                        |
| **UI Popup**: `company-mode` traditional dropdown.                                             | **`corfu`**: Lightweight, child-frame-based in-buffer completion.                                         |
| **Network Blocking**: LSP requests block the main thread, causing "stutter-and-vanish" popups. | **`cape-wrap-noninterruptible`**: Shields Eglot network requests from `quit` signals triggered by typing. |
| **Candidate Merging**: Exclusive backends (first to return wins).                              | **`cape-wrap-nonexclusive`**: Merges LSP candidates seamlessly with local Dabbrev and File candidates.    |
| **Hover Popover (`lsp-ui-doc`)**: WebKit child frame at point.                                 | **`eldoc-box`**: Intercepts ElDoc output into a posframe for hover documentation.                         | _Paradigm Shift_: Abandons heavy WebKit for native Markdown rendering.                                                    |
| **Completion Docs (`lsp-ui-doc`)**: Docs for selected candidate.                               | **`corfu-popupinfo`**: Displays rich candidate documentation in a side popup next to the Corfu menu .     | Completely replaces the need for `eldoc-box` during completion selection, preventing UI flicker and LSP network spam .    |
| **Deep Reading**: Focusing inside the `lsp-ui-doc` frame.                                      | **`eldoc-doc-buffer` (Emacs 29+)**: Opens a dedicated, fully interactive buffer.                          | Allows standard Evil motions, `isearch`, and copying from the documentation.                                              |
| **Signature Help**: `lsp-signature-mode` popup.                                                | **`eglot-signature-eldoc-function`** + **`corfu-popupinfo`**.                                             | Natively cycles overloaded signatures via `M-n`/`M-p` in the echo area, or renders them inside the Corfu popupinfo frame. |

---

## 3. Inline Diagnostics & Overlays (`flycheck` + `lsp-ui-flycheck` $\rightarrow$ `flymake` + `flyover`)

This is the most critical visual translation layer. Emacs 30+ has elevated `flymake` to achieve 1:1 parity with `flycheck`, and the introduction of **`flyover`** in 2025/2026 provides the aesthetic inline overlays that `lsp-ui` users expect [[1], [39]].

| Legacy Stack (`flycheck` + `lsp-ui`)                                                               | Native-First Stack (`flymake` + `flyover` + Emacs 30/31)                                                                            | Translation Notes                                                                                                                                    |
| :------------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Activation**: `flycheck-mode` / `global-flycheck-mode`.                                          | **`flymake-mode` / `global-flymake-mode`**: Automatically activated by `eglot-ensure` via `eglot--flymake-backend`.                 |
| **Checker Registry**: `flycheck-checkers` (100+ tools).                                            | **`flymake-collection`**: Ships with 100+ modern syntax-checking tools out of the box.                                              | Provides 1:1 ecosystem parity.                                                                                                                       |
| **Inline Overlays (`lsp-ui-flycheck` / `flycheck-posframe`)**: Ugly underlines or heavy posframes. | **`flyover`**: A modern, aesthetic inline overlay package for Flymake .                                                             | Replaces standard fringe/underline indicators and `flymake-show-diagnostics-at-end-of-line` with beautiful, theme-aware inline diagnostic overlays . |
| **Overlay Conflict Prevention**: Manual tweaking.                                                  | **`flyover` Auto-Hide**: Automatically hides overlays when completion popups (like Corfu) are active, preventing visual conflicts . |
| **Sideline Diagnostics (`lsp-ui-sideline`)**: Right margin text.                                   | **`sideline-flymake`** OR Emacs 30 native **`flymake-show-diagnostics-at-end-of-line`**.                                            | Native ghost-text rendering via C-level display properties.                                                                                          |
| **Sideline Hover/Actions**: `lsp-ui-sideline-show-hover`.                                          | **`sideline-eldoc`** + **`sideline-code-actions`**.                                                                                 | Shows contextual information and available code actions in the right margin.                                                                         |
| **Modeline Diagnostics**: `lsp-modeline-diagnostics-mode`.                                         | **Native Flymake Modeline** + **`doom-modeline`**.                                                                                  | Emacs 30+ features native Flymake modeline counters that display error/warning counts dynamically.                                                   |
| **Error List**: `flycheck-list-errors` / `lsp-treemacs-errors-list`.                               | **`flymake-show-project-diagnostics`** + **`consult-flymake`**.                                                                     | Native, auto-updating workspace-wide error buffers + Vertico fuzzy search.                                                                           |
| **Explain Error**: `flycheck-explain-error-at-point`.                                              | **`eldoc-box`** / **`flymake-popon`**.                                                                                              | Renders LSP diagnostic payloads in floating child frames on hover.                                                                                   |

---

## 4. Code Navigation & Peek Overlays (`lsp-ui-peek` $\rightarrow$ `consult-xref`)

| Legacy Stack (`lsp-mode` + `lsp-ui`)                         | Native-First Stack (`xref` + `consult` + `breadcrumb`)                                                | Translation Notes                                                                    |
| :----------------------------------------------------------- | :---------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------- |
| **Find Definition/References**: `lsp-find-definition`.       | **`xref-find-definitions` (`M-.`)**: Eglot acts as a native `xref` backend.                           |
| **Peek UI Overlay (`lsp-ui-peek`)**: Inline overlay window.  | **`consult-xref`**: Routes multiple definitions/references through Vertico with live buffer previews. | Replaces the intrusive `lsp-ui-peek` overlay with a searchable minibuffer dropdown.  |
| **Workspace Symbols**: `lsp-ui-peek-find-workspace-symbol`.  | **`consult-eglot-symbols`**: Async live-preview via Vertico/Orderless.                                |
| **Call Hierarchy / Tree Views**: `lsp-treemacs`.             | **`consult-imenu`** + **`consult-outline`** + **`treemacs`**.                                         | AST-aware outline navigation via Tree-sitter and `project.el`.                       |
| **Imenu Overlay (`lsp-ui-imenu`)**: Dedicated side window.   | **`consult-imenu`**: Driven by Eglot's `xref` backend and Tree-sitter.                                | Groups entries by "kind" natively; rendered via Vertico/Embark buffer layouts.       |
| **Headerline Breadcrumb**: `lsp-headerline-breadcrumb-mode`. | **`breadcrumb`**: Written by Eglot's author, integrates deeply with `project.el` and `imenu`.         | Renders a clickable, VSCode-style headerline breadcrumb (`project > file > symbol`). |

---

## 5. Code Actions, Lens, and Semantic Overlays

| Legacy Stack (`lsp-mode`)                                              | Native-First Stack (`eglot` + Emacs 30/31)                                    | Translation Notes                                                                                                                                                                                                                                                |
| :--------------------------------------------------------------------- | :---------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Execute Action**: `lsp-execute-code-action`.                         | **`eglot-code-actions`**: Routed through a `transient-define-prefix` menu.    |
| **Modeline Indicator**: `lsp-modeline-code-actions-mode`.              | **`eglot-code-action-indications`** (Emacs 31) + **`sideline-code-actions`**. |
| **Code Lens Overlay (`lsp-lens`)**: Inline reference counts/run links. | **`eglot-codelens`** (External) + **`eglot-inlay-hints-mode`** (Native).      | _Architectural Divergence_: Eglot core explicitly rejects native Code Lens to avoid UI clutter. Parity is achieved via the community `eglot-codelens` package for explicit lenses, and the native Emacs 30 `eglot-inlay-hints-mode` for inline type annotations. |
| **Semantic Tokens**: `lsp-semantic-tokens-mode`.                       | **`eglot-semantic-tokens-mode`**: Native Emacs 30 implementation.             | Queries server for token trees and applies Emacs faces natively.                                                                                                                                                                                                 |
| **Rename Highlighting Overlay**: Dedicated face during rename.         | **`symbol-overlay`** + **`eglot-rename`**.                                    | `symbol-overlay` provides the dedicated highlighting face for the identifier being renamed across the workspace via `xref`.                                                                                                                                      |

---

## 6. Formatting & Renaming (`lsp-format` $\rightarrow$ `apheleia`)

| Legacy Stack (`lsp-mode`)                                | Native-First Stack (`apheleia` + `eglot`)                                                                   | Translation Notes                      |
| :------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------- | :------------------------------------- |
| **Format Buffer**: `lsp-format-buffer` (sync/blocking).  | **`apheleia`**: Runs formatters asynchronously in the background.                                           | Zero cursor jumping, zero UI blocking. |
| **On-Type Formatting**: `lsp-enable-on-type-formatting`. | **`electric-pair-mode`**: Eglot intentionally avoids on-type LSP formatting to prevent main-thread stutter. |
| **Rename Symbol**: `lsp-rename` (custom UI).             | **`eglot-rename`**: Uses native `xref` to apply changes atomically across the workspace.                    |

---

## 7. Debugger Integration (`dap-mode` $\rightarrow$ `dape`)

| Legacy Stack (`dap-mode` + `dap-ui`)                                                 | Native-First Stack (`dape` + Emacs 30/31)                                                    | Translation Notes                                                                                                          |
| :----------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------- |
| **Core Client**: Heavy `dap.el` framework.                                           | **`dape`**: Lightweight, utilizes native Emacs APIs and `plz` for async HTTP.                | Vastly superior memory footprint.                                                                                          |
| **Configuration**: VSCode `launch.json`.                                             | **`dape-configs`** (Elisp plist) or `.dir-locals.el`.                                        | _Paradigm Shift_: `dape` explicitly rejects `launch.json` to avoid JSON parsing overhead. Mathematically safer and faster. |
| **UI Windows (`dap-ui`)**: `dap-ui-sessions`, `dap-ui-locals`, `dap-ui-breakpoints`. | **`dape-info-*` buffers**: `dape-info-sessions`, `dape-info-scope`, `dape-info-breakpoints`. | Native keymaps (`RET`, `d`, `D`, `m`, `u`, `U`) for breakpoint management.                                                 |
| **Controls / Tooltips**: `dap-ui-controls-mode`, `dap-tooltip-mode`.                 | **`dape-mouse-mode`** + **`eldoc`**.                                                         | On-screen controls and mouse-hover support for variable inspection.                                                        |
| **Menu**: `dap-hydra`.                                                               | **`transient-define-prefix`**: Grid-aligned, Nerd-Icon-accented menu (`ar/toggles-dape`).    |
| **Auto-Install**: `dap-ruby-setup`, `dap-go-setup`.                                  | **System Package Managers**: `dape` rejects `dap-install`.                                   | Relies on Mason, Nix, or OS packages for environment isolation.                                                            |

---

## 8. Summary of the External Package Manifest (The "Eglot Parity" Stack)

To achieve 100% visual and functional parity with the `lsp-mode` + `lsp-ui` stack using the native Emacs 30/31 architecture, the following external manifest is required:

### Core & Performance

1.  **`eglot-booster`**: Rust-accelerated JSON-RPC parsing.
2.  **`consult-eglot`**: Vertico-backed Workspace Symbol search.

### Overlays & UI (The `lsp-ui` Replacements)

3.  **`corfu-popupinfo`**: Replaces `lsp-ui-doc` for completion candidate documentation .
4.  **`eldoc-box`**: Replaces `lsp-ui-doc` for general hover popovers.
5.  **`flyover`**: Replaces `lsp-ui-flycheck` and `flycheck-posframe` with beautiful, aesthetic inline diagnostic overlays that auto-hide during Corfu completion .
6.  **`sideline` + `sideline-flymake` + `sideline-eldoc` + `sideline-code-actions`**: Replaces `lsp-ui-sideline`.
7.  **`consult-xref`**: Replaces `lsp-ui-peek` with live-preview Vertico dropdowns.
8.  **`breadcrumb`**: Replaces `lsp-headerline-breadcrumb` and `lsp-ui-imenu` headerline integration.
9.  **`symbol-overlay`**: Replaces `lsp-mode`'s automatic symbol highlighting and rename overlays.
10. **`eglot-codelens`**: Replaces `lsp-lens` (since Eglot core rejects it).

### Formatting, Linting & Debugging

11. **`apheleia`**: Replaces `lsp-format` entirely.
12. **`flymake-collection`**: The 100+ checker registry replacing `flycheck`.
13. **`dape`**: Replaces `dap-mode` and `dap-ui` entirely.

### Native Emacs 30/31 Features to Enable:

- `(eglot-semantic-tokens-mode 1)`
- `(eglot-inlay-hints-mode 1)` (Replaces type-hint code lenses)
- `(setq flymake-show-diagnostics-at-end-of-line t)` (Native Error Lens, alternative to `flyover`)
- `flymake-show-project-diagnostics` (Workspace error list)
