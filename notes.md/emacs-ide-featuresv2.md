---
---

---

# Peek Definition / Peek References — Emacs 31 Implementation

> **VS Code feature:** Shows the target inline in an expandable panel without switching editor tabs.

## Feature Overview

| Attribute              | Value                                                |
| ---------------------- | ---------------------------------------------------- |
| **Feature**            | Peek Definition / Peek References                    |
| **VS Code equivalent** | Inline expandable panel (Alt+F12 / Shift+Alt+F12)    |
| **Status**             | 🟢 `editor-chrome` · `peek` package · no lsp-ui      |
| **Category**           | Editor Chrome (UI treatment of LSP data)             |
| **LSP methods**        | `textDocument/definition`, `textDocument/references` |
| **Emacs routing**      | `eglot` → `xref` → `peek` overlay engine             |

## Implementation Stack

| Layer                     | Component                      | Role                                                                                            |
| ------------------------- | ------------------------------ | ----------------------------------------------------------------------------------------------- |
| **LSP Client**            | `eglot` (built-in, Emacs 31)   | Drives `textDocument/definition` and `textDocument/references`, routes responses through `xref` |
| **Navigation Framework**  | `xref` (built-in)              | Shared history, location abstraction, `xref-show-definitions-function` hook                     |
| **Inline Overlay Engine** | `peek` (sr.ht/~meow_king/peek) | Renders definition/reference preview as a transient overlay anchored below the cursor line      |
| **Documentation Layer**   | `eldoc`                        | Surfaces hover docs _inside_ the peek panel via `peek-mode-enable-eldoc`                        |

## Commands & Keybindings

| Action                      | Command                         | Keybinding        | Notes                                                |
| --------------------------- | ------------------------------- | ----------------- | ---------------------------------------------------- |
| Peek definition             | `peek-xref-definition`          | `SPC c p d`       | Inline overlay of `textDocument/definition` target   |
| Peek references             | `peek-xref-references`          | `SPC c p r`       | Inline list of all `textDocument/references` matches |
| Dismiss peek panel          | `peek-overlay-dwim`             | `SPC c p q`       | Closes the transient overlay without moving point    |
| Toggle eldoc in peek        | `peek-mode-enable-eldoc`        | —                 | Shows hover info inside the peek panel               |
| Native xref jump (fallback) | `xref-find-definitions`         | `M-.`             | Opens `*xref*` buffer (non-inline)                   |
| Native xref refs (fallback) | `xref-find-references`          | `M-?`             | Opens `*xref*` buffer (non-inline)                   |
| Edit references in place    | `xref-change-to-xref-edit-mode` | `e` (in `*xref*`) | Emacs 31 native — Grep-Edit style propagation        |

## Configuration

### Option A: `peek` package (true inline overlay — VS Code parity)

```emacs-lisp
(use-package peek
  :ensure (peek :host sourcehut :repo "~meow_king/peek")
  :commands (peek-xref-definition peek-xref-references peek-overlay-dwim)
  :custom
  (peek-mode-enable-eldoc t)                     ;; show eldoc inside peek panel
  (peek-definition-function #'xref-find-definitions)
  (peek-references-function #'xref-find-references))
```

### Option B: Pure-native bottom panel (zero third-party code)

```emacs-lisp
(add-to-list 'display-buffer-alist
 '("\\*xref\\*"
   (display-buffer-in-side-window)
   (side . bottom)
   (window-height . 0.3)
   (window-parameters (no-delete-other-windows . t))))
```

> Forces the `*xref*` buffer into a 30% bottom side-window — achieves the same panel layout natively via Emacs' window management, with full Emacs 31 editable-xref support.

## Why This Approach (vs. `lsp-ui-peek`)

| Consideration           | `peek` (chosen)                                               | `lsp-ui-peek` (rejected)                |
| ----------------------- | ------------------------------------------------------------- | --------------------------------------- |
| **LSP client coupling** | Works with _any_ xref backend (eglot, dumb-jump, etags)       | Hard-bound to `lsp-mode`                |
| **Protocol compliance** | Honors the `eglot`-only stack mandate                         | Requires forbidden `lsp-mode` ecosystem |
| **Rendering engine**    | Native Emacs overlays                                         | Custom child-frame pipeline             |
| **Emacs 31 synergy**    | Pairs with `xref-change-to-xref-edit-mode` for editable peeks | No integration with Emacs 31 xref edits |
| **Maintenance**         | Active (2025–2026)                                            | Stale — tracks lsp-mode lifecycle       |

## Behavioral Parity Matrix

| VS Code behavior                 | Emacs 31 equivalent                                 |
| -------------------------------- | --------------------------------------------------- |
| Alt+F12 opens inline panel       | `SPC c p d` → `peek-xref-definition`                |
| Shift+Alt+F12 for references     | `SPC c p r` → `peek-xref-references`                |
| Esc closes panel without jumping | `SPC c p q` → `peek-overlay-dwim`                   |
| Enter jumps to target            | `RET` inside peek overlay                           |
| Panel shows hover docs           | `peek-mode-enable-eldoc t`                          |
| Edit references in place         | Emacs 31 `xref-change-to-xref-edit-mode` (native)   |
| No tab switching                 | Overlay anchored to current line — cursor stays put |

## Emacs 31 Specific Enhancements

- **Editable xref buffers**: The new `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*`) turns reference panels into writable surfaces — edits propagate back to originating files, mirroring the Grep-Edit workflow.
- **Mouse-mode integration**: `xref-mouse-mode` enables Ctrl+Click jumps, matching VS Code's click-to-peek conventions.
- **Richer eldoc**: `eldoc-help-at-pt` and ephemeral buffer rendering let peek panels display full markdown docstrings inline without `eldoc-box`.

---

---

---

# Breadcrumbs Bar — Emacs 31 Implementation

> **VS Code feature:** Path › file › enclosing symbol navigation bar above the editor.

## Feature Overview

| Attribute              | Value                                                                                            |
| ---------------------- | ------------------------------------------------------------------------------------------------ |
| **Feature**            | Breadcrumbs bar                                                                                  |
| **VS Code equivalent** | Top navigation bar showing `path › file › class › method` hierarchy                              |
| **Status**             | 🟢 `editor-chrome` · `breadcrumb` package (GNU ELPA) · no lsp-ui                                 |
| **Category**           | Editor Chrome (UI treatment of LSP/imenu data)                                                   |
| **LSP methods**        | `textDocument/documentSymbol` (via imenu integration)                                            |
| **Emacs routing**      | `project.el` (file path) + `imenu` (symbol hierarchy) → `breadcrumb-mode` → `header-line-format` |

## Implementation Stack

| Layer                    | Component                                           | Role                                                              |
| ------------------------ | --------------------------------------------------- | ----------------------------------------------------------------- |
| **Breadcrumb Engine**    | `breadcrumb` (GNU ELPA, by João Távora)             | Renders path and symbol hierarchy in `header-line-format`         |
| **Path Resolution**      | `project.el` (built-in)                             | Provides project-relative file paths via `project-current`        |
| **Symbol Hierarchy**     | `imenu` + `treesit`                                 | Extracts AST-based symbol structure (classes, functions, methods) |
| **Rendering Surface**    | `header-line-format`                                | Displays clickable breadcrumb segments above the buffer           |
| **Emacs 31 Enhancement** | `header-line-active` / `header-line-inactive` faces | New faces for focused vs unfocused window breadcrumb styling      |

## Commands & Keybindings

| Action                      | Command               | Keybinding | Notes                                             |
| --------------------------- | --------------------- | ---------- | ------------------------------------------------- |
| Toggle breadcrumbs globally | `breadcrumb-mode`     | `SPC t b`  | Enables/disables header-line breadcrumbs          |
| Jump to breadcrumb segment  | `mouse-1` (click)     | —          | Click any breadcrumb segment to jump              |
| Navigate breadcrumb menu    | `breadcrumb-jump`     | `SPC c b`  | Keyboard-driven breadcrumb navigation via `imenu` |
| Refresh breadcrumbs         | `breadcrumb--refresh` | —          | Auto-refreshes on buffer change                   |

## Configuration

### Option A: `breadcrumb` package (recommended — eglot author's solution)

```emacs-lisp
(use-package breadcrumb
  :ensure t  ; GNU ELPA package by João Távora (eglot author)
  :defer t
  :commands (breadcrumb-mode breadcrumb-jump)
  :custom
  ;; Show project-relative paths instead of absolute paths
  (breadcrumb-project-max-length 30)
  ;; Show imenu symbol hierarchy (class > method > function)
  (breadcrumb-imenu-max-length 40)
  ;; Separator between breadcrumb segments
  (breadcrumb-separator " › ")
  :config
  ;; Enable globally for all programming buffers
  (breadcrumb-mode 1))
```

### Option B: Pure-native header-line (zero third-party code)

```emacs-lisp
(defun ar/native-breadcrumb ()
  "Generate native breadcrumb string from project + imenu."
  (let ((project-name (when-let ((proj (project-current)))
                        (file-name-nondirectory
                         (directory-file-name (project-root proj)))))
        (file-name (when buffer-file-name
                     (file-relative-name buffer-file-name
                                         (or (when-let ((proj (project-current)))
                                               (project-root proj))
                                             default-directory))))
        (symbol-path (when (and (fboundp 'imenu--make-index-alist)
                                (derived-mode-p 'prog-mode))
                       (ignore-errors
                         (let ((index (imenu--make-index-alist t)))
                           (when index
                             (ar/imenu-current-symbol-path index)))))))
    (string-join (delq nil (list project-name file-name symbol-path))
                 " › ")))

(defun ar/imenu-current-symbol-path (index)
  "Extract current symbol path from imenu INDEX."
  ;; Simplified: returns first matching symbol at point
  ;; Full implementation would recursively search nested imenu structures
  (let ((pos (point)))
    (catch 'found
      (dolist (entry index)
        (when (and (consp entry)
                   (markerp (cdr entry))
                   (<= (marker-position (cdr entry)) pos))
          (throw 'found (car entry)))))))

;; Inject into header-line-format for prog-mode buffers
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local header-line-format
                        '(:eval (ar/native-breadcrumb)))))
```

> **Note:** The native approach is significantly more complex and less performant than the `breadcrumb` package. Option A is strongly recommended.

## Why This Approach (vs. `lsp-headerline-breadcrumb-mode`)

| Consideration           | `breadcrumb` (chosen)                                           | `lsp-headerline-breadcrumb-mode` (rejected)           |
| ----------------------- | --------------------------------------------------------------- | ----------------------------------------------------- |
| **LSP client coupling** | Works with _any_ imenu backend (eglot, dumb-jump, treesit)      | Hard-bound to `lsp-mode` ecosystem                    |
| **Protocol compliance** | Honors the `eglot`-only stack mandate                           | Requires forbidden `lsp-mode` ecosystem               |
| **Rendering engine**    | Native Emacs `header-line-format`                               | Custom lsp-ui rendering pipeline                      |
| **Emacs 31 synergy**    | Leverages new `header-line-active`/`header-line-inactive` faces | No integration with Emacs 31 header-line enhancements |
| **Maintenance**         | Active (GNU ELPA, maintained by eglot author)                   | Stale — tracks lsp-mode lifecycle                     |
| **Symbol source**       | Uses `imenu` (works without LSP server)                         | Requires LSP `textDocument/documentSymbol`            |

## Behavioral Parity Matrix

| VS Code behavior                     | Emacs 31 equivalent                                         |
| ------------------------------------ | ----------------------------------------------------------- |
| Top bar shows `path › file › symbol` | `breadcrumb-mode` renders in `header-line-format`           |
| Click segment to jump                | `mouse-1` on breadcrumb segment triggers `imenu` jump       |
| Keyboard navigation                  | `SPC c b` → `breadcrumb-jump` opens `imenu` selection       |
| Auto-updates on cursor move          | `breadcrumb--refresh` hooks into `post-command-hook`        |
| Project-relative paths               | `project.el` integration via `project-current`              |
| AST-aware symbol hierarchy           | `imenu` + `treesit` provide class/function/method structure |
| Focused window highlighting          | Emacs 31 `header-line-active` face for selected window      |
| Unfocused window dimming             | Emacs 31 `header-line-inactive` face for other windows      |

## Emacs 31 Specific Enhancements

- **New header-line faces**: Emacs 31 introduces `header-line-active` and `header-line-inactive` faces, providing visual distinction between focused and unfocused window breadcrumbs .
- **Improved imenu with treesit**: Emacs 31's `treesit-aggregated-simple-imenu-settings` powers multi-language imenu trees for modes like `mhtml-ts-mode` and `php-ts-mode`, enriching breadcrumb symbol hierarchies.
- **Native project.el integration**: `breadcrumb` leverages Emacs 31's enhanced `project.el` for accurate project-relative path resolution without external dependencies.

## Integration with Existing Stack

The `breadcrumb` package integrates seamlessly with the eglot + treesit stack:

- **eglot**: Provides `textDocument/documentSymbol` which populates `imenu` — breadcrumb reads from imenu, not directly from eglot
- **treesit**: Powers AST-aware `imenu` generation for `*-ts-mode` buffers, giving breadcrumb accurate symbol hierarchies
- **project.el**: Supplies project-relative file paths for the path segment of breadcrumbs
- **vertico**: `breadcrumb-jump` uses `completing-read`, which vertico intercepts for fuzzy filtering

## Keybinding Registration

```emacs-lisp
;; Add to general.el global leader bindings
(ar/global-leader
  "t b" '(breadcrumb-mode :wk "Toggle breadcrumbs")
  "c b" '(breadcrumb-jump :wk "Jump to breadcrumb"))
```

---

---

---

# Problems Panel — Emacs 31 Implementation

> **VS Code feature:** Centralized, filterable list of all errors/warnings across the project.

## Feature Overview

| Attribute              | Value                                                                                                           |
| ---------------------- | --------------------------------------------------------------------------------------------------------------- |
| **Feature**            | Problems panel                                                                                                  |
| **VS Code equivalent** | Bottom panel listing errors/warnings/infos across the workspace, filterable by type and file                    |
| **Status**             | 🟢 `emacs 31` · `flymake` native + `consult-flymake`                                                            |
| **Category**           | Editor Chrome / Diagnostics                                                                                     |
| **LSP methods**        | `textDocument/publishDiagnostics` (push) · `textDocument/diagnostic` + `workspace/diagnostic` (pull, LSP 3.17+) |
| **Emacs routing**      | `eglot` → `flymake` → `*Flymake diagnostics*` (project-wide tabulated buffer)                                   |

## Implementation Stack

| Layer                 | Component                                             | Role                                                                                              |
| --------------------- | ----------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| **LSP Client**        | `eglot` (built-in, Emacs 31)                          | Receives diagnostic payloads via `textDocument/publishDiagnostics` and forwards them to `flymake` |
| **Diagnostic Engine** | `flymake` (built-in)                                  | Aggregates diagnostics per-buffer and project-wide; emits tabulated lists                         |
| **Project-Wide View** | `flymake-show-project-diagnostics` (Emacs 31 NEW)     | Lists every workspace diagnostic in a single filterable `*Flymake diagnostics*` buffer            |
| **Buffer-Local View** | `flymake-show-buffer-diagnostics` (Emacs 31 ENHANCED) | Lists diagnostics for the current buffer with highlight-nearby and fringe/margin click navigation |
| **Fuzzy Filtering**   | `consult-flymake`                                     | Vertico-powered fuzzy search across all project diagnostics with live preview                     |
| **Inline Rendering**  | `flymake-show-diagnostics-at-end-of-line 'fancy`      | Draws Unicode arrow graphics below the affected line — complements the panel view                 |

## Commands & Keybindings

| Action                      | Command                            | Keybinding       | Notes                                                                    |
| --------------------------- | ---------------------------------- | ---------------- | ------------------------------------------------------------------------ |
| Project-wide problems panel | `flymake-show-project-diagnostics` | `SPC c E`        | **Emacs 31 NEW** — single tabulated buffer for all workspace errors      |
| Buffer-local diagnostics    | `flymake-show-buffer-diagnostics`  | `SPC c B`        | Highlights the diagnostic nearest to point                               |
| Fuzzy filter diagnostics    | `consult-flymake`                  | `SPC c e`        | Vertico-powered filtering with live preview                              |
| Project-wide fuzzy filter   | `consult-flymake-project`          | `SPC c E` (alt)  | Searches across the entire workspace                                     |
| Next error                  | `flymake-goto-next-error`          | `SPC c n` / `]e` | Cycles forward through diagnostics                                       |
| Previous error              | `flymake-goto-prev-error`          | `SPC c p` / `[e` | Cycles backward through diagnostics                                      |
| Force recheck               | `flymake-start`                    | `SPC c !`        | Triggers on-demand pull-model refresh                                    |
| Filter in panel             | `/` (in tabulated buffer)          | Native           | Native `tabulated-list` filter — type to narrow by message/file/severity |
| Clear filter                | `C-u /` (in tabulated buffer)      | Native           | Resets the filter                                                        |
| Sort by column              | Click column header                | Mouse            | Sort by file, line, severity, or message                                 |

## Configuration

```emacs-lisp
;; ==========================================
;; FLYMAKE CORE (Emacs 31 native diagnostics)
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
;; CONSULT-FLYMAKE (Vertico-powered filtering)
;; ==========================================
(use-package consult-flymake
  :ensure nil  ; Bundled with consult
  :after (consult flymake))

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c e" '(consult-flymake :wk "Search errors (buffer)")
  "c E" '(consult-flymake-project :wk "Search errors (project)")
  "c B" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
  "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics")
  "c n" '(flymake-goto-next-error :wk "Next error")
  "c p" '(flymake-goto-prev-error :wk "Prev error")
  "c !" '(flymake-start :wk "Force recheck"))

;; Unimpaired-style bracket navigation for errors
(general-define-key
  :states 'motion
  "] e" 'flymake-goto-next-error
  "[ e" 'flymake-goto-prev-error)
```

## Why This Approach (vs. `flycheck` / `lsp-ui`)

| Consideration            | `flymake` (chosen)                                            | `flycheck` (rejected)                               | `lsp-ui` (rejected)                          |
| ------------------------ | ------------------------------------------------------------- | --------------------------------------------------- | -------------------------------------------- |
| **LSP client coupling**  | Works with _any_ flymake backend (eglot, clang, eslint)       | Requires explicit checker definitions per tool      | Hard-bound to `lsp-mode` ecosystem           |
| **Protocol compliance**  | Honors the `eglot`-only stack mandate                         | Works but adds a parallel diagnostic engine         | Requires forbidden `lsp-mode` ecosystem      |
| **Emacs 31 integration** | Native `flymake-show-project-diagnostics` ships with Emacs 31 | Requires third-party `flycheck-list-errors` buffers | No integration with Emacs 31 tabulated lists |
| **Performance**          | Built into Emacs core — zero additional packages              | Heavy — spawns independent checker processes        | Child-frame overhead on every hover          |
| **Filtering**            | Native tabulated-list `/` filter + `consult-flymake`          | Requires `helm-flycheck` or `ivy-flycheck`          | Limited built-in filtering                   |
| **Maintenance**          | Maintained by GNU Emacs core team                             | Maintained by community                             | Stale — tracks lsp-mode lifecycle            |

## Behavioral Parity Matrix

| VS Code behavior                                   | Emacs 31 equivalent                                                                |
| -------------------------------------------------- | ---------------------------------------------------------------------------------- |
| `Ctrl+Shift+M` opens Problems panel                | `SPC c P` → `flymake-show-project-diagnostics`                                     |
| Panel lists errors/warnings/infos across workspace | `*Flymake diagnostics*` tabulated buffer with all severity levels                  |
| Filter by severity (Errors / Warnings / Infos)     | Native `/` filter in tabulated-list + `consult-flymake` for fuzzy                  |
| Filter by file path                                | Type filename in `/` filter or use `consult-flymake` with `&file` annotation match |
| Click error to jump to location                    | `RET` on any row jumps to the diagnostic                                           |
| Column sorting (file, line, severity)              | Click column headers in `*Flymake diagnostics*` buffer                             |
| Error count badge in status bar                    | `doom-modeline-lsp` shows error/warning counts                                     |
| Squiggle underlines in editor                      | `flymake` renders squiggles via `flymake-error` / `flymake-warning` faces          |
| Inline message on hover                            | Emacs 31 `'fancy` end-of-line rendering (no `eldoc-box` needed)                    |
| "Quick Fix" from panel                             | `SPC c a` → `eglot-code-actions` at point                                          |

## Emacs 31 Specific Enhancements

- **`flymake-show-project-diagnostics` (NEW)**: A single command that lists every diagnostic across the entire workspace in one `*Flymake diagnostics*` tabulated buffer — eliminating the need to jump between per-file buffers. This is the direct functional equivalent of VS Code's Problems panel.
- **`flymake-show-buffer-diagnostics` (ENHANCED)**: Now highlights the diagnostic nearest to point in the listing and responds to fringe/margin mouse clicks, making buffer-local navigation instantaneous.
- **`flymake-show-diagnostics-at-end-of-line 'fancy`**: Lays out diagnostics _below_ the affected line using Unicode graphics that point back to the locus — renders inline context without stealing focus from the panel.
- **`flymake-indicator-type 'auto`**: Intelligently prefers fringes on GUI frames and falls back to margins on TTY frames, maintaining visual consistency across environments.
- **Dynamic column widths**: The tabulated list now dynamically adjusts column widths to fit content, preventing truncation of long file paths in monorepos.
- **Pull-model diagnostics**: Full support for LSP 3.17+ `textDocument/diagnostic` and `workspace/diagnostic` — client can request on-demand refreshes via `flymake-start` (`SPC c !`).

## Integration with Existing Stack

The Problems panel integrates seamlessly with the eglot + treesit stack:

- **`eglot`**: Feeds `textDocument/publishDiagnostics` payloads directly into `flymake` — no intermediate translation layer
- **`consult-flymake`**: Leverages `vertico` for fuzzy filtering with `orderless` dispatchers (`!warning` to exclude, `=error` for exact match)
- **`doom-modeline`**: Displays live error/warning counts via `doom-modeline-lsp t`, mirroring the VS Code status bar badge
- **`evil-collection`**: Provides Unimpaired-style `[e` / `]e` bracket navigation for cycling through errors
- **`embark`**: `C-;` on any diagnostic row triggers contextual actions (copy message, jump to file, open code action)

---

---

---

# Sticky Scroll — Emacs 31 Implementation

> **VS Code feature:** Pins the current enclosing function/class/block header at the top of the viewport while scrolling.

## Feature Overview

| Attribute              | Value                                                                        |
| ---------------------- | ---------------------------------------------------------------------------- |
| **Feature**            | Sticky Scroll                                                                |
| **VS Code equivalent** | Pinned nested scope headers at the top of the editor viewport                |
| **Status**             | 🟢 `native` · `treesit` + `header-line-format` (or `sticky-scroll-mode`)     |
| **Category**           | Editor Chrome (Pure editor polish, no protocol request)                      |
| **LSP methods**        | None (Relies entirely on local AST parsing)                                  |
| **Emacs routing**      | `treesit` (AST boundary detection) → `header-line-format` or Window Overlays |

## Implementation Stack

| Layer                  | Component                      | Role                                                                                                                                      |
| ---------------------- | ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------- |
| **AST Engine**         | `treesit` (built-in, Emacs 31) | Provides O(1) C-level traversal to find enclosing scopes (`class`, `function`, `method`) via `treesit-parent-until` and `treesit-node-at` |
| **Rendering Surface**  | `header-line-format` (Native)  | Displays the pinned scope headers at the top of the window without stealing buffer space                                                  |
| **Third-Party Engine** | `sticky-scroll-mode` (MELPA)   | Alternative package that spawns a dedicated 1-line window or overlay to render the exact literal source lines of enclosing blocks         |
| **Legacy Fallback**    | `which-function` / `topsy`     | Older regex/imenu-based approaches (rejected in favor of `treesit` for Emacs 31)                                                          |

## Commands & Keybindings

| Action                       | Command                 | Keybinding | Notes                                                         |
| ---------------------------- | ----------------------- | ---------- | ------------------------------------------------------------- |
| Toggle native sticky scroll  | `ar/sticky-scroll-mode` | `SPC t S`  | Enables/disables the `header-line` AST tracking               |
| Toggle package sticky scroll | `sticky-scroll-mode`    | `SPC t S`  | If using the third-party MELPA package                        |
| Jump to pinned header        | `mouse-1` (click)       | —          | Click the header-line segment to jump to the scope definition |

## Configuration

### Option A: Pure-Native `treesit` + `header-line` (Zero Dependencies, Recommended)

This approach leverages Emacs 31's native `treesit` API to walk up the AST and extract the first line of every enclosing scope. It renders them in the `header-line-format`, requiring absolutely zero third-party packages and guaranteeing zero network latency.

```emacs-lisp
(defvar ar/sticky-scroll-max-lines 3
  "Maximum number of nested scopes to pin in the header-line.")

(defun ar/sticky-scroll--extract-line (node)
  "Extract the raw text of the first line of the AST NODE."
  (let ((start (treesit-node-start node)))
    (save-excursion
      (goto-char start)
      (string-trim
       (buffer-substring-no-properties
        (line-beginning-position)
        (line-end-position))))))

(defun ar/sticky-scroll--header ()
  "Compute the sticky scroll header using treesit AST boundaries."
  (when (and (treesit-available-p) (treesit-parser-list))
    (let ((node (treesit-node-at (point)))
          (scopes '())
          (depth 0))
      ;; Walk up the AST tree to find enclosing structural blocks
      (while (and node (< depth ar/sticky-scroll-max-lines))
        (when (memq (treesit-node-type node)
                    '("function_definition" "class_definition"
                      "method_definition" "function_declaration"
                      "class_declaration" "interface_declaration"
                      "namespace_definition" "module"))
          (push (ar/sticky-scroll--extract-line node) scopes)
          (cl-incf depth))
        (setq node (treesit-parent node)))
      (when scopes
        (mapconcat #'identity scopes " › ")))))

(define-minor-mode ar/sticky-scroll-mode
  "Native Emacs 31 sticky scroll via treesit and header-line."
  :lighter " Sticky"
  (if ar/sticky-scroll-mode
      (setq-local header-line-format '(:eval (ar/sticky-scroll--header)))
    (kill-local-variable 'header-line-format)))

;; Auto-enable in programming modes
(add-hook 'prog-mode-hook #'ar/sticky-scroll-mode)
```

### Option B: `sticky-scroll-mode` (Third-Party Package)

If you prefer the exact VS Code visual paradigm (where the _literal_ source code lines are rendered as overlays or a separate window at the top of the buffer, rather than a single breadcrumb string), use the `sticky-scroll-mode` package.

```emacs-lisp
(use-package sticky-scroll-mode
  :ensure t
  :defer t
  :commands sticky-scroll-mode
  :custom
  ;; Limit the number of pinned lines to prevent header bloat
  (sticky-scroll-max-lines 3)
  :hook (prog-mode . sticky-scroll-mode))
```

## Why This Approach (vs. `topsy` / `which-function`)

| Consideration        | `treesit` + `header-line` (chosen)                 | `topsy` / `which-function` (rejected)                  |
| -------------------- | -------------------------------------------------- | ------------------------------------------------------ |
| **Parsing Engine**   | Native C-level `treesit` AST traversal             | Regex matching or `imenu` index scanning               |
| **Performance**      | O(1) pointer arithmetic, zero stutter              | O(N) regex scans, causes micro-stutters on scroll      |
| **Accuracy**         | Mathematically exact structural boundaries         | Fragile; breaks on multi-line signatures or decorators |
| **Emacs 31 Synergy** | Leverages `treesit-parent` and `treesit-node-type` | Relies on legacy `add-log` or `imenu` frameworks       |
| **Dependencies**     | Zero (Built-in)                                    | Requires external packages                             |

## Behavioral Parity Matrix

| VS Code behavior                          | Emacs 31 equivalent                                                                        |
| ----------------------------------------- | ------------------------------------------------------------------------------------------ |
| Pins `class` and `def` headers at the top | `treesit-parent` walks the AST to find `class_definition` and `function_definition` nodes  |
| Updates dynamically as you scroll         | `:eval` in `header-line-format` triggers on every redisplay cycle                          |
| Shows nested indentation context          | `ar/sticky-scroll--extract-line` preserves the exact whitespace and signature of the scope |
| Click header to jump to definition        | Native Emacs `header-line` mouse tracking can be bound to `goto-char` of the node start    |
| Works without Language Server             | 100% local via `treesit`; works even if `eglot` is disconnected                            |
| Respects folding                          | `treesit` AST nodes remain intact regardless of `treesit-fold` overlay concealment         |

## Emacs 31 Specific Enhancements

- **`treesit-parent-until` & `treesit-node-at`**: Emacs 31's mature C-level tree-sitter API allows walking up the syntax tree in microseconds. This completely replaces the need for `lsp-mode`'s `textDocument/documentSymbol` payload to determine enclosing scopes.
- **Multi-Language Treesit**: Because Emacs 31 standardizes `treesit` across `python-ts-mode`, `rust-ts-mode`, `typescript-ts-mode`, and `c++-ts-mode`, the native sticky scroll implementation works identically across all major programming languages without requiring language-specific regex configurations.
- **`header-line-active` / `header-line-inactive`**: Emacs 31 introduces distinct faces for focused vs. unfocused windows, ensuring the sticky scroll header visually dims when you switch to a side-window (like `treemacs` or `dirvish`), matching VS Code's UI polish.

---

---

---

# Bracket Pair Colorization — Emacs 31 Implementation

> **VS Code feature:** Matches bracket pairs by color.

## Feature Overview

| Attribute              | Value                                                                                             |
| ---------------------- | ------------------------------------------------------------------------------------------------- |
| **Feature**            | Bracket Pair Colorization                                                                         |
| **VS Code equivalent** | Nested bracket pairs colored by depth, plus structural match highlighting                         |
| **Status**             | 🟢 `native synergy` · `rainbow-delimiters` + `treesit`-backed `show-paren-mode`                   |
| **Category**           | Editor Chrome (Pure editor polish, no protocol request)                                           |
| **LSP methods**        | None (Relies entirely on local AST/syntax-table parsing)                                          |
| **Emacs routing**      | `rainbow-delimiters` (depth colorization) + `show-paren-mode` (structural matching via `treesit`) |

## Implementation Stack

| Layer                     | Component                    | Role                                                                                    |
| ------------------------- | ---------------------------- | --------------------------------------------------------------------------------------- |
| **Colorization Engine**   | `rainbow-delimiters`         | Applies depth-based font-lock faces to `()`, `{}`, and `[]` delimiters                  |
| **Structural Matching**   | `show-paren-mode` (Built-in) | Highlights the matching delimiter when the cursor is on or inside a pair                |
| **AST Parser (Emacs 31)** | `treesit` (Built-in)         | Replaces legacy regex matching with C-level AST traversal via `treesit-show-paren-data` |
| **Theme Integration**     | `doom-themes` (Tokyo Night)  | Maps depth levels 1–9 to specific neon hex codes for high-contrast visual tracking      |

## Commands & Keybindings

| Action                    | Command                   | Keybinding                  | Notes                                                                              |
| ------------------------- | ------------------------- | --------------------------- | ---------------------------------------------------------------------------------- |
| Toggle colorization       | `rainbow-delimiters-mode` | `SPC t r` (or auto-enabled) | Confined to `prog-mode` and `LaTeX-mode` to prevent Org-mode structural corruption |
| Toggle match highlighting | `show-paren-mode`         | `SPC t p` (or auto-enabled) | Native Emacs minor mode, globally enabled                                          |
| Jump to matching paren    | `evil-jump-item`          | `%`                         | Evil motion inherited from Vim, natively respects `show-paren` boundaries          |

## Configuration

The implementation leverages the existing `rainbow-delimiters` setup from the configuration, fortified with Emacs 31's native `treesit` and `show-paren` enhancements.

```emacs-lisp
;; ==========================================
;; 1. RAINBOW DELIMITERS (Depth Colorization)
;; ==========================================
(use-package rainbow-delimiters
  :defer t
  :custom-face
  ;; Tokyo Night depth mapping (Matches indent-bars palette)
  (rainbow-delimiters-depth-1-face ((t (:foreground "#7aa2f7"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "#e0af68"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "#9ece6a"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "#1abc9c"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "#bb9af7"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "#9d7cd8"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "#ff9e64"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "#f7768e"))))
  (rainbow-delimiters-depth-9-face ((t (:foreground "#7aa2f7"))))
  :hook ((prog-mode . rainbow-delimiters-mode)
         (LaTeX-mode . rainbow-delimiters-mode))
  :config
  ;; Aborts activation in massive buffers to prevent main-thread blocking.
  (define-advice rainbow-delimiters-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))

;; ==========================================
;; 2. SHOW PAREN MODE (Emacs 31 AST Matching)
;; ==========================================
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0.05)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-style 'parenthesis)
  ;; Emacs 29+: Render off-screen context as a window overlay.
  (show-paren-context-when-offscreen 'overlay)
  :config
  (show-paren-mode 1)
  ;; Emacs 31+: Prevent phantom bracket highlighting inside strings and comments.
  (when (boundp 'show-paren-not-in-comments-or-strings)
    (setq show-paren-not-in-comments-or-strings t)))
```

## Why This Approach (vs. VS Code's Native Engine)

| Consideration      | Emacs 31 Stack (chosen)                                                           | VS Code Native                         |
| ------------------ | --------------------------------------------------------------------------------- | -------------------------------------- |
| **Parsing Engine** | `treesit` (C-level AST) + `rainbow-delimiters` (syntax-table)                     | Native C++ Tree-sitter/TextMate engine |
| **Performance**    | O(1) syntax-table lookup for colors; O(1) AST pointer math for matching           | Highly optimized C++ background thread |
| **Accuracy**       | Mathematically exact structural boundaries via Emacs 31 `treesit-show-paren-data` | Exact structural boundaries            |
| **Customization**  | Full control over depth faces via `custom-set-faces` / `doom-themes`              | Limited to theme-defined color arrays  |
| **Dependencies**   | Zero external network dependencies                                                | Built into editor core                 |

## Behavioral Parity Matrix

| VS Code behavior                                | Emacs 31 equivalent                                                                 |
| ----------------------------------------------- | ----------------------------------------------------------------------------------- |
| Colors nested `()`, `{}`, `[]` by depth         | `rainbow-delimiters-mode` applies `rainbow-delimiters-depth-N-face`                 |
| Highlights matching bracket on cursor hover     | `show-paren-mode` with `show-paren-when-point-in-periphery t`                       |
| Shows context if matching bracket is off-screen | Emacs 31 `show-paren-context-when-offscreen 'overlay` renders a header-line overlay |
| Ignores brackets inside strings/comments        | Emacs 31 `show-paren-not-in-comments-or-strings t` + syntax-table awareness         |
| Matches generic types (e.g., `<T>` in Rust/TS)  | Emacs 31 `treesit-show-paren-data` uses the AST to match angle brackets accurately  |
| `%` jumps to matching pair                      | Evil's `%` (`evil-jump-item`) natively respects `show-paren` boundaries             |

## Emacs 31 Specific Enhancements

- **`treesit-show-paren-data`**: Emacs 31 overrides the default regex-based `show-paren-data-function` with a tree-sitter-backed alternative. This allows `show-paren-mode` to understand structural context, accurately matching JSX tags, Rust generics, and C++ templates without being confused by identical characters inside strings or comments.
- **`show-paren-not-in-comments-or-strings`**: A new Emacs 31 user option that explicitly suppresses paren highlighting when the cursor is inside a comment or string literal, eliminating the "phantom bracket" noise that plagued older Emacs versions.
- **`show-paren-context-when-offscreen 'overlay`**: When the matching delimiter is scrolled out of the viewport, Emacs 31 renders the surrounding context line as a transient overlay at the top of the window, perfectly mirroring VS Code's off-screen bracket hints without stealing echo-area space from `eldoc`.
- **Palette Synergy**: The `rainbow-delimiters` depth faces are explicitly mapped to the Tokyo Night palette, perfectly aligning with the `indent-bars` depth colorization to create a unified, multi-layered structural visualization system.

---

---

---

# Minimap — Emacs 31 Implementation

> **VS Code feature:** Miniature file overview on the right edge of the editor.

## Feature Overview

| Attribute              | Value                                                          |
| ---------------------- | -------------------------------------------------------------- |
| **Feature**            | Minimap                                                        |
| **VS Code equivalent** | Right-edge scaled-down buffer overview with viewport indicator |
| **Status**             | 🟢 `editor-chrome` · `minimap` (GNU ELPA) · no LSP dependency  |
| **Category**           | Editor Chrome (Pure editor polish, no protocol request)        |
| **LSP methods**        | None (Relies entirely on local buffer rendering)               |
| **Emacs routing**      | `minimap` (or `scrollpanel`) → dedicated right side-window     |

## Implementation Stack

| Layer                      | Component              | Role                                                                                                          |
| -------------------------- | ---------------------- | ------------------------------------------------------------------------------------------------------------- |
| **Minimap Engine**         | `minimap` (GNU ELPA)   | Renders a SublimeText-style scaled-down sidebar showing the full buffer with the current viewport highlighted |
| **Modern Alternative**     | `scrollpanel`          | Newer minimap-like side panel with mouse-based scrolling and auto-follow on buffer switch                     |
| **Detachable Alternative** | `demap`                | Detachable minimap buffer that can be moved, hidden, or killed like any regular buffer                        |
| **Window Management**      | `display-buffer-alist` | Routes minimap to a persistent right side-window with `no-delete-other-windows` protection                    |
| **Performance Guard**      | `too-long-file-p`      | Suspends minimap rendering in massive buffers to prevent main-thread blocking                                 |

## Commands & Keybindings

| Action                   | Command               | Keybinding | Notes                                                       |
| ------------------------ | --------------------- | ---------- | ----------------------------------------------------------- |
| Toggle minimap           | `minimap-mode`        | `SPC t M`  | Enables/disables the minimap sidebar for the current buffer |
| Toggle minimap globally  | `minimap-global-mode` | —          | Enables minimap across all programming buffers              |
| Scroll minimap (mouse)   | `mouse-1` drag        | —          | Drag the viewport indicator to scroll the main buffer       |
| Jump to minimap location | `mouse-1` click       | —          | Click anywhere on the minimap to jump the main buffer       |
| Refresh minimap          | `minimap-update`      | —          | Force re-render after external buffer modifications         |

## Configuration

### Option A: `minimap` (GNU ELPA — Doom Emacs default, recommended)

The canonical SublimeText-style minimap, maintained on GNU ELPA and used as the default in Doom Emacs' `:ui minimap` module .

```emacs-lisp
(use-package minimap
  :defer t
  :commands (minimap-mode minimap-global-mode)
  :custom
  ;; Place the minimap on the right edge (VS Code parity).
  (minimap-window-location 'right)
  ;; Width as a fraction of the frame — 15% matches VS Code's default.
  (minimap-width-fraction 0.15)
  ;; Highlight the currently visible region in the main buffer.
  (minimap-highlight-line t)
  ;; Update delay in seconds — 0.2 prevents main-thread stutter during fast scrolling.
  (minimap-update-delay 0.2)
  ;; Automatically kill the minimap buffer when the source buffer is killed.
  (minimap-kill-buffer-on-close t)
  ;; Hide the mode line inside the minimap window for a cleaner look.
  (minimap-hide-mode-line t)
  :custom-face
  ;; Tokyo Night synergy: dim the minimap background to reduce visual competition.
  (minimap-normal-face ((t (:foreground "#565f89" :background "#1a1b26"))))
  (minimap-active-region-background ((t (:background "#292e42"))))
  :config
  ;; Route the minimap buffer to a persistent right side-window that survives
  ;; `delete-other-windows` (SPC w m) and workspace transitions.
  (add-to-list 'display-buffer-alist
               '("\\*MINIMAP\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.15)
                 (window-parameters (no-delete-other-windows . t)
                                    (no-other-window . t))))
  ;; Abort activation in massive buffers to prevent main-thread freezing.
  (define-advice minimap-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p)))))
```

### Option B: `scrollpanel` (Modern alternative — mouse-native)

A newer, more interactive minimap-like panel that supports native mouse scrolling and auto-follows buffer switches .

```emacs-lisp
(use-package scrollpanel
  :defer t
  :commands scrollpanel-mode
  :custom
  ;; 10% side window width — matches the VS Code minimap footprint.
  (scrollpanel-width-fraction 0.10)
  ;; Auto-update when switching buffers in tracked windows.
  (scrollpanel-auto-follow t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*scrollpanel\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 1)
                 (window-width . 0.10)
                 (window-parameters (no-delete-other-windows . t)))))
```

### Option C: Pure-native approach (zero third-party packages)

A minimalist viewport indicator using a dedicated side-window with a scaled-down clone of the current buffer. Lacks interactive mouse scrolling but achieves the core visual overview without external dependencies.

```emacs-lisp
(defun ar/native-minimap-toggle ()
  "Toggle a native minimap-like side-window showing a scaled buffer clone."
  (interactive)
  (let ((buf (get-buffer "*native-minimap*")))
    (if buf
        (progn (kill-buffer buf)
               (message "Minimap closed"))
      (let* ((src (current-buffer))
             (map (get-buffer-create "*native-minimap*")))
        (with-current-buffer map
          (setq buffer-read-only t)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-buffer-substring src))
          (text-scale-set -4)  ;; Shrink text ~5x to mimic minimap scale
          (setq-local show-trailing-whitespace nil)
          (setq-local display-line-numbers nil))
        (display-buffer-in-side-window
         map '((side . right)
               (window-width . 0.12)
               (window-parameters (no-delete-other-windows . t)
                                  (no-other-window . t))))))))
```

## Why This Approach (vs. `sublimity` / legacy minimap hacks)

| Consideration         | `minimap` (chosen)               | `sublimity` (rejected)    | `demap` (alternative)    |
| --------------------- | -------------------------------- | ------------------------- | ------------------------ |
| **Maintenance**       | Active on GNU ELPA               | Experimental/unmaintained | Niche, smaller community |
| **Doom parity**       | Default `:ui minimap` module     | Not in Doom               | Not in Doom              |
| **Window safety**     | Native side-window support       | Custom window management  | Detachable but fragile   |
| **Performance**       | Guarded against large files      | Smooth-scroll overhead    | Moderate                 |
| **Mouse interaction** | Click-to-jump + drag viewport    | Limited                   | Detachable interactions  |
| **Emacs 31 synergy**  | Leverages `display-buffer-alist` | Legacy APIs               | Modern but less tested   |

## Behavioral Parity Matrix

| VS Code behavior            | Emacs 31 equivalent                                                   |
| --------------------------- | --------------------------------------------------------------------- |
| Right-edge minimap panel    | `minimap-window-location 'right` + `display-buffer-alist` side-window |
| Highlights visible viewport | `minimap-highlight-line t` with `minimap-active-region-background`    |
| Click minimap to jump       | Native `mouse-1` click handling in `minimap`                          |
| Drag viewport indicator     | Native mouse-drag in the minimap sidebar                              |
| Auto-updates on edit        | `minimap-update-delay 0.2` debounces redisplay                        |
| Toggle via command palette  | `SPC t M` → `minimap-mode`                                            |
| Hides on small windows      | `minimap-width-fraction` scales with frame size                       |
| Theme-aware colors          | `minimap-normal-face` inherits Tokyo Night palette                    |
| Disabled in peek/terminal   | `too-long-file-p` guard + `display-buffer-alist` exclusions           |

## Emacs 31 Specific Enhancements

- **Improved side-window management**: Emacs 31's refined `display-buffer-in-side-window` API provides more stable slot allocation, preventing the minimap from being displaced by transient popups or workspace transitions.
- **New window commands**: Emacs 31 adds more precise window manipulation for side-by-side layouts, making minimap coexistence with Treemacs/Dirvish sidebars more predictable .
- **Speedbar side-window parity**: Emacs 31's Speedbar now lives in a proper side-window rather than a separate frame , establishing a consistent pattern that minimap inherits via `display-buffer-alist`.
- **No native minimap in core**: Unlike sticky scroll or fancy flymake, Emacs 31 does not ship a built-in minimap engine — the third-party `minimap` package remains the canonical solution.

## Integration with Existing Stack

The minimap integrates cleanly with the existing sidebar ecosystem:

- **`treemacs`**: Both use `display-buffer-alist` side-windows; assign Treemacs to `slot . -1` (left) and minimap to `slot . 1` (right) to prevent collision.
- **`dirvish-side`**: Dirvish's 30% left sidebar coexists with the right-edge minimap without conflict.
- **`popper`**: Popper's reference-buffer regex ignores `*MINIMAP*` by default, preventing the minimap from being treated as a transient popup.
- **`ultra-scroll` / `good-scroll`**: Minimap updates are debounced via `minimap-update-delay` to prevent stuttering during high-speed pixel scrolling.
- **`solaire-mode`**: The minimap buffer inherits `solaire-mode`'s swapped background, maintaining visual distinction from the main editing window.

## Performance Considerations

- **Large file guard**: The `too-long-file-p` advice aborts `minimap-mode` in buffers exceeding 500,000 characters or 10,000 lines, preventing the scaled-down clone from freezing the main thread.
- **Debounced updates**: `minimap-update-delay 0.2` throttles re-renders during rapid scrolling, eliminating the "minimap lag" that plagues unguarded configurations.
- **Selective activation**: Restrict `minimap-global-mode` to `prog-mode` hooks — prose buffers (Org, Markdown) rarely benefit from minimap overview and waste rendering cycles.

---

---

---

# Multi-Cursor Editing — Emacs 31 Implementation

> **VS Code feature:** Place and edit from multiple cursors simultaneously (Ctrl+D, Alt+Click).

## Feature Overview

| Attribute              | Value                                                                     |
| ---------------------- | ------------------------------------------------------------------------- |
| **Feature**            | Multi-cursor editing                                                      |
| **VS Code equivalent** | Ctrl+D (select next), Ctrl+Shift+L (select all), Alt+Click (place cursor) |
| **Status**             | 🟢 `working` · `iedit` + `evil-multiedit` (lexical) / `eglot` (semantic)  |
| **Category**           | Editor Chrome / Inline Mutations                                          |
| **LSP methods**        | `textDocument/rename` (for semantic multi-file edits)                     |
| **Emacs routing**      | `iedit` (overlay engine) → `evil-multiedit` (Evil state wrapper)          |

## Implementation Stack

| Layer                 | Component                                 | Role                                                                                                                           |
| --------------------- | ----------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------ |
| **Core Engine**       | `iedit` (Built-in/ELPA)                   | Highlights and edits multiple matching regions simultaneously using Emacs overlays.                                            |
| **Evil Integration**  | `evil-multiedit`                          | Wraps `iedit` in a dedicated Evil state, providing Vim-like muscle memory and protecting the modal state machine.              |
| **Semantic Engine**   | `eglot` (Built-in)                        | Provides AST-aware, project-wide symbol renaming (`textDocument/rename`), replacing dangerous text-based multi-cursor renames. |
| **Rectangle Editing** | `evil-multiedit` / `iedit-rectangle-mode` | Handles vertical block selections and columnar edits natively.                                                                 |

## Commands & Keybindings

| Action                   | Command                                | Keybinding       | Notes                                                                                                              |
| ------------------------ | -------------------------------------- | ---------------- | ------------------------------------------------------------------------------------------------------------------ |
| Select next occurrence   | `evil-multiedit-match-symbol-and-next` | `M-d`            | Direct parity with VS Code's `Ctrl+D`.                                                                             |
| Select all occurrences   | `evil-multiedit-match-all`             | `M-D`            | Direct parity with VS Code's `Ctrl+Shift+L`.                                                                       |
| Toggle multi-cursor mode | `iedit-mode`                           | `SPC e e`        | Manual invocation on the current region/symbol.                                                                    |
| Rectangle multi-cursor   | `iedit-rectangle-mode`                 | `SPC e r`        | Vertical block editing.                                                                                            |
| Apply and exit           | `iedit-done` / `keyboard-quit`         | `RET` / `C-g`    | Commits the overlay edits to the buffer text.                                                                      |
| Semantic Rename (Safe)   | `eglot-rename`                         | `F2` / `SPC c r` | **Crucial:** Use this instead of multi-cursor for variable/function renaming to guarantee AST safety across files. |

## Configuration

The implementation leverages the existing `iedit` and `evil-multiedit` setup from the configuration, strictly avoiding `multiple-cursors.el` to prevent Evil state desyncs.

```emacs-lisp
;; ==========================================
;; 1. IEDIT (Foundational Overlay Engine)
;; ==========================================
(use-package iedit
  :defer t
  :commands (iedit-mode iedit-rectangle-mode)
  :custom
  ;; Prevent default C-; global keymap injection to avoid Embark conflicts.
  (iedit-toggle-key-default nil)
  ;; Disable automatic defun narrowing to allow buffer-wide multi-cursor.
  (iedit-auto-narrow nil)
  ;; Render one context line around matches for spatial awareness.
  (iedit-occurrence-context-lines 1))

;; ==========================================
;; 2. EVIL-MULTIEDIT (Modal State Wrapper)
;; ==========================================
(use-package evil-multiedit
  :defer t
  :after (evil iedit)
  :commands (evil-multiedit-match-symbol-and-next
             evil-multiedit-match-all
             evil-multiedit-toggle-or-restrict-region)
  :custom-face
  ;; Theme the match overlay to Tokyo Night purple for high contrast.
  (evil-multiedit-match-face ((t (:background "#bb9af7" :foreground "#1a1b26"))))
  :config
  ;; Register internal state navigation (TAB / S-TAB to jump between cursors).
  (evil-multiedit-default-keybinds))

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (VS Code Parity)
;; ==========================================
;; Placed entirely outside use-package to prevent deferred-registration traps.
(general-define-key
  :states '(normal visual)
  "M-d" #'evil-multiedit-match-symbol-and-next
  "M-D" #'evil-multiedit-match-all)
```

## Why This Approach (vs. `multiple-cursors.el`)

| Consideration          | `iedit` + `evil-multiedit` (chosen)                          | `multiple-cursors.el` (rejected)                                                               |
| ---------------------- | ------------------------------------------------------------ | ---------------------------------------------------------------------------------------------- |
| **Evil Compatibility** | Native Evil state integration; respects modal transitions.   | Creates "fake" cursors that fight with Evil's state machine, causing severe keymap collisions. |
| **Rendering Engine**   | Standard Emacs overlays (lightweight, zero redisplay hacks). | Custom fake-cursor rendering loop (heavy, prone to visual artifacts).                          |
| **Undo History**       | Commits as a single, clean undo boundary via `iedit`.        | Often fragments undo history or corrupts it during complex multi-line edits.                   |
| **Rectangle Support**  | Native `iedit-rectangle-mode` for vertical column edits.     | Requires separate `mc/mark-all-like-this-in-region` hacks.                                     |

## Behavioral Parity Matrix

| VS Code behavior                        | Emacs 31 equivalent                                                                 |
| --------------------------------------- | ----------------------------------------------------------------------------------- |
| `Ctrl+D` (Select next occurrence)       | `M-d` (`evil-multiedit-match-symbol-and-next`)                                      |
| `Ctrl+Shift+L` (Select all occurrences) | `M-D` (`evil-multiedit-match-all`)                                                  |
| `Alt+Click` (Place arbitrary cursor)    | Discouraged in Evil; use `M-d` or visual-block (`C-v`) + `I` for spatial edits.     |
| `F2` (Rename Symbol safely)             | `F2` / `SPC c r` (`eglot-rename`) — **Always prefer this for code symbols.**        |
| `Ctrl+Alt+Up/Down` (Column cursors)     | `C-v` (Evil visual-block) → `I` (Insert) → Type → `<escape>`                        |
| Multi-file rename                       | `eglot-rename` (LSP) or `xref-change-to-xref-edit-mode` (Emacs 31 native Grep-Edit) |

## Emacs 31 Specific Enhancements

- **Editable Xref Buffers (Grep-Edit):** Emacs 31 introduces `xref-change-to-xref-edit-mode` (bound to `e` in `*xref*` buffers). This acts as a **multi-file, multi-cursor engine**. When you need to edit 50 references across 10 files, you pull them into an xref buffer, press `e`, edit them simultaneously using standard Emacs text manipulation, and save. The changes propagate back to the originating files natively.
- **Treesit + Iedit Synergy:** While `iedit` is inherently regex/text-based, combining it with `evil-textobj-tree-sitter` allows you to visually select an AST node (e.g., `v f` for function), and then use `iedit` to mutate identical structural blocks without relying on fragile text matching.
- **Semantic vs. Lexical Boundary:** Emacs 31's mature `eglot` integration strictly enforces the boundary between _lexical_ multi-cursor (formatting text, aligning columns via `iedit`) and _semantic_ multi-cursor (renaming variables via `eglot-rename`). This prevents the catastrophic bugs common in VS Code when users accidentally `Ctrl+D` a common word like `id` or `name` across unrelated scopes.

---

---

---

# Quick Fix Lightbulb — Emacs 31 Implementation

> **VS Code feature:** Visual indicator that code actions are available at the cursor.

## Feature Overview

| Attribute              | Value                                                                                    |
| ---------------------- | ---------------------------------------------------------------------------------------- |
| **Feature**            | Quick Fix lightbulb                                                                      |
| **VS Code equivalent** | Lightbulb icon in gutter/margin when `textDocument/codeAction` returns non-empty results |
| **Status**             | 🟢 `emacs 31` · native `eglot` indicator (NEW in Emacs 31)                               |
| **Category**           | Editor Chrome / Code Actions                                                             |
| **LSP methods**        | `textDocument/codeAction` (queried on cursor movement)                                   |
| **Emacs routing**      | `eglot` → margin/nearby overlay indicator → `eglot-code-actions` invocation              |

## Implementation Stack

| Layer                 | Component                                      | Role                                                                                          |
| --------------------- | ---------------------------------------------- | --------------------------------------------------------------------------------------------- |
| **LSP Client**        | `eglot` (built-in, Emacs 31)                   | Queries `textDocument/codeAction` on cursor movement and renders visual indicator             |
| **Indicator Control** | `eglot-code-action-indications` (Emacs 31 NEW) | List of symbols controlling where the indicator appears: `margin`, `nearby`, or both          |
| **Indicator Glyph**   | `eglot-code-action-indicator` (Emacs 31 NEW)   | String/character rendered as the visual indicator (default: lightbulb emoji or Unicode glyph) |
| **Action Invocation** | `eglot-code-actions`                           | Opens `completing-read` menu when indicator is clicked or `SPC c a` is pressed                |
| **Mouse Integration** | `eglot-code-actions-at-mouse`                  | Pops up code action menu on `mouse-2` click over diagnostics                                  |

## Commands & Keybindings

| Action                  | Command                              | Keybinding                | Notes                                             |
| ----------------------- | ------------------------------------ | ------------------------- | ------------------------------------------------- |
| Invoke code actions     | `eglot-code-actions`                 | `SPC c a`                 | Opens `completing-read` menu of available actions |
| Quick fix (diagnostic)  | `eglot-code-action-quickfix`         | —                         | Executes quickfix actions for current diagnostic  |
| Organize imports        | `eglot-code-action-organize-imports` | `SPC c i`                 | Rearranges import declarations                    |
| Extract method/variable | `eglot-code-action-extract`          | —                         | Extracts selected code into function/variable     |
| Inline variable         | `eglot-code-action-inline`           | —                         | Inlines variable at point                         |
| Rewrite code            | `eglot-code-action-rewrite`          | —                         | Applies rewrite actions                           |
| Code actions at mouse   | `eglot-code-actions-at-mouse`        | `mouse-2` (on diagnostic) | Pops up menu at cursor position                   |

## Configuration

```emacs-lisp
;; ==========================================
;; EGLOT CODE ACTION INDICATOR (Emacs 31 NEW)
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Control where the lightbulb indicator appears.
  ;; - 'margin: Renders indicator in the left margin (VS Code parity)
  ;; - 'nearby: Renders indicator near point (inline)
  ;; Both can be enabled simultaneously.
  (eglot-code-action-indications '(margin nearby))

  ;; The actual glyph/string used as the indicator.
  ;; Default is a lightbulb emoji, but can be customized.
  ;; NOTE: The default indicator may cause UI glitches in some modes   .
  ;; Consider using a simpler Unicode character if you experience display issues.
  (eglot-code-action-indicator "💡")

  ;; Alternative: Use a simpler Unicode character to avoid rendering issues
  ;; (eglot-code-action-indicator "⚡")
  ;; (eglot-code-action-indicator "✦")
  )

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c a" '(eglot-code-actions :wk "Code actions (lightbulb)")
  "c i" '(eglot-code-action-organize-imports :wk "Organize imports")
  "c q" '(eglot-code-action-quickfix :wk "Quick fix"))
```

## Why This Approach (vs. `lsp-ui-sideline` / third-party packages)

| Consideration            | `eglot` native indicator (chosen)               | `lsp-ui-sideline` (rejected)            | Third-party lightbulb packages     |
| ------------------------ | ----------------------------------------------- | --------------------------------------- | ---------------------------------- |
| **LSP client coupling**  | Works exclusively with `eglot`                  | Hard-bound to `lsp-mode` ecosystem      | Often require `lsp-mode`           |
| **Protocol compliance**  | Honors the `eglot`-only stack mandate           | Requires forbidden `lsp-mode` ecosystem | Violates architectural constraints |
| **Emacs 31 integration** | Native feature shipped with Emacs 31            | No integration with Emacs 31 eglot      | Not maintained for eglot           |
| **Performance**          | Lightweight overlay, zero additional packages   | Heavy sideline rendering engine         | Variable quality                   |
| **Maintenance**          | Maintained by GNU Emacs core team (João Távora) | Stale — tracks lsp-mode lifecycle       | Fragmented ecosystem               |

## Behavioral Parity Matrix

| VS Code behavior                                     | Emacs 31 equivalent                                                        |
| ---------------------------------------------------- | -------------------------------------------------------------------------- |
| Lightbulb icon in gutter when code actions available | `eglot-code-action-indications '(margin)` renders indicator in left margin |
| Click lightbulb to open actions menu                 | `mouse-2` on diagnostic → `eglot-code-actions-at-mouse`                    |
| `Ctrl+.` opens quick fix menu                        | `SPC c a` → `eglot-code-actions`                                           |
| Indicator appears on current line                    | eglot queries `textDocument/codeAction` on cursor movement                 |
| Different icons for different action types           | Single unified indicator (eglot design philosophy)                         |
| Indicator disappears when no actions                 | eglot removes overlay when query returns empty                             |
| Quick fix for diagnostic errors                      | `eglot-code-action-quickfix` or click diagnostic with `mouse-2`            |

## Emacs 31 Specific Enhancements

- **`eglot-code-action-indications` (NEW)**: Emacs 31 introduces native visual indication of available code actions. The variable accepts a list of symbols: `margin` (renders in left margin), `nearby` (renders near point), or both .
- **`eglot-code-action-indicator` (NEW)**: Customizable string/glyph used as the visual indicator. Defaults to a lightbulb emoji but can be changed to any Unicode character .
- **Known display issues**: The default lightbulb emoji indicator has been reported to cause UI glitches in some major modes (particularly Rust and TypeScript) . Users experiencing display corruption should switch to a simpler Unicode character like `⚡` or `✦`.
- **Mouse integration**: `eglot-code-actions-at-mouse` provides click-to-invoke functionality on diagnostics, matching VS Code's click-to-fix paradigm .
- **Feature request (bug#78106)**: There is an active feature request to enhance mouse click integration for code action indications .

## Known Issues & Workarounds

### Display Glitches with Default Indicator

The default `eglot-code-action-indicator` (lightbulb emoji) has been reported to cause display corruption in certain major modes, particularly when combined with tree-sitter modes or complex font-lock configurations .

**Workaround:** Switch to a simpler Unicode character:

```emacs-lisp
(setq eglot-code-action-indicator "⚡")  ; High voltage sign
;; or
(setq eglot-code-action-indicator "✦")  ; Four-pointed star
;; or
(setq eglot-code-action-indicator "•")  ; Simple bullet
```

### Margin Width Issues

If the margin indicator causes text reflow or alignment issues, disable margin indication and use only `nearby`:

```emacs-lisp
(setq eglot-code-action-indications '(nearby))
```

### Performance Considerations

The indicator queries `textDocument/codeAction` on every cursor movement. For language servers with slow code action resolution, this may cause input latency. To mitigate:

1. **Disable indicator entirely**: `(setq eglot-code-action-indications nil)`
2. **Use manual invocation only**: Rely on `SPC c a` instead of visual indicator
3. **Optimize server**: Configure language server to cache code action results

## Integration with Existing Stack

The Quick Fix lightbulb integrates seamlessly with the eglot + treesit stack:

- **`eglot`**: Queries `textDocument/codeAction` and renders indicator via `eglot-code-action-indications`
- **`vertico`**: `eglot-code-actions` uses `completing-read`, which vertico intercepts for fuzzy filtering
- **`consult`**: Code action candidates can be filtered and previewed via consult integration
- **`flymake`**: Diagnostics with available quick fixes show the lightbulb indicator, providing visual feedback that `SPC c a` will offer fixes
- **`embark`**: `C-;` on a diagnostic triggers `embark-act`, which includes code action options

## Comparison with VS Code

| Aspect                 | VS Code                                           | Emacs 31 + eglot                          |
| ---------------------- | ------------------------------------------------- | ----------------------------------------- |
| **Indicator location** | Gutter (left margin)                              | Margin and/or nearby point (configurable) |
| **Indicator glyph**    | Lightbulb icon                                    | Customizable string (default: 💡)         |
| **Trigger**            | Cursor movement + debounced query                 | Cursor movement + immediate query         |
| **Action menu**        | Dropdown at indicator                             | `completing-read` in minibuffer (vertico) |
| **Keyboard shortcut**  | `Ctrl+.`                                          | `SPC c a`                                 |
| **Mouse interaction**  | Click indicator                                   | `mouse-2` on diagnostic                   |
| **Filtering**          | Categorized by type (Quick Fix, Refactor, Source) | Flat list in `completing-read`            |
| **Performance**        | Optimized C++ engine                              | Elisp overlay, server-dependent latency   |

## Advanced Configuration

### Custom Indicator Face

Style the indicator with a custom face for better visibility:

```emacs-lisp
(defface eglot-code-action-indicator-face
  '((t (:foreground "#e0af68" :weight bold)))
  "Face for eglot code action indicator.")

;; Apply face to indicator (requires eglot source modification or advice)
```

### Conditional Indication

Show indicator only for specific action kinds (e.g., only quick fixes, not refactors):

```emacs-lisp
;; This requires custom advice on eglot's code action query
;; Not natively supported in Emacs 31, but can be implemented via :around advice
```

### Disable for Specific Modes

Disable the indicator in modes where it causes issues:

```emacs-lisp
(add-hook 'rust-ts-mode-hook
          (lambda ()
            (setq-local eglot-code-action-indications nil)))
```

## Troubleshooting

### Indicator Not Appearing

1. **Verify server supports code actions**: Check `eglot-events-buffer` for `codeActionProvider` capability
2. **Check `eglot-code-action-indications`**: Ensure it's not `nil`
3. **Verify cursor position**: Indicator only appears when `textDocument/codeAction` returns non-empty results
4. **Check margin visibility**: If using `margin`, ensure left margin is enabled (`setq left-margin-width 2`)

### Display Corruption

1. **Switch indicator glyph**: Use simpler Unicode character (see Workarounds above)
2. **Disable margin indication**: `(setq eglot-code-action-indications '(nearby))`
3. **Update Emacs**: Ensure you're running latest Emacs 31 pretest with eglot patches

### Performance Issues

1. **Profile server latency**: Check `eglot-events-buffer` for `textDocument/codeAction` response times
2. **Disable indicator**: `(setq eglot-code-action-indications nil)` and use manual `SPC c a`
3. **Optimize server**: Configure language server to cache or debounce code action queries

## References

- [Bug#31.0.50: Lightbulb emoji eglot-code-action-indicator breaks display](https://lists.gnu.org/archive/html/bug-gnu-emacs/2026-02/msg00221.html)
- [eglot discussion #1492: codeActionProvider causing line duplication](https://github.com/joaotavora/eglot/discussions/1492)
- [eglot discussion: default eglot-code-action-indicator UI glitches](https://github.com/joaotavora/eglot/discussions/1492)
- [eglot README: mouse-2 invokes eglot-code-actions-at-mouse](https://github.com/joaotavora/eglot/blob/master/README.md)
- [Bug#78106: Feature request for mouse click code action indications](https://mail.gnu.org/archive/html/bug-gnu-emacs/2025-05/msg00197.html)
- [eglot documentation: eglot-code-actions-at-mouse](https://docs.jade.fyi/gnu/emacs/eglot.html)
- [eglot manual: eglot-code-action-indications](https://joaotavora.github.io/eglot/)
- [Bug#31.0.50: eglot-code-action-indicator containing U+F400](https://mail.gnu.org/archive/html/bug-gnu-emacs/2025-03/msg02492.html)
- [eglot discussion: default indicator causes UI glitches for Rust](https://github.com/joaotavora/eglot/discussions/1492)
- [eglot source: margin and nearby indication modes](https://github.com/joaotavora/eglot/blob/master/eglot.el)
