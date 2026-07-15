# lsp-mode & lsp-ui — Full Reference (as of July 2026)

Source: emacs-lsp.github.io/lsp-mode (last updated July 12, 2026), emacs-lsp.github.io/lsp-ui (last updated May 12, 2026), and the emacs-lsp/lsp-mode GitHub changelog. lsp-mode is currently at version **9.0.1**, requiring Emacs 28.1+.

---

## 1. lsp-mode — Installation & Core Setup

```elisp
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")   ;; or "s-l" (default), or "C-l"
  :hook ((XXX-mode . lsp)            ;; replace XXX-mode with your major mode
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
```

- Use `lsp` to start the server immediately in the current buffer, or `lsp-deferred` to defer startup (and `textDocument/didOpen`) until the buffer becomes visible — useful for large projects/many buffers opened at once.
- `lsp-mode` auto-activates `lsp-ui` as long as `lsp-auto-configure` is non-nil (the default). Setting `lsp-auto-configure` to `nil` disables all auto-configuration, including `lsp-ui`, flycheck/flymake wiring, company/capf wiring, etc.
- Doom Emacs / Spacemacs both ship their own `lsp-mode` module layers; if using a framework, prefer its module flags over raw `use-package` blocks.

### Minimal vanilla template (from the official C/C++ IDE guide)
```elisp
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'lsp-mode) (package-refresh-contents) (package-install 'lsp-mode))
(setq lsp-keymap-prefix "C-c l")
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
```

---

## 2. lsp-mode — Interactive Commands (by category)

### Server / session management
| Command | Purpose |
|---|---|
| `lsp` | Start LSP session in current buffer (with client auto-detection). |
| `lsp-deferred` | Like `lsp` but defers connection until buffer is shown. |
| `lsp-workspace-restart` | Restart the language server. |
| `lsp-workspace-shutdown` | Shut the language server down. |
| `lsp-describe-session` | Describe the current session/workspace. |
| `lsp-disconnect` | Disconnect current buffer from the server, server keeps running. |
| `lsp-workspace-folders-add` | Add a new project root to workspace folders. |
| `lsp-workspace-folders-remove` | Remove a project root from workspace folders. |
| `lsp-workspace-blocklist-remove` | Remove a root from the workspace blocklist. |
| `lsp-doctor` | Validate that your `lsp-mode` config is tuned for good performance. |
| `lsp-install-server` | Install/reinstall a supported language server automatically. |

### Navigation (xref-based)
| Command | Purpose |
|---|---|
| `lsp-find-definition` | Jump to definition of symbol at point. |
| `lsp-find-references` | List references of symbol at point. |
| `lsp-find-implementation` | Jump to implementation(s). |
| `lsp-find-type-definition` | Jump to type definition. |
| `lsp-find-declaration` | Jump to declaration. |
| `lsp-find-definition-mouse` | Definition lookup via mouse click (bound to `C-<down-mouse-1>`). |
| `lsp-extend-selection` | Semantic selection expansion (`textDocument/extendSelection`). |
| `lsp-treemacs-call-hierarchy` | Incoming call hierarchy (requires `lsp-treemacs`). |
| `xref-find-apropos` | Workspace symbol search (find all matching symbols). |

### Documentation / help
| Command | Purpose |
|---|---|
| `lsp-describe-thing-at-point` | Full hover documentation for symbol at point. |
| `lsp-signature-activate` | Manually trigger signature help. |
| `lsp-ui-doc-glance` (lsp-ui) | Show/hide hover doc on demand, hides on next keypress. |

### Refactoring / code actions
| Command | Purpose |
|---|---|
| `lsp-rename` | Rename symbol and all references. |
| `lsp-organize-imports` | Run `source.organizeImports` code action. |
| `lsp-execute-code-action` | Prompt and execute an available code action. |
| `lsp-avy-lens` | Trigger a code lens by point-and-click via `avy`. |
| `lsp-fix-all` | Run `source.fixAll` code action (added in a recent release). |
| `lsp-format-buffer` | Ask the server to format the whole document. |
| `lsp-format-region` | Format the region (or current line if none selected). |

### Diagnostics / symbols
| Command | Purpose |
|---|---|
| `lsp-treemacs-errors-list` | Project-wide error list (requires `lsp-treemacs`). |
| `lsp-symbol-highlight` | Manually highlight symbol at point. |
| `lsp-modeline-code-actions-mode` | Minor mode: show code-action count/icon on modeline. |
| `lsp-modeline-diagnostics-mode` | Minor mode: show diagnostic stats on modeline. |
| `lsp-headerline-breadcrumb-mode` | Minor mode: breadcrumb of path/file/symbols on headerline. |
| `lsp-toggle-symbol-highlight` | Toggle symbol highlighting. |
| `lsp-toggle-on-type-formatting` | Toggle on-type formatting. |
| `lsp-toggle-signature-auto-activate` | Toggle automatic signature help. |
| `lsp-lens-mode` / toggle code-lens overlays | Show/hide code lens overlays. |
| `lsp-toggle-trace-io` | Toggle client-server protocol logging. |

---

## 3. lsp-mode — Default Keybindings

All bound under `lsp-command-map`, prefixed by `lsp-keymap-prefix` (default `s-l`):

| Key | Action |
|---|---|
| `s-l w s` | Start server |
| `s-l w r` | Restart server |
| `s-l w q` | Shutdown server |
| `s-l w d` | Describe session |
| `s-l w D` | Disconnect buffer (keep server alive) |
| `s-l = =` | Format document |
| `s-l = r` | Format region/line |
| `s-l F a` | Add workspace folder |
| `s-l F r` | Remove workspace folder |
| `s-l F b` | Remove from workspace blocklist |
| `s-l T l` | Toggle code-lens overlays |
| `s-l T L` | Toggle protocol logging |
| `s-l T h` | Toggle symbol highlighting |
| `s-l T S` | Toggle sideline info (needs `lsp-ui`) |
| `s-l T d` | Toggle hover child-frame doc (needs `lsp-ui`) |
| `s-l T s` | Toggle signature auto-activate |
| `s-l T f` | Toggle on-type formatting |
| `s-l T T` | Toggle treemacs sync (needs `lsp-treemacs`) |
| `s-l g g` | Find definitions |
| `s-l g r` | Find references |
| `s-l g i` | Find implementations |
| `s-l g t` | Find type definitions |
| `s-l g d` | Find declarations |
| `s-l g h` | Incoming call hierarchy (needs `lsp-treemacs`) |
| `s-l g a` | Find all matching symbols |
| `s-l h h` | Show type signature + doc at point |
| `s-l h s` | Activate signature help |
| `s-l h g` | Hover popup, hides on next input |
| `s-l r r` | Rename symbol |
| `s-l r o` | Organize imports |
| `s-l a a` | Execute code action |
| `s-l a l` | Click lens via `avy` |
| `s-l a h` | Highlight symbol at point |
| `s-l G g` | Peek definitions (needs `lsp-ui`) |
| `s-l G r` | Peek references (needs `lsp-ui`) |
| `s-l G i` | Peek implementations (needs `lsp-ui`) |
| `s-l G s` | Peek workspace symbols (needs `lsp-ui`) |
| `C-u RET` (during completion) | Toggle insert vs. replace behavior |

**which-key integration:**
```elisp
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
```
Pass `t` to `lsp-enable-which-key-integration` to enable it globally across major modes (useful for `web-mode`/`vue-mode`, etc.).

---

## 4. lsp-mode — Core Features

- **Completion at point** — via `completion-at-point`; server-driven candidates. Recommended `company-mode` settings:
  ```elisp
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  ```
- **Code navigation** — `xref`-based, or via `lsp-treemacs`/`lsp-ui-peek`.
- **Code lens** — inline actionable annotations when the server supports `textDocument/codeLens`.
- **Modeline diagnostics** — `lsp-modeline-diagnostics-mode`; configurable scope via `lsp-modeline-diagnostics-scope` (`:global`/`:workspace`/`:file`).
- **Modeline code actions** — `lsp-modeline-code-actions-mode`; segments customizable via `lsp-modeline-code-actions-segments` (`'(count icon)` default, or `'(name icon)`, `'(icon)`, `'(count icon name)`).
- **Headerline breadcrumb** — `lsp-headerline-breadcrumb-mode`; segments via `lsp-headerline-breadcrumb-segments` (default `'(path-up-to-project file symbols)`); label symbol entries with `lsp-headerline-breadcrumb-enable-symbol-numbers`.
- **Symbol highlighting** — highlights all occurrences of symbol at point (read/write faces configurable: `lsp-face-highlight-read`, `lsp-face-highlight-write`, `lsp-face-highlight-textual`; skip current symbol via a core setting).
- **Formatting** — indent-size/tabs-vs-spaces are the only lsp-mode-level settings; actual formatting is server-specific (e.g. JDT LS uses `lsp-java-format-settings-url`, clangd uses `clangd-format`). On-type formatting controlled by `lsp-enable-on-type-formatting` (enabled by default). Format-on-save via `lsp-format-buffer-on-save` + `lsp-format-buffer-on-save-list`. Fix-all-on-save via `lsp-fix-all-on-save` + `lsp-fix-all-on-save-list`.
- **Semantic tokens (LSP 3.17)** — supported with compatible servers (recent clangd, rust-analyzer); toggle via `lsp-semantic-tokens-enable` (renamed from the deprecated `lsp-enable-semantic-highlighting`).
- **Debugger integration** — via sister package `dap-mode` (Debug Adapter Protocol).
- **File watchers** — `lsp-enable-file-watchers`; excludable via `lsp-file-watch-ignored-directories` (renamed from `lsp-file-watch-ignored`).
- **Document links** — `lsp-enable-links` makes clickable references to other docs/URLs.
- **Resource operations** — servers can create/rename/delete files via workspace edits; supports `workspace/willRenameFiles` / `workspace/didRenameFiles` (e.g. syncing with `vc-git-rename-file`).
- **Org-mode support** — lsp-mode ships an `lsp-org` integration for source blocks.
- **Imenu integration** — automatic when the server provides `textDocument/documentSymbol` (`lsp-enable-imenu`).
- **Folding** — `lsp-enable-folding`; `lsp-folding-range-limit`, `lsp-folding-line-folding-only`.

### Notable recent changelog items (2026)
- `lsp-fix-all` command + `lsp-fix-all-on-save` option for `source.fixAll` code actions.
- Diagnostics are now cleared after workspace edits to avoid stale diagnostics at wrong lines (e.g. after `lsp-organize-imports`).
- Support for `workspace/willRenameFiles`/`workspace/didRenameFiles` tied into `vc-git-rename-file`.
- Fixed invalid regex crashes in `lsp-file-watch-ignored-directories`.
- `lsp-clojure-extract-function` updated for clojure-lsp 2026.01+'s range-based extraction protocol.
- `lsp-zig` URL fixed to point at the new zigtools.org domain.
- Various fixes to `lsp--npm-dependency-path` executable resolution.

---

## 5. lsp-mode — Key Configuration Variables

### Core / mode (`lsp-mode` group)
- `lsp-keymap-prefix` — prefix key for all lsp bindings (default `s-l`).
- `lsp-auto-configure` — master switch for all auto-wiring (company, flycheck, lsp-ui, etc.).
- `lsp-auto-guess-root` — auto-detect project root (use carefully with `lsp-session-file`).
- `lsp-idle-delay` — how often (seconds) lsp-mode refreshes highlights/lenses/links while typing. Default tuning suggestion: `0.5`.
- `lsp-document-sync-method` — `lsp--sync-full`, `lsp--sync-incremental`, or `nil` (server-recommended).
- `lsp-response-timeout` — can be disabled entirely for slow servers.
- `lsp-enable-xref` — toggle xref integration.
- `lsp-enable-symbol-highlighting` — toggle read/write occurrence highlighting.
- `lsp-enable-indentation` — use server-driven indentation.
- `lsp-enable-links`, `lsp-enable-folding`, `lsp-enable-file-watchers`, `lsp-enable-on-type-formatting`, `lsp-enable-imenu`.
- `lsp-disabled-clients` — blocklist particular servers per major-mode; can be set file/dir-locally.
- `lsp-log-io` — protocol logging; **must** be `nil` for performance (huge perf hit if left on).
- `lsp-use-plists` — switch internal deserialization from hash-tables to plists for speed (requires `LSP_USE_PLISTS=true` env var + package reinstall).

### Performance tuning
- `gc-cons-threshold` — raise to ~100MB (`(setq gc-cons-threshold 100000000)`), the standard fix used by Spacemacs/Doom/Prelude.
- `read-process-output-max` — raise from the 4K Emacs default (`(setq read-process-output-max (* 1024 1024))` for 1MB), since LSP server responses can be 800K–3M.
- Native JSON (`json-serialize`) — requires Emacs 27+ built `--with-json` against libjansson; ~15x faster than the Elisp JSON parser.
- Native compilation (Emacs 28.1+) recommended for optimal responsiveness.
- `lsp-file-watch-ignored-directories` — exclude heavy directories (build artifacts, `node_modules`, `.git`, etc.) from file watching.
- Consider disabling `lsp-ui` on Windows if overlays/popups cause slowdowns.
- Run `M-x lsp-doctor` to get a live diagnostic of your configuration against these recommendations.

### Diagnostics
- `lsp-diagnostics-provider` — `:auto` / `:flycheck` / `:flymake` / `:none`.
- `lsp-modeline-diagnostics-enable`, `lsp-modeline-diagnostics-scope`.

### Completion
- `lsp-completion-provider` — backend selection (`:capf` for `completion-at-point`, or company).
- `lsp-completion-show-detail`, `lsp-completion-show-kind` — display toggles for completion candidate metadata.
- `lsp-completion-default-behaviour` — insert vs. replace default (toggle with `C-u RET`).

### Lens / Icons / Headerline / Modeline / Semantic tokens
Each has a dedicated settings page (`lsp-lens-*`, `lsp-headerline-*`, `lsp-modeline-*`, `lsp-semantic-tokens-*`), generally following the `lsp-<feature>-enable` naming convention for the master toggle, plus feature-specific display/format variables as shown above.

### Eldoc / signature help
- `lsp-eldoc-enable-hover` — show hover info via eldoc.
- `lsp-eldoc-render-all` — display the entire hover payload instead of the one-line signature.
- `lsp-signature-auto-activate`, `lsp-signature-render-documentation`, `lsp-signature-doc-lines`.
- Browse overloaded signatures with `M-n`/`M-p` after triggering (`C-M-SPC` or trigger char like `(`).

---

## 6. lsp-ui — Overview

`lsp-ui` bundles the higher-level UI modules for `lsp-mode` (sideline info, peek/xref UI, hover doc frames, imenu sidebar). It auto-activates whenever `lsp-mode` starts, provided `lsp-auto-configure` is non-nil.

```elisp
(use-package lsp-ui
  :commands lsp-ui-mode)
```

It has 4 sub-modules: **sideline**, **peek**, **doc**, **imenu**.

### 6.1 lsp-ui-sideline
Shows info for the symbol on the current line, plus flycheck diagnostics and available code actions, inline at end-of-line.

Key variables:
- `lsp-ui-sideline-enable` — master toggle.
- `lsp-ui-sideline-show-diagnostics` — show diagnostic messages.
- `lsp-ui-sideline-show-hover` — show hover info.
- `lsp-ui-sideline-show-code-actions` — show available code actions.
- `lsp-ui-sideline-update-mode` — `'line` (update on line change) vs. point-based updates.
- `lsp-ui-sideline-delay` — seconds before showing.
- `lsp-ui-sideline-diagnostic-max-lines` — lines of diagnostic text shown (increase for verbosity, decrease to reduce flicker).

### 6.2 lsp-ui-peek
VS Code–style "peek" UI for cross references, shown inline without leaving the buffer.

Commands:
- `lsp-ui-peek-find-definitions`
- `lsp-ui-peek-find-references`
- `lsp-ui-peek-find-implementation`
- `lsp-ui-peek-find-workspace-symbol` (e.g. `(lsp-ui-peek-find-workspace-symbol "pattern")`)
- `lsp-ui-peek-find-custom` — custom cross-reference lookups if the server supports them (e.g. `(lsp-ui-peek-find-custom 'base "$cquery/base")`)
- `lsp-ui-peek-jump-forward` / `lsp-ui-peek-jump-backward` — navigate a window-local jump list dedicated to peeked cross-references.

Remap over the default xref bindings:
```elisp
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
```

Key variables:
- `lsp-ui-peek-enable` — master toggle.
- `lsp-ui-peek-show-directory` — show file directory paths in the peek list.

### 6.3 lsp-ui-doc
Shows documentation for the symbol at point in a child frame (or an embedded WebKit widget on builds with `xwidget` support).

Key variables:
- `lsp-ui-doc-enable` — master toggle.
- `lsp-ui-doc-position` — `top`, `bottom`, or `at-point`.
- `lsp-ui-doc-side` — `left` or `right`.
- `lsp-ui-doc-delay` — seconds before showing.
- `lsp-ui-doc-show-with-cursor` — show when point moves over a symbol.
- `lsp-ui-doc-show-with-mouse` — show when the mouse hovers a symbol.
- `lsp-ui-doc-max-width`/`lsp-ui-doc-max-height` (frame sizing).
- `lsp-ui-doc-use-webkit` — render via embedded WebKit (`xwidget`) instead of a plain child frame.
- Focus the doc frame directly for scrolling/selection (there's a dedicated focus-frame command bound in `lsp-ui-doc-frame-mode`).

### 6.4 lsp-ui-imenu
A dedicated sidebar window listing document symbols (functions, classes, variables, etc.), grouped/kinded.

Key variables:
- `lsp-ui-imenu-kind-position` — where to show the symbol-kind label.
- `lsp-ui-imenu-buffer-position` — where the imenu window opens.
- `lsp-ui-imenu-window-width` — fixed width.
- `lsp-ui-imenu-window-fix-width` — prevent `balance-windows` from resizing it.
- `lsp-ui-imenu--custom-mode-line-format` — custom modeline format for the imenu buffer.
- `lsp-ui-imenu-auto-refresh` — auto-refresh on changes.
- `lsp-ui-imenu-refresh-delay` — refresh debounce delay.

---

## 7. Quick "How to Disable Specific Features" Cheat Sheet

(from the official "disabling/enabling lsp-mode features" tutorial)

| Feature | Setting |
|---|---|
| Hover-on-mouse doc frame | `(setq lsp-ui-doc-show-with-mouse nil)` |
| Code lenses | `(setq lsp-lens-enable nil)` |
| Headerline breadcrumb | `(setq lsp-headerline-breadcrumb-enable nil)` |
| Sideline entirely | `(setq lsp-ui-sideline-enable nil)` |
| Sideline code actions only | `(setq lsp-ui-sideline-show-code-actions nil)` |
| Sideline hover only | `(setq lsp-ui-sideline-show-hover nil)` |
| Modeline code actions | `(setq lsp-modeline-code-actions-enable nil)` |
| Diagnostics backend (flycheck/flymake) | `(setq lsp-diagnostics-provider :none)` |
| Sideline diagnostics only | `(setq lsp-ui-sideline-show-diagnostics nil)` |
| Eldoc hover | `(setq lsp-eldoc-enable-hover nil)` |
| Signature help auto-popup | `(setq lsp-signature-auto-activate nil)` (call `lsp-signature-activate` manually) |
| Signature doc text (keep signature) | `(setq lsp-signature-render-documentation nil)` |

---

## 8. Ecosystem / Related Packages
- `lsp-treemacs` — tree views for symbols, errors, call hierarchy.
- `lsp-ivy` / `helm-lsp` / `consult-lsp` — completion-framework-specific workspace symbol search.
- `dap-mode` — Debug Adapter Protocol integration.
- Per-language client packages (`lsp-java`, `lsp-haskell`, `lsp-pyright`, `lsp-metals`, `lsp-dart`, etc.) — over 100 languages supported, listed on the Languages page; many installable directly with `M-x lsp-install-server`.

---

*Compiled from official emacs-lsp documentation and GitHub sources, verified current as of July 15, 2026.*
