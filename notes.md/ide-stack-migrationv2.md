# Comprehensive Evaluation: Migration from Eglot to lsp-mode/lsp-ui Ecosystem

---

## 1. File Versions

| File              | Version / Identifier                                                                                                                                                                                 |
| ----------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `early-init.el`   | Unversioned; header states `no-byte-compile: t; lexical-binding: t`                                                                                                                                  |
| `config.org`      | `#+VERSION: 0.34` — authored by Ahsanur Rahman                                                                                                                                                       |
| `ide-features.md` | Verified against **lsp-mode 9.x**, **lsp-ui 8.x**, **flycheck 34+**, **apheleia (2026 MELPA snapshot)**, **dape 0.2x (GNU ELPA)**, and **dap-mode 0.7+** as of **July 2026**, running on Emacs 30/31 |

All three files have been fully ingested and cross-referenced.

---

## 2. Completion Framework — Corfu Stays Unchanged

**Verdict: No changes required.**

`lsp-mode` natively supports Corfu. When `lsp-completion-provider` is set to `:capf`, lsp-mode registers `lsp-completion-at-point` as a standard `completion-at-point-functions` (CAPF) backend . Corfu reads from CAPF directly — no merging layer like `cape` is strictly required because lsp-mode itself already blends LSP candidates with other CAPF sources . Your existing Corfu, Orderless, Cape, and Vertico stack will work unchanged. The only caveat is that lsp-mode injects its own `completion-category-overrides` entry for the `lsp-capf` category, which can interfere with Orderless dispatch; you may need to add `(lsp-capf (styles orderless basic))` to your `completion-category-overrides` to preserve fuzzy matching behaviour .

---

## 3. dap-mode vs dape with lsp-mode/lsp-ui

**Verdict: Retain dape. dap-mode offers no decisive advantage for your configuration.**

| Criterion                    | dape                                                                                                                     | dap-mode                                                                              |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------- |
| **lsp-mode coupling**        | Editor-agnostic; works with eglot, lsp-mode, or neither                                                                  | Tightly coupled to lsp-mode; uses `${workspaceFolder}` and lsp-mode's project context |
| **Project root detection**   | Your config already wires `dape-cwd-function` to `project-current`/`project-root`                                        | Inherits lsp-mode's workspace root automatically                                      |
| **UI**                       | Minimal: inline overlays, REPL, `dape-info` buffers; your Transient dashboard is already built                           | Fuller VS Code-style `dap-ui-mode` with dedicated locals/breakpoints/sessions panes   |
| **Template library**         | Good defaults for Python, Go, Rust, C/C++, JS/TS, Bash                                                                   | Larger library (~20 languages: Java, Elixir, Ruby, PHP, Dart/Flutter)                 |
| **Maintenance (mid-2026)**   | Actively maintained, GNU ELPA, lightweight                                                                               | Actively maintained, part of emacs-lsp org                                            |
| **Your existing investment** | Extensive: Transient dashboard, display-buffer-alist routing, breakpoint persistence, inlay hints guard, modeline hiding | None configured                                                                       |

The **only** advantage dap-mode holds over dape in an lsp-mode context is automatic `${workspaceFolder}` resolution from lsp-mode's workspace object, and a larger template library for niche languages (Java via `dap-java`, Elixir, Dart/Flutter) . However, your `dape-cwd-function` lambda already resolves project roots via `project-current`, which lsp-mode itself uses. The dap-ui multi-pane layout is the other differentiator, but your existing Transient dashboard and `dape-many-windows t` setting replicate this. **Switching to dap-mode would require dismantling your entire dape configuration for marginal gain.**

---

## 4. Eglot → lsp-mode/lsp-ui Replacement

The Eglot subsection in `config.org` will be **replaced entirely** with an lsp-mode/lsp-ui subsection. The `consult-eglot` and `consult-eglot-embark` packages will be replaced with `consult-lsp`. The custom `ar/eglot-rename-file` and `ar/eglot-moniker-at-point` functions become unnecessary because lsp-mode implements `workspace/willRenameFiles`/`didRenameFiles` natively , and moniker inspection is available via `lsp-execute-command`.

The `eglot-ignored-server-capabilities` list from your current config maps to the following lsp-mode variables (addressed in Section 8 below).

---

## 5. Flymake Subsection — Remove

**Verdict: Remove the entire Flymake subsection (flymake + flyover + consult-flymake-project).**

lsp-mode routes diagnostics through **flycheck** (preferred) or flymake as a fallback . The `lsp-ui` package provides sideline diagnostics rendering that supersedes both your flymake margin indicators and the flyover inline overlay engine . With `(setq lsp-diagnostics-provider :flycheck)`, all LSP diagnostics flow into flycheck's checker infrastructure, and `lsp-ui-sideline-show-diagnostics t` renders them inline at the point of error . Your custom `flymake-ruff` integration for Python also becomes unnecessary because ruff, when running as an LSP server, publishes diagnostics directly through the LSP protocol (addressed in Section 12).

**Replacement**: A new `flycheck` subsection will be needed, configured as lsp-mode's diagnostics backend.

---

## 6. eldoc-childframe Subsection — Remove

**Verdict: Remove the entire eldoc-childframe subsection.**

`lsp-ui-doc` provides a strictly superior hover documentation experience: it renders full Markdown-formatted hover payloads in a child frame with configurable position, delay, and dimensions . It replaces eldoc-childframe's functionality entirely. The `lsp-ui-doc` child frame supports header lines, clickable URLs, and syntax-highlighted code blocks natively. Your `M-h` binding for on-demand documentation can be remapped to `lsp-ui-doc-show` or `lsp-ui-doc-glance`.

---

## 7. Eldoc Subsection — Keep but Modify

**Verdict: Retain the Eldoc subsection with modifications.**

lsp-mode **does** use Eldoc under the hood. When `lsp-auto-configure` is `t` (the default), lsp-mode wires `textDocument/signatureHelp` and `textDocument/hover` into Eldoc's `eldoc-documentation-functions` automatically . The `lsp-eldoc-enable-hover` variable (default `t`) controls whether hover info flows through Eldoc .

**What to keep:**

- `eldoc-echo-area-use-multiline-p 2` — still governs the echo-area fallback when lsp-ui-doc is not active
- `eldoc-echo-area-prefer-doc-buffer 'maybe` — still valid
- `eldoc-idle-delay 0.1` — still valid; lsp-mode's `lsp-idle-delay` is a separate timer
- `global-eldoc-mode 1` — required for lsp-mode's Eldoc integration

**What to remove:**

- The `eldoc-help-at-pt` setopt (Emacs 31 keybinding hints) — orthogonal to LSP, can stay if desired
- The `elisp-eldoc-docstring-length-limit` — Elisp-specific, harmless to keep
- The `eglot-managed-mode-hook` lambda setting `eldoc-documentation-compose-eagerly` — **replace** with lsp-mode's own signature configuration:

```elisp
(setq lsp-signature-auto-activate '(:on-trigger-char :on-server-request)
      lsp-signature-render-documentation t
      lsp-signature-doc-lines 2)
```

---

## 8. Xref Subsection — Keep but Modify

**Verdict: Retain the Xref subsection with modifications.**

lsp-mode integrates with xref natively when `lsp-enable-xref` is `t` (the default) . It populates `xref-backend-functions` with its own backend that routes `xref-find-definitions` to `textDocument/definition` and `xref-find-references` to `textDocument/references` . However, lsp-mode also provides **dedicated commands** (`lsp-find-definition`, `lsp-find-declaration`, `lsp-find-type-definition`, `lsp-find-implementation`) that are strictly superior to generic xref because they can distinguish declaration from definition — plain `xref-find-definitions` cannot .

**What to keep:**

- `xref-search-program 'ripgrep` — still governs `xref-find-references` fallback when LSP is unavailable
- `xref-file-name-display 'project-relative` — still valid for xref result buffers
- The `xref-edit-mode` Evil normal state — still valid

**What to add:**

- `lsp-enable-xref t` (default, but explicit)
- Keybinding remaps from `xref-find-definitions` to `lsp-find-definition` for LSP buffers

---

## 9. Quickrun / Eval-Overlay Equivalent in lsp-mode

**Verdict: lsp-mode provides NO equivalent. Retain quickrun, eros, and eval-overlay.**

Code evaluation and execution is **not part of the LSP specification**. lsp-mode is strictly a language server protocol client; it handles completion, diagnostics, navigation, refactoring, and formatting — but not code execution . The `textDocument/executeCommand` LSP method invokes server-defined commands (e.g., "organize imports", "run test"), but it does not provide arbitrary region evaluation with output display.

Your quickrun + eros + eval-overlay stack is orthogonal to the LSP layer and must be retained in full.

---

## 10. Features to Disable (Matching Eglot's Ignored Capabilities)

Your current Eglot config disables these server capabilities:

```elisp
(eglot-ignored-server-capabilities '(:documentFormattingProvider
                                      :documentOnTypeFormattingProvider
                                      :colorProvider
                                      :inlayHintProvider
                                      :foldingRangeProvider))
```

The lsp-mode equivalents to disable:

| Eglot Capability                    | lsp-mode Variable                            | Rationale                                                                       |
| ----------------------------------- | -------------------------------------------- | ------------------------------------------------------------------------------- |
| `:documentFormattingProvider`       | `lsp-format-buffer-on-save nil` (default)    | Apheleia handles formatting asynchronously with point preservation              |
| `:documentOnTypeFormattingProvider` | `(setq lsp-enable-on-type-formatting nil)`   | Electric-indent + treesit provides equivalent behaviour without LSP round-trips |
| `:colorProvider`                    | `(setq lsp-enable-text-document-color nil)`  | colorful-mode already handles color visualization client-side                   |
| `:inlayHintProvider`                | `(setq lsp-inlay-hint-enable nil)` (default) | Reduces idle-timer overhead; dape provides debug-scoped inlay hints separately  |
| `:foldingRangeProvider`             | `(setq lsp-enable-folding nil)`              | treesit-fold + hideshow + vimish-fold already provide superior folding          |

**Additional performance-oriented disables:**

```elisp
(setq lsp-enable-symbol-highlighting t    ; Keep — see Section 11
      lsp-enable-links nil                ; See Section 15
      lsp-lens-enable nil                 ; See Section 13
      lsp-headerline-breadcrumb-enable t  ; See Section 12
      lsp-modeline-code-actions-enable t  ; Keep — lightweight
      lsp-modeline-diagnostics-enable t   ; Keep — lightweight
      lsp-enable-file-watchers t
      lsp-file-watch-threshold 4000
      lsp-log-io nil                      ; CRITICAL: logging causes massive perf hit
      lsp-idle-delay 0.3)                 ; Balance responsiveness vs. server load
```

---

## 11. symbol-overlay Package vs lsp-mode Built-in Symbol Highlighting

**Verdict: lsp-mode's built-in is semantically superior; retain it. Do NOT add symbol-overlay.**

lsp-mode's `lsp-enable-symbol-highlighting` (default `t`) uses the LSP `textDocument/documentHighlight` method, which returns **semantically classified** occurrences: `text` (textual match), `read` (variable being read), and `write` (variable being written to), each rendered with distinct faces (`lsp-face-highlight-textual`, `lsp-face-highlight-read`, `lsp-face-highlight-write`) . The symbol-overlay package, by contrast, uses client-side regex/font-lock matching and cannot distinguish read from write access .

**Performance consideration**: lsp-mode's highlighting costs one LSP round-trip per idle cycle (governed by `lsp-idle-delay`). At `lsp-idle-delay 0.3`, this is a 300ms debounce — negligible on modern hardware with a responsive server. The lsp-bridge author explicitly disabled symbol highlighting for performance reasons , but this is an extreme optimisation that sacrifices semantic accuracy. For your configuration, the built-in highlighting at a 0.3s idle delay is the correct trade-off.

If performance becomes an issue on very large files, add a buffer-local guard:

```elisp
(defun ar/lsp-large-file-tuning ()
  (when (> (buffer-size) 200000)
    (setq-local lsp-enable-symbol-highlighting nil)))
(add-hook 'lsp-mode-hook #'ar/lsp-large-file-tuning)
```

---

## 12. Breadcrumb: lsp-mode Built-in vs External breadcrumb.el

**Verdict: lsp-mode's built-in `lsp-headerline-breadcrumb-mode` is superior for your use case.**

lsp-mode's breadcrumb implementation uses `textDocument/documentSymbol` to render the full scope path (project → file → namespace → class → method) in the header line, with clickable segments and live diagnostic indicators . It is tightly integrated with lsp-mode's symbol resolution and updates automatically as point moves .

The external `breadcrumb.el` package (by João Távora, Eglot's author) is a **generic rendering library** that provides the visual breadcrumb infrastructure but requires manual wiring to data sources . It was designed primarily for Eglot, which lacks a built-in breadcrumb. Since you are migrating **to** lsp-mode, the built-in is already wired and requires zero additional integration.

**Known issue**: `lsp-headerline-breadcrumb-mode` can cause slowdowns during continuous scrolling on some systems . Mitigation:

```elisp
(setq lsp-headerline-breadcrumb-enable t
      lsp-headerline-breadcrumb-enable-diagnostics t
      lsp-headerline-breadcrumb-icons-enable t
      lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
```

If scrolling performance degrades, set `lsp-headerline-breadcrumb-enable-diagnostics nil` to remove the per-symbol error/warning count computation.

---

## 13. LSP Lens — Purpose and Usefulness

**Code Lens** displays actionable contextual information interspersed in source code — for example, "▶ Run Test", "🐛 Debug Test", "3 references", "2 implementations" rendered as clickable annotations above functions and classes .

**Verdict: Disable for your configuration.**

Rationale:

- Your `projection` and `projection-multi` packages already provide build/test/run commands via Transient menus
- LSP lens adds idle-timer overhead (each lens requires a `codeLens/resolve` round-trip)
- The visual clutter conflicts with your minimalist aesthetic (indent-bars, org-modern, etc.)
- `lsp-lens-show` has known reliability issues with some servers

```elisp
(setq lsp-lens-enable nil)
```

---

## 14. LSP Code Actions — Purpose and Usefulness

**Code Actions** are the LSP equivalent of VS Code's 💡 lightbulb: quick fixes for diagnostics, refactorings (extract method, inline variable), and source actions (organize imports, generate boilerplate) .

**Verdict: Keep enabled. This is one of lsp-mode's most valuable features.**

```elisp
(setq lsp-modeline-code-actions-enable t
      lsp-auto-execute-action t)
```

lsp-mode renders code actions both as a modeline indicator and, via `lsp-ui-sideline-show-code-actions t`, as an inline icon at the diagnostic site . The `lsp-execute-code-action` command (bound to `C-c l a a` by default) presents a completion menu of available actions. This is **not** replicable by any other package in your stack.

---

## 15. lsp-enable-links — What It Does

`lsp-enable-links` (default `t`) makes URLs and file paths detected by the language server's `textDocument/documentLink` capability clickable within the buffer . For example, a URL in a comment or a file path in a string literal becomes a clickable link that opens via `browse-url` or `find-file`.

**Verdict: Disable.**

Rationale:

- Your `link-hint` package already provides Avy-backed link navigation for URLs and file paths across all buffer types
- `lsp-enable-links` adds overlay rendering overhead on every idle cycle
- There is a known bug where setting it to `nil` does not always disable existing links

```elisp
(setq lsp-enable-links nil)
```

---

## 16. Python: lazy-ruff vs lsp-mode Range Formatting

**Verdict: Using ruff as an LSP server for range formatting is faster than lazy-ruff.**

`lazy-ruff` shells out to the `ruff` CLI binary as a subprocess for each formatting invocation. When ruff runs as an LSP server (`ruff server`), range formatting requests (`textDocument/rangeFormatting`) are handled **in-process** by the already-running server — eliminating subprocess spawn overhead entirely . The ruff server is written in Rust and handles range formatting natively.

However, **retain Apheleia for whole-file formatting**. Apheleia's RCS-patch reconciliation preserves point position and only touches changed hunks in the undo history — lsp-mode's `lsp-format-buffer` is synchronous and can jump the cursor on large reformats. The optimal Python formatting pipeline is:

| Operation                 | Tool                                               | Mechanism                          |
| ------------------------- | -------------------------------------------------- | ---------------------------------- |
| Whole-file format on save | Apheleia (`ruff format`)                           | Async subprocess, point-preserving |
| Range/region formatting   | lsp-mode (`lsp-format-region`) via ruff server     | In-process LSP request             |
| Import sorting            | Apheleia (`ruff isort`) or ruff server code action | Async / LSP                        |

This means **lazy-ruff can be removed** from the Python subsection.

---

## 17. Python: pyrefly/ty + ruff as Dual LSP Servers

**Verdict: The dual-server approach is superior and is the recommended architecture.**

lsp-mode natively supports running multiple LSP servers for the same file type via the `:add-on? t` flag . This is architecturally identical to how VS Code, Neovim, and Zed handle multiple servers .

**ty (or pyrefly) and ruff are complementary, not overlapping** :

| Concern                    | ty / pyrefly                     | ruff server        |
| -------------------------- | -------------------------------- | ------------------ |
| Type checking              | ✅ Primary purpose               | ❌                 |
| Go to definition           | ✅                               | ❌                 |
| Find references            | ✅                               | ❌                 |
| Rename symbol              | ✅                               | ❌                 |
| Auto-completion            | ✅                               | ❌                 |
| Auto-import                | ✅                               | ✅                 |
| Linting                    | Partial (dead code, unused deps) | ✅ Primary purpose |
| Formatting                 | ❌                               | ✅                 |
| Code actions (quick fixes) | ✅ (type-related)                | ✅ (lint fixes)    |
| Semantic highlighting      | ✅                               | ❌                 |
| Range formatting           | ❌                               | ✅                 |

Running both as LSP servers gives you:

- **Unified diagnostics** in flycheck/lsp-ui from both type errors and lint violations
- **Code actions from both servers** (type fixes + lint fixes) in a single `lsp-execute-code-action` menu
- **No need for flymake-ruff, flycheck-ruff, or any external linter integration**
- **Range formatting** via ruff server (replacing lazy-ruff)

The cost is two LSP processes per Python buffer. On modern hardware with Rust-based servers, this is negligible — ty recomputes diagnostics in ~4.7ms after edits , and ruff server is similarly fast.

**Recommended configuration:**

```elisp
(with-eval-after-load 'lsp-mode
  ;; Primary server: ty (type checking, navigation, completion)
  (add-to-list 'lsp-disabled-clients 'pyls)  ; Disable legacy servers
  ;; ruff as add-on server (linting, formatting, code actions)
  ;; lsp-mode's lsp-ruff client already registers with :add-on? t
  )
```

lsp-mode already ships `lsp-python-ty` and `lsp-ruff` clients as of the July 2026 changelog . The `lsp-ruff` client is registered with `:add-on? t` by default, meaning it starts in parallel with the primary server automatically.

**This eliminates the need for:**

- `flymake-ruff` (diagnostics come via LSP)
- `lazy-ruff` (range formatting comes via ruff server)
- The `ar/filter-eglot-python-notes` advice (no more Eglot note filtering)

---

## 18. lsp-bridge Evaluation

**Verdict: lsp-bridge is NOT recommended for your configuration.**

lsp-bridge uses Python's threading technology to build caches that bridge Emacs and LSP servers, aiming to be the fastest LSP client in the Emacs ecosystem . However:

1. **Reliability concerns**: Independent reviews describe it as "usable, but not perfectly reliable" with "the risk of bumping into a tree" .
2. **Ecosystem incompatibility**: lsp-bridge has its own completion UI, its own diagnostics rendering, and its own navigation commands. It does **not** integrate with flycheck, xref, imenu, or the broader Emacs ecosystem that your config depends on .
3. **Corfu incompatibility**: lsp-bridge initially used company-mode, then corfu, but eventually moved to its own completion frontend because the author found integration "much more painful" .
4. **Python dependency**: lsp-bridge requires a Python runtime for its threading layer, adding a failure mode that lsp-mode (pure Elisp + native JSON) does not have.
5. **Your config's complexity**: Your 0.34 configuration has deep integrations with Evil, general.el, Transient, consult, embark, dirvish, treemacs, denote, and 25+ other packages. lsp-bridge's parallel UI system would conflict with nearly all of them.

The performance gap between lsp-bridge and a properly tuned lsp-mode (with `lsp-use-plists`, `read-process-output-max`, and optionally `emacs-lsp-booster`) is negligible for day-to-day use . lsp-mode's performance page confirms that "when configured properly lsp-mode's performance is on par with mainstream LSP clients (e.g. VScode, Theia, etc.)" .

---

## 19. JSON-RPC Payload Overhead Re-evaluation

Your `early-init.el` currently sets:

```elisp
(setq read-process-output-max (* 4 1024 1024))  ; 4MB
```

**Re-evaluation for lsp-mode:**

| Setting                       | Current Value                   | lsp-mode Recommendation                        | Verdict                                                                                                                                                                                                                          |
| ----------------------------- | ------------------------------- | ---------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `read-process-output-max`     | 4MB                             | 1MB minimum                                    | **Keep at 4MB.** Some server responses (semantic tokens, completion lists) reach 800KB–3MB . 4MB provides headroom without measurable downside.                                                                                  |
| `gc-cons-threshold` (startup) | `most-positive-fixnum`          | 100MB+ during startup                          | **Keep.** Your `early-init.el` already maximises this during startup.                                                                                                                                                            |
| `gc-cons-threshold` (runtime) | 16MB (via `emacs-startup-hook`) | Managed by gcmh at 128MB                       | **Keep gcmh.** Your gcmh configuration with `gcmh-high-cons-threshold (* 128 1024 1024)` is correct for lsp-mode workloads.                                                                                                      |
| `lsp-use-plists`              | Not set                         | `t` (via `LSP_USE_PLISTS=true` env var)        | **Add to `early-init.el`:** `(setenv "LSP_USE_PLISTS" "true")`. Plist deserialization is measurably faster than hash-table and puts less GC pressure. **Critical**: must be set before lsp-mode is byte-compiled.                |
| `emacs-lsp-booster`           | Not installed                   | Optional Rust wrapper for pre-parsing JSON-RPC | **Consider.** The booster pre-parses LSP JSON-RPC output into Emacs's native bytecode format before Emacs processes it, providing the single largest available performance win for JSON-heavy servers . Last updated June 2026 . |

**Addition to `early-init.el`:**

```elisp
;; Enable plist-based LSP deserialization for lsp-mode.
;; MUST be set before lsp-mode is byte-compiled/loaded.
(setenv "LSP_USE_PLISTS" "true")
```

---

## Summary of Subsection Dispositions

| config.org Subsection                    | Action                           | Rationale                                                  |
| ---------------------------------------- | -------------------------------- | ---------------------------------------------------------- |
| **Eglot**                                | **Replace** with lsp-mode/lsp-ui | Core migration                                             |
| **consult-eglot / consult-eglot-embark** | **Replace** with consult-lsp     | Ecosystem alignment                                        |
| **Flymake + flyover**                    | **Remove**                       | lsp-ui sideline + flycheck replaces both                   |
| **eldoc-childframe**                     | **Remove**                       | lsp-ui-doc replaces it                                     |
| **Eldoc**                                | **Keep, modify**                 | lsp-mode uses Eldoc internally; remove Eglot-specific hook |
| **Xref**                                 | **Keep, modify**                 | lsp-mode uses xref internally; add lsp-find-* commands     |
| **Eval Overlay / quickrun / eros**       | **Keep unchanged**               | No LSP equivalent exists                                   |
| **Apheleia**                             | **Keep unchanged**               | Remains the whole-file formatter                           |
| **flymake-ruff**                         | **Remove**                       | ruff LSP server provides diagnostics natively              |
| **lazy-ruff**                            | **Remove**                       | ruff LSP server provides range formatting natively         |
| **Python**                               | **Rewrite**                      | Dual ty/pyrefly + ruff LSP servers                         |
| **Corfu / Orderless / Cape / Vertico**   | **Keep unchanged**               | lsp-mode supports CAPF natively                            |
| **dape**                                 | **Keep unchanged**               | No advantage from dap-mode for this config                 |

This completes the full evaluation. Shall I proceed with generating the actual `config.org` subsections for the lsp-mode/lsp-ui migration?
