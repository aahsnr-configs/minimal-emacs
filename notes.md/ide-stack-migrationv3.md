# Comprehensive Migration Analysis: Eglot → lsp-mode/lsp-ui

## File Versions Ingested

| File                | Version / Date | Notes                                                                                                                                     |
| ------------------- | -------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `config.org.txt`    | **v0.34**      | Authored by Ahsanur Rahman; personal minimal Emacs config tuned for Emacs 30/31                                                           |
| `early-init.el.txt` | Unversioned    | Bootstrap GC deferral, redisplay optimizations, native-comp ELN cache redirect                                                            |
| `ide-features.md`   | **July 2026**  | Verified against lsp-mode 9.x, lsp-ui 8.x, flycheck 34+, apheleia (2026 MELPA snapshot), dape 0.2x (GNU ELPA), dap-mode 0.7+, Emacs 30/31 |

I have fully ingested all three files. Your `config.org` currently uses **Eglot** (built-in), **Flymake** + **flyover**, **eldoc** + **eldoc-childframe**, **xref**, **dape**, **apheleia**, **corfu/cape/orderless/vertico**, and **quickrun/eros/eval-overlay**. The `ide-features.md` document provides the target lsp-mode architecture. Below is the complete re-evaluation.

---

## 1. Completion Framework: Corfu Stays — Confirmed

lsp-mode natively supports corfu. When you set `(lsp-completion-provider :capf)`, lsp-mode registers `lsp-completion-at-point` into `completion-at-point-functions`, which corfu reads directly — no merging layer like `cape` is strictly required for LSP candidates . Your existing corfu + orderless + vertico + marginalia + embark stack remains fully intact .

**Critical fix required:** lsp-mode injects its own entry into `completion-category-overrides` for the `lsp-capf` category, which can override your orderless styles . You must add this to your orderless `:config`:

```emacs-lisp
;; Prevent lsp-mode from overriding orderless in LSP buffers.
(with-eval-after-load 'lsp-mode
  (add-to-list 'completion-category-overrides
               '(lsp-capf (styles orderless basic))))
```

Your existing `cape` configuration for `cape-wrap-nonexclusive` on `eglot-completion-at-point` should be retargeted to `lsp-completion-at-point`:

```emacs-lisp
(with-eval-after-load 'lsp-mode
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive))
```

**JSON-RPC overhead note:** Completion triggers `textDocument/completion` + `completionItem/resolve` round-trips. With `lsp-use-plists t` and `emacs-lsp-booster`, the deserialization cost drops significantly . Your existing `corfu-auto-delay 0.2` and `corfu-auto-prefix 2` are appropriate guards against GC stutter during rapid LSP completion requests.

---

## 2. dap-mode vs dape with lsp-mode/lsp-ui

| Criterion          | dape (your current)                                          | dap-mode                                                           |
| ------------------ | ------------------------------------------------------------ | ------------------------------------------------------------------ |
| Dependencies       | Zero external deps; works with eglot, lsp-mode, or neither   | **Requires lsp-mode** for `${workspaceFolder}` and project context |
| Maintenance        | Actively maintained, GNU ELPA                                | Actively maintained, emacs-lsp org                                 |
| Config style       | Transparent plists, edit inline before launch                | Heavier `dap-register-debug-template` launch.json-style            |
| UI                 | Minimal: inline overlays, REPL, `dape-info` buffers          | Fuller VS Code-style: `dap-ui-mode` with dedicated panes           |
| Language templates | Good for Python, Go, Rust, C/C++, JS/TS, Bash                | Larger library: Java, Elixir, Ruby, PHP, Dart/Flutter              |
| lsp-mode coupling  | None — you wire `dape-cwd-function` to `project.el` yourself | Auto-discovers debug adapters via lsp-mode's server registry       |

**Verdict for your config:** **Keep dape.** Your config already has an extensive dape setup with transient dashboard, breakpoint persistence, inlay hints, spatial routing, and `projection-dape` integration. dap-mode's only advantage is tighter lsp-mode coupling for auto-discovery and a larger template library for exotic languages (Java/Elixir/Dart) . Since your workflow centers on Python/general programming and you've already built a superior UX layer around dape, switching would be a net regression. One blog confirms: "After my frustration trying to configure dap-mode, dape just worked!" .

---

## 3. Subsections to Remove

### 3a. Flymake + flyover — **REMOVE entirely**

lsp-mode routes diagnostics through **flycheck** (preferred) or flymake via `(lsp-diagnostics-provider :flycheck)` . The lsp-mode + flycheck integration is more mature: flycheck supports richer diagnostic metadata, multiple checkers per buffer, and `lsp-ui-sideline` renders flycheck diagnostics inline . Your entire `Flymake` subsection (flymake, flyover, `consult-flymake-project`) and the `flymake-ruff` package in the Python section should be replaced with flycheck.

### 3b. eldoc-childframe — **REMOVE entirely**

`lsp-ui-doc` provides a strictly superior hover documentation childframe with Markdown rendering, header lines, mouse tracking, and configurable positioning . It replaces both your `eldoc-childframe` package and the `M-h` on-demand binding. Your `eldoc-childframe` subsection (including all custom faces, offsets, and the `eldoc-childframe-hover-at-point-mode` hook) should be deleted.

### 3c. Eglot subsection — **REMOVE entirely** (replaced by lsp-mode)

All eglot-specific code (`eglot-ensure`, `eglot-server-programs`, `ar/eglot-rename-file`, `ar/eglot-moniker-at-point`, `consult-eglot`, `consult-eglot-embark`) is replaced by lsp-mode equivalents.

---

## 4. Subsections to Keep but Modify

### 4a. Eldoc — **KEEP, but simplify**

lsp-mode **uses eldoc under the hood** to display hover signatures in the echo area . When `lsp-auto-configure` is `t`, lsp-mode automatically enables `eldoc-mode` and wires `textDocument/signatureHelp` + `textDocument/hover` into eldoc's documentation strategy . Your eldoc subsection should be **retained but stripped of eglot-specific hooks**:

**Remove:**

- The `eglot-managed-mode-hook` lambda that sets `eldoc-documentation-compose-eagerly`
- The `eldoc-help-at-pt` setopt (Emacs 31 keybinding hints — orthogonal to LSP)

**Keep:**

- `eldoc-echo-area-use-multiline-p 2` (still governs echo area truncation)
- `eldoc-echo-area-prefer-doc-buffer 'maybe`
- `eldoc-idle-delay 0.1`
- `global-eldoc-mode 1`

lsp-mode's eldoc integration provides current-argument highlighting in signatures, which is superior to eglot's .

### 4b. Xref — **KEEP, but simplify**

lsp-mode **registers its own xref backend** (`lsp--xref-backend`) so that `xref-find-definitions`, `xref-find-references`, etc. route through LSP . However, lsp-mode also provides **dedicated commands** (`lsp-find-definition`, `lsp-find-declaration`, `lsp-find-type-definition`, `lsp-find-implementation`) that are strictly superior because plain `xref-find-definitions` cannot distinguish declaration from definition .

**Keep:**

- `xref-search-program 'ripgrep` (still used for non-LSP regex searches)
- `xref-file-name-display 'project-relative`
- The `evil-set-initial-state 'xref-edit-mode 'normal` guard

**Remove:** Nothing needs removal — xref remains the fallback navigation layer and lsp-mode builds on top of it.

---

## 5. Quickrun / Eros / Eval-Overlay — **KEEP entirely**

lsp-mode does **not** provide any code evaluation or execution features . The LSP protocol has no `textDocument/evaluate` method. Code execution is handled by language-specific REPLs (CIDER for Clojure, etc.) or external tools like quickrun . Your `quickrun`, `eros`, and `eval-overlay` subsections are orthogonal to the LSP layer and must remain unchanged.

---

## 6. LSP Features to Disable (Matching Eglot's Disabled Capabilities)

Your eglot config disables these server capabilities:

```emacs-lisp
(eglot-ignored-server-capabilities '(:documentFormattingProvider
                                      :documentOnTypeFormattingProvider
                                      :colorProvider
                                      :inlayHintProvider
                                      :foldingRangeProvider))
```

The lsp-mode equivalents to disable:

| Eglot capability                    | lsp-mode variable                                                 | Rationale                                                          |
| ----------------------------------- | ----------------------------------------------------------------- | ------------------------------------------------------------------ |
| `:documentFormattingProvider`       | `(lsp-enable-formatting nil)` or remove from `lsp-auto-configure` | Apheleia handles formatting asynchronously with point preservation |
| `:documentOnTypeFormattingProvider` | `(lsp-enable-on-type-formatting nil)`                             | Electric-indent + apheleia on save is sufficient                   |
| `:colorProvider`                    | `(lsp-enable-color-decorators nil)`                               | Your `colorful-mode` already handles this client-side              |
| `:inlayHintProvider`                | `(lsp-inlay-hint-enable nil)`                                     | Performance overhead; dape provides debug-scoped inlay hints       |
| `:foldingRangeProvider`             | `(lsp-enable-folding nil)`                                        | Your `treesit-fold` + `hideshow` + `vimish-fold` stack is superior |

Additionally disable these for performance (matching the `ide-features.md` appendix):

```emacs-lisp
(setq lsp-enable-symbol-highlighting nil   ; JSON-RPC round-trip per idle tick
      lsp-enable-links nil                 ; documentLink requests on every visible range
      lsp-lens-enable nil                  ; codeLens requests on visible range
      lsp-headerline-breadcrumb-enable t   ; KEEP — see §8
      lsp-modeline-code-actions-enable t   ; KEEP — see §11
      lsp-semantic-tokens-enable nil)      ; tree-sitter handles highlighting
```

---

## 7. symbol-overlay Package vs lsp-mode's Built-in Symbol Highlighting

lsp-mode's `lsp-enable-symbol-highlighting` uses `textDocument/documentHighlight`, which is **semantically aware** — it distinguishes read-access, write-access, and textual occurrences . The `symbol-overlay` package is purely **syntactic** (regex-based string matching) .

**Performance verdict:** lsp-mode's symbol highlighting incurs a JSON-RPC round-trip on every idle tick (`lsp-idle-delay`), which is the single most cited performance complaint . The lsp-bridge author explicitly states: "symbol highlight is not useful and it will slow down the render performance" . The official lsp-mode performance guide recommends disabling it .

**Recommendation:** Set `(lsp-enable-symbol-highlighting nil)`. If you want symbol highlighting, `symbol-overlay` is **faster** because it operates entirely client-side with zero JSON-RPC overhead. However, given your existing `evil-snipe` incremental highlighting and `iedit`/`evil-multiedit` for multi-occurrence editing, you likely don't need either.

---

## 8. Breadcrumb: lsp-mode Built-in vs External `breadcrumb.el`

| Feature        | `lsp-headerline-breadcrumb-mode` (lsp-mode)      | `breadcrumb.el` (GNU ELPA, João Távora) |
| -------------- | ------------------------------------------------ | --------------------------------------- |
| Data source    | LSP `textDocument/documentSymbol` — **semantic** | `imenu`/`xref` backends — **syntactic** |
| Scope path     | `project > file > namespace > class > method`    | `project > file > imenu-entry`          |
| Diagnostics    | Shows error/warning counts inline per segment    | No diagnostic awareness                 |
| Clickable      | Mouse + keyboard navigation to any segment       | Mouse + keyboard navigation             |
| LSP dependency | Requires active LSP session                      | Works without LSP (uses imenu)          |
| Performance    | One `documentSymbol` request per buffer change   | Uses cached imenu index                 |

**Verdict:** lsp-mode's built-in breadcrumb is **strictly superior** for your use case because it provides semantic scope resolution (namespace > class > method) that imenu-based `breadcrumb.el` cannot match . It also integrates diagnostic counts. Since you're adopting lsp-mode, use `lsp-headerline-breadcrumb-mode` and do **not** install the external `breadcrumb.el` package.

```emacs-lisp
(setq lsp-headerline-breadcrumb-enable t
      lsp-headerline-breadcrumb-enable-diagnostics t
      lsp-headerline-breadcrumb-icons-enable t
      lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
```

---

## 9. Python: lazy-ruff Range Formatting vs LSP Range Formatting

Your current stack: **apheleia** (whole-file, async, on-save) + **lazy-ruff** (region/org-src-block formatting via subprocess).

If ruff runs as an LSP server (`ruff server`), lsp-mode exposes `textDocument/rangeFormatting` natively. This is **faster** than lazy-ruff because:

1. **No process spawn:** The ruff server is already running as a persistent JSON-RPC connection; lazy-ruff spawns a new `ruff format` subprocess per invocation .
2. **Single round-trip:** One JSON-RPC request vs. subprocess creation + stdin/stdout piping + process cleanup.
3. **Server-side config:** The ruff server reads `pyproject.toml`/`ruff.toml` once at startup; lazy-ruff re-resolves config per invocation.

**Recommendation:** Replace `lazy-ruff` with `lsp-format-region` (bound to a key in your Python local leader). Keep apheleia for whole-file format-on-save because its RCS-patch point preservation is superior to synchronous `lsp-format-buffer` .

---

## 10. Python: pyrefly/ty + ruff as Dual LSP Servers

lsp-mode supports **multiple concurrent servers** per major mode via the `:add-on? t` flag . The recommended Python architecture:

```
┌─────────────────────────────────────────────────┐
│              python-ts-mode buffer              │
├────────────────────┬────────────────────────────┤
│  PRIMARY SERVER    │  ADD-ON SERVER             │
│  pyrefly / ty      │  ruff server               │
│  ─────────────     │  ────────────              │
│  • Completion      │  • Linting (replaces       │
│  • Go-to-def       │    flycheck-ruff)          │
│  • Hover/signature │  • Formatting (range +     │
│  • Type checking   │    document)               │
│  • Rename          │  • Code actions (auto-fix, │
│  • References      │    organize imports)       │
│  • Workspace       │  • isort                   │
│    symbols         │                            │
└────────────────────┴────────────────────────────┘
         │                      │
         └──────┬───────────────┘
                ▼
        flycheck (lsp checker)
        merges diagnostics from BOTH
```

**Is dual-server faster than single-server + external linter?**

**Yes**, for these reasons:

- Both servers communicate via **persistent JSON-RPC connections** — zero subprocess spawning for diagnostics .
- Diagnostics from both merge into a single flycheck `lsp` checker, rendered by `lsp-ui-sideline` .
- Ruff's code actions (auto-fix, organize imports) become available inline via `lsp-execute-code-action` — impossible with flycheck-ruff .
- Range formatting via ruff server eliminates lazy-ruff's subprocess overhead.

**ty vs pyrefly:** ty is written in Rust by Astral (the ruff team) and is currently in alpha . pyrefly is Meta's type checker. Both are valid primaries. The `ide-features.md` document lists `eglot-alternatives` for `pyrefly` and `ty` — in lsp-mode, you'd register both and let priority select:

```emacs-lisp
(with-eval-after-load 'lsp-mode
  ;; Primary: pyrefly (or ty when stable)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("pyrefly" "lsp"))
                    :major-modes '(python-mode python-ts-mode)
                    :priority 10
                    :server-id 'pyrefly))
  ;; Add-on: ruff server
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("ruff" "server"))
                    :major-modes '(python-mode python-ts-mode)
                    :add-on? t
                    :server-id 'ruff)))
```

This eliminates: `flymake-ruff`, `lazy-ruff`, and the `ar/filter-eglot-python-notes` advice. Apheleia remains for whole-file format-on-save.

---

## 11. LSP Lens and Code Actions — Usefulness Evaluation

### Code Lens (`lsp-lens-mode`)

Code lens displays **actionable contextual information interspersed in source code** — e.g., "▶ Run Test", "Debug Test", "42 references" above functions . It uses `textDocument/codeLens` requests on the visible range.

**Usefulness for your workflow:** Moderate. Primarily valuable for test runners (pytest, jest) and reference counts. **Performance cost:** Each visible code lens requires a JSON-RPC round-trip, and lenses refresh on every `lsp-idle-delay` tick .

**Recommendation:** **Disable globally** (`lsp-lens-enable nil`) for performance. Enable per-buffer only when needed via `M-x lsp-lens-show` .

### Code Actions (`lsp-execute-code-action`)

The 💡 lightbulb system: quick fixes, auto-imports, refactorings, organize imports . This is one of the **most valuable LSP features** and has no equivalent in a non-LSP setup.

**Usefulness:** **High.** Auto-import on completion, auto-fix diagnostics, extract method/variable, organize imports — these are core IDE features.

**Recommendation:** **Keep enabled.** Use `lsp-ui-sideline` for inline code action indicators, and bind `lsp-execute-code-action` to your local leader.

```emacs-lisp
(setq lsp-modeline-code-actions-enable t
      lsp-ui-sideline-show-code-actions t)
```

---

## 12. `lsp-enable-links` — What It Does

`lsp-enable-links` activates `textDocument/documentLink` support, which makes **URLs and file paths inside comments and strings clickable** . When enabled, lsp-mode renders detected links as clickable overlays; clicking invokes `browse-url` for HTTP URLs or `find-file` for file paths.

**Performance cost:** Low — one `documentLink` request per visible range on idle.

**Recommendation:** **Disable** (`lsp-enable-links nil`). Your `link-hint` package already provides superior Avy-backed link navigation across all buffer types, and `documentLink` adds a JSON-RPC round-trip for a feature you already have client-side.

---

## 13. lsp-bridge — Evaluation for Your Config

lsp-bridge uses Python threading to pre-parse JSON-RPC payloads, achieving the **fastest raw LSP performance** in the Emacs ecosystem . However:

| Criterion             | lsp-bridge                                                                              | lsp-mode (your target)                    |
| --------------------- | --------------------------------------------------------------------------------------- | ----------------------------------------- |
| Completion UI         | **Reimplements its own** (acm) — does NOT use corfu/company                             | Uses standard CAPF → corfu works natively |
| Evil integration      | Poor; posframe-heavy UI conflicts with evil states                                      | Full evil-collection support              |
| TRAMP                 | Broken for remote files                                                                 | Works via `lsp-remote`                    |
| Emacs API integration | Bypasses xref, flycheck, eldoc, imenu                                                   | Integrates with all standard APIs         |
| Your existing stack   | **Incompatible** with corfu, vertico, embark, consult, cape, evil-snipe, evil-multiedit | Fully compatible                          |
| Maintenance           | Single maintainer (manateelazycat)                                                      | emacs-lsp org, large contributor base     |

**Verdict:** lsp-bridge is **not suitable for your configuration**. It would require abandoning your entire corfu/vertico/embark/consult/cape completion stack, your evil-modal editing layer, and your posframe-based UI architecture . The performance gain over a properly tuned lsp-mode (with `emacs-lsp-booster` + `lsp-use-plists`) is marginal for most workloads .

---

## 14. JSON-RPC Payload Overhead — Complete Mitigation Strategy

Your `early-init.el` already sets `read-process-output-max` to 4MB, which is the correct foundation. The full optimization stack for lsp-mode:

### Layer 1: Process I/O (already done)

```emacs-lisp
;; early-init.el — KEEP AS-IS
(setq read-process-output-max (* 4 1024 1024))  ; 4MB chunks
```

### Layer 2: Deserialization

```emacs-lisp
;; init.el — NEW
(setq lsp-use-plists t)  ; Plist deserialization outperforms hash-table/json-read
```

lsp-mode's benchmarks show plist-based deserialization meaningfully outperforms the default hash-table path, particularly on nested JSON from semantic-tokens and completion responses .

### Layer 3: emacs-lsp-booster (Rust wrapper)

A small Rust binary that wraps the language server process, pre-parsing JSON-RPC output into Emacs's native fast-path bytecode **before Emacs ever sees it** . This is the single highest-leverage optimization available:

```emacs-lisp
;; Install: cargo install emacs-lsp-booster
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode) (funcall bytecode))))
      (apply old-fn args)))
(advice-add (if (progn (require 'json) (fboundp 'json-parse-buffer))
                'json-parse-buffer 'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?) (not (file-remote-p default-directory))
             lsp-use-plists (executable-find "emacs-lsp-booster"))
        (cons "emacs-lsp-booster" orig-result)
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
```

### Layer 4: Feature pruning (see §6)

Every disabled feature eliminates a class of JSON-RPC requests:

- Symbol highlighting: eliminates `textDocument/documentHighlight` per idle tick
- Lenses: eliminates `textDocument/codeLens` per visible range
- Links: eliminates `textDocument/documentLink` per visible range
- Semantic tokens: eliminates `textDocument/semanticTokens/range` per fontification pass
- Inlay hints: eliminates `textDocument/inlayHint` per visible range

### Layer 5: GC management (already done)

Your `gcmh` configuration with 128MB threshold is appropriate. lsp-mode's official performance guide recommends the same approach .

### Layer 6: Idle delay tuning

```emacs-lisp
(setq lsp-idle-delay 0.3)  ; Default 0.5; 0.3 balances responsiveness vs. request volume
```

---

## 15. Summary: Subsection Disposition Matrix

| config.org Subsection                     | Action     | Rationale                                       |
| ----------------------------------------- | ---------- | ----------------------------------------------- |
| **Eglot**                                 | 🗑️ REMOVE  | Replaced by lsp-mode                            |
| **consult-eglot / consult-eglot-embark**  | 🗑️ REMOVE  | Replaced by consult-lsp                         |
| **Flymake + flyover**                     | 🗑️ REMOVE  | Replaced by flycheck + lsp-ui-sideline          |
| **eldoc-childframe**                      | 🗑️ REMOVE  | Replaced by lsp-ui-doc                          |
| **Eldoc**                                 | ✏️ MODIFY  | Keep base config; remove eglot-specific hooks   |
| **Xref**                                  | ✏️ MODIFY  | Keep; lsp-mode registers its own xref backend   |
| **quickrun / eros / eval-overlay**        | ✅ KEEP    | No LSP equivalent exists                        |
| **Apheleia**                              | ✅ KEEP    | Async format-on-save with point preservation    |
| **Corfu / cape / orderless / vertico**    | ✅ KEEP    | lsp-mode supports CAPF natively                 |
| **dape**                                  | ✅ KEEP    | Superior to dap-mode for your workflow          |
| **colorful-mode**                         | ✅ KEEP    | Replaces LSP colorProvider                      |
| **treesit-fold / hideshow / vimish-fold** | ✅ KEEP    | Replaces LSP foldingRange                       |
| **link-hint**                             | ✅ KEEP    | Replaces LSP documentLink                       |
| **Python: flymake-ruff**                  | 🗑️ REMOVE  | Replaced by ruff LSP server diagnostics         |
| **Python: lazy-ruff**                     | 🗑️ REMOVE  | Replaced by `lsp-format-region` via ruff server |
| **Python: eglot-alternatives**            | ✏️ REPLACE | lsp-register-client with pyrefly + ruff add-on  |
| **NEW: lsp-mode**                         | ➕ ADD     | Core LSP client                                 |
| **NEW: lsp-ui**                           | ➕ ADD     | Hover doc, sideline, peek                       |
| **NEW: flycheck**                         | ➕ ADD     | Diagnostics provider for lsp-mode               |
| **NEW: consult-lsp**                      | ➕ ADD     | Fuzzy workspace symbols, references             |
| **NEW: lsp-treemacs**                     | ➕ ADD     | Error list, call/type hierarchy, symbols        |
| **NEW: emacs-lsp-booster**                | ➕ ADD     | JSON-RPC performance wrapper                    |

This completes the full re-evaluation. Shall I proceed to generate the actual `config.org` subsections for the new lsp-mode/lsp-ui/flycheck stack?
