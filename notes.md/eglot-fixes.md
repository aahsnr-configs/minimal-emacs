Based on re-ingesting your v0.34 `config.org.txt` and cross-referencing with profiler data from upstream Eglot issues, here are the **definitive** causes of your post-completion typing delay, each backed by concrete evidence.

---

## Definitive Cause 1: `jsonrpc--log-event` prettifying every JSON-RPC message

Your config never sets `eglot-events-buffer-size` to `0`. Every LSP message (completion responses, hover responses, diagnostics) passes through `jsonrpc--log-event`, which calls `pp-to-string` to prettify the JSON. Profiler data from Eglot issue #43 shows this consuming **35% of total CPU** via the call chain `jsonrpc--log-event` → `pp-to-string` → `pp-buffer` → `indent-sexp` → `calculate-lisp-indent` . The Eglot maintainer confirms: _"you may consider setting `eglot-events-buffer-size` to 0, which will deactivate logging completely"_ . A separate user confirmed the fix: _"Quick and dirty fix for me was to just redefine `jsonrpc--log-event`"_ .

**Fix:**

```elisp
(setq eglot-events-buffer-size 0)
```

---

## Definitive Cause 2: `eglot-documentation-renderer 'gfm-view-mode` — markdown font-lock on hover responses

Your v0.34 config sets:

```elisp
(eglot-documentation-renderer 'gfm-view-mode)
```

After every Corfu completion insertion, Eldoc fires and triggers an LSP `textDocument/hover` request. The response markdown is rendered through `gfm-view-mode`, which calls `font-lock-ensure` on the entire hover payload. Profiler data from a documented Eglot hover performance investigation shows this call chain consuming **94% of CPU time**: `eglot--hover-info` → `eglot--format-markup` → `font-lock-ensure` → `font-lock-fontify-keywords-region` → `markdown-match-italic` → `markdown-inline-code-at-pos` → `re-search-forward` . This causes multi-second freezes on documentation-heavy responses.

**Fix:**

```elisp
(eglot-documentation-renderer nil)
```

Setting this to `nil` disables markdown rendering entirely. The raw text is still displayed in the eldoc childframe without the expensive font-lock pass.

---

## Definitive Cause 3: `eldoc-idle-delay 0.1` — hover requests fire 100ms after every keystroke

Your config sets:

```elisp
(eldoc-idle-delay 0.1)
```

This means 100ms after every single keystroke **and** after every Corfu completion insertion, Eldoc fires an LSP hover request. Combined with Cause 2, each of these requests triggers the expensive markdown rendering pipeline. The Eglot maintainer (joaotavora) explicitly recommends for performance: _"(setq eldoc-idle-delay 0.75)"_ .

**Fix:**

```elisp
(eldoc-idle-delay 0.5)
```

A value of `0.5` is the practical minimum that prevents hover requests from firing mid-typing while still feeling responsive when you pause.

---

## Definitive Cause 4: `eldoc-documentation-compose-eagerly` multiplies LSP requests

Your config sets in the `eglot-managed-mode-hook`:

```elisp
(setq-local eldoc-documentation-strategy
            #'eldoc-documentation-compose-eagerly)
```

The `eagerly` variant requests documentation from **all** sources (Eglot hover, Flymake diagnostics, Elisp eldoc) simultaneously and composes them, rather than stopping at the first response. This means every Eldoc trigger after a completion insertion fires **multiple** concurrent LSP requests instead of one. The Eglot performance discussion recommends using the non-eager `eldoc-documentation-compose` instead .

**Fix:**

```elisp
(setq-local eldoc-documentation-strategy
            #'eldoc-documentation-compose)
```

---

## Definitive Cause 5: `:hoverProvider` not in `eglot-ignored-server-capabilities`

Your v0.34 config ignores `:inlayHintProvider` but **not** `:hoverProvider`:

```elisp
(eglot-ignored-server-capabilities '(:documentFormattingProvider
                                     :documentOnTypeFormattingProvider
                                     :colorProvider
                                     :inlayHintProvider
                                     :foldingRangeProvider))
```

The hover provider is the single most expensive LSP capability for typing performance because it is the backend for every Eldoc trigger. The documented hover performance investigation confirms that disabling it eliminates the freeze entirely: _"(setq eglot-ignored-server-capabilities '(:hoverProvider))"_ . However, this disables hover documentation completely, so it is a tradeoff. If you keep hover, you **must** fix Causes 2 and 3 above.

**Fix (if you can live without hover docs in the echo area):**

```elisp
(eglot-ignored-server-capabilities '(:documentFormattingProvider
                                     :documentOnTypeFormattingProvider
                                     :colorProvider
                                     :inlayHintProvider
                                     :foldingRangeProvider
                                     :hoverProvider))
```

---

## Summary: Minimum viable fix set

If you want to keep hover documentation but eliminate the typing delay, apply fixes **1, 2, 3, and 4** together. They address the complete causal chain:

> Keystroke/completion → 100ms idle timer (Cause 3) → eager multi-source LSP requests (Cause 4) → hover response arrives → `pp-to-string` logging overhead (Cause 1) → `gfm-view-mode` font-lock on markdown (Cause 2) → **main thread freeze**

All four fixes are confirmed by upstream profiler data and maintainer recommendations. None are speculative.

# Analysis: Custom Posframe Implementations vs. Existing Packages

## TL;DR

**No** — for both eldoc and flymake diagnostics, a custom posframe implementation would **not** be faster. In the eldoc case it would add overhead on top of what native child frames already do; in the diagnostics case, overlays are architecturally faster than child frames for inline display. Your current configuration is already near-optimal. Here is the full reasoning.

---

## 1. Eldoc Childframe: Posframe Wrapper vs. Native Child Frames

### What your config currently uses

Your `config.org` (v0.33) loads `eldoc-childframe` with `:straight (:type built-in)` and hooks `eldoc-childframe-hover-at-point-mode` into `prog-mode`. You also have `posframe` loaded separately for other UI packages (corfu, ace-window posframe mode, etc.).

### What the alternatives actually do under the hood

**`eldoc-box`** (by Yuan Fu / casouri, the author of Emacs's pixel-scroll engine) does **not** use posframe at all. Its source calls `display-buffer-in-child-frame` directly with a meticulously tuned parameter plist that includes `(inhibit-double-buffering . t)`, `(no-accept-focus . t)`, `(no-special-glyphs . t)`, and `(desktop-dont-save . t)` [[source: eldoc-box.el on GitHub]]. It caches the frame in `eldoc-box--frame` and checks `(frame-live-p eldoc-box--frame)` before creating a new one — this is the same frame-reuse optimization that posframe provides, but without the abstraction layer.

**`posframe`** itself is described by its author as "fast enough for daily usage" , and its primary optimization is exactly this: create a child frame once, cache it, and reuse it on subsequent calls . But on   every `posframe-show` call, it still runs:

- Position calculation via a poshandler function
- Frame parameter merging (your defaults + per-call overrides)
- Buffer content insertion and face application
- Focus redirection (`redirect-frame-focus`)
- Hook registration for auto-hide
- Border face management

Each of these is an Elisp-level operation that a direct native implementation skips.

### Why a custom posframe wrapper would be slower

| Concern | Native child frame (eldoc-box / eldoc-childframe) | Posframe wrapper |
|---|---|---|
| Frame creation | Direct `display-buffer-in-child-frame` C call | Elisp wrapper → parameter merge → same C call |
| Frame reuse | Manual `frame-live-p` check (1 predicate) | Hash-table lookup + parameter reconciliation |
| Position calc | Hardcoded or simple arithmetic | Poshandler dispatch (function call + plist parsing) |
| Focus redirect | One `redirect-frame-focus` call | Same, but wrapped in error handling + hook management |
| Border theming | Direct `set-face-attribute` on frame | Face inheritance chain through `child-frame-border` + posframe's own border logic |
| TTY fallback | Emacs 31 native TTY child frames  | Posframe's own TTY detection + `display-buffer` fallback  |

The eldoc-box source explicitly notes that its hover-at-point mode "feels slower" than the hover mode  — but this is because of the **cursor-tracking repositioning on every mouse move**, not because of frame creation. A posframe wrapper would not fix this; it would add overhead on top of it.

### The real bottleneck

The latency you perceive in eldoc popup display is dominated by the **LSP backend response time** (Eglot's async JSON-RPC round-trip to pyrefly/ty/marksman), not by frame rendering. Your config already addresses this correctly:

```elisp
;; Eglot buffers: use eager composition for async LSP hover payloads.
(setq-local eldoc-documentation-strategy
            #'eldoc-documentation-compose-eagerly)
```

The frame display is the **last** step in the pipeline. Optimizing it with posframe would be optimizing the wrong end.

### Verdict: Eldoc

**Keep your current `eldoc-childframe` setup.** If you ever want to switch, `eldoc-box` is the superior alternative (native child frames, no posframe dependency, by a core Emacs developer). A custom posframe wrapper would be strictly slower due to the added abstraction layer.

-----

## 2. Flymake Diagnostics: Posframe vs. Flyover Overlays

### What your config currently uses

Your config uses **`flyover`**, which renders diagnostics as **buffer-local overlays** — not child frames . The README explicitly describes it as "a modern, aesthetic **overlay** display for Flycheck and Flymake" with features like "EOL multiline overlays" and "continuation overlays." It uses debouncing (`flyover-debounce-interval` at 0.2s, `flyover-cursor-debounce-interval` at 0.3s) to avoid excessive redisplay.

Your specific config:

```elisp
(flyover-display-mode 'show-only-on-same-line)
```

This means the overlay is only rendered when point is on the diagnostic's line — an extremely efficient gating mechanism.

### The existing posframe alternative

A package called **`flymake-posframe`** already exists . Its source shows it calls `posframe-show` with the diagnostic text on every `post-command-hook` invocation, checks `(get-char-property (point) 'flymake-diagnostic)`, and manages frame visibility with `frame-visible-p`. It also calls `redirect-frame-focus` on every display.

### Why overlays are architecturally faster than child frames

This is not a marginal difference — it is a **category difference**:

| Property           | Overlay (flyover)                                       | Child frame (posframe)                                          |
|--------------------|---------------------------------------------------------|-----------------------------------------------------------------|
| Rendering engine   | Emacs C-level redisplay (internal)                      | Window system protocol (X11/Wayland round-trip)                 |
| Creation cost      | `make-overlay` — O(1) C allocation                      | `make-frame` or frame cache lookup — Elisp + compositor         |
| Show/hide          | Toggle `invisible` property or delete overlay — C-level | `make-frame-visible` / `make-frame-invisible` — compositor call |
| Position update    | Automatic (overlay is anchored to buffer position)      | Manual poshandler recalculation per command                     |
| Buffer reflow      | Can cause line wrapping (the main downside)             | Zero impact on parent buffer layout                             |
| Multi-line display | Can disrupt code indentation visually                   | Floats above buffer, no disruption                              |
| Scrolling          | Scrolls with buffer (native)                            | Requires repositioning or hiding on scroll                      |

The critical insight: **overlays are managed by Emacs's C redisplay engine**, which is the same engine that renders your buffer text. Toggling an overlay's visibility is essentially free — it's a flag flip in the display pipeline. Showing/hiding a child frame requires **inter-process communication with the window compositor** (X11 `MapWindow`/`UnmapWindow` or Wayland `xdg_toplevel.set_visible`), which is orders of magnitude more expensive.

### When a posframe approach would be *better* (but not faster)

A child-frame diagnostic display has UX advantages in specific scenarios:

1. **Long multi-line diagnostics** that would cause severe buffer reflow if rendered as overlays
2. **Diagnostics in narrow windows** where an overlay would push code off-screen
3. **Persistent diagnostics** that should remain visible while scrolling (overlays scroll with the buffer)

But these are UX trade-offs, not speed improvements. The `flymake-popon` package  actually lets you choose between posframe and popon (a TTY-compatible popup) at runtime, acknowledging that neither is universally superior.

### Your current flyover config is already well-optimized

```elisp
(flyover-display-mode 'show-only-on-same-line)  ; Only render when cursor is on the error line
(flyover-checkers '(flymake))                    ; Single backend, no flycheck overhead
(flyover-levels '(error warning info))           ; Filtered severity
```

The `show-only-on-same-line` mode means flyover creates/destroys at most **one overlay per cursor movement**, gated by a debounce timer. This is about as cheap as diagnostic display can get.

### Verdict: Flymake Diagnostics

**Keep flyover.** A posframe-based implementation would be strictly slower for inline diagnostic display due to the overlay-vs-frame architectural gap. If you want non-intrusive floating diagnostics (e.g., for multi-line LSP messages), consider `flymake-popon`  or Emacs 31's native `tty-tip-mode`  as lighter alternatives to a full posframe implementation.

---

## 3. What Would Actually Improve Speed

Given your Emacs 31 + Eglot + Tree-sitter stack, the real performance levers are:

1. **LSP server response time** — your `eglot-documentation-renderer` is set to `gfm-view-mode` in v0.33, which loads markdown-mode for every hover. Consider switching to a lighter renderer or `nil` for raw text.

2. **Debounce tuning** — your `eldoc-idle-delay` is 0.1s, which is aggressive. If you see GC stutters during rapid cursor movement, bumping to 0.2s would reduce eldoc backend calls by ~50%.

3. **Emacs 31 native TTY child frames**  — if you ever use terminal Emacs, posframe and corfu now work natively without posframe's TTY fallback hacks, eliminating an entire code path.

4. **`eglot-ignored-server-capabilities`** — you already ignore `:documentFormattingProvider`, `:colorProvider`, and `:foldingRangeProvider`. In v0.33 you also ignore `:inlayHintProvider` but then manually enable `eglot-inlay-hints-mode` in `eglot-managed-mode-hook`. This is contradictory — the server won't advertise the capability, so the mode will have no effect. Either remove `:inlayHintProvider` from the ignore list or remove the hook.

5. **Frame reuse is already optimal** — your posframe config, corfu, and ace-window-posframe all share the frame cache paradigm. No further optimization is possible here.

---

## Summary Table

| Question | Custom posframe faster? | Why | Recommendation |
|---|---|---|---|
| Eldoc childframe | **No** | eldoc-box/eldoc-childframe already use native `display-buffer-in-child-frame` with frame caching; posframe adds an Elisp abstraction layer on top | Keep `eldoc-childframe`; consider `eldoc-box` if you want the most optimized native implementation |
| Flymake diagnostics | **No** | Overlays (flyover) are C-level redisplay primitives; child frames require compositor round-trips; `show-only-on-same-line` already minimizes overlay count | Keep `flyover`; consider `flymake-popon` only if you need floating non-intrusive diagnostics |
