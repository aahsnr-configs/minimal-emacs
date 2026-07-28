# Deep Audit: eldoc-childframe.el, peek.el & markdown-ts-mode Integration

## 1. Does eldoc-childframe.el Tightly Integrate with markdown-ts-mode?

**No. There is zero direct coupling between eldoc-childframe.el and markdown-ts-mode.**

After analyzing both sources exhaustively:

### The Actual Integration Path (Indirect, via Eldoc)

The connection is a **three-hop indirect pipeline**, not a tight integration:

```
Eglot (LSP hover) → Eldoc display pipeline → eldoc-childframe (renderer)
                                                    ↑
markdown-ts-view-mode ←── eglot-documentation-renderer ←── Eglot
```

From your `config.org`:

```elisp
(eglot-documentation-renderer 'markdown-ts-view-mode)
```

This tells Eglot to render LSP hover payloads _using_ `markdown-ts-view-mode` as the rendering engine . The markdown-ts-mode source confirms it's designed for this:

> "markdown-ts-mode is ready for Emacs 31. It is already useful for reading and basic editing, and might provide, via Eglot and Eldoc, out of the box"

### What eldoc-childframe Actually Sees

eldoc-childframe.el v1.5.0 intercepts at the **Eldoc display layer** only:

```elisp
(setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
```

It receives already-composed documentation strings. It has:

- **No** `require` of `markdown-ts-mode`
- **No** reference to any `markdown-ts-*` face, variable, or function
- **No** awareness of whether the payload was rendered by markdown-ts-view-mode, gfm-view-mode, or plain text

### The One Subtle Interaction Point

markdown-ts-mode's `markdown-ts--fontify-delimiter` function contains this comment:

```elisp
;; In view-mode only, hide the whole line containing the fence
;; (including its terminating newline) so Eldoc/LSP markdown
;; snippets render without stray blank lines around the code block.
```

This means markdown-ts-view-mode **tunes its own rendering** for Eldoc consumption, but eldoc-childframe is completely unaware of this. The integration is **unidirectional**: markdown-ts-mode optimizes _for_ Eldoc; eldoc-childframe consumes _from_ Eldoc.

---

## 2. Deep Audit: eldoc-childframe.el (v1.5.0 in source)

### Architecture Overview

A keybinding-only childframe documentation viewer that replaces the Eldoc echo-area/buffer display pipeline with a floating popup.

### Critical Findings

#### ✅ Strengths

| Aspect                        | Assessment                                                                                                                          |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| **Flymake Origin Firewall**   | Correctly inspects `:origin` plist to block diagnostic payloads — this is the proper Emacs 31 `flymake-make-diagnostic` API pattern |
| **Spatial Debounce**          | Replaces the 0.5s `eldoc-pre-command-refresh-echo-area` inhibition with point-equality checks — eliminates the "cursor trap"        |
| **Buffer-Change Auto-Hide**   | `eldoc-childframe--hide-on-buffer-change` correctly handles workspace switching (critical for your bufferlo setup)                  |
| **TTY Degradation**           | Falls back to `eldoc-display-in-echo-area` on non-graphical frames — respects Emacs 31's `tty-child-frames` feature                 |
| **Corfu Collision Avoidance** | Checks `corfu--frame` visibility and offsets X position — prevents popup overlap                                                    |

#### ⚠️ Issues & Emacs 31 Best Practice Violations

**Issue 1: `eldoc-display-functions` Replacement is Too Aggressive**

```elisp
(setq-local eldoc-display-functions (list #'eldoc-childframe--route-display))
```

This **completely replaces** the display function list. In Emacs 31, `eldoc-display-functions` is designed as a **multi-source composition pipeline** . Your config also sets:

```elisp
(setq-local eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
```

The eager strategy calls _all_ documentation functions and passes results to _all_ display functions. By replacing the list with a single entry, you lose:

- The ability for `eldoc-display-in-echo-area` to show brief hints while the childframe shows full docs
- Future Emacs 31 display functions (e.g., the new `eldoc-help-at-pt` integration you've enabled via `setopt`)

**Emacs 31 Best Practice**: Use `add-to-list` with position control, or better, use the `:around` advice pattern on the existing display function.

**Issue 2: `eldoc-childframe--compose-doc` Duplicates Internal API**

```elisp
(defun eldoc-childframe--compose-doc (doc)
  (let ((thing (plist-get (cdr doc) :thing))
        (face (plist-get (cdr doc) :face)))
    ...))
```

This manually reconstructs what `eldoc--format-doc-buffer` already does. In Emacs 31, the internal `eldoc--format-doc-buffer` is stable and handles:

- Multiple doc composition
- `:thing` face application
- Truncation per `eldoc-echo-area-use-multiline-p`

**Issue 3: Missing `eldoc-documentation-compose-eagerly` Awareness**

When `eldoc-documentation-strategy` is `eldoc-documentation-compose-eagerly` (as in your Eglot buffers), the DOCS argument to display functions is a **list of (STRING . PLIST) pairs**. The router handles this correctly with `mapcar`, but the TTY fallback path calls:

```elisp
(eldoc-display-in-echo-area filtered interactive)
```

This is correct, but `filtered` may contain items whose `:origin` was flymake — the filter runs _before_ the branch, so this is actually fine. ✅

**Issue 4: Frame Parameter `no-accept-focus` Toggle is Fragile**

```elisp
(defun eldoc-childframe-focus-frame ()
  (set-frame-parameter eldoc-childframe--frame 'no-accept-focus nil)
  (set-frame-parameter eldoc-childframe--frame 'no-focus-on-map nil)
  ...)
```

Once focus is granted, it's **never restored**. If the user focuses the childframe, then quits it, the next spawn inherits `no-accept-focus = nil` from the stale frame object. The frame should be deleted and recreated, or parameters reset in `eldoc-childframe--get-frame`.

**Issue 5: `make-separator-line` Not Used**

The Markdown separator prettifier manually applies `strike-through` and `height 0.4`:

```elisp
(add-text-properties beg end '(face eldoc-childframe-markdown-separator))
```

Emacs 31's `make-separator-line` (which your peek.el correctly uses) produces a proper `:extend t` separator. The childframe could leverage this for consistent rendering.

**Issue 6: No `window-buffer-change-functions` Cleanup**

Unlike peek.el v0.3.0 which correctly uses `window-buffer-change-functions` and `kill-buffer-hook` for cleanup, eldoc-childframe has **no frame cleanup on buffer kill**. If the source buffer is killed while the childframe is visible, the childframe persists showing stale content until the next `post-command-hook` fires.

---

## 3. Deep Audit: peek.el (v0.3.0 in source)

### Architecture Overview

An overlay-based "peek view" that renders content (strings, xref definitions, eldoc docs) in a virtual window above/below point using `after-string` overlay properties.

### Critical Findings

#### ✅ Strengths (v0.3.0 Fixes)

| Fix                                  | Assessment                                                                                                                    |
| ------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| **`save-window-excursion`**          | Correctly replaces `save-excursion` — prevents `switch-to-buffer` inside xref functions from mutating the user's window state |
| **`make-separator-line` zero-arg**   | Correctly avoids the `wrong-type-argument integerp t` crash that passing `t` causes in Emacs 31                               |
| **`window-buffer-change-functions`** | Replaces the O(B×W) per-scroll `window-state-change-functions` with O(W_local) per-switch cleanup                             |
| **Eldoc Origin Firewall**            | Blocks flymake payloads via `:origin` plist inspection                                                                        |
| **Direct xref backend bypass**       | Calls `xref-backend-definitions` directly, preventing `xref-marker-stack` pollution                                           |

#### ⚠️ Issues & Emacs 31 Best Practice Violations

**Issue 1: `after-string` Overlay Cannot Be Cursor-Intangible**

The v0.3.0 changelog correctly notes:

> "Excised `cursor-intangible` text property injection inside `after-string`. The C redisplay engine ignores text properties inside virtual overlay strings for cursor intangibility."

This is correct. However, the **consequence** is that the cursor can visually "enter" the peek overlay region during vertical motion (`C-n`/`C-p`). In Emacs 31, the proper solution is:

```elisp
(overlay-put ol 'cursor-sensor-functions
             (list (lambda (_win _prev _dir)
                     ;; Push point out of the overlay region
                     ...)))
```

Or use the new Emacs 31 `cursor-sensor-functions` stickiness behavior (from NEWS.31: "Boundaries of `cursor-sensor-functions` now obey stickiness").

**Issue 2: `peek-display--overlay-update` on Every `post-command-hook`**

```elisp
(add-hook 'post-command-hook #'peek-display--overlay-update)
```

This fires on **every single command**, including self-insert, scrolling, and mouse events. The function checks `(overlay-get ol 'active)` and recalculates position. For a config with pixel-scroll-precision and evil-snipe incremental highlighting, this adds a function call to every keystroke.

**Emacs 31 Best Practice**: Use `jit-lock-after-change-extend-region-functions` or a more targeted hook. At minimum, add a fast-path bail:

```elisp
(defun peek-display--overlay-update (&optional ol)
  (when-let ((ol (or (and (overlayp ol) ol)
                     (peek-get-window-overlay)))
             ((overlay-get ol 'active)))
    ;; Only reposition if the window's point actually moved
    (unless (eq (point) (overlay-get ol 'peek--last-point))
      ...)))
```

**Issue 3: `peek--definition-func` Uses Dynamic Binding**

```elisp
(defvar peek--definition-func nil)
(defvar peek--definition-func-args nil)
```

These are `defvar` (dynamic) but used in a `let` binding inside `peek-definition`:

```elisp
(let ((peek--definition-func func)
      (peek--definition-func-args args))
  ...)
```

Since peek.el declares `lexical-binding: t`, this `let` creates a **lexical** binding that shadows the dynamic `defvar`. This works, but it's semantically confusing. The variables should either be:

- Lexical (`defvar-local` or just `let`-bound without `defvar`), or
- Truly dynamic (used across function boundaries that expect dynamic scope)

In this case, they're only used within `peek-definition` → `peek-overlay-auto-set-content` → `peek-definition--set-marker`, all within the same call stack. The `defvar` is unnecessary.

**Issue 4: No `too-long-file-p` Guard**

Your config defines `too-long-file-p` as a performance guard. peek.el's `peek-definition--get-surrounding-text` calls `font-lock-ensure` on a region:

```elisp
(font-lock-ensure p1 p2)
```

In a massive minified JSON buffer (which your config guards against in 15+ other packages), this could trigger a full tree-sitter reparse. peek.el should respect a buffer-size guard.

**Issue 5: `peek-overlay--format-content` Allocates on Every Update**

Every call to `peek-overlay-auto-set-content` → `peek-overlay--set-content` → `peek-overlay--format-content` creates:

- Two `make-separator-line` strings
- A `copy-sequence` for face application
- A `concat` of three strings

For live-update mode (`peek-live-update t`), this fires on every `after-change-functions` call. In Emacs 31, consider caching the formatted string and only rebuilding when `peek-lines` or `peek-offset` actually change.

**Issue 6: Missing `display-line-numbers` Margin Compensation**

peek.el requires `display-line-numbers` at the top:

```elisp
(require 'display-line-numbers)
```

But the overlay position calculation (`peek-overlay--get-supposed-position`) uses `forward-line` which doesn't account for the line-number margin width. The `after-string` will render at the text column, not accounting for the margin. This is cosmetically correct (the overlay appears in the text area), but the `peek-overlay-distance` of 2 lines may visually collide with the line numbers if the peek window is narrow.

---

## 4. Cross-Package Interaction Analysis

### eldoc-childframe ↔ peek.el Conflict

Both packages can be active simultaneously in `prog-mode` buffers:

- **eldoc-childframe**: Hooks `prog-mode` via `eldoc-childframe-hover-at-point-mode`
- **peek.el**: Hooks `eldoc-display-functions` via `peek-display-eldoc` (when `peek-enable-eldoc-display-integration` is non-nil)

Your config sets:

```elisp
(peek-enable-eldoc-display-integration nil)  ;; Not explicitly set, defaults to nil
```

So peek's Eldoc integration is **disabled** in your config. ✅ No conflict.

However, if both were active, they would **fight** over `eldoc-display-functions`:

- eldoc-childframe **replaces** the list with `(list #'eldoc-childframe--route-display)`
- peek **adds** `#'peek-display-eldoc` to the list

The replacement would silently remove peek's entry. This is a design flaw in eldoc-childframe's approach.

### eldoc-childframe ↔ markdown-ts-view-mode Rendering

When Eglot renders hover docs via `markdown-ts-view-mode`, the resulting string contains:

- `invisible` text properties (from `markdown-ts--fontify-delimiter` with `markdown-ts-hide-markup t`)
- `display` properties (from hidden markup replacements)
- `button` properties (from `markdown-ts--make-link-button`)

eldoc-childframe's `eldoc-childframe--display` inserts this string into its buffer and runs `eldoc-childframe-buffer-hook`. The `visual-line-mode 1` call is correct for wrapping, but:

- The `invisible` properties from markdown-ts-view-mode will **hide content** in the childframe buffer
- The `button` properties will be **non-functional** (no `button-mode` enabled in the childframe buffer)

**Recommendation**: Add `(button-mode 1)` to the childframe buffer setup, and strip `invisible` properties:

```elisp
(remove-text-properties (point-min) (point-max) '(invisible nil))
```

---

## 5. Emacs 31 Specific Recommendations

### For eldoc-childframe v2.0.0

1. **Use `eldoc-help-at-pt` integration** (new in Emacs 31):

   ```elisp
   ;; Your config already does: (setopt eldoc-help-at-pt t)
   ;; eldoc-childframe should respect this by showing keybinding hints
   ;; in the childframe when the symbol has a keybinding.
   ```

2. **Respect `eldoc-echo-area-prefer-doc-buffer`**: Your config sets this to `'maybe`. eldoc-childframe should check this and defer to the `*eldoc*` buffer for very long docs instead of creating a massive childframe.

3. **Use `child-frame-border` face inheritance** (Emacs 28+): Already done ✅

4. **Leverage `tty-child-frames`** (Emacs 31): Your `childframe-workable-p` already checks `(featurep 'tty-child-frames)` ✅

### For peek.el v0.4.0

1. **Use `treesit-thing-at` for definition context**: Instead of raw `thing-at-point 'symbol`, use the tree-sitter thing API for more precise symbol extraction in ts-modes.

2. **Respect `treesit-font-lock-level`**: When calling `font-lock-ensure` in `peek-definition--get-surrounding-text`, the fontification quality depends on this setting. Your config sets it to 4 (maximum) ✅

3. **Use `window-cursor-info`** (new in Emacs 31): For precise pixel-level positioning of the peek overlay relative to the cursor, instead of the current `pos-visible-in-window-p` approach.

---

## 6. Summary Verdict

| Package              | Integration with markdown-ts-mode     | Emacs 31 Readiness     | Critical Issues                                     |
| -------------------- | ------------------------------------- | ---------------------- | --------------------------------------------------- |
| **eldoc-childframe** | None (indirect via Eldoc pipeline)    | Good, minor gaps       | Frame focus leak; aggressive display-fn replacement |
| **peek.el**          | None (xref/eldoc only)                | Good post-v0.3.0 fixes | post-command-hook overhead; no buffer-size guard    |
| **markdown-ts-mode** | N/A (it's the renderer, not consumer) | Excellent (built-in)   | N/A                                                 |

The three packages form a **clean separation of concerns**: markdown-ts-mode renders, Eldoc routes, and eldoc-childframe/peek display. There is no tight coupling, which is architecturally correct. The only risk is the Eldoc display-function list being a shared mutable resource that both eldoc-childframe and peek could fight over if both Eldoc integrations are enabled simultaneously.
