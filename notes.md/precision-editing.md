# Precision Editing (Group 7) - Architectural Blueprint

This document serves as the exhaustive, standalone architectural blueprint for the **Precision Editing** section (Group 7) of the Emacs configuration. It contains every technical decision, visual rendering mechanic, package selection, and workflow paradigm discussed and finalized up to July 2026.

This blueprint is designed to be completely self-contained. When you are ready to configure this section, this document will provide all necessary context, hex codes, load-order physics, and logical boundaries without requiring reference to prior chat history.

---

## 1. Core Philosophy & Paradigm Shifts

The "Precision Editing" section governs spatial traversal, search tracking, and localized/bulk text mutations. The architecture is built upon three strict paradigm shifts:

1.  **Short-Distance vs. Long-Distance Traversal:** Local, flow-state jumps (1-2 lines) are handled by inline, non-modal engines (`evil-snipe`). Long-distance, cross-buffer, or complex jumps are handled by modal, screen-dimming engines (`avy` via `evil-easymotion`).
2.  **Lexical vs. Semantic Mutations:** Text-search tools (`anzu`, `iedit`) operate strictly on the _lexical_ plane (exact string/regex matching). Language Server Protocol (`lsp-mode`) operates on the _semantic_ AST plane. **They must remain isolated.** `anzu` is never integrated with LSP; `lsp-rename` is used for semantic refactoring, while `iedit`/`wgrep` are used for lexical bulk edits.
3.  **The "Opaque Spotlight" Visual Paradigm:** Avy's default overlay rendering causes "ghosting" (where the jump character blends with the underlying syntax-highlighted code). This is solved not by changing the jump algorithm, but by applying 100% opaque background faces that act as physical "neon tape" over the target character, combined with full-buffer dimming.

---

## 2. The Avy Core Engine (Visuals & Mathematics)

Avy is the foundational C-level radix-tree jumping engine. Its configuration is split into two orthogonal axes: **Visual Rendering** and **Path Generation**.

### A. Visual Rendering: The `at-full` Paradigm

- **The Decision:** `avy-style` is strictly set to `'at-full`.
- **The Rejection:** The `de-bruijn` style was explicitly rejected. While `de-bruijn` generates mathematically optimal sequences, Avy's source code hardcodes the `de-bruijn` algorithm to force the `'at` visual style (which replaces the single character and destroys peripheral word context). It does not support `'at-full`.
- **The Mechanics of `at-full`:**
  1.  The _entire target word_ receives a subtle, dark background wash to preserve peripheral context.
  2.  The _first letter_ of the target is eclipsed by a solid, 100% opaque rectangular box containing the jump character.
  3.  The rest of the word remains fully visible.
- **The "Opaque Spotlight" Faces:** To prevent the "ghosting/mixture" artifact where the overlay character blends with the underlying code, the `avy-lead-face` properties must use 100% opaque Tokyo Night background colors with the deep Tokyo Night background as the foreground.
  - `avy-lead-face-0` (Primary): Background `#f7768e` (Red), Foreground `#1a1b26`.
  - `avy-lead-face-1` (Secondary): Background `#e0af68` (Yellow), Foreground `#1a1b26`.
  - `avy-lead-face-2` (Tertiary): Background `#9ece6a` (Green), Foreground `#1a1b26`.
- **Buffer Dimming:** `avy-background` is set to `t`. This dims the rest of the buffer to a washed-out gray, forcing the eye strictly to the opaque neon spotlights.

### B. Path Generation: The Doom Emacs Home-Row Secret

- **The Decision:** Instead of using the `de-bruijn` algorithm, we use Avy's native Standard Shortest-Path Algorithm but restrict the `avy-keys` alphabet strictly to the home row.
- **The Configuration:** `(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))`
- **The Mechanics:** Because the alphabet is restricted to 9 home-row keys, 99% of jumps will require exactly one keystroke. The algorithm naturally optimizes for these keys.
- **The Overflow Mechanic (Targets > 9):** If there are 12 targets on the screen, Avy expands to the second depth level.
  - Targets 1-9 get single characters (`a`, `s`, `d`...).
  - Targets 10-12 get 2-character sequences (`aa`, `as`, `ad`).
  - _Visual Transition:_ Avy initially shows the _first_ character for all targets. When you press `a`, targets 2-9 vanish, and targets 10-12 dynamically update their opaque boxes to show their _second_ character (`a`, `s`, `d`). This keeps visual noise to an absolute minimum.

---

## 3. The Spatial Traversal Stack (Movement Layer)

### A. `evil-easymotion` (The Grammar Wrapper)

- **Purpose:** Raw Avy is mode-agnostic. `evil-easymotion` wraps Avy into Evil's operator-pending grammar.
- **Mechanics:** Maps Avy jumps to a unified `g s` (Goto Spatial) prefix using standard Vim motions (e.g., `g s w` for word, `g s j` for line down).
- **Operator-Pending Synergy:** Allows mutations like `d g s j` (Delete -> Spatial -> Line Down) to delete everything from the cursor to the jumped line.
- **Load-Order Physics:** Must be loaded _after_ `avy` so it inherits the custom `at-full` faces and `avy-keys`.

### B. `evil-snipe` (The Local Inline Jumper)

- **Purpose:** Replaces Avy for short-distance, 2-character jumps to preserve typing flow state.
- **Mechanics:** Hijacks Evil's native `f`, `F`, `t`, `T` motions. Typing `f r e` instantly highlights all `re` sequences on the visible screen.
- **Visuals:** Does **not** dim the screen. Draws a subtle Tokyo Night cyan (`#7dcfff`) underline beneath matches.
- **The `zap-to-char` Killer:** Natively hijacks `z` and `Z`. Pressing `z d e` highlights all `de` occurrences and deletes up to the selected one, completely replacing the obsolete `avy-zap` package.

### C. `link-hint` (The Link Jumper)

- **Purpose:** Replaces the aging `ace-link`. Provides Evil-native link jumping for read-only buffers (`*Help*`, `*Info*`, Org, EWW).
- **Mechanics:** Uses the Avy engine but specifically targets URLs and buttons. Places the opaque neon box _strictly on the first character_ of the link; the rest of the link text remains readable.

### D. `ace-window` (Cross-Window Jumper)

- **Purpose:** Uses the Avy radix-tree math to jump between split windows.
- **Routing:** Strictly routed through `general.el` leader bindings (e.g., `SPC w w`) to prevent it from polluting Evil's native keymaps. Reserved for 4+ window splits where spatial memory fails.

---

## 4. Search, Tracking & Validation (Perception Layer)

### A. `anzu` + `evil-anzu` (Search Tracking)

- **Purpose:** Displays current match index and total matches (e.g., `[3/15]`) in the modeline during `isearch` and Evil `/` searches.
- **Boundary:** Strictly isolated from LSP. It is a lexical text-search tracker.

### B. `avy-isearch` (Seek, then Jump)

- **Purpose:** Built-in Avy synergy. Allows hijacking an active `C-s` isearch session to drop Avy labels _only_ on the current search matches.
- **Routing:** Mapped to a mnemonic like `M-j` (Meta-Jump) to avoid awkward default bindings like `C-'`.

### C. `re-builder` (Built-in Regex Builder)

- **Purpose:** Interactive visual regex builder. Opens a split window to highlight matches and capture groups in real-time before feeding the regex into `anzu-query-replace-regexp` or `iedit-regexp`.

---

## 5. Inline & Bulk Mutations (Execution Layer)

### A. `iedit` + `evil-multiedit` (Local Multi-Cursor)

- **Purpose:** Localized, AST-aware multi-cursor refactoring.
- **Visuals:** Drops a Tokyo Night purple (`#bb9af7`) background highlight over matched instances.
- **Mnemonics:** Replaces the non-Vim-standard `M-d` with explicit Vim-mnemonics:
  - `m d` (Multi-cursor match and move **d**own/next): Pressed repeatedly to accumulate cursors.
  - `m a` (Multi-cursor match **a**ll): Pressed once to select all instances in the buffer/region.

### B. `wgrep` & Emacs 31 `grep-edit-mode` (Cross-Project Bulk Editing)

- **Purpose:** Makes read-only `*grep*` and `*occur*` buffers writable. Allows using `iedit` or Evil operators directly inside search results to mutate matches across 50+ files simultaneously.
- **Emacs 31 Defensive Guard:** Emacs 31 introduces `grep-edit-mode` natively. The configuration must use a defensive runtime guard `(when (fboundp 'grep-edit-mode) ...)` to use the native C-level engine in Emacs 31, falling back to the external `wgrep` package for Emacs 30.

### C. Custom Transient Menu (Avy Mutations)

- **Purpose:** Replaces `casual-avy`. Exposes Avy's powerful but hard-to-memorize mutation APIs (`avy-copy-line`, `avy-move-line`, `avy-kill-region`) under a dedicated Transient menu (e.g., `SPC c a`) to prevent global keymap pollution.

---

## 6. Architectural Boundaries & Rejections

The following packages and patterns were explicitly evaluated and **rejected** for this configuration:

- **`avy-zap`:** Rejected. Obsolete, decade-old package. `evil-snipe` natively handles visual `z/Z` zapping.
- **`casual-avy`:** Rejected. Uses generic transient implementations that clash with the established `ar/...-transient` formatting and Nerd Icons headers. Replaced by a Custom Transient Menu.
- **`ace-link`:** Rejected. Old, lacks deep Evil state awareness. Replaced by `link-hint`.
- **`de-bruijn` (Avy Style):** Rejected. Forces the `at` visual style, overriding the required `at-full` word-highlighting engine.
- **Anzu + LSP Integration:** Rejected. They operate on orthogonal planes. Keep them isolated.
- **`avy-flash` / `key-leap`:** Rejected. Niche ports of Neovim plugins. The combination of `consult-line` + `evil-snipe` + `avy` is mathematically superior and more stable in Vanilla Emacs.

---

## 7. The 9-Step Visual Workflow (UX Benchmark)

This workflow serves as the definitive UX benchmark to verify that the configuration is functioning correctly once implemented.

1.  **Local Inline Jump (`evil-snipe`):** Normal state. Type `f r e`. Screen does _not_ dim. Cyan underline appears under all `re` sequences. Press `;` to cycle to the target.
2.  **Search & Track (`anzu`):** Normal state. Press `/`, type `user_auth`. Modeline lights up with `[3/15]`. Press `n` to cycle; modeline updates to `[4/15]`.
3.  **Multi-Cursor (`evil-multiedit`):** Normal state. Cursor on word. Press `m d` three times. Purple highlights appear on 4 instances. Type `c w`, rename, press `<ESC>`. All update simultaneously.
4.  **Seek, then Jump (`avy-isearch`):** Normal state. Press `C-s`, type `ERROR`. Press `M-j`. Screen dims. Opaque red boxes appear _only_ on `ERROR` matches. Press home-row key to teleport.
5.  **Long-Distance Modal Jump (`evil-easymotion`):** Normal state. Type `g s w`, then `l o`. Screen dims. Opaque red boxes appear on words starting with `lo`. The target word gets a subtle background wash, but the first letter is eclipsed by the red box. Press home-row key to teleport.
6.  **Operator-Pending Mutation (`evil-easymotion`):** Normal state. Type `d g s j`, then `c l`. Screen dims. Green boxes appear at column 0 of every line. Press the target key. Avy jumps, and Evil automatically deletes the region between start and target.
7.  **Advanced Mutation Dashboard (Custom Transient):** Normal state. Press `SPC c a`. Echo area displays Nerd Icons header and Avy mutation grid. Press `m` (Move Line). Screen dims, select target line. Line teleports.
8.  **Cross-Window Jump (`ace-window`):** Normal state. Press `SPC w w`. Massive yellow opaque blocks appear in the top-left corner of all split windows. Press target key to shift focus.
9.  **Link Jump (`link-hint`):** Normal state in `*Help*` buffer. Press `SPC l o`. Opaque blocks appear strictly on the first character of clickable links. Press target key to open link.

---

## 8. Deferred Decisions

- **Keybinding Scheme:** The exact keybinding scheme (Doom Emacs vs. Spacemacs style) is **deferred**. The routing concepts (e.g., `g s` for easymotion, `m d` for multiedit, `M-j` for isearch-jump) are locked in, but the actual `general.el` implementation will be decided when the rest of the configuration's leader-key real estate is finalized.
- **Header Status:** The section header is locked as `** TODO Precision Editing`. It will not be marked `DONE` until the code is fully written and tested.

---

## 9. Finalized Software Manifest

When you give the signal to proceed, the following packages will be implemented in isolated `#+begin_src` blocks under the `** TODO Precision Editing` header:

1.  `avy` (Core engine)
2.  `evil-easymotion` (Grammar wrapper)
3.  `evil-snipe` (Local inline jumper)
4.  `link-hint` (Link jumper)
5.  `ace-window` (Cross-window jumper)
6.  `anzu` + `evil-anzu` (Search tracking)
7.  `iedit` + `evil-multiedit` (Multi-cursor)
8.  `wgrep` (Writable grep)
9.  `grep-edit-mode` (Emacs 31 native writable grep, defensively guarded)
10. `re-builder` (Built-in regex builder)
11. Custom Transient Menu definitions (Avy mutations)

_End of Blueprint. Awaiting explicit greenlight to generate code._
