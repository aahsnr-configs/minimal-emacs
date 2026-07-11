# Editor Architecture & Structural Editing Paradigm

This document serves as the definitive architectural guide for the **Editor Behaviour** main section. It encompasses both the massive v5 logical restructuring of the section (absorbing AST engines, code folding, terminal mechanics, and UI overlays) and the philosophical/technical implementation of the Non-Lisp Abstract Syntax Tree (AST) structural editing stack.

## Part I: The "Editor Behaviour" Taxonomy & Load-Order

By pulling the AST engine, code folding, terminal mechanics, and UI overlays into this section, `Editor Behaviour` acts as the centralized pillar for how Emacs renders, parses, and manipulates text and buffers.

### The 8 Logical Groups

**Group 1: Foundational UI, Chrome & Visual Baseline**
_Provides the visual baseline, glyph substitution, and UI APIs required by downstream packages._

1. Child Frame
2. Nerd Icons
3. Fonts
4. Doom Themes
5. Solaire Mode
6. Transient
7. Which-Key _(Migrated: Keystroke discovery UI overlay)_
8. Prettify Symbols _(Migrated & Simplified: Global glyph substitution)_

**Group 2: Core Buffer, Terminal & Editing Mechanics**
_Fundamental behaviors that dictate how text, interactive buffers, and the display operate._

1. Autorevert
2. Subword
3. Sudo edit
4. Buffer Terminator
5. Terminal _(Migrated: Eat/Vterm/Eshell core interactive buffer mechanics & ANSI rendering)_
6. Scrolling

**Group 3: The AST Engine & Non-Lisp Structural Stack**
_The C-level parsing foundation and the typing/wrapping layer._

1. Treesit _(Migrated: The foundational C-level AST engine. Must load before Group 3/4 dependents)_
2. elec-pair
3. evil-surround
4. delete-pair
5. show-paren-mode
6. blink-matching-paren
7. rainbow-delimiters
8. evil-matchit
9. evil-textobj-tree-sitter / evil-ts-obj
10. Native treesit-navigate-thing
11. expreg / treesit-expreg
12. tree-edit

**Group 4: Code Folding (AST & Textual)**
_Structural and textual collapsing mechanisms._

1. Treesit-Fold _(Migrated: AST-aware folding powered by Group 3's Treesit engine)
2. Vimish-Fold _(Migrated: Evil-compatible textual/regex folding)_

**Group 5: Whitespace, Indentation & Formatting**
_Packages that manage spatial alignment, trailing characters, and indentation guides._

1. Stripspace
2. Dirt Indent
3. Indent Bars

**Group 6: Visual Indicators & Window Chrome**
_Packages that render visual feedback, lines, and modelines._

1. Line Numbers
2. Display dividers between windows
3. hl-line
4. Rainbow Mode
5. Modeline
6. Hide Modeline
7. Text Scaling

**Group 7: Navigation, Search & Multi-cursor**
_Tools for moving through text, searching, and localized multi-cursor refactoring._

1. Avy
2. Anzu
3. iedit

**Group 8: Text Correction, Help & Complex Workflows**
_Spellchecking, documentation tools, and heavy multi-window tools._

1. Jinx
2. Helpful
3. Ediff

### Architectural Impact & Migrations

1. **The `Treesit` Load-Order Prerequisite**: Moving `treesit` to the top of Group 3 is an architectural necessity. It is the C-level engine that compiles and exposes the AST. Packages like `evil-textobj-tree-sitter`, `treesit-navigate-thing`, and `treesit-fold` will throw fatal `void-function` errors if the grammar engine is not initialized first.
2. **The Code Folding Pillar (Group 4)**: Extracting `treesit-fold` and `vimish-fold` creates a dedicated folding domain. `treesit-fold` handles semantic AST collapsing, while `vimish-fold` handles traditional Vim-style visual region folding (`zf`). Keeping them adjacent prevents keybinding and fringe marker conflicts.
3. **Terminal Mechanics (Group 2)**: Terminal emulators are fundamental interactive buffers requiring strict input-latency tuning and ANSI rendering safeguards. Placing them near `Scrolling` and `Autorevert` groups them with core redisplay and I/O mechanics.
4. **UI Overlays (Group 1)**: `Which-Key` belongs with `Transient` as a foundational UI overlay. `Prettify Symbols` is simplified to a global alist; language-specific prettification is delegated to Tree-sitter font-lock queries or LSP semantic tokens.

---

## Part II: The Non-Lisp Structural Editing Paradigm (The AST Stack)

### 1. The Paradigm Shift: S-Expressions vs. Abstract Syntax Trees (AST)

Because the Lisp family of languages is explicitly excluded from this workflow, the paradigm of parenthesis and bracket handling shifts fundamentally.

- **The Lisp Paradigm**: Code is a parenthesis tree. This necessitates aggressive structural enforcement tools like `smartparens-strict`, `paredit`, or `puni` to prevent un-evaluable unbalanced buffers.
- **The Non-Lisp Paradigm**: In Python, C++, Rust, JS, TS, and LaTeX, brackets are merely syntax delimiters. Code structure is defined by indentation, curly braces, or semantic tags.
- **The Regex Failure**: Regex-based pair trackers (like `smartparens`) inevitably lead to "pair desync" in complex string interpolations (e.g., Python f-strings, JS template literals).
- **The Irrelevance of Slurping/Barfing**: Lisp-centric operations offer diminishing returns in non-Lisp languages.
- **The Solution**: The Ultimate Non-Lisp Stack abandons regex-based pair tracking and relies entirely on Tree-sitter AST awareness and Native Emacs 30/31 C-level parsing.

### 2. The Doom Emacs Context: Why Heavy Structural Editors Were Dropped

- **The `evil-textobj-tree-sitter` API Fracture (Now Resolved)**: Prior to Emacs 29, the ecosystem relied on `elisp-tree-sitter` (`tsc`). When Emacs 29 introduced native `treesit.el`, hardcoded queries broke. The package has since been patched to support native `treesit` and is now mathematically safe for Emacs 30/31.
- **The `combobulate` Philosophical Friction (Permanent)**: `combobulate` introduces a heavyweight "node-hopping" paradigm via dedicated prefix keymaps that fundamentally clashes with Evil's Operator-Pending Text Objects (`vaf`, `cic`), causing massive keymap collisions.

### 3. Pillar 1: Insertion, Wrapping, and Deletion (The Typing Layer)

- **`electric-pair-mode` (Built-in)**: The definitive engine for forward-typing auto-pairing and region wrapping.
- **`evil-surround` (Third-Party)**: The undisputed champion for retro-active Vim-style wrapping (`ys`, `cs`, `ds`).
- **`delete-pair` (Built-in Emacs 28+)**: Intelligently deletes the parenthesis pair surrounding the cursor, regardless of nesting depth.

### 4. Pillar 2: Visual Feedback (The Perception Layer)

- **`show-paren-mode` (Built-in)**: Highlights matching brackets. Optimized with a custom ElDoc backend for off-screen context.
- **`blink-matching-paren` (Built-in)**: Optimized by setting `blink-matching-paren-distance` to `nil` for massive files.
- **`rainbow-delimiters` (Third-Party)**: Colorizes nested brackets based on depth.

### 5. Pillar 3: Navigation and Text Objects (The Movement Layer)

- **`evil-matchit` (Third-Party)**: Semantic block jumping via `%`.
- **`evil-textobj-tree-sitter` (Third-Party)**: Maps Vim text objects directly to the native Emacs `treesit` AST (e.g., `vaf`, `cia`, `vaC`).
- **Native Emacs 30 `treesit-navigate-thing`**: Native C-level AST node hopping, replacing Combobulate's prefix maps.

### 6. Pillar 4: Structural Manipulation (The Refactoring Layer)

- **`expreg` / `treesit-expreg`**: AST-aware visual selection expansion (word -> string -> expression -> block).
- **`evil-ts-obj` / `evil-ts`**: Advanced structural actions and "teleportation" of AST nodes using standard Vim bindings.
- **`tree-edit`**: Logic-programming deep refactoring and node extraction.

### 7. Summary Matrix

| Category           | Package / Feature                         | Purpose & Paradigm                                   |
| :----------------- | :---------------------------------------- | :--------------------------------------------------- |
| Insertion/Wrapping | `electric-pair-mode` + `evil-surround`    | Auto-pairing and Vim-style retro-active wrapping.    |
| Unwrapping         | `delete-pair` (Built-in)                  | Instant unwrapping of deeply nested structures.      |
| Visuals            | `show-paren-mode` + `rainbow-delimiters`  | Off-screen echo area context and depth colorization. |
| Boundary Jumping   | `evil-matchit` + `treesit-navigate-thing` | Semantic block jumping and native AST node hopping.  |
| AST Text Objects   | `evil-textobj-tree-sitter`                | AST-accurate Vim text objects (`vaf`, `cia`).        |
| Region Expansion   | `expreg` (with Tree-sitter)               | AST-aware visual selection expansion.                |
| Deep Refactoring   | `evil-ts-obj` / `tree-edit`               | Node swapping, teleportation, and AST extraction.    |

---

## Part III: Architectural Implementation Rules & Constraints

When implementing this stack in `config.org`, the following strict boundaries must be maintained:

1.  **No `combobulate`**: Fundamentally clashes with Evil's operator-pending grammar.
2.  **No `smartparens` or `puni`**: Regex-based pair tracking causes desyncs in complex strings/templates.
3.  **Isolate Refactoring Tools**: Bind AST manipulation commands to a dedicated Transient menu or leader prefix (e.g., `SPC c r`) to prevent accidental structural mutations during rapid typing.
4.  **Emacs 30/31 API Safety**: Any configuration utilizing native Emacs 30 AST functions must be wrapped in defensive runtime guards (e.g., `(when (fboundp 'treesit-navigate-thing) ...)`).

# Avy Stack

I have conducted a deep-dive analysis into the current Emacs spatial traversal ecosystem (up to July 2026), specifically focusing on how to complement **Avy** within a strict **Vanilla Emacs + Evil** workflow. I have also analyzed the specific visual friction you mentioned regarding Avy's overlays obscuring target characters.

Below is a detailed architectural breakdown of the packages, built-in features, and styling paradigms that solve your issues and complete the spatial navigation stack. **As per the strict greenlight protocol, zero configuration code is included.**

### 1. Solving the Avy Overlay "Blocking" Issue

The issue you identified—where Avy replaces characters or obscures the target letter when the jump key matches the underlying text—is a known visual friction point caused by Avy's default overlay rendering engine.

- **The `avy-style` Paradigm Shift:** By default, Avy often uses the `pre` or `at` styles, which inject a single character overlay directly on top of or immediately before the target . To solve the obscuration issue, modern configurations shift to **`at-full`** or **`words`**.
  - `at-full`: Instead of replacing a single character, Avy highlights the _entire word_ containing the target, and applies a distinct, high-contrast face to the specific jump character . This preserves the surrounding context and makes the target instantly readable.
  - `words`: Uses a dictionary of short, unique words (e.g., "the", "and", "for") as jump labels instead of single characters. This completely eliminates single-character overlap and is highly favored by touch-typists.
- **De Bruijn Sequences:** For users who prefer single characters, setting `avy-style` to **`de-bruijn`** generates mathematically optimal, non-overlapping character sequences . This ensures that the sequence of keys you type is unique and minimizes visual clutter.
- **Face Contrast & Background Dimming:** The obscuration issue is often mitigated not by changing the overlay text, but by changing the _background_. By enabling `avy-background` (which dims the rest of the buffer) and customizing `avy-lead-face-0`, `avy-lead-face-1`, and `avy-lead-face-2` with high-contrast, opaque background colors (e.g., bright neon blocks), the jump labels act as distinct "spotlights" rather than text replacements . The underlying code remains visible through the contrast, and the dimming effect forces the eye to focus strictly on the labeled targets.

### 2. The "Short-Distance vs. Long-Distance" Paradigm (The Leap.nvim Equivalent)

In the Neovim ecosystem, plugins like `leap.nvim` and `flash.nvim` have gained massive popularity by combining incremental search with spatial labels . In the Emacs/Evil ecosystem, this paradigm is best achieved by pairing **Avy** with **`evil-snipe`**.

- **`evil-snipe` (The Local Jumper):** Created by Henrik Lissner (the architect behind Doom Emacs), `evil-snipe` is the definitive complement to Avy . While Avy requires a global buffer scan and a modal "jump state," `evil-snipe` extends Evil's native `f`, `F`, `t`, and `T` motions to search for **2-character combinations** across the visible screen .
  - _Why it complements Avy:_ `evil-snipe` provides instant, low-latency jumps for local movement (e.g., jumping to the next `def` or `return` statement) without breaking your typing flow. You reserve Avy for long-distance jumps, complex targets, or cross-window navigation.
  - _Visual Synergy:_ `evil-snipe` highlights all matches inline with a subtle underline or background color, providing immediate visual feedback before you even commit to the jump, which is a feature `leap.nvim` users praise .
- **`avy-flash` / `key-leap` (The Modern Alternatives):** There have been recent community efforts to port `flash.nvim` directly to Emacs (e.g., `avy-flash`) , and packages like `key-leap` attempt to replicate the "leap to visible lines" mechanic . However, for a Vanilla Emacs + Evil stack, the combination of `consult-line` (for search-based jumping) + `evil-snipe` (for 2-char local jumping) + `avy` (for N-char global jumping) is mathematically superior and more stable than adopting niche ports.

### 3. Essential External Complementary Packages

To build a complete spatial traversal stack, the following packages are considered mandatory companions to Avy in modern configurations:

- **`ace-window`:** The definitive companion to Avy for window management. While Avy jumps the cursor _within_ a buffer, `ace-window` uses the exact same radix-tree algorithm to jump _between_ split windows or frames . It maps window labels to the home row, allowing you to switch focus to any visible window in 1-2 keystrokes, completely eliminating the need for `C-x o` or mouse clicks.
- **`ace-link`:** Emacs' native `*Help*`, `*Info*`, and `*Custom*` buffers are filled with unclickable text links. Navigating them with standard motions is painful. `ace-link` uses Avy-style overlays to let you jump to and open any visible link in 2 keystrokes . It integrates seamlessly with `evil-collection` and `helpful`.
- **`avy-zap` (or `zap-to-char` integration):** Native Emacs `zap-to-char` (`M-z`) blindly deletes text up to the _first_ occurrence of a character, often destroying code if the character appears multiple times on the line. `avy-zap-to-char-dwim` replaces this by dropping Avy labels on every instance of the target character, allowing you to visually select exactly which instance to delete up to .
- **`casual-avy` / `avy-menu`:** Avy has a massive API (`avy-copy-line`, `avy-move-line`, `avy-kill-region`, `avy-goto-char-timer`) that is difficult to memorize. `casual-avy` provides a Transient-based UI (a menu that appears in the echo area) to discover and execute these advanced Avy actions without memorizing dozens of distinct keybindings .

### 4. Built-in Emacs Synergies

You do not always need external packages to complement Avy; Emacs' native features offer deep integration:

- **`isearch` + Avy Hijack:** Avy can natively intercept `isearch`. When you are in the middle of a standard `C-s` incremental search, pressing your designated Avy key (often `C-'` or `M-j`) instantly drops Avy labels _only_ on the current search matches . This is significantly faster than `consult-line` for purely visual, in-buffer targeting because it requires zero context switching.
- **Native Emacs 30 `treesit-thing` Navigation:** As configured in your Group 3, `treesit-thing-next` and `treesit-thing-prev` provide O(1) AST sibling hopping. This replaces the need for Avy when jumping between functions, classes, or arguments, reserving Avy strictly for arbitrary string/character targeting where the AST is irrelevant.
- **`xref` and `lsp-mode` Jumps:** While Avy is for _visible_ text, `xref-find-definitions` (`g d`) and `xref-find-references` (`g D`) handle _semantic_ spatial traversal. These are already mapped in your `general.el` block and form the "backend" to Avy's "frontend" visual jumping.

### 5. Addressing Common Productivity Issues

Implementing this complementary stack solves several major productivity bottlenecks frequently cited by Emacs users:

- **The "Mouse Dependency" Trap:** Users frequently reach for the mouse to click on a distant line, a specific error in the compilation buffer, or a link in a help buffer. The combination of Avy + `ace-window` + `ace-link` completely eliminates the need for mouse input, keeping hands strictly on the home row and maintaining the "flow state" .
- **Context Switching in Read-Only Buffers:** Navigating Emacs' native documentation buffers is notoriously difficult with standard Vim motions. `ace-link` and `avy-goto-line` turn these read-only buffers into instantly navigable dashboards.
- **The "Which-Key" Ambiguity:** When using `avy-goto-char`, if there are multiple identical characters on screen, the user must type multiple keys. Using `avy-goto-char-2` (requiring a 2-character sequence) or `avy-goto-word-1` drastically reduces the "decision tree" depth, making jumps faster and less cognitively taxing .

### Summary of the Recommended Stack

To finalize the **Spatial Traversal & Inline Mutations (Group 7)** section, the architectural stack should consist of:

1.  **Avy** (Core engine, styled with `at-full` or `words` to prevent obscuration).
2.  **`evil-snipe`** (For 2-character local `f`/`t` extensions).
3.  **`ace-window`** (For cross-window jumping).
4.  **`ace-link`** (For Help/Org/Info link jumping).
5.  **`avy-zap`** (For visual `zap-to-char` replacement).

I am locked in and awaiting your signal to proceed with generating the `config.org` blocks for Avy and its companions.
