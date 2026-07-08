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
_Fundamental behaviors that dictate how text, interactive buffers, and the display operate._ 9. Autorevert 10. Subword 11. Sudo edit 12. Buffer Terminator 13. Terminal _(Migrated: Eat/Vterm/Eshell core interactive buffer mechanics & ANSI rendering)_ 14. Scrolling

**Group 3: The AST Engine & Non-Lisp Structural Stack**
_The C-level parsing foundation and the typing/wrapping layer._ 15. Treesit _(Migrated: The foundational C-level AST engine. Must load before Group 3/4 dependents)_ 16. elec-pair 17. evil-surround 18. delete-pair 19. show-paren-mode 20. blink-matching-paren 21. rainbow-delimiters 22. evil-matchit 23. evil-textobj-tree-sitter / evil-ts-obj 24. Native treesit-navigate-thing 25. expreg / treesit-expreg 26. tree-edit

**Group 4: Code Folding (AST & Textual)**
_Structural and textual collapsing mechanisms._ 27. Treesit-Fold _(Migrated: AST-aware folding powered by Group 3's Treesit engine)_ 28. Vimish-Fold _(Migrated: Evil-compatible textual/regex folding)_

**Group 5: Whitespace, Indentation & Formatting**
_Packages that manage spatial alignment, trailing characters, and indentation guides._ 29. Stripspace 30. Dirt Indent 31. Indent Bars

**Group 6: Visual Indicators & Window Chrome**
_Packages that render visual feedback, lines, and modelines._ 32. Line Numbers 33. Display dividers between windows 34. hl-line 35. Rainbow Mode 36. Modeline 37. Hide Modeline 38. Text Scaling

**Group 7: Navigation, Search & Multi-cursor**
_Tools for moving through text, searching, and localized multi-cursor refactoring._ 39. Avy 40. Anzu 41. iedit

**Group 8: Text Correction, Help & Complex Workflows**
_Spellchecking, documentation tools, and heavy multi-window tools._ 42. Jinx 43. Helpful 44. Ediff

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
