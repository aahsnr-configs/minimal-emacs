# The Ultimate Non-Lisp Bracket, Navigation & Structural Editing Stack in Emacs

## 1. The Paradigm Shift: S-Expressions vs. Abstract Syntax Trees (AST)

Because the Lisp family of languages is explicitly excluded from this workflow, the paradigm of parenthesis and bracket handling shifts fundamentally.

### The Lisp Paradigm (S-Expressions)

In Lisp, code _is_ a parenthesis tree. The syntax and the data structure are identical. This necessitates heavy, aggressive structural enforcement tools like `smartparens-strict`, `paredit`, or `puni`. These tools act as aggressive gatekeepers, physically preventing the user from deleting a single bracket if it would leave the buffer syntactically unbalanced, because an unbalanced Lisp buffer is completely un-evaluable.

### The Non-Lisp Paradigm (AST & Delimiters)

In languages like Python, C++, Rust, JavaScript, TypeScript, and LaTeX, brackets and parentheses are merely syntax delimiters for blocks, arrays, and function arguments. Code structure is defined by indentation (Python), curly braces (C/Rust/JS), or semantic tags (LaTeX).

- **The Regex Failure:** Relying on regex-based pair trackers (like `smartparens`) in these languages inevitably leads to "pair desync". When brackets appear inside complex string interpolations (e.g., Python f-strings, JS template literals), nested quotes, or unusual comment syntax, the regex parser misinterprets a string delimiter as a structural boundary. This causes the engine to falsely believe the buffer is unbalanced, subsequently blocking deletions or wrapping operations.
- **The Irrelevance of Slurping/Barfing:** Lisp-centric structural editors rely heavily on "slurping" (pulling the next word into the current parenthesis) and "barfing" (pushing the last word out). In non-Lisp languages, these operations offer diminishing returns because code blocks are rarely defined by deeply nested S-expressions.

**The Ultimate Non-Lisp Stack** abandons regex-based pair tracking and Lisp-centric manipulation entirely. Instead, it relies on **Tree-sitter AST (Abstract Syntax Tree) awareness** and **Native Emacs 30/31 C-level parsing**. Emacs natively parses the code into a mathematical tree, allowing the editor to understand the exact semantic difference between a parenthesis inside a string literal and a parenthesis defining a function signature.

---

## 2. The Doom Emacs Context: Why Heavy Structural Editors Were Dropped

Historically, Doom Emacs included `combobulate` and `evil-textobj-tree-sitter` in its tree-sitter module, but both were commented out or removed during the Emacs 29/30 migration cycles. Understanding the architectural friction that caused this provides crucial context for building a Vanilla Emacs configuration.

### The `evil-textobj-tree-sitter` API Fracture (Now Resolved)

Prior to Emacs 29, the ecosystem relied on a third-party package called `elisp-tree-sitter` (and its C-binding library `tsc`). `evil-textobj-tree-sitter` was built entirely on this legacy API.

- **The Breakage:** When Emacs 29 introduced native, C-level Tree-sitter support via the built-in `treesit.el` library, it completely deprecated the third-party `tsc` library. As upstream language grammars updated, the hardcoded queries inside `evil-textobj-tree-sitter` broke, throwing fatal `tsc-make-query: Query contains invalid node type` errors. Doom removed it during this volatile transition.
- **Current State:** The package has since been patched to explicitly support native `treesit` (verified via upstream Issue #109 and test suite branching). Modern native forks like `evil-ts-obj` also exist. It is now mathematically safe, robust, and the definitive choice for Emacs 30/31.

### The `combobulate` Philosophical Friction (Permanent)

`combobulate` is widely considered the most powerful structural editing engine in Emacs. However, it was commented out of Doom's default configuration due to severe philosophical friction with Evil (Vim emulation).

- **Paradigm Clash:** Vim relies on **Operator-Pending Text Objects** (e.g., `vaf` to visually select "around function", `cic` to change "inner class"). `combobulate` does not natively map itself to Vim's `a` (around) and `i` (inner) grammar. Instead, it introduces its own heavyweight paradigm of "node-hopping" and manipulation via a dedicated prefix keymap (e.g., `C-c o n`).
- **Keymap Collisions:** Forcing `combobulate` onto an Evil user causes massive keybinding collisions. It attempts to manage cursor placement, region highlighting, and structural boundaries in ways that frequently hijack or conflict with Evil's visual state and operator-pending state machine. Doom removed it because it violates the cohesive, zero-config Vim experience mandate.

---

## 3. Pillar 1: Insertion, Wrapping, and Deletion (The Typing Layer)

This layer handles the physical act of typing, wrapping, and removing delimiters. The goal is seamless forward-typing and retro-active Vim-style wrapping without relying on heavy background parsers.

- **`electric-pair-mode` (Built-in):** The definitive engine for forward-typing auto-pairing. When an opening bracket is typed, it inserts the closing bracket. When a region is highlighted and an opening bracket is typed, it wraps the region.
  - _Optimization:_ Configure `electric-pair-inhibit-predicate` to inhibit auto-pairing for single quotes (`'`) when typing contractions in comments/strings, and respect Tree-sitter string boundaries to prevent "escaped quote" desyncs.
- **`evil-surround` (Third-Party):** The undisputed champion for retro-active Vim-style wrapping. It provides the `ys` (surround), `cs` (change surround), and `ds` (delete surround) operator-pending motions (e.g., `ysiw"`, `cs[{`, `ds(`). It operates perfectly alongside `electric-pair-mode` without conflict.
- **`delete-pair` (Built-in Emacs 28+):** A highly underutilized native Emacs command. While `evil-surround` provides `ds` (which requires navigating to the exact bracket), `delete-pair` intelligently deletes the parenthesis pair _surrounding the current cursor position_, regardless of whether the cursor is directly on the bracket or deep inside the nested block. Binding this to a leader key provides a massive speed boost for unwrapping deeply nested C++ or JS function calls.

---

## 4. Pillar 2: Visual Feedback (The Perception Layer)

This layer provides instantaneous visual confirmation of bracket boundaries and nesting depth, crucial for parsing complex JSON, CSS, or C++ template metaprogramming.

- **`show-paren-mode` (Built-in):** Highlights the matching bracket.
  - _Optimization:_ Implement a custom ElDoc backend to display the off-screen matching line in the echo area. This allows the user to see the opening `\begin{document}` or `class MyClass {` without scrolling up.
- **`blink-matching-paren` (Built-in):** Momentarily highlights the opening bracket when a closing bracket is typed.
  - _Optimization:_ Set `blink-matching-paren-distance` to `nil` (unlimited). By default, Emacs stops searching after a few thousand lines. Setting it to `nil` allows Emacs to blink the matching opening tag even if it is 10,000 lines up in a massive JSON or C++ file.
- **`rainbow-delimiters` (Third-Party):** Colorizes nested brackets based on depth. Essential for visually parsing deeply nested structures without losing track of the current scope.

---

## 5. Pillar 3: Navigation and Text Objects (The Movement Layer)

This layer bridges the gap between standard Vim text objects and the Tree-sitter AST. This is where the modern Emacs 30/31 stack vastly outperforms legacy configurations.

- **`evil-matchit` (Third-Party):** Retained for jumping between semantic blocks via the `%` motion. It natively understands the AST/syntax boundaries of LaTeX (`\begin` to `\end`), Python (`if` to `else`), and HTML (opening to closing tags).
- **`evil-textobj-tree-sitter` (Third-Party):** The holy grail for non-Lisp Vim users. It maps Vim text objects directly to the native Emacs `treesit` AST.
  - _Standard Vim vs. AST:_ Standard Vim text objects (`iw`, `i"`, `i(`) rely on naive character scanning. `evil-textobj-tree-sitter` provides AST-accurate text objects.
  - _Examples:_
    - `vaf` (Visual Around Function): Selects the entire function, including decorators and docstrings, regardless of indentation.
    - `cia` (Change Inner Argument): Instantly changes the exact function argument the cursor is resting on, safely ignoring commas and nested brackets inside that argument.
    - `vaC` (Visual Around Conditional): Selects an entire `if/else if/else` block chain.
- **`evil-tex-ts` (Third-Party):** A modern, native `treesit`-based toolkit specifically for LaTeX editing in Evil mode. It replaces the legacy regex-based `evil-tex`, providing mathematically perfect text objects and motions for LaTeX environments and math blocks.
- **Native Emacs 30 `treesit-navigate-thing`:** Emacs 30 natively integrated structural Tree-sitter navigation directly into the C-core. Functions like `treesit-beginning-of-thing`, `treesit-end-of-thing`, and `treesit-thing-next` allow for zero-package AST node hopping. By defining what a "thing" is for a specific language, you can map these native functions to Evil motions, replacing Combobulate's `C-c o n` prefix with lightweight, native C-level traversal.

---

## 6. Pillar 4: Structural Manipulation (The Refactoring Layer)

In non-Lisp languages, structural manipulation means moving AST nodes (e.g., swapping two function arguments, moving a dictionary key up or down, or extracting a variable) without relying on the clipboard. Because `combobulate` is rejected due to Evil friction, the following modular alternatives are utilized:

- **`expreg` / `treesit-expreg` (Third-Party / Built-in Emacs 30+):** The modern, AST-aware replacement for the legacy `expand-region` package. It uses Tree-sitter to expand the highlighted region through logical syntactic boundaries (word -> string -> expression -> statement -> block). Because it relies on native Emacs region semantics, it plays perfectly with Evil's Visual state and operator-pending commands (e.g., `c` + expand-region to change the exact AST node).
- **`evil-ts-obj` / `evil-ts` (Third-Party):** Utilizes the native Emacs 30 `treesit` API to provide advanced structural actions specifically designed for Evil.
  - _The "Teleport" Feature:_ Unlike Combobulate, which requires a dedicated transient menu to move nodes, `evil-ts-obj` allows you to "teleport" the current text object after or before its parent node using standard Vim-style bindings (e.g., `M-l` / `M-h`), seamlessly integrating AST manipulation into standard editing flows.
- **`tree-edit` (Third-Party):** The logic-programming alternative to `combobulate`. It uses a mix of the Tree-sitter AST and logic programming to structurally destructure, navigate, and restructure the AST. It provides language-agnostic editing operations for deep refactoring (e.g., extracting a nested expression into a variable across multiple languages). It operates more as a dedicated "refactoring state" rather than a continuous navigation tool.

---

## 7. Summary: The Definitive Non-Lisp Stack Matrix

To achieve the ultimate, mathematically sound workflow for Python, C++, Web Dev, and LaTeX in Emacs 30/31, the following modular stack is required:

| Category               | Package / Feature                                | Purpose & Paradigm                                                             |
| :--------------------- | :----------------------------------------------- | :----------------------------------------------------------------------------- |
| **Insertion/Wrapping** | `electric-pair-mode` + `evil-surround`           | Auto-pairing and Vim-style retro-active wrapping (`ys`, `cs`, `ds`).           |
| **Unwrapping**         | `delete-pair` (Built-in)                         | Instant unwrapping of deeply nested structures from anywhere inside the block. |
| **Visuals**            | `show-paren-mode` + `rainbow-delimiters`         | Off-screen echo area context and depth-based colorization.                     |
| **Boundary Jumping**   | `evil-matchit` + Native `treesit-navigate-thing` | Semantic block jumping (`%`) and native C-level AST node hopping.              |
| **AST Text Objects**   | `evil-textobj-tree-sitter` + `evil-tex-ts`       | AST-accurate Vim text objects (`vaf`, `cia`) and LaTeX environment jumping.    |
| **Region Expansion**   | `expreg` (with Tree-sitter backends)             | AST-aware visual selection expansion.                                          |
| **Deep Refactoring**   | `evil-ts-obj` / `tree-edit`                      | Node swapping, teleportation, and AST extraction without clipboard reliance.   |

---

## 8. Architectural Implementation Rules

When implementing this stack in `config.org`, the following strict boundaries must be maintained:

1.  **Do NOT use `combobulate`:** Its heavyweight, non-Vim structural paradigm fundamentally clashes with Evil's operator-pending grammar and requires extensive manual glue code to prevent keymap collisions.
2.  **Do NOT use `smartparens` or `puni`:** Regex-based pair tracking causes desyncs in complex strings/templates. Furthermore, Lisp-centric soft-deletion and slurping/barfing are irrelevant and cognitively burdensome for indentation/tag-based languages.
3.  **Isolate Refactoring Tools:** Bind AST manipulation commands (node swapping, extracting, teleporting) to a dedicated Transient menu or a specific leader prefix (e.g., `SPC c r` for structural refactoring). This keeps them entirely isolated from standard `hjkl` navigation and operator-pending muscle memory, preventing accidental structural mutations during rapid typing.
4.  **Emacs 30/31 API Safety:** Any configuration utilizing `treesit-navigate-thing` or native Emacs 30 AST functions must be wrapped in defensive runtime guards (e.g., `(when (fboundp 'treesit-navigate-thing) ...)`) to ensure the configuration does not crash on Emacs 29.
