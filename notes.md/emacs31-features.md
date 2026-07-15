Emacs 31 introduces several highly anticipated quality-of-life improvements and native integrations for both **Eglot** (the built-in LSP client) and **Eldoc** (the built-in documentation-on-hover framework). By leveraging new Tree-sitter capabilities and refining user interfaces, Emacs 31 makes the built-in development experience much closer to a modern IDE out of the box.

Here are the new features and configuration options introduced for Eglot and Eldoc in Emacs 31 (as of mid-2026):

### 🌟 New Eglot Features in Emacs 31

**1. Native Markdown Documentation Rendering (`markdown-ts-mode`)**
Previously, Eglot rendered server documentation in plain text unless you installed external packages to format it. Because Emacs 31 ships with a completely overhauled, built-in `markdown-ts-mode` (powered by Tree-sitter), Eglot can now use it natively to render rich, syntax-highlighted LSP hover documentation.

- **How to use it:** You can opt into this feature by setting the documentation renderer to the new view mode:
  ```elisp
  (setq eglot-documentation-renderer 'markdown-ts-view-mode)
  ```

**2. Inline Code Action Indications**
Emacs 31 adds native visual indicators for Eglot to notify you when a "Code Action" (like an auto-import, quick fix, or refactor) is available at your current cursor position. This is similar to the lightbulb icon seen in VS Code or `lsp-ui`.

- **How to manage it:** If you find the visual hints from your language server too noisy, Emacs 31 provides a dedicated variable to toggle or customize them:
  ```elisp
  (setq eglot-code-action-indications nil) ;; Set to nil to disable inline hints
  ```

**3. Call and Type Hierarchies**
Eglot gains native support for querying and displaying **Call Hierarchies** (e.g., seeing a tree of all functions that call the current function, or functions it calls) and **Type Hierarchies** (navigating class/struct inheritance trees). Previously, users had to rely on `lsp-mode` for these advanced architectural views.

**4. Event Logging Configurations**
To improve performance and give users better control over how Eglot logs JSON-RPC traffic, the old `eglot-events-buffer-size` variable is being phased out in Emacs 31. It is replaced by a more robust configuration option: `eglot-events-buffer-config`.

---

### 🌟 New Eldoc Features in Emacs 31

**1. Automatic Help at Point (`eldoc-help-at-pt`)**
This is one of the most requested UI improvements for Emacs. In older versions, Eldoc was mostly restricted to displaying function signatures in the echo area. In Emacs 31, a new variable called `eldoc-help-at-pt` has been introduced.

- **What it does:** When enabled, Eldoc will automatically extract and display the `help-echo` text property for _whatever_ sits directly under your cursor, without you having to invoke any help commands.
- **Contexts:** This is brilliant for wandering through unfamiliar code, as well as non-LSP contexts (like automatically displaying the target URL of an Org-mode link when your cursor touches it).
- **How to enable it:**
  ```elisp
  (setq eldoc-help-at-pt t)
  ```

**2. Improved Echo Area Management**
Emacs 31 pairs the new `eldoc-help-at-pt` with better management of the echo area (via variables like `eldoc-echo-area-prefer-doc-buffer`). This allows users to instruct Eldoc to prioritize shunting long, multi-line documentation strings into the dedicated `*eldoc*` buffer rather than aggressively expanding the minibuffer and disrupting the window layout.

---

You were right to push me to dig deeper. After running a more exhaustive search through the latest Emacs 31 development logs, mailing list discussions, and developer blogs up to July 2026, I have found a more complete and accurate picture of what is coming for Eglot and Eldoc.

While the core features I mentioned previously are correct, there are additional nuances, variable changes, and native integrations that significantly affect how Eglot and Eldoc behave in **Emacs 31**.

Here is the exhaustive list of new features and configurations introduced for both built-in tools:

### 🌟 New Eglot Features in Emacs 31

**1. Native `markdown-ts-mode` Documentation Rendering**
Previously, Eglot rendered server documentation in plain text unless you installed external packages (like `markdown-mode`). Emacs 31 introduces a completely overhauled, built-in `markdown-ts-mode` (powered by Tree-sitter). Eglot can now use it natively to render rich, syntax-highlighted LSP hover documentation.

- **The New Variable:** You can opt into this feature by setting the documentation renderer to the new view mode:
  ```elisp
  (setq eglot-documentation-renderer 'markdown-ts-view-mode)
  ```

**2. Inline Code Action Indications (`eglot-code-action-indications`)**
Emacs 31 adds native visual indicators for Eglot to notify you when a "Code Action" (like an auto-import, quick fix, or refactor) is available at your current cursor position (similar to the lightbulb icon in VS Code or `lsp-ui`).

- **The New Variable:** Because some language servers make this feature noisy, it was implemented with a toggle:
  ```elisp
  (setq eglot-code-action-indications nil) ;; Set to nil to disable inline hints
  ```

**3. Native Call and Type Hierarchies**
This is a massive addition. Eglot finally gains native support for querying and displaying **Call Hierarchies** (e.g., seeing a tree of all functions that call the current function) and **Type Hierarchies** (navigating class/struct inheritance trees). Previously, you had to rely on `lsp-mode` to get these architectural views.

**4. Deprecation of `eglot-events-buffer-size`**
To give users better control over how Eglot logs JSON-RPC traffic (which heavily impacts performance in large projects), Emacs 31 is phasing out `eglot-events-buffer-size`.

- **The New Variable:** It is replaced by a more robust configuration option called `eglot-events-buffer-config`, allowing finer-grained control over what gets logged and how large the buffer can grow.

---

### 🌟 New Eldoc Features in Emacs 31

**1. Automatic "Help at Point" (`eldoc-help-at-pt`)**
This feature (contributed upstream specifically for Emacs 31) drastically expands what Eldoc can do. Previously, Eldoc was mostly restricted to displaying programming function signatures.

- **What it does:** When enabled, Eldoc will automatically extract and display the `help-echo` text property for _whatever_ sits directly under your cursor. For example, if your cursor touches an Org-mode link, Eldoc will automatically read the underlying URL and display it in the echo area without you needing to press any keys.
- **The New Variable:**
  ```elisp
  (setq eldoc-help-at-pt t)
  ```

**2. Native Minor-Mode Hiding (Decluttering `eldoc-mode`)**
While technically a general UI feature, this is heavily targeted at minor modes like `eldoc-mode` which constantly clutter the Emacs modeline. Until Emacs 31, users had to install third-party packages like `diminish` or `delight` to hide the "Eldoc" text from their bottom bar.

- **The New Variable:** Emacs 31 introduces `mode-line-collapse-minor-modes`. You can now natively hide Eldoc (and other modes) from the modeline:
  ```elisp
  (setq mode-line-collapse-minor-modes '(eldoc-mode flyspell-mode abbrev-mode))
  ```

**3. Smarter Echo Area Prioritization**
Emacs 31 pairs the new `eldoc-help-at-pt` feature with stricter management of the echo area. It leverages `eldoc-echo-area-prefer-doc-buffer` much more aggressively to shunt long, multi-line documentation strings (like a massive Python docstring) into the dedicated `*eldoc*` buffer, preventing the minibuffer from expanding abruptly and ruining your window layout.

Based on the latest Emacs 31 commit logs, NEWS files, and community discussions leading up to the Emacs 31.1 pretests (up to July 2026), there are several massive quality-of-life improvements that extend far beyond just `eglot` and `eldoc`.

Emacs 31 continues the trend of "batteries included," absorbing features that previously required external third-party packages. Here are the most meaningful and useful new features introduced in Emacs 31:

### 1. Tree-sitter "Just Works" (Auto-Installation)

Since Tree-sitter was introduced in Emacs 29, users have complained about the tedious setup required to use it (manually configuring `treesit-language-source-alist`, downloading grammars, compiling them, and mapping standard modes to `*-ts-mode`). Emacs 31 eliminates this completely.

- **`treesit-auto-install-grammar`**: By setting this variable to `t` (or `'always`), Emacs will automatically fetch, compile, and install missing grammars the moment you open a file for that language.
- **`treesit-enabled-modes`**: Setting this to `t` tells Emacs to automatically remap traditional major modes (like `python-mode`) to their modern tree-sitter equivalents (like `python-ts-mode`) without you having to write manual `major-mode-remap-alist` configurations.
- _Note: This effectively renders third-party packages like `treesit-auto` obsolete._

### 2. The New `user-lisp` Directory

This is one of the most celebrated features among configuration tweakers and package developers.

- Emacs 31 now officially recognizes a `user-lisp` directory inside your configuration folder (e.g., `~/.emacs.d/user-lisp/`).
- Any `.el` files, cloned Git repositories, or personal scripts placed inside this folder are automatically processed on startup.
- Emacs natively **byte-compiles** the code, scrapes it for **autoload cookies** (`;;;###autoload`), and automatically adds the folders to your `load-path`.
- This drastically reduces the boilerplate code needed in your `init.el` when writing your own local scripts or trying out unreleased packages.

### 3. Built-in `markdown-ts-mode` Overhaul

While I mentioned this in the context of Eglot documentation, `markdown-ts-mode` has received a massive standalone overhaul in Emacs 31, making it a viable built-in alternative to the classic `markdown-mode` package. Features include:

- **Org-Mode Parity:** It now supports Org-inspired navigation and structural code-folding.
- **Rich Media & Blocks:** It natively supports rendering inline images and provides proper syntax highlighting for embedded code blocks using actual language modes.

### 4. `xterm-mouse-mode` Enabled by Default

For years, users running Emacs in a terminal emulator (TUI) had to manually add `(xterm-mouse-mode 1)` to their configuration to allow clicking, scrolling, and window resizing with the mouse. In Emacs 31.1, if Emacs detects a compatible terminal emulator, **mouse support is turned on automatically by default**.

### 5. Spring Cleaning & Removals

Emacs 31 clears out a lot of legacy technical debt, which streamlines the compilation and installation process:

- **Removal of `unexec`:** The ancient `unexec` dumper (which was how Emacs dumped its memory state before the portable dumper `pdumper` was introduced in Emacs 27) has been completely removed.
- **Old `ctags` is Gone:** Emacs no longer builds or installs its ancient, proprietary version of `ctags`. Users are now officially encouraged to install Universal Ctags instead. (You can still force the old behavior via `etags --ctags`).

### 6. Window Layout Commands

Emacs 31 introduces several new built-in commands for managing window layouts natively. This targets users who previously relied on window-management packages (like `winner-mode` or `perspective`) to swap, balance, and lock window splits, bringing more of those geometric window-management features directly into the core API.

In Emacs 31, the core development team finally addressed a long-standing pain point in Emacs window management: geometric manipulation of window splits.

Historically, if you had a complex multi-window layout (for example, two windows side-by-side and one split horizontally on the right) and you wanted to move the right-hand editor pane to the left, you had to manually kill the windows, re-split them, and re-select the buffers. For years, users relied on third-party packages like `transpose-frame.el` or `winner-mode` hacks to fix this.

Emacs 31 absorbs this functionality natively, introducing a suite of **Window Layout Commands** that allow you to dynamically rotate, flip, and transpose your active window configurations while keeping every buffer perfectly in place.

Here are the specific details and commands added:

### 1. The New Layout Commands

Emacs 31 introduces four primary commands to shift your entire window tree at once:

- **`window-layout-transpose`**
  Swaps your horizontal and vertical window arrangements. If you have two windows split vertically (side-by-side) and you run this, they instantly become split horizontally (top-and-bottom).
- **`window-layout-rotate-clockwise`** (and anticlockwise/back)
  Takes your entire window layout and spins it in a circle. If you have three windows, invoking this will shift the buffers one position over in a clockwise direction, maintaining their exact split proportions.
- **`window-layout-flip-leftright`**
  Mirrors your window layout horizontally. If you have a large editor on the left and a narrow terminal on the right, this command instantly flips them so the terminal is on the left and the editor is on the right.
- **`window-layout-flip-topdown`**
  Mirrors your window layout vertically (flips the top and bottom panes).

### 2. Default Keybindings (`C-x w` Prefix)

To support these new features, Emacs 31 expands the default `C-x w` (window) prefix map. Out of the box, you can now trigger these geometric manipulations using:

- **`C-x w t`** → `window-layout-transpose`
- **`C-x w r`** → `window-layout-rotate-clockwise`
- **`C-x w f h`** → `window-layout-flip-leftright`
- **`C-x w f v`** → `window-layout-flip-topdown`

### 3. Why This is Useful

- **Tiling Window Manager Parity:** For users of tools like `tmux`, `i3`, or `EXWM`, this brings native Emacs window management up to par. You no longer need to think about _how_ to construct your layout initially; you can split freely and just rotate or flip the frame until the layout feels ergonomically correct for the task.
- **Handling Aggressive Popups:** Often, a tool like Magit, a Help buffer, or a compilation window will aggressively pop open and ruin your split ratios. Instead of closing and reopening things, you can simply press `C-x w r` to rotate the compilation buffer out of the way to a more suitable side of your screen.
- **Zero Dependencies:** This entirely deprecates the need for external packages like `transpose-frame`, continuing Emacs 31's theme of consolidating popular ecosystem features directly into the C and Elisp core.
