# Doom Emacs `SPC o` (Open) Prefix — Complete Keybinding Analysis

## Source Verification

The `SPC o` prefix is defined in `modules/config/default/+evil-bindings.el` within the doomemacs repository . The base structure was confirmed via the Gitee mirror of the file , and the current master additions (ghostel, dirvish) were confirmed via search result snippets from the live repository .

---

## Your Active `SPC o` Bindings (Cross-Referenced with `init.el`)

Based on your `init.el` module declarations, here is the complete mapping of every binding you see in the which-key popup:

| Key | Which-Key Label              | Command                          | Source Module                    | Condition                           |
| --- | ---------------------------- | -------------------------------- | -------------------------------- | ----------------------------------- |
| `/` | Open directory in dirvish    | `#'dirvish`                      | `:emacs dired (+dirvish +icons)` | `(featurep! :emacs dired +dirvish)` |
| `a` | +org agenda (sub-prefix)     | _prefix-map_                     | `:lang org (+pretty)`            | Always (org is enabled)             |
| `A` | Org Agenda                   | `#'org-agenda`                   | `:lang org (+pretty)`            | Always                              |
| `b` | Default browser              | `#'browse-url-of-file`           | `:config default (+bindings)`    | Always (unconditional)              |
| `d` | start a debugger             | `#'+debugger/start`              | `:tools debugger`                | `(featurep! :tools debugger)`       |
| `f` | New frame                    | `#'make-frame`                   | `:config default (+bindings)`    | Always (unconditional)              |
| `F` | Select frame                 | `#'select-frame-by-name`         | `:config default (+bindings)`    | Always (unconditional)              |
| `p` | Project sidebar              | `#'+treemacs/toggle`             | `:ui treemacs`                   | `(featurep! :ui treemacs)`          |
| `P` | Find file in project sidebar | `#'treemacs-find-file`           | `:ui treemacs`                   | `(featurep! :ui treemacs)`          |
| `r` | REPL                         | `#'+eval/open-repl-other-window` | `:tools eval (+overlay)`         | `(featurep! :tools eval)`           |
| `R` | REPL (same window)           | `#'+eval/open-repl-same-window`  | `:tools eval (+overlay)`         | `(featurep! :tools eval)`           |
| `t` | Toggle ghostel popup         | `#'+ghostel/toggle`              | `:term ghostel`                  | `(featurep! :term ghostel)`         |
| `T` | Open ghostel here            | `#'+ghostel/here`                | `:term ghostel`                  | `(featurep! :term ghostel)`         |

---

## Sub-Prefix: `SPC o a` (Org Agenda)

| Key | Label       | Command             |
| --- | ----------- | ------------------- |
| `a` | Agenda      | `#'org-agenda`      |
| `t` | Todo list   | `#'org-todo-list`   |
| `m` | Tags search | `#'org-tags-view`   |
| `v` | View search | `#'org-search-view` |

---

## Sub-Prefix: `SPC o d` (Debugger)

From search result , the current master also defines a debugger sub-prefix:

| Key | Label                  | Command                         |
| --- | ---------------------- | ------------------------------- |
| `d` | Start debugger         | `#'dape`                        |
| `p` | (debugger sub-command) | `#'dape` (with project context) |

---

## Bindings You Do NOT Have (Module Not Enabled)

These exist in the upstream `+evil-bindings.el` but are **inactive** in your config because the required modules are not declared in your `init.el`:

| Key | Label                       | Command                     | Required Module                   | Your Status               |
| --- | --------------------------- | --------------------------- | --------------------------------- | ------------------------- |
| `-` | Dired                       | `#'dired-jump`              | `:emacs dired` (without +dirvish) | Replaced by `/` (dirvish) |
| `e` | Toggle eshell popup         | `#'+eshell/toggle`          | `:term eshell`                    | ❌ Not enabled            |
| `E` | Open eshell here            | `#'+eshell/here`            | `:term eshell`                    | ❌ Not enabled            |
| `D` | Docker                      | `#'docker`                  | `:tools docker`                   | ❌ Not enabled            |
| `m` | mu4e / notmuch / wanderlust | `#'=mu4e` etc.              | `:email mu4e` etc.                | ❌ Not enabled            |
| `o` | Reveal in Finder            | `#'+macos/reveal-in-finder` | `:os macos`                       | ❌ Not enabled (Linux)    |

---

## Command Details

### `#'+debugger/start` (SPC o d)

This is a Doom autoload function that launches the debugger. With your `:tools debugger` module and `:tools lsp (+eglot)`, it routes through `dape` (the modern DAP client). The function detects the current project type and prompts for a debug configuration .

### `#'+eval/open-repl-other-window` (SPC o r)

Opens a REPL for the current buffer's major mode in a **separate window**. For Python buffers, this opens a Python REPL; for Emacs Lisp, it opens IELM. The `+overlay` flag in your `:tools eval` module adds inline evaluation overlays .

### `#'+eval/open-repl-same-window` (SPC o R)

Same as above but replaces the current buffer with the REPL in the **same window**.

### `#'+treemacs/toggle` (SPC o p)

Toggles the Treemacs sidebar. Since you have `:ui treemacs` enabled, this is the active project sidebar binding (as opposed to `+neotree/open` which would be active if `:ui neotree` were enabled instead).

### `#'treemacs-find-file` (SPC o P)

Opens Treemacs and reveals/highlights the current buffer's file in the tree.

### `#'+ghostel/toggle` (SPC o t)

Toggles a Ghostel terminal popup window. This is the Doom wrapper around Ghostel's popup functionality, analogous to `+vterm/toggle` or `+shell/toggle` for other terminal modules.

### `#'+ghostel/here` (SPC o T)

Opens a Ghostel terminal in the current window, replacing the current buffer.

### `#'dirvish` (SPC o /)

Opens Dirvish (the enhanced Dired) for directory browsing. This binding is injected by the `:emacs dired (+dirvish)` module and **replaces** the default `#'dired-jump` binding on `-` .

### `#'browse-url-of-file` (SPC o b)

Opens the current file in your system's default web browser. For HTML files, this renders them; for other files, behavior depends on your OS `xdg-open` configuration.

### `#'make-frame` (SPC o f)

Creates a new Emacs frame (OS-level window).

### `#'select-frame-by-name` (SPC o F)

Prompts for a frame name and switches focus to it.

---

## Architectural Note: Binding Priority

The `SPC o` prefix uses a **last-writer-wins** pattern for conditional bindings. When multiple terminal modules are enabled (e.g., `:term shell` AND `:term vterm`), the last one in the `map!` block wins the `t`/`T` slots. In your config, only `:term ghostel` is enabled, so it exclusively owns `t` and `T`.

Similarly, `:ui treemacs` and `:ui neotree` are mutually exclusive for the `p`/`P` slots — you have treemacs, so neotree's bindings are never registered.

---

## Sources

- — doomemacs/doomemacs `+evil-bindings.el` on GitHub (master branch)
- — Search snippet confirming `(:prefix-map ("o" . "open")` structure with org-agenda bindings
- — Search snippet confirming `"Open directory in dirvish"` description
- — Gitee mirror of `+evil-bindings.el` (full source, older version)
- — Search snippet confirming dirvish `/` binding and debugger `d` sub-prefix with `dape`
- — Search snippet confirming REPL `"r"` binding
- — doomemacs/modules repository (ghostel module added July 2026)
