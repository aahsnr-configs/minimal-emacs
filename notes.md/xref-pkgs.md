The built-in Emacs `xref` framework standardizes cross-referencing commands (like finding definitions and finding references). Several packages from GNU ELPA, MELPA, and individual Git repositories complement `xref` by enhancing its user interface (frontends), extending the underlying sources of information (backends), or providing advanced navigation stack utilities.

The following list of packages complement and extend `xref`:

---

### I. UI & Frontend Enhancements (Selecting and Previewing Results)

These packages replace or enhance the default `*xref*` buffer layout, allowing you to preview or select definitions in alternative ways:

- **`consult-xref` (Included in `consult` on GNU ELPA / MELPA)**  
  Part of the modern, `completing-read`-based `consult` suite, this function lets you preview code definitions in real time as you filter results in the minibuffer. Setting `xref-show-xrefs-function` and `xref-show-definitions-function` to `#'consult-xref` routes search queries directly into your active completion vertical UI (like Vertico).
- **`consult-xref-stack` (Git / GitHub)**  
  Developed by Brett Lempereur, this package complements the xref history stack. It provides a searchable, `consult`-powered menu (`consult-xref-stack`) that allows you to easily navigate back and forth through multiple layers of symbol-jumping history.
- **`ivy-xref` (on MELPA)**  
  For users of the Ivy completion framework, this package overrides the default prompt style. It formats and lists potential definitions or references inside an interactive `ivy-read` session for rapid terminal-based narrowing.
- **`helm-xref` (on MELPA)**  
  If you use Helm, this package routes your xref definition and reference results into a dedicated Helm buffer, complete with multi-candidate selections and Helm-native actions.
- **`xref-posframe` (Git / GitHub)**  
  Created by developer _fmdkdd_, this package uses child frames (`posframe`) to preview a definition in a temporary floating window directly over the point. Pressing the command once shows the preview; pressing it a second time jumps directly to the location.
- **`nerd-icons-xref` (on MELPA)**  
  An aesthetic helper that injects Nerd Font icons into your `*xref*` results buffer, visually separating file paths, line numbers, and search matches.

---

### II. Backend Extensions & Aggregators (Enhancing the Search Sources)

By default, major modes register a backend on `xref-backend-functions`. These packages introduce more robust backend engines or allow you to bridge different backends:

- **`xref-union` (on GNU ELPA)**  
  Maintained by Philip Kaludercic, this is an aggregator for multiple backends. Normally, Emacs executes the first available backend and stops if it succeeds. `xref-union` allows you to merge results from multiple engines (such as querying both `Eglot` and `Etags` simultaneously) and displays them in a single, unified view.
- **`dumb-jump` (on MELPA)**  
  A widely used, lightweight fallback tool. It scans code patterns using search programs like `ripgrep`, `ag`, or `grep`. Rather than running a full Language Server (LSP), registering `dumb-jump-xref-activate` on `xref-backend-functions` provides a quick, heuristic definition lookup across dozens of programming languages with zero indexing overhead.
- **`citre` (on MELPA / GitHub)**  
  `citre` acts as a modern Ctags/readtags frontend. It adds `citre-xref-backend` to the top of the backend chain, giving you fast index-based jumps while seamlessly integrating with active `Eglot` servers as a fallback.
- **`gxref` (on MELPA / GitHub)**  
  A specialized backend that interfaces with GNU Global (`gtags`) databases. This is useful for large C/C++ or Java projects where local tags are preferred over heavy external utilities.

---

### III. Navigation & Stack Utilities (Navigating Marker History)

When jumping into definitions, `xref` records where you came from, but managing these marks across multiple files or windows is often extended by these utilities:

- **`empx.el` (Git / GitHub)**  
  "Extended `xref.el`" is a package designed to build on the concept of forward/backward navigation. Since standard xref tracks jumps initiated by code symbols, `empx` allows manual position marking and idle-time automatic markers to build a unified back-and-forth historical stack.
- **Built-in Window/Project Stack Extension**  
  While not an external package, a modern Emacs feature (introduced in Emacs 29) allows configuring `xref-history-storage`. Changing this variable to `#'xref-window-local-history` implements per-window stacks. Several developers have published Git snippets to extend this to per-project/Projectile stacks.
