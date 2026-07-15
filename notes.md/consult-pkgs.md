Since `consult` relies entirely on the standard Emacs `completing-read` API, its visual appearance is dictated by the active completion framework (usually Vertico). However, there are several packages on ELPA, MELPA, and Git repositories designed specifically to improve or alter the visual presentation, preview behavior, and layout of `consult` commands.

---

### I. Frame Layout & Live Preview Enhancements

These packages directly alter how and where `consult` displays its interactive lists and live-preview windows:

- **`vertico-posframe-preview` (Git Repository / GitHub)**  
  A package designed to address a common layout challenge when combining `vertico-posframe` with `consult`. Normally, if you use a centered child frame for your minibuffer searches, `consult`'s live previews can pop up behind or underneath the child frame, blocking your view. This package generates a secondary "preview sidecar" (a separate child frame) next to your main search frame. When you navigate candidates in commands like `consult-line` or `consult-grep`, the preview content is rendered in this sidecar window.
- **`vertico-multiform` (Built-in extension to Vertico on GNU ELPA / MELPA)**  
  While technically an official Vertico extension, this is heavily utilized to configure `consult`'s UI on a command-by-command basis. You can use it to dynamically change the UI layout depending on which `consult` command is running. For example, you can configure:
  - `consult-line` to display in a flat, horizontal layout at the bottom of your screen.
  - `consult-grep` and `consult-ripgrep` to automatically expand into a dedicated sidebar or top split-buffer (`vertico-buffer`) instead of a cramped minibuffer, giving you more space to view file previews.

---

### II. Icons & Visual Annotations

These packages inject icons and structured metadata next to your search candidates to help you identify items quickly:

- **`marginalia` (on GNU ELPA / MELPA)**  
  The standard annotation engine for `completing-read`. It adds structured, aligned metadata to the right of your `consult` candidates. When switching buffers with `consult-buffer`, it displays the buffer size, mode, and file path. When running `consult-imenu`, it annotates symbol types (variables, functions, classes) so you can easily scan the list.
- **`nerd-icons-completion` (on MELPA)**  
  This package bridges modern Nerd Font icons into the minibuffer. It integrates directly with `marginalia` and `consult` to add visual glyphs next to your search results. This is highly useful in `consult-buffer` or `consult-find`, where file names are accompanied by their file-type icons.
- **`all-the-icons-completion` (on MELPA)**  
  An older equivalent to `nerd-icons-completion` that uses the "All the Icons" font library to place graphical icons inside your active `consult` selection lists.

---

### III. UI Aggregators & Contextual Launchers

These packages bundle multiple search paths and tools into a single, cohesive dashboard layout inside your completion system:

- **`consult-omni` (Git Repository / GitHub)**  
  A highly versatile successor to the `consult-web` package. It creates an "Omni Search" dashboard similar to macOS Spotlight or Alfred. It aggregates and dynamically organizes search results from different local sources (Org notes, local directories, calculators) and web sources (Google, Wikipedia, YouTube) into structured, visually grouped categories directly inside the `consult` interface.
- **`consult-dir` (on MELPA)**  
  Improves the directory-switching user experience. It acts as a directory-jumping companion that integrates with `consult-buffer` or `find-file`. It lets you insert directory paths directly into your current prompt, pull paths from your file history, or jump to active project roots.

---

### IV. Buffer Exporting & Structural Editing

- **`embark-consult` (on GNU ELPA / MELPA)**  
  While `embark` acts as an action system, `embark-consult` significantly changes the UI workflow of `consult`. If you run an asynchronous search like `consult-ripgrep` or `consult-line`, you can use this integration to "export" the current UI list of search results into a standard Emacs buffer (such as `occur-mode` or `grep-mode`). Combined with `wgrep` (writable grep), this allows you to visually edit those search results directly in the buffer and apply the changes across multiple files on your disk.
