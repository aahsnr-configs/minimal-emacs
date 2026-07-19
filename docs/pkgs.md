Here is the comprehensive, categorized package inventory extracted strictly from the `ide-features.md` and `config.org.txt` source documents. The packages are classified by their origin (Built-in vs. External/Bundled) and mapped to their respective architectural domains.

## 1. IDE Features Parity Stack (`ide-features.md`)

This section catalogs the packages required to achieve VS Code parity, as defined in the IDE features specification document.

### Code Intelligence & Completion

- **`eglot`** [Built-in] - LSP client driving completion, hover, and diagnostics.
- **`corfu`** [External] - In-buffer completion UI engine.
- **`corfu-candidate-overlay`** [External] - Ghost text (inline preview) rendering.
- **`corfu-popupinfo`** [Bundled] - Candidate-level documentation popups.
- **`cape`** [External] - Completion At Point Extensions (backend merger).
- **`orderless`** [External] - Space-separated fuzzy filtering engine.
- **`nerd-icons-corfu`** [External] - Margin icon formatter for Corfu.
- **`yasnippet-capf`** [External] - Snippet integration into the CAPF pipeline.

### Hover, Signatures & Documentation

- **`eldoc`** [Built-in] - Core documentation router (echo area & ephemeral buffers).
- **`markdown-ts-mode`** [Built-in] - Tree-sitter markdown fontification for `*eldoc*` buffers.
- **`eldoc-box`** [External] - _Referenced for GUI childframe hover parity (rejected in native stack, but documented)._
- **`peek`** [External] - Inline overlay engine for Peek Definition/References.

### Navigation & Code Jumping

- **`xref`** [Built-in] - Core cross-referencing and history framework.
- **`consult-xref`** [Bundled] - Vertico-powered live previews for xref locations.
- **`xref-mouse-mode`** [Built-in] - Emacs 31 native Ctrl+Click jump routing.
- **`breadcrumb`** [External] - Header-line breadcrumb navigation.
- **`imenu`** [Built-in] - Native document symbol indexing.
- **`consult-eglot`** [External] - Fuzzy workspace and document symbol searching.

### Code Actions, Refactoring & Formatting

- **`apheleia`** [External] - Async CLI whole-file formatting.
- **`lazy-ruff`** [External] - Python CLI range formatting.
- **`electric` / `elec-pair`** [Built-in] - Native on-type formatting and auto-pairing.

### Visual Enhancements & UI Overlays

- **`treesit-fold`** [External] - AST-aware code folding.
- **`hideshow`** [Built-in] - Native regex/indentation fallback folding.
- **`vimish-fold`** / **`evil-vimish-fold`** [External] - Visual region folding.
- **`expreg`** [External] - AST-aware smart expand/shrink region.
- **`auto-rename-tag`** [External] - Zero-latency linked editing for HTML/JSX tags.
- **`colorful-mode`** [External] - Inline CSS/color swatches.
- **`rainbow-mode`** [External] - Fallback hex/RGB highlighting.
- **`rainbow-delimiters`** [External] - Depth-based bracket pair colorization.
- **`paren` (show-paren-mode)** [Built-in] - Native matching delimiter highlighting.
- **`minimap`** [External] - Scaled buffer overview sidebar.
- **`diff-hl`** [External] - Fringe git diff indicators (integrated with minimap).
- **`topsy`** / **`sticky-scroll-mode`** [External] - Sticky scroll header pins.

### Diagnostics & Debugging

- **`flymake`** [Built-in] - Core diagnostic engine (push/pull models).
- **`flyover`** [External] - Inline diagnostic overlay engine for `flymake`, rendering squiggles and messages directly in the buffer.
- **`consult-flymake`** [Bundled] - Vertico-powered diagnostic filtering.
- **`dape`** [External] - Debug Adapter Protocol client (inline variables).

### Precision Editing

- **`iedit`** [External] - Core multi-cursor foundation.
- **`evil-multiedit`** [External] - Symbol-based multi-cursor state.
- **`evil-mc`** [External] - Arbitrary/rectangular fake cursor placement.

---

## 2. Configuration Implementation Stack (`config.org.txt`)

This section catalogs the packages actively configured, loaded, or referenced in the specified `config.org` sections.

### Workflow Management

#### Dired & Dirvish

- **`dired`** [Built-in] - Core directory editor.
- **`dirvish`** [External/Git] - Modern visual file manager overlay for Dired.
- **`dired-x`** [Built-in] - Extended Dired features (omit mode, OS integration).
- **`diredfl`** [External] - Modern syntax highlighting for Dired listings.
- **`wdired`** [Built-in] - Writable Dired buffers for bulk renaming.
- **`nerd-icons-dired`** [External] - Glyph rendering for Dired listings.

#### Treemacs

- **`treemacs`** [External] - Persistent file/project explorer sidebar.
- **`treemacs-nerd-icons`** [External] - Icon theme integration for Treemacs.
- **`treemacs-magit`** [External] - Magit synergy for Treemacs.
- **`treemacs-evil`** [External] - Evil state integration for Treemacs.

#### Buffer Management

- **`ibuffer`** [Built-in] - Advanced buffer dashboard and filtering.
- **`nerd-icons-ibuffer`** [External] - Glyph rendering for Ibuffer.
- **`ibuffer-vc`** [External] - Version control status injection for Ibuffer.
- **`projection-ibuffer`** [External/Bundled] - Project-aware Ibuffer filter groups.
- **`buffer-terminator`** [External] - Automated inactive buffer cleanup engine.

#### Project Management

- **`project`** [Built-in] - Native workspace and project root detector.
- **`projection`** [External] - Projectile-like abstractions and transient menus for `project.el`.
- **`projection-multi`** [External] - Multi-target compilation integration.
- **`projection-multi-embark`** [External] - Embark action bridging for projection targets.

#### Workspaces

- **`tab-bar`** [Built-in] - Headless native workspace container.
- **`bufferlo`** [External] - Strict buffer isolation and workspace persistence layer.

### Completion Framework

- **`orderless`** [External] - Fuzzy matching and style dispatchers.
- **`vertico`** [External] - Vertical minibuffer completion UI.
  - _Bundled Extensions:_ **`vertico-directory`**, **`vertico-quick`**, **`vertico-repeat`**, **`vertico-multiform`**, **`vertico-buffer`**, **`vertico-grid`**, **`vertico-unobtrusive`**.
- **`marginalia`** [External] - Rich minibuffer annotations.
- **`nerd-icons-completion`** [External] - Minibuffer glyph injection.
- **`consult`** [External] - Async search, navigation, and preview commands.
- **`consult-dir`** [External] - Directory jumping and spatial navigation.
- **`embark`** [External] - Contextual action and export engine.
- **`embark-consult`** [External] - Embark/Consult live-preview synergy.
- **`embark-org`** [Built-in/External] - Org-mode specific Embark actions.
- **`corfu`** [External] - In-buffer completion popup.
  - _Bundled Extensions:_ **`corfu-history`**, **`corfu-popupinfo`**, **`corfu-quick`**.
- **`nerd-icons-corfu`** [External] - Corfu margin formatter.
- **`cape`** [External] - Capf merging and boundary guards.
- **`dabbrev`** [Built-in] - Native dynamic abbreviation scanning.
- **`emacs` (simple/minibuffer)** [Built-in] - Core `tab-always-indent` and `read-extended-command-predicate` configurations.

### Org Mode

- **`org`** [Built-in] - Core AST, markup, and structural engine.
- **`org-modern`** [External] - Typography, badges, and visual prettification.
- **`org-habit`** [Built-in] - Habit tracking and consistency graphs.
- **`org-gtd`** [External] - Pure GTD task management workflow.
- **`org-edna`** [External] - Dependency and trigger engine (required by `org-gtd`).
- **`org-agenda`** [Built-in] - Centralized scheduling and dashboard views.
- **`org-capture`** [Built-in] - Hierarchical templating and rapid inbox routing.
- **`org-super-agenda`** [External] - Advanced agenda grouping and filtering.
- **`org-fragtog`** [External] - Lazy LaTeX fragment toggling.
- **`ox-latex`** [Built-in] - LaTeX export backend and class definitions.
- **`org-download`** [External] - _Referenced in keybindings for image/screenshot yanking._

### Second Brain & Productivity

#### Active / Configured

- **`denote`** [External] - Core Zettelkasten and file-naming convention engine.
- **`denote-journal`** [External] - Journaling extension for Denote.
- **`denote-org`** [External] - Org-mode specific Denote integrations.
- **`consult-denote`** [External] - Vertico-powered Denote searching and grepping.
- **`citar-denote`** [External] - Bibliography note bridging for Denote.

#### Referenced / Commented / Planned (Found in TODOs or commented blocks)

- **`denote-explore`** [External] - _Listed in global remaining work TODOs._
- **`citar`** [External] - _Referenced in commented bibliography management block._
- **`citeproc`** [External] - _Referenced in commented bibliography management block._
- **`citar-embark`** [External] - _Referenced in commented bibliography management block._
- **`reftex`** [Built-in] - _Referenced in commented bibliography management block._
- **`org-noter`** [External] - _Commented out PDF annotation session manager._
- **`org-pdftools`** [External] - _Commented out PDF link handler._
- **`org-noter-pdftools`** [External] - _Commented out precise PDF note integration._
