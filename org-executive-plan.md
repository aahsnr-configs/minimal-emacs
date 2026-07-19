### 1. Core Architecture & Load-Order Physics

- **Module Pruning (`org-modules`)**: We agreed to use `(defvar org-modules nil)` in the `:preface` block to prevent Org from loading its massive array of legacy extensions at startup. This satisfies the byte-compiler and forces explicit opt-in.
- **Exception for `org-habit`**: As noted in your feedback `[NOTE 8]`, `org-habit` is still required for your workflow. It will be explicitly opted back into the `org-modules` list rather than being pruned.
- **Alphabetical Lists**: `(setq org-list-allow-alphabetical t)` will be placed in `:preface` so Org's parser recognizes `a)`, `A)`, and `a.` list formats before initialization.
- **Cache Routing**: `setq` declarations will route `org-persist-directory`, `org-preview-latex-image-directory`, and `org-publish-timestamp-directory` strictly into the `no-littering` var directory.
- **Rejection of Doom Tripwires**: We rejected Doom's `defvar` tripwires for `org-directory` and `org-id-locations-file` in `:preface`, as they serve no functional purpose in a Vanilla configuration and risk interfering with native defaults.

### 2. Babel Execution & Lazy-Loading

- **The Babel Lazy-Load Hack**: We designed a mathematically correct Vanilla Emacs translation of Doom's Babel lazy-loader using the native `define-advice` macro (built into `nadvice.el`). It overrides `org-babel-do-load-languages` with `#'ignore` and dynamically requires `ob-<lang>` packages only when needed, utilizing an alias map for divergent language names.
- **The `emacs-jupyter` Exception**: Because this hack breaks `emacs-jupyter` kernel initialization, the implementation will be kept strictly commented out. If enabled in the future, `ob-jupyter` must be explicitly required in the Jupyter setup block.

### 3. Echo Area, ElDoc, and Breadcrumbs

- **LSP Limitations in Babel Blocks**: We established that LSP hover-info is mathematically impossible inside Org Babel blocks due to `textDocument` URI constraints and lack of workspace context.
- **Custom `org-eldoc` Teardown**: As per your directive `[NOTE: 6]`, the heavy, AST-parsing custom ElDoc functions will be completely replaced. We will drop the abandoned `org-contrib` package and author a custom, highly optimized local Elisp library. This new library will strip Emacs <28 dead code, implement spatial guards, utilize the async callback API, and defer structural breadcrumbs entirely to the external `breadcrumb` package in the header-line to guarantee zero echo-area latency.

### 4. Workflow, Agenda, and Denote Integration

- **Context via Tags**: As you noted `[NOTE 2]`, using tags and categories for context is the correct approach. We will rely on `#+FILETAGS` and standard Org categories to separate project contexts in the agenda.
- **Excision of Dynamic State Swapping**: Per your confirmation `[NOTE: 5]`, the entire dynamic context registry, the `find-file-hook` switching logic, and the dynamic directory structure will be completely removed to prevent I/O latency and agenda cache corruption.
- **Simplifying the Workflow Soup**: As per `[NOTE: 7]`, `org-gtd` will be dropped due to its rigid file structures. Task management will rely on standard Org TODO states, `org-agenda`, `org-refile`, and `org-super-agenda` grouped by standard context tags.
- **Denote Silos**: Denote will remain the pure Zettelkasten engine, completely decoupled from GTD/Agenda task management, utilizing Denote's native Silos feature for isolated, git-tracked note collections.

### 5. UI/UX Sanitization & Typography

- **Rejection of Doom's Sanitization Hooks**: As you noted `[NOTE 3]` and `[NOTE 4]`, Doom's aggressive UI sanitization is unnecessary for your setup. Because you do not use `org-indent-mode`, `show-paren-mode` is mathematically safe to leave enabled globally without local Org guards. Furthermore, you have your own established behaviors for navigation, so Doom's `C-a`/`C-e` and `TAB` subtree toggling remaps will not be adopted.
- **Better Font Faces**: The `dolist` loop setting typography scaling for `org-level-1` through `org-level-8`, as well as the `custom-set-faces` block for global `bold` and `italic` mappings, will be strictly preserved as they are mathematically orthogonal to Org's structural rendering.

### 6. Final Execution Summary Plan

When the `GREEN LIGHT` is given, the Org Mode and Second Brain teardown will execute the following steps:

1.  **Excise**: Delete the dynamic state-swapping registry, the `find-file-hook` switching logic, and the `org-gtd` package configuration.
2.  **Optimize `:preface`**: Inject the `defvar` for `org-modules` (explicitly keeping `org-habit`), the `setq` for alphabetical lists, and the `no-littering` cache routing for persist, latex, and timestamp directories.
3.  **Implement Babel Hack (Commented)**: Insert the mathematically corrected Vanilla Emacs Babel lazy-load advices and alias maps, wrapped in a comment block to protect `emacs-jupyter`.
4.  **Architect Agenda & Denote**: Rewrite `org-agenda-custom-commands` for static, scoped dashboard routing based on tags and `org-refile`. Configure Denote Silos for multi-directory note trees.
5.  **Preserve Typography**: Retain the heading scaling and `bold`/`italic` face mappings in the Better Font Faces subsection.
6.  **Author Custom ElDoc**: Write the optimized, local Elisp library that strips dead code, applies AST guards, and offloads breadcrumbs to the header-line.
