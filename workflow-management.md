# Architectural Blueprint: Workflow Management Context Engine (v2)

**Target Audience:** Future LLM Sessions / Context Ingestion
**Subject:** Buffer Management, Project Management, and Workspaces Triad
**Emacs Target:** 31.0.90 (PGTK, Native Comp) | **Package Manager:** Elpaca v0.12
**Status:** Verified against upstream `mohkale/projection`, `florommel/bufferlo`, and Emacs 30/31 `project.el` APIs.

---

## 1. The Triad Paradigm (Executive Summary)

In the v0.9 Emacs configuration, the **Workflow Management** section is anchored by a synergistic triad that strictly replaces the legacy, monolithic `projectile` + `persp-mode` stack. This triad forms the "Context Engine" of the IDE, mathematically isolating buffers, defining project boundaries, and persisting complex workflows across daemon restarts.

**Strict Negative Constraints (Must Obey):**

- **NO `projectile`:** Replaced entirely by `projection` and native `project.el`.
- **NO `persp-mode` / `perspective.el`:** Replaced entirely by `bufferlo` and native `tab-bar`.
- **NO `consult-projectile`:** Obsolete and unmaintained; `consult` natively supports `project.el`.

### The Triad Components:

1.  **Buffer Management (`ibuffer` + `projection-ibuffer`):** The visual dashboard for buffer triage, dynamically grouped by project roots.
2.  **Project Management (`projection` + `project.el`):** The structural boundary engine that defines "what constitutes a project" without the overhead of a background indexing daemon.
3.  **Workspaces (`bufferlo`):** The spatial isolation layer that leverages native Emacs `tab-bar` (hidden) to provide mathematically strict buffer lists per workflow context.

---

## 2. Subsection 1: Buffer Management

**Packages:** `ibuffer` (Built-in), `nerd-icons-ibuffer` (MELPA), `projection-ibuffer` (MELPA).

### Architectural Decisions & Physics

- **Why `ibuffer` over `bufler`/`buffalo`?** Packages like `bufler` introduce heavy, custom DSLs (Domain Specific Languages) for grouping buffers, violating the "Vanilla Emacs Paradigm" constraint. `ibuffer` is built-in, highly performant, and natively supports complex programmatic filter groups.
- **The `projection-ibuffer` Synergy:** Instead of the obsolete `ibuffer-projectile`, we use `projection-ibuffer`. It queries the native `project.el` API (managed by `projection`) to automatically generate filter groups.
  - _Implementation:_ Hook `ibuffer-projection-set-filter-groups` into `ibuffer-hook` to dynamically group all open buffers by their `project.el` roots.
- **Empty Group Suppression:** `ibuffer-show-empty-filter-groups` must be set to `nil`. Without this, switching projects leaves ghost headers for projects with no open buffers.
- **Human-Readable Sizes:** The `size` column must be redefined via `define-ibuffer-column` to use `file-size-human-readable`, converting raw byte counts into K/M/G formats.
- **Visual Parity:** `nerd-icons-ibuffer` is hooked into `ibuffer-mode` to provide the same visual glyphs seen in `vertico`/`marginalia` and `dirvish`.

---

## 3. Subsection 2: Project Management

**Packages:** `projection`, `projection-multi`, `projection-multi-embark` (MELPA).

### Architectural Decisions & Physics

- **Why `projection` over `projectile`?** `projectile` is a 6000-line monolith. `projection` is a lightweight, pluggable extension layer atop Emacs' native `project.el` . It provides the "batteries-included" features of Projectile (compilation commands, `ibuffer` grouping, `find-other-file`) without overriding core Emacs APIs or requiring a background indexing daemon.
- **Doom Emacs `SPC p p` Parity (Switch Project):**
  - In Doom, `SPC p p` invokes `projectile-switch-project` .
  - _Vanilla Solution:_ Route `SPC p p` to the native `project-switch-project`. Because `projection` extends `project.el`, it automatically inherits and manages the known projects list.
- **Enriching the Dispatch Menu (`project-switch-commands`):**
  - Emacs 30/31 uses the `project-switch-commands` variable to populate the dispatch menu presented _after_ selecting a project .
  - _Mandatory Injection:_ You must inject `consult-find`, `consult-ripgrep`, and `magit-project-status` into this list so the user gets a rich, Doom-like action menu instead of just defaulting to `project-find-file` .
  - _Syntax:_ `(add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t)`
- **Doom Emacs `SPC f p` Parity (Find File in Emacs Config):**
  - In Doom, `SPC f p` instantly searches the private config directory (`~/.doom.d/`) .
  - _Vanilla Solution:_ Inject a custom O(1) wrapper function named `ar/find-file-in-emacs-config`. This function must temporarily let-bind `default-directory` to `user-emacs-directory` and invoke `consult-find`.
- **Multi-Target Compilation:** `projection-multi` (and `projection-multi-embark`) must be integrated to allow interactive selection of compilation targets (e.g., CMake, Tox, Make) via `consult`, completely replacing `projectile-compile-project` .

---

## 4. Subsection 3: Workspaces

**Packages:** `bufferlo` (GNU ELPA).

### Architectural Decisions & Physics

- **Why `bufferlo` over `persp-mode`/`perspective.el`?** `persp-mode` relies on heavy, custom frame/window state tracking that frequently clashes with `popper`, `ace-window`, and daemon client frames. `bufferlo` is an official GNU ELPA package that leverages Emacs' native `tab-bar-mode` (hiding the visual tabs) to provide mathematically isolated buffer lists per workspace .
- **Session Persistence:** `bufferlo` natively persists workspace layouts across daemon restarts, fulfilling the requirement for complex, multi-day scientific writing and programming workflows.
- **The `consult-buffer` Integration (CRITICAL CORRECTION FROM V1):**
  - _V1 Error:_ V1 suggested manually injecting custom `bufferlo` sources into `consult-buffer-sources`.
  - _V2 Verified Physics:_ The upstream `bufferlo` documentation explicitly warns that manual source injection can cause "consult narrowing keys interfering with one another" .
  - _The Mathematically Superior Solution:_ `bufferlo` exposes a native filter function. You MUST simply set `(setq consult-buffer-list-function #'bufferlo-local-buffers)` . This 1-line configuration forces `consult-buffer` to strictly show file-visiting buffers from the _current workspace_ by default, entirely solving the "buffer soup" problem without breaking Consult's narrowing engine.
- **Tab-Bar Abstraction:** `tab-bar-mode` must be enabled but visually hidden (`tab-bar-show nil`). `bufferlo` uses the underlying tab structures to maintain strict buffer isolation.

---

## 5. Cross-Subsection Integration (The Triad)

The true power of this architecture lies in how these three subsections communicate:

1.  **The Root Detection Chain:** When a file is opened, `project.el` (extended by `projection`) determines the project root.
2.  **The Isolation Layer:** `bufferlo` intercepts buffer creation and assigns the buffer to the active workspace's hidden tab.
3.  **The Visualization Layer:** When `ibuffer` is summoned, `projection-ibuffer` queries `project.el` for the roots of all _visible_ buffers (filtered by `bufferlo`'s workspace isolation) and dynamically generates the filter groups.
4.  **The Minibuffer Layer:** When `consult-buffer` is summoned, `bufferlo-local-buffers` filters the candidates to the current workspace, while `consult-dir` uses `project.el` to offer project roots as jump targets.

---

## 6. Global Configuration Integration (v0.9 `config.org`)

The next LLM session must understand how this triad impacts the rest of the v0.9 configuration:

### A. Workflow Management (Dired, Dirvish, Treemacs)

- **Treemacs:** `treemacs-project-follow-mode` relies entirely on `project.el` to track the active project root. `projection` ensures this detection is fast and accurate.
- **Dirvish/Dired:** The custom `ar/dired-strip-unsupported-ls-flags` and `project-dired` commands rely on `project.el` roots. The commented-out `persp-before-switch-functions` hook in Dirvish **MUST BE PERMANENTLY EXCISED** and replaced with `bufferlo`'s native window management.
- **Ghostel (Terminal):** `ghostel-project` uses `project.el` to spawn terminals in the correct project root. It must be added to `project-switch-commands`.

### B. Completion Framework (Consult, Vertico, Marginalia)

- **`consult-buffer`:** Must use `(setq consult-buffer-list-function #'bufferlo-local-buffers)` in the `bufferlo` `:config` block.
- **`consult-dir`:** `consult-dir-project-list-function` should be mapped to pull from `project.el`'s known projects (managed by `projection`), bypassing `consult-projectile`.
- **`consult-ripgrep` / `consult-find`:** These natively use `project-current` to determine the search boundary. `projection` ensures this boundary is correctly identified.

### C. Development Tools (LSP, Envrc, DAP)

- **`envrc` (Direnv):** The `envrc` package uses `project.el` to locate `.envrc` files when `project-root` is detected.
- **`lsp-mode`:** `lsp-mode` relies on `project-current` to determine the workspace root for spawning language servers. `projection`'s accurate root detection prevents LSP from spawning in the wrong directory.

### D. Keybindings (General.el Leader Paradigm)

The following Doom-style leader bindings must be routed via `ar/global-leader` strictly _outside_ the `use-package` blocks:

- `SPC b` (Buffers): `ibuffer`, `consult-buffer`, `kill-buffer`.
- `SPC p` (Projects): `project-switch-project` (`p p`), `projection-multi-compile` (`p c`), `project-find-file` (`p f`), `ar/find-file-in-emacs-config` (`f p`).
- `SPC TAB` (Workspaces): `bufferlo-switch-to-buffer`, `bufferlo-new-tab`, `bufferlo-close-tab`.

---

## 7. Load-Order Physics & Elpaca Directives (Instructions for Next LLM)

When generating the `#+begin_src emacs-lisp` blocks for these subsections, the next LLM must adhere to these strict load-order physics:

1.  **`project.el` is Built-in:** It does not need a `use-package` block, but `projection` must be loaded `:after project` or explicitly required in `:config` to ensure its extensions patch `project.el` correctly.
2.  **`bufferlo` requires `tab-bar`:** `tab-bar-mode` must be initialized (even if hidden via `tab-bar-show nil`) before `bufferlo` attempts to manage workspaces.
3.  **`consult` Integration Timing:** The injection of `(setq consult-buffer-list-function #'bufferlo-local-buffers)` MUST happen in a `with-eval-after-load 'consult` block within the `bufferlo` `:config`, otherwise the variable will be unbound or overwritten by Consult's defaults.
4.  **`ibuffer` Hook Timing:** `ibuffer-projection-set-filter-groups` must be called via `ibuffer-mode-hook`, not globally, to prevent errors when `ibuffer` is invoked before a project is detected.
5.  **Header Status:** Per `system_prompt_protocol.yaml`, all three headers (`** TODO Buffer Management`, `** TODO Project Management`, `** TODO Workspaces`) MUST remain `TODO` until the user explicitly issues the `GREEN LIGHT` and the exact phrase `change status from TODO to DONE`.
6.  **No Package Merging:** Each package (`ibuffer`, `nerd-icons-ibuffer`, `projection`, `projection-multi`, `bufferlo`) MUST have its own isolated `use-package` block, even if they are conceptually grouped under the same Org header.
7.  **`project-switch-commands` Syntax:** When adding commands to the project dispatch menu, use the exact syntax: `(add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep") t)`.

---

_End of Architectural Blueprint v2. This document contains the verified, hallucination-free structural logic required to generate the v0.9 Emacs Lisp implementation for the Workflow Management Context Engine._
