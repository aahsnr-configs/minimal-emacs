I am locked in the **READ-ONLY planning state**. Awaiting explicit `GREEN LIGHT` to execute the rewrite.

Below is the comprehensive architectural plan, load-order physics, and Doom Emacs parity mapping for the 1st iteration of the **Project Management** subsection. This plan strictly adheres to the v5 `system_prompt_protocol.yaml`, the `elpaca_package_manager.yaml`, and the `workflow_management.yaml` (v3) triad blueprint.

### 1. The Core Engine: Native `project.el` (Built-in)

To achieve Doom Emacs / Projectile parity without the 6000-line monolith and background indexing daemon, we must heavily configure Emacs' native `project.el` to recognize the same project roots that Projectile did.

- **Root Detection Parity (`project-vc-extra-root-markers`):** By default, `project.el` only recognizes VCS roots (`.git`, `.hg`, etc.). To match Projectile's out-of-the-box experience, we will inject common build/dependency files (e.g., `Makefile`, `package.json`, `Cargo.toml`, `pyproject.toml`, `.projectile`, `.dir-locals.el`) into `project-vc-extra-root-markers` .
- **The Dispatch Menu (`project-switch-commands`

  ):** When you invoke `project-switch-project` (`SPC p p`), Emacs presents a dispatch menu. We will enrich this alist to include `project-find-file`, `project-find-regexp`, `project-dired`, `project-eshell`, `magit-project-status`, `ghostel-project`, and `projection-multi-compile` [[9], [33]].

- **Consult Integration (`consult-project-function`):** We will explicitly map `consult-project-function` to `#'project-current` . This mathematically guarantees that `consult-ripgrep`, `consult-fd`, and `consult-buffer` automatically restrict their scope to the current project when invoked from a project buffer.

### 2. The Extension Layer: `projection` (MELPA)

`projection` acts as the pluggable backend that extends `project.el` with IDE-like features .

- **`global-projection-hook-mode`:** Automatically applies project-specific hooks to buffers.
- **`projection-customize-compilation-mode`:** Injects project-specific build paths into `compilation-search-path` and error regexes into `compilation-error-regexp-alist`, allowing seamless `M-x next-error` navigation through CMake/Make build logs.
- **Find Other File (`projection-find-other-file`):** Replaces `projectile-find-other-file`. It natively supports jumping between C++ `.h`/`.cpp`, Python `src`/`test`, and other structural associations without extra configuration.

### 3. Multi-Target Compilation: `projection-multi` & `projection-multi-embark`

This strictly replaces `projectile-compile-project` and `helm-make`.

- **`projection-multi`:** Interrogates the project type (CMake, Make, Tox, Poetry, Cargo) and dynamically extracts available compilation targets, presenting them in a `consult` powered minibuffer menu.
- **`projection-multi-embark`

  :** Integrates with `embark-act` (`C-;`), allowing you to select a compilation target from the minibuffer and immediately set it as the default build/test command for the project via Embark actions [[41], [46]].

### 4. Doom Emacs Parity & Keybindings (`general.el`)

We will route all bindings through `ar/global-leader` strictly outside of `use-package` closures to prevent deferred-registration traps.

- **`SPC p p`** $\rightarrow$ `project-switch-project` (Switch project & summon dispatch menu).
- **`SPC p f`** $\rightarrow$ `project-find-file` (Find file in project).
- **`SPC p s`** $\rightarrow$ `consult-ripgrep` (Search project).
- **`SPC p b`** $\rightarrow$ `consult-project-buffer` (Switch project buffer).
- **`SPC p d`** $\rightarrow$ `project-dired` (Dired in project root).
- **`SPC p e`** $\rightarrow$ `project-eshell` (Eshell in project root).
- **`SPC p t`** $\rightarrow$ `ghostel-project` (Terminal in project root).
- **`SPC p c`** $\rightarrow$ `projection-multi-compile` (Multi-target compile).
- **`SPC p C`** $\rightarrow$ `project-compile` (Standard compile).
- **`SPC p o`** $\rightarrow$ `projection-find-other-file` (Jump header/impl/test).
- **`SPC p r`** $\rightarrow$ `projection-reset-project-cache` (Reset cached commands).
- **`SPC f p`** $\rightarrow$ `ar/find-file-in-emacs-config` (Custom O(1) wrapper that let-binds `default-directory` to `user-emacs-directory` and summons `consult-fd` or `find-file`).

### 5. Triad Integration Physics

- **Buffer Management (`ibuffer`):** Already finalized. `projection-ibuffer` queries `project.el` to dynamically group buffers.
- **Workspaces (`bufferlo`):** Already finalized. `bufferlo-isolate-project` (`SPC TAB p`) natively uses `project-current` to spawn a hidden `tab-bar` workspace strictly for the current project.
- **Dirvish / Treemacs:** Both natively rely on `project.el` for root detection and follow-modes. `projection` ensures this detection is fast and accurate across diverse project types.

### 6. Elpaca Load-Order Physics

1.  **`project`**: Built-in. `:ensure nil`. Configured via `:custom` and `:config`.
2.  **`projection`**: MELPA. `:ensure t`. Must be loaded `:after project` to ensure its `eioio` project-type definitions successfully patch `project.el`.
3.  **`projection-multi`**: MELPA. `:ensure t`. `:after projection`.
4.  **`projection-multi-embark`**: MELPA. `:ensure t`. `:after (projection-multi embark)`. Demands immediate setup of its command map.
5.  **`consult` Integration**: Must be wrapped in `(with-eval-after-load 'consult ...)` inside the `project` block to prevent `void-variable` errors if Consult loads asynchronously after `project.el`.

### 7. Proposed Structure for the `config.org` Subsection

When you issue the `GREEN LIGHT`, I will generate the following isolated blocks under the `** TODO Project Management` header:

1.  **`=project=`**: Core Emacs project engine, root markers, dispatch menu, and Consult integration.
2.  **`=projection=`**: Project type extensions, compilation path injection, and global hook mode.
3.  **`=projection-multi=`**: Multi-target compilation engine.
4.  **`=projection-multi-embark=`**: Embark integration for build targets.
5.  **Custom Elisp**: The `ar/find-file-in-emacs-config` wrapper.
6.  **`=general.el=`**: Centralized `SPC p` and `SPC f p` leader keybindings.

I am locked in the **READ-ONLY planning state**. Awaiting your verdict on this architectural plan and your explicit `GREEN LIGHT` to execute the rewrite.
