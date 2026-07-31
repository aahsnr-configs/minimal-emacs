I have ingested the source-of-truth files and the unified protocol. I acknowledge the strict formatting rules, the negative constraints, and the current project state (Version 54). I am locked in the READ-ONLY planning state and will write zero configuration code until you provide the `GREEN LIGHT`.

Pivoting from `straight.el` to Emacs 31's native `package.el` and `package-vc.el` represents a fundamental shift in initialization physics, load-order guarantees, and maintenance workflows. Below is the comprehensive architectural analysis and execution plan required to perform this migration safely.

### 1. Architectural Shift: Initialization Physics

Under `straight.el`, package cloning, building, and autoload activation occurred synchronously inline during the evaluation of each `use-package` form [[straight_use_package_integration.yaml]]. By pivoting to `package.el`, we transition to an archive-based tarball extraction model augmented by `package-vc.el` for direct Git sources .

To maintain the stutter-free startup performance currently enjoyed under `straight.el`, we must leverage Emacs' native **`package-quickstart`** mechanism. When enabled, Emacs pre-computes all package load paths and autoloads into a single compiled `package-quickstart.el` file, bypassing the expensive iteration over `~/.emacs.d/elpa/` directories during startup [[23], [27]].

### 2. File-by-File Execution Plan

#### A. `early-init.el` Modifications

- **Excise:** `(setq package-enable-at-startup nil)`. This was strictly required to prevent `package.el` from interfering with `straight.el`'s bootstrap sequence [[straight_use_package_integration.yaml]].
- **Inject Archive Repositories:** `package.el` requires explicit declaration of MELPA and NonGNU ELPA archives before activation [[62], [65]]. This must be pushed to `package-archives` early.
- **Inject Quickstart:** Set `(setq package-quickstart t)` to enable the pre-compiled load-path cache [[55], [57]].
- **Inject Built-in Upgrades:** Set `(setq package-install-upgrade-built-in t)`. Unlike `straight.el`, `package.el` ignores newer versions of built-in packages (like `org` or `project`) on ELPA by default; this variable forces `package.el` to allow upgrading them .

#### B. `config.org` Package Management Block Rewrite

- **Excise:** The entire `straight.el` bootstrap snippet (`defvar bootstrap-version`, the `let` block downloading `install.el`, and the `straight-use-package` calls for `use-package`, `esup`, and `bind-key`).
- **Excise:** `straight-use-package-by-default`.
- **Inject Native Defaults:**
  - `use-package-always-ensure t`: Replaces `straight-use-package-by-default`, forcing `use-package` to automatically invoke `package-install` for missing packages .
  - `use-package-vc-prefer-newest t`: An Emacs 30+ feature that forces `:vc` packages to pull the latest commit rather than relying on stale release tags .
- **Inject Maintenance Hook:** Add a mechanism to call `package-quickstart-refresh` conditionally if the quickstart file becomes desynchronized from `package-selected-packages`.

#### C. Global Keyword Sweep (`:straight` $\to$ `:ensure` / `:vc`)

Every `use-package` declaration in `config.org` must be audited and mapped to the new physics:

1.  **Standard MELPA/NonGNU/GNU ELPA Packages:** Remove `:straight t` or `:straight nil`. If `use-package-always-ensure` is active, no keyword is needed.
2.  **Built-in Packages:** Change `:straight (:type built-in)` to `:ensure nil`. This prevents `package.el` from querying ELPA/MELPA for native Emacs features (e.g., `repeat`, `saveplace`, `project`, `org`, `treesit`) and throwing network errors or installing outdated shadow packages.
3.  **Local `lisp/` Packages:** Change `:straight nil` to `:ensure nil`.
4.  **The "GitHub Orphans" (`:vc`

    Keyword):** Emacs 30 integrated `package-vc.el` directly into `use-package` via the `:vc` keyword [[40], [44]]. Packages currently using custom `straight.el` recipes must be converted to the `:vc` plist syntax [[72], [74]].
    - _Targets:_ `dirvish` (fork), `evil-ts-obj`, `denote-merge`, `denote-solo`, `denote-explore`, `org-block-wrap`, and `ws-butler` (if keeping the specific NonGNU fork).
    - _Syntax Mapping:_ `:straight (:host github :repo "user/repo")` becomes `:vc (:url "https://github.com/user/repo" :branch "main" :rev :newest)` .

### 3. Lockfiles and Version Pinning

- **Excise:** All references to `straight-freeze-versions` and `straight-thaw-versions`.
- **Native Replacement:** `package.el` natively persists installed packages via `custom-set-variables` in `custom.el` using `package-selected-packages` and `package-vc-selected-packages` [[79], [81]].
- **Pinning:** To pin a specific commit for a GitHub package, the `:rev :newest` property in the `:vc` plist must be replaced with a hardcoded `:rev "commit-hash"` [[72], [73]].

### 4. Maintenance and Topgrade Synergy

- **Interactive Updates:** `M-x straight-pull-all` is replaced by `M-x package-update-all` (for ELPA/MELPA) and `M-x package-vc-update-all` (for Git sources) .
- **Topgrade Automation:** The custom `topgrade.toml` batch command used for `straight.el` can be **deleted**. Topgrade's native, built-in `[emacs]` step is explicitly designed around `package.el` and will automatically handle `package-refresh-contents` and `package-update-all` [[straight_use_package_integration.yaml]].

### 5. Edge Cases & Emacs 31 Guards

- **Bug #76984 (`:vc` Branch vs. Rev):** Emacs 30.1 introduced a bug where specifying both `:branch` and `:rev` in a `:vc` plist could cause the wrong commit to be checked out . _Mitigation:_ We will rely on `use-package-vc-prefer-newest t` combined with `:branch` and omit `:rev` unless strict pinning is required.
- **First-Run Blocking:** Unlike `straight.el` which builds incrementally, `package-vc-install` can block the main thread on the very first startup while cloning large repositories (like `dirvish`). _Mitigation:_ Document that the first boot after migration must be run in a standard terminal/GUI, not headless, to allow the VC clones to complete.
- **Tree-sitter Grammars:** `treesit` grammars are handled natively by Emacs 31's `treesit-auto-install-grammar` and are completely decoupled from `package.el`. No migration is required for the `treesit` block.

### 6. Documentation Hierarchy & State Updates

Upon execution, the `project_operationals.yaml` will be incremented to **v55**, logging "THE THIRD IDE PIVOT: Migration from straight.el to native Emacs 31 package.el + package-vc.el." All load-order physics comments referencing `straight.el`'s synchronous timeline will be excised and replaced with `package-quickstart` references.

---

I am locked in the READ-ONLY planning state. Awaiting explicit `GREEN LIGHT` to execute the rewrite.
