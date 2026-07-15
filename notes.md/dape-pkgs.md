Unlike `dap-mode` (which requires a large ecosystem of helper and adapter packages like `dap-python` or `dap-go`), **`dape`** is designed to be a lightweight, "batteries-included" client. It leverages core Emacs APIs and consolidates all debugger adapters within a single configuration variable (`dape-configs`).

Because of this self-contained design, the packages that complement and enhance `dape` focus on project-level target selection, specialized language or test orchestrations, and interactive user interface (UI) enhancements.

---

### I. Target Selection & Project Integrations

One challenge when using debuggers in Emacs is dynamically identifying build artifacts (like compiled executables or test binaries) to feed into the debugger. These packages bridge that gap:

- **`projection-dape` (Part of the `projection` package on MELPA)**  
  `projection` is a project-management library built on top of the native `project.el`. Its `projection-dape` extension provides a workflow to interactively discover, select, and launch debuggable artifacts within your current project using `dape` configurations.
- **`gotest-ts` (Git Repository / GitHub)**  
  Designed for Go developers, this package uses Tree-sitter to analyze your source code and identify tests or table-driven subtests. It integrates directly with `dape` to automatically generate and run a delve-based `dape` debug session targeting the specific test at your cursor's position.

---

### II. Language-Specific Orchestrators

Some language ecosystems require heavy orchestration (compiling, starting simulators, and managing build directories) before a debugger can attach. These tools automate the background work for `dape`:

- **`swift-development` (Git Repository / GitHub)**  
  Developed by Mikael Konradsson, this package is designed for professional iOS and macOS developers who want to avoid using Xcode. It handles simulator management, build optimization, and caching, and orchestrates `dape` (via `swift-lsp.el` and `xcode-build-config.el`) to compile, launch, and debug apps directly on Apple Simulators or physical devices.

---

### III. UI & Minibuffer Input Enhancements

Because `dape` relies on core Emacs buffers, you can customize and polish its interface using standard completion and visual aids:

- **`corfu` and `cape` (on GNU ELPA / MELPA)**  
  `dape` provides a native Read-Eval-Print Loop (REPL) and interactive evaluation prompts. Users often pair `dape` with `corfu` (for inline completion overlays) and `cape` (to provide Completion-At-Point extensions) to get modern, dropdown-based code auto-completion inside the active `dape` REPL buffer.
- **`eldoc` (Built-in)**  
  While built into Emacs, `eldoc-mode` is explicitly recommended by the author of `dape` to complete its user interface. When `eldoc` is enabled alongside an active `dape` debug session, hovering over variable names in your code buffer automatically displays their evaluated runtime values in the echo area.
- **`repeat-mode` (Built-in)**  
  Another built-in Emacs feature that is critical to the `dape` user experience. Enabling `repeat-mode` allows you to execute sequential stepping commands (like Step Over, Step Into, or Continue) with single-key presses (e.g., hitting `n` or `s` repeatedly) after invoking the initial keyboard prefix, bypassing the need to type the full command prefix every single time.
- **Floating Debugging Toolbars (Community Git Configurations)**  
  A common UI pattern in modern Emacs configurations involves utilizing floating toolbars. Using child-frame libraries (like `posframe`) paired with `nerd-icons`, community scripts provide a small, mouse-interactive debugging panel containing buttons for play, pause, stop, and step commands that floats at the top of your active code window during a `dape` session.
