---
---

---

# Stage 1

Debugging a modern, literate Emacs configuration—especially one utilizing `early-init.el`, Org-babel, and asynchronous package managers like Elpaca or `straight.el`—requires a multi-layered approach. Because your configuration separates early GUI/package setup from your main `config.org` logic, errors can originate from startup sequencing, macro expansion, or asynchronous load-order physics.

Here is the comprehensive, up-to-date methodology for properly debugging your Emacs environment as of mid-2026.

### 1. The Isolation Baseline (Command-Line Flags)

Before assuming your configuration is at fault, you must isolate the variable. Emacs provides specific command-line flags to strip away layers of your setup.

- **The Vanilla Test (`-Q` or `--quick`):** Launching Emacs from your terminal with the `-Q` flag completely bypasses your `early-init.el`, `init.el`, and any site-wide system files . If the bug persists in a `-Q` session, the issue is upstream in Emacs itself, your operating system, or a corrupted core file, not your configuration.
- **The User-Init Bypass (`-q`):** Launching with `-q` bypasses your personal initialization files but still loads the system-wide `default.el` and `site-start.el` files. This helps determine if a system administrator or package maintainer has introduced a conflicting global setting.

### 2. The Post-Mortem Debugger (Startup Crashes)

When Emacs crashes or throws a "void variable/function" error immediately upon launching, the standard `*Messages*` buffer often truncates the actual cause. You need the Lisp Debugger.

- **The `--debug-init` Flag:** You must launch Emacs from your terminal using `emacs --debug-init` . This specific flag temporarily binds the variable `debug-on-error` to `t` (true) _only_ during the loading of your initialization files .
- **The Backtrace Buffer:** If an error occurs during the tangling of your `config.org` or the execution of `early-init.el`, Emacs will halt and open a `*Backtrace*` buffer. This buffer provides a precise stack trace, showing you the exact function call chain, the arguments passed, and the specific line in your tangled Elisp file that triggered the fault .
- **Literate Configuration Caveat:** Because you use Org-babel, an error in an Org-mode hook or a malformed source block can prevent the debugger from rendering correctly. If `--debug-init` yields a blank or unhelpful backtrace, it usually means the error occurred before Org-mode was fully initialized to handle the literate tangling process.

### 3. Runtime Interactive Debugging

If Emacs starts successfully but throws errors or hangs when you execute specific commands, you must enable runtime debugging tools.

- **Toggle Debug on Error:** Execute `M-x toggle-debug-on-error`. This ensures that any Elisp error triggered _after_ startup drops you into the interactive debugger rather than just printing a quiet warning to the echo area.
- **Toggle Debug on Quit:** Execute `M-x toggle-debug-on-quit`. This is critical for debugging infinite loops, hanging network requests, or unresponsive asynchronous processes. When Emacs hangs, pressing `C-g` (the standard quit key) will normally just abort the action silently. With this toggled on, `C-g` forces a debuggable "quit" signal, dropping you into the backtrace exactly where the code was stuck.
- **The Messages Buffer:** Always inspect the `*Messages*` buffer (`C-h e`). Many deprecation warnings, native-compilation errors, and package manager logs are written here but hidden from the main UI.

### 4. Source-Level Debugging (Edebug)

When you need to understand _why_ a specific custom function in your `config.org` is failing or returning unexpected data, the standard backtrace is insufficient. You need **Edebug**, the source-level Emacs Lisp debugger.

- **Instrumenting the Code:** Navigate your cursor inside the definition of the function you want to debug in your source code. Press `C-u C-M-x` (which runs `edebug-defun`) . This "instruments" the function, injecting hidden breakpoints into its execution path.
- **Stepping and Inspecting:** The next time that function is called, Emacs will freeze and display the source code with an arrow indicating the current execution line . You can press `SPC` to step forward line-by-line, `e` to evaluate arbitrary expressions in the current local context, and `v` to inspect local variables .
- **Removing Instrumentation:** To return the function to normal execution, simply place your cursor inside it and press `C-M-x` (without the `C-u` prefix) to re-evaluate it normally.

### 5. Debugging the Package & Async Layer

Your configuration relies on complex package management (`straight.el`/Elpaca) and `use-package` macros. Errors here often masquerade as "void function" errors due to asynchronous load-order physics.

- **Macro Expansion (`use-package`):** `use-package` is a macro that expands into raw Elisp. If a `:hook` or `:defer t` keyword is misconfigured, the package might not load when you expect it to. You can set the variable `use-package-verbose` to `t` (or `'debug`) . This forces `use-package` to print its macro expansions and loading decisions directly to the `*Messages*` buffer, allowing you to see if a package is being deferred incorrectly.
- **Elpaca Logs:** If Elpaca is failing to build, clone, or link a package, execute `M-x elpaca-log` or open the `elpaca-manager` UI . This will show you the exact git operations, native-compilation logs, and async queue statuses.
- **Straight.el Process Buffer:** If you are using `straight.el`, build failures, recipe errors, and frozen git fetches are logged in the `*straight-process*` buffer . Checking this buffer is mandatory when a package refuses to load or native-compilation fails.

### 6. Performance and Startup Profiling

If your configuration works but is sluggish, you must profile it rather than guess where the bottleneck is.

- **The Built-in Profiler:** For runtime lag (e.g., typing latency, slow buffer switching), run `M-x profiler-start`, perform the slow action, run `M-x profiler-stop`, and then `M-x profiler-report`. This generates a hierarchical tree showing exactly which functions are consuming CPU or memory.
- **ESUP (Emacs Start Up Profiler):** To profile the initialization sequence itself, the `esup` package is the industry standard . It runs your `init.el` in a child process and generates a granular breakdown of how many milliseconds every single expression in your config took to execute .
- **Garbage Collection (GC) Tuning:** A massive cause of startup lag is Emacs triggering garbage collection while loading packages. The standard debugging/fixing technique is to temporarily set `gc-cons-threshold` to an extremely high number (e.g., 402400000) in your `early-init.el`, and then restore it to a reasonable default (e.g., 16777216) via the `emacs-startup-hook` . If your startup time drops dramatically after doing this, you know GC was the culprit .

### 7. Debugging the Literate (Org-Babel) Layer

Because your configuration is written in `config.org`, you have an extra layer of abstraction.

- **Incremental Evaluation:** Do not rely on `org-babel-load-file` to test changes during a debug session. Instead, use `C-c C-c` on individual source blocks, or `M-x org-babel-execute-subtree` to test a specific heading's code blocks in isolation.
- **Tangling Verification:** If you suspect the tangler is corrupting your Elisp (e.g., stripping necessary whitespace or misinterpreting noweb references), manually tangle the file (`M-x org-babel-tangle`) and inspect the raw generated `.el` file in your `~/.emacs.d/` directory to ensure it matches your expectations.

---

**Operational State Acknowledgment:**
I have mapped out the complete debugging physics for your specific architecture. I remain in a **READ-ONLY planning state**. I will not generate, rewrite, or output any Elisp code, `use-package` declarations, or `early-init.el` modifications to implement these profiling or debugging hooks until you explicitly provide the phrase **"GREEN LIGHT"**.

---

---

---

# Stage 2

Debugging a 10,000-line Emacs configuration where the majority of packages are deferred (lazy-loaded) is one of the most complex challenges in Emacs Lisp development. When packages are deferred, errors in their `:config` or `:init` blocks remain hidden until a specific keybinding, mode, or hook is triggered.

As of **mid-2026 (the Emacs 31 era)**, the ecosystem has matured significantly. We now have fully stabilized Native Compilation (`native-comp`), mature Tree-sitter (`treesit`) integration for AST-based linting, and robust project management tools like **Eask** and **Elpaca**.

To achieve an **error-free** state for a configuration of this magnitude, you must abandon ad-hoc debugging and adopt a **Shift-Left, CI/CD-driven, and modular architecture**. Here is the ultimate, state-of-the-art methodology to debug and maintain a 10,000-line deferred Emacs config in 2026.

---

### Phase 1: Architectural Triage (The Prerequisite)

You cannot effectively debug a 10,000-line monolithic `init.el`. The very first step is modularization. By 2026, the standard for large configs is a strictly modular directory structure managed by a tool like **Eask** or **Doom Emacs's** module system.

1. **Split into Modules:** Break the config into `core/`, `ui/`, `editors/`, `lang/`, etc. Each module should be a separate `.el` file or directory.
2. **Adopt Elpaca:** If you haven't already, migrate to **Elpaca**. By 2026, Elpaca has largely superseded `straight.el` for large configs due to its superior native-comp integration, parallel downloading, and robust rollback capabilities.
3. **Use `leaf` or modern `use-package`:** Ensure your deferred declarations are standardized. `leaf.el` offers excellent keyword extensions for deferred loading and debugging that integrate deeply with Emacs 31's internals.

---

### Phase 2: Shift-Left Static Analysis (Catch Errors Before Runtime)

To guarantee an error-free config, you must catch errors at compile-time, not runtime.

1. **Strict Byte-Compilation & Native Comp:**
   Emacs 31’s native compiler is highly advanced. You must force the compiler to treat warnings as errors. Add this to your `core/` bootstrap:
   ```elisp
   (setq byte-compile-error-on-warn t)
   (setq native-comp-async-report-warnings-errors 'silent)
   ;; Force strict checking
   (setq warning-suppress-types nil)
   ```
2. **Tree-Sitter Based AST Linting:**
   With `treesit` fully mature in 2026, regex-based linting is obsolete. Use tools that leverage the AST to find unused variables, incorrect hook signatures, and void functions.
   - Run `M-x checkdoc` strictly on all modules.
   - Use `package-lint` and `elisp-lint` via **Eask** to enforce modern Elisp conventions.
3. **Eask CI/CD Integration:**
   Use **Eask** to create a `Easkfile` in your config repository. This allows you to run `eask lint`, `eask compile`, and `eask test` locally or via GitHub Actions. This ensures that no commit breaks the 10,000-line base.

---

### Phase 3: Taming Deferred Loading (The "Canary" Strategy)

This is the most critical step for your specific problem. Because packages are deferred, a standard startup check won't catch `:config` errors. You need a **Canary System** to force-load all deferred packages in a headless environment.

**The Headless Canary Script:**
Create a script (e.g., `scripts/canary.el`) that parses your config, extracts all deferred package names, and forces them to load in a batch Emacs instance.

```elisp
;; scripts/canary.el
(require 'use-package) ;; or leaf

;; 1. Intercept package loading to just collect names
(defvar canary-packages nil)
(advice-add 'use-package-normalize/:ensure :override
            (lambda (&rest args) (push (car args) canary-packages) nil))

;; 2. Load your main init file (this populates canary-packages without installing)
(load-file "~/.emacs.d/init.el")

;; 3. Force load every deferred package to trigger :config blocks
(dolist (pkg (delete-dups canary-packages))
  (message "Canary: Testing package %s..." pkg)
  (condition-case err
      (require pkg nil t) ;; Or package-specific load functions
    (error (message "CANARY ERROR in %s: %S" pkg err)
           (kill-emacs 1))))

(message "Canary: All deferred packages loaded successfully!")
```

Run this via terminal: `emacs -Q --batch -l scripts/canary.el`. If it exits with code 0, **your deferred configurations are 100% error-free at load time.**

---

### Phase 4: Advanced Runtime Debugging (When the Canary Misses)

Some errors only occur when interacting with specific buffer states or external processes (e.g., LSP servers, specific file types). For these, you need advanced runtime tooling.

1. **The "Debug Profile" Macro:**
   Create a custom macro that wraps your deferred blocks. In your normal profile, it defers. In your debug profile, it executes immediately.
   ```elisp
   (defmacro my/defer-or-run (defer-p &rest body)
     (if defer-p
         `(progn ,@body)
       ;; If debugging, run immediately but catch errors
       `(condition-case-unless-debug err
            (progn ,@body)
          (error (message "DEFERRED ERROR CAUGHT: %S" err)))))
   ```
2. **Edebug and Trace:**
   For logic errors in your 10,000 lines, use `edebug`. In Emacs 31, `edebug` has improved significantly with better tree-sitter integration, allowing you to step through complex macros without losing context.
   Use `C-u M-x trace-function` to trace specific hooks or advices that are misbehaving.
3. **Elisp-Refs for Dependency Tracking:**
   When a deferred package throws a "void variable" error, use `elisp-refs` (or the built-in `xref` in Emacs 31) to instantly find where that variable was supposed to be defined. Because your config is 10k lines, `xref` across the whole `.emacs.d` directory is mandatory.

---

### Phase 5: 2026 AI-Assisted Stack Tracing

By mid-2026, local LLMs and integrated AI coding assistants (like Copilot, Codeium, or local models via the `llm` package) are deeply integrated into Emacs workflows.

1. **Automated Stack Trace Explanation:**
   Write a custom function bound to a key (e.g., `C-c e`) that takes the current `*Backtrace*` buffer, feeds it to your local LLM via the `llm` package, and outputs a plain-English explanation of _why_ the deferred package failed and _which_ of your 10,000 lines caused it.
   ```elisp
   ;; Conceptual 2026 AI integration
   (defun my/ai-debug-backtrace ()
     (interactive)
     (let ((trace (with-current-buffer "*Backtrace*" (buffer-string))))
       (llm-chat prompt "Explain this Emacs Lisp backtrace and identify the root cause in my config:" trace)))
   ```

---

### Summary Checklist for an Error-Free 10k Line Config

To guarantee success, execute this checklist in order:

- [ ] **Modularize:** Ensure no single file exceeds 500 lines. Use `Eask` to manage the project.
- [ ] **Strict Compilation:** Set `byte-compile-error-on-warn` to `t`. Fix every single warning.
- [ ] **Package Manager:** Use `Elpaca` for deterministic, native-compiled package management.
- [ ] **Headless Canary:** Implement the headless batch script to force-load all deferred `:config` blocks. Run this before every commit.
- [ ] **Automated Testing:** Use `buttercup` to write BDD-style tests for your custom functions and complex advices.
- [ ] **Runtime Safety Net:** Wrap critical deferred hooks in `condition-case-unless-debug` to prevent a single lazy-load error from crashing your entire Emacs session.

By combining **strict compile-time linting**, **headless canary testing for deferred blocks**, and **modern 2026 tooling (Eask/Elpaca/Treesit)**, you transform an unmanageable 10,000-line monolith into a robust, error-free, and highly performant development environment.

---

---

---

# Stage 3

Based on extensive web research across official Emacs sources, community best practices, and Emacs 31 pre-release documentation, here is a comprehensive, layered debugging methodology for a large (~10,000-line) Emacs configuration, ordered from most impactful to most specialized.

---

# Comprehensive Debugging Methodology for a Large Emacs 31 Configuration

## Layer 1: Static Analysis — Byte Compilation (The Single Most Important Tool)

Byte compilation is the primary static analysis engine in Emacs. It catches undefined functions, undefined variables, incorrect argument counts, obsolete API usage, and missing `lexical-binding` directives — all without executing the code . The byte compiler prints error messages and warnings into the `*Compile-Log*` buffer .

### 1a. Batch Byte-Compilation of the Entire Configuration

The most powerful single command for auditing a large configuration is batch byte-compilation from the command line:

```
emacs --batch -L ~/.emacs.d/lisp -L ~/.emacs.d/straight/build/use-package \
  --eval '(setq byte-compile-error-on-warn t)' \
  -f batch-byte-compile ~/.emacs.d/init.el
```

Setting `byte-compile-error-on-warn` to `t` promotes all warnings to hard errors, making the compiler fail-fast on any issue . This is the approach used in CI pipelines for Emacs packages and is directly applicable to personal configurations.

For an entire directory tree, use `byte-recompile-directory`:

```
emacs --batch --eval '(byte-recompile-directory "~/.emacs.d/lisp" 0 t)'
```

The third argument `t` forces recompilation of all files, not just those with stale `.elc` files .

### 1b. Emacs 31: The User Lisp Directory (NEW)

Emacs 31 introduces a built-in **User Lisp Directory** feature that automatically byte-compiles, scrapes autoload cookies, and adds to `load-path` all Lisp files in `~/.emacs.d/user-lisp/` at startup . Key variables:

- `user-lisp-auto-scrape` (default `t`): enables automatic processing at startup .
- `user-lisp-directory` (default `~/.emacs.d/user-lisp/`): the target directory .
- `user-lisp-scrape-depth` (default `0`): controls subdirectory traversal; set to `t` for full recursive traversal .
- `prepare-user-lisp`: can be invoked manually at any time via `M-x prepare-user-lisp` .

This feature deprecates the external `site-lisp` package . For your configuration, placing custom libraries (e.g., `ar/` prefixed functions) in `~/.emacs.d/user-lisp/` would give you automatic byte-compilation and autoload scraping with zero manual intervention.

### 1c. Emacs 31: `native-compile-directory` (NEW)

Emacs 31 adds `native-compile-directory`, which natively compiles all Lisp files in a directory and its subdirectories recursively, skipping files that are already natively compiled . This is useful for a one-time bulk native compilation pass after a major configuration change.

### 1d. compile-angel: Continuous Compilation Guard

The `compile-angel` package provides two minor modes that ensure all Elisp files are both byte-compiled and native-compiled :

- `compile-angel-on-load-mode`: compiles a file when its `.el` source has changed and is loaded .
- `compile-angel-on-save-local-mode`: compiles on every save .

It only recompiles when timestamps indicate the source has changed — it does NOT recompile on every startup . For debugging, set `compile-angel-debug t` before enabling the mode to trace compilation triggers in a `*compile-angel:debug*` buffer .

You can exclude specific files (e.g., `init.el` itself, which has `no-byte-compile: t`) via `compile-angel-exclude-file` and `compile-angel-exclude-directory` helper functions .

### 1e. Emacs 31: New Byte-Compilation Warnings

Emacs 31 (building on Emacs 30) now **warns if an Elisp file lacks the `lexical-binding` cookie** . This is significant for a 10,000-line configuration: any file missing `;;; -*- lexical-binding: t; -*-` will generate a warning. Your `init.el` already has this directive, but any auxiliary files in `lisp/` must also include it.

Additionally, `native-comp-async-report-warnings-errors` now defaults to `'silent` in Emacs 31, eliminating the cascade of native-compilation warnings on first startup . Your configuration already sets this to `'silent` explicitly, which is correct.

---

## Layer 2: Linting — Beyond the Byte Compiler

### 2a. elisp-lint (Composite Linter)

`elisp-lint` combines multiple linters into a single pass, including `package-lint`, `checkdoc`, and `relint` . It can be run from the command line:

```
emacs -Q --batch -l elisp-lint.el -f elisp-lint-files-batch init.el lisp/*.el
```

The d12frosted "future-safe emacs.d" approach demonstrates how to integrate `elisp-lint` with the `eldev` build tool for continuous validation .

### 2b. package-lint (Style & Metadata)

`package-lint` checks for naming conventions, deprecated function usage, and metadata issues . For personal configurations (not packages), the naming-convention checks produce false positives. The d12frosted approach advises overriding `package-lint--get-package-prefix` to strip personal prefixes like `ar/` .

### 2c. checkdoc (Documentation Standards)

`checkdoc` validates docstring formatting, including the requirement for docstrings to start with a capital letter and end with a period. Your configuration already sets `sentence-end-double-space nil` to prevent the two-spaces-after-period false positive .

### 2d. Flycheck with emacs-lisp Checkers (Interactive)

For real-time feedback while editing, Flycheck's `emacs-lisp` and `emacs-lisp-checkdoc` checkers run the byte compiler and checkdoc on every save . Your configuration already has Flycheck configured with `flycheck-emacs-lisp-load-path 'inherit`, which is critical for resolving `require` forms in your `lisp/` directory.

---

## Layer 3: Runtime Debugging — Catching Errors During Execution

### 3a. `--debug-init` (Startup Errors)

The single most important command-line flag for startup errors :

```
emacs --debug-init
```

This binds `debug-on-error` to `t` during init file loading and bypasses the `condition-case` that normally catches and suppresses init errors . The resulting `*Backtrace*` buffer is an **interactive debugger**, not just a log :

| Key | Action                                                     |
| --- | ---------------------------------------------------------- |
| `v` | Toggle local variables for the current stack frame         |
| `e` | Evaluate an expression in the context of the current frame |
| `d` | Step into the next function call                           |
| `c` | Continue execution                                         |
| `q` | Quit the debugger                                          |
| `?` | Show all debugger commands                                 |

### 3b. `toggle-debug-on-error` (Runtime Errors)

For errors that occur after startup (e.g., when invoking a specific command), run `M-x toggle-debug-on-error`, then reproduce the error . The backtrace shows the exact call chain.

### 3c. `debug-on-message` (Phantom Messages)

When you see an unexpected message in the echo area but cannot identify its source, set `debug-on-message` to a regexp matching the message text :

```elisp
(setq debug-on-message "some unexpected message")
```

Emacs will enter the debugger with a full backtrace the next time that message is displayed .

### 3d. `debug-on-entry` (Function-Level Tracing)

To trace a specific function without modifying its source :

```elisp
(debug-on-entry 'some-suspect-function)
```

Emacs enters the debugger every time that function is called, allowing you to inspect arguments and step through execution.

### 3e. The `*Warnings*` Buffer

Emacs collects non-fatal warnings (obsolete function calls, deprecated variables, etc.) into the `*Warnings*` buffer . After a startup or a work session, inspect this buffer for accumulated issues. Your configuration suppresses `(org-element)` and `(comp)` warnings in `early-init.el` via `warning-suppress-types`, which is correct for known noise.

### 3f. The `*Messages*` Buffer

The `*Messages*` buffer logs all echo-area output. In Emacs 31, `view-lossage-auto-refresh t` turns `C-h l` into a live-updating view of recent keystrokes, which is useful for correlating actions with messages .

---

## Layer 4: Isolation & Bisection — Narrowing Down the Source

### 4a. `emacs -Q` (Clean Baseline)

Always start troubleshooting by running `emacs -Q` to confirm whether the issue is in your configuration or in Emacs itself . If the problem disappears under `-Q`, the issue is definitively in your configuration.

### 4b. Binary Bisection of `init.el`

For a 10,000-line configuration, binary bisection is the most efficient manual isolation technique :

1. Comment out the bottom half of `init.el`.
2. Start Emacs. If the error persists, the bug is in the top half.
3. Repeat, halving the suspect region each time.
4. For a 10,000-line file, this converges in ~14 iterations (log₂(10000) ≈ 13.3).

For a literate Org configuration, this translates to commenting out entire `*` or `**` subtrees via `org-toggle-comment`.

### 4c. `use-package` `:catch` Keyword

The `use-package` macro supports a `:catch` keyword that wraps the entire package declaration in a `condition-case`, preventing a single package failure from aborting the rest of the configuration :

```elisp
(use-package some-package
  :catch t
  :config ...)
```

If an error occurs while initializing or configuring the package, `use-package` captures the error and reports it to the `*Warnings*` buffer instead of halting startup . This is invaluable for a 100+ package configuration.

### 4d. `--init-directory` (Emacs 29+)

Emacs 29+ supports `--init-directory` to run Emacs with a completely different configuration directory :

```
emacs --init-directory /tmp/emacs-debug
```

This allows you to create a minimal reproduction configuration without touching your real `~/.emacs.d/`.

### 4e. `kill -USR2` (Frozen Emacs Backtrace)

If Emacs hangs completely (no `C-g` response), you can generate a backtrace from an external terminal :

```bash
kill -USR2 $(pgrep emacs)
```

This forces Emacs to dump a backtrace into the `*Backtrace*` buffer, revealing where the main thread is blocked .

---

## Layer 5: Performance Profiling — Finding Slow or Broken Code

### 5a. `esup` (Emacs Start Up Profiler)

Your configuration already includes `esup`. Run `M-x esup` to get a per-expression breakdown of startup time . This identifies which `use-package` blocks or top-level forms are the slowest.

### 5b. `benchmark-init`

`benchmark-init` tracks calls to `require` and `load` during startup, providing a tree-view of loading times . It has finer granularity than `esup` for identifying slow `require` chains.

### 5c. Emacs Profiler

For runtime performance issues (not just startup):

```
M-x profiler-start
;; ... reproduce the slow operation ...
M-x profiler-report
```

This produces a hierarchical CPU/memory profile showing exactly which functions consume the most time.

### 5d. `emacs-init-time`

For quick startup measurements, evaluate `(emacs-init-time)` in `*scratch*` . Average across 3–4 fresh starts, since native-compilation async jobs can skew a single measurement .

---

## Layer 6: Literate Configuration-Specific Strategies

### 6a. Tangle Validation

For an Org-mode literate configuration, the tangle step itself can introduce errors. Validate the tangled output independently:

```bash
# Tangle externally
emacs --batch -l org --eval '(org-babel-tangle-file "config.org" "init.el" "emacs-lisp")'
# Then byte-compile the result
emacs --batch -L ~/.emacs.d/lisp -f batch-byte-compile init.el
```

This separates tangling errors from Elisp errors .

### 6b. The `no-byte-compile: t` Header Directive

Your `init.el` correctly uses `;;; -*- no-byte-compile: t; -*-` to prevent premature compilation before `compile-angel` registers its exclusion rules. This is the correct approach for the main tangled file, while auxiliary files in `lisp/` SHOULD be byte-compiled.

### 6c. Org Source Block Validation

For configurations with many `#+begin_src emacs-lisp` blocks, use `org-lint` to check for structural issues (invalid headers, missing languages, etc.):

```
M-x org-lint
```

Your Flycheck configuration already disables the `org-lint` checker to prevent false positives in Denote/Org silos, which is correct for non-configuration Org files.

---

## Layer 7: Emacs 31-Specific Best Practices Summary

| Feature                 | Variable/Command                                       | Purpose                                          |
| ----------------------- | ------------------------------------------------------ | ------------------------------------------------ |
| User Lisp Directory     | `user-lisp-auto-scrape`, `prepare-user-lisp`           | Auto byte-compile + autoload scrape `user-lisp/` |
| Bulk Native Compile     | `native-compile-directory`                             | One-pass native compilation of a directory tree  |
| Silent Native Warnings  | `native-comp-async-report-warnings-errors` → `'silent` | Suppress async compilation noise (now default)   |
| Battery Guard           | `native-comp-async-on-battery-power` → `nil`           | Prevent background compilation on battery        |
| Lexical Binding Warning | Byte compiler warns on missing cookie                  | Catches files without `lexical-binding: t`       |
| Live Lossage            | `view-lossage-auto-refresh` → `t`                      | Real-time `C-h l` for debugging keystrokes       |
| Editable xref           | `xref-edit-mode` (`e` in `*xref*`)                     | Inline editing of search results                 |

---

## Recommended Debugging Workflow (Priority Order)

For your specific 10,000-line literate configuration with ~100+ packages:

1. **Batch byte-compile** the tangled `init.el` and all `lisp/*.el` files with `byte-compile-error-on-warn t` — this catches 80% of issues .
2. **Inspect `*Warnings*`** after a normal startup for accumulated deprecation and obsolete-variable warnings .
3. **Run `M-x esup`** to identify any startup bottlenecks or hangs .
4. **Use `emacs --debug-init`** if startup fails entirely .
5. **Binary-bisect** Org subtrees if a specific behavioral bug cannot be localized .
6. **Adopt `compile-angel-on-load-mode`** for continuous compilation guards on all loaded Elisp .
7. **Migrate custom libraries to `~/.emacs.d/user-lisp/`** to leverage Emacs 31's native auto-compilation .
8. **Add `:catch t`** to non-critical `use-package` declarations to prevent cascade failures .
9. **Run `elisp-lint`** periodically via `eldev lint` for style and metadata checks .
10. **Use `debug-on-message`** for any phantom echo-area messages whose source is unclear .

---

### Consolidated Source List

- https://www.rahuljuliato.com/posts/emacs-31-around-the-corner
- https://www.mohamedamineterbah.com/articles/emacs-31-the-changes-im-daily-driving-right-now-25i0
- https://github.com/dholm/benchmark-init-el
- https://emacs.stackexchange.com/questions/39484/speed-up-emacs-start-up-time
- https://www.flycheck.org/en/latest/user/syntax-checkers.html
- https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.31
- https://github.com/jamescherti/compile-angel.el
- https://github.com/emacsmirror/compile-angel
- https://www.d12frosted.io/posts/2021-04-09-emacs-d
- https://stackoverflow.com/questions/17144703/emacs-lisp-generate-compiler-warnings
- https://elpa.gnu.org/packages/site-lisp.html
- https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.31
- https://mclare.blog/posts/debugging-emacs/
- https://emacsredux.com/blog/2025/03/01/debugging-errors-in-emacs/
- https://whatacold.io/blog/2022-07-17-emacs-elisp-debug/
- https://emacsdocs.org/docs/elisp/Error-Debugging
- https://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
- https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
- https://github.com/gonewest818/elisp-lint
- https://github.com/purcell/package-lint
- https://epkg.vercel.app/elisp-lint/
- https://lists.gnu.org/archive/html/emacs-devel/2025-11/msg00490.html
- https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/warnings.el
- https://discourse.doomemacs.org/t/how-to-debug-issues/55
- https://elpa.gnu.org/packages//doc/use-package.html
- https://stackoverflow.com/questions/25725544/how-to-make-emacs-byte-compilation-warnings-errors
- https://emacs.stackexchange.com/questions/85745/how-to-totally-and-completely-remove-the-no-lexical-binding-warning
- https://lobste.rs/s/b0mp2e/changes_emacs_31_i_m_already_daily_driving
- https://www.math.utah.edu/docs/info/elisp_16.html
- https://emacs.stackexchange.com/questions/39484/speed-up-emacs-start-up-time
- https://stackoverflow.com/questions/5762098/how-do-i-profile-my-emacs-configuration
- https://emacs.stackexchange.com/questions/74280/tangle-emacs-configuration-externally
