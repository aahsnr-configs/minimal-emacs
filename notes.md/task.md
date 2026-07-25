# Comprehensive Task Specification: Echo-Area-First Contextual Intelligence Engine

## Target Environment

- **Emacs Version**: pretest 31.0.90 (PGTK build, `--with-native-compilation=aot`)
- **Org Version**: 9.8 (bundled with Emacs 31)
- **Package Manager**: Elpaca v0.12+ with `use-package` integration
- **Modal Editing**: Evil mode with `general.el` leader keybindings
- **Theme**: Tokyo Night (doom-tokyo-night)
- **Date Context**: All information must be current as of July 23, 2026

---

## Core Philosophy

The echo area is Emacs' most underutilized prime real estate. It sits permanently at the bottom of every frame, requires zero window management, and is visible in every buffer. This project transforms it into a **contextual intelligence layer** that provides instant, glanceable spatial and semantic awareness without stealing focus or screen space.

**The 2-Line Rule:**

- **Echo area**: Strictly 1–2 lines of high-density, glanceable information (breadcrumbs, signatures, link targets, property values, clock status)
- **Childframe/popup** (`eldoc-childframe.el`): Reserved for complex, multi-line documentation (>2 lines) such as full function docstrings, LSP hover payloads, and type signatures with generics

Any information exceeding 2 lines in the echo area is **distracting** and must be routed to the childframe. The echo area is for _orientation_; the childframe is for _study_.

---

## Existing Files (Source of Truth)

All files reside in `~/.emacs.d/lisp/` unless otherwise noted:

| File                  | Role                                                                                                                                |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `org-eldoc.el`        | Custom Org-mode ElDoc router (current version handles src-block headers, link targets, property drawers, Babel language delegation) |
| `eldoc-childframe.el` | Custom fork of eldoc-box: Flymake Firewall, Evil spatial debouncing, TTY degradation guards, Corfu collision avoidance              |
| `peek.el`             | Modernized fork (v0.3.0): inline definition/reference panels via overlays, xref-backend bypass, Eldoc Origin Firewall               |
| `org-src-context.el`  | Injects surrounding source blocks into `org-edit-special` buffers for LSP context (Eglot)                                           |
| `breadcrumb.el`       | Upstream (v1.0.1, João Távora): project + imenu breadcrumb paths with idle-timer caching engine                                     |

The main configuration is in `~/.emacs.d/config.org` (tangled to `init.el`).

---

## Task 1: Rewrite `org-eldoc.el` as a Multi-Layer Echo-Area Composition Engine

### Objective

Replace the current single-function router with a **multi-function composition engine** that registers multiple focused functions into `eldoc-documentation-functions` with explicit priority ordering, using `eldoc-documentation-compose` strategy.

### Requirements

1. **Breadcrumb Layer (always active, depth -10)**:
   - Call `breadcrumb-imenu-crumbs` (the standalone autoloaded function from `breadcrumb.el`) to get the cached heading path
   - This leverages breadcrumb's `bc--ipath-alist` idle-timer cache and `buffer-chars-modified-tick` invalidation — zero additional AST traversal cost
   - Format: `Project → Architecture → Frontend` with custom faces (`shadow` for ancestors, `font-lock-keyword-face` bold for current heading)
   - Separator: `→` (propertized with `(:inherit shadow :height 0.8)`)
   - Fallback: `"Top Level"` at buffer start, `"Bottom Level"` at buffer end
   - **Do NOT use `org-get-outline-path`** — it is O(N) per keystroke. Breadcrumb's imenu cache is O(1) amortized.

2. **Heading Metadata Layer (depth -40)**:
   - When point is on a headline: show `TODO [#A] :@computer:urgent:`
   - Use `org-element-at-point` (C-level cached in Org 9.8) for O(1) property access
   - Faces: `org-get-todo-face`, `org-get-priority-face`, `org-tag`

3. **Timestamp Layer (depth -30)**:
   - Show `SCHEDULED: <2026-07-25> │ DEADLINE: <2026-08-01>` when heading has timestamps
   - Extract `:scheduled`, `:deadline`, `:closed` from the headline element

4. **Link Target Layer (depth -80)**:
   - When point is on a link: show `Link: denote:20240101T120000 [Title]` or `File: ~/org/note.org`
   - Use `org-element-context` for O(1) context detection
   - Support: file, denote, id, fuzzy, http/https link types

5. **Property Drawer Layer (depth -60)**:
   - When point is inside a property drawer: show `CUSTOM_ID: my-id`
   - Use `org-element-context` → `node-property` type

6. **Src Block Header Layer (depth -75)**:
   - When point is on `#+begin_src` / `#+end_src` line: show `python :results output :session`
   - Use `org-babel-get-src-block-info 'light` (avoids heavy header evaluation)

7. **Table Cell Layer (depth -70)**:
   - When point is in a table: show `Cell[3,2] Formula: $1+$2`
   - Guard: `(org-at-table-p)` and `(not (org-at-table.el-p))`

8. **Clock Layer (depth -100)**:
   - When `org-clock-current-task` is active: show `CLOCKING: Task Name`
   - Faces: `org-special-keyword` for label, `org-clock-overlay` for task

9. **Babel Language Delegation (depth -90)**:
   - When inside a src block body: delegate to the language's native `eldoc-documentation-functions`
   - For `emacs-lisp`: use `elisp-eldoc-var-docstring` + `elisp-eldoc-funcall` (native C-level introspection, no LSP needed)
   - For other languages: use the temp-buffer caching mechanism (spawn headless buffer, enable major mode, scrape hook)
   - **CRITICAL**: This is LOCAL introspection only. Eglot/LSP does NOT run inside inline Org src blocks. Do not claim LSP delegation.

### Composition Strategy

```elisp
(setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
```

- `eldoc-documentation-compose` calls ALL functions in `eldoc-documentation-functions` and displays all non-nil results joined by newlines
- Functions earlier in the hook (lower depth number) are considered higher priority
- Set `eldoc-echo-area-use-multiline-p` to `2` (allow exactly 2 lines in echo area)
- Any documentation exceeding 2 lines is automatically routed to `eldoc-childframe` via the display function routing (Task 3)

### Performance Guards

- Wrap the entire router in `(unless (and (fboundp 'too-long-file-p) (too-long-file-p)) ...)` to abort in massive files (>500K chars or >1000 lines)
- All `org-element-context` / `org-element-at-point` calls are O(1) via Org 9.8's C-level element cache
- Breadcrumb's imenu refresh is deferred to idle time via `bc--idle-timer` and only fires if the buffer is visible

### Registration

```elisp
(defun org-eldoc-load ()
  "Register all org-eldoc layers in the buffer."
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--clock -100 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--babel-delegation -90 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--link-target -80 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--src-header -75 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--table-cell -70 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--property-drawer -60 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--heading-info -40 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--timestamps -30 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--breadcrumb -10 t))
```

---

## Task 2: Breadcrumb in Echo Area for `prog-mode` and `emacs-lisp-mode`

### Objective

Use breadcrumb's caching engine to display the imenu structural path (e.g., `MyClass > my_method > inner_func`) in the **echo area** for all programming modes and Emacs Lisp mode. The header-line is NOT used.

### Requirements

1. Create a small ElDoc bridge function `prog-eldoc--breadcrumb` that:
   - Calls `breadcrumb-imenu-crumbs` (standalone, no minor mode needed)
   - Calls `breadcrumb-project-crumbs` for the project path
   - Composes: `project/path │ Class > Method`
   - Returns the composed string to the ElDoc callback

2. Register this function in `eldoc-documentation-functions` for:
   - `prog-mode-hook` (covers all programming languages)
   - `emacs-lisp-mode-hook` (explicit, since elisp is not always derived from prog-mode in all configs)

3. **Do NOT activate `breadcrumb-local-mode`** (which injects into `header-line-format`)
4. **Do NOT activate `breadcrumb-mode`** (the globalized minor mode)
5. Keep all `:custom` settings for `breadcrumb-imenu-max-length`, `breadcrumb-project-max-length`, separators, and faces
6. Set `breadcrumb-idle-time` to `2.0` via `setq` in `:config` (it's a `defvar`, not `defcustom`)

### Eglot Synergy

When Eglot is active in a buffer, it enriches `imenu--index-alist` with `breadcrumb-region` text properties (Eglot 1.14+). Breadcrumb automatically detects this and switches from `bc--ipath-plain` to `bc--ipath-rich`, providing LSP-accurate structural paths (e.g., nested C++ namespaces, Python class methods). No additional configuration needed.

### config.org Integration

```elisp
(use-package breadcrumb
  :defer t
  :custom
  (breadcrumb-project-max-length 0.3)
  (breadcrumb-imenu-max-length 0.4)
  (breadcrumb-project-crumb-separator " / ")
  (breadcrumb-imenu-crumb-separator " > ")
  :custom-face
  (breadcrumb-face ((t (:inherit shadow))))
  (breadcrumb-project-crumbs-face ((t (:inherit shadow))))
  (breadcrumb-project-base-face ((t (:inherit shadow :weight bold))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-type-face :weight bold))))
  (breadcrumb-imenu-crumbs-face ((t (:inherit shadow))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :weight bold))))
  :config
  (setq breadcrumb-idle-time 2.0)
  ;; NO breadcrumb-mode activation. Used purely as a library.
  )
```

---

## Task 3: Intelligent Display Routing (Echo Area vs. Childframe)

### Objective

Implement a custom `eldoc-display-functions` router that splits documentation based on line count:

- **≤ 2 lines** → echo area (via `eldoc-display-in-echo-area`)
- **> 2 lines** → childframe (via `eldoc-childframe--display-function`)

### Requirements

1. Modify `eldoc-childframe.el` to add a line-count routing function:

```elisp
(defun eldoc-childframe--route-display (docs interactive)
  "Route DOCS to echo area (≤2 lines) or childframe (>2 lines)."
  (let* ((composed (string-join (mapcar #'car docs) "\n"))
         (line-count (1+ (cl-count ?\n composed))))
    (if (<= line-count 2)
        nil  ; Return nil to let eldoc-display-in-echo-area handle it
      (eldoc-childframe--display-function docs interactive))))
```

2. Set `eldoc-display-functions` to:

```elisp
(setq eldoc-display-functions
      '(eldoc-childframe--route-display
        eldoc-display-in-echo-area))
```

3. The Flymake Firewall in `eldoc-childframe.el` must still filter diagnostic payloads from the childframe path (inspect `:origin` plist, block `flymake-eldoc-function`)

4. Set `eldoc-echo-area-use-multiline-p` to `2` globally (allow 2 lines max in echo area)

5. Emacs 31's new `eldoc-help-at-pt` user option should be enabled:

```elisp
(setq eldoc-help-at-pt t)
```

This displays `help-at-pt-kbd-string` via ElDoc, showing keybinding hints for buttons/links when hovering.

---

## Task 4: Additional Echo-Area Intelligence (Beyond ElDoc)

### Objective

Identify and implement additional uses for the echo area that provide instant, glanceable information without requiring a keypress.

### Candidates to Evaluate and Implement

1. **Evil State + Mode Indicator** (already in doom-modeline, but consider echo-area echo on state change):
   - Brief 1-line flash on state transition: `-- INSERT --` / `-- VISUAL --`
   - Use `evil-state-change-hook` with a 0.5s `message` that auto-clears

2. **Org Clock Pulse** (when clocking in/out):
   - Flash `⏱ Clocked in: Task Name` for 2 seconds
   - Use `org-clock-in-hook` / `org-clock-out-hook`

3. **Git Branch Context** (on buffer switch):
   - When switching to a file in a git repo, briefly show `⎇ main │ 3↑ 1↓`
   - Use `buffer-list-update-hook` with debounce (0.3s idle timer)
   - Source: `vc-git--symbolic-ref` or `magit-get-current-branch`

4. **Denote Silo Indicator** (on buffer switch in org-mode):
   - Show which Denote silo the current file belongs to: `📁 zettelkasten` or `📁 projects/website`
   - Use `denote-directory` resolution from `.dir-locals.el`

5. **Flymake Diagnostic Summary** (persistent, not per-diagnostic):
   - Show `⚠ 2E 1W` (2 errors, 1 warning) as a persistent echo-area suffix
   - Use `flymake-diagnostics` count after each diagnostic cycle
   - Must not conflict with ElDoc (use `eldoc-documentation-compose` to include it as a layer)

6. **Window/Buffer Context on Focus**:
   - When switching windows, briefly show buffer name + major mode + line count
   - Use `window-selection-change-functions` with 0.2s debounce

7. **Macro Recording Indicator**:
   - When recording a keyboard macro, show `● REC (3 commands)` in echo area
   - Use `kmacro-counter` and `defining-kbd-macro`

### Implementation Constraints

- All echo-area messages must respect the 2-line maximum
- Use `message` with `minibuffer-message-timeout` for auto-clearing flashes
- Never block the main thread; use idle timers for debounced updates
- Must not conflict with ElDoc's echo area usage (ElDoc has priority)
- Use `inhibit-message` binding when performing batch operations to suppress noise

---

## Task 5: `peek.el` Integration with the Echo-Area/Childframe Split

### Objective

Ensure `peek.el` (inline definition panels) respects the 2-line rule and integrates cleanly with the ElDoc routing.

### Requirements

1. `peek.el`'s Eldoc Origin Firewall must block `flymake-eldoc-function` payloads (already implemented in v0.3.0)
2. When `peek-xref-definition` is invoked, the peek overlay shows the definition inline (below cursor) — this is separate from ElDoc and does not use the echo area
3. When `peek-overlay-dwim` is invoked with a region, it shows the region text in the peek overlay
4. The peek overlay must NOT conflict with `eldoc-childframe` positioning (Corfu collision avoidance already handles this)
5. Add `peek-xref-definition` and `peek-xref-references` to the Development Tools transient menu

---

## Task 6: `org-src-context.el` Validation and Hardening

### Objective

Ensure the LSP context injection for `org-edit-special` buffers works correctly with Eglot in Emacs 31.

### Requirements

1. The package advises `org-edit-src-code` with a command whitelist (`org-edit-special`, `org-edit-src-code`, `evil-org-edit-src-code`)
2. It collects surrounding source blocks matching language + tangle target
3. It injects them as read-only context with proper text property stickiness
4. It mocks `buffer-file-name` so Eglot can find the project root
5. **Emacs 31 validation**: Ensure `save-window-excursion` (not `save-excursion`) is used to prevent `switch-to-buffer` from mutating window state
6. **Exclusion**: Emacs Lisp blocks must NOT trigger Eglot (native `elisp-eldoc-*` functions handle this)
7. Cleanup hooks on both `org-edit-src-exit` and `org-edit-src-abort` must remove injected text before Org writes back

---

## Task 7: config.org Subsection Updates

### Breadcrumb Subsection (in `* TODO Editor Behavior`)

- Remove `(breadcrumb-mode 1)` from `:config`
- Remove `:hook ((prog-mode conf-mode) . breadcrumb-local-mode)`
- Keep all `:custom` and `:custom-face` settings
- Add `setq breadcrumb-idle-time 2.0` in `:config`
- Description: "Provides cached project and structural path rendering for the echo area."

### Org Eldoc Subsection (in `* TODO Org Mode & Second Brain`)

- Replace the commented-out block with:

```elisp
(use-package org-eldoc
  :ensure nil
  :defer t
  :hook (org-mode . org-eldoc-load))
```

- Description: "Displays lightweight context-aware metadata for Org elements in the echo area."

### Eldoc Childframe Subsection (in `* TODO Development Tools`)

- Update to reflect the new routing logic
- Add `eldoc-help-at-pt` configuration
- Description: "Routes complex documentation to a floating popup and keeps the echo area concise."

---

## Emacs 31-Specific Features to Leverage

| Feature                      | Variable/Function                          | Usage                                                        |
| ---------------------------- | ------------------------------------------ | ------------------------------------------------------------ |
| Help at point via ElDoc      | `eldoc-help-at-pt`                         | Show keybinding hints for buttons/links                      |
| Eager composition            | `eldoc-documentation-compose-eagerly`      | Display async results as they arrive (use for Eglot buffers) |
| Multiline control            | `eldoc-echo-area-use-multiline-p`          | Set to `2` for strict 2-line echo area                       |
| Elisp funcall with docstring | `elisp-eldoc-funcall-with-docstring`       | Richer elisp echo (new in Emacs 31)                          |
| Docstring length limit       | `elisp-eldoc-docstring-length-limit`       | Cap at 1000 chars for echo area                              |
| Tree-sitter imenu            | `treesit-aggregated-simple-imenu-settings` | Breadcrumb automatically uses TS-powered imenu               |
| TTY childframes              | `(featurep 'tty-child-frames)`             | eldoc-childframe TTY degradation guard                       |

---

## Negative Constraints (MUST NOT)

- **NO** `breadcrumb-mode` or `breadcrumb-local-mode` activation (no header-line injection)
- **NO** `org-get-outline-path` in the hot path (O(N) per keystroke)
- **NO** LSP/Eglot claims for inline Org src blocks (Eglot cannot run in the main Org buffer)
- **NO** `eldoc-documentation-function` (singular, obsolete since Emacs 28)
- **NO** echo area messages exceeding 2 lines
- **NO** blocking the main thread for breadcrumb/eldoc computation
- **NO** `breadcrumb-opinionated-mode` (destroys mode-line, diminishes eldoc-mode)
- **NO** `org-contrib` dependency (the legacy `org-eldoc.el` is unmaintained)
- **NO** header-line usage for breadcrumbs (echo area only)
- **NO** `which-func-mode` (superseded by breadcrumb's cached imenu engine)

---

## Success Criteria

1. Opening any Org file shows `Project → Heading A → Heading B` in the echo area within 0.1s of cursor movement
2. Hovering over a link shows `Link: denote:20240101T120000 [Title]` in the echo area (1 line)
3. Opening a Python file shows `myproject / src / main.py │ MyClass > my_method` in the echo area
4. Requesting documentation for a complex type (e.g., Rust generics) routes to the childframe popup
5. The echo area NEVER displays more than 2 lines of ElDoc information
6. No measurable input latency (<1ms) when moving cursor in a 10,000-line Org file
7. `org-edit-special` on a Python block provides full Eglot completion/hover in the edit buffer
8. Flymake diagnostics never appear in the childframe (Firewall active)
9. All breadcrumb computation uses the idle-timer cache (no synchronous imenu rescans on keystroke)

---

## File Delivery Checklist

- [ ] `lisp/org-eldoc.el` — Complete rewrite with multi-layer composition engine
- [ ] `lisp/eldoc-childframe.el` — Add line-count routing function
- [ ] `lisp/peek.el` — Validate Emacs 31 compatibility (already done in v0.3.0)
- [ ] `lisp/org-src-context.el` — Validate and harden for Emacs 31
- [ ] `config.org` — Update Breadcrumb, Org Eldoc, and Eldoc Childframe subsections
- [ ] `config.org` — Add prog-mode breadcrumb ElDoc bridge
- [ ] `config.org` — Add echo-area intelligence hooks (Task 4)

````markdown
# Comprehensive Task Specification: Echo-Area-First Contextual Intelligence Engine

## Target Environment

- **Emacs Version**: pretest 31.0.90 (PGTK build, `--with-native-compilation=aot`)
- **Org Version**: 9.8 (bundled with Emacs 31)
- **Package Manager**: Elpaca v0.12+ with `use-package` integration
- **Modal Editing**: Evil mode with `general.el` leader keybindings
- **Theme**: Tokyo Night (doom-tokyo-night)
- **Date Context**: All information must be current as of July 23, 2026

---

## Core Philosophy

The echo area is Emacs' most underutilized prime real estate. It sits permanently at the bottom of every frame, requires zero window management, and is visible in every buffer. This project transforms it into a **contextual intelligence layer** that provides instant, glanceable spatial and semantic awareness without stealing focus or screen space.

**The 2-Line Rule:**

- **Echo area**: Strictly 1–2 lines of high-density, glanceable information (breadcrumbs, signatures, link targets, property values, clock status)
- **Childframe/popup** (`eldoc-childframe.el`): Reserved for complex, multi-line documentation (>2 lines) such as full function docstrings, LSP hover payloads, and type signatures with generics

Any information exceeding 2 lines in the echo area is **distracting** and must be routed to the childframe. The echo area is for _orientation_; the childframe is for _study_.

---

## Existing Files (Source of Truth)

All files reside in `~/.emacs.d/lisp/` unless otherwise noted:

| File                  | Role                                                                                                                                |
| --------------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| `org-eldoc.el`        | Custom Org-mode ElDoc router (current version handles src-block headers, link targets, property drawers, Babel language delegation) |
| `eldoc-childframe.el` | Custom fork of eldoc-box: Flymake Firewall, Evil spatial debouncing, TTY degradation guards, Corfu collision avoidance              |
| `peek.el`             | Modernized fork (v0.3.0): inline definition/reference panels via overlays, xref-backend bypass, Eldoc Origin Firewall               |
| `org-src-context.el`  | Injects surrounding source blocks into `org-edit-special` buffers for LSP context (Eglot)                                           |
| `breadcrumb.el`       | Upstream (v1.0.1, João Távora): project + imenu breadcrumb paths with idle-timer caching engine                                     |

The main configuration is in `~/.emacs.d/config.org` (tangled to `init.el`).

---

## Task 1: Rewrite `org-eldoc.el` as a Multi-Layer Echo-Area Composition Engine

### Objective

Replace the current single-function router with a **multi-function composition engine** that registers multiple focused functions into `eldoc-documentation-functions` with explicit priority ordering, using `eldoc-documentation-compose` strategy.

### Requirements

1. **Breadcrumb Layer (always active, depth -10)**:
   - Call `breadcrumb-imenu-crumbs` (the standalone autoloaded function from `breadcrumb.el`) to get the cached heading path
   - This leverages breadcrumb's `bc--ipath-alist` idle-timer cache and `buffer-chars-modified-tick` invalidation — zero additional AST traversal cost
   - Format: `Project → Architecture → Frontend` with custom faces (`shadow` for ancestors, `font-lock-keyword-face` bold for current heading)
   - Separator: `→` (propertized with `(:inherit shadow :height 0.8)`)
   - Fallback: `"Top Level"` at buffer start, `"Bottom Level"` at buffer end
   - **Do NOT use `org-get-outline-path`** — it is O(N) per keystroke. Breadcrumb's imenu cache is O(1) amortized.

2. **Heading Metadata Layer (depth -40)**:
   - When point is on a headline: show `TODO [#A] :@computer:urgent:`
   - Use `org-element-at-point` (C-level cached in Org 9.8) for O(1) property access
   - Faces: `org-get-todo-face`, `org-get-priority-face`, `org-tag`

3. **Timestamp Layer (depth -30)**:
   - Show `SCHEDULED: <2026-07-25> │ DEADLINE: <2026-08-01>` when heading has timestamps
   - Extract `:scheduled`, `:deadline`, `:closed` from the headline element

4. **Link Target Layer (depth -80)**:
   - When point is on a link: show `Link: denote:20240101T120000 [Title]` or `File: ~/org/note.org`
   - Use `org-element-context` for O(1) context detection
   - Support: file, denote, id, fuzzy, http/https link types

5. **Property Drawer Layer (depth -60)**:
   - When point is inside a property drawer: show `CUSTOM_ID: my-id`
   - Use `org-element-context` → `node-property` type

6. **Src Block Header Layer (depth -75)**:
   - When point is on `#+begin_src` / `#+end_src` line: show `python :results output :session`
   - Use `org-babel-get-src-block-info 'light` (avoids heavy header evaluation)

7. **Table Cell Layer (depth -70)**:
   - When point is in a table: show `Cell[3,2] Formula: $1+$2`
   - Guard: `(org-at-table-p)` and `(not (org-at-table.el-p))`

8. **Clock Layer (depth -100)**:
   - When `org-clock-current-task` is active: show `CLOCKING: Task Name`
   - Faces: `org-special-keyword` for label, `org-clock-overlay` for task

9. **Babel Language Delegation (depth -90)**:
   - When inside a src block body: delegate to the language's native `eldoc-documentation-functions`
   - For `emacs-lisp`: use `elisp-eldoc-var-docstring` + `elisp-eldoc-funcall` (native C-level introspection, no LSP needed)
   - For other languages: use the temp-buffer caching mechanism (spawn headless buffer, enable major mode, scrape hook)
   - **CRITICAL**: This is LOCAL introspection only. Eglot/LSP does NOT run inside inline Org src blocks. Do not claim LSP delegation.

### Composition Strategy

```elisp
(setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
```

- `eldoc-documentation-compose` calls ALL functions in `eldoc-documentation-functions` and displays all non-nil results joined by newlines
- Functions earlier in the hook (lower depth number) are considered higher priority
- Set `eldoc-echo-area-use-multiline-p` to `2` (allow exactly 2 lines in echo area)
- Any documentation exceeding 2 lines is automatically routed to `eldoc-childframe` via the display function routing (Task 3)

### Performance Guards

- Wrap the entire router in `(unless (and (fboundp 'too-long-file-p) (too-long-file-p)) ...)` to abort in massive files (>500K chars or >1000 lines)
- All `org-element-context` / `org-element-at-point` calls are O(1) via Org 9.8's C-level element cache
- Breadcrumb's imenu refresh is deferred to idle time via `bc--idle-timer` and only fires if the buffer is visible

### Registration

```elisp
(defun org-eldoc-load ()
  "Register all org-eldoc layers in the buffer."
  (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--clock -100 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--babel-delegation -90 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--link-target -80 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--src-header -75 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--table-cell -70 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--property-drawer -60 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--heading-info -40 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--timestamps -30 t)
  (add-hook 'eldoc-documentation-functions #'org-eldoc--breadcrumb -10 t))
```

---

## Task 2: Breadcrumb in Echo Area for `prog-mode` and `emacs-lisp-mode`

### Objective

Use breadcrumb's caching engine to display the imenu structural path (e.g., `MyClass > my_method > inner_func`) in the **echo area** for all programming modes and Emacs Lisp mode. The header-line is NOT used.

### Requirements

1. Create a small ElDoc bridge function `prog-eldoc--breadcrumb` that:
   - Calls `breadcrumb-imenu-crumbs` (standalone, no minor mode needed)
   - Calls `breadcrumb-project-crumbs` for the project path
   - Composes: `project/path │ Class > Method`
   - Returns the composed string to the ElDoc callback

2. Register this function in `eldoc-documentation-functions` for:
   - `prog-mode-hook` (covers all programming languages)
   - `emacs-lisp-mode-hook` (explicit, since elisp is not always derived from prog-mode in all configs)

3. **Do NOT activate `breadcrumb-local-mode`** (which injects into `header-line-format`)
4. **Do NOT activate `breadcrumb-mode`** (the globalized minor mode)
5. Keep all `:custom` settings for `breadcrumb-imenu-max-length`, `breadcrumb-project-max-length`, separators, and faces
6. Set `breadcrumb-idle-time` to `2.0` via `setq` in `:config` (it's a `defvar`, not `defcustom`)

### Eglot Synergy

When Eglot is active in a buffer, it enriches `imenu--index-alist` with `breadcrumb-region` text properties (Eglot 1.14+). Breadcrumb automatically detects this and switches from `bc--ipath-plain` to `bc--ipath-rich`, providing LSP-accurate structural paths (e.g., nested C++ namespaces, Python class methods). No additional configuration needed.

### config.org Integration

```elisp
(use-package breadcrumb
  :defer t
  :custom
  (breadcrumb-project-max-length 0.3)
  (breadcrumb-imenu-max-length 0.4)
  (breadcrumb-project-crumb-separator " / ")
  (breadcrumb-imenu-crumb-separator " > ")
  :custom-face
  (breadcrumb-face ((t (:inherit shadow))))
  (breadcrumb-project-crumbs-face ((t (:inherit shadow))))
  (breadcrumb-project-base-face ((t (:inherit shadow :weight bold))))
  (breadcrumb-project-leaf-face ((t (:inherit font-lock-type-face :weight bold))))
  (breadcrumb-imenu-crumbs-face ((t (:inherit shadow))))
  (breadcrumb-imenu-leaf-face ((t (:inherit font-lock-function-name-face :weight bold))))
  :config
  (setq breadcrumb-idle-time 2.0)
  ;; NO breadcrumb-mode activation. Used purely as a library.
  )
```

---

## Task 3: Intelligent Display Routing (Echo Area vs. Childframe)

### Objective

Implement a custom `eldoc-display-functions` router that splits documentation based on line count:

- **≤ 2 lines** → echo area (via `eldoc-display-in-echo-area`)
- **> 2 lines** → childframe (via `eldoc-childframe--display-function`)

### Requirements

1. Modify `eldoc-childframe.el` to add a line-count routing function:

```elisp
(defun eldoc-childframe--route-display (docs interactive)
  "Route DOCS to echo area (≤2 lines) or childframe (>2 lines)."
  (let* ((composed (string-join (mapcar #'car docs) "\n"))
         (line-count (1+ (cl-count ?\n composed))))
    (if (<= line-count 2)
        nil  ; Return nil to let eldoc-display-in-echo-area handle it
      (eldoc-childframe--display-function docs interactive))))
```

2. Set `eldoc-display-functions` to:

```elisp
(setq eldoc-display-functions
      '(eldoc-childframe--route-display
        eldoc-display-in-echo-area))
```

3. The Flymake Firewall in `eldoc-childframe.el` must still filter diagnostic payloads from the childframe path (inspect `:origin` plist, block `flymake-eldoc-function`)

4. Set `eldoc-echo-area-use-multiline-p` to `2` globally (allow 2 lines max in echo area)

5. Emacs 31's new `eldoc-help-at-pt` user option should be enabled:

```elisp
(setq eldoc-help-at-pt t)
```

This displays `help-at-pt-kbd-string` via ElDoc, showing keybinding hints for buttons/links when hovering.

---

## Task 4: Additional Echo-Area Intelligence (Beyond ElDoc)

### Objective

Identify and implement additional uses for the echo area that provide instant, glanceable information without requiring a keypress.

### Candidates to Evaluate and Implement

1. **Evil State + Mode Indicator** (already in doom-modeline, but consider echo-area echo on state change):
   - Brief 1-line flash on state transition: `-- INSERT --` / `-- VISUAL --`
   - Use `evil-state-change-hook` with a 0.5s `message` that auto-clears

2. **Org Clock Pulse** (when clocking in/out):
   - Flash `⏱ Clocked in: Task Name` for 2 seconds
   - Use `org-clock-in-hook` / `org-clock-out-hook`

3. **Git Branch Context** (on buffer switch):
   - When switching to a file in a git repo, briefly show `⎇ main │ 3↑ 1↓`
   - Use `buffer-list-update-hook` with debounce (0.3s idle timer)
   - Source: `vc-git--symbolic-ref` or `magit-get-current-branch`

4. **Denote Silo Indicator** (on buffer switch in org-mode):
   - Show which Denote silo the current file belongs to: `📁 zettelkasten` or `📁 projects/website`
   - Use `denote-directory` resolution from `.dir-locals.el`

5. **Flymake Diagnostic Summary** (persistent, not per-diagnostic):
   - Show `⚠ 2E 1W` (2 errors, 1 warning) as a persistent echo-area suffix
   - Use `flymake-diagnostics` count after each diagnostic cycle
   - Must not conflict with ElDoc (use `eldoc-documentation-compose` to include it as a layer)

6. **Window/Buffer Context on Focus**:
   - When switching windows, briefly show buffer name + major mode + line count
   - Use `window-selection-change-functions` with 0.2s debounce

7. **Macro Recording Indicator**:
   - When recording a keyboard macro, show `● REC (3 commands)` in echo area
   - Use `kmacro-counter` and `defining-kbd-macro`

### Implementation Constraints

- All echo-area messages must respect the 2-line maximum
- Use `message` with `minibuffer-message-timeout` for auto-clearing flashes
- Never block the main thread; use idle timers for debounced updates
- Must not conflict with ElDoc's echo area usage (ElDoc has priority)
- Use `inhibit-message` binding when performing batch operations to suppress noise

---

## Task 5: `peek.el` Integration with the Echo-Area/Childframe Split

### Objective

Ensure `peek.el` (inline definition panels) respects the 2-line rule and integrates cleanly with the ElDoc routing.

### Requirements

1. `peek.el`'s Eldoc Origin Firewall must block `flymake-eldoc-function` payloads (already implemented in v0.3.0)
2. When `peek-xref-definition` is invoked, the peek overlay shows the definition inline (below cursor) — this is separate from ElDoc and does not use the echo area
3. When `peek-overlay-dwim` is invoked with a region, it shows the region text in the peek overlay
4. The peek overlay must NOT conflict with `eldoc-childframe` positioning (Corfu collision avoidance already handles this)
5. Add `peek-xref-definition` and `peek-xref-references` to the Development Tools transient menu

---

## Task 6: `org-src-context.el` Validation and Hardening

### Objective

Ensure the LSP context injection for `org-edit-special` buffers works correctly with Eglot in Emacs 31.

### Requirements

1. The package advises `org-edit-src-code` with a command whitelist (`org-edit-special`, `org-edit-src-code`, `evil-org-edit-src-code`)
2. It collects surrounding source blocks matching language + tangle target
3. It injects them as read-only context with proper text property stickiness
4. It mocks `buffer-file-name` so Eglot can find the project root
5. **Emacs 31 validation**: Ensure `save-window-excursion` (not `save-excursion`) is used to prevent `switch-to-buffer` from mutating window state
6. **Exclusion**: Emacs Lisp blocks must NOT trigger Eglot (native `elisp-eldoc-*` functions handle this)
7. Cleanup hooks on both `org-edit-src-exit` and `org-edit-src-abort` must remove injected text before Org writes back

---

## Task 7: config.org Subsection Updates

### Breadcrumb Subsection (in `* TODO Editor Behavior`)

- Remove `(breadcrumb-mode 1)` from `:config`
- Remove `:hook ((prog-mode conf-mode) . breadcrumb-local-mode)`
- Keep all `:custom` and `:custom-face` settings
- Add `setq breadcrumb-idle-time 2.0` in `:config`
- Description: "Provides cached project and structural path rendering for the echo area."

### Org Eldoc Subsection (in `* TODO Org Mode & Second Brain`)

- Replace the commented-out block with:

```elisp
(use-package org-eldoc
  :ensure nil
  :defer t
  :hook (org-mode . org-eldoc-load))
```

- Description: "Displays lightweight context-aware metadata for Org elements in the echo area."

### Eldoc Childframe Subsection (in `* TODO Development Tools`)

- Update to reflect the new routing logic
- Add `eldoc-help-at-pt` configuration
- Description: "Routes complex documentation to a floating popup and keeps the echo area concise."

---

## Emacs 31-Specific Features to Leverage

| Feature                      | Variable/Function                          | Usage                                                        |
| ---------------------------- | ------------------------------------------ | ------------------------------------------------------------ |
| Help at point via ElDoc      | `eldoc-help-at-pt`                         | Show keybinding hints for buttons/links                      |
| Eager composition            | `eldoc-documentation-compose-eagerly`      | Display async results as they arrive (use for Eglot buffers) |
| Multiline control            | `eldoc-echo-area-use-multiline-p`          | Set to `2` for strict 2-line echo area                       |
| Elisp funcall with docstring | `elisp-eldoc-funcall-with-docstring`       | Richer elisp echo (new in Emacs 31)                          |
| Docstring length limit       | `elisp-eldoc-docstring-length-limit`       | Cap at 1000 chars for echo area                              |
| Tree-sitter imenu            | `treesit-aggregated-simple-imenu-settings` | Breadcrumb automatically uses TS-powered imenu               |
| TTY childframes              | `(featurep 'tty-child-frames)`             | eldoc-childframe TTY degradation guard                       |

---

## Negative Constraints (MUST NOT)

- **NO** `breadcrumb-mode` or `breadcrumb-local-mode` activation (no header-line injection)
- **NO** `org-get-outline-path` in the hot path (O(N) per keystroke)
- **NO** LSP/Eglot claims for inline Org src blocks (Eglot cannot run in the main Org buffer)
- **NO** `eldoc-documentation-function` (singular, obsolete since Emacs 28)
- **NO** echo area messages exceeding 2 lines
- **NO** blocking the main thread for breadcrumb/eldoc computation
- **NO** `breadcrumb-opinionated-mode` (destroys mode-line, diminishes eldoc-mode)
- **NO** `org-contrib` dependency (the legacy `org-eldoc.el` is unmaintained)
- **NO** header-line usage for breadcrumbs (echo area only)
- **NO** `which-func-mode` (superseded by breadcrumb's cached imenu engine)

---

## Success Criteria

1. Opening any Org file shows `Project → Heading A → Heading B` in the echo area within 0.1s of cursor movement
2. Hovering over a link shows `Link: denote:20240101T120000 [Title]` in the echo area (1 line)
3. Opening a Python file shows `myproject / src / main.py │ MyClass > my_method` in the echo area
4. Requesting documentation for a complex type (e.g., Rust generics) routes to the childframe popup
5. The echo area NEVER displays more than 2 lines of ElDoc information
6. No measurable input latency (<1ms) when moving cursor in a 10,000-line Org file
7. `org-edit-special` on a Python block provides full Eglot completion/hover in the edit buffer
8. Flymake diagnostics never appear in the childframe (Firewall active)
9. All breadcrumb computation uses the idle-timer cache (no synchronous imenu rescans on keystroke)

---

## File Delivery Checklist

- [ ] `lisp/org-eldoc.el` — Complete rewrite with multi-layer composition engine
- [ ] `lisp/eldoc-childframe.el` — Add line-count routing function
- [ ] `lisp/peek.el` — Validate Emacs 31 compatibility (already done in v0.3.0)
- [ ] `lisp/org-src-context.el` — Validate and harden for Emacs 31
- [ ] `config.org` — Update Breadcrumb, Org Eldoc, and Eldoc Childframe subsections
- [ ] `config.org` — Add prog-mode breadcrumb ElDoc bridge
- [ ] `config.org` — Add echo-area intelligence hooks (Task 4)
````

For all the tasks, ingest the necessary files as .el.txt files as needed. Search the web and think longer for all the tasks. Make sure you have the latest information till July 23, 2026. Perform all the above tasks with GREEN LIGHT
