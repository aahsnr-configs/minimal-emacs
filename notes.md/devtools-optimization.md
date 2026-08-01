I will now perform a comprehensive, exhaustive analysis of every single subsection under `* TODO Development Tools`. Let me begin by re-fetching the reference URLs and searching for the latest 2026 information on every package involved.


I am locked in the **READ-ONLY planning state**. Awaiting explicit `GREEN LIGHT` to execute the rewrite.

Below is the exhaustive, subsection-by-subsection deep analysis of the **entire** `* TODO Development Tools` tree in `config.org.txt` — all **13 subsections**, every variable, every hook, every commented-out line, cross-referenced against Doom Emacs `+lsp.el` , Andrey Orst's configuration , the official lsp-mode performance documentation , Flycheck 38 release notes , and the Emacs 31 NEWS you provided.

---

## Subsection 1: `** DONE Eldoc`

### Current State
Your Eldoc block is minimal and mostly correct. You set `eldoc-echo-area-use-multiline-p` to `2`, `eldoc-echo-area-prefer-doc-buffer` to `'maybe`, and `eldoc-idle-delay` to `0.1`. You enable `global-eldoc-mode` and use `setopt eldoc-help-at-pt t` for the Emacs 31 help-at-point integration. You also guard `elisp-eldoc-docstring-length-limit` with `boundp`.

### Analysis Against Emacs 31 NEWS
The Emacs 31 NEWS you provided confirms three new Eldoc features:
- `elisp-eldoc-funcall-with-docstring` — a new ElDoc function that includes the current function's docstring in the echo area.
- `elisp-eldoc-funcall-with-docstring-length` — controls whether `'short` (first sentence) or `'full` docstring is shown.
- `elisp-eldoc-docstring-length-limit` — caps docstring length at 1000 characters by default.
- `eldoc-help-at-pt` — shows `help-at-pt-kbd-string` via ElDoc, an alternative to `help-at-pt-display-when-idle`.

You already have `eldoc-help-at-pt` and `elisp-eldoc-docstring-length-limit` wired. You are **missing** `elisp-eldoc-funcall-with-docstring` and `elisp-eldoc-funcall-with-docstring-length`. These are relevant because your config uses `lsp-ui-doc` for hover documentation, but Eldoc still handles Elisp buffers and any non-LSP contexts. Adding `elisp-eldoc-funcall-with-docstring` to `eldoc-documentation-functions` in Elisp buffers would give you richer inline docs without a childframe.

### Issues Found
1. **Missing `elisp-eldoc-funcall-with-docstring`**: Not registered. This is an Emacs 31 feature that enriches Elisp Eldoc output.
2. **Missing `elisp-eldoc-funcall-with-docstring-length`**: Not set. Defaults to `'short`, which is fine, but should be explicitly declared for documentation clarity.
3. **Commented-out eglot hook**: The block at the bottom has a commented-out `eglot-managed-mode-hook` that references `eldoc-documentation-compose-eagerly`. Since your architecture mandates lsp-mode (not eglot), this dead code should remain commented out but is architecturally irrelevant.
4. **`eldoc-echo-area-prefer-doc-buffer` set to `'maybe`**: This is correct. It means long docs overflow to `*eldoc*` rather than truncating. No change needed.
5. **`eldoc-idle-delay` at `0.1`**: This is aggressive. Doom does not set this. Andrey Orst does not set this. The default is `0.5`. At `0.1`, Eldoc fires very quickly, which can cause micro-stutters in large buffers when combined with LSP hover payloads. However, since you use `lsp-ui-doc` for LSP buffers and Eldoc primarily for Elisp, `0.1` is acceptable but worth noting.

### Verdict
Mostly solid. Two missing Emacs 31 variables. One piece of dead eglot code that should stay commented.

---

## Subsection 2: `** DONE Xref`

### Current State
You set `xref-search-program` to `'ripgrep`, `xref-file-name-display` to `'project-relative`, and guard `evil-set-initial-state` for `xref-edit-mode` with `fboundp`.

### Analysis Against Emacs 31 NEWS
The Emacs 31 NEWS confirms:
- **Xref commands now use `display-buffer`** with category `xref-jump`, allowing customization via `display-buffer-alist`.
- **New minor mode `xref-mouse-mode`**: Binds `C-<down-mouse-1>` for control-click jump-to-definition.
- **New command `xref-change-to-xref-edit-mode`**: Bound to `e`, switches an Xref buffer into editable mode.
- **Xref commands no longer suggest visiting a tags table** when no tags file is loaded.

### Issues Found
1. **Missing `display-buffer-alist` entry for `xref-jump` category**: Emacs 31 now routes Xref jumps through `display-buffer` with the `xref-jump` category. You have no `display-buffer-alist` entry for this. Without one, Xref jumps use the default window selection, which may conflict with your Popper/side-window layout. Doom Emacs does not explicitly set this either, but your config has extensive `display-buffer-alist` rules for other categories.
2. **`xref-mouse-mode` not addressed**: You are an Evil user, so mouse-based navigation is likely irrelevant. No action needed, but worth documenting the decision.
3. **`xref-edit-mode` Evil state guard**: Your `fboundp` guard is correct and forward-looking. No issue.
4. **`xref-search-program` set to `'ripgrep`**: Correct. The official lsp-mode performance docs recommend ripgrep for search backends .
5. **No `xref-show-definitions-function` or `xref-show-xrefs-function` override**: You route these through Consult in the Completion Framework section (`setq xref-show-xrefs-function #'consult-xref`). This is correct and should not be duplicated here.

### Verdict
Solid. One potential gap: no `display-buffer-alist` entry for the new Emacs 31 `xref-jump` category.

---

## Subsection 3: `** TODO Language Server Client` (lsp-mode)

### Current State
This is the core LSP block. You hook `lsp-deferred` into `prog-mode`. You set 20+ custom variables. You have three commented-out lines.

### Variable-by-Variable Analysis

| Variable | Your Value | Doom Value | Andrey Orst | Official Docs | Verdict |
|---|---|---|---|---|---|
| `lsp-completion-provider` | `:none` | `:none` (with corfu) | `:none` | N/A | **Correct**. Routes to CAPF → corfu. |
| `lsp-idle-delay` | `0.3` | Not set (default `0.5`) | `0.5` | `0.500` recommended  | **Too aggressive**. `0.3` increases main-thread polling frequency. Recommend `0.5`. |
| `lsp-keymap-prefix` | `nil` | `nil` | `"C-c l"` | N/A | **Correct**. You use general.el leader. |
| `lsp-enable-xref` | `t` | Not set (default `t`) | `t` | N/A | **Correct**. |
| `lsp-log-io` | `nil` | Not set | `nil` | **Must be nil**  | **Correct**. Official docs warn this causes "great performance hit". |
| `lsp-file-watch-threshold` | `4000` | Not set | Not set | Default `1000` | **Acceptable** for large monorepos. |
| `lsp-enable-folding` | `nil` | `nil` | `nil` | N/A | **Correct**. You use treesit-fold/hideshow. |
| `lsp-enable-on-type-formatting` | `nil` | `nil` | `nil` | N/A | **Correct**. Prevents unexpected code modifications. |
| `lsp-enable-text-document-color` | `nil` | `nil` | `nil` | N/A | **Correct**. You use colorful-mode. |
| `lsp-enable-links` | `nil` | Not set | `nil` | N/A | **Correct**. You use link-hint. |
| `lsp-format-buffer-on-save` | `nil` | Not set | Not set | N/A | **Correct**. You use apheleia. |
| `lsp-lens-enable` | `nil` | Not set | `nil` | N/A | **Correct**. |
| `lsp-inlay-hint-enable` | `nil` | Not set | Not set | N/A | **Correct** for minimal aesthetic. |
| `lsp-enable-symbol-highlighting` | `t` | Not set (default `t`) | `nil` | N/A | **Keep `t`**. You correctly identified that Tree-sitter does NOT provide symbol highlighting. Andrey disables it for performance, but you want the feature. |
| `lsp-headerline-breadcrumb-enable` | `nil` | `nil` | `nil` | N/A | **CONTRADICTION**. Your `project_operationals.yaml` mandates "Headerline: `lsp-headerline-breadcrumb-mode` exclusively". You said you want it disabled by default with a toggle keybinding. This is architecturally consistent with your stated intent but contradicts the YAML. |
| `lsp-headerline-breadcrumb-enable-diagnostics` | `nil` | Not set | `nil` | N/A | **Correct** if breadcrumb is disabled. |
| `lsp-signature-auto-activate` | `'(:on-trigger-char :on-server-request)` | Not set | Not set | N/A | **Correct**. |
| `lsp-signature-render-documentation` | `t` | Not set | Not set | N/A | **Correct**. |
| `lsp-signature-doc-lines` | `2` | Not set | `1` | N/A | **Acceptable**. Andrey uses `1` for minimalism. |

### Missing Variables (from Doom and Official Docs)

1. **`lsp-keep-workspace-alive`**: Doom sets this to `nil` and implements a deferred shutdown timer . You do NOT set this. Default is `t`, meaning the LSP server stays alive indefinitely after the last buffer is killed. This wastes memory and CPU. **Critical omission**.
2. **Deferred shutdown advice**: Doom wraps `lsp--shutdown-workspace` with a 3-second `run-at-time` timer to prevent expensive server restarts when quickly switching between project buffers . You have no equivalent. **Critical omission**.
3. **`lsp-session-file`**: You have this commented out. Doom sets it to a cache directory. The default is `~/.emacs.d/.lsp-session-v1`. With `no-littering`, this should be routed to the var directory. **Should be uncommented and corrected**.
4. **`lsp-auto-configure`**: Andrey Orst sets this to `nil` to prevent lsp-mode from auto-enabling features . You do not set it. Default is `t`. Since you explicitly disable every feature you don't want, this is acceptable, but setting it to `nil` would be a safety net against future lsp-mode additions.
5. **`lsp-semantic-tokens-enable`**: Not set. Default varies by lsp-mode version. Since you use Tree-sitter for syntax highlighting and want `lsp-enable-symbol-highlighting` for symbol highlighting, you should explicitly set this to `nil` to prevent double-rendering overhead from LSP semantic tokens.
6. **`lsp-enable-file-watchers`**: Andrey Orst sets this to `nil` . You do not. Default is `t`. File watchers can cause I/O overhead in large projects. However, they are needed for `lsp-file-watch-threshold` to work. Keep `t` but note the tradeoff.
7. **`lsp-enable-suggest-server-download`**: Andrey sets to `nil` . You do not. Default is `t`. This prompts to download missing servers. Useful for interactive use. Keep `t`.
8. **`lsp-modeline-code-actions-enable`**, **`lsp-modeline-diagnostics-enable`**, **`lsp-modeline-workspace-status-enable`**: Andrey disables all three . You do not. Since you use `doom-modeline` with `lsp` integration, these should be `nil` to prevent duplicate modeline rendering.

### Commented-Out Lines Analysis
- `;;(lsp-session-file ...)`: Should be uncommented and routed to `no-littering-expand-var-file-name`.
- `;;(lsp-headerline-breadcrumb-icons-enable t)`: Dead code if breadcrumb is disabled. Keep commented.
- `;;(lsp-diagnostics-provider :flycheck)`: Should be uncommented. Explicitly routing to flycheck is safer than relying on auto-detection, especially since your negative constraints ban flymake.

### Verdict
**The most critical subsection**. Missing deferred shutdown, `lsp-keep-workspace-alive`, `lsp-semantic-tokens-enable`, and modeline deduplication. `lsp-idle-delay` too aggressive. Three commented-out lines need resolution.

---

## Subsection 4: `** TODO Language Server Visuals` (lsp-ui)

### Current State
You hook `lsp-ui-mode` into `lsp-mode`. You set 12 custom variables. You have a large block of commented-out sideline variables and commented-out childframe face customizations.

### Variable-by-Variable Analysis

| Variable | Your Value | Doom Value | Verdict |
|---|---|---|---|
| `lsp-ui-doc-show-with-cursor` | `nil` | Not set (default `nil`) | **Correct**. |
| `lsp-ui-doc-show-with-mouse` | `nil` | `nil` | **Correct**. Doom: "don't disappear on mouseover". |
| `lsp-ui-doc-position` | `'at-point` | `'at-point` | **Correct**. |
| `lsp-ui-doc-max-height` | `8` | `8` | **Correct**. |
| `lsp-ui-doc-max-width` | `72` | `72` | **Correct**. Doom: "150 (default) is too wide". |
| `lsp-ui-doc-header` | `t` | Not set | **Acceptable**. |
| `lsp-ui-doc-footer` | `t` | Not set | **Acceptable**. |
| `lsp-ui-sideline-enable` | `nil` | Not set (default `t`) | **Correct** for your architecture. You use flycheck-annotate-mode for inline diagnostics. |
| `lsp-ui-peek-enable` | `t` | Conditional on `+peek` flag | **Correct**. |
| `lsp-ui-peek-always-show` | `t` | Not set | **Acceptable**. Forces peek overlay even for single definitions. |

### Missing Variables

1. **`lsp-ui-doc-delay`**: NOT SET. Default is `0.2`. Doom explicitly sets this to `0.75` because "0.2 (default) is too naggy" . **Critical omission**. At `0.2`, the doc childframe flickers aggressively during cursor movement.
2. **`lsp-ui-sideline-ignore-duplicate`**: Doom sets to `t` . You do not. Irrelevant since sideline is disabled, but should be set for consistency if you ever toggle it.
3. **`lsp-ui-sideline-show-hover`**: Doom sets to `nil` . You have this commented out. Should be uncommented or deleted (commented).
4. **`lsp-ui-sideline-actions-icon`**: You set this in `:config` to `lsp-ui-sideline-actions-icon-default`. Doom does the same . **Correct**, but irrelevant since sideline is disabled.

### Commented-Out Lines Analysis
- The entire block of `lsp-ui-sideline-*` variables (6 lines): Dead code since `lsp-ui-sideline-enable` is `nil`. Per your directive, these must be **commented out, never deleted**.
- The `:custom-face` block with `lsp-ui-doc-border` and `lsp-ui-doc-background`: Dead code. Doom themes handle childframe borders. Keep commented.

### Doom's `lsp--auto-configure` Advice
Doom wraps `lsp--auto-configure` to prevent it from forcing `lsp-ui-mode` on, using a hook instead . You do not have this. Since you explicitly hook `lsp-ui-mode` into `lsp-mode`, this is not strictly necessary, but it prevents lsp-mode from double-enabling lsp-ui if `lsp-auto-configure` is `t`.

### Verdict
**Missing `lsp-ui-doc-delay`** is the critical gap. Commented-out sideline code should stay commented. The `:config` block's `lsp-ui-sideline-actions-icon` setting is dead code but harmless.

---

## Subsection 5: `** TODO Consult LSP`

### Current State
You declare `consult-lsp` with `:defer t`, `:after (consult lsp-mode)`, and list four commands. You have a `general-define-key` remap for `xref-find-apropos` and a commented-out `ar/local-leader` block.

### Analysis
The `consult-lsp` package provides `consult-lsp-symbols`, `consult-lsp-file-symbols`, `consult-lsp-diagnostics`, and `consult-lsp-file-diagnostics` . Your command list is correct.

### Issues Found
1. **`:defer t` with `:commands` missing**: You list commands in the body but do NOT use the `:commands` keyword. You use `:defer t` instead. This means the package is deferred but the commands are not autoloaded by `use-package`. The commands will only be available after something else loads `consult-lsp`. The `general-define-key` remap at the bottom references `consult-lsp-symbols`, which will trigger autoload. This works but is fragile.
2. **Commented-out `ar/local-leader` block**: Per your directive, this stays commented. However, the global remap `[remap xref-find-apropos]` already covers workspace symbol search. The local leader bindings are redundant.
3. **Doom's approach**: Doom uses `:defer t` with `:when (modulep! :completion vertico)` and a single `map!` remap . Your approach is equivalent.

### Verdict
Functionally correct. The `:commands` keyword should be added for robustness. Commented-out local leader block stays.

---

## Subsection 6: `** TODO LSP Treemacs`

### Current State
You declare `lsp-treemacs` with `:defer t`, `:after (lsp-mode treemacs)`, four commands, and `lsp-treemacs-sync-mode 1` in `:config`. You have an `ar/local-leader` block with four bindings.

### Analysis
The `lsp-treemacs` package provides tree-view UI for errors, symbols, call hierarchy, and type hierarchy . Your configuration is standard.

### Issues Found
1. **`lsp-treemacs-sync-mode 1`**: This synchronizes the treemacs sidebar with the active buffer's LSP symbols. **Correct**.
2. **Andrey Orst's `lsp-treemacs-theme "Iconless"`**: Andrey uses the "Iconless" theme to reduce visual noise . You use `nerd-icons` and `treemacs-nerd-icons`, so keeping icons is correct for your aesthetic. No change needed.
3. **`ar/local-leader` block**: This is correctly placed OUTSIDE the `use-package` block per your structural rules. The bindings are logical.
4. **Missing `:commands` keyword**: Same issue as consult-lsp. The commands are listed in the body but not via `:commands`.

### Verdict
Solid. No critical issues.

---

## Subsection 7: `** TODO Flycheck`

### Current State
This is a large block. You hook `global-flycheck-mode` and `global-flycheck-annotate-mode` into `after-init`. You set 8 custom variables, 6 custom faces, and have a substantial `:config` block.

### Analysis Against Flycheck 38
Flycheck 38 (released July 29, 2026) is a landmark release . Key new features:
- **`flycheck-annotate-mode`**: Native inline diagnostics (Error Lens style). You already have this. **Excellent**.
- **`flycheck-eglot-mode`**: Built-in Eglot bridge. **Irrelevant** — you use lsp-mode, not eglot.
- **`flycheck-lsp-mode`**: Native LSP server diagnostics without a client. **Potentially relevant** for standalone linters like Ruff, but since you run Ruff through lsp-mode as an add-on server, this is redundant.
- **Project-wide diagnostics**: Press `P` in the error list for project scope. **No configuration needed**.
- **TRAMP support**: Checkers now run over TRAMP. **No configuration needed**.
- **Fix application**: `C-c ! f` applies machine-applicable fixes. **No configuration needed**.
- **Error list overhaul**: Group by file/checker/level. **No configuration needed**.
- **Secondary locations**: `C-c ! j` jumps to related locations. **No configuration needed**.
- **Interrupted checks**: Running checks are interrupted when newer ones start. **No configuration needed**.

### Variable-by-Variable Analysis

| Variable | Your Value | Verdict |
|---|---|---|
| `flycheck-emacs-lisp-load-path` | `'inherit` | **Correct**. |
| `flycheck-indication-mode` | `'right-fringe` | **Correct**. |
| `flycheck-highlighting-mode` | `'symbols` | **Correct**. Underlines only the erroneous symbol. |
| `flycheck-check-syntax-automatically` | `'(save idle-change mode-enabled)` | **Correct**. Removes `new-line` to prevent LSP flooding. |
| `flycheck-idle-change-delay` | `1.0` | **Correct**. Prevents main-thread blocking. |
| `flycheck-display-errors-delay` | `0.25` | **Correct**. |
| `flycheck-buffer-switch-check-intermediate-buffers` | `t` | **Set twice** — once in `:custom` and once in `:config`. **Duplicate**. |

### Issues Found
1. **Duplicate `flycheck-buffer-switch-check-intermediate-buffers`**: Set in both `:custom` and `:config`. The `:config` value wins. Remove one (comment it out).
2. **Duplicate `flycheck-display-errors-delay`**: Set in both `:custom` (`0.25`) and `:config` (`0.25`). Same value, but redundant. Comment out the `:config` one.
3. **`delq 'new-line flycheck-check-syntax-automatically`** in `:config`: This is redundant because you already set `flycheck-check-syntax-automatically` to `'(save idle-change mode-enabled)` in `:custom`, which already excludes `new-line`. The `delq` in `:config` operates on the already-modified list. **Dead code**. Comment it out.
4. **`eval` form for `flycheck-checker-get`**: The `(eval '(setf (flycheck-checker-get 'emacs-lisp 'predicate) ...))` form in `:config` is unusual. The `eval` wrapper is unnecessary — this could be a direct `setf`. However, it works. The predicate restricts the `emacs-lisp` checker to buffers inside a project. **Acceptable but should be documented**.
5. **`flycheck-disabled-checkers` set to `'(org-lint)`**: Correct. Prevents false positives in Denote/Org silos.
6. **Custom faces**: You define 6 faces with Tokyo Night colors and force straight underlines to prevent PGTK/Wayland bezier curve stutter. **Correct and well-reasoned**.
7. **`display-buffer-alist` for Flycheck buffers**: You route `*Flycheck error messages*` and `*Flycheck errors*` to a bottom side-window. **Correct**.
8. **Missing `flycheck-annotate-background`**: Flycheck 38 offers `flycheck-annotate-background` for Error Lens-style whole-line tint . You do not set this. Default is `nil`. Consider enabling for visual parity with VS Code.
9. **Missing `global-flycheck-eglot-mode`**: Flycheck 38 has a built-in Eglot bridge . **Irrelevant** — you use lsp-mode. Do NOT enable this.
10. **Missing `global-flycheck-lsp-mode`**: Flycheck 38 can talk directly to linter LSP servers (Ruff, RuboCop, Biome, Harper) . Since you run Ruff through lsp-mode as an add-on, this is redundant. Do NOT enable.

### Verdict
Three duplicate/dead code lines in `:config`. Missing `flycheck-annotate-background` consideration. Otherwise excellent, especially the forward-looking `flycheck-annotate-mode` adoption.

---

## Subsection 8: `** TODO Flycheck Keybindings`

### Current State
The **entire block is commented out**. It contains Unimpaired `]e`/`[e` bindings and global leader `c e`/`c F` bindings.

### Analysis
1. The `]e`/`[e` bindings for `flycheck-next-error`/`flycheck-previous-error` are standard Unimpaired motions. You have similar patterns in Evil Snipe and other packages.
2. The `c e` (list errors) and `c F` (consult errors) bindings overlap with the Consult Flycheck subsection below.
3. Since the entire block is commented out, you currently have **no keybindings for Flycheck navigation** except the default `C-c ! n`/`C-c ! p` from Flycheck itself.

### Issues Found
1. **No active Flycheck navigation keybindings**: This is a functional gap. You cannot quickly jump between errors without using Flycheck's default `C-c !` prefix, which is not Vim-idiomatic.
2. **Overlap with Consult Flycheck**: The `c F` binding appears in both this block and the Consult Flycheck subsection. If you uncomment this block, you must resolve the duplication.

### Verdict
Entirely commented out. Functional gap for Vim-style error navigation. Needs a decision: uncomment the Unimpaired bindings, or rely on Flycheck defaults.

---

## Subsection 9: `** TODO Consult Flycheck`

### Current State
You declare `consult-flycheck` with `:defer t`, `:after (consult flycheck)`, and `:commands (consult-flycheck)`. Below the `use-package` block, there is a **separate** commented-out `ar/global-leader` block binding `c F` to `consult-flycheck`.

### Analysis
1. The `consult-flycheck` package provides a Consult-powered Flycheck error navigator with live previews and severity narrowing.
2. The commented-out `ar/global-leader` block is correctly placed OUTSIDE the `use-package` block per your structural rules.
3. The `c F` binding duplicates the one in the Flycheck Keybindings subsection.

### Issues Found
1. **Commented-out keybinding**: The `c F` binding is commented out. Without it, `consult-flycheck` is only accessible via `M-x`.
2. **Duplication with Flycheck Keybindings subsection**: Both subsections define `c F`. Only one should be active.

### Verdict
Functionally correct but the keybinding is inactive. Duplication with the Flycheck Keybindings subsection must be resolved.

---

## Subsection 10: `** TODO Minimap` (demap)

### Current State
You declare `demap` with `:defer t`, three commands, two custom variables, five custom faces, and a `:config` block that integrates `diff-hl-mode` and `solaire-mode` into the minimap buffer.

### Analysis
The `demap` package provides a detachable minimap sidebar . It is a community package, not part of GNU ELPA. The built-in `minimap` package on GNU ELPA  is an alternative but lacks demap's detachable window and ecosystem integration.

### Issues Found
1. **`demap-minimap-font-face` height set to `25`**: This is a very small font size (2.5pt at 10x scale). This is intentional for a zoomed-out minimap. **Correct**.
2. **`demap-minimap-window-side` set to `'right`**: Correct. Matches your Treemacs-left, info-right paradigm.
3. **`demap-minimap-window-width` set to `15`**: Reasonable.
4. **Custom faces inherit theme faces**: `demap-visible-region-face` inherits `region`, `demap-current-line-face` inherits `hl-line`. **Correct** for automatic doom-themes and solaire-mode synergy.
5. **`ar/demap-integrate-ecosystem` function**: Enables `diff-hl-mode` and `solaire-mode` in the minimap buffer. **Correct** but should be guarded with `ignore-errors` to prevent startup crashes if either package fails to load. Currently not guarded.
6. **No `too-long-file-p` guard**: The minimap renders a zoomed-out view of the entire buffer. In massive files (>500KB), this could cause significant rendering overhead. You have `too-long-file-p` guards on other packages (rainbow-delimiters, whitespace, indent-bars) but NOT on demap.

### Verdict
Solid concept. Missing `ignore-errors` guard on the ecosystem integration hook and `too-long-file-p` guard for large buffers.

---

## Subsection 11: `** DONE Dape`

### Current State
This is the largest subsection in Development Tools. You have a comprehensive `dape` configuration with 20+ custom variables, custom faces, extensive `:config` hooks, `display-buffer-alist` routing, and a full Transient dashboard. You also have `evil-ghostel`, `ghostel-eshell`, `ghostel-compile`, and `ghostel-comint` integrations in the Ghostel section (not in Development Tools, but related).

### Analysis
Your Dape configuration is thorough and well-researched. The `project_operationals.yaml` v51 changelog documents the hallucination corrections you already performed (renaming `dape-breakpoint-condition` → `dape-breakpoint-expression`, `dape-timeout` → `dape-request-timeout`, etc.).

### Issues Found
1. **`dape-key-prefix` set to `nil`**: Correct. You use general.el leader bindings.
2. **`dape-buffer-window-arrangement` set to `'right`**: Correct. Matches your spatial paradigm.
3. **`dape-many-windows` set to `t`**: Correct. Full VS Code-like multi-panel layout.
4. **`dape-info-hide-mode-line` set to `t`**: Correct. Matches `ar/hide-modeline-mode` aesthetic.
5. **`dape-cwd-function`**: Uses `when-let*` (correct for Emacs 31, where `when-let` is obsolete). Routes to `project-current`. **Correct**.
6. **`dape-variable-auto-expand-alist` set to `'((0 . 1))`**: Correct. Auto-expands locals one level.
7. **`dape-inlay-hints` set to `10`**: Correct. Renders inlay hints for 10 lines around the stopped frame.
8. **`too-long-file-p` guard on `dape-display-source-hook`**: Correct. Prevents main-thread blocking in massive files.
9. **Transient dashboard**: Comprehensive with 6 groups (Session, Navigation, Breakpoints, Evaluation, Info Panels, Exit). **Correct**.
10. **`display-buffer-alist` routing**: Routes REPL, memory, disassembly, and compilation buffers to side-windows. **Correct**.
11. **`dape-default-breakpoints-file`**: Routed to `no-littering-expand-var-file-name`. **Correct**.
12. **Breakpoint persistence hooks**: `kill-emacs-hook` for save, `after-init-hook` for load. **Correct**.

### Verdict
**Excellent**. No critical issues. This subsection is the most polished in the entire Development Tools section.

---

## Subsection 12: `** DONE Direnv` (envrc)

### Current State
A single-line configuration: `(use-package envrc :hook (after-init . envrc-global-mode))`.

### Analysis
The `envrc` package by Steve Purcell provides buffer-local direnv integration . It operates by invoking `direnv` to obtain per-directory environment variables and setting them buffer-locally.

### Issues Found
1. **Minimal configuration**: This is intentional. The `envrc` package is designed to work out of the box with `envrc-global-mode`. Doom Emacs uses a similar minimal approach .
2. **No TRAMP guard**: `envrc-global-mode` will attempt to run `direnv` on remote buffers. The `envrc` package handles this gracefully by checking `file-remote-p`, but it is worth noting.
3. **No `exec-path` integration**: The `envrc` package modifies `process-environment` but does NOT modify `exec-path`. If your `.envrc` modifies `PATH`, Emacs' `exec-path` will not reflect this. The `exec-path-from-shell` package or manual `exec-path` manipulation would be needed. However, since you use `envrc-global-mode` which calls `direnv exec`, the `process-environment` is correctly set for subprocesses. `exec-path` is only relevant for `executable-find` calls within Emacs itself.
4. **Interaction with Ghostel**: Your Ghostel configuration strips `DIRENV_` variables from `process-environment` in `ghostel-pre-spawn-hook` to enforce local `.envrc` evaluation. This is correct and complementary.

### Verdict
**Correct and intentionally minimal**. No changes needed.

---

## Subsection 13: `** DONE Apheleia`

### Current State
You hook `apheleia-mode` into `prog-mode`. You set `apheleia-remote-algorithm` to `'cancel` and `apheleia-hide-log-buffers` to `t`.

### Analysis
Apheleia provides async code formatting via external CLI tools . It is the mandated whole-file formatter in your architecture (lsp-mode handles range formatting).

### Issues Found
1. **`apheleia-remote-algorithm` set to `'cancel`**: Correct. Prevents TRAMP hangs.
2. **`apheleia-hide-log-buffers` set to `t`**: Correct. Prevents `*apheleia-log*` buffer clutter.
3. **No `apheleia-formatters` customization**: You rely on Apheleia's defaults. The default formatters include `black` for Python, `prettier` for JS/TS/CSS, `gofmt` for Go, etc. . Since you use Ruff (not Black) for Python via the Languages section, you should verify that `apheleia-mode-alist` maps `python-mode` and `python-ts-mode` to `(ruff-isort ruff)`. You DO set this in the Languages/Python section with `setf (alist-get ...)`. **Correct**.
4. **No `apheleia-inhibit` predicates**: You do not set `apheleia-inhibit` to prevent formatting in specific buffers (e.g., read-only, TRAMP). The `apheleia-remote-algorithm 'cancel` handles TRAMP. Read-only buffers are handled by Apheleia internally.
5. **Interaction with lsp-mode**: Your `lsp-format-buffer-on-save` is `nil`, so there is no conflict. Apheleia handles whole-file formatting; lsp-mode handles range formatting via `lsp-format-region`. **Correct separation**.
6. **Doom Emacs approach**: Doom uses `+format-with-lsp` and `+format-with-formatter` to route between LSP and external formatters. Your approach is simpler and correct: Apheleia for whole-file, lsp-mode for range. No issue.
7. **Emacs Redux recommendation**: The March 2026 Emacs Redux article recommends Apheleia for "async formatting and cursor stability" . Your configuration aligns with this recommendation.

### Verdict
**Correct and well-integrated**. No changes needed.

---

## Cross-Cutting Issues Across ALL 13 Subsections

### 1. The Breadcrumb Contradiction
Your `project_operationals.yaml` mandates `lsp-headerline-breadcrumb-mode` exclusively. Your `config.org.txt` sets `lsp-headerline-breadcrumb-enable` to `nil`. You stated you want it disabled by default with a toggle keybinding. This is a **documentation contradiction** that must be resolved in the YAML or the config.

### 2. Missing Deferred Server Shutdown
Doom Emacs implements a critical `define-advice` around `lsp--shutdown-workspace` with a 3-second deferred timer . You have no equivalent. This causes the LSP server to aggressively spin down and restart when switching between project buffers, wasting CPU and memory.

### 3. Missing `lsp-keep-workspace-alive nil`
Without this, the LSP server stays alive indefinitely after the last buffer is killed. Combined with the missing deferred shutdown, this creates a worst-case scenario: servers never die, but if they do die, they restart immediately.

### 4. Missing `lsp-ui-doc-delay`
Default `0.2` is "too naggy" per Doom . Should be `0.75`.

### 5. Duplicate/Dead Code in Flycheck `:config`
Three lines are duplicated or redundant: `flycheck-buffer-switch-check-intermediate-buffers`, `flycheck-display-errors-delay`, and `delq 'new-line`.

### 6. Flycheck Keybindings Entirely Inactive
Both the Flycheck Keybindings and Consult Flycheck keybinding blocks are commented out. You have no Vim-idiomatic error navigation.

### 7. `lsp-semantic-tokens-enable` Not Set
Should be explicitly `nil` to prevent double-rendering with Tree-sitter.

### 8. Modeline Deduplication
`lsp-modeline-code-actions-enable`, `lsp-modeline-diagnostics-enable`, and `lsp-modeline-workspace-status-enable` are not set to `nil`. Since `doom-modeline` renders its own LSP indicators, these create duplicate modeline elements.

---

## Summary of Intended Actions (Pending `GREEN LIGHT`)

Per your strict directive: **comment out, NEVER delete**.

### lsp-mode block
- Comment out `lsp-idle-delay 0.3`, add `lsp-idle-delay 0.5`
- Uncomment `lsp-session-file`, route to `no-littering-expand-var-file-name`
- Uncomment `lsp-diagnostics-provider :flycheck`
- Add `lsp-keep-workspace-alive nil`
- Add `lsp-semantic-tokens-enable nil`
- Add `lsp-modeline-code-actions-enable nil`, `lsp-modeline-diagnostics-enable nil`, `lsp-modeline-workspace-status-enable nil`
- Add deferred shutdown `define-advice` in `:config`
- Add breadcrumb toggle keybinding
- Keep `lsp-headerline-breadcrumb-enable nil` (per your explicit instruction)

### lsp-ui block
- Add `lsp-ui-doc-delay 0.75`
- Keep all commented-out sideline code commented

### Flycheck block
- Comment out the duplicate `flycheck-buffer-switch-check-intermediate-buffers` in `:config`
- Comment out the duplicate `flycheck-display-errors-delay` in `:config`
- Comment out the redundant `delq 'new-line` in `:config`
- Consider adding `flycheck-annotate-background t`

### Eldoc block
- Add `elisp-eldoc-funcall-with-docstring` and `elisp-eldoc-funcall-with-docstring-length`

### Xref block
- Consider adding `display-buffer-alist` entry for `xref-jump` category

### Consult LSP block
- Add `:commands` keyword

### Minimap block
- Add `ignore-errors` guard to `ar/demap-integrate-ecosystem`
- Add `too-long-file-p` guard

### Flycheck Keybindings / Consult Flycheck
- Resolve duplication. Uncomment one set of bindings.

### Dape, Direnv, Apheleia
- No changes needed.

---

I am locked in the **READ-ONLY planning state**. Awaiting explicit `GREEN LIGHT` to execute the rewrite.

### Sources
-  Doom Emacs `+lsp.el` module — https://github.com/doomemacs/modules/blob/main/modules/tools/lsp/%2Blsp.el
-  Flycheck 38 release — https://emacsredux.com/blog/2026/07/29/flycheck-38/
-  Andrey Orst, "Migrating from LSP-Mode to Eglot" — https://andreyor.st/posts/2023-09-09-migrating-from-lsp-mode-to-eglot/
-  lsp-mode Performance — https://emacs-lsp.github.io/lsp-mode/page/performance/
-  lsp-ui repository — https://github.com/emacs-lsp/lsp-ui
-  consult-lsp repository — https://github.com/gagbo/consult-lsp
-  lsp-treemacs repository — https://github.com/emacs-lsp/lsp-treemacs
-  dape ELPA — https://elpa.gnu.org/devel/dape.html
-  envrc repository — https://github.com/purcell/envrc
-  apheleia repository — https://github.com/radian-software/apheleia
-  demap repository — https://github.com/emacsmirror/demap
-  Emacs Redux, "Code Formatting in Emacs" — https://emacsredux.com/blog/2026/03/11/code-formatting-in-emacs/
-  Emacs 31 NEWS — https://github.com/emacs-mirror/emacs/blob/master/etc/NEWS.31
