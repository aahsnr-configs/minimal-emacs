
# Core Configuration Audit: Issues 1–10 (Full Context Reprint)

---

## Issue 1: `org-modules nil` — Wrong Mechanism, Missing `org-habit`

**Severity: 🚨 Critical**

**Current code:**
```emacs-lisp
:custom
(org-modules nil)
```

**Executive plan mandate :**

> "We agreed to use `(defvar org-modules nil)` in the `:preface` block to prevent Org from loading its massive array of legacy extensions at startup. This satisfies the byte-compiler and forces explicit opt-in."

> "Exception for `org-habit`: `org-habit` is still required for your workflow. It will be explicitly opted back into the `org-modules` list rather than being pruned."

**Three distinct problems:**

| # | Problem | Detail |
|---|---------|--------|
| A | Wrong keyword | `:custom` expands to `customize-set-variable`, which runs *before* the package loads. But `org-modules` is read by `org.el`'s `org-load-modules-maybe` during initialization. The executive plan mandates `:preface` with `defvar`, which runs at both byte-compile time and load time , guaranteeing the variable is bound before `org.el`'s `defcustom` evaluates. |
| B | Wrong value | Setting `org-modules` to `nil` prunes **all** modules, including `org-habit`. The Org Habit subsection (`** TODO Org Habit`) expects `org-habit` to be loaded via `org-modules`. With `nil`, `org-habit` will never load, breaking habit consistency graphs in `habits.org`. |
| C | `defvar` vs `setq` physics | `defvar` only sets the variable if it is **unbound**. Since `org.el` hasn't loaded yet when `:preface` runs, the variable is unbound, so `defvar` creates it. When `org.el`'s `defcustom org-modules` subsequently runs, it sees the variable is already bound and **preserves our value** . Using `setq` would work at load time but causes byte-compiler "free variable" warnings. |

**Verified loading physics :**

| Module | Loaded with `'(org-habit)`? | Reason |
|--------|---------------------------|--------|
| `org-habit` | ✅ Yes | Explicitly in `org-modules` |
| `org-crypt` | ❌ No | Pruned from default list |
| `org-info` | ❌ No | Pruned from default list |
| `org-protocol` | ❌ No | Pruned from default list |
| `org-id` | ⚠️ On-demand | Loaded when `org-capture` or ID links invoke it |
| `org-tempo` | ⚠️ On-demand | Loaded when `org-structure-template-alist` is non-nil (your Org Structure Templates subsection sets this) |

**Fix:**
```emacs-lisp
:preface
(defvar org-modules '(org-habit))
```

---

## Issue 2: Missing `org-list-allow-alphabetical` in `:preface`

**Severity: ⚠️ Omission**

**Executive plan mandate :**

> "Alphabetical Lists: `(setq org-list-allow-alphabetical t)` will be placed in `:preface` so Org's parser recognizes `a)`, `A)`, and `a.` list formats before initialization."

**Current state:** Completely absent from the Core Configuration subsection.

**Why `:preface` and not `:custom`:** Org's list parser reads `org-list-allow-alphabetical` during its initialization sequence, which occurs when `org.el` is loaded. The `:custom` keyword runs via `customize-set-variable` before the package loads , which *should* work. However, the executive plan explicitly mandates `:preface` because:

1. `:preface` runs at **both** byte-compile time and load time .
2. During byte-compilation of `init.el`, the byte-compiler needs to know this variable exists to avoid "free variable" warnings in any downstream code that references it.
3. Org's parser initialization is sensitive to variable state at `require` time, and `:preface` guarantees the earliest possible binding.

**Fix:**
```emacs-lisp
:preface
(defvar org-list-allow-alphabetical t)
```

---

## Issue 3: Missing Cache Routing via `no-littering`

**Severity: ⚠️ Omission**

**Executive plan mandate :**

> "Cache Routing: `setq` declarations will route `org-persist-directory`, `org-preview-latex-image-directory`, and `org-publish-timestamp-directory` strictly into the `no-littering` var directory."

**Current state:** None of these three variables are set anywhere in `config.org.txt`.

**Impact without routing:**

| Variable | Default location | Problem |
|----------|-----------------|---------|
| `org-persist-directory` | `~/.cache/emacs/org-persist/` | Scatters Org element cache and babel session data outside `no-littering` management |
| `org-preview-latex-image-directory` | `~/.cache/emacs/org-preview-latex/` (or `ltximg/` relative) | LaTeX preview images pollute `~/.cache/` or the Org file's directory |
| `org-publish-timestamp-directory` | `~/.cache/emacs/org-publish/` | Publishing timestamps scatter outside `no-littering` |

**Your `no-littering` paradigm :** The Package Management subsection already establishes `no-littering` with `:ensure (:wait t)` and routes `custom-file` to `no-littering-expand-etc-file-name`. The `early-init.el` routes `eln-cache` to `var/eln-cache/`. The `undo-fu-session` subsection routes to `no-littering-expand-var-file-name`. Org's cache directories must follow the same pattern.

**Placement:** Must be in `:init` (not `:custom`), because these are path variables that must be set before `org.el` loads and creates its default directories. `:init` runs unconditionally before the package loads .

**Fix:**
```emacs-lisp
:init
(setq org-persist-directory (no-littering-expand-var-file-name "org/persist/")
      org-preview-latex-image-directory (no-littering-expand-var-file-name "org/ltximg/")
      org-publish-timestamp-directory (no-littering-expand-var-file-name "org/timestamps/"))
```

---

## Issue 4: Redundant `org-directory` Setting

**Severity: 💡 Redundancy**

**Current code:**
```emacs-lisp
:custom
(org-directory my/org-directory)
```

**Conflict with Dynamic Directory Structure subsection:**

The `** DONE Dynamic Directory Structure` subsection already authoritatively sets:
```emacs-lisp
(setq org-directory my/org-directory)
```

This is in the `*** Org Package Wiring` source block, which runs via `with-eval-after-load 'org` and establishes `org-directory` as the single source of truth.

**Executive plan mandate :**

> "Rejection of Doom Tripwires: We rejected Doom's `defvar` tripwires for `org-directory` and `org-id-locations-file` in `:preface`, as they serve no functional purpose in a Vanilla configuration and risk interfering with native defaults."

**Dual-source-of-truth problem:** If the Dynamic Directory Structure block changes `my/org-directory` (e.g., to respect a different XDG path), the `:custom` block here would silently diverge because `customize-set-variable` runs at a different point in the load-order timeline than the `with-eval-after-load` block.

**Fix:** Remove `(org-directory my/org-directory)` from this subsection entirely. The Dynamic Directory Structure subsection is the authoritative source.

---

## Issue 5: `org-hide-emphasis-markers t` Without `org-appear`

**Severity: ⚠️ Functional Gap**

**Current code:**
```emacs-lisp
:custom
(org-hide-emphasis-markers t)
```

**Current `org-appear` state in config.org.txt:**
```emacs-lisp
;; (straight-use-package '(org-appear :type git :host github :repo "awth13/org-appear"))
;; (add-hook 'org-mode-hook 'org-appear-mode)
```

Completely commented out. This means emphasis markers (`*bold*`, `/italic/`, `_underline_`, `=code=`, `~verbatim~`) are **permanently invisible** with no cursor-proximate reveal mechanism.

**org-modern interaction :**

The org-modern audit confirms that `org-modern` handles visual prettification of emphasis independently via the `display` text property. Setting `org-hide-emphasis-markers t` with `org-modern` active is the **standard pairing** — org-modern renders the visual representation while the raw markup is hidden.

**org-appear maintenance status :**

`org-appear` (awth13/org-appear) has had **no commits since 2023** and open issues remain unresolved. It is effectively unmaintained as of July 2026.

**Built-in alternative:**

Org 9.8 provides `org-toggle-pretty-entities` bound to `C-c C-x C-v`, which toggles visibility of emphasis markers and entities globally in the buffer. This is a manual toggle, not cursor-proximate, but combined with `org-modern`'s visual prettification, it covers the editing use case.

**Decision (per v32 project_operationals.yaml ):**

- Keep `org-hide-emphasis-markers t`
- Do NOT add `org-appear` (unmaintained)
- Rely on `org-modern` for visual prettification
- Use `C-c C-x C-v` for manual markup reveal when editing

---

## Issue 6: `org-highlight-latex-and-related nil` vs. Academic Writing

**Severity: ⚠️ Design Decision (Revised)**

**Current code:**
```emacs-lisp
:custom
(org-highlight-latex-and-related nil)
```

**Original rationale:** Performance in large files like `todo.org`.

**Revised premise:** You will write academic papers in Org files. Your config already has a full LaTeX Writing Environment section with `auctex`, `cdlatex`, `org-fragtog`, and `org-latex-classes`.

**What `org-highlight-latex-and-related` controls :**

| Value       | Behavior                                                                                         |
| ----------- | ------------------------------------------------------------------------------------------------ |
| `nil`       | No LaTeX syntax highlighting. `$...$`, `\[...\]`, `\begin{...}` appear as plain monochrome text. |
| `'latex`    | Highlights LaTeX fragments and environments.                                                     |
| `'entities` | Highlights LaTeX entities like `\alpha`, `\beta`.                                                |
| `'native`   | Uses native fontification (most comprehensive).                                                  |
| `t`         | Highlights both fragments and entities.                                                          |

**Impact on academic writing with `nil`:**
- `$E = mc^2$` appears as plain text — no visual distinction from prose.
- `\begin{equation}...\end{equation}` blocks are not visually separated.
- LaTeX entities (`\alpha`, `\sum`, `\int`) are not highlighted.
- Makes it extremely difficult to visually parse equations from prose during writing.

**Performance concern re-evaluated:**
- `todo.org` contains **no LaTeX content** — the font-lock engine scans for LaTeX patterns, finds none, and moves on. Cost is effectively zero.
- Academic paper files (in Denote silos) **do** contain LaTeX and benefit enormously from highlighting.
- `org-fragtog` handles LaTeX **preview** (rendering equations as images) independently of `org-highlight-latex-and-related` (which handles **syntax highlighting**). They are complementary, not redundant .

**Revised decision:** Change to `'native` for full LaTeX syntax highlighting.

---

## Issue 7: `org-return-follows-link t` vs. Evil Mode

**Severity: 💡 Design Consideration**

**Current code:**
```emacs-lisp
:custom
(org-return-follows-link t)
```

**Evil interaction analysis:**

In Evil normal state, `RET` is not typically used for link following (Evil uses `gf` or `C-]`). Setting `org-return-follows-link t` means:

- In **insert state**: `RET` follows links instead of creating a new line. This could cause accidental navigation.
- In **normal state**: `RET` is bound to Evil's own commands, so `org-return-follows-link` does not interfere.

**Mitigation:** With Evil, the user can use `o`/`O` for new lines in normal state, and `C-j` or `C-o RET` in insert state for literal newlines. The `org-return-follows-link` behavior in insert state is a common Org workflow preference.

**Verdict:** No conflict. Keep as-is. This is a user preference that works correctly with Evil's modal paradigm.

---

## Issue 8: `:custom` vs `:init` vs `:config` Placement

**Severity: ⚠️ Load-Order Physics**

**Elpaca keyword execution order :**

```
:disabled → :ensure → :preface → :if/:when/:unless → :custom → :init → [package loads] → :config
```

**Current problem:** All settings are in `:custom`. This is correct for most `defcustom` variables, but several variables require different placement:

| Variable | Current | Correct | Reason |
|----------|---------|---------|--------|
| `org-modules` | `:custom` | `:preface` | Must be bound before `org.el` loads; `defvar` semantics required for byte-compiler  |
| `org-list-allow-alphabetical` | Missing | `:preface` | Org's list parser reads this during initialization  |
| `org-persist-directory` | Missing | `:init` | Path variable; must be set before org creates default directories  |
| `org-preview-latex-image-directory` | Missing | `:init` | Same as above |
| `org-publish-timestamp-directory` | Missing | `:init` | Same as above |
| `org-directory` | `:custom` | **Remove** | Already set in Dynamic Directory Structure  |
| All other `defcustom` variables | `:custom` | `:custom` ✅ | Correct placement; `customize-set-variable` runs before package loads  |

**Why `:custom` is correct for the remaining variables:**

Per the Elpaca integration YAML : "`:custom` / `:custom-face` — Processed before `:init`. Expands to `customize-set-variable` calls that run prior to the package loading, allowing the package to adopt your custom value upon load."

This is the correct mechanism for standard `defcustom` variables like `org-pretty-entities`, `org-hide-leading-stars`, `org-src-fontify-natively`, etc.

---

## Issue 9: Org 9.8 Compatibility Check

**Severity: ✅ Verification Required**

**Target:** Emacs 31 ships with Org v9.8 .

**Variable-by-variable verification:**

| Variable                             | Status in Org 9.8 | Notes                                                |
| ------------------------------------ | ----------------- | ---------------------------------------------------- |
| `org-modules`                        | ✅ Valid           | Default: `'(org-crypt org-id org-info org-protocol)` |
| `org-return-follows-link`            | ✅ Valid           | No changes                                           |
| `org-pretty-entities`                | ✅ Valid           | No changes                                           |
| `org-hide-leading-stars`             | ✅ Valid           | No changes                                           |
| `org-hide-emphasis-markers`          | ✅ Valid           | No changes                                           |
| `org-fontify-whole-heading-line`     | ✅ Valid           | No changes                                           |
| `org-adapt-indentation`              | ✅ Valid           | No changes                                           |
| `org-fontify-done-headline`          | ✅ Valid           | No changes                                           |
| `org-fontify-quote-and-verse-blocks` | ✅ Valid           | No changes                                           |
| `org-src-fontify-natively`           | ✅ Valid           | No changes                                           |
| `org-src-preserve-indentation`       | ✅ Valid           | No changes                                           |
| `org-src-tab-acts-natively`          | ✅ Valid           | No changes                                           |
| `org-edit-src-content-indentation`   | ✅ Valid           | No changes                                           |
| `org-element-use-cache`              | ✅ Valid           | Default `t` in 9.8                                   |
| `org-element-cache-persistent`       | ✅ Valid           | No changes                                           |
| `org-highlight-latex-and-related`    | ✅ Valid           | Accepts `nil`, `'latex`, `'entities`, `'native`, `t` |
| `org-startup-folded`                 | ✅ Valid           | No changes                                           |
| `org-startup-with-inline-images`     | ✅ Valid           | No changes                                           |
| `org-startup-with-latex-preview`     | ✅ Valid           | No changes                                           |
| `org-cycle-separator-lines`          | ✅ Valid           | No changes                                           |
| `org-list-allow-alphabetical`        | ✅ Valid           | No changes                                           |

**Emacs 31 `line-spacing` enhancement :**

NEWS.31 confirms: "`line-spacing` now supports specifying spacing above the line. The user option can now be set to a cons cell to specify spacing both above and below the line, which allows for vertically centering text."

This is relevant to the org-modern audit , which recommends `(1 . 1)` or `(2 . 2)` for vertical centering of org-modern badges. However, this is configured in the Fonts subsection, not here.

**No deprecations found for any of these variables in Org 9.8.**

---

## Issue 10: `org-hide-leading-stars t` vs. `org-modern-star`

**Severity: ✅ No Conflict**

**Current code:**
```emacs-lisp
:custom
(org-hide-leading-stars t)
```

**org-modern configuration (from config.org.txt):**
```emacs-lisp
(setq org-modern-hide-stars nil
      org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◇" "▷"))
```

**Note:** The org-modern audit  identified that `org-modern-star` is set incorrectly — it should be `'replace` with the list moved to `org-modern-replace-stars`. But that is a separate subsection issue (Org Modern), not a Core Configuration issue.

**Interaction physics:**

| Mechanism                  | Property used                                 | Effect                                  |
| -------------------------- | --------------------------------------------- | --------------------------------------- |
| `org-hide-leading-stars t` | `invisible` text property via `org-hide` face | Makes leading asterisks invisible       |
| `org-modern-star 'replace` | `display` text property                       | Replaces asterisks with Unicode symbols |

These operate on **different text properties** and do not conflict. When both are active:
- `org-modern`'s `display` property takes visual precedence for the asterisks it replaces.
- `org-hide-leading-stars` handles any remaining asterisks not covered by org-modern.

**Verdict:** No conflict. Keep `org-hide-leading-stars t`.

---

## Summary Table

| Issue                                               | Severity          | Action                                                                                                                            |
| --------------------------------------------------- | ----------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| 1. `org-modules nil`                                | 🚨 Critical       | Move to `:preface` with `(defvar org-modules '(org-habit))`                                                                       |
| 2. Missing `org-list-allow-alphabetical`            | ⚠️ Omission       | Add `(defvar org-list-allow-alphabetical t)` to `:preface`                                                                        |
| 3. Missing cache routing                            | ⚠️ Omission       | Add `org-persist-directory`, `org-preview-latex-image-directory`, `org-publish-timestamp-directory` to `:init` via `no-littering` |
| 4. Redundant `org-directory`                        | 💡 Redundancy     | Remove from `:custom`                                                                                                             |
| 5. `org-hide-emphasis-markers` without `org-appear` | ⚠️ Functional Gap | Keep `t`; do NOT add `org-appear` (unmaintained); rely on `org-modern` + `C-c C-x C-v`                                            |
| 6. `org-highlight-latex-and-related nil`            | ⚠️ Revised        | Change to `'native` for academic writing                                                                                          |
| 7. `org-return-follows-link t` vs Evil              | 💡 Consideration  | Keep as-is; no conflict                                                                                                           |
| 8. `:custom` vs `:init` vs `:config`                | ⚠️ Load-Order     | Restructure per Issue 1–3 fixes                                                                                                   |
| 9. Org 9.8 compatibility                            | ✅ Pass            | All variables verified valid                                                                                                      |
| 10. `org-hide-leading-stars` vs `org-modern-star`   | ✅ No Conflict     | Keep as-is                                                                                                                        |
