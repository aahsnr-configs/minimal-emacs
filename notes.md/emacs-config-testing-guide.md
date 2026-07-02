# Emacs Configuration Testing Guide

## Sections: Org Mode & Second Brain & Productivity

---

## How to Use This Guide

Before running any test, enable debug mode so errors surface with full backtraces:

```
M-x toggle-debug-on-error RET
```

After all testing is done, disable it:

```
M-x toggle-debug-on-error RET
```

Check the `*Messages*` buffer continuously throughout testing — almost every test produces output there:

```
M-x view-echo-area-messages   (or  C-h e)
```

Each test below is labelled with a **Pass Condition** (what correct behaviour looks like) and a **Failure Signal** (what to look for if something is wrong). Run tests in the order listed within each section, as some depend on prior ones.

---

---

# PART I — ORG MODE

---

## 1. Dynamic Directory Structure

**Purpose:** Verify that `my/org-directory` and all subdirectories are created on startup, and that seed `.org` files are written with the correct template content.

### 1.1 — Directory creation at startup

```
M-x eval-expression RET my/org-directory RET
```

**Pass:** Returns a string ending in `org/` (symlinks resolved via `file-truename`).

```
M-x eval-expression RET (mapcar #'file-directory-p
  (mapcar (lambda (d) (expand-file-name d my/org-directory))
          '("roam" "roam/zettel" "roam/literature" "roam/ideas"
            "roam/projects" "roam/daily" "roam/meetings"
            "gtd" "reviews" "downloads" "noter"
            "archive" "attachments" "backups"))) RET
```

**Pass:** Returns a list of all `t` values. Any `nil` means that subdirectory was not created.

### 1.2 — Seed file creation

In a shell (or `M-x shell`):

```bash
ls ~/org/journal.org ~/org/habits.org ~/org/goals.org \
   ~/org/reading.org ~/org/meetings.org ~/org/roam/ideas/fleeting.org
```

**Pass:** All six files exist and are non-empty.

Open each file and verify: `journal.org` should contain `#+filetags: :journal:`. `habits.org` should have a `* Daily Habits` heading. `fleeting.org` must start with `#+title: Fleeting Notes` — if the `%<%Y-%m-%d>` format string was not expanded, it will appear as a literal `%<...>` string in the file (a template substitution failure).

### 1.3 — `my/ensure-org-file` idempotency

Run Emacs twice (restart or `M-x server-start` then reconnect). Check that files are not overwritten or truncated on second load. Open `journal.org`, add a line, save, then restart Emacs and confirm the line is still there.

**Failure Signal:** File content reset to template on startup.

### 1.4 — Convenience variable resolution

```
M-x eval-expression RET my/org-roam-directory RET
M-x eval-expression RET my/org-gtd-directory RET
M-x eval-expression RET my/org-reviews-directory RET
```

**Pass:** Each returns a path that is a subdirectory of `my/org-directory`. None should be `nil`.

---

## 2. Per-Project Org Context

**Purpose:** Verify that the dynamic project-context switching system correctly activates and deactivates per-project org settings when entering and leaving a `.git`-rooted project directory.

### 2.1 — Registry empty by default

```
M-x eval-expression RET my/project-org-contexts RET
```

**Pass:** Returns `nil` (no entries pre-registered, since the hardcoded `~/physics/` bootstrap was intentionally removed).

### 2.2 — Manual registration

In `*scratch*`, evaluate:

```elisp
(my/register-project-org-context "~/"
  :name "home-test"
  :roam-dir (expand-file-name "~/org/roam/")
  :gtd-dir  (expand-file-name "~/org/gtd/"))
```

Then:

```
M-x eval-expression RET (length my/project-org-contexts) RET
```

**Pass:** Returns `1`.

### 2.3 — Global snapshot taken on first activation

```
M-x eval-expression RET my/global-org-context RET
```

Before any project is entered this should be `nil`. After visiting any file inside the registered project root, it should become a plist with `:name "global"`.

### 2.4 — Context switch on file open

With the test entry from 2.2 registered, open any file under `~/` (e.g., `C-x C-f ~/.bashrc`). Check `*Messages*` for:

```
[org-ctx] ▶ home-test
```

**Failure Signal:** No message, or `void-function` / `wrong-type-argument` error.

### 2.5 — Context deactivation

```
M-x my/project-org-force-global
```

**Pass:** `*Messages*` shows `[org-ctx] ▶ global`. Evaluate `my/active-org-project` — should return `nil`.

### 2.6 — Status command

```
M-x my/project-org-status
```

**Pass:** Echoes either the global context line or the active project context line without errors.

### 2.7 — `my/project-org--post-switch` side effects

After a switch, verify:

- If `org-roam` is loaded, `org-roam-db-sync` is queued (you will see it run within ~3 idle seconds).
- If an `*Org Agenda*` buffer is open, it refreshes automatically.

---

## 3. Better Font Faces

**Purpose:** Verify heading fonts and the bullet-replacement substitution load without errors.

### 3.1 — Font setup hook fires

Open any `.org` file. Then:

```
M-x eval-expression RET (font-lock-ensure) RET
```

**Pass:** No error. List items using `-` should render with `•` characters visually (though the underlying buffer character remains `-`). Check with `C-u C-x =` on a bullet to confirm the composition overlay is present.

### 3.2 — Heading font applied

Place point on a level-1 heading. Run:

```
M-x describe-face RET org-level-1 RET
```

**Pass:** Font shows `JetBrainsMono Nerd Font`. If the font is not installed on the system, Emacs will fall back silently — no error, but the visual effect will be absent. Confirm font availability with `M-x list-faces-display`.

**Failure Signal:** `(error "Invalid face: ...")` in `*Messages*`.

---

## 4. Core Configuration

**Purpose:** Confirm that the `org` use-package block loads and all custom variables are set to expected values.

### 4.1 — Package load

```
M-x eval-expression RET (featurep 'org) RET
```

Open an `.org` file to trigger deferred load. Recheck — should now return `t`.

### 4.2 — Variable spot-check

```
M-x eval-expression RET org-log-done RET        ; expect: time
M-x eval-expression RET org-log-into-drawer RET  ; expect: t
M-x eval-expression RET org-src-fontify-natively RET  ; expect: t
M-x eval-expression RET org-element-use-cache RET     ; expect: t
M-x eval-expression RET org-startup-folded RET        ; expect: overview
```

**Failure Signal:** Any variable returns `nil` when it should not, suggesting a `use-package` expansion failure or a variable name mismatch.

---

## 5. Hooks

### 5.1 — Org mode hooks

Open an `.org` file. Verify:

- `visual-line-mode` is active (long lines wrap, no horizontal scroll).
- `ar/org-font-setup` was called: level-1 headings are bold and at 1.13× height.
- `yas-parents` is set to `(latex-mode)` locally (check with `M-: yas-parents`).

### 5.2 — Agenda mode hooks

The `org-agenda-mode-hook` lambda sets five things. Each needs a separate check.

**Setup:** Open the agenda with `M-x org-agenda` then press `a` for the weekly view. Run all checks inside the `*Org Agenda*` buffer.

---

**Check A — `visual-line-mode` is OFF**

```
M-: visual-line-mode
```

Must return `nil`. If it returns `t`, long agenda lines will soft-wrap mid-entry, making dates and tags appear on the wrong line. The hook calls `(visual-line-mode -1)` to counteract the `org-mode-hook` which enables it globally — the agenda is not an `org-mode` buffer so this hook is the only guard.

---

**Check B — `truncate-lines` is ON**

```
M-: truncate-lines
```

Must return `t`. This is what actually clips long lines at the window edge. `(toggle-truncate-lines 1)` sets this. If `nil`, agenda entries longer than the window width will wrap even with `visual-line-mode` off, because `truncate-lines` is the lower-level control.

---

**Check C — line numbers are hidden**

Look at the left fringe — there should be no line number gutter. Confirm with:

```
M-: display-line-numbers-mode
```

Must return `nil`. The hook calls `(display-line-numbers-mode -1)`. Line numbers in the agenda have no meaning (the buffer is read-only and generated) and break the column alignment of the agenda's time grid.

---

**Check D — mode-line is gone**

The bottom of the `*Org Agenda*` window should be blank — no file name, no major mode name, no evil state indicator, nothing. Confirm with:

```
M-: mode-line-format
```

Must return `nil`. Note: this is a `setq`, not `setq-local` in the hook, which means it sets the global default. This is actually a latent bug — see the failure note below.

---

**Check E — header-line is gone**

There should be no line at the top of the agenda window either. Some themes or packages (e.g. consult-org-roam previews) inject a `header-line-format`. Confirm:

```
M-: header-line-format
```

Must return `nil`.

---

**Failure signals and their meaning:**

| Symptom                                     | Cause                                                                                                                                                                            |
| ------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Lines wrap mid-entry                        | `visual-line-mode` still on, or `truncate-lines` nil                                                                                                                             |
| Mode-line visible                           | `mode-line-format` not nil — check if another hook ran after and reset it                                                                                                        |
| Mode-line disappears in _other_ buffers too | The `setq` (not `setq-local`) bug — every buffer's mode-line vanishes after the agenda opens. Fix: change both `setq` calls in the `org-agenda-mode-hook` lambda to `setq-local` |
| Header line shows buffer path               | A package (vertico-posframe, consult preview, etc.) wrote `header-line-format` after the hook ran                                                                                |

---

The most important thing to verify is the `setq` vs `setq-local` distinction in Check D. If after opening the agenda you notice your modeline has disappeared in unrelated buffers (e.g. a code file you switch to), that is the bug — the hook is wiping the global value. The fix would be to change the hook's `(setq mode-line-format nil)` and `(setq header-line-format nil)` to `(setq-local mode-line-format nil)` and `(setq-local header-line-format nil)`.

### 5.3 — Capture mode hooks

Trigger a capture with `SPC X` or `M-x org-capture`. Verify the capture buffer has no mode-line and no header line.

---

## 6. Keywords

### 6.1 — All TODO states appear

In any `.org` buffer, type `C-c C-t` (or `SPC m t`). You should see all four sequences in the selection prompt:

- `TODO NEXT PROG WAIT | DONE CANCEL`
- `PLAN ACTIVE PAUSED | ACHIEVED DROPPED`
- `INBOX GTD-NEXT GTD-WAIT | GTD-DONE GTD-CNCL`
- `CRITICAL COMMENT | RESOLVED DISABLED`

**Failure Signal:** Only one sequence visible, or an error about duplicate keywords.

### 6.2 — Keyword faces

Mark a heading with `TODO`. Confirm it renders in `#f7768e` (coral red). Mark it `DONE` — confirm green `#9ece6a`. Mark it `CRITICAL` — confirm it is both `#f7768e` and underlined.

**Note:** The `DONE` keyword in the GTD sequence has a logging shortcut `d!` (auto-timestamps completion). Test this specifically: set a heading to `GTD-DONE` using `C-c C-t` and confirm a `CLOSED:` timestamp is inserted automatically.

---

## 7. Tags & Priorities

### 7.1 — Tag picker

In an `.org` heading, press `C-c C-q` (or `SPC m x t`). The tag completion prompt should offer:
`@work`, `@home`, `@computer`, `@errands`, `read`, `meeting`, `urgent`, `someday` — each with its shortcut key.

### 7.2 — Priority faces

Set a heading to `[#A]` priority. Verify it uses the `error` face (usually red). `[#B]` should use `warning` (yellow/orange). `[#C]` should use `success` (green).

---

## 8. Org Structure Templates

### 8.1 — Babel languages load

```
M-x eval-expression RET
  (assoc 'python org-babel-load-languages) RET
```

**Pass:** Returns a non-nil cons cell. Repeat for `emacs-lisp`, `shell`, `C`, `java`, `plantuml`.

**Failure Signal:** `(wrong-type-argument ...)` on `org-babel-do-load-languages` — commonly triggered if a language interpreter is missing from `PATH`. Check `*Messages*` for any `Cannot find ...` warnings.

### 8.2 — Structure template abbreviations

In an `.org` buffer on a blank line, type `<el` then `TAB`.

**Pass:** Expands to an `#+begin_src emacs-lisp … #+end_src` block.

Try `<sh` for shell, `<py` for python, `<tex` for latex, `<jpy` for `jupyter-python`.

**Failure Signal:** TAB does not expand, or `org-tempo-complete-tag` throws `void-function`.

### 8.3 — `org-confirm-babel-evaluate` is nil

```
M-x eval-expression RET org-confirm-babel-evaluate RET
```

**Pass:** Returns `nil`. If `t`, every code block execution will prompt for confirmation.

---

## 9. Transient Template System

### 9.1 — Smart `<` key behaviour

Open an `.org` file. Move point to the beginning of a blank line and press `<`.

**Pass:** The `ar/org-template-transient` popup appears (a `transient` menu with Basic, Head, Source, Misc groups).

In the middle of a line (not at column 0), press `<`.

**Pass:** Inserts a literal `<` character.

**Failure Signal:** Menu never appears, or `void-function: ar/org-template-transient`, or `void-function: transient-define-prefix`.

### 9.2 — Template expansions from the menu

Open the menu (blank line, press `<`). Press `e` (emacs-lisp source).

**Pass:** Inserts `#+begin_src emacs-lisp` block with cursor inside.

Press `<` on a blank line again, then `m` (mermaid).

**Pass:** Inserts `#+begin_src mermaid :file chart.png` block.

### 9.3 — Region wrapping

Select a region of text. Press `<`. Select `o` (quote).

**Pass:** Selected text is wrapped inside a `#+begin_quote … #+end_quote` block.

### 9.4 — `transient` feature guard

```
M-x eval-expression RET (featurep 'transient) RET
```

**Pass:** Returns `t`. If `nil`, the smart `<` handler falls back to `require`-ing it, which may delay the first menu appearance.

---

## 10. Org Modern

### 10.1 — Mode active in org buffers

Open an `.org` file.

```
M-x eval-expression RET (bound-and-true-p org-modern-mode) RET
```

**Pass:** `t`.

### 10.2 — Visual features

- `#+TITLE:` keyword should render as a badge (due to `org-modern-keyword t`).
- Horizontal rules (`-----`) should draw as styled lines (`org-modern-horizontal-rule t`).
- `[3/5]` statistics cookies should render as badges.
- `#+begin_src` and `#+end_src` should display as `»` / `«` delimiters (per the `org-modern-block-name` alist).
- `#+begin_quote` should show `"` / `"` delimiters.
- Tables should show light vertical separators (due to `org-modern-table-vertical 1`).

**Failure Signal:** `org-modern-mode` fails to activate, or `void-variable: org-modern-block-name` (would indicate the config block's closing paren is misplaced — a known concern since the original had a stray commented `setq` after the closing paren).

### 10.3 — Agenda finalization hook

Open the agenda. The `org-modern-agenda` hook should have fired. Agenda items should use modern styling (badges for timestamps and statistics).

---

## 11. Org Src Buffer Naming

### 11.1 — Simplified edit-buffer name

In any `.org` file, enter a source block and press `C-c '` to open the src edit buffer.

**Pass:** The edit buffer is named `src code` regardless of the language.

**Failure Signal:** Buffer is named something like `*Org Src org-mode[ python ]*`. This means `ar/org-src-simplify-buffer-name` is not being called, i.e., the `advice-add` did not execute.

```
M-x eval-expression RET
  (advice-member-p #'ar/org-src-simplify-buffer-name
                   'org-src--construct-edit-buffer-name) RET
```

**Pass:** Returns `t`.

---

## 12. Org Habit

### 12.1 — Package loaded

```
M-x eval-expression RET (featurep 'org-habit) RET
```

If `nil`, open the agenda or a file with a `:STYLE: habit` property to trigger deferred load.

### 12.2 — Habit graph configuration

```
M-x eval-expression RET org-habit-graph-column RET    ; expect 60
M-x eval-expression RET org-habit-preceding-days RET  ; expect 21
M-x eval-expression RET org-habit-following-days RET  ; expect 7
```

### 12.3 — Nerd-icons glyph assignment (guarded)

```
M-x eval-expression RET (fboundp 'nerd-icons-codicon) RET
```

If `t`, then:

```
M-x eval-expression RET
  (characterp org-habit-completed-glyph) RET
```

**Pass:** `t` (a single character, not a string or nil). If `nerd-icons-codicon` is not available, `org-habit-completed-glyph` should retain its default value — not throw an error.

---

## 13. Org Pomodoro

### 13.1 — Alert rule installed

```
M-x eval-expression RET (featurep 'org-pomodoro) RET
```

Trigger load: `M-x org-pomodoro`. Then:

```
M-x eval-expression RET
  (cl-some (lambda (rule)
              (equal (plist-get rule :category) "org-pomodoro"))
            alert-rules) RET
```

**Pass:** Returns `t`.

### 13.2 — Cond alert style (the key FIX)

```
M-x eval-expression RET
  (plist-get
    (car (cl-remove-if-not
           (lambda (r) (equal (plist-get r :category) "org-pomodoro"))
           alert-rules))
    :style) RET
```

**Pass:** Returns a symbol (`libnotify`, `growl`, `notifier`, or the value of `alert-default-style`) — not an error. If it returns the result of _calling_ `alert-default-style` as a function, the old bug was not fixed.

---

## 14. Org Download

### 14.1 — Screenshot method

```
M-x eval-expression RET org-download-screenshot-method RET
```

**Pass:** Returns `"grim -g \"$(slurp)\" - | swappy -f - -o -"`. This is Wayland-specific (requires `grim` and `slurp`). On non-Wayland systems, screenshots will fail — test this only if you are on Wayland with those tools installed.

### 14.2 — Image directory and attach method

```
M-x eval-expression RET org-download-image-dir RET    ; expect "assets"
M-x eval-expression RET org-download-method RET       ; expect attach
M-x eval-expression RET org-image-actual-width RET    ; expect 600
```

### 14.3 — Drag-and-drop displays image

Drag an image file onto an `.org` buffer. It should be saved under the `assets/` attachment directory and inline-displayed. The `advice-add` for `org-download-dnd` calls `org-download-display-inline-images` after saving.

---

## 15. Org Remark

### 15.1 — Global tracking mode active

```
M-x eval-expression RET
  (bound-and-true-p org-remark-global-tracking-mode) RET
```

**Pass:** `t`.

### 15.2 — Basic highlight workflow

Open a text file or `.org` file. Select a region. Run `M-x org-remark-mark`. Confirm a highlight overlay appears. Close and reopen the file — the highlight should persist (tracked in a separate `.org` file in the same directory).

---

## 16. Eldoc for Org Mode

### 16.1 — Eldoc strategy set

Open an `.org` file. Check:

```
M-x eval-expression RET
  (eq eldoc-documentation-strategy
      'eldoc-documentation-compose) RET
```

**Pass:** `t`.

### 16.2 — Breadcrumb function

Move point inside a nested heading (e.g., a third-level heading). Wait ~1 second for eldoc to fire, or call directly:

```
M-x eval-expression RET
  (ar/org-eldoc-breadcrumb #'message) RET
```

**Pass:** `*Messages*` shows a heading path like `Chapter → Section → Subsection` with the final element in keyword-face.

**Failure Signal:** `void-function: org-get-outline-path` — indicates org was not yet loaded when this ran.

### 16.3 — Heading info function

Move point to a heading that has a TODO keyword, a priority, and at least one tag. Check `*Messages*` for the eldoc output, or call:

```
M-x eval-expression RET
  (ar/org-eldoc-heading-info #'message) RET
```

**Pass:** Shows the TODO keyword (coloured), `[#A]`-style priority, and `:tag:` string in one line.

### 16.4 — Link function

Move point onto an `[[id:...][Description]]` or `[[https://...][Link]]` link.

```
M-x eval-expression RET
  (ar/org-eldoc-link #'message) RET
```

**Pass:** Shows `Link: id:<uuid> [Description]` or similar.

### 16.5 — Table function

Place point inside an org table cell.

```
M-x eval-expression RET
  (ar/org-eldoc-table #'message) RET
```

**Pass:** Shows `Cell[row,col]`. If the cell has a formula, shows `Formula: =...` as well.

**Note:** The `row` value is computed relative to the table start using `count-lines` — verify the number increments correctly as you move down rows.

### 16.6 — Statistics on-demand (keybinding test)

In an `.org` buffer, position point on a heading. Press `C-c i s` (bound to `ar/org-show-statistics`).

**Pass:** `*Messages*` shows word count and task counts (e.g., `142 words │ Tasks: 2/5`).

**Failure Signal:** `wrong-type-argument: functionp, t` — indicates the `org-map-entries` lambda fix was not applied correctly.

### 16.7 — Src block info on-demand

Position point inside a source block. Press `C-c i b` (bound to `ar/org-show-src-block-info`).

**Pass:** `*Messages*` shows something like `Source: python [:session :exports both]`.

### 16.8 — Clock eldoc

Start clocking with `C-c C-x C-i` on a heading. The eldoc rotation should now include `CLOCKING: <task name>` in the echo area.

---

## 17. Org Appear

### 17.1 — Mode active

Open an `.org` file. Check that `org-appear-mode` is on:

```
M-x eval-expression RET (bound-and-true-p org-appear-mode) RET
```

**Pass:** `t`.

### 17.2 — Markers appear on cursor entry

In an `.org` buffer with `org-hide-emphasis-markers t`, place text like `*bold*`. With `org-appear-mode` active, moving point _onto_ the bold text should temporarily reveal the `*` markers; moving away hides them again.

---

---

# PART II — SECOND BRAIN & PRODUCTIVITY

---

## 18. Org Agenda (base configuration)

### 18.1 — Package loads

```
M-x org-agenda RET
```

No error on opening. The `*Org Agenda*` buffer appears.

### 18.2 — Time grid

Press `a` to enter the daily agenda view. The time grid should display at hours 8, 10, 12, 14, 16, 18, 20 with `┄┄┄┄┄` separator characters. The current-time indicator should show `◀ now ─────...`.

### 18.3 — Block separator

Check `org-agenda-block-separator`:

```
M-x eval-expression RET org-agenda-block-separator RET
```

**Pass:** Returns `?─` (character 9472), not a string.

### 18.4 — Agenda span

```
M-x eval-expression RET org-agenda-span RET
```

**Pass:** `day` (not `week`).

### 18.5 — Skipping done items

Add a SCHEDULED task to a test `.org` file. Mark it DONE. Open the daily agenda view. The done scheduled item should not appear (due to `org-agenda-skip-scheduled-if-done t`).

---

## 19. Org GTD

### 19.1 — Update ack suppresses warning

```
M-x eval-expression RET org-gtd-update-ack RET
```

**Pass:** Returns `"4.0.0"`. On startup, no blocking org-gtd upgrade dialog should have appeared.

**Critical:** This `setq` is in `:init` (before load). If it were in `:config` (after load), the warning dialog would fire on every startup.

### 19.2 — Package loads without error

Trigger deferred load:

```
M-x org-gtd-capture
```

Check `*Messages*` for any `void-function`, `void-variable`, or `wrong-type-argument` errors.

### 19.3 — `org-edna-mode` active

```
M-x eval-expression RET (bound-and-true-p org-edna-mode) RET
```

**Pass:** `t` (activated in `:config`).

**Failure Signal:** `nil` — means either `org-edna` was not installed (check `straight`), or the `:config` block did not run.

### 19.4 — GTD mode activates via idle timer

Wait 30 seconds after startup (or simulate: `M-x eval-expression RET (run-with-idle-timer 0 nil (lambda () (when (and (featurep 'org-gtd) (not (bound-and-true-p org-gtd-mode))) (org-gtd-mode 1)))) RET`).

```
M-x eval-expression RET (bound-and-true-p org-gtd-mode) RET
```

**Pass:** `t`.

### 19.5 — Keyword mapping

```
M-x eval-expression RET org-gtd-keyword-mapping RET
```

**Pass:** Returns an alist with entries for `todo`, `next`, `wait`, `done`, `canceled` mapped to `INBOX`, `GTD-NEXT`, `GTD-WAIT`, `GTD-DONE`, `GTD-CNCL` respectively.

**Note:** The config defines a `canceled` (no second `l`) key. Verify the spelling matches what `org-gtd` 4.0 expects — `org-gtd` uses `canceled` (American English). If it expects `cancelled`, the mapping will silently be ignored.

### 19.6 — `org-agenda-files` authoritatively set

```
M-x eval-expression RET org-agenda-files RET
```

**Pass:** A list containing `my/org-directory` and `my/org-gtd-directory`. It must NOT include `my/org-roam-directory` (knowledge notes must not appear in task views — a deliberate design decision noted in the config).

**Failure Signal:** `org-agenda-files` is empty (means org-gtd hasn't loaded yet), or includes the roam directory.

### 19.7 — Keyword conflict check

The config defines `org-todo-keywords` in both the `org` use-package (global) and the `org-gtd` use-package (`:custom`). Evaluate which one wins at runtime:

```
M-x eval-expression RET org-todo-keywords RET
```

The GTD-specific sequence `(sequence "INBOX" "GTD-NEXT" "GTD-WAIT" "|" "GTD-DONE" "GTD-CNCL")` must appear in the list alongside the standard sequences. The GTD `:custom` block has 3 sequences; the global org block has 4 (it includes `CRITICAL/COMMENT/RESOLVED/DISABLED`). Verify the final runtime value contains all keywords needed.

### 19.8 — GTD workflow end-to-end

1. Press `SPC g d c` — a capture buffer should open into the GTD inbox.
2. Type a test task, confirm with `C-c C-c`.
3. Press `SPC g d p` — opens the inbox-processing interface.
4. Assign the task as `GTD-NEXT`.
5. Press `SPC g d n` — the task should appear in the NEXT actions list.
6. Press `SPC g d e` — the daily engagement view.
7. Press `SPC g d s` — stuck projects view (no items if no projects exist yet).

**Failure Signal at any step:** `Symbol's function definition is void: org-gtd-capture` indicates the commands list is not being autoloaded.

---

## 20. Org Roam

### 20.1 — DB autosync enabled after idle

The `:hook` entry schedules `org-roam-db-autosync-enable` via a 10-second idle timer after `after-init`. After Emacs has been idle for 10+ seconds:

```
M-x eval-expression RET
  (bound-and-true-p org-roam-db-autosync-mode) RET
```

**Pass:** `t`.

**Failure Signal:** `nil` after 30+ seconds — the hook form was previously misplaced inside `:custom` (a known fix in the config). Verify this fix actually resolved the issue by checking that `after-init-hook` contains the lambda.

### 20.2 — Directory and subdirectory setup

```
M-x eval-expression RET org-roam-directory RET
```

**Pass:** Same value as `my/org-roam-directory` (i.e., `~/org/roam/`).

Check all subdirectories exist:

```
M-x eval-expression RET
  (mapcar #'file-directory-p
    (mapcar (lambda (d) (expand-file-name d org-roam-directory))
            '("zettel" "literature" "ideas" "projects"
              "daily" "reference" "concepts" "people" "meetings"))) RET
```

**Pass:** All `t`. Pay particular attention to `meetings/` — the capture template targets `meetings/${slug}.org` and hard-errors if the directory is absent.

### 20.3 — Node display template

```
M-x eval-expression RET org-roam-node-display-template RET
```

**Pass:** Returns a string containing `${title:*}` and a propertized `${tags:10}`.

### 20.4 — Capture templates — all 8 keys present

```
M-x eval-expression RET
  (mapcar #'car org-roam-capture-templates) RET
```

**Pass:** Returns `("z" "l" "p" "i" "r" "c" "P" "m")`.

Note: After org-roam loads, a GTD bridge template `"g"` is appended via `add-to-list`. Check:

```
M-x eval-expression RET
  (assoc "g" org-roam-capture-templates) RET
```

**Pass:** Non-nil.

### 20.5 — Zettel capture end-to-end

```
M-x org-roam-capture RET
```

Select `z` (zettel). Enter a title. Confirm with `C-c C-c`.

**Pass:** A new file is created under `~/org/roam/zettel/<slug>.org` with the correct header (`#+title:`, `#+filetags: :zettel:permanent:`, `#+date:`). The file has the full structure: Core Idea, Elaboration, Evidence, Connections, Source headings.

### 20.6 — Fleeting note capture

Select `i` (fleeting) from `org-roam-capture`. Multiple captures with `i` should all **prepend** to `ideas/fleeting.org` (`:prepend t`) — not create separate files.

**Failure Signal:** Each fleeting capture creates a new `fleeting-<slug>.org` file. This means `:target (file+head ...)` is not working as expected for the file-append case.

### 20.7 — Meeting capture creates file in `meetings/`

Select `m` from `org-roam-capture`. Enter a meeting title.

**Pass:** File is created at `~/org/roam/meetings/<slug>.org`. If the `meetings/` directory was missing, this would throw a `file-error: Creating directory` error.

### 20.8 — Daily notes

```
M-x org-roam-dailies-capture-today
```

Select `d` (default). Type a note and confirm.

**Pass:** Creates/appends to `~/org/roam/daily/YYYY-MM-DD.org`. The file header should contain `#+filetags: :daily:`.

Try `m` (morning review) — the template should insert a structured morning entry with Goals, Priorities, Notes sub-headings.

Try `e` (evening reflection) — four sub-headings: What went well, What could improve, Learnings, Tomorrow's focus.

### 20.9 — Node find and backlinks

```
SPC n f  (org-roam-node-find)
```

The completion interface should list all captured nodes. Select one. Press `SPC n l` (or `M-x org-roam-buffer-toggle`) — the `*org-roam*` backlinks buffer should appear on the right.

### 20.10 — org-roam-ui

```
M-x org-roam-ui-open
```

**Pass:** Opens a browser window (or tab) showing the network graph. If browser does not open automatically, check that `org-roam-ui-open-on-start` is `nil` (correct) and call `org-roam-ui-open` explicitly.

---

## 21. Zettelkasten ↔ GTD Bridges

### 21.1 — `ar/gtd-to-zettel`

Open a file in the GTD directory (e.g., `~/org/gtd/projects.org`). Move point to a project heading. Run:

```
SPC n g   (ar/gtd-to-zettel)
```

**Pass:**

1. A new Zettelkasten project note is created under `~/org/roam/projects/`.
2. A backlink is inserted under the GTD heading, formatted as `- Zettelkasten Note: [[id:<uuid>][Project: <heading>]]`.
3. `*Messages*` shows `Created Zettelkasten project note: Project: <heading>`.

**Failure Signal:** `"Note created, but ID could not be resolved yet"` — means `org-roam-db-sync` ran but the new node's ID was not queryable via `org-roam-node-from-title-or-alias`. This can happen if org-roam-db-sync did not complete before the lookup. Retry after running `M-x org-roam-db-sync` manually.

### 21.2 — `ar/zettel-to-gtd`

Open a file inside `my/org-roam-directory` (e.g., a zettel note). Ensure it has at least one incomplete TODO item. Run:

```
SPC n G   (ar/zettel-to-gtd)
```

**Pass:**

1. `*Messages*` shows `Sent N tasks to GTD inbox`.
2. `~/org/gtd/inbox.org` now contains the extracted tasks with a backlink to the source note.

**Failure Signal:** `user-error: Must be in an org-roam buffer` — triggered if the current buffer's file is not under `my/org-roam-directory`. Verify `buffer-file-name` starts with the correct path.

**Failure Signal:** `wrong-type-argument: functionp, t` — the `org-map-entries` lambda fix was not applied. This is distinct from the original bug where bare `t` was passed.

### 21.3 — `ar/fleeting-to-permanent`

```
SPC n p   (ar/fleeting-to-permanent)
```

**Pass:** Opens `~/org/roam/ideas/fleeting.org` at the top of the file.

**Failure Signal:** `user-error: No fleeting notes file found` — the file was not created by the directory bootstrap (test 1.2 must pass first).

### 21.4 — `ar/weekly-review`

```
SPC n w   (ar/weekly-review)
```

**Pass:**

1. Creates `~/org/reviews/YYYY-WNN-review.org` (if it doesn't already exist).
2. The file contains live statistics: inbox count, NEXT count, WAIT count, fleeting note count.
3. Opens the file and places point at the `** What Went Well?` heading.

**Failure Signal:** `wrong-type-argument: functionp, t` on any of the `org-map-entries` calls (all three use the fixed lambda form — confirm all three are present).

**Note:** The weekly review file uses `%U` week numbering. Verify the filename matches the current week number with `(format-time-string "%Y-W%U")`.

---

## 22. Org Capture

### 22.1 — Smart capture dispatch

```
SPC X   (ar/capture-dispatch)
```

A `read-char-choice` prompt appears: `Capture: [t]ask [n]ote [f]leeting [j]ournal [q]uit?`

Test each option:

- `t` → calls `org-gtd-capture` (GTD inbox).
- `n` → calls `org-roam-capture` (Zettelkasten note).
- `f` → opens `fleeting.org`, prompts for a thought with `read-string`, appends it.
- `j` → triggers the `j` journal capture template.
- `q` → quits cleanly with `keyboard-quit`.

**Failure Signal for `f`:** `void-function: org-roam-capture` if org-roam hasn't loaded yet (deferred load). Trigger org-roam load first.

### 22.2 — Journal capture template

```
M-x org-capture RET j RET
```

**Pass:** Opens `~/org/journal.org`, adds an entry under a date-tree subtree (`file+olp+datetree`). The entry heading is the current time in `HH:MM` format.

### 22.3 — Meeting capture template

```
M-x org-capture RET m RET
```

**Pass:** Opens `~/org/journal.org` under the `Meetings` headline (which must exist — created by the seed file). The capture buffer has `:clock-in t` so a clock should start automatically. Confirm with `C-c C-c` and verify the clock stopped.

**Failure Signal:** `error: Cannot find target heading "Meetings"` — means `journal.org` was not seeded with the `* Meetings` heading, or the heading was deleted.

### 22.4 — Book/article capture template

```
M-x org-capture RET b RET
```

**Pass:** Opens `~/org/reading.org` as a new top-level entry with `:read:` tag and `AUTHOR`, `TYPE`, `STATUS` properties.

### 22.5 — Habit capture template

```
M-x org-capture RET h RET
```

**Pass:** Opens `~/org/habits.org` under the `Habits` headline. The entry template includes `SCHEDULED: <%Y-%m-%d %a .+1d>` — the `.+1d` repeat cookie makes it a habit. The `:STYLE: habit` property must also be present.

---

## 23. Org Super Agenda

### 23.1 — Mode active in agenda

Open the agenda (`M-x org-agenda`). Check:

```
M-x eval-expression RET
  (bound-and-true-p org-super-agenda-mode) RET
```

**Pass:** `t` (the `:hook` in the use-package block adds it to `org-agenda-mode-hook`).

### 23.2 — Default grouping applied

Open the daily agenda view (`a` from agenda dispatcher). The view should be divided into named sections including `🔴 Overdue`, `🔥 GTD Inbox (Process Today)`, `⚡ Today's Schedule`, `🎯 High Priority`, `💻 @Computer`, `🏠 @Home`, `📞 @Phone`, `⏳ Waiting For`, `📆 Due Soon`, `📋 Other Tasks`.

**Failure Signal:** All tasks appear in a flat list with no named groups — `org-super-agenda-mode` not active.

### 23.3 — No roam/projects group present

Verify there is NO group titled `🚀 Active Projects (Zettelkasten)` in the default view (it was intentionally removed in the FIX comment because roam files are excluded from `org-agenda-files`).

---

## 24. Org Agenda Custom Commands

### 24.1 — Custom command dispatcher

From the agenda dispatcher:

```
M-x org-agenda RET d RET   ; Daily Dashboard
```

**Pass:** Opens a split view with an agenda block (time-blocked today, overdue, inbox) and an alltodo block (priority A, contexts, waiting).

```
M-x org-agenda RET w RET   ; Weekly Review
```

**Pass:** Four blocks: GTD Inbox, Waiting For, Stuck Projects, All NEXT Actions.

```
M-x org-agenda RET p RET   ; Projects Dashboard
```

**Pass:** One block — GTD projects from `gtd/projects.org` grouped as Stuck, Active, Paused. There should be NO second block pulling from `roam/projects` (removed in the FIX).

```
M-x org-agenda RET g RET   ; GTD Views group
```

**Pass:** Shows a submenu. Then:

```
M-x org-agenda RET gn RET  ; Next Actions
M-x org-agenda RET gi RET  ; Inbox
M-x org-agenda RET gw RET  ; Waiting For
M-x org-agenda RET gs RET  ; Stuck Projects
M-x org-agenda RET gc RET  ; By Context
```

Each should produce the corresponding agenda view without error.

### 24.2 — Context view keybindings test

The `"gc"` view combines four `tags-todo` blocks. Verify all four contexts (`@computer`, `@home`, `@errands`, `@phone`) appear as separate sections. The query format is `"@computer/GTD-NEXT"` — confirm the slash-separated keyword filter works (it restricts to `GTD-NEXT` state only, not all states).

### 24.3 — Stuck projects definition

The `"gs"` view uses `(stuck "")`. By default, org-agenda considers a project stuck if it has no `NEXT` (or equivalent) subtask. Verify:

1. Create a test project heading in `gtd/projects.org` with one `TODO` child but no `GTD-NEXT` child.
2. Run `SPC g d s` or the `"gs"` agenda command.
3. The test project should appear as stuck.

---

## 25. Cross-section Integration Tests

These tests exercise interactions between the Org Mode and Second Brain sections together.

### 25.1 — Org-capture → Org-agenda visibility

1. Capture a task via `SPC X t` (GTD inbox).
2. Open the daily agenda (`SPC n a d`).
3. The task should appear in the `🔥 GTD Inbox (Process Today)` group.

### 25.2 — Org-roam capture → no agenda pollution

Capture a new zettel note via `SPC X n`. Open the daily agenda. The zettel note should NOT appear anywhere in the agenda (because `org-roam-directory` is excluded from `org-agenda-files`).

### 25.3 — Per-project context with org-roam

Register a test project using `my/register-project-org-context` with a custom `:roam-dir` and `:roam-db`. Open a file in that project. Verify:

- `org-roam-directory` has switched to the project-specific value.
- `org-roam-db-location` points to the project-specific DB.
- After leaving the project (open a file outside it), both values revert to global.

### 25.4 — Weekly review statistics accuracy

1. Ensure there is at least one `INBOX` item in `gtd/inbox.org`.
2. Run `SPC n w`.
3. In the generated review file, confirm `Inbox items: *N*` reflects the actual count.

**Failure Signal:** Count shows 0 even with items present — means the `org-map-entries` call in `ar/weekly-review` is not finding the GTD inbox file. Verify `my/org-gtd-directory` resolves to the correct path.

### 25.5 — Perspective + org-agenda isolation

1. Create two perspectives: `M-x persp-switch RET work RET` and `M-x persp-switch RET personal RET`.
2. Open the agenda in `work`. Switch to `personal`. Open the agenda again.
3. Each perspective should have its own `*Org Agenda*` buffer (Treemacs is set to `Perspectives` scope; agenda isolation depends on whether perspective.el isolates buffers by name — verify both agendas reflect the same `org-agenda-files` since no per-project override is active).

---

## 26. Keybinding Verification

All keybindings below are defined in the `General Keybindings` section but exercise the Org Mode and Second Brain systems. Verify each is registered and calls the correct function.

Run for any binding to check it:

```
M-x describe-key RET <keybinding>
```

| Keybinding           | Expected Command                       |
| -------------------- | -------------------------------------- |
| `SPC X`              | `ar/capture-dispatch`                  |
| `SPC g d c`          | `org-gtd-capture`                      |
| `SPC g d p`          | `org-gtd-process-inbox`                |
| `SPC g d e`          | `org-gtd-engage`                       |
| `SPC g d n`          | `org-gtd-show-all-next`                |
| `SPC g d s`          | `org-gtd-show-stuck-projects`          |
| `SPC n c`            | `ar/capture-dispatch`                  |
| `SPC n f`            | `org-roam-node-find`                   |
| `SPC n F`            | `consult-org-roam-file-find`           |
| `SPC n i`            | `org-roam-node-insert`                 |
| `SPC n s`            | `consult-org-roam-search`              |
| `SPC n l`            | `org-roam-buffer-toggle`               |
| `SPC n b`            | `consult-org-roam-backlinks`           |
| `SPC n B`            | `consult-org-roam-backlinks-recursive` |
| `SPC n L`            | `consult-org-roam-forward-links`       |
| `SPC n v`            | `org-roam-graph`                       |
| `SPC n u`            | `org-roam-ui-open`                     |
| `SPC n j`            | `org-roam-dailies-capture-today`       |
| `SPC n g`            | `ar/gtd-to-zettel`                     |
| `SPC n G`            | `ar/zettel-to-gtd`                     |
| `SPC n p`            | `ar/fleeting-to-permanent`             |
| `SPC n w`            | `ar/weekly-review`                     |
| `SPC n a d`          | daily agenda                           |
| `SPC n a w`          | weekly agenda                          |
| `SPC n a p`          | projects agenda                        |
| `SPC TAB TAB`        | `persp-switch`                         |
| `SPC TAB P s`        | `ar/persp-project-switch`              |
| `SPC TAB P p`        | `ar/persp-project-perspective`         |
| `SPC TAB P k`        | `ar/persp-project-kill`                |
| `, b t` (org-mode)   | `org-babel-tangle`                     |
| `, b e` (org-mode)   | `org-babel-execute-src-block`          |
| `C-c i s` (org-mode) | `ar/org-show-statistics`               |
| `C-c i b` (org-mode) | `ar/org-show-src-block-info`           |

**Note on `SPC TAB B` and `SPC TAB X` and `SPC TAB l`:** These bind `ar/persp-switch-buffer-all`, `ar/persp-kill-other-buffers`, and `ar/persp-list-all-buffers` respectively. These functions are defined inside a commented-out block in the config (the `Helper Functions` subsection of Perspective). Pressing these bindings will produce `void-function` errors until the block is uncommented.

---

## 27. Deferred Load Timing Tests

Because the configuration uses aggressive deferral (`use-package :defer t`), some components only activate after specific triggers or idle timers. Test each deferred activation point explicitly.

| Component              | Trigger                           | Expected activation                                  |
| ---------------------- | --------------------------------- | ---------------------------------------------------- |
| `org`                  | Open any `.org` file              | `(featurep 'org)` → `t`                              |
| `org-agenda`           | `M-x org-agenda`                  | `(featurep 'org-agenda)` → `t`                       |
| `org-gtd`              | `M-x org-gtd-capture`             | `(featurep 'org-gtd)` → `t`                          |
| `org-roam`             | `M-x org-roam-node-find`          | `(featurep 'org-roam)` → `t`                         |
| `org-roam-ui`          | `M-x org-roam-ui-open`            | `(featurep 'org-roam-ui)` → `t`                      |
| `org-habit`            | Open agenda after loading org     | `(featurep 'org-habit)` → `t`                        |
| `org-pomodoro`         | `M-x org-pomodoro`                | `(featurep 'org-pomodoro)` → `t`                     |
| `org-download`         | `M-x org-download-screenshot`     | `(featurep 'org-download)` → `t`                     |
| `org-remark`           | `M-x org-remark-mark`             | `(featurep 'org-remark)` → `t`                       |
| `org-capture`          | `M-x org-capture`                 | `(featurep 'org-capture)` → `t`                      |
| `org-super-agenda`     | Open agenda (hooked)              | `(featurep 'org-super-agenda)` → `t`                 |
| `perspective`          | Emacs startup (`after-init` hook) | `(featurep 'perspective)` → `t`                      |
| `consult-org-roam`     | `M-x consult-org-roam-search`     | `(featurep 'consult-org-roam)` → `t`                 |
| `org-gtd-mode`         | After 30s idle                    | `(bound-and-true-p org-gtd-mode)` → `t`              |
| `org-roam-db-autosync` | After 10s idle (after-init)       | `(bound-and-true-p org-roam-db-autosync-mode)` → `t` |

---

## 28. Dirvish Syntax Error Check

The Dirvish use-package block contains a structural issue to verify:

```elisp
(setq dirvish-side-width 30
      dirvish-side-follow-buffer-file t)
      dirvish-side-display-alist '((side . bottom) (slot . -1)))
```

The closing paren on the `setq` form lands after `follow-buffer-file t` — the `dirvish-side-display-alist` line is outside the `setq`. This means it is parsed as a stray closing paren followed by a bare symbol form, which is a syntax/runtime error.

To test:

```
M-x eval-expression RET dirvish-side-display-alist RET
```

**Pass:** Returns the alist `((side . bottom) (slot . -1))`.

**Failure Signal:** `void-variable: dirvish-side-display-alist` — means the assignment never ran, confirming the paren issue. If so, open Dirvish (`M-x dirvish-side`) and see if it errors or produces unexpected layout behavior (side panel appearing at bottom instead of a side position).

---

## 29. `*Messages*` Final Sweep

After completing all tests above, review the full `*Messages*` log:

```
M-x view-echo-area-messages
```

Look for any of the following patterns that indicate problems in these two sections:

- `void-function: org-...` — a function called before its package loaded
- `void-variable: org-...` — a variable referenced before it was defined
- `wrong-type-argument: functionp, t` — bare `t` passed as a function (unfixed `org-map-entries` call)
- `Symbol's function definition is void: alert-default-style` — the alert cond bug in org-pomodoro (should be fixed)
- `user-error: org-roam-directory...` — roam directory path issues
- `file-error: No such file or directory` — a capture template targets a non-existent directory
- `org-gtd upgrade` message (not an error, but indicates `org-gtd-update-ack` is wrong or missing)
- `Org-roam-capture -: Symbol's function definition is void: org-roam-node-create` — org-roam not loaded before a capture attempt

---

_End of Testing Guide_
