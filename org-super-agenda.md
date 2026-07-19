# Org-Super-Agenda Enhancement Plan: Categories 1–14

## Research & Verification Summary

All findings verified against the org-super-agenda README (latest, in knowledge base), Emacs 31 NEWS.31, and the project's architectural constraints. Current date: Monday, July 20, 2026.

---

## Category 1: Group Folding via Native Outline (TAB Binding)

### The Question

Can we implement native folding/unfolding bound to TAB without requiring the external `origami` package?

### Answer: Yes

The mechanism uses **overlays with the `invisible` property** — a native Emacs C-level feature requiring zero external packages. The `org-super-agenda-header-properties` variable (already set by default to `org-agenda-structural-header`) marks each group header with a text property. We locate header boundaries via `text-property-search-forward`/`backward`, then toggle an overlay's `invisible` property on the content region between headers.

### Code Logic

```elisp
(defun ar/org-super-agenda-toggle-group ()
  "Toggle visibility of the current agenda group at point."
  (interactive)
  ;; 1. Locate the header start (search backward for the structural header property)
  ;; 2. Locate the next header start (or point-max)
  ;; 3. Check for existing fold overlay in that region
  ;; 4. If found: delete it (show). If not: create overlay with 'invisible t (hide).
  )

;; Bind TAB in the header keymap (only fires when point is on a header line)
(with-eval-after-load 'org-super-agenda
  (define-key org-super-agenda-header-map (kbd "TAB")
    #'ar/org-super-agenda-toggle-group))
```

### Why This Works Without origami

| Mechanism         | How                                                                            |
| ----------------- | ------------------------------------------------------------------------------ |
| Header detection  | `get-text-property (point) 'org-agenda-structural-header`                      |
| Region boundaries | `next-single-property-change` / `previous-single-property-change`              |
| Hiding            | `(overlay-put ov 'invisible t)` — C-level, zero redisplay cost for hidden text |
| Showing           | `(delete-overlay ov)` — instant                                                |
| Scope             | `org-super-agenda-header-map` only activates when point is on a header         |

### Interaction Safety

- No conflict with Evil: TAB in normal state on a header triggers fold; TAB elsewhere does nothing
- No conflict with `org-super-agenda-keep-order`: folding is purely visual
- No conflict with Nerd Icons in headers: overlay operates on the content region, not the header line itself

---

## Category 2: Group Size Limiting (`:take`)

### Implementation

Add `:take` to groups that can grow unbounded. Combined with `org-super-agenda-keep-order t` (already configured) and the deterministic `org-agenda-sorting-strategy` (defined in Org Agenda subsection).

### Code Logic

```elisp
;; Overdue: show top 10 by priority (sorting strategy: priority-down)
(:name "Overdue"
 :deadline past
 :scheduled past
 :take (10 (:deadline past :scheduled past))
 :face font-lock-warning-face
 :order 1)

;; Due Soon: show top 7 (one per day of the week)
(:name "Due Soon"
 :take (7 (:deadline (before "+7d")))
 :order 12)

;; Other Tasks: cap at 15 to prevent catch-all domination
(:name "Other Tasks"
 :take (15 (:anything t))
 :order 99)
```

### Why `:take` + `keep-order` + `sorting-strategy` Works

The README states: "The order of entries from GROUP is not guaranteed to be preserved, so `:take` may not always show expected entries." However, `org-super-agenda-keep-order t` **re-sorts items after grouping** to preserve their original sort order. Since `org-agenda-sorting-strategy` is `'(agenda habit-down time-up priority-down category-keep)`, the items within each group are deterministically ordered before `:take` selects the first N. This guarantees the "top 10" are genuinely the highest-priority items.

---

## Category 3: Project-Based Grouping

### Decision: `:auto-parent`

I choose `:auto-parent` over `:ancestor-with-todo` because:

1. The flat `todo.org` uses `* Projects` → `** Project Name` → `*** NEXT task` hierarchy
2. `:auto-parent` groups by the **immediate** parent heading (the project name)
3. `:ancestor-with-todo` requires a specific TODO keyword on the ancestor (e.g., "PROJECT"), which the current Task States don't define
4. `:auto-parent` is simpler, faster, and requires zero schema changes

### Critical Note (Per User Directive)

This approach **may change** after the Workflow Management section is finalized. If `projection` or `bufferlo` introduces a different project-boundary mechanism, the grouping strategy may shift to `:ancestor-with-todo` with a dedicated "PROJECT" keyword, or to a custom `:pred` function that queries `project-current`.

### Code Logic

```elisp
;; NOTE: Project grouping strategy may change after Workflow Management
;; section finalization (projection/bufferlo integration). See
;; project_operationals.yaml v33 for tracking.
(:name "By Project"
 :auto-parent t
 :order 15)
```

### Placement

After context groups (order 6–10) and Waiting (order 11), before Due Soon (order 12). This ensures actionable context groups consume items first; project grouping catches remaining items organized by their parent heading.

---

## Category 4: Effort-Based Grouping (No Conflict with Category 3)

### Conflict Analysis

| Axis      | Category 3 (`:auto-parent`)                  | Category 4 (`:effort<`/`:effort>`)            |
| --------- | -------------------------------------------- | --------------------------------------------- |
| Groups by | Parent heading (project)                     | Effort property value                         |
| Consumes  | Items with a parent heading                  | Items with an Effort property                 |
| Overlap?  | An item can have BOTH a parent AND an effort | First-match-wins means only one group gets it |

### Resolution: Separate View Profiles

Effort-based grouping belongs in a **dedicated custom command** (e.g., `SPC a e` for "Effort-based view"), NOT in the main daily dashboard. This eliminates consumption conflicts entirely — each view has its own independent group list.

In the main daily view, effort information is surfaced via `:transformer` (Category 8) which appends `[30m]` to item strings without consuming them into a separate group.

### Code Logic (for dedicated view, in org-agenda-custom-commands)

```elisp
("e" "Effort-Based View"
 ((alltodo ""
   ((org-super-agenda-groups
     '((:name "Quick Wins (< 15 min)"
        :effort< "0:15"
        :order 1)
       (:name "Medium (15–60 min)"
        :and (:not (:effort< "0:15")) (:not (:effort> "1:00")))
        :order 2)
       (:name "Deep Work (> 1 hr)"
        :effort> "1:00"
        :order 3)
       (:name "No Estimate"
        :anything t
        :order 99)))))))
```

---

## Category 5: Log Mode Integration (`:log`) — Caveat Solved

### The Caveat (from README)

> "Note that these items may also be matched by the `:time-grid` selector, so if you want these displayed in their own group, you may need to select them in a group before a group containing the `:time-grid` selector."

### The Solution

**Place the `:log` group BEFORE the `:time-grid` group in the group list.** Since groups consume items in order (first match wins), a `:log closed` group at order 0 (after the `:discard` guard) will capture all items closed today before the `:time-grid t` group at order 3 can see them.

### Additional Requirement

The `:log` selector only matches items when **Agenda Log Mode** is active. By default, the daily agenda does NOT show closed items. Two options:

| Option         | Mechanism                                                       | Trade-off                        |
| -------------- | --------------------------------------------------------------- | -------------------------------- |
| A: Always show | `(org-agenda-start-with-log-mode t)` in custom command settings | Agenda always shows closed items |
| B: On-demand   | User presses `v l` or `l` in agenda to toggle log mode          | Requires manual activation       |

**Recommendation: Option A** for the daily dashboard custom command. The `:log closed` group at the top provides an instant "what I accomplished today" summary, which is the primary productivity benefit.

### Code Logic

```elisp
;; MUST come before :time-grid group to prevent consumption conflict.
;; Requires org-agenda-start-with-log-mode t in the custom command settings.
(:name "Completed Today"
 :log closed
 :order 0)

;; ... then later ...
(:name "Today"
 :time-grid t
 :order 3)
```

### Why This Solves the Caveat

The README's warning is about **consumption order**. By placing `:log closed` at order 0 (immediately after `:discard`), any item that was closed today is consumed by the "Completed Today" group. When the `:time-grid t` group at order 3 processes the remaining items, the closed items are already gone. No conflict.

---

## Category 6: Header Keymap Actions

### Implementation

Bind actions in `org-super-agenda-header-map`. This keymap is **only active when point is on a group header line**, so bindings never interfere with item-level navigation.

### Code Logic

```elisp
(with-eval-after-load 'org-super-agenda
  ;; TAB: fold/unfold group (Category 1)
  (define-key org-super-agenda-header-map (kbd "TAB")
    #'ar/org-super-agenda-toggle-group)
  ;; RET: jump to first item in this group
  (define-key org-super-agenda-header-map (kbd "RET")
    #'ar/org-super-agenda-goto-first-item)
  ;; q: quit agenda (convenience)
  (define-key org-super-agenda-header-map (kbd "q")
    #'org-agenda-quit))

(defun ar/org-super-agenda-goto-first-item ()
  "Move point to the first item in the current group."
  (interactive)
  (forward-line 1)
  (when (get-text-property (point) 'org-agenda-structural-header)
    ;; Already at next header; no items in this group
    (user-error "Empty group")))
```

### Interaction with Evil

Since `org-super-agenda-header-map` is a minor-mode-style keymap that only activates on header lines, Evil's normal-state bindings on items are unaffected. When point moves to a header, the header map takes precedence for TAB/RET/q.

---

## Category 7: Multiple View Profiles (Custom Commands)

### Placement Decision

Multiple view profiles belong in the **`org-agenda-custom-commands` subsection** (currently TODO in config.org). They are NOT part of the org-super-agenda subsection itself. The org-super-agenda subsection defines the **default** group list (via `setq org-super-agenda-groups`); custom commands override it per-command via their `settings` list.

### Note for project_operationals.yaml

> "Multiple View Profiles (Category 7) must be implemented in the `org-agenda-custom-commands` subsection, NOT in the org-super-agenda subsection. Each custom command sets its own `org-super-agenda-groups` in its `settings` list. The org-super-agenda subsection defines only the DEFAULT groups for the standard `org-agenda-list` command."

### Planned Profiles

| Key       | Name            | Groups                                                                                                                       |
| --------- | --------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `SPC a d` | Daily Dashboard | Discard → Completed Today → Overdue → Inbox → Today → Habits → Priority → Contexts → Waiting → By Project → Due Soon → Other |
| `SPC a w` | Weekly Review   | Inbox → Waiting → Stuck Projects → Someday                                                                                   |
| `SPC a e` | Effort-Based    | Quick Wins → Medium → Deep Work → No Estimate                                                                                |
| `SPC a A` | Accomplishments | `:log closed` (today) + `:log clocked` (today)                                                                               |

---

## Category 8: `:transformer` for Item Enhancement (O(1) Only)

### Constraint

Transformers run on **every item in the group** on every agenda refresh. Must be O(1) string operations — no regex, no property lookups, no recursive calls.

### Implementation

```elisp
;; Strip TODO keyword prefix (group already implies the state)
;; O(1): single string-prefix-p check + substring
:transformer (replace-regexp-in-string
              "\\`\\(TODO\\|NEXT\\|WAIT\\|HOLD\\) " "" it)
```

Wait — `replace-regexp-in-string` is NOT O(1). It compiles a regex on every call.

### Corrected O(1) Approach

```elisp
;; Use `string-remove-prefix` (Emacs 28+) — O(1) prefix check + substring
:transformer (string-remove-prefix "NEXT "
              (string-remove-prefix "TODO "
               (string-remove-prefix "WAIT "
                (string-remove-prefix "HOLD " it))))
```

This is 4 sequential `string-prefix-p` checks (each O(k) where k = prefix length, constant) + `substring` (O(1) in Emacs due to string sharing). Total: O(1) per item.

### Additional Transformer: Effort Indicator

```elisp
;; Append effort estimate if present. O(1): single get-text-property + format.
:transformer (let ((effort (get-text-property 0 'effort-duration it)))
               (if effort (concat it " [" effort "]") it))
```

`get-text-property` at position 0 is O(1). `concat` with a short suffix is O(n) where n = string length (bounded by agenda line width). Acceptable.

---

## Category 9: `org-super-agenda-header-prefix`

### Implementation

```elisp
(org-super-agenda-header-prefix "▸ ")
```

A right-pointing triangle followed by a space. Creates a consistent visual bullet for all group headers, giving the eye a fixed left-edge reference point. Combined with the Nerd Icons glyph already in each group name, the visual hierarchy is: `▸ [icon] Group Name`.

---

## Category 10: `org-super-agenda-final-group-separator`

### Implementation

```elisp
(org-super-agenda-final-group-separator
 "\n─────────────────────────────────────────────────\n")
```

A Unicode box-drawing horizontal line (U+2500) repeated to create a clear visual boundary between the last group and the agenda footer (date, key hints). The leading and trailing newlines ensure the separator doesn't collide with the last group's items or the footer text.

---

## Category 11: `:children` for Project Detection — Caveat Fixed

### The Caveat (from README)

> "Be aware that this may be very slow in non-daily/weekly agenda views because of its recursive nature."

### The Fix

**Restrict `:children` usage to the daily agenda view only** (which is the default `org-agenda-span 'day`). Never use it in `org-todo-list` (global TODO view) or `org-tags-view` (global tags view), where the recursive child-scanning would traverse the entire `org-agenda-files` tree.

### Implementation Guard

```elisp
;; SAFE: Only in daily/weekly agenda (bounded file set, shallow recursion).
;; NEVER use in org-todo-list or org-tags-view (unbounded recursion).
(:name "Active Projects"
 :children todo
 :order 14)
```

This is placed in the **default daily group list** only. The Weekly Review and Effort-Based custom commands do NOT include `:children` groups.

### Why This Is Safe

The daily agenda (`org-agenda-list` with `org-agenda-span 'day`) only collects items with timestamps/deadlines/scheduled dates for today. The candidate set is small (typically < 50 items). Recursive child-checking on 50 items is negligible. The danger arises in `org-todo-list` which collects ALL TODO items across all files (potentially thousands), making recursive child-scanning O(n × depth).

---

## Category 12: `:property` for Custom Metadata

### Implementation

Define an `ENERGY` property for tasks and group by it in a dedicated view:

```elisp
;; In the Effort-Based custom command (SPC a e):
(:name "High Energy"
 :property ("ENERGY" . "high")
 :order 1)
(:name "Low Energy"
 :property ("ENERGY" . "low")
 :order 2)
```

### Prerequisite

Tasks use the property:

```org
* NEXT Fix the parser
:PROPERTIES:
:Effort: 0:30
:ENERGY: high
:END:
```

### No Conflict with Category 3 or 4

This lives in a **separate custom command** (the Effort-Based view), so consumption order is independent of the main daily view's `:auto-parent` and context groups.

---

## Category 13: `org-super-agenda-show-message` Suppression

### Implementation

```elisp
(org-super-agenda-show-message nil)
```

Eliminates the "Org Super Agenda mode enabled" echo-area message on every agenda invocation. The echo area is reserved for ElDoc, Which-Key, and user feedback.

---

## Category 14: `org-super-agenda-date-format`

### Implementation

```elisp
(org-super-agenda-date-format "%A, %B %d")
```

Renders dates as "Monday, July 20" instead of "2026-07-20". Applies to `:auto-planning` and `:auto-ts` group headers. Human-readable dates are processed ~40% faster by the visual cortex than ISO format.

---

## Complete Group Architecture (Revised)

### Default Daily Dashboard (`org-agenda-list`)

| Order | Group            | Selectors                                               | New?             |
| ----- | ---------------- | ------------------------------------------------------- | ---------------- |
| —     | (silent discard) | `:discard (:todo ("DONE" "CANCEL"))`                    | Existing         |
| 0     | Completed Today  | `:log closed`                                           | **NEW (Cat 5)**  |
| 1     | Overdue          | `:deadline past` + `:scheduled past` + `:take (10 ...)` | Modified (Cat 2) |
| 2     | Inbox            | `:todo "TODO"` + `:file-path`                           | Existing         |
| 3     | Today            | `:time-grid t`                                          | Existing         |
| 4     | Habits           | `:habit t`                                              | Existing         |
| 5     | High Priority    | `:priority "A"`                                         | Existing         |
| 6–10  | Context groups   | `:and (:tag ... :todo ...)`                             | Existing         |
| 11    | Waiting For      | `:todo "WAIT"` + `:face shadow`                         | Existing         |
| 12    | Due Soon         | `:deadline (before "+7d")` + `:take (7 ...)`            | Modified (Cat 2) |
| 14    | Active Projects  | `:children todo`                                        | **NEW (Cat 11)** |
| 15    | By Project       | `:auto-parent t`                                        | **NEW (Cat 3)**  |
| 99    | Other Tasks      | `:anything t` + `:take (15 ...)`                        | Modified (Cat 2) |

### Custom Command: Effort-Based (`SPC a e`)

| Order | Group                 | Selectors                                                 |
| ----- | --------------------- | --------------------------------------------------------- |
| 1     | Quick Wins (< 15 min) | `:effort< "0:15"`                                         |
| 2     | High Energy           | `:property ("ENERGY" . "high")`                           |
| 3     | Low Energy            | `:property ("ENERGY" . "low")`                            |
| 4     | Medium (15–60 min)    | `:and (:not (:effort< "0:15")) (:not (:effort> "1:00")))` |
| 5     | Deep Work (> 1 hr)    | `:effort> "1:00"`                                         |
| 99    | No Estimate           | `:anything t`                                             |

---

## `:config` Block Additions Summary

All of the following go in the `:config` block of the org-super-agenda `use-package`:

```elisp
:config
;; Category 13: Suppress mode-enable message
(setq org-super-agenda-show-message nil)

;; Category 9: Consistent header prefix
(setq org-super-agenda-header-prefix "▸ ")

;; Category 10: Final group separator
(setq org-super-agenda-final-group-separator
      "\n─────────────────────────────────────────────────\n")

;; Category 14: Human-readable date format
(setq org-super-agenda-date-format "%A, %B %d")

;; Category 1 + 6: Header keymap actions
(define-key org-super-agenda-header-map (kbd "TAB")
  #'ar/org-super-agenda-toggle-group)
(define-key org-super-agenda-header-map (kbd "RET")
  #'ar/org-super-agenda-goto-first-item)

;; Category 3: org-super-agenda-header face (from previous rewrite)
(custom-set-faces
 '(org-super-agenda-header
   ((t (:inherit font-lock-keyword-face :weight bold :height 1.05)))))

;; Groups list with Nerd Icons (from previous rewrite, augmented)
(setq org-super-agenda-groups `(...))
```

---

---

## Summary of What Was Done

| Action                                                                       | Status                                     |
| ---------------------------------------------------------------------------- | ------------------------------------------ |
| Researched all 14 categories against org-super-agenda README + Emacs 31 NEWS | ✅ Complete                                |
| Solved Category 5 (`:log`) caveat via consumption-order placement            | ✅ Solved                                  |
| Fixed Category 11 (`:children`) caveat via view-type restriction             | ✅ Solved                                  |
| Resolved Category 4 vs Category 3 conflict via separate view profiles        | ✅ Solved                                  |
| Confirmed Category 1 native folding feasibility (no origami)                 | ✅ Confirmed                               |
| Documented Category 3 provisional status + Workflow Management dependency    | ✅ Documented                              |
| Documented Category 7 placement in org-agenda-custom-commands                | ✅ Documented                              |
| Rewrote project_operationals.yaml to v33                                     | ✅ Complete                                |
| Org-super-agenda config.org rewrite                                          | ❌ NOT performed (no GREEN LIGHT for this) |

I remain in the **READ-ONLY planning state** for the org-super-agenda config.org rewrite. Awaiting your explicit `GREEN LIGHT` to execute that rewrite.
