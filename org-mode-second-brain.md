# Combined Section: Name & Subsection Order

## Recommended Section Name

**`* TODO Org Mode & Second Brain`**

### Rationale

| Candidate                         | Verdict            | Reason                                                                                                                                      |
| --------------------------------- | ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------- |
| `Org Mode & Second Brain`         | ✅ **Recommended** | Already used in your executive plan documents. Captures both the tool (Org) and the paradigm (Second Brain). Accessible to non-programmers. |
| `Org Workflow & Knowledge Engine` | ❌                 | Violates "NO Technical Jargon in Headers" — "engine" is jargon.                                                                             |
| `Org Ecosystem`                   | ❌                 | Too vague. Doesn't convey the dual-domain (Factory + Library) architecture.                                                                 |
| `Knowledge & Task Management`     | ❌                 | Loses the "Org" identity. Could be confused with external tools.                                                                            |
| `Org Mode & Productivity`         | ⚠️ Acceptable      | "Productivity" is broad and doesn't specifically evoke the Zettelkasten/Denote paradigm.                                                    |

The name `Org Mode & Second Brain` is already established in your `org-executive-plan.md` (Section 4: "Workflow, Agenda, and Denote Integration") and `second+brain.md` (title: "Second Brain & Productivity"). It is the canonical name for this unified domain.

---

## Recommended Subsection Order

The ordering follows a strict **dependency-first, domain-grouped** architecture:

```

Infrastructure → Core Engine → Visual Layer → Behavioral Wiring → Task Factory → Knowledge Library → Capture Bridge → Editing Tools

```

---

### Phase 1: Infrastructure & Core Engine

| #   | Subsection                       | Rationale                                                                                                                                         |
| --- | -------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1   | `** Dynamic Directory Structure` | **Absolute foundation.** All paths (`my/org-directory`, Denote silos, `agenda/`) must exist before any package references them.                   |
| 2   | `** Core Configuration`          | The `use-package org` block. Every downstream Org feature depends on `org-modules`, `org-directory`, and core `setq` variables being established. |

---

### Phase 2: Visual & Typography Layer

| #   | Subsection             | Rationale                                                                                                                                                                                    |
| --- | ---------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 3   | `** Better Font Faces` | Global typography (`bold`, `italic`) and conditional heading zoom. **Defines `ar/org-font-setup`.** Must precede Hooks (which consumes this function) and Org Modern (both touch font-lock). |
| 4   | `** Org Modern`        | Prettification engine. Consumes faces from Task States (`org-todo-keyword-faces`, `org-tag-faces`). Must come after Font Faces to avoid redisplay collision ordering issues.                 |
| 5   | `** Org Appear`        | Markup revealing. Interacts with Org Modern's `invisible` text properties. Must come after Org Modern.                                                                                       |

---

### Phase 3: Behavioral Wiring

| #   | Subsection | Rationale                                                                                                                                                                                                                                       |
| --- | ---------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 6   | `** Hooks` | Wires `org-mode-hook`, `org-agenda-mode-hook`, `org-capture-mode-hook`. **Consumes `ar/org-font-setup` (defined in Better Font Faces at position 3).** Must come AFTER the Visual & Typography Layer to satisfy the define-before-use contract. |

#### Why Hooks Moved After the Visual Layer

The `ar/org-font-setup` function is **defined** in the Better Font Faces subsection and **consumed** by the Hooks subsection via:

```emacs-lisp
(add-hook 'org-mode-hook #'ar/org-font-setup)
```

If Hooks were placed before Better Font Faces (as in the original ordering), the tangled `init.el` would reference `ar/org-font-setup` before its `defun` is evaluated. While this does not cause a runtime crash (because `add-hook` stores the symbol without verifying `fboundp`, and the hook only fires after `init.el` finishes loading), it violates the **define-before-use** principle and would produce byte-compiler warnings if `no-byte-compile: t` were ever removed.

**Corrected dependency:**

```
Better Font Faces ──── defines ar/org-font-setup
        │
        ▼
     Hooks ◄──── consumes ar/org-font-setup (now safely defined)
```

---

### Phase 4: Task Management (The Factory)

| #   | Subsection                      | Rationale                                                                                                         |
| --- | ------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
| 7   | `** Task States & Contexts`     | Defines TODO keywords, tags, priorities, refile targets. The agenda and super-agenda depend on these definitions. |
| 8   | `** Org Agenda`                 | Core agenda configuration (`org-agenda-files`, time grid, span). Depends on Task States for keyword filtering.    |
| 9   | `** Org Super Agenda`           | Dynamic grouping layer on top of `org-agenda`. Depends on Org Agenda being configured.                            |
| 10  | `** Org Agenda Custom Commands` | Custom dashboard views. Depends on both Org Agenda and Org Super Agenda.                                          |
| 11  | `** Org Habit`                  | Habit consistency graphs in `habits.org`. Uses `org-agenda` rendering. Depends on Org Agenda.                     |

---

GFf### Phase 5: Knowledge Management (The Library)

| #   | Subsection            | Rationale                                                                                                |
| --- | --------------------- | -------------------------------------------------------------------------------------------------------- |
| 12  | `** Denote Directory` | Silo path variables (`my/denote-directory`). Must precede the Denote package.                            |
| 13  | `** Denote`           | Core engine. File naming, keyword inference, `denote-rename-buffer-mode`. All extensions depend on this. |
| 14  | `** Denote Journal`   | Daily journal routing. Depends on Denote core.                                                           |
| 15  | `** Denote Org`       | Org-specific integrations (link types, capture). Depends on Denote + Org.                                |
| 16  | `** Denote Explore`   | Analytics (random walks, statistics, orphans). Depends on Denote core.                                   |
| 17  | `** Consult Denote`   | Vertico-powered search scoped to silos. Depends on Denote + Consult.                                     |
| 18  | `** Citar Denote`     | Bibliography → literature note pipeline. Depends on Denote + Citar.                                      |

---

### Phase 6: Capture & Workflow Bridge

| #   | Subsection         | Rationale                                                                                                                                                          |
| --- | ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| 19  | `** Org Capture`   | **The unified entry point.** Routes tasks to `todo.org` AND spawns Denote notes. Depends on BOTH Task States (Phase 4) and Denote (Phase 5). Must come after both. |
| 20  | `** Weekly Review` | Bridges tasks and notes. Queries GTD inbox counts AND Denote note creation stats. Depends on both domains being configured.                                        |

---

### Phase 7: Editing & Templates

| #   | Subsection                     | Rationale                                                                             |
| --- | ------------------------------ | ------------------------------------------------------------------------------------- |
| 21  | `** Org Structure Templates`   | Babel language loading, `org-tempo` templates. Independent of task/knowledge domains. |
| 22  | `** Transient Template System` | Template UI overlay. Depends on Org Structure Templates.                              |
| 23  | `** Org Src Buffer Naming`     | Source edit buffer naming. Independent utility.                                       |
| 24  | `** Org Eldoc`                 | Documentation in Org buffers. Independent utility.                                    |

---

## Dependency Graph (Visual Summary)

```
Dynamic Directory Structure
        │
        ▼
  Core Configuration
        │
        ▼
  Better Font Faces ──── defines ar/org-font-setup
        │                        │
        ▼                        │
   Org Modern                    │
        │                        │
        ▼                        │
   Org Appear                    │
        │                        │
        ▼                        ▼
     Hooks ◄──── consumes ar/org-font-setup
        │
        ├──────────────────────┐
        ▼                      ▼
  Task States & Contexts   Denote Directory
        │                      │
        ▼                      ▼
   Org Agenda              Denote Core
        │                      │
        ▼                      ├─► Denote Journal
  Org Super Agenda             ├─► Denote Org
        │                      ├─► Denote Explore
        ▼                      ├─► Consult Denote
  Agenda Custom Commands       └─► Citar Denote
        │                              │
        ▼                              │
    Org Habit                          │
        │                              │
        └──────────┬───────────────────┘
                   ▼
             Org Capture
                   │
                   ▼
             Weekly Review
                   │
                   ▼
        Org Structure Templates
                   │
                   ▼
        Transient Template System
                   │
                   ▼
        Org Src Buffer Naming
                   │
                   ▼
            Org Eldoc
```

---

## Key Ordering Principles Applied

1. **Path-before-package**: `Dynamic Directory Structure` and `Denote Directory` establish filesystem paths before any `use-package` block references them.
2. **Engine-before-extension**: `Denote` core precedes all `denote-*` extensions. `Org Agenda` precedes `Org Super Agenda`.
3. **Face-before-badge**: `Better Font Faces` (face definitions) precedes `Org Modern` (badge renderer that consumes those faces).
4. **Define-before-use**: `Better Font Faces` (defines `ar/org-font-setup`) precedes `Hooks` (consumes `ar/org-font-setup` via `org-mode-hook`).
5. **Both-domains-before-bridge**: `Org Capture` and `Weekly Review` depend on BOTH the Task Factory and the Knowledge Library, so they are placed after both.
6. **Independent-utilities-last**: Templates, src naming, and eldoc have no downstream dependents and are placed at the end.

---

## Change Log (vs. Previous Version)

| Position | Previous (Wrong)       | Corrected                   | Reason                                                            |
| -------- | ---------------------- | --------------------------- | ----------------------------------------------------------------- |
| 3        | Hooks                  | **Better Font Faces**       | Must define `ar/org-font-setup` before Hooks references it        |
| 4        | Better Font Faces      | **Org Modern**              | Consumes faces; independent of Hooks                              |
| 5        | Org Appear             | **Org Appear**              | Interacts with Org Modern                                         |
| 6        | Task States & Contexts | **Hooks**                   | Now safely references `ar/org-font-setup` (defined at position 3) |
| 7–11     | Org Agenda block       | **Task States → Org Habit** | Shifted down by one                                               |

I remain in the **READ-ONLY planning state**. Awaiting your explicit `GREEN LIGHT` to execute any structural rewrite.
