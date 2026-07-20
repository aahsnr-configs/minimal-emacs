# Emacs Keybinding Reference

This document consolidates a multi-pass planning process into one final, collision-free reference. It has two independent parts:

- **Part I — Org Mode & Second Brain (Denote) Keybindings**: the finished plan for a Doom-Emacs-flavored `general.el` leader configuration built around Org Agenda and the Denote note-taking ecosystem.
- **Part II — Doom Emacs vs. Spacemacs Comparison**: a reference table for developers migrating between the two distributions, covering fuzzy search, LSP, debugging, multiple cursors, and syntax checking.

Earlier drafts of Part I worked through several rounds of revision and introduced a few genuine key collisions along the way (most notably around `SPC n r`, `SPC n s`, and `SPC o p`). Those are resolved below in favor of a single, non-colliding scheme.

---

## Part I — Org Mode & Second Brain Keybindings

### Design principles

1. **Denote replaces org-roam.** Every place Doom Emacs would normally bind an `org-roam` command, the equivalent Denote or `consult-denote` command is bound instead, under the same mnemonic key where possible (e.g., Doom's `SPC n r` roam-buffer-toggle becomes `denote-backlinks`).
2. **Sub-prefixes prevent key exhaustion.** The Denote ecosystem spans roughly 25 packages (core, search, journaling, sequence notes, silos, analytics, management, bibliography). Rather than flattening all of them into `SPC n <letter>`, related commands are grouped into two-key sub-prefixes (`SPC n q` for sequence notes, `SPC n e` for explore/analytics, `SPC n m` for management, `SPC n B` for bibliography, `SPC n S` for silos).
3. **"Factory" vs. "Library" separation.** Task management (Org Agenda — the "Factory") stays decoupled from knowledge management (Denote — the "Library"). Agenda-buffer local-leader bindings never overlap with Denote linking commands.
4. **`SPC o` stays "open."** Doom's `SPC o` prefix is a general-purpose "open a dashboard, sidebar, terminal, or REPL" namespace — it isn't Org-specific. Org Agenda is nested under `SPC o a` as its own sub-prefix rather than overwriting existing bindings for Dired, Treemacs, terminals, REPLs, and the debugger. This is what actually avoids collisions; a flat, single-letter `SPC o` scheme runs out of room the moment you add five or six new agenda views.
5. **Doom parity where it exists.** Anywhere Doom Emacs already ships a binding for stock `org-mode`, that binding is preserved and merged alongside the user's existing custom prefixes (Babel, Insert, Text, Link) rather than replacing them.
6. **`general-define-key` placement.** All `general-define-key`, `ar/global-leader`, and `ar/local-leader` definitions live entirely **outside** any `(use-package ...)` form, but **inside** the same `#+begin_src emacs-lisp` block as the packages they configure. This preserves correct load order without breaking up the literate-config grouping.

---

### 1. Global Leader (`SPC`) Routes

#### `SPC n` — Notes / Denote Ecosystem Prefix

**Core operations**

| Binding   | Command                                 | Description                  |
| --------- | --------------------------------------- | ---------------------------- |
| `SPC n n` | `denote`                                | New note in the current silo |
| `SPC n N` | `denote-subdirectory`                   | New note in a subdirectory   |
| `SPC n r` | `denote-rename-file`                    | Rename file & front matter   |
| `SPC n R` | `denote-rename-file-using-front-matter` | Rename from front matter     |
| `SPC n k` | `denote-rename-file-keywords`           | Edit keywords                |
| `SPC n d` | `denote-dired`                          | Denote-scoped Dired          |

**Search & discovery**

| Binding   | Command                | Description                 |
| --------- | ---------------------- | --------------------------- |
| `SPC n f` | `consult-denote-find`  | Vertico-powered find note   |
| `SPC n s` | `consult-denote-grep`  | Ripgrep search across notes |
| `SPC n x` | `denote-regexp-search` | Regexp search               |

**Linking**

| Binding   | Command                | Description              |
| --------- | ---------------------- | ------------------------ |
| `SPC n l` | `denote-link`          | Insert link              |
| `SPC n b` | `denote-backlinks`     | Show backlinks buffer    |
| `SPC n T` | `org-transclusion-add` | Transclude a Denote note |

**Journaling**

| Binding   | Command                                | Description       |
| --------- | -------------------------------------- | ----------------- |
| `SPC n j` | `denote-journal-new-or-existing-entry` | Today's entry     |
| `SPC n J` | `denote-journal-new-entry`             | Force a new entry |

**Silo management — `SPC n S`**

| Binding     | Command                                |
| ----------- | -------------------------------------- |
| `SPC n S s` | `denote-silo-select-silo-then-command` |
| `SPC n S o` | `denote-silo-open-or-create`           |
| `SPC n S n` | `denote-silo-create-note`              |
| `SPC n S d` | `denote-silo-dired`                    |

**Sequence notes / Folgezettel — `SPC n q`** _(`q` for se`q`uence, keeping `s` free for search)_

| Binding     | Command                                     |
| ----------- | ------------------------------------------- |
| `SPC n q n` | `denote-sequence` (new sequence note)       |
| `SPC n q p` | `denote-sequence-new-parent`                |
| `SPC n q c` | `denote-sequence-new-child-of-current`      |
| `SPC n q b` | `denote-sequence-new-sibling-of-current`    |
| `SPC n q f` | `denote-sequence-find` (navigate hierarchy) |
| `SPC n q l` | `denote-sequence-link`                      |
| `SPC n q r` | `denote-sequence-reparent`                  |
| `SPC n q v` | `denote-sequence-view-hierarchy`            |

**Explore & analytics — `SPC n e`**

| Binding     | Command                           |
| ----------- | --------------------------------- |
| `SPC n e s` | `denote-explore-statistics`       |
| `SPC n e o` | `denote-explore-orphans`          |
| `SPC n e r` | `denote-explore-random-note`      |
| `SPC n e n` | `denote-explore-network-keywords` |
| `SPC n e w` | `denote-wordcloud`                |

**Management & refactoring — `SPC n m`**

| Binding     | Command                  | Description                      |
| ----------- | ------------------------ | -------------------------------- |
| `SPC n m m` | `denote-menu`            | Tabulated list interface         |
| `SPC n m e` | `denote-merge`           | Merge notes                      |
| `SPC n m z` | `denote-solo`            | Distraction-free focus mode      |
| `SPC n m r` | `denote-review`          | Spaced-repetition review         |
| `SPC n m x` | `denote-sections-create` | Extract section into a new note  |
| `SPC n m l` | `denote-sections-link`   | Link back to source section      |
| `SPC n m f` | `denote-refs-forward`    | Forward references               |
| `SPC n m b` | `denote-refs-backward`   | Backward references / dependents |

**Bibliography & publishing — `SPC n B`**

| Binding     | Command                        |
| ----------- | ------------------------------ |
| `SPC n B o` | `citar-denote-open-note`       |
| `SPC n B c` | `citar-denote-create-note`     |
| `SPC n B i` | `citar-denote-insert-citation` |
| `SPC n B p` | `denote-publish`               |

**Top-level misc**

| Binding   | Command                                                 |
| --------- | ------------------------------------------------------- |
| `SPC n A` | `denote-agenda` (chronological view)                    |
| `SPC n P` | `denote-project-notes`                                  |
| `SPC n w` | `ar/weekly-review` (custom Denote-backed weekly review) |

#### `SPC o` — Open (Dashboards, Sidebars, External Apps, and Org Agenda)

The existing `SPC o` "open" bindings are preserved untouched, and Org Agenda is added as its own two-key sub-prefix (`SPC o a`) rather than claiming single letters that would collide with Treemacs, the terminal, or the debugger.

**Existing / preserved `SPC o` bindings**

| Binding               | Command                                         | Description                                        |
| --------------------- | ----------------------------------------------- | -------------------------------------------------- |
| `SPC o -`             | `dired-jump`                                    | Dired at current file                              |
| `SPC o /`             | `dirvish-dwim`                                  | Open Dirvish                                       |
| `SPC o b`             | `+default/browse-url`                           | Open OS default browser                            |
| `SPC o d`             | `dap-debug` / `dape`                            | Start a debugger                                   |
| `SPC o e` / `SPC o E` | `+eval/open-repl` / fullscreen shell            | Popup vs. fullscreen shell                         |
| `SPC o f` / `SPC o F` | `make-frame` / `select-frame-by-name`           | New frame / switch frame                           |
| `SPC o l`             | `+llm/open`                                     | Local LLM / AI chat dashboard                      |
| `SPC o p` / `SPC o P` | `treemacs-select-window` / `treemacs-find-file` | Project sidebar / locate file in sidebar           |
| `SPC o r` / `SPC o R` | `+eval/open-repl` / same-window variant         | REPL popup / REPL in current window                |
| `SPC o s`             | `universal-sidecar-toggle`                      | Toggle contextual sidecar (backlinks/bibliography) |
| `SPC o t` / `SPC o T` | `terminal popup` / `terminal here`              | Persistent terminal drawer / inline terminal       |

**New — Org Agenda sub-prefix `SPC o a`**

| Binding     | Command              | Description                                      |
| ----------- | -------------------- | ------------------------------------------------ |
| `SPC o a a` | `org-agenda`         | Default dispatcher / daily-weekly dashboard      |
| `SPC o a d` | Custom command       | Daily Dashboard                                  |
| `SPC o a w` | Custom command       | Weekly Review (Inbox + Waiting + Stuck)          |
| `SPC o a e` | Custom command       | Effort-Based (Quick Wins + Energy + Deep Work)   |
| `SPC o a A` | Custom command       | Accomplishments (`:log closed` + `:log clocked`) |
| `SPC o a p` | Custom command       | Project Status                                   |
| `SPC o a m` | `org-tags-view`      | Global tags search                               |
| `SPC o a t` | `org-todo-list`      | Global TODO list                                 |
| `SPC o a v` | `consult-org-agenda` | Fuzzy heading jump across all agenda files       |

#### `SPC X` — Global Capture

| Binding | Command                                                      |
| ------- | ------------------------------------------------------------ |
| `SPC X` | `ar/capture-dispatch` (custom smart hierarchical dispatcher) |

---

### 2. Local Leader (`SPC m`) Routes

#### Org buffer local leader

Synthesized from Doom's `+org-init-keybinds-h`, merged with user-preserved prefixes.

**Single-key Doom parity**

| Binding   | Command                                           |
| --------- | ------------------------------------------------- |
| `SPC m #` | `org-update-statistics-cookies`                   |
| `SPC m '` | `org-edit-special` (edit Babel block)             |
| `SPC m *` | `org-ctrl-c-star`                                 |
| `SPC m -` | `org-ctrl-c-minus`                                |
| `SPC m ,` | `org-switchb`                                     |
| `SPC m .` | `consult-org-heading` (fuzzy jump in buffer)      |
| `SPC m /` | `consult-org-agenda`                              |
| `SPC m @` | `org-cite-insert`                                 |
| `SPC m A` | `org-archive-subtree-default`                     |
| `SPC m e` | `org-export-dispatch`                             |
| `SPC m f` | `org-footnote-action`                             |
| `SPC m h` | `org-toggle-heading`                              |
| `SPC m i` | `org-toggle-item`                                 |
| `SPC m I` | `org-id-get-create`                               |
| `SPC m k` | `ar/org-babel-remove-result-one-or-many` (see §3) |
| `SPC m n` | `org-store-link`                                  |
| `SPC m o` | `org-set-property`                                |
| `SPC m q` | `org-set-tags-command`                            |
| `SPC m t` | `org-todo`                                        |
| `SPC m x` | `org-toggle-checkbox`                             |

**Prefix routes**

| Prefix                                                                     | Sub-keys                                                     |
| -------------------------------------------------------------------------- | ------------------------------------------------------------ |
| `SPC m a` (Attachments)                                                    | `a`ttach, `d`elete, `o`pen, `r`eveal                         |
| `SPC m b` (Babel) _user-preserved_                                         | `t`angle, `e`xecute, `b`uffer, `c`heck                       |
| `SPC m c` (Clock)                                                          | `c`ancel, `g`oto, `i`n, `o`ut, `r`esolve                     |
| `SPC m d` (Date/Deadline)                                                  | `d`eadline, `s`chedule, `t`imestamp                          |
| `SPC m g` (Goto)                                                           | `g` heading (`consult-org-heading`), `c`lock, `i`d, `r`efile |
| `SPC m l` (Link) _user-preserved_                                          | `l` insert, `s`tore, `i`d, `d`elete                          |
| `SPC m p` (Priority)                                                       | `d`own, `u`p, `p`riority                                     |
| `SPC m r` (Refile)                                                         | `r`efile, `l`ast location                                    |
| `SPC m s` (Subtree)                                                        | `n`arrow, `S`ort, `h` promote, `l` demote, `j` down, `k` up  |
| `SPC m T` (Tables) _remapped from Doom's `b` to avoid the Babel collision_ | `a`lign, `c`reate, `e`dit, `r`ecalc                          |

#### Agenda buffer local leader

Mapped directly from Doom's agenda local-leader for seamless task management inside `org-super-agenda` views.

| Binding              | Command / Sub-keys                      |
| -------------------- | --------------------------------------- |
| `SPC m d` (Date)     | `d`eadline, `s`chedule                  |
| `SPC m c` (Clock)    | `c`ancel, `g`oto, `i`n, `o`ut, `r`eport |
| `SPC m p` (Priority) | `d`own, `u`p, `p`riority                |
| `SPC m q`            | `org-agenda-set-tags`                   |
| `SPC m r`            | `org-agenda-refile`                     |
| `SPC m t`            | `org-agenda-todo`                       |
| `SPC m f`            | `org-agenda-filter-by-tag`              |

---

### 3. Evil / Modal Integration

#### `evil-org` (Org buffers)

Hooked into `org-mode`, configured via:

```elisp
(evil-org-set-key-theme '(navigation insert textobjects additional calendar todo heading))
```

- **Navigation:** `gh`/`gl` (heading boundaries), `]]`/`[[` (jump between headings), `M-h`/`M-l` (promote/demote), `M-j`/`M-k` (move subtree up/down).
- **Text objects:** `ih`/`ah` (inner/a heading), `ir`/`ar` (inner/a subtree).
- **Insert:** `M-RET` (insert heading below), `M-S-RET` (insert TODO heading below).
- **Tables:** `gj`/`gk` (move rows), `M-l`/`M-h` (move columns).

#### `evil-org-agenda` (Agenda buffers)

Hooked into `org-agenda-mode`, configured via `(evil-org-agenda-set-keys)`:

- **Navigation:** `J`/`K` (move items up/down), `gd` (goto date), `C-f`/`C-b` (scroll), `gk`/`gj` (previous/next item), `C-SPC` (show and scroll up).
- **Actions:** `gr` (refresh), `RET` (open), `q` (quit).

#### Custom DWIM `RET`

Doom binds `RET` in normal/motion states to `+org/dwim-at-point`, which contextually toggles TODO states, checkboxes, follows links, or opens folds. Parity is implemented with a small wrapper bound into `evil-org-mode-map`:

```elisp
(defun ar/org-return-dwim (&optional arg)
  "Doom Emacs parity: context-aware RET in Org normal/motion states.
- On a TODO keyword: cycle TODO state (e.g. TODO -> DONE).
- On a checkbox: toggle checkbox.
- On a link: follow link.
- On a folded heading: open fold.
- Otherwise: delegate to `org-ctrl-c-ctrl-c'."
  (interactive "P")
  (cond
   ((and (org-at-heading-p) (org-get-todo-state))
    (org-todo arg))
   ((org-at-item-checkbox-p)
    (org-toggle-checkbox arg))
   ((org-in-regexp org-link-any-re 1)
    (org-open-at-point arg))
   ((and (org-at-heading-p) (org-invisible-p))
    (org-fold-show-entry))
   (t (org-ctrl-c-ctrl-c arg))))
```

#### Custom Babel result cleanup

Doom binds `SPC m k` to `org-babel-remove-result`. This wrapper adds support for a prefix argument to clear every result in the buffer at once:

```elisp
(defun ar/org-babel-remove-result-one-or-many (&optional arg)
  "Remove babel results. With ARG, remove all results in buffer."
  (interactive "P")
  (org-babel-remove-result-one-or-many arg))
```

#### Unimpaired-style motions

| Binding     | Command                                                              |
| ----------- | -------------------------------------------------------------------- |
| `]h` / `[h` | `org-forward-heading-same-level` / `org-backward-heading-same-level` |
| `]l` / `[l` | `org-next-link` / `org-previous-link`                                |
| `]c` / `[c` | `org-babel-next-src-block` / `org-babel-previous-src-block`          |

---

### 4. Implementation notes

- **`use-package` vs. leader definitions:** put the `use-package` declarations for `evil-org` and `evil-org-agenda` at the top of the source block; put the `ar/global-leader`, `ar/local-leader`, and any `general-define-key` calls for Evil overrides immediately after, still inside the same `#+begin_src emacs-lisp` block but outside every `use-package` form.
- **Load order:** because the leader macros run after the `use-package` forms in the same block, all referenced commands are already defined/autoloaded by the time the keybindings are evaluated.
- **Factory/Library boundary:** the agenda-buffer local leader (`SPC m` while inside `org-agenda-mode`) intentionally has zero overlap with the Denote linking commands under `SPC n`, keeping task management and knowledge management on separate rails.

---

## Part II — Doom Emacs vs. Spacemacs Comparison

A reference for anyone moving between the two distributions. Both frameworks let you rebind these prefixes, so treat this as the **default** configuration — a given dotfile may differ.

### Consult / Fuzzy Search & Navigation

| Function                             | Doom Emacs (`:completion vertico`) | Spacemacs (`compleseus`/helm/ivy) |
| ------------------------------------ | ---------------------------------- | --------------------------------- |
| Find file in project                 | `SPC SPC` / `SPC p f`              | `SPC p f`                         |
| Find file from directory             | `SPC .` / `SPC f f`                | `SPC f f`                         |
| Switch buffer (workspace)            | `SPC ,` / `SPC b b`                | `SPC b b`                         |
| Switch buffer (all)                  | `SPC <` / `SPC b B`                | `SPC b B`                         |
| Search buffer (consult-line)         | `SPC s s` / `SPC s b`              | `SPC s s`                         |
| Search symbol at point in buffer     | `SPC s S`                          | `SPC s S`                         |
| Search project (ripgrep)             | `SPC s p`                          | `SPC s p` / `SPC /`               |
| Search another project               | `SPC s P`                          | `SPC s P`                         |
| Search current directory             | `SPC s d`                          | `SPC s d`                         |
| Search another directory             | `SPC s D`                          | `SPC s D`                         |
| Jump to symbol in file (imenu-style) | `SPC s i`                          | `SPC s j` (imenu)                 |
| Resume last completion session       | `SPC '`                            | `SPC r r` (helm) / varies         |
| Enhanced M-x                         | `M-x` / `SPC :`                    | `SPC SPC` (M-x)                   |
| Preview candidate                    | `C-SPC`                            | `M-.` / `C-SPC`                   |
| Next/previous candidate              | `C-j` / `C-k`                      | `C-j` / `C-k`                     |
| Embark actions menu                  | `C-;` / `SPC a`                    | not core to Spacemacs             |
| Export results to editable buffer    | `C-c C-e`                          | `C-c C-e` (wgrep)                 |

Spacemacs' native completion layers are `helm` and `ivy`; Consult is available only if you add the `compleseus` layer (vertico + consult), which preserves the same `SPC s`/`SPC b`/`SPC p` mnemonics.

### LSP Mode

| Function                          | Doom Emacs                    | Spacemacs (`lsp` layer)                 |
| --------------------------------- | ----------------------------- | --------------------------------------- |
| Find definition                   | `SPC c d` (also `gd`)         | `SPC m g d`                             |
| Find definition (other window)    | —                             | `SPC m g D`                             |
| Find references                   | `SPC c D` (also `gD`)         | `SPC m g r`                             |
| Find implementations              | —                             | `SPC m g i`                             |
| Find type definition              | —                             | `SPC m g t`                             |
| Rename symbol                     | `SPC c r`                     | `SPC m r r`                             |
| Code actions                      | `SPC c a`                     | `SPC m a a`                             |
| Fix / refactor / source actions   | —                             | `SPC m a f` / `SPC m a r` / `SPC m a s` |
| Documentation at point            | `SPC c k` / `K`               | `SPC m h h`                             |
| Format buffer                     | `SPC c =` (varies)            | `SPC m = b`                             |
| Format region                     | —                             | `SPC m = r`                             |
| Organize imports                  | —                             | `SPC m = o`                             |
| List diagnostics/errors (project) | `SPC c x`                     | `SPC m g e` (lsp-treemacs errors)       |
| Symbol search in project          | `SPC s p` (consult)           | `SPC m g s` (helm-lsp)                  |
| Imenu / buffer symbols            | `SPC c f` (varies)            | `SPC m g M` (lsp-ui-imenu)              |
| Restart / shutdown workspace      | `lsp-workspace-restart` (M-x) | `SPC m b r` / `SPC m b s`               |
| Describe session                  | —                             | `SPC m b d`                             |
| Toggle doc/sideline overlays      | —                             | `SPC m T l d` / `SPC m T l s`           |
| Peek navigation variants          | `lsp-ui-peek-*` (via config)  | `SPC m G` prefix                        |
| Add/remove workspace folder       | —                             | `SPC m F a` / `SPC m F r`               |

Doom nests all raw `lsp-mode` commands under `SPC c l …`, but its "code" prefix (`SPC c`) also exposes higher-level, backend-agnostic bindings (`d`, `D`, `r`, `a`, `k`) that work with either `lsp-mode` or `eglot`. Spacemacs organizes everything under the major-mode leader `SPC m` with scoped sub-prefixes (`a`=actions, `g`=goto, `G`=peek, `r`=refactor, `=`=format, `h`=help, `b`=backend, `F`=folders).

### DAP Mode (Debugger)

| Function                             | Doom Emacs                                  | Spacemacs (`dap` layer)                 |
| ------------------------------------ | ------------------------------------------- | --------------------------------------- |
| Global debug prefix                  | mostly `M-x dap-*`, some under `SPC d`      | `SPC d`                                 |
| Start debugging                      | `SPC d d` / `M-x dap-debug`                 | `SPC d d d`                             |
| Debug last/recent config             | —                                           | `SPC d d l` / `SPC d d r`               |
| Edit debug template                  | `M-x dap-debug-edit-template`               | `SPC d d e`                             |
| Toggle breakpoint                    | `M-x dap-breakpoint-toggle`                 | `SPC d b b`                             |
| Add/delete breakpoint                | `M-x dap-breakpoint-add`                    | `SPC d b a` / `SPC d b d`               |
| Clear all breakpoints                | —                                           | `SPC d b D`                             |
| Conditional/log/hit-count breakpoint | —                                           | `SPC d b c` / `SPC d b l` / `SPC d b h` |
| List breakpoints                     | —                                           | `SPC d w b`                             |
| Continue                             | `M-x dap-continue`                          | `SPC d c`                               |
| Step in/out/next                     | —                                           | `SPC d i` / `SPC d o` / `SPC d s`       |
| Restart frame                        | —                                           | `SPC d r`                               |
| Switch frame/session/thread          | —                                           | `SPC d S f` / `SPC d S s` / `SPC d S t` |
| Debug REPL                           | `M-x dap-ui-repl`                           | `SPC d '`                               |
| Eval / eval region / eval at point   | `M-x dap-tooltip-at-point`                  | `SPC d e e` / `SPC d e r` / `SPC d e t` |
| Inspect value                        | `M-x dap-tooltip-at-point`                  | `SPC d v` / `SPC d I *`                 |
| List local variables                 | —                                           | `SPC d w l`                             |
| Add watch expression                 | `SPC m d` (`dap-ui-expressions-add-prompt`) | —                                       |
| Abandon session(s)                   | —                                           | `SPC d a` / `SPC d A`                   |
| Transient/hydra state                | —                                           | `SPC m d .`                             |

Doom's `dap-mode` integration is much thinner and largely relies on `M-x dap-*` commands (a few bound under `SPC d` if configured), while Spacemacs ships a fully mapped, mnemonic `SPC d` prefix out of the box.

### iedit / evil-multiedit (Multiple Cursors / Batch Editing)

| Function                                       | Doom Emacs (`evil-multiedit`) | Spacemacs (iedit core + `multiple-cursors` layer) |
| ---------------------------------------------- | ----------------------------- | ------------------------------------------------- |
| Enter multi-edit on symbol/selection           | `M-d` (normal or visual)      | `SPC s e` (iedit-mode, all matches)               |
| Match one-by-one / next occurrence             | `M-d` repeated                | `SPC s E` + `C-n`                                 |
| Match previous                                 | `M-D`                         | —                                                 |
| Select all occurrences (from visual selection) | `R` (in visual mode)          | `SPC s e` after visual selection                  |
| Next/previous match while editing              | `C-n` / `C-p`                 | `C-n` / `C-p`                                     |
| Toggle/restrict a match in region              | `RET`                         | `SPC` (narrow scope inside iedit)                 |
| Exit / abort                                   | `ESC` / `C-M-d` (restore)     | `C-c C-c` (accept) / `C-c C-k` (abort)            |
| evil-mc: mark all cursors like this            | `gzm`                         | `g r m` (evil-mc layer)                           |
| evil-mc: cursor at point                       | `gzc`                         | `g r h`                                           |
| evil-mc: next/prev cursor match                | `gzn` / `gzp`                 | `g r n` / `g r p`                                 |
| evil-mc: undo all cursors                      | `gzu`                         | `g r q`                                           |
| evil-mc: toggle pause                          | `gzz` / `gzt`                 | `g r s` / `g r r`                                 |
| mc.el: mark all dwim                           | —                             | `SPC s m a`                                       |
| mc.el: mark all like this                      | —                             | `SPC s m b`                                       |
| mc.el: edit lines                              | —                             | `SPC s m r`                                       |
| mc.el: mark more like this                     | —                             | `SPC s m m`                                       |

Doom's default multi-edit experience is `evil-multiedit` (`M-d`/`M-D`/`R`, which is `iedit` under the hood), plus an independent `evil-mc` implementation on the `gz` prefix. Spacemacs binds raw `iedit-mode` directly to `SPC s e`/`SPC s E`, and offers a separate, fully mnemonic `multiple-cursors` layer (`SPC s m …`) or the `evil-mc` package (`g r …`) as alternate backends.

### Errors & Syntax Checking (Flycheck/Flymake)

| Function                           | Doom Emacs                                | Spacemacs (`syntax-checking` layer)               |
| ---------------------------------- | ----------------------------------------- | ------------------------------------------------- |
| List all errors (buffer)           | `SPC c x` (or custom `SPC e`)             | `SPC e l`                                         |
| List errors + focus buffer         | —                                         | `SPC e L`                                         |
| Check now                          | —                                         | `SPC e b`                                         |
| Clear errors                       | —                                         | `SPC e c`                                         |
| Next/previous error                | `]e` / `[e`                               | `SPC e n` / `SPC e p`                             |
| Explain error at point             | —                                         | `SPC e x`                                         |
| Select/verify checker              | —                                         | `SPC e s` / `SPC e v` / `SPC e S`                 |
| Describe checker                   | —                                         | `SPC e h`                                         |
| Disable checker in buffer          | —                                         | `SPC e d`                                         |
| Toggle flycheck                    | —                                         | `SPC t s`                                         |
| LSP-wide diagnostics list          | `SPC c x` / custom `lsp-ui-flycheck-list` | `SPC m g e` (lsp-treemacs) / `SPC m G E` (lsp-ui) |
| Error-list buffer: next/prev       | `C-j` / `C-k`                             | `j` / `k` (or `J`/`K` without preview)            |
| Error-list buffer: go to error     | `RET`                                     | `RET`                                             |
| Error-list buffer: filter/unfilter | —                                         | `f` / `F`                                         |

Doom doesn't ship a dedicated top-level errors prefix by default — community guides commonly recommend binding `SPC e`/`SPC E` to `flycheck-list-errors`/`lsp-ui-flycheck-list`, with next/previous error on `]e`/`[e` via evil's bracket-jump convention. Spacemacs' `syntax-checking` layer ships a full `SPC e` prefix out of the box.

---

**Sources:** Doom Emacs official documentation (`docs.doomemacs.org`), the `evil-org-mode` and `evil-org-mode`/`evil-org-agenda` repositories, community cheat sheets, and Spacemacs' official layer READMEs (`lsp`, `dap`, `syntax-checking`, `multiple-cursors`, `compleseus`) via `spacemacs.org`/`syl20bnr/spacemacs`. Both frameworks let you rebind every prefix shown here, so treat this as the default configuration rather than a fixed standard.
