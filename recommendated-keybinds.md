Let me get the actual Doom module docs for vertico (consult), lsp, dap, and the editor:multiple-cursors/evil-multiedit module, and Spacemacs layers.Now let's get lsp-mode, dap-mode, and syntax checkers modules for Doom.Let me check the raw github source for the leader lsp keybindings and dap keybindings in Doom's default bindings and dap module.Let me fetch the actual +evil-bindings.el source from GitHub for precise keybindings on code/lsp/dap/checkers.Excellent, comprehensive data. Now let's get Spacemacs LSP layer keybindings and syntax-checking (flycheck) layer, plus multiple-cursors/iedit layer.Excellent, comprehensive Spacemacs LSP data. Now let's get Spacemacs syntax-checking layer (flycheck) and multiple-cursors/iedit layer keybindings.Let me get the full syntax-checking layer README for the complete keybinding table.Let me fetch the full raw README for syntax-checking layer to get complete keybinding table.Now let's find Spacemacs multiple-cursors layer (which includes evil-multiedit and iedit) keybindings.Here's a comprehensive comparison of Doom Emacs vs. Spacemacs keybindings for these five areas, based on official docs and module READMEs (current as of mid-2026).

## Consult / Fuzzy Search & Navigation

| Function                             | Doom Emacs (`:completion vertico`) | Spacemacs (`compleseus`/helm/ivy)                 |
| ------------------------------------ | ---------------------------------- | ------------------------------------------------- |
| Find file in project                 | `SPC SPC` / `SPC p f`              | `SPC p f`                                         |
| Find file from directory             | `SPC .` / `SPC f f`                | `SPC f f`                                         |
| Switch buffer (workspace)            | `SPC ,` / `SPC b b`                | `SPC b b`                                         |
| Switch buffer (all)                  | `SPC <` / `SPC b B`                | `SPC b B`                                         |
| Search buffer (consult-line)         | `SPC s s` / `SPC s b`              | `SPC s s`                                         |
| Search symbol at point in buffer     | `SPC s S`                          | `SPC s S`                                         |
| Search project (ripgrep)             | `SPC s p`                          | `SPC s p` / `SPC /`                               |
| Search another project               | `SPC s P`                          | `SPC s P`                                         |
| Search current directory             | `SPC s d`                          | `SPC s d`                                         |
| Search another directory             | `SPC s D`                          | `SPC s D`                                         |
| Jump to symbol in file (imenu-style) | `SPC s i`                          | `SPC s j` (imenu)                                 |
| Resume last completion session       | `SPC '`                            | `SPC r r` (helm) / resume varies                  |
| Enhanced M-x                         | `M-x` / `SPC :`                    | `SPC SPC` (M-x)                                   |
| Preview candidate                    | `C-SPC`                            | `M-.` / `C-SPC` (compleseus-consult-preview-keys) |
| Next/previous candidate              | `C-j` / `C-k`                      | `C-j` / `C-k`                                     |
| Embark actions menu                  | `C-;` / `SPC a`                    | N/A (Embark not core to Spacemacs)                |
| Export results to editable buffer    | `C-c C-e`                          | `C-c C-e` (wgrep)                                 |

Note: Spacemacs' native completion layers are `helm` and `ivy`; Consult itself is only used if you add the `compleseus` layer (vertico+consult), which is Spacemacs' modern alternative but preserves the same `SPC s`/`SPC b`/`SPC p` mnemonic bindings.

## LSP Mode

| Function                          | Doom Emacs                         | Spacemacs (`lsp` layer)                          |
| --------------------------------- | ---------------------------------- | ------------------------------------------------ |
| Find definition                   | `SPC c d` (also `gd`)              | `SPC m g d`                                      |
| Find definition (other window)    | —                                  | `SPC m g D`                                      |
| Find references                   | `SPC c D` (also `gD`)              | `SPC m g r`                                      |
| Find implementations              | —                                  | `SPC m g i`                                      |
| Find type definition              | —                                  | `SPC m g t`                                      |
| Rename symbol                     | `SPC c r`                          | `SPC m r r`                                      |
| Code actions                      | `SPC c a`                          | `SPC m a a`                                      |
| Fix / refactor / source actions   | —                                  | `SPC m a f` / `SPC m a r` / `SPC m a s`          |
| Documentation at point            | `SPC c k` / `K`                    | `SPC m h h`                                      |
| Format buffer                     | `SPC c =` (varies)                 | `SPC m = b`                                      |
| Format region                     | —                                  | `SPC m = r`                                      |
| Organize imports                  | —                                  | `SPC m = o`                                      |
| List diagnostics/errors (project) | `SPC c x`                          | `SPC m g e` (lsp-treemacs errors)                |
| Symbol search in project          | `SPC s p` (consult)                | `SPC m g s` (helm-lsp)                           |
| Imenu / buffer symbols            | `SPC c f` (varies by config)       | `SPC m g M` (lsp-ui-imenu)                       |
| Restart / shutdown workspace      | `lsp-workspace-restart` (M-x)      | `SPC m b r` / `SPC m b s`                        |
| Describe session                  | —                                  | `SPC m b d`                                      |
| Toggle doc/sideline overlays      | —                                  | `SPC m T l d` / `SPC m T l s`                    |
| Peek navigation variants          | `lsp-ui-peek-*` (bound via config) | `SPC m G` prefix (peek equivalents of `SPC m g`) |
| Add/remove workspace folder       | —                                  | `SPC m F a` / `SPC m F r`                        |

Doom nests all raw lsp-mode commands under `SPC c l …` by default, but its "code" prefix (`SPC c`) also exposes higher-level, backend-agnostic bindings (`d`, `D`, `r`, `a`, `k`) that work whether you use lsp-mode or eglot. Spacemacs organizes everything under the major-mode leader `SPC m` with clearly scoped sub-prefixes (`a`=actions, `g`=goto, `G`=peek, `r`=refactor, `=`=format, `h`=help, `b`=backend, `F`=folders).

## DAP Mode (Debugger)

| Function                             | Doom Emacs                                  | Spacemacs (`dap` layer)                 |
| ------------------------------------ | ------------------------------------------- | --------------------------------------- |
| Global debug prefix                  | — (mostly `M-x dap-*`, some under `SPC d`)  | `SPC d`                                 |
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

Doom's dap-mode integration is much thinner and largely relies on `M-x dap-*` commands (a few bound under `SPC d` if you configure them), while Spacemacs ships a fully mapped, mnemonic `SPC d` prefix out of the box.

## iedit / evil-multiedit (Multiple Cursors / Batch Editing)

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

Doom's default "multiedit" experience is `evil-multiedit` (M-d/M-D/R), which is iedit under the hood, plus an independent `evil-mc` implementation bound to the `gz` prefix. Spacemacs binds raw `iedit-mode` directly to `SPC s e`/`SPC s E`, and offers a separate, fully mnemonic `multiple-cursors` layer (`SPC s m …`) or the `evil-mc` package (`g r …`) as alternate backends.

## Errors & Syntax Checking (Flycheck/Flymake)

| Function                           | Doom Emacs                                  | Spacemacs (`syntax-checking` layer)                                  |
| ---------------------------------- | ------------------------------------------- | -------------------------------------------------------------------- |
| List all errors (buffer)           | `SPC c x` (or custom `SPC e`)               | `SPC e l`                                                            |
| List errors + focus buffer         | —                                           | `SPC e L`                                                            |
| Check now                          | —                                           | `SPC e b`                                                            |
| Clear errors                       | —                                           | `SPC e c`                                                            |
| Next/previous error                | `]e` / `[e`                                 | `SPC e n` / `SPC e p` (via next-error)                               |
| Explain error at point             | —                                           | `SPC e x`                                                            |
| Select/verify checker              | —                                           | `SPC e s` (select) / `SPC e v` (verify) / `SPC e S` (set executable) |
| Describe checker                   | —                                           | `SPC e h`                                                            |
| Disable checker in buffer          | —                                           | `SPC e d`                                                            |
| Toggle flycheck                    | —                                           | `SPC t s`                                                            |
| LSP-wide diagnostics list          | `SPC c x` / `lsp-ui-flycheck-list` (custom) | `SPC m g e` (lsp-treemacs) / `SPC m G E` (lsp-ui)                    |
| Error-list buffer: next/prev       | `C-j` / `C-k`                               | `j` / `k` (or `J`/`K` without preview)                               |
| Error-list buffer: go to error     | `RET`                                       | `RET`                                                                |
| Error-list buffer: filter/unfilter | —                                           | `f` / `F`                                                            |

Doom doesn't ship a dedicated top-level errors prefix by default — many guides (e.g., the Doom community wiki) recommend manually binding `SPC e`/`SPC E` to `flycheck-list-errors`/`lsp-ui-flycheck-list`, and next/previous error is `]e`/`[e` via evil's bracket-jump convention. Spacemacs' `syntax-checking` layer ships a full, ready-made `SPC e` prefix.

---

**Sources:** Doom Emacs official docs (`docs.doomemacs.org`, `:completion vertico`, `:tools lsp`, `:tools lookup` modules), Doom Discourse threads, evil-multiedit GitHub README, and community cheat sheets; Spacemacs official layer READMEs (`lsp`, `dap`, `syntax-checking`, `multiple-cursors`, `compleseus`) via `spacemacs.org`/GitHub `syl20bnr/spacemacs`. Both frameworks let you rebind these prefixes, so treat this as the **default** configuration — your own dotfile may differ.
