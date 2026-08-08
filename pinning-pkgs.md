## Straight.el-Managed Packages — Priority Channel & Commit (live-verified 2026-08-08)

### GNU ELPA — 1st priority (core + GNU packages)

| Package                                                                                        | Channel         | Version (channel pin identifier)               |
| ---------------------------------------------------------------------------------------------- | --------------- | ---------------------------------------------- |
| `ace-window` / `avy` / `bind-key` / `bufferlo` / `cape` / `colorful-mode`                      | GNU ELPA        | 0.10.0 / 0.5.0 / 2.4.1 / 1.2 / 2.8 / 1.2.5     |
| `consult` / `consult-denote` / `corfu` / `dape` / `denote` / `denote-journal`                  | GNU ELPA        | 3.7 / 0.5.1 / 2.12 / 0.27.1 / 4.2.3 / 0.3.0    |
| `denote-menu` / `denote-org` / `denote-review` / `denote-sequence` / `denote-silo` / `diff-hl` | GNU ELPA        | 1.4.0 / 0.3.0 / 1.0.7 / 0.3.3 / 0.3.2 / 1.10.0 |
| `eldoc`* / `embark` / `embark-consult` / `expreg` / `gcmh` / `indent-bars`                     | GNU ELPA        | 1.16.0 / 1.2 / 1.2 / 1.4.1 / 0.2.1 / 1.0.0     |
| `jinx` / `marginalia` / `orderless` / `org`* / `org-modern` / `org-transclusion`               | GNU ELPA        | 2.9 / 2.11 / 1.7 / 9.8.8 / 1.15 / 1.4.0        |
| `popper` / `posframe` / `project`* / `transient` / `use-package`* / `vertico`                  | GNU ELPA        | 0.4.8 / 1.5.2 / 0.12.0 / 0.13.7 / 2.4.6 / 2.12 |
| `xref`* / `yasnippet` / `markdown-ts-mode`*                                                    | GNU ELPA (core) | 1.7.0 / 0.14.3 / built-in Emacs 31             |

\* `:straight (:type built-in)` commented out in `config.org` → treated as straight-managed per your rule. _Footnote G:_ GNU ELPA publishes signed release tarballs (versions), not SHAs; straight.el clones these from `emacs-straight/gnu-elpa-mirror`, and the exact mirror SHA is materialized in the lockfile by the pinning workflow below.

### MELPA Stable — 1st priority (release-cadence pins)

| Package                                                  | Channel      | Version @ commit                                                                           |
| -------------------------------------------------------- | ------------ | ------------------------------------------------------------------------------------------ |
| `lsp-mode`                                               | MELPA Stable | 10.0.0 @ `913a6c`                                                                          |
| `evil`                                                   | MELPA Stable | 1.14.2 @ `162a94`                                                                          |
| `magit`                                                  | MELPA Stable | 4.7.0 @ `67f203`                                                                           |
| `flycheck`                                               | MELPA Stable | 38.3 @ `4414c1`                                                                            |
| `treemacs`                                               | MELPA Stable | 3.2 @ `55079b`                                                                             |
| `doom-themes` / `doom-modeline`                          | MELPA Stable | 2.3.0 @ `d79a41` / 4.2.1 @ `2f6112`                                                        |
| `forge` / `helpful`                                      | MELPA Stable | 0.6.8 @ `29f45d` / 0.21 @ `ced07f`                                                         |
| `anzu` / `evil-anzu`                                     | MELPA Stable | 0.67 @ `be0151` / 0.3 @ `64cc08`                                                           |
| `apheleia`                                               | MELPA Stable | 4.5.0 @ `b5d120a419816f9d6b3d0e45f0951dd3d6a10b77` (full SHA from stable archive-contents) |
| `buffer-terminator`                                      | MELPA Stable | 1.2.5 @ `0f31cbbb1a8368f08b486525f419dc3d059f05df` (full SHA)                              |
| `no-littering` / `rainbow-delimiters` / `super-save`     | MELPA Stable | 1.9.0 @ `c949f3` / 2.1.5 @ `791968` / 0.5.0 @ `c7bcc9`                                     |
| `yasnippet-snippets` / `consult-dir` / `org-auto-tangle` | MELPA Stable | 1.1 @ `6fafad` / 0.1 @ `08f543` / 0.7.0 @ `b4e7ab`                                         |
| `nerd-icons` / `move-text` / `sudo-edit`                 | MELPA Stable | 0.1.0 @ `619a03` / 2.0.10 @ `c47727` / 0.1.1 @ `a7ae17`                                    |
| `dtrt-indent` / `iedit` / `esup`                         | MELPA Stable | 1.28 @ `8402da` / 0.9.9.9.9 @ `699e17` / 0.7.1 @ `49e05d`                                  |
| `ibuffer-vc` / `org-super-agenda`                        | MELPA Stable | 0.12 @ `1388d2` / 1.3 @ `0d7851`                                                           |
| `envrc` / `universal-sidecar`                            | MELPA Stable | 0.12 @ `2316e0` / 1.9.2 @ `3b6bd9`                                                         |

### MELPA — 1st priority (snapshot pins; not on GNU ELPA nor MELPA Stable)

| Package                      | Commit   |     | Package                          | Commit   |
| ---------------------------- | -------- | --- | -------------------------------- | -------- |
| `general`                    | `a48768` |     | `evil-surround`                  | `14dc69` |
| `undo-fu` (codeberg)         | `5684ef` |     | `evil-embrace`                   | `3081d3` |
| `undo-fu-session` (codeberg) | `db5e16` |     | `evil-easymotion`                | `629c89` |
| `goto-chg`                   | `72f556` |     | `evil-snipe`                     | `16317d` |
| `evil-collection`            | `fa8da0` |     | `evil-matchit`                   | `751e74` |
| `evil-commentary`            | `c5945f` |     | `evil-textobj-tree-sitter`       | `fecc0e` |
| `evil-args`                  | `a81515` |     | `evil-org`                       | `b1f309` |
| `evil-numbers`               | `616aff` |     | `evil-vimish-fold`               | `b6e0e6` |
| `evil-exchange`              | `3030e2` |     | `vimish-fold`                    | `f71f37` |
| `evil-goggles`               | `34ca27` |     | `solaire-mode`                   | `1bd013` |
| `evil-lion`                  | `5a0bca` |     | `link-hint`                      | `8fda5d` |
| `evil-multiedit`             | `23b53b` |     | `git-timemachine` (codeberg)     | `d1346a` |
| `diredfl`                    | `fe72d2` |     | `dirvish`¹                       | `bf164e` |
| `treemacs-nerd-icons`        | `0c5ddc` |     | `lsp-ui`                         | `8d888a` |
| `treemacs-magit`             | `68e444` |     | `consult-lsp`                    | `f41a39` |
| `treemacs-evil`              | `55079b` |     | `lsp-treemacs`                   | `3519ac` |
| `projection`                 | `870a60` |     | `consult-flycheck`               | `9dd953` |
| `projection-multi`           | `66dfa4` |     | `demap` (gitlab)                 | `c42ec4` |
| `projection-multi-embark`    | `50d4f0` |     | `hl-todo`                        | `527d54` |
| `projection-dape`            | `50d4f0` |     | `flycheck-hl-todo`               | `16b66e` |
| `nerd-icons-ibuffer`         | `590bd8` |     | `consult-todo`                   | `f9ba06` |
| `nerd-icons-completion`      | `45b585` |     | `magit-todos`                    | `7294a9` |
| `nerd-icons-corfu`           | `f821e9` |     | `denote-journal-capture` (sr.ht) | `64ca22` |
| `consult-yasnippet`          | `89e398` |     | `denote-regexp` (sr.ht)          | `08d62c` |
| `yasnippet-capf`             | `f53c42` |     | `denote-wordcloud` (codeberg)    | `01a74f` |
| `denote-sections` (sr.ht)    | `dde683` |     | `denote-citar-sections` (sr.ht)  | `c76659` |

### Source-repo-only (explicit `:straight` recipes; no ELPA channel)

`ghostel` @ `037378b` · `evil-ghostel` @ `037378b` (same repo) · `evil-ts-obj` @ `8d0104f` · `denote-merge` @ `2f8d168` · `denote-solo` @ `69b25cb` · `denote-explore` @ `bd442d7` · `org-block-wrap` @ `9248c9f4`

### NonGNU ELPA only — ignored per your directive

`ws-butler` — **only available in nongnu-elpa.** `treesit-fold` — **only available in nongnu-elpa** (live-verified absent from both MELPA and MELPA Stable today).

¹ Your config's explicit recipe points at a custom GitHub repo for `dirvish`; the channel commit above is MELPA's; the pin will follow your recipe's repo (the lockfile handles this automatically).

## The best way & place to pin ALL straight.el packages in one git-tracked file

Per your `straight_use_package_integration.yaml` (Sections 6–7), the canonical one-place mechanism is straight.el's **version lockfile** — not per-package `:pin` (straight has no bulk-exclusion keyword; `:pin`-per-package for ~120 packages is unmaintainable, and `use-package`'s `:pin` is package.el-only and meaningless here).

1. **The file:** `~/.emacs.d/straight/versions/<profile>.el` (default profile `default.el`). It is a single alist mapping every registered package → exact 40-char SHA-1 — precisely the values tabulated above (the site short-hashes are prefixes of these SHAs; the stable `archive-contents` already shows the full form).
2. **Make it central & trackable:** set `straight-profiles` to a named lockfile (e.g. `packages-lock.el`), then commit that file to your dotfiles Git repo (symlink or move it; straight reads it during Phase-0 bootstrap and checks out exactly those commits on any machine — 100% reproducible).
3. **Populate/refresh it:** after `M-x straight-pull-all`, run `M-x straight-freeze-versions`. Revert with `M-x straight-thaw-versions`; repair drift with `M-x straight-normalize-all`.
4. **Headless automation (topgrade/CI):** `emacs --batch -l ~/.emacs.d/early-init.el -l ~/.emacs.d/init.el --eval "(progn (straight-pull-all) (straight-freeze-versions))"` — never point topgrade's `[git]` glob at `straight/repos/*` (detached-HEAD repos break blind `git pull`).

This gives you exactly what you asked for: every package pinned by commit, in **one** central file, under your Git control, with MELPA-Stable's release-cadence commits providing the durable pins and the lockfile freezing the volatile MELPA snapshots.
