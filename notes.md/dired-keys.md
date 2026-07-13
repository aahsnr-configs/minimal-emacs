# Doom Emacs Dired & Dirvish Commands / Keybindings Reference

Below is the exhaustive list of Dired and Dirvish commands and their corresponding keybindings extracted directly from the Doom Emacs `emacs/dired` module, translated into Vanilla Emacs paradigms.

## Core Dired Bindings

| Keybinding | Command                        | Description                                                           |
| :--------- | :----------------------------- | :-------------------------------------------------------------------- |
| `C-c C-e`  | `wdired-change-to-wdired-mode` | Enter writable Dired mode for bulk regex renames.                     |
| `SPC m h`  | `dired-omit-mode`              | (Local Leader) Toggle hiding of uninteresting files (`.git`, `.elc`). |

## Dirvish Navigation & Layout

| Keybinding      | State         | Command                  | Description                                          |
| :-------------- | :------------ | :----------------------- | :--------------------------------------------------- |
| `?`             | Normal        | `dirvish-dispatch`       | Open the Transient help/cheatsheet menu.             |
| `q`             | Normal        | `dirvish-quit`           | Quit Dirvish session or bury buffer.                 |
| `h` / `<left>`  | Normal/Motion | `dired-up-directory`     | Navigate to parent directory.                        |
| `l` / `<right>` | Normal/Motion | `dired-find-file`        | Open file or enter directory at point.               |
| `TAB` / `gl`    | Normal        | `dirvish-subtree-toggle` | Toggle subtree expansion at point.                   |
| `gh`            | Normal        | `dirvish-subtree-up`     | Jump to parent directory of current subtree.         |
| `F`             | Normal        | `dirvish-layout-toggle`  | Toggle between defined `dirvish-layout-recipes`.     |
| `z`             | Normal        | `dirvish-history-jump`   | Jump to recently visited directories via minibuffer. |
| `b`             | Normal        | `dirvish-quick-access`   | Summon quick-access bookmarks menu.                  |
| `f`             | Normal/Visual | `dirvish-file-info-menu` | Open file info menu (permissions, stats).            |

## Dirvish History & Emergence (Motion State Brackets)

| Keybinding   | State         | Command                         | Description                            |
| :----------- | :------------ | :------------------------------ | :------------------------------------- |
| `[h` / `M-b` | Motion/N-V    | `dirvish-history-go-backward`   | Navigate backward in Dirvish history.  |
| `]h` / `M-f` | Motion/N-V    | `dirvish-history-go-forward`    | Navigate forward in Dirvish history.   |
| `[e`         | Motion        | `dirvish-emerge-next-group`     | Jump to next emerged group header.     |
| `]e`         | Motion        | `dirvish-emerge-previous-group` | Jump to previous emerged group header. |
| `M-e`        | Normal/Visual | `dirvish-emerge-menu`           | Open Transient menu for emerge groups. |

## Dirvish Tools & Menus

| Keybinding | State         | Command              | Description                                   |
| :--------- | :------------ | :------------------- | :-------------------------------------------- |
| `S`        | Normal/Visual | `dirvish-quicksort`  | Change `ls` sorting criteria on the fly.      |
| `M-n`      | Normal/Visual | `dirvish-narrow`     | Live-filter current directory via minibuffer. |
| `M-m`      | Normal/Visual | `dirvish-mark-menu`  | Open mark operations menu.                    |
| `M-s`      | Normal/Visual | `dirvish-setup-menu` | Open setup menu (toggle attributes/preview).  |
| `p`        | Normal        | `dirvish-yank`       | Open yank/paste menu (async copy/move).       |

## Yank & Symlink Prefixes

| Prefix | Key | Command                       | Description                           |
| :----- | :-- | :---------------------------- | :------------------------------------ |
| `y`    | `y` | `dired-do-copy`               | Standard Dired copy.                  |
| `y`    | `n` | `dirvish-copy-file-name`      | Copy filename to kill-ring.           |
| `y`    | `p` | `dirvish-copy-file-path`      | Copy relative path to kill-ring.      |
| `y`    | `l` | `dirvish-copy-file-true-path` | Copy absolute true path to kill-ring. |
| `y`    | `r` | `dirvish-copy-remote-path`    | Copy TRAMP/remote path to kill-ring.  |
| `s`    | `s` | `dirvish-symlink`             | Create absolute symlink.              |
| `s`    | `S` | `dirvish-relative-symlink`    | Create relative symlink.              |
| `s`    | `h` | `dirvish-hardlink`            | Create hardlink.                      |
