I am locked in the **READ-ONLY planning state**. I have conducted a deep-dive audit of the current Dired iteration, cross-referencing it against modern Emacs 29/30/31 best practices, Doom Emacs paradigms, and recent Emacs 31 bug tracker discussions up to July 2026.

While the current iteration successfully resolves the fatal "blank screen" load-order physics bug via `dired-before-readin-hook` [[14], [17], [20]], it is missing several critical modern features and IDE-grade Git integrations that are standard in high-tier Vanilla configurations.

Below is the detailed audit of friction points, missing Emacs 31 features, and proposed architectural enhancements.

---

### 1. Audit of Current Iteration (Friction & Edge Cases)

#### A. `dired-hide-details-mode` (Friction Point)

- **The Issue:** The current hook enables `dired-hide-details-mode` by default.
- **Analysis:** While minimalist configurations (like Protesilaos Stavrou's) enable this to reduce visual clutter [[30], [41], [45]], for an IDE power-user workflow, hiding file sizes, permissions, and modification dates by default defeats the primary purpose of a file manager. You will inevitably find yourself constantly pressing `(` to toggle details back on when verifying file states.
- **Recommendation:** Remove it from the default hook. Let the user toggle it manually only when they want a clean, launcher-style view. Don't need a toggle

#### B. `dired-auto-revert-buffer` & Emacs 31 Bug #75626

- **The Issue:** The current config uses `#'dired-buffer-stale-p` to prevent TRAMP I/O hangs while auto-reverting local directories [[22], [24]].
- **Analysis:** There is an ongoing discussion in the Emacs 31 bug tracker (bug#75626, Jan 2025) regarding edge cases where `dired-buffer-stale-p` might miss or double-process files during rapid auto-revert cycles over certain network mounts [[21], [23]].
- **Recommendation:** Replace the raw function symbol with a defensive lambda that explicitly guarantees zero auto-reverts over TRAMP, bypassing the Emacs 31 edge case entirely while keeping local directories snappy:
  ```emacs-lisp
  (dired-auto-revert-buffer (lambda (dir) (and (not (file-remote-p dir)) (dired-buffer-stale-p dir))))
  ```

---

### 2. Missing Modern Best Practices (Emacs 29/30/31)

#### A. `dired-mouse-drag-files` (Emacs 29+)

- **The Feature:** Introduced in Emacs 29, this allows you to click and drag files directly out of the Dired buffer into other applications (like a web browser, Slack, or OS file manager) or other Emacs windows [[37], [38]].
- **Best Practice:** This is highly recommended for modern OS integration and is present in top-tier Emacs 29+ configs [[39], [40], [41]].

#### B. `dired-isearch-filenames`

- **The Feature:** By default, pressing `C-s` (isearch) in Dired searches the entire line, including permissions, owners, and dates.
- **Best Practice:** Setting this to `'dwim` restricts the search to _only_ the file names when the cursor is on a file name . This is a massive QoL improvement for navigating large directories without matching random permission strings like `rwx`.

---

### 3. Missing IDE & Git Workflow Integrations

#### A. `dired-vc-rename-file` (Crucial for Magit)

- **The Issue:** By default, when you rename or move a file in Dired (or WDired), Emacs performs a raw filesystem `mv`. Git sees this as a deleted file and a new untracked file, breaking your commit history.
- **Best Practice:** Setting `dired-vc-rename-file` to `t` forces Dired to use `vc-rename-file` (which executes `git mv`) when operating inside a Git repository [[53], [54], [57]]. Doom Emacs enables this by default . This is mathematically required for a seamless Magit workflow.

#### B. `dired-create-destination-dirs`

- **The Issue:** If you try to copy or move a file into a nested directory that doesn't exist yet (e.g., `src/components/new-folder/`), Dired will throw an error and abort.
- **Best Practice:** Setting this to `'ask` (Doom's standard) prompts you to automatically create the missing parent directories [[46], [48]].

#### C. `dired-clean-up-buffers-too`

- **The Issue:** When you delete a file in Dired, the buffer visiting that file remains open, cluttering your buffer list and `consult-buffer`.
- **Best Practice:** Set to `t` to automatically kill buffers associated with deleted files.

#### D. `dired-guess-shell-alist-user`

- **The Issue:** Pressing `!` on a file prompts for a shell command. Dired's default guesses are often outdated or missing modern extensions.
- **Best Practice:** Inject modern defaults like `xdg-open` (since you are on Arch Linux/PGTK) to handle PDFs, images, and videos seamlessly without typing the command manually.

---

### 4. Proposed Delta (For Your Review)

I have **not** performed a rewrite. Below is the exact Elisp delta required to inject these modern best practices into the existing `:custom` and `:config` blocks.

**Additions to `:custom` block:**

```emacs-lisp
  ;; Git Integration: Use `git mv` when renaming files in VC repositories.
  (dired-vc-rename-file t)
  ;; Prompt to create missing parent directories when copying/moving.
  (dired-create-destination-dirs 'ask)
  ;; Kill associated buffers when files are deleted via Dired.
  (dired-clean-up-buffers-too t)
  ;; Restrict C-s (isearch) to file names only when point is on a file.
  (dired-isearch-filenames 'dwim)
  ;; Emacs 29+: Enable drag-and-drop of files to external applications.
  (dired-mouse-drag-files t)
  ;; Defensive auto-revert: Revert local dirs, but strictly ignore TRAMP to prevent I/O hangs.
  (dired-auto-revert-buffer (lambda (dir)
                              (and (not (file-remote-p dir))
                                   (dired-buffer-stale-p dir))))
```

**Additions to `:config` block:**

```emacs-lisp
  ;; Arch Linux / PGTK: Map common media and document extensions to xdg-open.
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "xdg-open" "evince" "zathura")
          ("\\.\\(?:jpe?g\\|png\\|gif\\|svg\\|webp\\)\\'" "xdg-open" "imv")
          ("\\.\\(?:mp4\\|mkv\\|webm\\|mov\\)\\'" "xdg-open" "mpv")
          ("\\.\\(?:mp3\\|flac\\|wav\\|ogg\\)\\'" "xdg-open")))
```

**Removal from `:hook` block:**

- Remove `(dired-mode . dired-hide-details-mode)` to preserve vital file metadata visibility by default.

---

I await your explicit `GREEN LIGHT` to execute the final integration of these modern Emacs 31 best practices into the Dired subsection.
