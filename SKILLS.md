# SKILLS.md — procedures for testing and debugging the Emacs 31.1 configuration

Companion to **AGENTS.md** (rules, invariants, definition of done). Each skill
below is self-contained: *when to use it · prerequisites · procedure · how to
read the result · typical failure modes*.

> **Audit status (2026-10-03).** GNU Emacs 31.1 is installed; the build reports
> native compilation, tree-sitter and PGTK available. T0 was run on the ten
> configuration Lisp files and passed. The straight package tree is absent,
> and this checkout has no `tools/` or `test/` directory, so package-dependent
> boot, ERT, trigger and interactive procedures were not run. The code blocks
> naming files under those directories are recipes, not checked-in tools or
> tests; save them before using their commands. Internal/private APIs
> (`straight--*`, `advice--*`, `timer--*`) may differ by version. Verify any
> command name with `C-h f` / `M-x apropos-command` before relying on it.

| # | Skill | Tier |
|---|---|---|
| S0 | Provision an isolated sandbox | all |
| S1 | Static gates: read-all, byte-compile (G2), native-comp | T0–T1 |
| S2 | Form inventory, ledger, duplicate-definition scan | T2 |
| S3 | Batch boot + invariant tests (and `after-init` caveat) | T3 |
| S4 | Deferred-trigger testing | T4 |
| S5 | Keybinding / leader audit | T5 |
| S6 | Interactive driving: TTY (tmux), daemon, GUI/PGTK | T6 |
| S7 | Writing ERT tests; coverage; worked examples | T3–T7 |
| S8 | Debugging errors: backtraces, bisection, forgiving loader | — |
| S9 | Startup & runtime performance, hangs | T9 |
| S10 | Debugging packages (built-in and external) | — |
| S11 | Subsystem runbooks (per module) | T7 |
| S12 | Reload, idempotence, bytecode cache | T8 |
| S13 | Fix → verify → report workflow | — |

---

## S0 — Provision an isolated sandbox

**Why:** loading the config writes to the Org directory, may replace
`<root>/eln-cache`, starts autosaving, loads/saves bufferlo workspaces,
downloads grammars, etc.
(AGENTS §5). Never test on real state.

**Prerequisites:** `emacs` 31.x, `rsync`, `git`, a C compiler for
tree-sitter grammars / jinx / some packages, network for first-time straight
clones (unless you copy a populated `var/straight/`).

The following Bash functions are a recipe; `tools/ar-env.sh` is not checked
in. If you save it, create the `tools/` directory first and source it from the
project root. `AR_SRC` must identify this checkout (or another intended config
root); the default uses the current Git worktree root.

```bash
# tools/ar-env.sh — source me.  Never run against the real tree.
ar_sandbox_create() {
  local src tmp_root link target resolved rel
  src="${AR_SRC:-$(git -C "$PWD" rev-parse --show-toplevel 2>/dev/null)}"
  [ -n "$src" ] && [ -d "$src" ] || { echo "Set AR_SRC to the config root" >&2; return 1; }
  src="$(cd -- "$src" && pwd -P)" || return
  tmp_root="${TMPDIR:-/tmp/opencode}"
  mkdir -p -- "$tmp_root" || return
  export AR_SRC="$src" AR_SB
  AR_SB="$(mktemp -d "$tmp_root/ar-sb.XXXXXX")" || return
  mkdir -p -- "$AR_SB"/{cfg,home/.config,home/.cache,home/.local/share,home/.local/state,docs,run,tmp,snippets,etc}
  chmod 700 "$AR_SB/run"
  # Copy source only. Never copy user customizations, snippets or machine state.
  rsync -a --exclude='/.git' --exclude='/eln-cache' \
    --exclude='/etc/custom.el' --exclude='/etc/abbrev_defs' \
    --exclude='/snippets/***' --exclude='/var/***' \
    "$src"/ "$AR_SB/cfg"/ || return
  # Optionally reuse straight's package checkouts/builds, but keep them isolated.
  if [ -d "$src/var/straight" ]; then
    mkdir -p -- "$AR_SB/cfg/var/straight" || return
    rsync -a "$src/var/straight"/ "$AR_SB/cfg/var/straight"/ || return
  fi
  # Redirect absolute links into the source copy to its sandbox counterpart.
  while IFS= read -r -d '' link; do
    target="$(readlink -- "$link")" || return
    case "$target" in
      "$src") ln -sfn -- "$AR_SB/cfg" "$link" || return ;;
      "$src"/*)
        rel="${target#"$src"/}"
        ln -sfn -- "$AR_SB/cfg/$rel" "$link" || return ;;
    esac
  done < <(find "$AR_SB/cfg" -type l -print0)
  ar_sandbox_check "$src" || return
  printf 'sandbox: %s\n' "$AR_SB"
}

ar_sandbox_check() {   # Reject every symlink that resolves outside the sandbox.
  local src="${1:-${AR_SRC:?Set AR_SRC}}" link resolved
  src="$(cd -- "$src" && pwd -P)" || return
  while IFS= read -r -d '' link; do
    resolved="$(realpath -m -- "$link")" || return
    case "$resolved" in
      "$AR_SB"/*) ;;
      *) echo "ESCAPING SYMLINK: $link -> $resolved (source: $src)" >&2; return 1 ;;
    esac
  done < <(find "$AR_SB/cfg" -type l -print0)
}

# Environment wrapper: every path Emacs could touch lives inside $AR_SB.
ar_env() {
  : "${AR_SB:?Run ar_sandbox_create first}"
  env HOME="$AR_SB/home" XDG_CONFIG_HOME="$AR_SB/home/.config" \
      XDG_CACHE_HOME="$AR_SB/home/.cache" XDG_DATA_HOME="$AR_SB/home/.local/share" \
      XDG_STATE_HOME="$AR_SB/home/.local/state" XDG_DOCUMENTS_DIR="$AR_SB/docs" \
      XDG_RUNTIME_DIR="$AR_SB/run" TMPDIR="$AR_SB/tmp" "$@"
}

# Batch test mode: startup has already passed before these explicit loads.
# Config-registered startup hooks must be invoked manually in a dedicated test.
ar_batch() (
  : "${AR_SB:?Run ar_sandbox_create first}"
  cd -- "$AR_SB/cfg" || exit
  ar_env emacs -Q --batch --init-directory "$AR_SB/cfg" \
    -l "$AR_SB/cfg/early-init.el" -l "$AR_SB/cfg/init.el" "$@"
)
```

**Isolation preflight (run before loading config):**

```bash
ar_env emacs -Q --batch --init-directory "$AR_SB/cfg" \
  --eval '(princ (format "%S\n" (list emacs-version
    (native-comp-available-p) (treesit-available-p) (featurep (quote pgtk))
    user-emacs-directory noninteractive)))'
```

Expected: Emacs `31.x`; build capabilities are reported, not assumed;
`user-emacs-directory` is `$AR_SB/cfg/` before `early-init.el` runs. If the
directory is not inside `$AR_SB`, stop. `--init-directory` alone does not
redirect `HOME` or XDG paths; `ar_env` does.

**Config boot (separate, potentially networked):** `ar_batch` loads `init.el`;
if `var/straight/` is not populated, straight bootstraps and installs packages.
Run it in the foreground only after confirming that network/package builds are
acceptable. Keep the log and monitor the foreground process; interrupt it with
Ctrl-C if needed. A failed bootstrap raises `Package bootstrap FAILED (n): …`.

```bash
set -o pipefail
ar_batch --eval '(princ (format "BOOT-OK config=%s var=%s custom=%s\n"
  ar/config-dir user-emacs-directory custom-file))' 2>&1 | tee "$AR_SB/boot.log"
```

This checkout currently has no `var/straight/`, so a full config boot needs
network access and may compile packages. Do not use `emacs --batch
--init-directory ...` as an automatic-config mode: `--batch` implies `-q` and
does not load `early-init.el` or `init.el`.

**Failure modes:** `rsync` copies `var/straight/build` symlinks → `ar_sandbox_check`
catches links escaping the sandbox; `Cannot open load file` for `straight` →
bootstrap file missing (allow network or copy an approved populated
`var/straight/`); sockets too long (`XDG_RUNTIME_DIR` path > ~100 chars) → use
a shorter sandbox parent directory.

---

## S1 — Static gates (T0, T1)

### T0: every file reads to EOF

Save this as `tools/ar-check-syntax.el` before running the command below:

```elisp
;;; -*- lexical-binding: t; -*-
(defun ar-check-syntax (files)
  (let (failed)
    (dolist (f files)
      (with-temp-buffer
        (insert-file-contents f)
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (goto-char (point-min))
        (unless (looking-at-p ".*-\\*-.*lexical-binding: *t")
          (push (format "%s:1: missing lexical-binding cookie" f) failed))
        (condition-case err
            (while (progn (forward-comment (buffer-size)) (not (eobp)))
              (read (current-buffer)))
          (error (push (format "%s:%d: %S" f (line-number-at-pos) err) failed)))))
    (if failed
        (progn (dolist (m (nreverse failed)) (princ (concat m "\n"))) (kill-emacs 1))
      (princ "T0 OK\n"))))
```

```bash
emacs -Q --batch -l tools/ar-check-syntax.el \
  --eval '(ar-check-syntax (append (list "early-init.el" "init.el") (file-expand-wildcards "lisp/*.el")))'
```

### T1: byte-compile gate (**G2**) — zero warnings

```bash
ar_compile_gate() {
  local out log
  out="$(mktemp -d "$AR_SB/elc.XXXXXX")" || return
  log="$(mktemp "$AR_SB/compile.XXXXXX.log")" || return
  set -o pipefail
  ar_batch \
    --eval "(setq byte-compile-error-on-warn t
                  byte-compile-dest-file-function
                  (lambda (f) (expand-file-name (concat (file-name-nondirectory f) \"c\") \"$out\")))" \
    --eval "(add-to-list 'load-path (expand-file-name \"lisp\" ar/config-dir))" \
    -f batch-byte-compile "$AR_SB"/cfg/lisp/*.el 2>&1 | tee "$log" || return
  if grep -Eqi 'warning|error' "$log"; then
    echo "Warnings/errors in $log" >&2
    return 1
  fi
}
```

- Writes `.elc` into a fresh directory under `$AR_SB`, never next to sources
  (the project forbids `.elc` beside `lisp/`). Logs are also kept in the
  sandbox; no cleanup command is required.
- The modules are already loaded (`init.el` `require`s them) so macros and
  `declare-function`s resolve like in the real gate. The source states the gate
  loads the `early-init` + `init` pair first because modules call `ar/var-file`
  at compile time.
- Read warnings by category: *free variable* → missing `(defvar x)` **before**
  first use (ordering matters; the source documents this three times);
  *not known to be defined* → missing `declare-function`; *obsolete* →
  replace (`when-let` → `when-let*`); *docstring wider than 80* → reflow.
- Compile `early-init.el` and `init.el` as informational (not part of G2).

### Native-comp spot check

```bash
ar_batch --eval '(progn (setq native-comp-verbose 1 native-comp-async-report-warnings-errors t
                              native-comp-jit-compilation nil)
                        (dolist (f (file-expand-wildcards (expand-file-name "lisp/*.el" ar/config-dir)))
                          (native-compile f (expand-file-name (concat (file-name-base f) ".eln") (getenv "TMPDIR")))))'
```

Native-comp warnings are silenced in normal sessions
(`native-comp-async-report-warnings-errors 'silent`, `warning-suppress-types
((comp))`); this makes them visible. Also read `*Async-native-compile-log*`
in a live session.

---

## S2 — Form inventory, ledger, duplicate scan (T2)

**Purpose:** realise AGENTS §8. Produces one TSV row per top-level form
(`test/LEDGER.tsv`) and finds symbols defined more than once across the
config — including `defun`s nested inside `use-package :config` bodies
(`ar/undo-fu-session-hash-path`, `ar/lsp-sync-workspaces`,
`ar/yas-reload-snippets-on-save`, `ar/embrace-markdown-setup`, …).

`tools/ar-inventory.el`:

Save the following code as `tools/ar-inventory.el` before running the commands.

```elisp
;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)

(defconst ar-inv--def-heads
  '(defun defmacro defsubst cl-defun cl-defmacro defvar defvar-local defconst
    defcustom defgroup define-minor-mode define-derived-mode define-advice
    transient-define-prefix transient-define-suffix transient-define-infix))

(defun ar-inv--read-forms (file)
  "Return list of (LINE . FORM) for every top-level form in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let (acc)
      (while (progn (forward-comment (buffer-size)) (not (eobp)))
        (let ((line (line-number-at-pos)))
          (push (cons line (read (current-buffer))) acc)))
      (nreverse acc))))

(defun ar-inv--def-name (x)
  "If X is a definition form with a value/body, return its symbol, else nil."
  (when (and (consp x) (memq (car x) ar-inv--def-heads) (symbolp (cadr x)) (cadr x))
    (cond ((and (eq (car x) 'defvar) (null (cddr x))) nil)       ; (defvar foo) = declaration
          ((eq (car x) 'define-advice)                            ; SYMBOL@NAME
           (intern (format "%s@%s" (cadr x) (nth 2 (nth 2 x)))))
          (t (cadr x)))))

(defun ar-inv--nested-defs (form)
  (let (acc)
    (cl-labels ((walk (x)
                  (when (consp x)
                    (let ((n (ar-inv--def-name x))) (when n (push (cons (car x) n) acc)))
                    (let ((y x)) (while (consp y) (walk (car y)) (setq y (cdr y)))))))
      (walk form))
    (nreverse acc)))

(defun ar-inv--class (head kws form)
  (cond
   ((memq head '(declare-function provide)) "static")
   ((and (eq head 'defvar) (null (cddr form))) "static")
   ((eq head 'use-package)
    (cond ((memq :demand kws) "eager(:demand)")
          ((cl-intersection kws '(:defer :commands :hook :bind :bind* :mode :interpreter :magic))
           (format "deferred[%s]" (mapconcat #'symbol-name
                    (cl-intersection kws '(:defer :commands :hook :bind :bind* :mode :interpreter :magic :after)) ",")))
          ((memq :after kws) "after-features(see macroexpand)")
          (t "eager(implicit)")))
   ((eq head 'with-eval-after-load) (format "after-load:%S" (nth 1 form)))
   ((eq head 'add-hook) (format "hook:%S" (nth 1 form)))
   ((memq head '(advice-add add-function)) "advice")
   ((memq head '(run-with-idle-timer run-at-time run-with-timer)) "timer")
   ((memq head '(defun defmacro defsubst cl-defun cl-defmacro defcustom defconst defvar
                 defvar-local defgroup define-minor-mode define-derived-mode
                 transient-define-prefix define-advice))
    "definition")
   ((memq head '(ar/global-leader ar/local-leader general-define-key)) "keybinding")
   (t "toplevel-effect")))

(defun ar-inv-ledger (files)
  "Print the TSV ledger for FILES to stdout."
  (princ "file\tline\thead\tname\tkeywords\tclass\tstatus\tevidence\n")
  (dolist (f files)
    (dolist (lf (ar-inv--read-forms f))
      (let* ((form (cdr lf)) (head (car-safe form))
             (kws (when (eq head 'use-package)
                    (cl-loop for x in (nthcdr 2 form) when (keywordp x) collect x)))
             (name (and (consp form) (memq head '(defun defmacro defvar defcustom defconst defgroup
                          defvar-local use-package define-minor-mode define-advice
                          transient-define-prefix declare-function with-eval-after-load add-hook
                          define-derived-mode cl-defun defsubst provide))
                        (format "%S" (nth 1 form)))))
        (princ (format "%s\t%d\t%s\t%s\t%s\t%s\tUNVERIFIED\t\n"
                       (file-name-nondirectory f) (car lf) head (or name "")
                       (mapconcat #'symbol-name kws " ") (ar-inv--class head kws form)))))))

(defun ar-inv-duplicates (files)
  "Report symbols defined (with value/body) more than once across FILES."
  (let ((tbl (make-hash-table :test 'eq)) (dups 0))
    (dolist (f files)
      (dolist (lf (ar-inv--read-forms f))
        (dolist (d (ar-inv--nested-defs (cdr lf)))
          (push (format "%s:%d (%s)" (file-name-nondirectory f) (car lf) (car d))
                (gethash (cdr d) tbl)))))
    (maphash (lambda (sym locs)
               (when (cdr locs)
                 (cl-incf dups)
                 (princ (format "DUPLICATE %s -> %s\n" sym (string-join (reverse locs) "; ")))))
             tbl)
    (princ (format "%d duplicate symbol(s)\n" dups))
    (when (> dups 0) (kill-emacs 1))))
```

```bash
mkdir -p test
mkdir -p "${AR_TMP:-/tmp/opencode}"
ledger_candidate="$(mktemp "${AR_TMP:-/tmp/opencode}/LEDGER.generated.XXXXXX.tsv")"
emacs -Q --batch -l tools/ar-inventory.el \
  --eval '(let ((files (append (list "early-init.el" "init.el")
                              (directory-files "lisp" t "\\.el$"))))
            (ar-inv-ledger files))' > "$ledger_candidate"
if [ ! -e test/LEDGER.tsv ]; then
  cp "$ledger_candidate" test/LEDGER.tsv
else
  echo "Existing test/LEDGER.tsv retained; compare it with $ledger_candidate."
fi
emacs -Q --batch -l tools/ar-inventory.el \
  --eval '(ar-inv-duplicates (append (list "early-init.el" "init.el")
                                     (directory-files "lisp" t "\\.el$")))'
```

**Reading the class column.** `deferred[...]` rows need a trigger test (S4).
`after-features(see macroexpand)` and `eager(implicit)` are *hints*: confirm
by expansion, because `use-package` semantics decide whether the body runs:

```elisp
(pp (macroexpand-all '(use-package evil-commentary :after evil :config (evil-commentary-mode 1))))
```

Look for an unconditional `require` vs. `with-eval-after-load` vs. nothing.
Historical failures in this config included deferred `evil-textobj-tree-sitter`
and `undo-fu-session` blocks that had no working trigger. The current forms
use `:demand t`; they are not current examples of dead deferral. Confirm each
present-day block from its macroexpansion and actual trigger.

This config declares no intentional dual definitions, so every duplicate is
a finding (AGENTS §4.1 #2).

---

## S3 — Batch boot + invariant tests (T3)

### 3.1 Cold boot with logs

```bash
set -o pipefail
ar_batch --eval '(message "BOOT-OK features=%d" (length features))' 2>&1 | tee "$AR_SB/boot.log"
if grep -Ei 'error|warning|✗|failed|void' "$AR_SB/boot.log"; then
  echo "BOOT NOISE — triage" >&2
fi
```

Any `Warning (…)`, `Package bootstrap FAILED`, `lsp-mode compiled with
lsp-use-plists=…` (plist mismatch), or `Symbol's value as variable is void`
is a finding.

### 3.2 The `after-init-hook` caveat

With `ar_batch` (`-Q` plus explicit `-l`), `after-init-hook` is processed
before the config files are loaded, so the config's `:hook (after-init . …)`
handlers are not automatically invoked. `--batch` also implies `-q`; there is
no Mode B that automatically loads the config. To test the handler bodies,
invoke the hook after loading the config, in a dedicated sandbox process (this
can start direnv/diff-hl work):

```bash
ar_batch --eval '(progn (run-hooks (quote after-init-hook))
  (message "%S" (list global-corfu-mode super-save-mode editorconfig-mode
                      envrc-global-mode global-diff-hl-mode)))'
```

For the GC/file-handler restore body, `emacs-startup-hook` is not an automatic
batch trigger. In a separate `ar_batch` process, invoke
`(run-hooks 'emacs-startup-hook)` after config loading, then inspect
`(list gc-cons-threshold gc-cons-percentage (and file-name-handler-alist t))`.
Use S6 as well when the actual live-startup ordering or frame behavior matters.

### 3.3 Invariant ERT suite — `test/ar-test-boot.el`

```elisp
;;; -*- lexical-binding: t; -*-
(require 'ert) (require 'cl-lib) (require 'subr-x)

(ert-deftest ar-test/boot/paths ()
  (should (equal (file-name-as-directory user-emacs-directory) ar/var-dir))
  (should (string-prefix-p (expand-file-name ar/etc-dir) (expand-file-name custom-file)))
  (should (string-prefix-p (expand-file-name ar/var-dir) (expand-file-name recentf-save-file)))
  (should (string-prefix-p (expand-file-name ar/var-dir) (expand-file-name savehist-file))))

(ert-deftest ar-test/boot/modules ()
  (should (equal ar-modules '(core-config editor-config ui-config completion-config
                              org-config dev-config vc-projects-config keybinds-config)))
  (dolist (m ar-modules) (should (featurep m))))

(ert-deftest ar-test/boot/env-and-numbers ()
  (should (equal (getenv "LSP_USE_PLISTS") "true"))
  (should (= scroll-margin 0))                       ; AGENTS §4.4 #12
  (should (null package-enable-at-startup))
  (should (= gc-cons-threshold (* 256 1024 1024)))   ; batch cap from early-init
  (should (equal vc-handled-backends '(Git))))

(ert-deftest ar-test/boot/gc-literals-agree ()
  ;; early-init hardcodes 100000000 in the restore hook; core defines the const.
  (should (= ar/gc-steady-cons-threshold 100000000)))

(ert-deftest ar-test/boot/evil-stack ()
  (should (bound-and-true-p evil-mode))
  (should (eq evil-undo-system 'undo-fu))
  (should (fboundp 'ar/global-leader))
  (should (fboundp 'ar/local-leader)))

(ert-deftest ar-test/boot/hooks-and-advice ()
  (should (memq 'ar/lsp-ensure-maybe prog-mode-hook))
  (should (memq 'flymake-mode prog-mode-hook))
  (should (memq 'apheleia-mode prog-mode-hook))
  (should (advice-member-p #'ar/popper-close-window-hack 'keyboard-quit))
  (should (timerp ar/recentf-flush-timer)))

(ert-deftest ar-test/boot/no-warnings-buffer ()
  (should-not (get-buffer "*Warnings*")))

;; Deferral contract (AGENTS §4.2 #7). A failure is a FINDING to triage:
;; either the contract or the config is stale.
(ert-deftest ar-test/boot/deferred-features-not-loaded ()
  (dolist (f '(magit forge lsp-mode dape ghostel yasnippet jinx dirvish treemacs))
    (should-not (featurep f))))

(ert-deftest ar-test/boot/eager-features-loaded ()
  (dolist (f '(evil evil-collection general nerd-icons doom-modeline which-key
               vertico org popper undo-fu-session))
    (should (featurep f))))

;; Drift: every :custom value in every use-package form should still be the
;; default-value after the whole config loaded (later setq / package defvar /
;; theme can silently override).  Output is a TRIAGE LIST, not a verdict.
(defun ar-test--read-forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let (acc) (while (progn (forward-comment (buffer-size)) (not (eobp)))
                 (push (read (current-buffer)) acc))
         (nreverse acc))))

(defun ar-test--customs (form)
  (when (eq (car-safe form) 'use-package)
    (let (acc cur)
      (dolist (a (nthcdr 2 form))
        (cond ((keywordp a) (setq cur a))
              ((and (eq cur :custom) (consp a) (symbolp (car a)))
               (push (list (nth 1 form) (car a) (cadr a)) acc))))
      (nreverse acc))))

(ert-deftest ar-test/boot/custom-drift ()
  (let (drift)
    (dolist (f (file-expand-wildcards (expand-file-name "lisp/*.el" ar/config-dir)))
      (dolist (form (ar-test--read-forms f))
        (dolist (c (ar-test--customs form))
          (pcase-let ((`(,pkg ,var ,val) c))
            (unless (or (memq (car-safe val) '(lambda function))
                        (not (boundp var)))
              (let ((want (condition-case nil (eval val t) (error 'ar--unevaluable)))
                    (have (default-value var)))
                (unless (or (eq want 'ar--unevaluable) (equal want have))
                  (push (list pkg var want have) drift))))))))
    (when drift (message "CUSTOM DRIFT:\n%s" (mapconcat #'prin1-to-string (nreverse drift) "\n")))
    (should (null drift))))
```

Save this ERT suite as `test/ar-test-boot.el` in the config root before
creating the sandbox. Run it from the sandbox root:

```bash
ar_batch -l ert -l test/ar-test-boot.el -f ert-run-tests-batch-and-exit
```

The sample tests define no ERT `:tags`; do not select an `after-init` tag.
Test startup hooks with the explicit hook invocation above, or add a tag to a
test only when its `:tags` property actually exists.

> `custom-drift` evaluating `:custom` values assumes they are side-effect-free
> expressions (they are: `ar/var-file …`, quoted lists, literals). Variables
> whose package is unloaded may be unbound at read time — those are skipped.

---

## S4 — Deferred-trigger testing (T4)

**Rule:** a deferred block is VERIFIED only when you (1) name its trigger,
(2) fire the trigger in a fresh process, (3) assert an *observable effect of
the block's body* (not merely `featurep`).

Save trigger tests as `test/ar-test-triggers.el` before creating the sandbox.
Run each trigger test in its own process so earlier loads don't mask bugs:

```bash
ar_batch -l ert -l test/ar-test-triggers.el \
  --eval '(mapc (lambda (test) (princ (format "%s\n" (ert-test-name test))))
                 (ert-select-tests t t))' > "$AR_SB/ert-test-names" || exit 1
while IFS= read -r test_name; do
  if ar_batch -l ert -l test/ar-test-triggers.el \
       --eval "(ert-run-tests-batch-and-exit '$test_name)" >"$AR_SB/ert-test.log" 2>&1; then
    echo "ok   $test_name"
  else
    echo "FAIL $test_name"
    tail -20 "$AR_SB/ert-test.log"
  fi
done
```

The ERT test file and sandbox must exist before running this loop.

### Recipes by trigger type

| Trigger in source | Fire it | Assert the body took effect |
|---|---|---|
| `:commands` / autoload | `(autoload-do-load (symbol-function 'CMD) 'CMD)` | `(featurep 'PKG)`, `(commandp 'CMD)` |
| `:hook (HOOK . FN)` / `add-hook` | Set up the buffer/mode if needed, then explicitly run the exact hook named in the declaration, e.g. `(run-hooks 'HOOK)`. `delay-mode-hooks` suppresses hooks; do not assume a mode call triggers the hook if the mode is delayed. | Assert the hook's observable effect; stub external processes and other risky side effects. |
| `:bind (:map M …)` | Check the binding after config load, then invoke the bound command in a safe context to fire its autoload | `(keymap-lookup M "KEY")` ⇒ expected command; then assert package/body effect |
| `:after X` (no `:demand`) | `(require 'X)` | package body effect; `featurep PKG` |
| `with-eval-after-load 'F` | In a fresh process, load/provide `F` after registering the form | Assert the form's observable effect (for example, the expected function becomes defined). |
| `:defer N` | find the idle timer, call `(timer-event-handler tm)` — idle timers never fire in batch | `(featurep 'PKG)` and `:config` effect |
| `run-with-idle-timer` / `run-at-time` | same; inspect `timer-list` / `timer-idle-list` | handler's effect |
| `advice-add` on not-yet-defined fn | define/load the target | `(advice-member-p ADV 'TARGET)` still true after load |
| `:custom-face` | The use-package declaration is evaluated when its containing form runs | Verify the effective face attributes after loading the form and enabling the intended theme; avoid relying on undocumented face properties. |

Idle-timer helper (internal accessors):

```elisp
(defun ar-test-fire-idle-timers-matching (pred)
  "Run every pending idle/regular timer whose function or args satisfy PRED."
  (dolist (tm (append timer-list timer-idle-list))
    (when (funcall pred tm) (timer-event-handler tm))))     ; internal-ish
;; use-package :defer N arms (run-with-idle-timer N nil #'require 'PKG nil t):
(ar-test-fire-idle-timers-matching (lambda (tm) (memq 'buffer-terminator (timer--args tm)))) ; internal
```

### Representative deferred blocks and their triggers

This is a source-backed starting checklist, not a complete inventory. Refresh it
from the current S2 ledger after edits and include every presently deferred
block; do not keep historical failures listed as active deferrals.

| Module | Block | Trigger | Effect to assert |
|---|---|---|---|
| core | `tabulated-list` | `(require 'tabulated-list)` | `q` bound to `quit-window` in `tabulated-list-mode-map` |
| core | `buffer-terminator` | `:defer 60` idle timer | `buffer-terminator-mode` on; timeout 1800 s, interval 600 s |
| core | `whitespace`, `display-line-numbers` | `prog-mode`/`conf-mode` hooks | mode on in a small buffer; **guard advice** refuses in a >500 k buffer (and `C-u -1` bypass) |
| editor | `undo-fu`, `goto-chg`, `evil-numbers`, `evil-ts-obj`, `ws-butler` | undo integration, `:commands`, prog/mode hooks | `glc-default-span` 16; hooks enable the modes; commands autoload |
| editor | `evil-textobj-tree-sitter`, `undo-fu-session` | eager (`:demand t`), not deferred | `:config` effects present after module load |
| editor | `super-save` | `after-init` hook | mode on after `(run-hooks 'after-init-hook)`; triggers list includes `ace-window` |
| editor | `with-eval-after-load 'transient` (×2) | `(require 'transient)` | `ar/ast-refactor-transient`, `ar/avy-transient` fboundp |
| editor | `jinx`, `helpful`, `hl-todo`, `sudo-edit` | `text-mode-hook`, commands | `jinx-mode` in text buffer; `jinx-local-words` buffer-local = personal words |
| ui | large-file guard advices (6) | calling the mode with a large buffer | mode refuses; `C-u -1` bypass; `treesit-fold-mode` also refuses in Lisp modes |
| ui | `posframe` | verify the actual current consumer; ace-window childframe hints were dropped | Assert only the consumer's configured posframe effect; do not use ace-window as the trigger |
| ui | `with-eval-after-load 'face-remap` | first zoom command | `ar/global-zoom-*` use the face-remap vars |
| completion | `vertico-*` extensions, `consult-dir`, `consult-todo`, `embark*`, `cape`, `dabbrev`, `grep` | commands / `:after` | e.g. `completion-at-point-functions` contains `cape-elisp-block`; `lsp-completion-at-point` advised with `cape-wrap-nonexclusive` once lsp-completion loads |
| org | `org-agenda`, `org-super-agenda`, `org-habit`, each `denote-*`, `org-transclusion`, `org-capture`, `org-modern`, `evil-org`, `universal-sidecar`, `denote-sections` | `:after`, hooks, commands | see S11 Org runbook |
| dev | `lsp-mode` (hook `prog-mode`) | open a non-excluded `prog-mode` buffer | `lsp-completion-mode-hook` has `ar/lsp-completion-orderless`; `lsp--workspaces` mirror defined |
| dev | `eldoc-box`, `flymake`, `editorconfig`, `python`, `apheleia`, `envrc`, `yasnippet*`, `dape` | hooks / commands | `apheleia-mode-alist` Python entries = `(ruff-isort ruff)`; `yas-snippet-dirs` first-or-second is `my/snippets-directory`; `dape-default-breakpoints-file` under `var/dape/` |
| vc | `magit`, `forge`, `magit-todos`, `diff-hl`, `git-timemachine`, `blamer`, `dired`, `dirvish`, `treemacs*`, `project`, `projection`, `ibuffer`, `bufferlo`, `ediff`, `woman`, `ghostel*` | commands / hooks | `forge-add-default-bindings` nil; `magit-todos-mode` on in a magit buffer; `woman-manpath` set; ghostel display rule present |
| keybinds | `ace-window` | `:commands` | command autoload loads package and configured command |
| keybinds | `transient`, `winner`, `popper` | eager/after-load | `popper-mode` on; `keyboard-quit` advised (advice skips active regions and terminal buffers) |

---

## S5 — Keybinding / leader audit (T5)

Static + dynamic, because general.el's resolution depends on evil state and
override keymaps. Don't conclude "missing" from one method.

`test/ar-test-keys.el`:

```elisp
;;; -*- lexical-binding: t; -*-
(require 'ert) (require 'cl-lib) (require 'subr-x)
(load (expand-file-name "test/ar-test-boot.el" ar/config-dir) nil t) ; reuse ar-test--read-forms

(defun ar-test--map-subforms (fn form)
  (when (consp form)
    (funcall fn form)
    (let ((y form)) (while (consp y) (ar-test--map-subforms fn (car y)) (setq y (cdr y))))))

(defun ar-test--leader-pairs (form)
  "For (ar/global-leader K V K V …) return ((LEADER KEY V) …)."
  (when (memq (car-safe form) '(ar/global-leader ar/local-leader))
    (let ((args (cdr form)) acc)
      (while args
        (if (stringp (car args))
            (progn (push (list (car form) (car args) (cadr args)) acc) (setq args (cddr args)))
          (setq args (cdr args))))
      (nreverse acc))))

(defun ar-test--all-leader-pairs ()
  (let (acc)
    (dolist (f (file-expand-wildcards (expand-file-name "lisp/*.el" ar/config-dir)))
      (dolist (form (ar-test--read-forms f))
        (ar-test--map-subforms
         (lambda (x) (dolist (p (ar-test--leader-pairs x)) (push (cons (file-name-nondirectory f) p) acc)))
         form)))
    (nreverse acc)))

(defun ar-test--target-symbol (spec)
  "SPEC is the quoted value: (quote (SYM :wk ..)) | (quote ((lambda ..) :wk ..)) | (quote (:ignore t ..))."
  (let ((v (if (eq (car-safe spec) 'quote) (cadr spec) spec)))
    (cond ((and (consp v) (symbolp (car v)) (not (keywordp (car v))) (car v)))
          (t nil))))

(ert-deftest ar-test/keys/targets-resolve ()
  (let (bad)
    (dolist (row (ar-test--all-leader-pairs))
      (pcase-let ((`(,file ,leader ,key ,spec) row))
        (let ((sym (ar-test--target-symbol spec)))
          (when sym
            (unless (or (commandp sym)
                        (and (fboundp sym) (autoloadp (symbol-function sym))
                             (ignore-errors (autoload-do-load (symbol-function sym) sym) (commandp sym))))
              (push (list file leader key sym) bad))))))
    (should (null bad))))

(ert-deftest ar-test/keys/no-leaf-vs-prefix-conflict ()
  (let* ((rows (ar-test--all-leader-pairs)) bad)
    (dolist (a rows)
      (pcase-let ((`(,fa ,la ,ka ,sa) a))
        (unless (ar-test--prefix-spec-p sa)
          (dolist (b rows)
            (when (and (eq la (nth 1 b)) (string-prefix-p (concat ka " ") (nth 2 b)))
              (push (list fa la ka 'is-leaf-but-prefix-of (nth 2 b)) bad))))))
    (should (null bad))))

(defun ar-test--prefix-spec-p (spec)
  (let ((v (if (eq (car-safe spec) 'quote) (cadr spec) spec)))
    (and (consp v) (eq (car v) :ignore))))

(ert-deftest ar-test/keys/duplicate-key-different-target ()
  (let ((seen (make-hash-table :test 'equal)) bad)
    (dolist (row (ar-test--all-leader-pairs))
      (pcase-let ((`(,file ,leader ,key ,spec) row))
        (let* ((k (cons leader key)) (prev (gethash k seen)))
          (if (and prev (not (equal (cdr prev) spec)) (not (ar-test--prefix-spec-p spec)))
              (push (list key 'in file 'vs (car prev)) bad)
            (puthash k (cons file spec) seen)))))
    (should (null bad))))
```

Also audit every `#'SYMBOL` in all modules (catches dead references the byte
compiler can't, e.g. commands in packages that were dropped from the manifest):

```elisp
(ert-deftest ar-test/refs/function-quotes-resolve ()
  (let (syms bad)
    (dolist (f (file-expand-wildcards (expand-file-name "lisp/*.el" ar/config-dir)))
      (dolist (form (ar-test--read-forms f))
        (ar-test--map-subforms
         (lambda (x) (when (and (eq (car x) 'function) (symbolp (cadr x)) (cadr x)) (push (cadr x) syms)))
         form)))
    (dolist (s (delete-dups syms))
      (unless (or (fboundp s) (macrop s)) (push s bad)))
    ;; Triage: symbols from not-yet-loaded packages are NOT bugs — retry after autoload-do-load
    ;; on their package, or run this test after exercising S4 triggers.
    (should (null bad))))
```

### Dynamic resolution

```elisp
(defun ar-test-key (keys &optional state)
  "What does KEYS do in evil STATE (default normal)?  Multiple strategies."
  (with-temp-buffer
    (evil-local-mode 1)
    (evil-change-state (or state 'normal))
    (list :key-binding (key-binding (kbd keys))
          :evil-map (lookup-key (evil-get-auxiliary-keymap evil-normal-state-map 'normal) (kbd keys)) ; may be nil
          :general (and (fboundp 'general-describe-keybindings) :see-general-describe-keybindings))))
```

If `key-binding` is nil for a key you know exists, fall back to a real
keystroke test in S6 (tmux `send-keys`) — only that proves what the user gets.
Also check, per leader, the **both prefixes**: `SPC …` (states normal/visual/
emacs/insert) and `C-SPC …` (global). In *insert* state `SPC` must still
self-insert — only `C-SPC` should open the tree there; verify, because
`ar/global-leader` declares `insert` in `:states` with `:prefix "SPC"`.

Other keymap checks: `keymap-lookup` for the `ghostel-semi-char-mode-map`
(`C-s`→`consult-line`, `C-k`, `M-p`, `M-n`), `corfu-map` (`TAB`/`S-TAB`
cycle, `RET` unbound), minibuffer maps (`<escape>`→`ar/escape-key`,
`C-s`→`consult-history`), `tabulated-list-mode-map` `q`, global zoom keys.

---

## S6 — Interactive driving (T6)

Some behaviour exists only in a live session: `emacs-startup-hook` state,
`after-init` modes, faces/fonts, child frames, which-key popups, popper,
display-buffer rules, daemon client frames.

### 6.1 TTY via tmux (works headless, best for keystroke truth)

```bash
ar_tty_start() {
  tmux kill-session -t ar 2>/dev/null
  tmux new-session -d -s ar -x 200 -y 55 \
    "$(printf '%q ' env HOME="$AR_SB/home" XDG_CONFIG_HOME="$AR_SB/home/.config" \
       XDG_CACHE_HOME="$AR_SB/home/.cache" XDG_DOCUMENTS_DIR="$AR_SB/docs" \
       XDG_RUNTIME_DIR="$AR_SB/run" TERM=xterm-256color \
       emacs -nw --init-directory "$AR_SB/cfg" \
       --eval '(setq server-name "ar-tty")' --eval '(server-start)')"
  sleep 8
}
ar_keys()   { tmux send-keys -t ar "$@"; }          # e.g. ar_keys Space f f   |  ar_keys C-g  |  ar_keys M-x
ar_screen() { tmux capture-pane -p -t ar; }
ar_ec()     { ar_env emacsclient -s ar-tty -e "$1"; }  # evaluate in the live session
```

Examples:

```bash
ar_keys Space; sleep 1.2; ar_screen        # which-key (idle delay 1.0 s) must list the top-level groups
ar_keys Escape; ar_keys Space f e i        # opens init.el
ar_ec '(list gc-cons-threshold gc-cons-percentage (and file-name-handler-alist t))'   # steady state after startup
ar_ec '(list (childframe-workable-p) (display-graphic-p))'  # TTY ⇒ nil: exercises the fallbacks
```

What TTY cannot prove: fonts, nerd-icon glyphs, child-frame popups
(`childframe-workable-p` is nil), pixel scrolling, PGTK input-method code.

### 6.2 Daemon + client frames

```bash
ar_env emacs --daemon=ar-d --init-directory "$AR_SB/cfg"
ar_env emacsclient -s ar-d -e '(list (daemonp) (featurep (quote server)) (length (frame-list)))'
ar_env emacsclient -s ar-d -c -nw          # run inside tmux to see the client frame
ar_env emacsclient -s ar-d -e '(kill-emacs)'
```

Checks: `server-after-make-frame-hook` contains `ar/setup-fonts` once;
`after-make-frame-functions` contains `ar/pgtk-disable-im-context` on PGTK
builds; LSP does not start at daemon startup (only when a buffer becomes
visible); timers survive client disconnects; `recentf` flush timer set.

### 6.3 Real GUI (PGTK/Wayland) without a desktop

```bash
weston --backend=headless --socket=ar-wl --width=1600 --height=1000 &   # or: cage -- sleep infinity
ar_env env WAYLAND_DISPLAY=ar-wl emacs --init-directory "$AR_SB/cfg" &
```

Then drive via `emacsclient` (start `server-start` with `--eval`). For
screenshots use whatever the compositor offers (`grim` on wlroots-style
compositors) or Emacs' own `x-export-frames` (SVG/PNG/PDF; availability
depends on the build — check `(fboundp 'x-export-frames)`). GUI-only checks:

```elisp
(list (find-font (font-spec :family "JetBrains Mono"))
      (find-font (font-spec :family "Symbols Nerd Font Mono"))
      (find-font (font-spec :family "Noto Sans"))
      (face-attribute 'default :font) custom-enabled-themes
      (childframe-workable-p) (icons-displayable-p)
      (frame-parameter nil 'internal-border-width))
```

If a GUI session is impossible, mark display-only forms (`ar/setup-fonts`,
`ar/hide-modeline-mode` rendering, posframe/corfu child frames, `solaire`,
`indent-bars`, `colorful-mode`) **BLOCKED(no display)** — do not mark them
verified.

---

## S7 — Writing ERT tests; coverage; worked examples

### Conventions

- Files: `test/ar-test-<module>.el`; names `ar-test/<module>/<topic>`.
- Always `(delay-mode-hooks (MODE))` when you only need a major mode set in a
  temp buffer — plain `(python-mode)` runs `prog-mode-hook` ⇒ `ar/lsp-ensure-maybe`
  ⇒ real LSP start, `flymake`, `apheleia`, `yasnippet`, `jinx`, …
- Stub boundaries with `cl-letf` on `symbol-function` (`lsp`, `lsp-deferred`,
  `executable-find`, `start-process`, `shell-command-to-string`).
- Branch on `noninteractive`/`display-graphic-p` by `let`-binding the variable
  (`noninteractive` is special) to exercise **both** branches.
- Test timers/hooks by calling the function directly, not by waiting.
- Each test must fail on the unfixed code (prove by `git stash` of the fix).

### Worked examples

```elisp
;;; -*- lexical-binding: t; -*-
(require 'ert) (require 'cl-lib)
(defvar ar-test--log nil)
(defvar lsp-use-plists)

;; core: with-eval-after-load-all fires once, after the LAST feature, order-independent.
(ert-deftest ar-test/core/with-eval-after-load-all ()
  (setq ar-test--log nil)
  (eval '(with-eval-after-load-all (ar-test-fa ar-test-fb) (push 'ran ar-test--log)) t)
  (should-not ar-test--log)
  (provide 'ar-test-fb) (should-not ar-test--log)
  (provide 'ar-test-fa) (should (equal ar-test--log '(ran)))
  ;; already-loaded feature is satisfied immediately
  (setq ar-test--log nil)
  (eval '(with-eval-after-load-all (ar-test-fa) (push 'again ar-test--log)) t)
  (should (equal ar-test--log '(again)))
  ;; argument validation
  (should-error (macroexpand '(with-eval-after-load-all () 1)))
  (should-error (macroexpand '(with-eval-after-load-all 5 1))))

;; core: too-long-file-p — also documents suspected defect D2.
(ert-deftest ar-test/core/too-long-file-p-chars ()
  (with-temp-buffer (should-not (too-long-file-p))
                    (insert (make-string 500001 ?x)) (should (too-long-file-p))))
(ert-deftest ar-test/core/too-long-file-p-single-long-line ()
  :expected-result :failed   ; flip to normal once D2 is decided/fixed
  (with-temp-buffer (insert (make-string 20000 ?x))   ; one 20 000-col line
                    (should (too-long-file-p))))
(ert-deftest ar-test/core/too-long-file-p-many-lines ()
  (with-temp-buffer (dotimes (_ 10001) (insert "a\n"))
                    (should (too-long-file-p))))        ; passes today (line COUNT)

;; ui: pure poshandler.
(ert-deftest ar-test/ui/poshandler-near-bottom ()
  (should (equal (ar/posframe-poshandler-frame-center-near-bottom
                  '(:parent-frame-width 1000 :parent-frame-height 800
                    :posframe-width 200 :posframe-height 100 :font-height 20))
                 '(400 . 660))))

;; dev: lsp-ensure routes by mode and by noninteractive.
(ert-deftest ar-test/dev/lsp-ensure-maybe ()
  (require 'lsp-mode)   ; load FIRST: loading it later would overwrite the stubs below
  (let (called)
    (cl-letf (((symbol-function 'lsp-deferred) (lambda () (setq called 'deferred)))
              ((symbol-function 'lsp) (lambda (&rest _) (setq called 'lsp)))
              ((symbol-function 'lsp-workspace-folders-add) #'ignore))
      (let ((noninteractive nil))
        (with-temp-buffer (delay-mode-hooks (emacs-lisp-mode)) (ar/lsp-ensure-maybe)
                          (should-not called))
        (with-temp-buffer (delay-mode-hooks (python-mode)) (ar/lsp-ensure-maybe)
                          (should (eq called 'deferred))))
      (setq called nil)
      (let ((noninteractive t))                 ; batch branch: direct `lsp`
        (with-temp-buffer (delay-mode-hooks (python-mode)) (ar/lsp-ensure-maybe)
                          (should (eq called 'lsp)))))))

;; dev: booster advice — bytecode vs JSON passthrough, and command prefixing.
(ert-deftest ar-test/dev/booster-json-parse ()
  (with-temp-buffer
    (insert (prin1-to-string (byte-compile (lambda () 42))))
    (goto-char (point-min))
    (should (equal (lsp-booster--advice-json-parse (lambda (&rest _) 'fallback)) 42)))
  (with-temp-buffer
    (insert "{\"a\":1}") (goto-char (point-min))
    (should (eq (lsp-booster--advice-json-parse (lambda (&rest _) 'fallback)) 'fallback))))

(ert-deftest ar-test/dev/booster-final-command ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (n &rest _) (and (member n '("emacs-lsp-booster" "clangd")) (concat "/usr/bin/" n)))))
    (let ((lsp-use-plists t) (default-directory "/tmp/"))
      (should (equal (lsp-booster--advice-final-command (lambda (c _t) (list (car c))) '("clangd") nil)
                     '("emacs-lsp-booster" "/usr/bin/clangd")))
      ;; test? and remote must be left untouched
      (should (equal (lsp-booster--advice-final-command (lambda (c _t) (list (car c))) '("clangd") t)
                     '("clangd")))
      (let ((default-directory "/ssh:host:/tmp/"))
        (should (equal (lsp-booster--advice-final-command (lambda (c _t) (list (car c))) '("clangd") nil)
                       '("clangd")))))))

;; editor: delete-pair DWIM (prog-mode only).
(ert-deftest ar-test/editor/delete-pair-dwim ()
  (with-temp-buffer (delay-mode-hooks (emacs-lisp-mode))
    (insert "(foo \"bar\")") (ar/delete-pair-dwim)
    (should (equal (buffer-string) "foo \"bar\"")))
  (with-temp-buffer (delay-mode-hooks (text-mode))
    (should-error (ar/delete-pair-dwim) :type 'user-error)))

;; editor: undo-fu-session hashing stays under filesystem limits.
(ert-deftest ar-test/editor/undo-hash-name-length ()
  (let* ((long (concat "/tmp/" (make-string 300 ?a) ".txt"))
         (out (ar/undo-fu-session-hash-path long ".zst")))
    (should (< (length (file-name-nondirectory out)) 255))))   ; basename itself is long → expect FAIL; see note
```

> The last test encodes a thing to **check**, not assume: the function
> appends `(file-name-nondirectory file)` to the hash, so a 300-char basename
> still yields a >255-byte filename, contradicting its docstring ("bypass OS
> 255-char limits"). If it fails, that is a real finding (AGENTS §12.1, D4).

Other functions that need tests (by name): `ar/escape-key`, `window-toggle-split-direction`
(2×2 layouts), `ar/evil-open-below-smart` / `…above-smart` (comment line vs
code line; Python `#`, Elisp `;;`, C `//`), `ar/elec-pair-multi-char-prose`
(`**`→`****` with cursor between; no-fire in Org src and Markdown code),
`ar/electric-indent-words-fn`, `ar/exclude-org-agenda-files-p`,
`ar/fold-*` (hideshow vs treesit-fold vs vimish routing), `ar/org--icon`
(batch ⇒ empty string), `ar/capture-*`, `my/ensure-*` (temp dir),
`ar/workspace-*`, `ar/project-*`, `ar/ghostel-toggle` (no ghostel buffer ⇒
calls `ghostel`), `ar/popper-close-window-hack`, `ar/byte-compile-stale-modules`
(staleness logic with a temp `ar/bytecode-dir`, stubbing `start-process`).

### Coverage

For helper-dense files use `testcover` (statement/branch coverage of
instrumented `defun`s):

```elisp
(require 'testcover)
(testcover-start "lisp/core-config.el")         ; instruments & loads the file
(ert-run-tests-batch "ar-test/core")            ; run tests that call the helpers
(testcover-mark-all "lisp/core-config.el")      ; mark unhit forms in the buffer
;; then list regions with face `testcover-nohits` (or inspect with M-x describe-text-properties)
```

`testcover` only covers forms evaluated through it; configuration forms with
side effects at load time are covered by the boot test, and `use-package`
bodies by S4. Record in the ledger which mechanism verified each form.

---

## S8 — Debugging errors

### 8.1 Get the real backtrace

| Situation | Command |
|---|---|
| Error during startup (batch) | `ar_batch …` — `backtrace-on-error-noninteractive` is t by default; keep full stderr |
| Error during startup (interactive) | `emacs --debug-init --init-directory …`; `debug-on-error` t |
| Error swallowed by `ignore-errors`/`condition-case` | `--eval '(setq debug-on-signal t debug-ignored-errors nil)'` (noisy; combine with a narrow `debug-on-error` list) |
| A message that precedes the problem | `(setq debug-on-message "REGEX")` |
| Hang / infinite loop (interactive) | `kill -USR2 $(pgrep -n emacs)` ⇒ enters the debugger with a Lisp backtrace |
| Hang in C | `gdb -p PID -batch -ex bt` then `source src/.gdbinit` + `xbacktrace` if you have the Emacs source |
| Keyboard stuck | `C-g C-g`; `(toggle-debug-on-quit)` then `C-g` in the loop |
| Error in a timer / process filter | those print `Error running timer: …`; wrap with `(setq debug-on-error t)` or `(trace-function 'FN)` |

### 8.2 Classify

`void-function X` ⇒ package not installed/loaded or autoload missing
(check `ar/packages`, `straight--build-dir`, `(locate-library "X")`).
`void-variable X` ⇒ referenced before `defvar`/load; check module order.
`wrong-type-argument` after an upgrade ⇒ package API drift (S10).
`Cannot open load file` ⇒ `load-path` (bytecode cache, S12) or missing build.
`Symbol's function definition is void: lsp-…` ⇒ lsp-mode built with a
different plist setting or not loaded.

### 8.3 Module-level bisection (order-dependent ⇒ truncate the list)

```bash
ALL="core-config editor-config ui-config completion-config org-config dev-config vc-projects-config keybinds-config"
for n in 1 2 3 4 5 6 7 8; do
  M=$(echo $ALL | cut -d' ' -f1-$n)
  if ar_env emacs -Q --batch --init-directory "$AR_SB/cfg" \
       --eval "(defvar ar-modules '($M))" \
       -l "$AR_SB/cfg/early-init.el" -l "$AR_SB/cfg/init.el" >"$AR_SB/b$n.log" 2>&1
  then echo "prefix $n ok"; else echo "prefix $n FAILS ($(echo $M | awk '{print $NF}'))"; break; fi
done
```

(`init.el`'s own `defvar ar-modules` is a no-op once the variable is bound.)

### 8.4 Form-level bisection — continue-on-error loader

`load` stops at the first error; this evaluates each top-level form and
reports all failures with line numbers:

```elisp
(defun ar-debug-load-forgiving (file)
  (with-temp-buffer
    (insert-file-contents file)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (goto-char (point-min))
    (let (errors)
      (while (progn (forward-comment (buffer-size)) (not (eobp)))
        (let ((line (line-number-at-pos)) (form (read (current-buffer))))
          (condition-case err (eval form t)
            (error (push (list (file-name-nondirectory file) line (car-safe form)
                               (car-safe (cdr-safe form)) err) errors)))))
      (nreverse errors))))
```

Note `load-file-name`/`current-load-list` differ from a real `load`; forms
using `load-file-name` need a manual `let`.

### 8.5 Interactive debugging tools

- `edebug-defun` on a function (`C-u C-M-x` for the debugger-on-entry), then
  trigger it by keys in tmux/GUI; `edebug-on-error`.
- `(trace-function 'ar/lsp-ensure-maybe)` / `trace-function-background` for
  call logs; `untrace-all` afterward.
- `M-x debug-on-entry RET FUNCTION`; `cancel-debug-on-entry`.
- Introspection: `(symbol-file 'f)`, `(advice-mapc (lambda (f p) (print f)) 'target)`,
  `(get 'var 'standard-value)`, `(get 'var 'custom-requests)`,
  `(local-variable-p 'v)`, `(assoc "feature" after-load-alist)`,
  `(seq-filter (lambda (h) (not (symbolp h))) prog-mode-hook)` (anonymous hooks),
  `M-x list-timers`, `M-x view-lossage`, `M-x describe-key`, `(current-active-maps)`,
  `(minor-mode-key-binding KEY)`, `(evil-state-property evil-state :keymap)`.
- Record a session for later analysis: `open-dribble-file`.

### 8.6 Config-vs-package arbitration

1. Reproduce with the full config ⇒ failing.
2. `emacs -Q` + straight build dirs on `load-path` (S10.3) ⇒ failing?
   Yes ⇒ package bug. No ⇒ config interaction: bisect with 8.3/8.4, then
   disable suspects (`--eval '(setq SUSPECT-var nil)'`, `(advice-remove …)`,
   `(remove-hook …)`).

---

## S9 — Performance, startup and hangs (T9)

### Startup

```bash
# wall time of a full boot in batch (hooks that need a frame excluded)
( time ar_batch --eval '(kill-emacs 0)' ) 2>&1 | tail -3
# how many features load eagerly; compare to the saved baseline
ar_batch --eval '(princ (mapconcat #'"'"'symbol-name (sort (copy-sequence features) #'"'"'string<) "\n"))' > "$AR_SB/features.now"
diff test/features.baseline "$AR_SB/features.now" || echo "eager-feature set CHANGED"
```

Per-block cost: set before `init.el`: `--eval '(setq use-package-compute-statistics t)'`,
then `M-x use-package-report` (live) or print `use-package-statistics` in
batch. Interactive: `emacs-init-time`, `(list gcs-done gc-elapsed)`; startup
GC budget (early-init comment): ≤ 1 collection.

Measure `setopt`/`custom-load-symbol` regressions: diff `features` before and
after a candidate edit (AGENTS §4.2 #8).

### Runtime

- `M-x profiler-start RET cpu+mem`, reproduce, `M-x profiler-report`.
- `elp`: `(elp-instrument-package "ar/")` then `(elp-results)`.
- Redisplay/lag: `M-x toggle-debug-on-quit`; check `jit-lock-defer-time`
  (0.05), `long-line-threshold`, `bidi-inhibit-bpa`; `M-x font-lock-debug`-style
  checks: `(setq font-lock-support-mode nil)` to isolate.
- LSP latency: `lsp-log-io` t (temporarily) + `lsp-toggle-trace-io`; `read-process-output-max`
  is already 4 MiB; confirm booster is in the process command line:
  `(process-command (lsp--workspace-cmd-proc (car (lsp-workspaces))))`.
- GC: `(setq garbage-collection-messages t)` temporarily; remember the
  minibuffer hook doubles the threshold and collects on exit.
- Memory leaks in a daemon: `(memory-use-counts)`, `M-x memory-report`,
  `buffer-terminator` settings (30 min inactivity, 10 min interval).

---

## S10 — Debugging packages (built-in and external)

### 10.1 Locate the code that is actually running

```elisp
(symbol-file 'SYMBOL)                    ; file that defined it
(locate-library "PKG")                   ; first match on load-path (.elc or .el!)
(find-library "PKG") / (find-function 'FN) / (find-variable 'VAR)
(advice-mapc (lambda (f p) (message "%S %S" f p)) 'FN)
(let ((fn (symbol-function 'FN))) (list (subrp fn) (byte-code-function-p fn)
      (and (fboundp 'native-comp-function-p) (native-comp-function-p fn)) (autoloadp fn)))
(seq-filter (lambda (e) (string-match-p "PKG" (car e))) load-history)
```

External packages: checkout `var/straight/repos/<repo>`, build
`var/straight/build/<pkg>` (compiled copies, autoloads file
`<pkg>-autoloads.el`). Built-ins: Emacs' own `lisp/` (+ `.eln` in the native
cache). Different layers can disagree: source newer than `.elc`
(`load-prefer-newer` helps), `.eln` older than `.elc`, straight build older
than repo (`straight-check-for-modifications` is `check-on-save`, so edits
made *outside* Emacs are **not** noticed — run `straight-rebuild-package`).

### 10.2 straight.el toolbox (sandbox only for anything mutating)

| Need | Command |
|---|---|
| Where is it | `straight-visit-package`, `straight-visit-package-website` |
| Recipe as straight sees it | `(straight-get-recipe 'PKG)` |
| Rebuild after local edits or a plist change | `M-x straight-rebuild-package` (e.g. `lsp-mode`) |
| Is the repo clean/valid | `M-x straight-check-package`, `straight-normalize-package` |
| Process output of clone/build | buffer `*straight-process*` |
| Upgrade for diagnosis (**sandbox**) | `straight-pull-package`, `straight-fetch-package` |
| Install failure triage | the init error lists each failing entry; reproduce with `(straight-use-package 'ENTRY)` and read `*straight-process*` |

Recipe pitfalls specific to this manifest: `evil-ts-obj` needs
`:files ("lisp/*.el" "README.org")`; `ws-butler` is served from a NonGNU ELPA
mirror branch with `:local-repo "ws-butler"` and a **pin**; `denote-merge` and
`org-block-wrap` are git-only (GitHub / GitLab); `projection-dape` is excluded
by the MELPA `:files` spec, so no such command exists; `flycheck` is not
installed (lsp diagnostics are flymake). Built-ins are declared
`:type built-in` to stop straight rebuilding bundled libraries — if a
package demands a newer `transient`/`org`/`project`, that is where
version-skew bugs show up.

### 10.3 Minimal reproducer without the config (`emacs -Q` + straight builds)

```bash
BUILD="$AR_SB/cfg/var/straight/build"
ar_env emacs -Q --batch \
  --eval "(dolist (d (directory-files \"$BUILD\" t \"\\\\`[^.]\")) (add-to-list 'load-path d))" \
  --eval "(require 'PKG)" -l repro.el
```

For a GUI/TTY repro replace `--batch` with `-nw`. If it reproduces here, it
is upstream: record `emacs-version`, the package version header
(`(lm-version (locate-library "PKG"))` after `(require 'lisp-mnt)`), and
`git -C $AR_SB/cfg/var/straight/repos/PKG rev-parse HEAD`.

### 10.4 Bisect an upstream regression with git

```bash
cd "$AR_SB/cfg/var/straight/repos/PKG"
git bisect start BAD GOOD
git bisect run sh -c '
  cd "'"$AR_SB"'/cfg" &&
  ( . tools/ar-env.sh; AR_SB="'"$AR_SB"'";
    ar_batch --eval "(straight-rebuild-package \"PKG\")" >/dev/null 2>&1 &&
    ar_batch -l repro.el )'
```

(`straight-check-for-modifications` is `check-on-save`, hence the explicit
rebuild each step.) `lsp-mode` rebuilds inherit `LSP_USE_PLISTS=true` from
`early-init.el` — that is why plist changes require a rebuild.

### 10.5 Patching without forking

Order of preference: (1) config option; (2) `define-advice`/`advice-add` with
a comment naming the upstream issue and the version it targets; (3) a
`straight` recipe override (`:fork`, `:branch`, `:pin`); (4) upstream PR.
Never edit the checkout in the real tree. This config already does (2) in
several places (`ar/posframe--isolate-fringe`, `cape-wrap-nonexclusive`
around capfs, booster advice, `popper` hack) — follow that style.

### 10.6 Built-in package specifics

- `use-package` (built-in, 2.4.5 per comments): `(pp (macroexpand-all FORM))`
  is the ground truth for `:after`/`:defer`/`:hook` semantics.
- `project`: treat the value of `(project-current)` as opaque — always go
  through `project-root`; inspect `project-find-functions` to see which
  backend answered.
- `treesit`: see Tree-sitter runbook.
- `flymake`: `flymake-diagnostic-functions`, `flymake-running-backends`,
  `flymake-reporting-backends`, `flymake-log-level`, `*Flymake log*` buffer.
- `org`: the bundled version; use `org-version`, `org-lint`,
  `(setq org-element--cache-self-verify t)` temporarily (internal),
  `org-element-cache-reset`.
- `transient`: bundled; a `transient-define-prefix` error appears at *load of
  transient*, not at module load, because the editor/org/vc modules define
  prefixes inside `with-eval-after-load 'transient`.
- `which-key`, `editorconfig`, `use-package` are built into 31.1 — there is
  no package in the manifest to rebuild.
- Compare behaviour with `emacs -Q` (no config) to separate built-in bugs from
  config effects; read the actual source via `find-library`, not memory.

---

## S11 — Subsystem runbooks (T7)

Format: **smoke test → probes → common causes**. All commands inside the
sandbox. "(tmux)" = drive with S6.

### Evil / general / evil-collection
- Smoke: (tmux) `ciw`, `dd`, `u`/`C-r` (undo-fu), `gcc` (commentary), `gx` (exchange),
  `ysiw)` (surround), `gl` (lion), `o`/`O` on a comment line.
- Probes: `evil-state`, `(key-binding (kbd "SPC"))`, `evil-collection-mode-list`
  vs buffer's major mode, `(evil-initial-state 'MODE)`, `general-describe-keybindings`.
- Causes: `evil-want-keybinding` must be nil before load (set in `:init`);
  minibuffer integration is intentionally off (`evil-want-minibuffer nil`);
  text-object key collisions: `a` is bound twice (evil-args then
  evil-textobj-tree-sitter `parameter/call`) — the later definition wins;
  `evil-numbers` is deliberately unbound.

### Completion stack (vertico, orderless, marginalia, consult, embark, corfu, cape)
- Smoke: `M-x`, `SPC f f`, `SPC s s` (consult-line), `SPC s p` (needs `rg`), Embark `C-.`‑style
  act (check actual key in `completion-config.el`), in a Python buffer type 2+ chars ⇒ corfu popup after 0.24 s.
- Probes: `completion-styles`, `completion-category-defaults`,
  `completion-at-point-functions` (buffer-local + global order), `corfu-mode`,
  `(minibuffer-depth)`, `vertico-multiform` commands, `consult--*` previews.
- Causes: corfu is *disabled in the minibuffer* and routed through
  `consult-completion-in-region`; lsp capf is made non-exclusive by advice;
  `lsp-capf` style forced to `orderless` by `ar/lsp-completion-orderless` (hook order!);
  corfu `RET` unbound on purpose; `TAB` cycles; `tab-always-indent` is `complete`.
  Missing `rg` ⇒ `grep` block no-ops (`when-let*` guard).

### LSP (lsp-mode, booster, eldoc-box, consult-lsp, flymake)
- Smoke **(G4-style, batch)**: create a tiny C project and observe the mirror
  variable the config defines:

  ```bash
  mkdir -p "$AR_SB/proj" && printf 'int main(void){return 0;}\n' > "$AR_SB/proj/a.c"
  ar_batch --eval '(progn (find-file (expand-file-name "proj/a.c" (getenv "AR_SB")))
    (c-ts-mode)        ; runs prog-mode-hook => ar/lsp-ensure-maybe (batch branch)
    (let ((n 0)) (while (and (null lsp--workspaces) (< n 150)) (accept-process-output nil 0.2) (cl-incf n)))
    (message "LSP-WORKSPACES=%S" lsp--workspaces))'
  ```
  `ar/lsp-ensure-maybe` takes the batch branch (`lsp-workspace-folders-add` +
  direct `lsp`). Empty after 30 s ⇒ server missing (`clangd`) or init failed.
- Interactive: (tmux) open a source file, `SPC c a`, `SPC c r`, `K`, `] e`, `SPC c e`.
- Probes: `M-x lsp-describe-session`, `lsp-workspace-restart`, `lsp-workspace-shutdown`,
  `lsp-toggle-trace-io` (+ `lsp-log-io t`), buffers `*lsp-log*` and per-server
  stderr buffers, `lsp-diagnostics-provider` (:flymake), `flymake-diagnostic-functions`
  containing lsp's backend, `lsp-completion-mode-hook`, `lsp-use-plists`,
  `lsp-completion-plist-value-when-compiled`, `(executable-find "emacs-lsp-booster")`,
  server binaries on `exec-path` (clangd, pyright/basedpyright, …), `envrc` effect on `exec-path`.
- Causes: **plist mismatch** (rebuild lsp-mode); `lsp-deferred` waits for a
  visible buffer; excluded modes list; popper pattern `^\*lsp.*\*$` turns lsp
  buffers into popups; Python server is system-side (not configured);
  booster message `Using emacs-lsp-booster for …` absent ⇒ remote
  `default-directory`, `lsp-use-plists` nil, or no binary; Emacs 31 native JSON
  makes bytecode translation mostly moot.

### Tree-sitter
- Smoke: open `.py`, `.c`, `.json`, `.org`?; `(treesit-language-available-p 'python)`.
- Probes: `treesit-enabled-modes`, `major-mode-remap-alist`, `treesit-extra-load-path`
  (`var/tree-sitter/`), `treesit-language-source-alist`, `treesit-inspect-mode`,
  `treesit-explore-mode`, `(treesit-language-abi-version 'LANG)` vs
  `(treesit-library-abi-version)`, `treesit-font-lock-level` (4).
- Causes: first visit auto-installs (network, `git`, C compiler); ABI mismatch
  between grammar and libtree-sitter; `bash`/`css` grammars missing in source
  alist; `evil-textobj-tree-sitter` queries require the right language names;
  `treesit-fold` is blocked in Lisp modes by guard advice.

### Formatting / environment (apheleia, ruff, envrc, editorconfig)
- Smoke: Python file, `SPC c f`; save triggers formatting; `.envrc` project
  changes `exec-path`.
- Probes: `apheleia-mode-alist` entries for `python-mode`/`python-ts-mode`,
  `apheleia-formatters`, `(executable-find "ruff")`, `apheleia-hide-log-buffers`
  (set nil to see logs), `envrc-global-mode`, `envrc--status`-style buffer `*envrc*`.
- Causes: `ruff` absent; direnv not allowed (`direnv allow`); ghostel strips
  `DIRENV_*` on spawn by design.

### Debugging (dape)
- Smoke: Python with `debugpy`, `SPC d S`, `SPC d b`, `SPC d c`, `SPC d n`.
- Probes: `dape-configs`, adapter binaries on `PATH`, `dape-default-breakpoints-file`
  (`var/dape/breakpoints.eld`), `dape-request-timeout` (5 s), the debug/events
  buffer of your dape version (`M-x apropos-variable RET dape-debug`),
  display rules for `dape-repl-mode` etc., `dape-cwd-function` ⇒ project root.
- Causes: timeout 5 s too short for slow adapters; `dape-key-prefix` nil
  (no `C-x C-a`); breakpoints load happens on the *first dape load*; `projection-dape` absent.

### Snippets / spelling
- yasnippet: expansion in prog/text/conf/comint; personal dir
  `<root>/snippets` is added to `yas-snippet-dirs`; saving a snippet file there
  reloads all (`ar/yas-reload-snippets-on-save`); `yasnippet-capf` in
  `completion-at-point-functions`.
- jinx: needs a built module + enchant dictionaries (`en`); disabled in the modes in
  `ar/jinx-disabled-modes`; `jinx-local-words` injected. Probe: `(featurep 'jinx-mod)`, compile
  output buffer on first use.

### Org / Denote
- Smoke (sandbox docs dir): `SPC X` capture each template; `SPC o a` agenda;
  create a Denote note; silo creation `my/create-denote-project-silo`.
- Probes: `my/org-directory`, `org-directory`, `org-agenda-files` all exist,
  `org-capture-templates` (iterate: every `file+headline` target exists — seeds in
  `agenda/*.org`), `denote-directory` per silo via `.dir-locals.el`,
  `org-id-locations-file` under `var/org/`, `org-persist-directory`,
  `org-super-agenda` groups, `org-habit` graph, `org-modern` faces & the Noto Sans
  fontset fallbacks, transclusion bridge, `embrace-org-mode-hook`, `ar/org-template-transient`.
- Checks: `(org-lint)` on seeded files; `(with-temp-buffer (org-mode) …)` for
  babel helpers (`ar/org-babel-remove-result-one-or-many`); capture dispatch
  `ar/capture-dispatch` with stubs for `read-string`/clipboard.
- Causes: org loaded eagerly — never `with-eval-after-load` dependent on org
  being absent; module load creates and seeds files (use the sandbox docs dir);
  org-element cache errors ⇒ `org-element-cache-reset`; Denote 4.x front-matter
  placeholders (5 `%s`) must match `denote-org-front-matter`.

### VC (magit, forge, diff-hl, git-timemachine, blamer)
- Smoke: create a repo in `$AR_SB/proj`, `SPC g g`, stage/commit, diff-hl fringe
  marks, blame, timemachine.
- Probes: `magit-process-buffer`, `magit-git-executable`, `git --version`,
  `vc-handled-backends` `(Git)`, forge database under `var/` (needs sqlite
  support), auth for forge/`auth-source`, `magit-todos-mode` in status buffer.
- Causes: gravatars only with ImageMagick; `forge` bindings disabled
  (`forge-add-default-bindings nil`) ⇒ use `forge-dispatch`; TRAMP + Git only.

### Files, projects, workspaces (dired, dirvish, treemacs, project, projection, bufferlo)
- Smoke: `SPC o /`, `SPC o p`, `SPC p …`, workspace create/switch/kill, `SPC q s/l`.
- Probes: `bufferlo-mode`, `bufferlo-exclude-buffer-filters` (starts as `(".*")`),
  `consult-buffer-list-function`, `tab-bar-mode`, `(project-current)`,
  `ar/workspace-*`, ibuffer groups via `ibuffer-vc`, `dirvish` subtree and
  transient `ar/dirvish-transient`.
- Causes: bufferlo whitelist only `*scratch*`/`*Messages*` — other buffers
  vanish from `consult-buffer` unless added to the workspace; bufferlo
  loads/saves bookmarks at startup/exit (sandbox!); treemacs/dirvish buffers
  excluded after their packages load.

### Terminal (ghostel / evil-ghostel / eshell)
- Smoke: `SPC o t`, `SPC o T`, run `ls`, `vim` from eshell opens in ghostel,
  compilation output renders via `ghostel-compile-global-mode`.
- Probes: `(featurep 'ghostel)`, `*Messages*` for native-module load errors,
  `display-buffer-alist` rule (bottom 25 %), `ghostel-eval-cmds` contains the magit entry,
  `ghostel-pre-spawn-hook` strips `DIRENV_*`, `evil-ghostel-mode` in buffer.
- Causes: native module not built/found ⇒ BLOCKED with message; popper treats
  terminals as non-closable by `C-g`.

### UI (themes, fonts, icons, modeline, folding, child frames)
- Probes (GUI): fonts via `find-font`; `custom-enabled-themes`; faces
  `(face-attribute 'line-number :foreground)` vs hex in core-config (a theme
  re-asserts its own faces); `doom-modeline-mode`; `nerd-icons` font installed
  (`nerd-icons-install-fonts`); `solaire-global-mode`; `colorful-mode`,
  `rainbow-delimiters`, `indent-bars` guards; `hideshow` / `treesit-fold` /
  `vimish-fold` routing in `ar/fold-*`; `ar/hide-modeline-mode` caches the three
  line formats buffer-locally.
- Causes: `ar/setup-fonts` runs only when `(not noninteractive)` and again on
  `server-after-make-frame-hook` — GUI client frames in a TTY-started daemon
  get fonts only via that hook.

### Platform (PGTK/Wayland)
- Probes: `(featurep 'pgtk)`, `XDG_SESSION_TYPE`, `x-gtk-use-system-tooltips` nil,
  `use-dialog-box` nil, `(pgtk-use-im-context nil)` applied per new frame,
  `pixel-scroll-precision-mode`, `scroll-margin` 0.
- Causes: hangs on hover (system tooltips), daemon freeze on GTK dialogs, input latency (IM context).

### Persistence & state (undo-fu-session, recentf, savehist, saveplace)
- Probes: files created under `var/`; `undo-fu-session-compression`
  (`zst` if `zstd` on PATH, else `gz`, else nil); recentf exclusions
  (TRAMP, own state files, org agenda files via cache); `savehist-additional-variables`.
- Causes: cache refresh timers and agenda cache staleness; incompatible-modes lists.

### TRAMP
- Config touches TRAMP only through routing (persistency file, backup dirs),
  Git-only VC, `recentf` exclusion, `super-save-remote-files nil`,
  ghostel TRAMP integration. Probes: `tramp-verbose` 6, `*tramp/…*` buffers
  (hidden by bufferlo), `file-name-handler-alist` restored after startup
  (it is nil'd during startup — if it stays nil, remote files silently break).

---

## S12 — Reload, idempotence, bytecode cache (T8)

### 12.1 Reproduce suspected defect D1 (reload vs `var/bytecode`)

```bash
# Build the cache synchronously (the config does it in an async subprocess after 8 idle seconds)
ar_batch --eval '(progn
  (make-directory ar/bytecode-dir t)
  (let ((byte-compile-dest-file-function
         (lambda (f) (expand-file-name (concat (file-name-nondirectory f) "c") ar/bytecode-dir))))
    (dolist (m ar-modules)
      (byte-compile-file (expand-file-name (format "lisp/%s.el" m) ar/config-dir)))))'
# Fresh process: cache is now "fresh" so var/bytecode/ is prepended to load-path
ar_batch --eval '(progn
  (message "bytecode-on-load-path=%S" (and (member ar/bytecode-dir load-path) t))
  (message "locate-library => %S" (locate-library "core-config"))
  (condition-case e (progn (reload-init-file) (message "RELOAD OK"))
    (error (message "RELOAD ERROR: %S" e))))'
```

If `locate-library` returns `…/var/bytecode/core-config.elc` and the reload
errors with a missing `core-config.el`, D1 is confirmed. Candidate fix (apply
only after confirming, with a regression test): resolve the **source** with
`(locate-file (concat (symbol-name mod) ".el") (list (expand-file-name "lisp" ar/config-dir)))`
instead of `locate-library`, and either implement the "remove stale .elc"
the comment promises or correct the comment.

### 12.2 Idempotence — hooks and advice must not grow

```elisp
(defun ar-test--hook-lengths ()
  (let (acc)
    (mapatoms (lambda (s) (when (and (boundp s) (string-match-p "\\(-hook\\|-functions\\)\\'" (symbol-name s))
                                     (listp (symbol-value s)) (symbol-value s))
                            (push (cons s (length (symbol-value s))) acc))))
    (sort acc (lambda (a b) (string< (car a) (car b))))))

(defun ar-test--advice-counts ()
  (let (acc)
    (mapatoms (lambda (s) (when (and (fboundp s) (advice--p (symbol-function s))) ; internal
                            (let ((n 0)) (advice-mapc (lambda (&rest _) (cl-incf n)) s)
                                 (push (cons s n) acc)))))
    (sort acc (lambda (a b) (string< (car a) (car b))))))

(ert-deftest ar-test/reload/idempotent ()
  (let ((h0 (ar-test--hook-lengths)) (a0 (ar-test--advice-counts)))
    (reload-init-file) (reload-init-file)
    (let ((h1 (ar-test--hook-lengths)) (a1 (ar-test--advice-counts)))
      (should (null (cl-set-difference h1 h0 :test #'equal)))
      (should (null (cl-set-difference a1 a0 :test #'equal))))))
```

Expect failures from anonymous lambdas (defect D3): corfu (`save-buffer`
advice), cape, dape, ghostel, `compilation-mode-hook`. Fix by naming the
functions (`defun` + `#'name`) so `add-hook`/`advice-add` de-duplicate.
Note `reload-init-file` does not undo removed hooks/packages — a restart is
still needed for those (documented in its docstring).

### 12.3 Cold-start parity

After any change: restart and rerun S3 + S4 for touched blocks. A reload
that passes while a cold start fails (or vice-versa) means order dependence
or stale state — treat as a bug.

### 12.4 Stale artifacts checklist

`var/bytecode/*.elc` (and whether the dir is on `load-path`), `var/eln-cache`
(and the `<root>/eln-cache` symlink target), `var/straight/build/*` vs `repos`,
`etc/custom.el` overriding config, `.dir-locals.el` in Denote silos.
When in doubt: new sandbox, `rm -rf $AR_SB/cfg/var/bytecode $AR_SB/cfg/var/eln-cache`.

---

## S13 — Fix → verify → report workflow

1. Reproduce (S0 + minimal command). Save the command in `test/repro/<id>.sh`.
2. Classify (AGENTS §9) and locate the owner (S10.1).
3. Write the failing ERT test first (S7). Confirm red.
4. Make the smallest change in the right layer. Keep the commentary style.
5. Gates: T0 → T1 (zero warnings) → T2 (no duplicates, ledger updated) →
   T3 → T4/T5 for touched and neighbouring forms → T8 → T9 if perf-adjacent.
6. Revert the fix and confirm the test is red again (proves the test).
7. Report in the AGENTS §13 format; attach the ledger diff and list
   **BLOCKED** items with exact missing prerequisites (display, network,
   binary, version).
8. Update AGENTS.md / SKILLS.md if the change alters an invariant, a trigger,
   or a command used here.

### Quick reference — one-liners

```bash
. tools/ar-env.sh && ar_sandbox_create            # S0
emacs -Q --batch -l tools/ar-check-syntax.el --eval '(ar-check-syntax (file-expand-wildcards "lisp/*.el"))'   # T0
ar_compile_gate                                   # T1 / G2
ar_batch -l ert -l test/ar-test-boot.el -f ert-run-tests-batch-and-exit        # T3
ar_batch -l ert -l test/ar-test-keys.el -f ert-run-tests-batch-and-exit        # T5
ar_tty_start; ar_keys Space; sleep 1.2; ar_screen # T6
```
