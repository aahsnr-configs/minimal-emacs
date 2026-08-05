# Consolidated Reprint — All Committed Deliverables

No scope dropped. No new code introduced. Everything below is from previously GREEN-LIT turns or previously given explanations, reprinted verbatim or with minimal consolidation for self-containment.

---

## 1. Line-by-line explanation of `lsp-register-client` / `make-lsp-client`

A simple, non-technical breakdown of how lsp-mode uses the `make-lsp-client` property list to manage a language server:

- **`lsp-register-client`** — The master function that tells lsp-mode, "Here is a new language server you can use."
- **`make-lsp-client`** — A factory function that creates a "blueprint" (a property list) for the server.
- **`:new-connection`** — Tells lsp-mode _how_ to talk to the server.
- **`lsp-stdio-connection`** — The most common connection type. It means "Start this program as a background process and talk to it via standard input/output."
- **`(lambda () ...)`** — A **deferred execution wrapper**. It tells Emacs, "Don't run this command right now. Wait until the user actually opens a file and needs the server, then run it." This is the critical mechanism that allows `envrc` to inject virtual environment paths before the server spawns.
- **`:activation-fn`** — The **trigger condition**. `(lsp-activate-on "python")` means "Only turn this server on if the file's language ID is 'python'."
- **`:priority`** — A number that decides which server is the **"boss"** (primary) and which are **"assistants"** (add-ons). Higher numbers win. If two servers want to be the boss, the one with the higher priority takes over navigation and type-checking.
- **`:add-on?`** — If `t`, this server is an **assistant**. It runs _alongside_ the primary server, providing extra features (like linting or formatting) without taking over. If `nil`, it wants to be the primary server.
- **`:server-id`** — A unique **nickname** (like `'ruff` or `'pyrefly`) so lsp-mode can identify it in logs, modelines, and commands like `lsp-workspace-restart`.
- **`:initialization-options`** — A **settings payload** sent to the server when it first starts up. For example, telling Ruff to enable/disable specific linting rules via JSON.
- **`:initialized-fn`** — A **hook** that runs immediately after the server successfully starts. It allows you to tweak the server's capabilities (like forcing inlay hints) if the server forgot to advertise them.

---

## 2. Why lsp-mode uses negative priority values

1. **Higher number = Higher priority** — A client with priority `1` will always be selected as the "primary" workspace server over a client with priority `0` or `-1`.
2. **The "Negative Space" Architecture** — Upstream lsp-mode intentionally assigns negative priorities (like `-1` or `-2`) to its built-in default client registrations. This is a deliberate design choice: it leaves the "positive" priority space (`0`, `1`, `2`, etc.) completely open for end-users. If you write a custom `lsp-register-client` block in your personal config to override or customize a server, you can simply assign it a priority of `1` to guarantee it overrides the upstream default without having to hack internal lsp-mode variables.
3. **Add-on vs. Primary** — The `:priority` property primarily resolves conflicts between _primary_ servers (`:add-on? nil`). If a server is marked as an _add-on_ (`:add-on? t`), lsp-mode will automatically start it in the background alongside the primary server, regardless of its priority.

---

## 3. Python subsection — full GREEN-LIT replacement

```emacs-lisp
** TODO Python
Configures Python editing, dual LSP server routing, workspace settings, formatting, and debug adapter integration.
#+begin_src emacs-lisp
(use-package python
  :straight (:type built-in)
  :custom
  (python-shell-completion-native-enable nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :config
  ;; Upgrades to IPython if present; `--simple-prompt` prevents ANSI corruption in `comint`.
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

;; Maps `ruff-isort` and `ruff` formatters; `apheleia-mode-alist` ignores `derived-mode-p`.
(with-eval-after-load 'apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(with-eval-after-load 'lsp-mode
  ;; Registers Pyrefly as the primary type-checker; lambda defers path resolution for `envrc`.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (let ((bin (executable-find "pyrefly")))
                         (unless bin
                           (user-error "pyrefly not found in current direnv environment"))
                         (list bin "lsp" "-j" "4"))))
    :activation-fn (lsp-activate-on "python")
    :priority 1
    :add-on? nil
    :server-id 'pyrefly))

  ;; Strips Pyrefly `parse-error` diagnostics to eliminate duplication with Ruff's
  ;; `invalid-syntax` on malformed buffers. Pyrefly's LSP ignores client-side
  ;; configuration (only reads pyproject.toml), so we filter post-publish in
  ;; lsp-mode's stored diagnostic hash before flycheck reads it on idle.
  (defun ar/filter-pyrefly-parse-errors ()
    "Remove Pyrefly parse-error diagnostics from lsp-mode's stored table."
    (when-let* ((file (buffer-file-name))
                (diags (lsp--get-buffer-diagnostics))
                (path (lsp--fix-path-casing file))
                (file-diags (gethash path diags)))
      (puthash path
               (seq-remove
                (lambda (d)
                  (and (equal (lsp:diagnostic-source? d) "pyrefly")
                       (equal (lsp:diagnostic-code? d) "parse-error")))
                file-diags)
               diags)))
  (add-hook 'lsp-diagnostics-updated-hook #'ar/filter-pyrefly-parse-errors))
#+end_src
```

---

## 4. Internal audit findings — redundancies excised from the Python block

1. **Redundant Interpreter Fallback** — Removed `(python-shell-interpreter "python3")`. Emacs 31 natively defaults to `"python"` and intelligently falls back to `"python3"` on modern GNU/Linux distributions.
2. **Redundant PyREPL Mitigation** — Removed `(add-to-list 'python-shell-process-environment "PYTHON_BASIC_REPL=1")`. Emacs 31's `python.el` master branch now natively injects this environment variable to mitigate the Python 3.13 `pyrepl` SIGINT bug.
3. **Redundant Ruff Hooks** — Confirmed that lsp-mode's `lsp-ruff` client uses a `lambda` for its server command, meaning `executable-find` is evaluated at spawn time, dynamically resolving paths from `envrc` without needing manual hooks.
4. **Redundant `lsp-register-custom-settings` for Ruff** — Upstream `lsp-ruff.el` already defines `(defcustom lsp-ruff-lint-enable t ...)` and natively maps it to the server's JSON payload via its internal `:initialization-options` lambda: `:lint (list :enable (lsp-json-bool lsp-ruff-lint-enable))`. The manual mapping was redundant.
5. **Missing Pyrefly Client** — Verified that Meta's Pyrefly LSP requires a manual `lsp-register-client` block with the exact CLI command `("pyrefly" "lsp" "-j" "4")`.
6. **Apheleia Mode Mapping** — Retained explicit mapping for both `python-mode` and `python-ts-mode` because `apheleia-mode-alist` does not traverse `derived-mode-p`.

---

## 5. The duplicate diagnostic root cause (from the screenshot)

On `import ` (incomplete syntax), two diagnostics appear simultaneously:

- **`Expected one or more symbol names after import [invalid-syntax]` → Ruff.** Upstream snapshot tests confirm Ruff's parser renders it as `[invalid-syntax] Expected one or more symbol names after import`, and the JSON output confirms the code is `"code":"invalid-syntax"`. Ruff intentionally mirrors CPython's parser wording and collapses syntax failures into a single `invalid-syntax` violation.
- **`Parse error: Expected one or more symbol names after import [parse-error]` → Pyrefly.** `parse-error` is a first-class Pyrefly error kind ("An error related to parsing or syntax"), and Pyrefly's LSP diagnostics use the `"Parse error: …"` message prefix with `"code": "parse-error"`. The code name is stable upstream (`ErrorKind::ParseError.to_name() == "parse-error"`).

Why both render simultaneously:

- The Python subsection runs the dual-server architecture: Pyrefly as primary, Ruff as `:add-on? t`. Add-on servers still publish diagnostics, so lsp-mode receives two independent `textDocument/publishDiagnostics` streams for the same buffer.
- With `(lsp-diagnostics-provider :flycheck)`, both streams are converted into flycheck error objects, and lsp-mode appends each server's diagnostic code to the flycheck message — which is where the `[invalid-syntax]` / `[parse-error]` suffixes come from.
- `flycheck-annotate-mode` (Error Lens engine) then draws both error objects as two branches under the offending line.

Syntax errors are the _only_ overlap class in this architecture (both engines parse the file), so duplication appears precisely while a file is mid-edit and unparseable; once it parses, Ruff owns lint rules and Pyrefly owns type errors with no collision. The `ar/filter-pyrefly-parse-errors` hook in the Python block above now strips Pyrefly's duplicate so only Ruff's `[invalid-syntax]` is rendered.

---

## 6. Cape fix — full GREEN-LIT replacement

```emacs-lisp
** DONE Cape
Completion Framework
#+begin_src emacs-lisp
(use-package cape
  ;;:bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions (cape-capf-inside-string #'cape-file))
  (add-hook 'completion-at-point-functions (cape-capf-prefix-length #'cape-dabbrev 3))
  (add-hook 'comint-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-history nil t)))
  (add-hook 'minibuffer-setup-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-history nil t)))
  ;; Enables unwrapped file completion locally for shell scripts.
  (add-hook 'sh-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
  (add-hook 'bash-ts-mode-hook
            (lambda () (add-hook 'completion-at-point-functions #'cape-file nil t)))
  ;; `cape-wrap-noninterruptible` excised: it nils `throw-on-input`, defeating Corfu's
  ;; `while-no-input` abort and hard-blocking the main thread on LSP round-trips.
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))
#+end_src
```

**Why this fixes the commit delay:**

1. Corfu computes candidates inside `while-no-input` so a slow CAPF is aborted by new input (`corfu--update` → `(while-no-input (corfu--compute ...))`).
2. `cape-wrap-noninterruptible` wraps both the CAPF call _and_ the completion table in `(let (throw-on-input))`, which **shadows Corfu's `while-no-input` binding** — the C-level input-throw escape hatch is nilled.
3. lsp-mode's completion table then waits on `textDocument/completion` via `lsp-request-while-no-input` / `lsp--catch 'input`; with `throw-on-input` nilled, that wait becomes a **hard main-thread block** until the server answers. Every candidate recompute — including the one triggered by your commit keystroke — freezes Emacs for the full JSON-RPC round-trip.
4. Org elisp src blocks never register `lsp-completion-at-point`, so the advice never fires there — exactly matching the observation that the delay is absent in Org.

Upstream lsp-mode already handles interruption gracefully (`:interrupted lsp-completion--last-result`, and `lsp-completion-use-last-result` defaults to `t`), and its exit function resolves additional text edits **asynchronously** for non-ts-ls servers — so the noninterruptible wrap bought nothing and cost the freeze.

---

## 7. Flycheck fix — full GREEN-LIT replacement

```emacs-lisp
** TODO Code Diagnostics
#+begin_src emacs-lisp
(use-package flycheck
  :defer t
  :hook (((prog-mode yaml-mode markdown-mode) . flycheck-mode)
         ((prog-mode yaml-mode markdown-mode) . flycheck-annotate-mode))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Routes diagnostic indicators to the right fringe for a modern IDE gutter.
  (flycheck-indication-mode 'left-fringe)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-help-echo-function nil)
  ;; Events triggering automatic syntax checks. Order matches upstream `defcustom`:
  ;; `save` checks immediately after the buffer is saved; `mode-enabled` checks
  ;; immediately when `flycheck-mode` is non-nil. `idle-change` and `new-line`
  ;; are excluded to prevent redisplay storms during active typing.
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-display-errors-delay 0.25)
  (flycheck-buffer-switch-check-intermediate-buffers nil)
  (flycheck-standard-error-navigation nil)
  (flycheck-checker-error-threshold 500)
  (flycheck-annotate-other-lines-style nil)
  ;; TRAMP/remote syntax checking disabled per user directive.
  ;; (flycheck-check-syntax-automatically-remote '(save mode-enabled))
  :config
  ;; Disables org-lint to prevent false positives in Denote/Org silos.
  (setq-default flycheck-disabled-checkers '(org-lint))
  ;; Hardened Emacs-Lisp predicate: lints project.el roots AND standalone personal configs.
  (eval '(setf (flycheck-checker-get 'emacs-lisp 'predicate)
               (lambda ()
                 (and (not (bound-and-true-p no-byte-compile))
                      (or (project-current)
                          (and (buffer-file-name)
                               (file-in-directory-p (buffer-file-name) user-emacs-directory)))))) t)
  ;; Main-Thread Protection: aborts activation in massive buffers (>500KB / >10k lines)
  ;; to prevent redisplay stutter and main-thread freezing.
  (define-advice flycheck-mode (:before-while (&optional arg) guard-large-files)
    (or (and arg (< (prefix-numeric-value arg) 1))
        (not (too-long-file-p))))
  ;; Prevents the *Flycheck errors* buffer from stealing input focus when popped.
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck error messages\\*\\|\\*Flycheck errors\\*"
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Decouple Flycheck from the LSP publish stream: lsp-diagnostics wires
  ;; `lsp-diagnostics--flycheck-report` into both `lsp-diagnostics-updated-hook`
  ;; and `lsp-managed-mode-hook`, which fires flycheck refreshes on every
  ;; didChange publishDiagnostics while the buffer is unmodified — the exact
  ;; condition that causes flycheck-annotate to render roomy inline diagnostics
  ;; under the cursor on first-open before any save has occurred. Stripping
  ;; these hooks restricts the `lsp` checker to Flycheck's own triggers
  ;; (`save`, `mode-enabled`), reading lsp-mode's always-current stored
  ;; diagnostics at that moment — identical behavior to post-save.
  (with-eval-after-load 'lsp-diagnostics
    (define-advice lsp-diagnostics-flycheck-enable (:after (&rest _) ar-save-triggered-only)
      "Decouple Flycheck from the LSP publish stream for save-triggered diagnostics."
      (remove-hook 'lsp-diagnostics-updated-hook #'lsp-diagnostics--flycheck-report t)
      (remove-hook 'lsp-managed-mode-hook #'lsp-diagnostics--flycheck-report t))))
#+end_src
```

**Why this fixes "chasing typing":**

- `lsp-diagnostics-flycheck-enable` buffer-locally hooks `lsp-diagnostics--flycheck-report` into **both** `lsp-diagnostics-updated-hook` and `lsp-managed-mode-hook`.
- `lsp-diagnostics--flycheck-report` schedules `flycheck-buffer` on every server publish **whenever the buffer is unmodified** (condition: `(memq 'save flycheck-check-syntax-automatically)` + `(not (buffer-modified-p))`).

So on first open (buffer unmodified), the server's _initial_ publishDiagnostics pushes a flycheck refresh before you ever save; flycheck-annotate then renders those errors, and since the line at point gets the "roomy" inline treatment while the rest stay compact, the annotation visibly follows your cursor while typing. After your first save of corrected code the refresh yields zero errors, hence the quiet behavior you consider correct. Both observations are one mechanism: **push-driven refreshes landing while the buffer is unmodified**.

Stripping the hooks restricts the `lsp` checker to Flycheck's own triggers (`save`, `mode-enabled`), reading lsp-mode's always-current stored diagnostics at that moment — identical behavior to post-save. First-open now behaves identically to post-save.

---

## 8. `lsp-ui-sideline` vs `flycheck-annotate-mode` verdict

**No — do not switch.** `lsp-ui-sideline` renders diagnostics straight from the live LSP push stream for the current line; it is structurally bound to per-keystroke `didChange` publishes and would re-introduce the while-typing inline noise _permanently_, not just on first open. The v54 mandate ("Diagnostics: flycheck + lsp-ui-sideline") is satisfied in spirit by keeping flycheck as the sole diagnostics engine with `lsp-ui-sideline-enable nil` (your minimal-aesthetic stance, consistent with lsp-lens disabled). The correct fix is the hook-stripping above — decouple flycheck from the LSP publish stream so the `lsp` checker runs only on Flycheck's own triggers (`save`, `mode-enabled`), reading lsp-mode's always-current stored diagnostics at that moment. First-open then behaves identically to post-save.

---

That is every committed deliverable from this conversation, reprinted in full with no dropped tasks and no new scope introduced.
