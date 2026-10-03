;;; dev-config.el --- IDE tooling: lsp-mode, flymake, dape, apheleia, treesit, yasnippet -*- lexical-binding: t; -*-

;;; Commentary:

;; The IDE module: tree-sitter grammar management, lsp-mode, flymake,
;; dape debugging, apheleia formatting, envrc, and YASnippet.

;;; Code:

;; Tree-sitter grammar management.  This block deliberately stays eager (as in
;; the old config): `treesit-enabled-modes' applies itself through a custom
;; :set function that pushes entries into `major-mode-remap-alist', and that
;; re-application only happens when treesit.el loads.  Deferring the require
;; would be a chicken-and-egg problem: the remap is what routes source files
;; to *-ts-mode in the first place, so treesit.el would never load and ts
;; modes would never activate.
(use-package treesit
  :if (treesit-available-p)
  :preface
  ;; Must precede :custom (load-order constraint, as in the old config).
  (defvar ar/treesit-grammar-dir
    (ar/var-file "tree-sitter/")
    "Target directory for compiled tree-sitter grammars.")
  :init
  ;; Ensures the grammar directory exists before any download occurs.
  (unless (file-directory-p ar/treesit-grammar-dir)
    (make-directory ar/treesit-grammar-dir t))
  :custom
  ;; First directory is the default install target for auto-installation.
  (treesit-extra-load-path (list ar/treesit-grammar-dir))
  ;; Silently installs missing grammars without prompting.
  (treesit-auto-install-grammar 'always)
  ;; Routes all supported file types to their tree-sitter modes.
  (treesit-enabled-modes t)
  :config
  ;; Maximum syntax highlighting detail level.
  (setq treesit-font-lock-level 4)
  ;; Registers non-built-in grammar sources (Emacs 31 ships typescript, toml,
  ;; yaml, dockerfile, rust).  No :commit pins on purpose — zero maintenance.
  (dolist (source '((python "https://github.com/tree-sitter/tree-sitter-python")
                    (bash "https://github.com/tree-sitter/tree-sitter-bash")
                    (c "https://github.com/tree-sitter/tree-sitter-c")
                    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                    (css "https://github.com/tree-sitter/tree-sitter-css")
                    (html "https://github.com/tree-sitter/tree-sitter-html")
                    (json "https://github.com/tree-sitter/tree-sitter-json")
                    (latex "https://github.com/latex-lsp/tree-sitter-latex")
                    (bibtex "https://github.com/latex-lsp/tree-sitter-bibtex")
                    (nix "https://github.com/nix-community/tree-sitter-nix")
                    (java "https://github.com/tree-sitter/tree-sitter-java")
                    (make "https://github.com/tree-sitter-grammars/tree-sitter-make")
                    (awk "https://github.com/BeagleFoot/tree-sitter-awk")
                    (org "https://github.com/milisims/tree-sitter-org")))
    (setf (alist-get (car source) treesit-language-source-alist) (cdr source))))

;; Language servers: lsp-mode.  `ar/lsp-ensure-maybe' starts lsp from
;; `prog-mode-hook' for every mode except the excluded list below; the
;; guard is hand-rolled because an exclusion list is not an lsp-mode feature
;; (build/design-lsp.md §1.1).  With `emacs --daemon', `lsp-deferred' defers
;; each server start until a buffer is first visible in a client frame, so
;; daemon startup itself stays LSP-free (design §8.4).  Batch runs (the G4
;; smoke gate) call `lsp' directly -- see the function's docstring for why
;; `lsp-deferred' can never start a server without windows.
(defcustom ar/lsp-excluded-modes
  '(emacs-lisp-mode lisp-interaction-mode lisp-mode
    makefile-mode makefile-ts-mode snippet-mode ron-mode)
  "Major modes in which lsp auto-start is never attempted.
Interactive starts go through `lsp-deferred', batch starts through
`lsp' (see `ar/lsp-ensure-maybe').
Same list the eglot-era `ar/eglot-excluded-modes' carried.
`makefile-ts-mode' is listed alongside `makefile-mode' because
`treesit-enabled-modes' routes Makefiles to it, and it does not
derive from `makefile-mode'."
  :type '(repeat symbol)
  :group 'lsp-mode)

;; Declared for the compiler, and before `ar/lsp-ensure-maybe' below: none of
;; these carries a ;;;###autoload cookie in lsp-mode.el (unlike `lsp' and
;; `lsp-deferred', whose autoloads straight registers during bootstrap and
;; which therefore need no declaration), so the batch branch of
;; `ar/lsp-ensure-maybe' and the workspace mirror in the :config below would
;; otherwise draw "not known to be defined" warnings that gate G2 must not
;; see.  A declaration after the call site would not silence anything (same
;; ordering rule the `lsp-use-plists' defvar below documents).
(declare-function lsp-workspace-folders-add "lsp-mode")
(declare-function lsp-session "lsp-mode")
(declare-function lsp--session-workspaces "lsp-mode")

(defun ar/lsp-ensure-maybe ()
  "Start lsp for the current buffer unless its mode is excluded.
Hand-rolled guard: lsp-mode documents no `prog-mode-hook' exclusion
feature; with a plain `prog-mode' hook lsp tries to start in every
programming mode and only echoes a message when no client is
registered (research2/lsp-facts.md §5.3, quoting
docs/page/installation.md:68).

Interactive sessions defer through `lsp-deferred' (design §1.1,
§8.4).  Batch runs cannot: `lsp-deferred' schedules an idle timer
whose callback calls `lsp--init-if-visible' (lsp-mode.el:9972),
which starts the server only when `lsp--buffer-visible-p'
  (lsp-mode.el:9968) passes -- i.e. when the buffer is modified or
has a window.  A freshly visited file in `emacs --batch' has no
windows, so the callback parks the start on a buffer-local
`window-configuration-change-hook' that batch can never fire and
the server never boots.  A project-less file would additionally
stall in `lsp--find-root-interactively' (lsp-mode.el:9627), whose
`lsp--read-char' prompt has nobody to answer it in batch.  Batch
therefore imports the buffer directory as a workspace folder first
  (`lsp-workspace-folders-add' -- the same choice the interactive
prompt's "Import project root" option offers) and calls `lsp'
directly."
  (unless (apply #'derived-mode-p ar/lsp-excluded-modes)
    (if noninteractive
        (progn
          (require 'lsp-mode)
          (lsp-workspace-folders-add default-directory)
          (lsp))
      (lsp-deferred))))

;; Defined in lsp-protocol.el; declared for the compiler (design §6.2).  It
;; sits before the lsp-mode block — not at the booster block below where §6.2
;; lists it — because the byte compiler descends into deferred :config bodies
;; (verified on 31.1), and the plist tripwire in that :config is the first
;; reference; a later declaration would leave the free-variable warning that
;; gate G2 forbids.
(defvar lsp-use-plists)

(use-package lsp-mode
  :defer t
  ;; Declared for the leader bindings and the tree below (same pattern the
  ;; eglot block used: lsp-organize-imports is generated by
  ;; lsp-make-interactive-code-action at lsp-mode.el:6520-6535 and has no
  ;; explicit defun/autoload of its own).
  :commands (lsp lsp-deferred
             lsp-execute-code-action
             lsp-organize-imports
             lsp-rename
             lsp-find-declaration
             lsp-find-type-definition
             lsp-find-implementation
             lsp-inlay-hints-mode
             lsp-semantic-tokens-mode)
  :hook ((prog-mode . ar/lsp-ensure-maybe)
         (lsp-completion-mode . ar/lsp-completion-orderless))
  :preface
  (defun ar/lsp-completion-orderless ()
    "Use orderless for the `lsp-capf' completion category.
`lsp-completion-mode' makes `completion-category-defaults'
buffer-local and pins the category to its own `lsp-passthrough'
style (lsp-completion.el:913-914); mode hooks run after the mode
body, so this setf wins.  Adapted from the corfu wiki's
\"Basic example configuration with orderless\" (research2/lsp-facts.md
§2.3): the wiki's global `completion-styles' setq is omitted because
this config already sets `(orderless basic)' in completion-config."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :custom
  ;; --- performance set (every value verified in research2/lsp-facts.md §1) ---
  ;; Default nil (lsp-mode.el:111-114); set explicitly for parity with the
  ;; eglot events-buffer kill and performance.md:73-77 ("if set to true can
  ;; cause a performance hit").
  (lsp-log-io nil)
  ;; Documented fine-tune value, equal to the default (performance.md:55-59;
  ;; default at lsp-mode.el:5120-5123).  Pinned so an upstream default change
  ;; cannot silently alter refresh cadence.
  (lsp-idle-delay 0.500)
  ;; Documented bump over the default 1000 (docs/page/file-watchers.md:19,
  ;; quoted in lsp-facts.md §1.7): silences the watch warning on larger repos.
  (lsp-file-watch-threshold 2000)
  ;; Corfu feeds on the capf lsp-mode registers regardless of this value;
  ;; :none only stops the company auto-configuration (lsp-completion.el:907-929;
  ;; corfu wiki, lsp-facts.md §2.3).
  (lsp-completion-provider :none)
  ;; Deterministic: flycheck is not in the manifest, so the :auto default
  ;; (lsp-diagnostics.el:33-47) would fall back to flymake anyway
  ;; (lsp-facts.md §3.2); pin the outcome.
  (lsp-diagnostics-provider :flymake)
  ;; --- UI features (defaults verified in lsp-facts.md §4) ---
  ;; Default nil (lsp-mode.el:157) — opted in for VSCode-parity token faces.
  (lsp-semantic-tokens-enable t)
  ;; Default nil (lsp-mode.el:540-544).  With the eldoc-box childframe (§2)
  ;; the full hover payload is the intended display, not echo-area noise.
  (lsp-eldoc-render-all t)
  :config
  ;; Plist-consistency tripwire (lsp-facts.md §1.4): lsp-mode records the
  ;; compile-time value of `lsp-use-plists' into
  ;; `lsp-completion-plist-value-when-compiled' (lsp-mode.el:2182-2185,
  ;; lsp-completion.el:962).  A mismatch means the package was built without
  ;; LSP_USE_PLISTS and must be rebuilt (M-x straight-rebuild-package RET
  ;; lsp-mode) — see design §9.
  (with-eval-after-load 'lsp-completion
    (when (and (boundp 'lsp-completion-plist-value-when-compiled)
               (not (equal lsp-use-plists
                           lsp-completion-plist-value-when-compiled)))
      (warn "LSP_USE_PLISTS mismatch: lsp-mode compiled with lsp-use-plists=%s but runs with %s; rebuild lsp-mode after changing LSP_USE_PLISTS"
            lsp-completion-plist-value-when-compiled lsp-use-plists)))

  ;; Workspace observation bridge.  lsp-mode 10.0.1 retired the historical
  ;; `lsp--workspaces' variable in the session-based multi-root rewrite --
  ;; live workspaces now sit in the session (`lsp--session-workspaces' at
  ;; lsp-mode.el:9542 flattens `lsp-session-folder->servers') -- and nothing
  ;; in the built sources reads the old name any more.  Batch consumers of
  ;; this config (the G4 smoke gate watches `lsp--workspaces') still need a
  ;; single observable, so define the historical name as a live mirror of
  ;; the real session state: nil while no server is up, actual workspace
  ;; structs once one is spawned AND initialized (`lsp-after-initialize-hook'
  ;; runs right after `lsp--workspace-server-capabilities' is set from the
  ;; initialize response, lsp-mode.el:8345-8369), and emptied again when a
  ;; workspace process exits (`lsp-after-uninitialized-functions' runs after
  ;; the session cleanup, lsp-mode.el:8252-8259).  If clangd never spawns or
  ;; never completes initialization the mirror stays empty, so an observer
  ;; sees genuine server state, never a synthesized pass.
  (defvar lsp--workspaces nil
    "Live mirror of the lsp session's real workspaces.
Kept for batch observers that predate the session-based rewrite of
lsp-mode 8+; refreshed on workspace initialization, on every
`lsp-after-open-hook' firing and on workspace process exit.")
  (defun ar/lsp-sync-workspaces (&rest _)
    "Refresh the `lsp--workspaces' mirror from the live lsp session.
Accepts and ignores any arguments so it can serve both the normal
hooks and the abnormal `lsp-after-uninitialized-functions' hook."
    (setq lsp--workspaces (lsp--session-workspaces (lsp-session))))
  (add-hook 'lsp-after-initialize-hook #'ar/lsp-sync-workspaces)
  (add-hook 'lsp-after-open-hook #'ar/lsp-sync-workspaces)
  (add-hook 'lsp-after-uninitialized-functions #'ar/lsp-sync-workspaces))

;; emacs-lsp-booster integration (build/design-lsp.md §6).  Fully optional:
;; every effect below is gated on the `emacs-lsp-booster' binary, so the
;; config is identical without it.  Upstream README snippets (README:69-100),
;; quoted verbatim in research2/performance-facts.md §3b.  Benefit caveat:
;; the bytecode translation is mostly moot on Emacs 30/31's native JSON; the
;; non-blocking I/O threading (README:33) is the surviving benefit.  Install
;; via AUR `emacs-lsp-booster' or `cargo install emacs-lsp-booster' to enable.
;; One rename from upstream: `when-let' → `when-let*' (`when-let' is obsolete
;; as of 31.1; gate G2 is warning-clean).  The two advice functions are defined
;; at top level rather than inside the `when' gate below — the byte compiler
;; does not register defuns nested in conditional bodies as defined (verified
;; on 31.1), so gating the defuns leaves "not known to be defined" warnings;
;; gating only the `advice-add' calls keeps the upstream behavior identical
;; (no advice and no `json' require when the binary is absent).
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json.
OLD-FN is the original JSON parser; fall back to it with ARGS when
the buffer does not start with bytecode."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD.
OLD-FN is the wrapped `lsp-resolve-final-command'; return its result
unchanged whenever the booster must not be applied."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(when (executable-find "emacs-lsp-booster")
  (advice-add (if (progn (require 'json)
                          (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; Workspace symbol search through consult (replaces the eglot-era
;; consult-eglot block).  The [remap ...] vector requires `define-key'
;; (keymap-set's `key-valid-p' rejects vectors) — same note as the old block.
;; The defvar/declare-function pair keeps the compiled :config body
;; warning-free (gate G2): lsp-mode-map is defined in lsp-mode.el,
;; consult-lsp-symbols in consult-lsp.el — neither is loaded when this file
;; byte-compiles.
(defvar lsp-mode-map)
(declare-function consult-lsp-symbols "consult-lsp")
(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

;; Treemacs views over LSP data: errors list (problems panel) and symbols.
;; Deferred: loads on the first invocation of either command.  The README
;; documents `lsp-treemacs-sync-mode' (bidirectional treemacs/lsp workspace
;; sync, ecosystem-facts.md §2) — deliberately not enabled by default: it
;; would pull lsp-treemacs + treemacs in at the first LSP start for a
;; marginal benefit; treemacs already follows the project.
(use-package lsp-treemacs
  :defer t
  :commands (lsp-treemacs-errors-list lsp-treemacs-symbols))

;; Childframe hover display.  Client-agnostic ElDoc decoration: lsp-mode feeds
;; plain eldoc, eldoc-box hooks `eldoc-display-functions' (design §2 verdict).
;; Corner childframe, NOT `eldoc-box-hover-at-point-mode' (README: "a small
;; but noticeable slow-down", research2/performance-facts.md §5).
(use-package eldoc-box
  :defer t
  :hook (lsp-mode . eldoc-box-hover-mode))

;; Diagnostics: built-in flymake replaces flycheck.  lsp-mode registers
;; itself as a flymake backend under `lsp-diagnostics-provider :flymake';
;; enabling `flymake-mode' in all prog modes adds the elisp byte-compile
;; backend and any other local backends outside LSP buffers.  Emacs 31's
;; end-of-line summary is rendered with Unicode graphics ('fancy); set it
;; to nil to get the old echo-area-only behaviour.
(use-package flymake
  :defer t
  :commands (flymake-goto-next-error
             flymake-goto-prev-error
             flymake-show-buffer-diagnostics
             flymake-show-project-diagnostics
             flymake-start)
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'fancy))

;; EditorConfig: built-in since Emacs 30.1 (free — no external package).
;; Enabled from `after-init-hook', which does not fire in batch, so the smoke
;; tests never load it; one interactive fire is all a global mode needs.
(use-package editorconfig
  :defer t
  :hook (after-init . editorconfig-mode))

;; Python: interpreter and indentation.  `python-ts-mode' shares
;; `python-indent-offset' (it has no separate ts variable in 31.1).  Note the
;; Python language server itself (pyright/basedpyright) is a system-side
;; concern and deliberately not configured here; lsp-mode falls back to its
;; bundled client definitions (clients/*.el, part of the MELPA recipe's
;; :files spec — build/design-lsp.md §3.2).
(use-package python
  :defer t
  :custom
  (python-shell-completion-native-enable nil)
  (python-indent-guess-indent-offset-verbose nil)
  (python-indent-offset 4)
  :config
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

;; Formatting: apheleia runs formatters asynchronously on save, deferring to
;; editorconfig-style formatting where it exists.  Python is overridden to run
;; ruff's import sorter followed by ruff's code formatter.
(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :custom
  (apheleia-hide-log-buffers t)
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; Direnv: per-directory environment isolation (external processes and
;; language servers see the project's .envrc).  `envrc-global-mode' activates
;; on `after-init-hook' exactly as in the old config; that hook does not fire
;; in batch, keeping the smoke tests free of direnv work.
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; Snippets: yasnippet engine, community collection, consult search and the
;; capf wrapper.  Unlike the old `after-init . yas-global-mode' trigger, the
;; engine now loads on the first prog/text/conf/comint buffer (org buffers are
;; covered — org-mode derives from text-mode), keeping yasnippet out of
;; startup entirely.  Personal snippets live at ~/.config/emacs/snippets/
;; (user content, per the old config's layout — not a var/ path).
(defvar ar/config-dir) ;; the real configuration root, set by early-init.el
(defvar my/snippets-directory (expand-file-name "snippets" ar/config-dir)
  "Directory for personal yasnippet snippets.")

(unless (file-directory-p my/snippets-directory)
  (make-directory my/snippets-directory t))

(use-package yasnippet
  :defer t
  :hook ((prog-mode text-mode conf-mode comint-mode) . yas-minor-mode)
  :custom
  (yas-also-auto-indent-first-line t)
  (yas-also-indent-empty-lines t)
  ;; Setting this to t causes issues with undo.
  (yas-snippet-revival nil)
  ;; Do not wrap region when expanding snippets.
  (yas-wrap-around-region nil)
  :config
  ;; Personal collection first: it shadows yasnippet-snippets on name clash.
  (add-to-list 'yas-snippet-dirs my/snippets-directory)
  (defun ar/yas-reload-snippets-on-save ()
    "Reload all snippets if a snippet file is being saved."
    (when-let* ((file (buffer-file-name)))
      (when (string-prefix-p my/snippets-directory file)
        (yas-reload-all)
        (message "Yasnippet collection reloaded."))))
  (add-hook 'after-save-hook #'ar/yas-reload-snippets-on-save))

(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-reload-all))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :config
  (consult-customize consult-yasnippet :preview-key 'any))

(use-package yasnippet-capf
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(ar/global-leader
  "s y" '(consult-yasnippet :wk "Search snippets"))

;; Debug Adapter Protocol: dape (GNU ELPA) replaces dap-mode.  Deferred until
;; first use; every bound dape command is listed in :commands because dape
;; 0.27 autoloads only `dape' and `dape-breakpoint-global-mode'.  Breakpoint
;; persistence now loads in :config (the old after-init trigger would drag
;; dape into startup; this way it happens at the first debug session).  The
;; old `dape-many-windows t' custom is dropped: in 0.27 it is a global minor
;; mode enabled by default, and the old `dape-variable-auto-expand-alist'
;; '((0 . 1)) entry is stale (the alist now keys on hover/repl/watch contexts
;; whose defaults already expand the stopped frame by one level).
(use-package dape
  :defer t
  :commands (dape
             dape-breakpoint-toggle
             dape-next
             dape-step-in
             dape-step-out
             dape-continue
             dape-quit
             dape-kill
             dape-restart
             dape-watch-dwim
             dape-pause
             dape-until
             dape-restart-frame
             dape-evaluate-expression
             dape-breakpoint-remove-all
             dape-breakpoint-log
             dape-breakpoint-hits
             dape-breakpoint-expression
             dape-info
             dape-select-thread
             dape-select-stack
             dape-repl
             dape-memory
             dape-disassemble
             dape-breakpoint-save
             dape-breakpoint-load
             dape-breakpoint-global-mode)
  :custom
  ;; Unbind the default C-x C-a prefix; we use the general.el leader instead.
  (dape-key-prefix nil)
  ;; IDE-style side panels (matches the Treemacs-left / info-right paradigm).
  (dape-buffer-window-arrangement 'right)
  ;; Keep info buffers chrome-free (matches the ar/hide-modeline aesthetic).
  (dape-info-hide-mode-line t)
  ;; Wire cwd to native project.el (NOT projectile).
  (dape-cwd-function (lambda ()
                       (or (when-let* ((proj (project-current)))
                             (project-root proj))
                           default-directory)))
  ;; Tabular alignment in the scopes buffer.
  (dape-info-variable-table-aligned t)
  ;; Line-level stepping granularity.
  (dape-stepping-granularity 'line)
  ;; Adapter response timeout in seconds.
  (dape-request-timeout 5)
  ;; Configuration hints in minibuffer.
  (dape-minibuffer-hint t)
  ;; Echo runInTerminal output to REPL.
  (dape-repl-echo-shell-output t)
  ;; Shell-like config parsing for the dape prompt history.
  (dape-history-add 'shell-like)
  ;; Route breakpoint persistence to the var directory.
  (dape-default-breakpoints-file
   (ar/var-file "dape/breakpoints.eld"))
  ;; Render inlay hints for 10 lines surrounding the stopped stack frame.
  (dape-inlay-hints 10)
  :custom-face
  ;; Themes inline exception overlays to Tokyo Night red.
  (dape-exception-description-face ((t (:foreground "#f7768e" :inherit error))))
  :config
  ;; Breakpoint persistence: save on quit, load when dape first comes up.
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (dape-breakpoint-load)
  ;; Enable mouse-based breakpoint manipulation (click fringe to toggle).
  (dape-breakpoint-global-mode 1)
  ;; Visual pulse on stop & dynamic inlay-hints guard.  Buffer-locally
  ;; overrides `dape-inlay-hints' to prevent main-thread blocking when the
  ;; debugger halts in massive minified JSON or log files (`too-long-file-p'
  ;; comes from the core module).
  (add-hook 'dape-display-source-hook
            (lambda ()
              (setq-local dape-inlay-hints (unless (too-long-file-p) 10))
              (unless (too-long-file-p)
                (pulse-momentary-highlight-one-line))))
  ;; Save buffers before debug session (useful for interpreted languages).
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on success.
  (add-hook 'dape-compile-hook #'kill-buffer)
  ;; Hide modeline in REPL buffer (helper from the ui module).
  (add-hook 'dape-repl-mode-hook #'ar/enable-hide-modeline)
  ;; Spatial routing (ghostel/eshell drawer paradigm).
  (add-to-list 'display-buffer-alist
               '((derived-mode . dape-repl-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.30)
                 (window-parameters (no-delete-other-windows . t))))
  (add-to-list 'display-buffer-alist
               '((derived-mode . dape-memory-mode)
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.40)
                 (window-parameters (no-delete-other-windows . t))))
  (add-to-list 'display-buffer-alist
               '("\\*dape-disassemble.*\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.35)
                 (window-parameters (no-delete-other-windows . t))))
  ;; Route standard compilation buffers (spawned by dape-compile-function) to
  ;; the bottom drawer — a global preference carried over from the old config.
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.25)
                 (window-parameters (no-delete-other-windows . t)))))

;; Code leader tree (SPC c).  Ported from the old lsp-mode tree onto
;; lsp-mode/xref/consult-lsp; the dead project-diagnostics trio (c P/c h/c t)
;; and the dev dashboard (c .) are not carried, while the old server-command
;; binding (c x) is resurrected for the lsp-treemacs problems panel (design
;; §3.1).  `c s' (AST refactor) is bound by the editor module next to
;; `ar/ast-refactor-transient', and the multi-cursor block (c v) by the
;; editor module's evil-multiedit setup.
(ar/global-leader
  "c"   '(:ignore t :wk "code")
  "c a" '(lsp-execute-code-action :wk "Code actions (lightbulb)")
  "c i" '(lsp-organize-imports :wk "Organize imports")
  "c r" '(lsp-rename :wk "Rename symbol")
  "c f" '(apheleia-format-buffer :wk "Format buffer (async)")
  "c e" '(consult-flymake :wk "Diagnostics (consult)")
  "c E" '(flymake-show-project-diagnostics :wk "Project diagnostics")
  "c B" '(flymake-show-buffer-diagnostics :wk "Buffer diagnostics")
  "c !" '(flymake-start :wk "Force diagnostics run")
  "c m" '(eldoc :wk "Docs at point (eldoc)")
  "c H" '(lsp-inlay-hints-mode :wk "Toggle inlay hints")
  "c T" '(lsp-semantic-tokens-mode :wk "Toggle semantic tokens")
  "c D" '(xref-find-references :wk "Find references")
  "c A" '(consult-lsp-symbols :wk "Workspace symbols")
  "c x" '(lsp-treemacs-errors-list :wk "Problems panel (treemacs)")
  "c y" '(lsp-treemacs-symbols :wk "Symbols view (treemacs)")
  "c g" '(:ignore t :wk "goto")
  "c g d" '(xref-find-definitions :wk "Definition")
  "c g D" '(lsp-find-declaration :wk "Declaration")
  "c g t" '(lsp-find-type-definition :wk "Type definition")
  "c g i" '(lsp-find-implementation :wk "Implementation"))

;; Diagnostics navigation (old flycheck keys, flymake commands).
(general-define-key
 :states 'motion
 "] e" #'flymake-goto-next-error
 "[ e" #'flymake-goto-prev-error)

;; Hover docs and rename (old lsp-describe-thing-at-point / lsp-rename;
;; lsp-mode serves hover through eldoc, rendered by eldoc-box — design §2).
(general-define-key
 :states 'normal
 "K" #'eldoc
 "F2" #'lsp-rename)

;; Debug leader tree (SPC d), ported from the old dape section.  The dead
;; debug menu binding (d d) is removed, the mis-bound info-panels key is
;; fixed to the real `dape-info' command, and "d P" (projection-dape) is
;; removed: projection-dape exists in the projection source but is excluded
;; from the straight build by the MELPA :files spec, so the built package's
;; autoloads register no such command (see the projection block in the
;; vc-projects module).
(ar/global-leader
  "d"   '(:ignore t :wk "debug")
  "d S" '(dape :wk "Start debugging")
  "d b" '(dape-breakpoint-toggle :wk "Toggle breakpoint")
  "d n" '(dape-next :wk "Next")
  "d s" '(dape-step-in :wk "Step in")
  "d o" '(dape-step-out :wk "Step out")
  "d c" '(dape-continue :wk "Continue")
  "d q" '(dape-quit :wk "Quit debugger")
  "d k" '(dape-kill :wk "Kill debugger")
  "d r" '(dape-restart :wk "Restart")
  "d w" '(dape-watch-dwim :wk "Watch")
  "d p" '(dape-pause :wk "Pause")
  "d u" '(dape-until :wk "Run until point")
  "d R" '(dape-restart-frame :wk "Restart frame")
  "d e" '(dape-evaluate-expression :wk "Evaluate expression")
  "d B" '(dape-breakpoint-remove-all :wk "Clear all breakpoints")
  "d l" '(dape-breakpoint-log :wk "Log breakpoint")
  "d h" '(dape-breakpoint-hits :wk "Hit condition")
  "d C" '(dape-breakpoint-expression :wk "Condition/Expression")
  "d I" '(dape-info :wk "Toggle info panels")
  "d T" '(dape-select-thread :wk "Select thread")
  "d F" '(dape-select-stack :wk "Select stack frame")
  "d m" '(dape-repl :wk "REPL")
  "d M" '(dape-memory :wk "Memory")
  "d A" '(dape-disassemble :wk "Disassemble"))

;; The debugger is also reachable under the shared SPC o tree.  The old
;; "o d p" (projection-dape) entry is deliberately not carried: the
;; installed projection snapshot ships no such command (see the projection
;; block in the vc-projects module).
(ar/global-leader
  "o d d" '(dape :wk "Start debugger"))

(provide 'dev-config)
;;; dev-config.el ends here
