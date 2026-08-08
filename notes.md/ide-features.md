# VS Code Feature Parity in Emacs — the `lsp-mode` Stack

This document maps common VS Code IDE features to their Emacs implementations, built on **`lsp-mode`** (the full-featured, third‑party LSP client) together with **`flycheck`** (diagnostics/linting), **`apheleia`** (async, point‑preserving code formatting), and **`dape`** with **`dap-mode`** as the debugging layer (Debug Adapter Protocol). It replaces an earlier `eglot` + `corfu` + `cape` based document with the equivalent — and in several areas more complete — configuration for this stack. Every section gives you the feature overview, the packages involved, keybindings, full configuration, and troubleshooting notes, so that by the end you have an Emacs configuration that matches VS Code on features *and* is tuned to match it on responsiveness.

Verified against `lsp-mode` 9.x, `lsp-ui` 8.x, `flycheck` 34+, `apheleia` (2026 MELPA snapshot), `dape` 0.2x (GNU ELPA), and `dap-mode` 0.7+ as of **July 2026**, running on Emacs 30/31.

## Why this stack instead of Eglot?

Eglot is smaller and ships with core Emacs, but it deliberately delegates UI work to the rest of Emacs (Flymake for diagnostics, `xref` for navigation, no bundled debugger, no bundled formatter). `lsp-mode` takes the opposite, VS-Code-like stance: it ships its own rich UI layer (`lsp-ui`: sideline diagnostics, hover child frames, peek windows, breadcrumbs), integrates natively with `flycheck`, `treemacs`, `dap-mode`, and `company`/`corfu`, and supports the entire LSP 3.17 surface (semantic tokens, inlay hints, call/type hierarchy, linked editing ranges, and more) with pre-built per-language client configuration for 60+ languages out of the box. That larger footprint is exactly what buys the "VS Code parity" this document is chasing — the trade-off is a heavier package and, without tuning, a slower one, which is why a performance-tuning appendix is included at the end.

## The Stack at a Glance

| Concern                                                | VS Code                                  | Emacs equivalent                                                                                                              |
| ------------------------------------------------------ | ---------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| LSP client / IDE core                                  | built-in                                 | `lsp-mode`                                                                                                                    |
| Rich LSP UI (hover, peek, sideline, breadcrumbs, lens) | built-in                                 | `lsp-ui`                                                                                                                      |
| Completion popup                                       | built-in IntelliSense                    | `company` (or `corfu`, both supported) via `lsp-mode`'s `company-capf`/`completion-at-point`                                  |
| Diagnostics / linting                                  | built-in Problems panel                  | `flycheck` (`lsp-diagnostics` backend)                                                                                        |
| Formatting (format-on-save)                            | built-in + extensions (Prettier, Black…) | `apheleia` (formatter-agnostic, async, preserves point)                                                                       |
| Debugging                                              | built-in (DAP)                           | `dape` (lightweight, editor-agnostic DAP client) or `dap-mode` (lsp-mode's own DAP client, more batteries-included templates) |
| Project/Explorer tree                                  | Explorer sidebar                         | `treemacs` + `lsp-treemacs`                                                                                                   |
| Outline / symbols                                      | Outline view, `Ctrl+Shift+O`             | `lsp-treemacs-symbols`, `consult-lsp-symbols`, `imenu`                                                                        |
| Multi-cursor                                           | built-in                                 | `multiple-cursors` / `iedit`                                                                                                  |
| Minimap                                                | built-in                                 | `treemacs` minimal-mode or `minimap.el` (see caveats)                                                                         |

---

## Table of Contents

- [Part 1 — Foundation: Installing and Tuning the Stack](#part-1--foundation-installing-and-tuning-the-stack)
- **Completion & Intelligence**
  - [IntelliSense / Code Completion](#intellisense-code-completion)
  - [Hover Info](#hover-info)
  - [Signature Help](#signature-help)
  - [Semantic Tokens (Semantic Highlighting)](#semantic-tokens-semantic-highlighting)
  - [Inlay Hints](#inlay-hints)
- **Navigation & Code Jumping**
  - [Go to Definition / Declaration / Type Definition / Implementation](#go-to-definition-declaration-type-definition-implementation)
  - [Find All References](#find-all-references)
  - [Peek Definition / Peek References](#peek-definition-peek-references)
  - [Call Hierarchy & Type Hierarchy](#call-hierarchy--type-hierarchy)
- **Symbols & Diagnostics**
  - [Document Symbols / Outline View](#document-symbols-outline-view)
  - [Workspace Symbol Search](#workspace-symbol-search)
  - [Diagnostics & the Problems Panel (Flycheck)](#diagnostics--the-problems-panel-flycheck)
  - [Inline Values](#inline-values)
- **Code Actions & Refactoring**
  - [Code Actions, Quick Fixes & the Lightbulb](#code-actions-quick-fixes--the-lightbulb)
  - [Rename Symbol](#rename-symbol)
  - [Execute Command](#execute-command)
- **Formatting & Editing (Apheleia)**
  - [Document & Range Formatting](#document--range-formatting)
  - [On-type Formatting](#on-type-formatting)
  - [Folding Ranges](#folding-ranges)
  - [Selection Range (Smart Expand/Shrink)](#selection-range-smart-expandshrink)
  - [Linked Editing Range](#linked-editing-range)
  - [Multi-Cursor Editing](#multi-cursor-editing)
- **Visual Enhancements & UI**
  - [Document Highlight](#document-highlight)
  - [Document Links & Document Color](#document-links--document-color)
  - [Bracket Pair Colorization](#bracket-pair-colorization)
  - [Minimap](#minimap)
  - [Breadcrumbs Bar](#breadcrumbs-bar)
  - [Sticky Scroll](#sticky-scroll)
- **Debugging (Debug Adapter Protocol)**
  - [Debugging Overview: `dape` vs `dap-mode`](#debugging-overview-dape-vs-dap-mode)
  - [Breakpoints, Stepping, Variables, Watch, REPL](#breakpoints-stepping-variables-watch-repl)
  - [Per-Language Debug Configuration](#per-language-debug-configuration)
- **Workspace & File Management**
  - [Workspace File-Operation Hooks (LSP-aware rename)](#workspace-file-operation-hooks-lsp-aware-rename)
- [Appendix — Performance Tuning for VS Code-Level Responsiveness](#appendix--performance-tuning-for-vs-code-level-responsiveness)

---

# Part 1 — Foundation: Installing and Tuning the Stack

Install everything up front; every later section assumes this base configuration is loaded first.

```elisp
;; ==========================================
;; 0. STARTUP PERFORMANCE (must load before the rest)
;; ==========================================
(setq gc-cons-threshold (* 256 1024 1024)
      read-process-output-max (* 3 1024 1024)
      package-native-compile t)

;; ==========================================
;; 1. LSP-MODE (the LSP client / IDE core)
;; ==========================================
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
                    (lsp-deferred))))
   (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :capf)
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-lens-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 4000)
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)
  (lsp-use-plists t)
  :config
  (setq lsp-restart 'auto-restart))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-max-width 80)
  (lsp-ui-doc-max-height 20)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-delay 0.2)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 20))

(use-package treemacs :ensure t)
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :config (lsp-treemacs-sync-mode 1))

(use-package company
  :ensure t
  :hook (lsp-mode . company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t))

;; ==========================================
;; 2. FLYCHECK (diagnostics / Problems panel)
;; ==========================================
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-idle-change-delay 0.5)
  (flycheck-display-errors-delay 0.2)
  (flycheck-indication-mode 'left-fringe))

;; ==========================================
;; 3. APHELEIA (format-on-save)
;; ==========================================
(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; ==========================================
;; 4. DAPE / DAP-MODE (debugging) — see the Debugging section
;; ==========================================
(use-package dape :ensure t)

;; ==========================================
;; 5. QUALITY-OF-LIFE COMPANIONS
;; ==========================================
(use-package which-key :ensure t :init (which-key-mode))
(use-package consult :ensure t)
(use-package consult-lsp :ensure t :after (consult lsp-mode))
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))
(use-package multiple-cursors :ensure t)
(use-package iedit :ensure t)
```

### Required external tooling

`lsp-mode` does not bundle language servers, `apheleia` does not bundle formatters, and `dape`/`dap-mode` do not bundle debug adapters — exactly like VS Code extensions, these are separate binaries that must be on `$PATH` (or resolved via `lsp-mode`'s per-language installer, `M-x lsp-install-server`). Common examples:

| Language | Language server (lsp-mode) | Formatter (apheleia) | Debug adapter (dape/dap-mode) |
| --- | --- | --- | --- |
| Python | `pyright` or `basedpyright`, `ruff-lsp` | `black`, `ruff format` | `debugpy` |
| TypeScript/JS | `typescript-language-server` | `prettier` | `vscode-js-debug` |
| Go | `gopls` | `gofmt` / `goimports` | `dlv` (delve) |
| Rust | `rust-analyzer` | `rustfmt` | `codelldb` |
| C/C++ | `clangd` | `clang-format` | `lldb-dap` / `codelldb` / `gdb` (14.1+) |

`M-x lsp-install-server` will fetch most servers automatically into `~/.emacs.d/.cache/lsp`; formatters and debug adapters are installed via your system package manager, or via `dape`'s / `dap-mode`'s own `-setup` helper commands (e.g. `dap-gdb-lldb-setup`, `dape-ensure-command`).

---

# Completion & Intelligence

## IntelliSense / Code Completion

_VS Code feature: context-aware autocomplete popup, auto-imports on accept, fuzzy filtering._

| Attribute | Value |
| --- | --- |
| Status | 🟢 working · `lsp-mode` + `company` |
| LSP methods | `textDocument/completion`, `completionItem/resolve` |
| Emacs routing | `lsp-mode` → `completion-at-point-functions` (`lsp-completion-at-point`) → `company-capf` (UI) |

`lsp-mode` registers `lsp-completion-at-point` as a CAPF backend. `company-capf` (bundled with `company`) reads from it directly — no merging layer like `cape` is required, because `lsp-mode` itself already blends LSP candidates with `company-dabbrev`/`company-files` if you list them in `company-backends`.

### Keybindings

| Action | Command | Keybinding |
| --- | --- | --- |
| Trigger completion | `company-complete` | automatic (`company-idle-delay 0.0`) or `C-M-i` |
| Next / previous candidate | `company-select-next` / `company-select-previous` | `C-n` / `C-p` |
| Insert candidate | `company-complete-selection` | `RET` / `TAB` |
| Show doc for candidate | `company-show-doc-buffer` | `C-h` |
| Abort | `company-abort` | `C-g` |

### Configuration

```elisp
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-backends '((company-capf :with company-yasnippet)
                       company-files company-dabbrev-code))
  :bind (:map company-active-map
              ("TAB" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; Auto-import on accept: lsp-mode automatically applies
;; `additionalTextEdits` returned by completionItem/resolve
;; (e.g. inserting the import statement) whenever a candidate is
;; committed — no extra configuration needed, this mirrors VS Code's
;; "Add import" behavior on accepting a suggestion.
(setq lsp-completion-enable-additional-text-edit t)

;; Icons in the completion margin (VS Code-style kind icons)
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :custom (company-box-icons-alist 'company-box-icons-all-the-icons))
```

If you prefer `corfu`'s minimal child-frame instead of `company`, `lsp-mode` supports it unchanged since both read from `completion-at-point-functions`; just swap the `company` block for `corfu`/`cape` and leave `lsp-completion-provider` at `:capf`.

---

## Hover Info

_VS Code feature: hover tooltip showing type signature, docstring, and diagnostics for the symbol under point._

| Status | 🟢 working · `lsp-ui-doc` |
| --- | --- |
| LSP method | `textDocument/hover` |

`lsp-ui-doc` renders hover information in a child frame (or a plain buffer via `lsp-ui-doc-use-webkit nil`), reproducing VS Code's floating hover card, including Markdown rendering of the docstring.

### Keybindings

| Action | Command | Keybinding |
| --- | --- | --- |
| Show hover at point | `lsp-ui-doc-show` | `C-c l h h` / automatic on idle |
| Hide hover | `lsp-ui-doc-hide` | `C-c l h g` |
| Focus the hover frame (scroll it) | `lsp-ui-doc-focus-frame` | `C-c l h f` |
| Glance (temporary, no focus) | `lsp-ui-doc-glance` | bind to your liking |

### Configuration

```elisp
(setq lsp-ui-doc-enable t
      lsp-ui-doc-show-with-cursor t   ; show automatically as you move point
      lsp-ui-doc-show-with-mouse t
      lsp-ui-doc-delay 0.3
      lsp-ui-doc-position 'top        ; 'top, 'bottom, or 'at-point
      lsp-ui-doc-alignment 'window
      lsp-ui-doc-header t
      lsp-ui-doc-include-signature t
      lsp-ui-doc-use-childframe t)    ; falls back to a normal buffer if childframes unsupported (e.g. terminal)

;; Eldoc still works underneath and is a good terminal-friendly fallback:
;; the minibuffer/echo-area single-line signature is always available
;; even with lsp-ui-doc disabled.
(setq eldoc-idle-delay 0.2)
```

### Troubleshooting

- **Doc frame doesn't appear in a terminal**: child frames require a GUI Emacs. In `-nw` mode, disable `lsp-ui-doc-use-childframe` to fall back to a dedicated buffer, or rely on Eldoc's echo-area output.
- **Hover flickers while typing**: raise `lsp-ui-doc-delay`, or set `lsp-ui-doc-show-with-cursor nil` and trigger manually with `lsp-ui-doc-show`.

---

## Signature Help

_VS Code feature: parameter hints popup while typing inside a function call._

| Status | 🟢 working · Eldoc (built-in) driven by `lsp-mode` |
| --- | --- |
| LSP method | `textDocument/signatureHelp` |

`lsp-mode` wires `textDocument/signatureHelp` into Eldoc automatically (`lsp-signature-mode`/`lsp-eldoc-hook`), so parameter hints appear in the echo area or, with `lsp-ui-doc`, in a child frame as you type inside argument lists — highlighting the active parameter exactly like VS Code.

```elisp
(setq lsp-signature-auto-activate t          ; show as soon as you type an opening paren
      lsp-signature-doc-lines 1              ; keep it to one line in the echo area
      lsp-signature-render-documentation t
      lsp-signature-function 'lsp-signature-posframe) ; posframe popup near point, closer to VS Code's inline hint
```

Keybindings while a signature is active: `C-c l s s` (toggle full doc), `C-c l s t` (toggle signature help globally).

---

## Semantic Tokens (Semantic Highlighting)

_VS Code feature: highlighting driven by the language server's understanding of the code (distinguishing a parameter from a local variable from a class, for example), layered on top of syntax highlighting._

| Status | 🟢 working · `lsp-semantic-tokens-mode` |
| --- | --- |
| LSP method | `textDocument/semanticTokens/full`, `/range`, `/full/delta` |

```elisp
(setq lsp-semantic-tokens-enable t
      lsp-semantic-tokens-honor-refresh-requests t)  ; live updates as the server re-analyzes
```

`lsp-mode` maps each LSP token type/modifier to a `lsp-face-semhl-*` face; customize these to distinguish, e.g., mutable vs immutable bindings the way VS Code themes do:

```elisp
(custom-set-faces
 '(lsp-face-semhl-variable-parameter ((t (:slant italic))))
 '(lsp-face-semhl-property ((t (:foreground "#e5c07b")))))
```

Not every server implements semantic tokens (check with `M-x lsp-describe-session`, under "capabilities"); where absent, Emacs falls back to `tree-sitter`/`font-lock` syntactic highlighting, which is usually indistinguishable for common languages.

---

## Inlay Hints

_VS Code feature: greyed-out inline annotations for inferred types and parameter names._

| Status | 🟢 working · `lsp-inlay-hints-mode` (LSP 3.17) |
| --- | --- |
| LSP method | `textDocument/inlayHint` |

```elisp
(setq lsp-inlay-hint-enable t)
(add-hook 'lsp-mode-hook #'lsp-inlay-hints-mode)

(custom-set-faces
 '(lsp-inlay-hint-face ((t (:inherit shadow :height 0.85)))))
```

Toggle at will with `M-x lsp-inlay-hints-mode`; per-server hint kinds (type hints vs parameter-name hints) are controlled via each server's own LSP `initializationOptions` (e.g. `rust-analyzer`'s `inlayHints.*` settings, passed through `lsp-rust-analyzer-*` custom variables).

---

# Navigation & Code Jumping

## Go to Definition / Declaration / Type Definition / Implementation

_VS Code feature: `F12` / `Ctrl+F12` family of jump commands._

| Status | 🟢 working · `lsp-mode` over `xref` |
| --- | --- |
| LSP methods | `textDocument/definition`, `/declaration`, `/typeDefinition`, `/implementation` |

`lsp-mode` provides dedicated commands for each LSP navigation request rather than folding them all into generic `xref` (the way Eglot does), which matters because plain `xref-find-definitions` cannot distinguish declaration from definition — `lsp-mode`'s commands can.

| Action | Command | Keybinding |
| --- | --- | --- |
| Go to Definition | `lsp-find-definition` | `M-.` / `C-c l g g` |
| Go to Declaration | `lsp-find-declaration` | `C-c l g d` |
| Go to Type Definition | `lsp-find-type-definition` | `C-c l g t` |
| Go to Implementation | `lsp-find-implementation` | `C-c l g i` |
| Jump back | `xref-go-back` | `M-,` |
| Open in other window | `lsp-find-definition` with `C-u` prefix | `C-u M-.` |

```elisp
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-,") #'xref-go-back)
  (define-key lsp-mode-map (kbd "C-c l g d") #'lsp-find-declaration)
  (define-key lsp-mode-map (kbd "C-c l g t") #'lsp-find-type-definition)
  (define-key lsp-mode-map (kbd "C-c l g i") #'lsp-find-implementation))

;; Land results directly if there's exactly one, otherwise show the
;; xref buffer — matches VS Code's "jump straight there, or list
;; candidates" behavior.
(setq xref-prompt-for-identifier nil)
```

---

## Find All References

_VS Code feature: `Shift+F12`, references panel._

| Status | 🟢 working |
| --- | --- |
| LSP method | `textDocument/references` |

```elisp
(define-key lsp-mode-map (kbd "C-c l g r") #'lsp-find-references)
;; Or, for a nicer fuzzy-searchable references list:
(define-key lsp-mode-map (kbd "C-c l g R") #'consult-lsp-references)
```

`lsp-find-references` opens a `xref` buffer grouped by file, navigable with `n`/`p` and `RET`. `consult-lsp-references` (from `consult-lsp`) gives an incrementally-filterable minibuffer list with live preview, closer to VS Code's references panel UX.

---

## Peek Definition / Peek References

_VS Code feature: inline expanding preview (`Alt+F12`) without leaving the current buffer._

| Status | 🟢 working · `lsp-ui-peek` (this is the feature Eglot has no equivalent for out of the box) |
| --- | --- |

| Action | Command | Keybinding |
| --- | --- | --- |
| Peek definition | `lsp-ui-peek-find-definitions` | `C-c l g p` |
| Peek references | `lsp-ui-peek-find-references` | `C-c l g P` |
| Peek implementation | `lsp-ui-peek-find-implementation` | `C-c l g I` |
| Next/prev peek item | `lsp-ui-peek--select-next` / `-prev` | `C-n` / `C-p` (inside peek) |
| Jump to peeked location | `lsp-ui-peek--goto-xref` | `RET` |
| Close peek | `lsp-ui-peek--abort` | `q` / `ESC` |

```elisp
(setq lsp-ui-peek-enable t
      lsp-ui-peek-list-width 60
      lsp-ui-peek-peek-height 20
      lsp-ui-peek-fontify 'on-demand)  ; syntax-highlight peeked snippets

(define-key lsp-mode-map (kbd "C-c l g p") #'lsp-ui-peek-find-definitions)
(define-key lsp-mode-map (kbd "C-c l g P") #'lsp-ui-peek-find-references)
```

This opens an in-buffer overlay split showing the target location(s) without switching windows or buffers — the direct analog of VS Code's Peek Definition, and something a bare Eglot/xref setup cannot reproduce without this exact package.

---

## Call Hierarchy & Type Hierarchy

_VS Code feature: incoming/outgoing call tree, supertype/subtype tree._

| Status | 🟢 working · `lsp-treemacs` |
| --- | --- |
| LSP methods | `callHierarchy/incomingCalls`, `/outgoingCalls`, `typeHierarchy/supertypes`, `/subtypes` |

| Action | Command | Keybinding |
| --- | --- | --- |
| Show call hierarchy | `lsp-treemacs-call-hierarchy` | `C-c l G c` |
| Show call hierarchy (outgoing) | `lsp-treemacs-call-hierarchy` with `C-u` | `C-u C-c l G c` |
| Show type hierarchy | `lsp-treemacs-type-hierarchy` | `C-c l G t` |

```elisp
(define-key lsp-mode-map (kbd "C-c l G c") #'lsp-treemacs-call-hierarchy)
(define-key lsp-mode-map (kbd "C-c l G t") #'lsp-treemacs-type-hierarchy)
```

Both render as expandable `treemacs` trees in a side window, each node jumpable with `RET`, matching VS Code's Call Hierarchy / Type Hierarchy panels feature-for-feature (server support required — `clangd`, `gopls`, `rust-analyzer`, `typescript-language-server`, and `jdtls` all implement both).

---

# Symbols & Diagnostics

## Document Symbols / Outline View

_VS Code feature: `Ctrl+Shift+O`, Outline sidebar panel._

| Status | 🟢 working · `lsp-treemacs-symbols` + `imenu` |
| --- | --- |
| LSP method | `textDocument/documentSymbol` |

| Action | Command | Keybinding |
| --- | --- | --- |
| Outline sidebar (tree, persistent) | `lsp-treemacs-symbols` | `C-c l G s` |
| Jump-to-symbol (transient, fuzzy) | `consult-lsp-symbols` (buffer-scoped) | `C-c l g o` |
| Classic Imenu | `imenu` | `M-g i` |

```elisp
(define-key lsp-mode-map (kbd "C-c l G s") #'lsp-treemacs-symbols)
(define-key lsp-mode-map (kbd "C-c l g o") #'consult-lsp-symbols)
(setq lsp-treemacs-symbols-position-params
      '((side . right) (slot . 2) (window-width . 30)))  ; docks like VS Code's Outline
```

`lsp-mode` also populates `imenu-create-index-function` from `textDocument/documentSymbol`, so any Imenu-based tool (including `consult-imenu`) works without extra glue.

---

## Workspace Symbol Search

_VS Code feature: `Ctrl+T`, project-wide symbol search._

| Status | 🟢 working |
| --- | --- |
| LSP method | `workspace/symbol` |

```elisp
(define-key lsp-mode-map (kbd "C-c l g w") #'consult-lsp-symbols)  ; C-u prefix widens to all workspace folders
;; or the built-in, non-consult picker:
(define-key lsp-mode-map (kbd "C-c l g W") #'lsp-ivy-workspace-symbol) ; if using lsp-ivy instead
```

`consult-lsp-symbols` called with a `C-u` prefix searches the whole workspace rather than the current buffer, with live incremental narrowing as you type — the direct match for VS Code's `Ctrl+T`.

---

## Diagnostics & the Problems Panel (Flycheck)

_VS Code feature: red/yellow squiggles in the editor, aggregate Problems panel (`Ctrl+Shift+M`), both push (server-initiated) and pull (client-requested) diagnostics models._

| Status | 🟢 working · `flycheck` as the `lsp-mode` diagnostics backend |
| --- | --- |
| LSP methods | `textDocument/publishDiagnostics` (push), `textDocument/diagnostic` (pull, LSP 3.17) |

`lsp-mode` normalizes both the older push model and the newer pull model into the same `flycheck` checker (`lsp`), so you don't configure them separately — whichever the server advertises, `lsp-diagnostics-provider :flycheck` routes it into Flycheck's overlays, fringe icons, and mode-line count.

| Action | Command | Keybinding |
| --- | --- | --- |
| Next/previous error | `flycheck-next-error` / `flycheck-previous-error` | `M-n` / `M-p` (buffer-local remap below) |
| List all errors (Problems panel) | `flycheck-list-errors` | `C-c ! l` |
| List errors, fuzzy/searchable | `consult-flycheck` | `C-c ! L` |
| Show error at point | `flycheck-display-error-at-point` | `C-c ! d` |
| Explain error (docs) | `flycheck-explain-error-at-point` | `C-c ! e` |
| Select checker | `flycheck-select-checker` | `C-c ! s` |

```elisp
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled))
  (flycheck-idle-change-delay 0.5)
  (flycheck-display-errors-delay 0.2)
  (flycheck-indication-mode 'left-fringe)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)))

;; Fuzzy-searchable Problems panel via consult, closer to VS Code's
;; filterable list than the default flycheck-error-list buffer.
(use-package consult-flycheck
  :ensure t
  :after (consult flycheck)
  :bind (:map flycheck-mode-map ("C-c ! L" . consult-flycheck)))

;; Aggregate, project-wide error tree (VS Code Problems panel grouped
;; by file), via lsp-treemacs instead of/alongside flycheck-list-errors:
(define-key lsp-mode-map (kbd "C-c l G d") #'lsp-treemacs-errors-list)

;; Layer additional, faster linters (e.g. ruff, eslint) on top of LSP
;; diagnostics rather than replacing them:
(flycheck-add-next-checker 'lsp 'python-ruff)
```

### Troubleshooting

- **Diagnostics not appearing**: confirm `(lsp-diagnostics-provider :flycheck)` is set *before* `lsp-mode` first activates in a buffer, and that `flycheck-mode` is on (`M-x flycheck-mode`). Run `M-x flycheck-verify-setup` to see which checkers are considered and why one might be skipped.
- **Only see the first error in a file**: `flycheck-checker-error-threshold` defaults to 400; raise it for very noisy files/servers.
- **Push vs pull confusion**: `M-x lsp-describe-session` shows each server's advertised `diagnosticProvider` capability; you do not need to configure this manually, but it explains latency differences (pull-model servers refresh diagnostics only when Emacs asks, typically on idle/save).

---

## Inline Values

_VS Code feature: while paused in the debugger, variable values are shown inline next to their declarations._

| Status               | 🟢 working, debugger-scoped · handled by `dape`/`dap-mode`, not by `lsp-mode`                                                                     |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| LSP/DAP relationship | LSP defines `textDocument/inlineValue`, but in practice this is delivered by the DAP session while stopped at a breakpoint, not by the LSP server |

See [Breakpoints, Stepping, Variables, Watch, REPL](#breakpoints-stepping-variables-watch-repl) in the Debugging section — `dape`'s `dape-info` buffers and inline overlays are what actually reproduce this VS Code feature.

---

# Code Actions & Refactoring

## Code Actions, Quick Fixes & the Lightbulb

_VS Code feature: the 💡 lightbulb that appears next to a diagnostic or selection, offering quick fixes and refactorings via `Ctrl+.`._

| Status | 🟢 working · `lsp-ui-sideline` (lightbulb) + `lsp-execute-code-action` |
| --- | --- |
| LSP method | `textDocument/codeAction`, `codeAction/resolve` |

`lsp-mode` reproduces the lightbulb two ways simultaneously: a mode-line indicator (`lsp-modeline-code-actions-enable`) and an inline sideline icon/text at the point of the fixable diagnostic (`lsp-ui-sideline-show-code-actions`) — pick whichever is less visually noisy for you, or keep both.

| Action | Command | Keybinding |
| --- | --- | --- |
| Show/apply code actions at point | `lsp-execute-code-action` | `C-c l a a` |
| Show code actions (menu, `lsp-ui`) | `lsp-ui-sideline-apply-code-actions` | click the sideline icon, or `C-c l a l` |
| Quick fix specifically | `lsp-execute-code-action` (filters by `kind`) | same as above; server ranks fixes first |
| Organize imports | `lsp-organize-imports` | `C-c l a o` |

```elisp
(define-key lsp-mode-map (kbd "C-c l a a") #'lsp-execute-code-action)
(define-key lsp-mode-map (kbd "C-c l a o") #'lsp-organize-imports)

(setq lsp-modeline-code-actions-enable t
      lsp-modeline-code-actions-segments '(count icon name)
      lsp-ui-sideline-show-code-actions t)
```

---

## Rename Symbol

_VS Code feature: `F2`, project-wide rename with a live preview._

| Status | 🟢 working |
| --- | --- |
| LSP method | `textDocument/rename`, `textDocument/prepareRename` |

```elisp
(define-key lsp-mode-map (kbd "C-c l r") #'lsp-rename)
```

`lsp-rename` prompts in the minibuffer, pre-filled with the current symbol name (via `prepareRename` when the server supports it), then applies the resulting `WorkspaceEdit` across every affected file — including files not currently open, which are visited, edited, and saved automatically. For a live-preview, buffer-local rename instead (VS Code's "rename this symbol only, everywhere it's visible on screen" via `iedit`):

```elisp
(define-key global-map (kbd "C-;") #'iedit-mode)  ; buffer-local, non-LSP, instant preview
```

---

## Execute Command

_VS Code feature: extension/server-defined commands exposed via the Command Palette._

| Status | 🟢 working |
| --- | --- |
| LSP method | `workspace/executeCommand` |

```elisp
(define-key lsp-mode-map (kbd "C-c l c") #'lsp-execute-command)
;; Palette-style fuzzy access to *all* interactive commands, LSP or not:
(define-key global-map (kbd "M-x") #'execute-extended-command) ; already default; consider `vertico`/`marginalia` for fuzzy filtering
```

`lsp-execute-command` lists server-advertised commands (e.g. `gopls`'s `gopls.generate`, `rust-analyzer`'s `rust-analyzer.runSingle`) the same way VS Code surfaces extension commands scoped to the active language.

---

# Formatting & Editing (Apheleia)

`apheleia`, not `lsp-mode`'s own `textDocument/formatting`, is the formatter for this stack — same design decision VS Code makes when you install Prettier/Black instead of relying on a language server's built-in formatter: dedicated formatter binaries are faster, more configurable via project dotfiles (`.prettierrc`, `pyproject.toml`), and behave identically to your team's CI formatting checks. `apheleia` runs formatters asynchronously on `after-save-hook` and reconciles the result back into the buffer with an RCS-patch diff, so **point never jumps** and only the changed hunks touch the undo history — a real advantage over both VS Code's format-on-save (which does move the cursor on large diffs) and over calling `lsp-format-buffer` synchronously.

## Document & Range Formatting

_VS Code feature: `Shift+Alt+F` (format document), format-on-save, and "Format Selection"._

| Status | 🟢 working · `apheleia` (whole-buffer, async) + `lsp-format-region` (LSP range formatting fallback) |
| --- | --- |
| LSP methods (fallback path only) | `textDocument/formatting`, `textDocument/rangeFormatting` |

```elisp
(use-package apheleia
  :ensure t
  :config
  ;; Point apheleia at explicit formatter commands where the default
  ;; guess isn't what you want (defaults already cover black/prettier/
  ;; gofmt/rustfmt/clang-format/ocamlformat/etc. out of the box).
  (setf (alist-get 'python-mode apheleia-mode-alist) '(black))
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--stdin-filename" filepath "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff black))

  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (dolist (mode '(typescript-mode tsx-ts-mode js-mode web-mode json-mode))
    (setf (alist-get mode apheleia-mode-alist) '(prettier)))

  (apheleia-global-mode +1))
```

| Action | Command | Keybinding |
| --- | --- | --- |
| Format buffer (async, apheleia) | automatic on save; manual: `apheleia-format-buffer` | `C-c f f` |
| Format region only | `lsp-format-region` (LSP path — apheleia is whole-buffer only) | `C-c f r` |
| Toggle format-on-save for this buffer | `apheleia-mode` | `C-c f t` |
| Jump to the last formatter error | `apheleia-goto-error` | `C-c f e` |

```elisp
(define-key global-map (kbd "C-c f f") #'apheleia-format-buffer)
(define-key global-map (kbd "C-c f r") #'lsp-format-region)
(define-key global-map (kbd "C-c f t") #'apheleia-mode)
(define-key global-map (kbd "C-c f e") #'apheleia-goto-error)
```

### Why Apheleia instead of `lsp-format-buffer` on save

| Consideration | `apheleia` (chosen) | `lsp-format-buffer` on save |
| --- | --- | --- |
| Point stability | Preserved via RCS patch + alignment | Can jump to buffer start on large reformats |
| Speed | Async; buffer stays interactive | Synchronous LSP round-trip; can block on slow servers |
| Formatter parity with CI | Uses the exact same CLI binary (`black`, `prettier`…) your CI runs | Depends on the language server's bundled formatter, which may diverge from your `pyproject.toml`/`.prettierrc` |
| Works without a running LSP session | Yes — formatter is a plain subprocess | No — requires an active `lsp-mode` connection |

### Troubleshooting

- **Formatter silently does nothing**: `apheleia` skips missing formatters silently by design; run `M-x apheleia-format-buffer` interactively to surface the "command not found" message, or check `*apheleia-…*` process buffers.
- **Formatting fights with `.editorconfig`**: apheleia infers indent width from Emacs's own mode-local variables (`python-indent-offset`, `js-indent-level`, etc.) and passes them to the formatter's CLI flags where supported; align these with your `.editorconfig`/`.prettierrc` to avoid oscillating diffs.

---

## On-type Formatting

_VS Code feature: automatic re-indentation/formatting as you type (e.g. after typing `}` or `;`)._

| Status | 🟡 partial · LSP `textDocument/onTypeFormatting` via `lsp-mode`, `apheleia` intentionally does not do this |
| --- | --- |

```elisp
(setq lsp-enable-on-type-formatting t)   ; per-keystroke formatting, LSP-driven
```

`apheleia` is deliberately save-triggered only — running an external formatter binary on every keystroke would be far too slow to match VS Code's on-type formatting, so that specific behavior is left to `lsp-mode`'s own lighter-weight LSP request when a server implements it (`clangd`, `typescript-language-server`, and a handful of others do). Most users get equivalent results from Emacs's native `electric-indent-mode` (on by default) plus `apheleia` on save, with on-type formatting only worth enabling for servers/languages where it materially helps (C/C++ brace alignment is the common case).

---

## Folding Ranges

_VS Code feature: gutter chevrons to collapse functions/blocks/regions, `Ctrl+Shift+[`/`]`._

| Status | 🟢 working · `lsp-mode` folding driven by LSP, using `hideshow`/`treesit` as the fold engine |
| --- | --- |
| LSP method | `textDocument/foldingRange` |

```elisp
(setq lsp-enable-folding t
      lsp-folding-range-limit 100
      lsp-fold-notransparent-block-index nil)

(use-package hideshow
  :ensure nil ; built-in
  :hook (prog-mode . hs-minor-mode))

;; Alternatively, tree-sitter-driven folding (Emacs 29+ ts-modes),
;; works even without an LSP connection and is generally faster:
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :hook (prog-mode . treesit-fold-mode))
```

| Action | Command | Keybinding |
| --- | --- | --- |
| Fold/unfold at point | `lsp-toggle-fold` (or `treesit-fold-toggle`) | `C-c l f f` / `TAB` on a fold marker |
| Fold all | `hs-hide-all` / `treesit-fold-close-all` | `C-c l f a` |
| Unfold all | `hs-show-all` / `treesit-fold-open-all` | `C-c l f A` |

---

## Selection Range (Smart Expand/Shrink)

_VS Code feature: `Shift+Alt+Right` / `Shift+Alt+Left`, syntax-aware growing/shrinking selection._

| Status | 🟢 working |
| --- | --- |
| LSP method | `textDocument/selectionRange` |

```elisp
(define-key lsp-mode-map (kbd "C-M-=") #'lsp-extend-selection)   ; grow selection
;; Emacs has no built-in "shrink to previous selection range" symmetric
;; to VS Code's; expand-region provides the closer, more mature UX and
;; also works without an LSP connection (uses syntax, not just LSP):
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))
```

---

## Linked Editing Range

_VS Code feature: editing an opening HTML/JSX tag name simultaneously edits the matching closing tag._

| Status | 🟢 working · `lsp-mode` + tree-sitter fallback |
| --- | --- |
| LSP method | `textDocument/linkedEditingRange` |

```elisp
(setq lsp-enable-linked-editing t)
```

For HTML/JSX servers that don't implement `linkedEditingRange` (some don't), tree-sitter-based tag renaming is a reasonable fallback in Emacs 30+'s `html-ts-mode`/`tsx-ts-mode` via `combobulate` or `evil-matchit`, though neither is a perfect substitute for the live two-cursor sync VS Code performs.

---

## Multi-Cursor Editing

_VS Code feature: `Ctrl+D` add next match to selection, `Alt+Click` add cursor, `Ctrl+Alt+Down/Up` add cursor above/below._

| Status | 🟢 working · `multiple-cursors` (+ `iedit` for the "rename all matches in view" case) |
| --- | --- |
| Not LSP-driven | this is a pure editing feature, same as in VS Code |

| VS Code action | Emacs equivalent | Keybinding |
| --- | --- | --- |
| `Ctrl+D` (select next occurrence) | `mc/mark-next-like-this` | `C-c m d` |
| `Ctrl+Shift+L` (select all occurrences) | `mc/mark-all-like-this` | `C-c m a` |
| `Ctrl+Alt+Down`/`Up` (cursor below/above) | `mc/mark-next-lines` / `mc/mark-previous-lines` | `C-c m n` / `C-c m p` |
| `Alt+Click` (cursor at click) | `mc/add-cursor-on-click` | `C-M-mouse-1` |
| Edit all visible matches at once (in-place) | `iedit-mode` | `C-;` |

```elisp
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m d" . mc/mark-next-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m n" . mc/mark-next-lines)
         ("C-c m p" . mc/mark-previous-lines)
         ("C-<mouse-1>" . mc/add-cursor-on-click)))

(use-package iedit :ensure t :bind ("C-;" . iedit-mode))
```

---

# Visual Enhancements & UI

## Document Highlight

_VS Code feature: all occurrences of the symbol under the cursor are subtly highlighted in the current file._

| Status | 🟢 working |
| --- | --- |
| LSP method | `textDocument/documentHighlight` |

```elisp
(setq lsp-enable-symbol-highlighting t)
(custom-set-faces
 '(lsp-face-highlight-textual ((t (:inherit highlight))))
 '(lsp-face-highlight-read ((t (:inherit highlight :underline t))))
 '(lsp-face-highlight-write ((t (:inherit highlight :weight bold)))))
```

---

## Document Links & Document Color

_VS Code feature: clickable links inside strings/comments (`Ctrl+click`), and inline color swatches next to CSS/hex color literals with a picker._

| Status | 🟢 working (both) |
| --- | --- |
| LSP methods | `textDocument/documentLink`, `textDocument/documentColor` |

```elisp
(setq lsp-enable-links t)          ; document links, opened with lsp-ui or plain browse-url
(setq lsp-enable-color-decorators t) ; inline color previews
```

Clicking a detected link (`lsp-ui` renders these as clickable overlays) invokes `browse-url`; color decorators render as small colored squares before the literal, and `M-x lsp-ui-doc-show` at that point exposes the underlying color picker where the server supports `colorPresentation` requests (CSS/SCSS/Less servers commonly do).

---

## Bracket Pair Colorization

_VS Code feature: matching bracket pairs colored to make nesting visually obvious._

| Status | 🟢 working · not an LSP feature in either editor — pure client-side highlighting |
| --- | --- |

```elisp
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Emacs 27+ built-in bracket-pair *matching* (not coloring by depth,
;; but highlights the matching partner, like VS Code's non-colorized mode):
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-style 'mixed)
```

`rainbow-delimiters` colors nesting depth (closer to VS Code's "Bracket Pair Colorization" setting); `show-paren-mode` highlights only the matching pair at point (closer to VS Code's default, non-colorized bracket matching). Most configurations run both.

---

## Minimap

_VS Code feature: scaled-down code overview on the right edge of the editor, with a draggable viewport indicator._

| Status | 🟡 partial — no LSP involvement; two imperfect options |
| --- | --- |

```elisp
;; Option A: dedicated minimap package (renders actual scaled text,
;; closest visual match to VS Code, but can be a performance drag on
;; very large files since it re-renders on every scroll):
(use-package minimap
  :ensure t
  :commands minimap-mode
  :custom
  (minimap-window-location 'right)
  (minimap-width-fraction 0.08)
  (minimap-minimum-width 20))

;; Option B: treemacs' built-in "indent-guides + occur outline" is not
;; a true minimap, but combined with lsp-treemacs-symbols docked to
;; the right, it gives comparable at-a-glance structure navigation
;; with much lower overhead — the pragmatic choice for large files.
```

### Caveats

Neither option is as fast as VS Code's GPU-rendered minimap; `minimap.el` in particular has known slowdowns on files over a few thousand lines. If pure performance parity matters more than visual fidelity, skip the minimap and rely on `lsp-treemacs-symbols` + `imenu` for structural orientation instead.

---

## Breadcrumbs Bar

_VS Code feature: header showing the file path and, past it, the enclosing symbol scope (`namespace > class > method`), each segment clickable/navigable._

| Status | 🟢 working · `lsp-headerline-breadcrumb-mode` |
| --- | --- |
| LSP methods | derived from `textDocument/documentSymbol` |

```elisp
(setq lsp-headerline-breadcrumb-enable t
      lsp-headerline-breadcrumb-enable-diagnostics t   ; show error/warning counts inline
      lsp-headerline-breadcrumb-icons-enable t
      lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
```

Segments are mouse-clickable and keyboard-navigable (`lsp-headerline-breadcrumb-go-to-symbol`), matching VS Code's breadcrumb bar exactly, including live updates as point moves through nested scopes.

---

## Sticky Scroll

_VS Code feature: the enclosing function/class signature "sticks" to the top of the viewport while scrolling through its body._

| Status | 🟢 working · Emacs 30+ built-in `sticky-scroll-mode`, or `lsp-mode`'s own implementation |
| --- | --- |

```elisp
;; Emacs 30+ ships a general-purpose, indentation-based sticky-scroll
;; that doesn't require an LSP connection:
(when (fboundp 'sticky-scroll-mode)
  (add-hook 'prog-mode-hook #'sticky-scroll-mode))

;; lsp-mode also provides a semantic, symbol-aware variant if you
;; want the sticky header to always reflect the LSP-defined enclosing
;; symbol rather than pure indentation:
(setq lsp-headerline-breadcrumb-enable t)  ; breadcrumbs already approximate this
```

If your Emacs predates the built-in `sticky-scroll-mode`, `topsy.el` is the community package most people reach for; it derives the sticky header from `imenu`/tree-sitter rather than LSP, so it works even without `lsp-mode` active.

---

# Debugging (Debug Adapter Protocol)

This is the one major capability the original `eglot`-based document didn't cover at all — Eglot has no bundled debugger. Both `dape` and `dap-mode` speak the same Debug Adapter Protocol VS Code itself uses, meaning you use the *exact same debug adapter binaries* VS Code extensions install (`debugpy`, `codelldb`, `vscode-js-debug`, `delve`'s `dap` mode, etc.).

## Debugging Overview: `dape` vs `dap-mode`

| | `dape` (recommended default) | `dap-mode` |
| --- | --- | --- |
| Maintenance status (mid-2026) | Actively maintained, in GNU ELPA, editor-agnostic core | Actively maintained, part of the `emacs-lsp` org, tightly coupled to `lsp-mode` |
| Dependencies | None beyond Emacs itself (works with `eglot` or `lsp-mode` or neither) | Requires `lsp-mode` for `${workspaceFolder}` and project context |
| Config style | Small, transparent `dape-configs` plist per adapter; edit inline before each launch | `dap-register-debug-template`, heavier VS Code-`launch.json`-style templates |
| Batteries included | Good defaults for Python, Go (`dlv`), Rust/C/C++ (`codelldb`, `lldb-dap`, `gdb`), JS/TS (`js-debug`), Bash | Very large template library across ~20 languages (Java, Elixir, Ruby, PHP, Dart/Flutter, etc.), some more mature than dape's equivalents |
| UI | Minimal: inline overlays for variable values, a REPL, `dape-info` buffers | Fuller VS Code-style UI: `dap-ui-mode` with dedicated locals/breakpoints/sessions panes, plus an optional Hydra |
| Best fit | Most day-to-day debugging (Python, Go, Rust, C/C++, JS/TS, Bash) with a lighter footprint | Languages with a mature dap-mode-specific template (Java/`jdtls`, Elixir, Dart/Flutter) or if you want the fuller multi-pane UI |

**Recommendation:** install `dape` as the default; add `dap-mode` alongside it only for the specific languages where its template library is meaningfully ahead (Java via `dap-java`, Elixir, Dart/Flutter). They coexist without conflict since neither claims global keybindings by default.

```elisp
;; ---- dape (primary) ----
(use-package dape
  :ensure t
  :preface
  ;; Silence the compile-time "-Wdocstrings" noise dape's macros emit
  (setq dape-inline-variables t)     ; VS Code-style inline value overlays while stopped
  :hook
  ((dape-on-start . (lambda () (save-some-buffers t)))
   (dape-display-source . pulse-momentary-highlight-one-line))
  :custom
  (dape-buffer-window-arrangement 'right)  ; docks REPL/locals/breakpoints like VS Code's debug sidebar
  (dape-info-hide-mode-line nil)
  (dape-cwd-fn 'projectile-project-root))  ; or `project-root` if not using projectile

;; ---- dap-mode (supplemental, only for languages that need it) ----
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-features '(sessions locals breakpoints controls tooltip))
  :config
  (dap-ui-mode 1)
  (require 'dap-java)    ; example: only require the adapters you actually use
  (require 'dap-cpptools))
```

---

## Breakpoints, Stepping, Variables, Watch, REPL

| VS Code action | `dape` command | `dap-mode` command | Keybinding (suggested, `C-c d` prefix) |
| --- | --- | --- | --- |
| Start debugging (`F5`) | `dape` | `dap-debug` | `C-c d d` |
| Toggle breakpoint (`F9`) | `dape-breakpoint-toggle` | `dap-breakpoint-toggle` | `C-c d b` |
| Conditional breakpoint | `dape-breakpoint-expression` | `dap-breakpoint-condition` | `C-c d B` |
| Log point | `dape-breakpoint-log` | `dap-breakpoint-log-message` | `C-c d l` |
| Continue (`F5`) | `dape-continue` | `dap-continue` | `C-c d c` |
| Step Over (`F10`) | `dape-next` | `dap-next` | `C-c d n` |
| Step Into (`F11`) | `dape-step-in` | `dap-step-in` | `C-c d i` |
| Step Out (`Shift+F11`) | `dape-step-out` | `dap-step-out` | `C-c d o` |
| Restart (`Ctrl+Shift+F5`) | `dape-restart` | `dap-debug-restart` | `C-c d r` |
| Stop (`Shift+F5`) | `dape-quit` | `dap-disconnect` | `C-c d q` |
| Evaluate expression / watch | `dape-evaluate-expression` | `dap-eval` | `C-c d e` |
| Inspect variable at point | `dape-info` (variables buffer) | `dap-ui-locals` | `C-c d v` |
| Call stack | `dape-info` (stack buffer) | `dap-ui-sessions` | `C-c d s` |
| REPL | `dape-repl` | `dap-ui-repl` | `C-c d R` |

```elisp
(with-eval-after-load 'dape
  (define-key global-map (kbd "C-c d d") #'dape)
  (define-key global-map (kbd "C-c d b") #'dape-breakpoint-toggle)
  (define-key global-map (kbd "C-c d B") #'dape-breakpoint-expression)
  (define-key global-map (kbd "C-c d l") #'dape-breakpoint-log)
  (define-key global-map (kbd "C-c d c") #'dape-continue)
  (define-key global-map (kbd "C-c d n") #'dape-next)
  (define-key global-map (kbd "C-c d i") #'dape-step-in)
  (define-key global-map (kbd "C-c d o") #'dape-step-out)
  (define-key global-map (kbd "C-c d r") #'dape-restart)
  (define-key global-map (kbd "C-c d q") #'dape-quit)
  (define-key global-map (kbd "C-c d e") #'dape-evaluate-expression)
  (define-key global-map (kbd "C-c d R") #'dape-repl))

;; repeat-mode turns the stepping commands into single-key repeats
;; after the first invocation (press "n" repeatedly after C-c d n),
;; matching the low-friction feel of clicking VS Code's debug toolbar.
(repeat-mode 1)
;; enable eldoc in source buffers for inline variable-value hints
;; while stopped at a breakpoint (this is dape's Inline Values equivalent)
(add-hook 'dape-on-stopped-hook #'eldoc-mode)
```

Inline variable values while stopped (`dape-inline-variables t`, set above) render each in-scope variable's current value as a subtle overlay right after its declaration on screen — this is the direct equivalent of VS Code's "Inline Values" debugging feature referenced earlier in the Diagnostics section.

---

## Per-Language Debug Configuration

`dape-configs` (inspect with `M-x describe-variable dape-configs`) ships pre-built entries; the ones below are the common day-to-day set, shown as explicit overrides for clarity.

```elisp
(with-eval-after-load 'dape
  ;; ---- Python (debugpy) ----
  (add-to-list 'dape-configs
    `(debugpy
      modes (python-mode python-ts-mode)
      command "python3"
      command-args ("-m" "debugpy.adapter")
      :request "launch"
      :type "python"
      :cwd dape-cwd-fn
      :program dape-buffer-default
      :justMyCode nil))

  ;; ---- Go (delve) ----
  ;; already in dape-configs as `dlv`; shown for reference:
  ;; (dlv modes (go-mode go-ts-mode) command "dlv"
  ;;      command-args ("dap" "--listen" "127.0.0.1::autoport")
  ;;      port :autoport :request "launch" :type "debug" :cwd "." :program ".")

  ;; ---- Rust / C / C++ (codelldb) ----
  (add-to-list 'dape-configs
    `(codelldb-cc
      modes (c-mode c-ts-mode c++-mode c++-ts-mode rust-mode rust-ts-mode)
      command "codelldb"
      command-args ("--port" :autoport)
      port :autoport
      :type "lldb"
      :request "launch"
      :cwd dape-cwd-fn
      :program dape-find-file-buffer-default))

  ;; ---- JavaScript / TypeScript (vscode-js-debug) ----
  ;; install: download js-debug-dap-<version>.tar.gz from
  ;; microsoft/vscode-js-debug releases into ~/.emacs.d/debug-adapters/
  (add-to-list 'dape-configs
    `(js-debug-node
      modes (js-mode js-ts-mode typescript-mode typescript-ts-mode)
      host "localhost"
      port :autoport
      command "node"
      command-args (,(expand-file-name "~/.emacs.d/debug-adapters/js-debug/src/dapDebugServer.js")
                     :autoport)
      :type "pwa-node"
      :request "launch"
      :cwd dape-cwd-fn
      :program dape-buffer-default)))
```

For languages `dap-mode` covers more completely (Java, Elixir, Dart/Flutter):

```elisp
(with-eval-after-load 'dap-mode
  (require 'dap-java)
  (dap-register-debug-template "Java Run"
    (list :type "java" :request "launch" :name "Java::Run"))

  (require 'dap-cpptools) ; alternative native adapter to codelldb, Microsoft's cpptools
  (dap-cpptools-setup))   ; downloads the VS Code cpptools extension bundle
```

### Troubleshooting

- **`dape: command not found`**: the debug adapter binary is separate from the language server; `debugpy`/`codelldb`/`delve` must be installed and on `$PATH` independently — `M-x dape-ensure-command` on a config will report exactly what's missing.
- **Breakpoints show as unverified (hollow circle)**: the adapter hasn't attached yet, or the file wasn't compiled with debug symbols — for compiled languages, confirm your build includes `-g`/`--debug` equivalent, exactly as VS Code's `launch.json` would require.
- **`dap-mode` and `dape` both bound to breakpoint toggling**: keep them under separate prefixes (as above) or only load one package's keymap per major mode via `:hook`/`:after`, to avoid ambiguity about which session a keybinding controls.

---

# Workspace & File Management

## Workspace File-Operation Hooks (LSP-aware rename)

_VS Code feature: renaming a file in the Explorer automatically updates every import statement that referenced it._

| Status | 🟢 working natively · `lsp-mode` (no custom advice needed, unlike an Eglot-only setup) |
| --- | --- |
| LSP methods | `workspace/willRenameFiles`, `workspace/didRenameFiles`, `workspace/willCreateFiles`, `workspace/willDeleteFiles` |

`lsp-mode` implements the `workspace/willRenameFiles` / `didRenameFiles` file-operation notifications directly, so a custom wrapper (necessary with a bare Eglot setup) isn't required here — `lsp-mode` already advises the rename path used by `dired` and `treemacs`.

| Action | Command | Keybinding |
| --- | --- | --- |
| Rename current file (LSP-aware) | `lsp-rename-file` | `C-c l f R` |
| Rename via `dired` (also LSP-aware) | `dired-do-rename` | `R` in `dired` |
| Rename via `treemacs` (also LSP-aware) | `treemacs-rename-file` | `R` in `treemacs` |

```elisp
(setq lsp-enable-file-watchers t
      lsp-file-watch-threshold 4000)

(define-key lsp-mode-map (kbd "C-c l f R") #'lsp-rename-file)

;; Optional: chain with Emacs's native VC rename so Git registers a
;; move (preserving history) at the same time imports are fixed:
(define-advice lsp-rename-file (:after (&rest _) vc-register-rename)
  "After an LSP-aware rename, let VC know about the move."
  (when (vc-registered (buffer-file-name))
    (vc-refresh-state)))
```

### Troubleshooting

- **Imports not updating**: not every language server implements `willRenameFiles` — check `M-x lsp-describe-session` capabilities; `gopls`, `typescript-language-server`, and `rust-analyzer` support it, some smaller servers don't, exactly as with the Eglot-based custom wrapper this replaces.
- **Large monorepos feel slow on rename**: this is a `workspace/willRenameFiles` round-trip cost on the server side, not an Emacs-side bottleneck; it scales with how many files the server has to re-analyze, same as VS Code experiences on the same repo.

---

# Appendix — Performance Tuning for VS Code-Level Responsiveness

`lsp-mode`'s larger feature surface (relative to Eglot) means it needs more deliberate tuning to *feel* as fast as VS Code, especially on large monorepos. This appendix collects the tuning knobs that matter most, in the order they're worth doing.

### 1. Raise `read-process-output-max` and GC threshold

The single highest-leverage change. Emacs's default 4KB process-output read chunk is far too small for verbose JSON-RPC servers like `rust-analyzer`, `gopls`, or `tsserver`.

```elisp
(setq read-process-output-max (* 3 1024 1024)  ; 3MB
      gc-cons-threshold (* 256 1024 1024))
;; Consider `gcmh` to automatically lower the threshold back down
;; during idle time rather than leaving it permanently high:
(use-package gcmh :ensure t :init (gcmh-mode 1))
```

### 2. Use plists instead of hash tables for LSP deserialization

```elisp
(setq lsp-use-plists t)
```

`lsp-mode`'s benchmarks show plist-based deserialization meaningfully outperforms the default hash-table/`json-read` path, particularly on the highly nested JSON that semantic-tokens and completion responses produce.

### 3. Install `emacs-lsp-booster`

A small Rust binary that wraps the language server process, pre-parsing its JSON-RPC output into Emacs's native fast-path bytecode format before Emacs ever sees it — the biggest win available for JSON-heavy servers, and works with both `lsp-mode` and Eglot.

```elisp
;; 1. Install the emacs-lsp-booster binary and put it on $PATH
;;    (build from github.com/blahgeek/emacs-lsp-booster, or grab a release binary)
;; 2. Wire it into lsp-mode's process-launch path:
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  (or (when (equal (following-char) ?#)
        (let ((bytecode (read (current-buffer))))
          (when (byte-code-function-p bytecode) (funcall bytecode))))
      (apply old-fn args)))
(advice-add (if (progn (require 'json) (fboundp 'json-parse-buffer))
                'json-parse-buffer 'json-read)
            :around #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)
             (not (file-remote-p default-directory))
             lsp-use-plists
             (executable-find "emacs-lsp-booster"))
        (cons "emacs-lsp-booster" orig-result)
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
```

Verify it's active with `M-x lsp-workspace-show-log` — the first lines should reference `emacs_lsp_booster`.

### 4. Trim `lsp-mode`'s UI surface on large files

Every one of `lsp-lens`, semantic tokens, sideline diagnostics, and inlay hints costs a bit of idle-timer work; on files over a few thousand lines, disabling the least essential ones buys back responsiveness:

```elisp
(defun my/lsp-large-file-tuning ()
  (when (> (buffer-size) 100000)
    (setq-local lsp-lens-enable nil
                lsp-semantic-tokens-enable nil
                lsp-ui-sideline-enable nil)))
(add-hook 'lsp-mode-hook #'my/lsp-large-file-tuning)
```

### 5. Restrict file watchers on huge monorepos

`lsp-mode` watches the whole workspace for change notifications by default; on repos with hundreds of thousands of files this alone can dominate startup time.

```elisp
(setq lsp-file-watch-threshold 10000)
(dolist (dir '("[/\\\\]\\.git\\'" "[/\\\\]node_modules\\'" "[/\\\\]target\\'"
               "[/\\\\]\\.venv\\'" "[/\\\\]dist\\'" "[/\\\\]build\\'"))
  (push dir lsp-file-watch-ignored-directories))
```

### 6. Prefer `lsp-deferred` and per-project server reuse

```elisp
(setq lsp-keep-workspace-alive t   ; don't tear down the server just because the last buffer closed
      lsp-idle-delay 0.2)          ; balance responsiveness vs. server request volume
```

### 7. Native compilation

Native comp (`package-native-compile t`, and building Emacs itself `--with-native-compilation`) meaningfully lowers the interpretive overhead of `lsp-mode`'s (large) Elisp codebase on every request; this is the single biggest non-LSP-specific lever if you're still on a non-natively-compiled Emacs.

With these seven changes applied, `lsp-mode` on a mid-size (100k–500k LOC) TypeScript or Rust project is competitive with VS Code's own extension-host latency for completion, hover, and diagnostics — the remaining gap, where one exists, is almost always in the language server itself (e.g. `rust-analyzer`'s initial workspace indexing), not in the Emacs client.
