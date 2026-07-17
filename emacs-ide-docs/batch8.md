Filename: range-formatting.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Range Formatting</title>
    <link
      href="https://cdn.jsdelivr.net/npm/@fontsource/jetbrains-mono@5.0.18/index.min.css"
      rel="stylesheet"
    />
    <link
      href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css"
      rel="stylesheet"
    />
    <link rel="stylesheet" href="shared-styles.css" />
  </head>
  <body>
    <div class="overlay" id="overlay" aria-hidden="true"></div>
    <div class="focus-hint" id="focusHint" aria-live="polite">
      <kbd>ESC</kbd> <span>Exit Focus Mode</span>
    </div>
    <aside class="sidebar" id="sb" aria-label="Main Navigation">
      <div class="sidebar-head">
        <svg
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          aria-hidden="true"
        >
          <path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5" />
        </svg>
        <span class="brand">Emacs IDE</span>
      </div>
      <nav class="sidebar-nav" aria-label="Sidebar Menu">
        <button class="nav" data-tip="IntelliSense">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M9.5 2A2.5 2.5 0 0 1 12 4.5v15a2.5 2.5 0 0 1-4.96.44 2.5 2.5 0 0 1-2.96-3.08 3 3 0 0 1-.34-5.58 2.5 2.5 0 0 1 1.32-4.24A2.5 2.5 0 0 1 9.5 2z"
            />
            <path
              d="M14.5 2A2.5 2.5 0 0 0 12 4.5v15a2.5 2.5 0 0 0 4.96.44 2.5 2.5 0 0 0 2.96-3.08 3 3 0 0 0 .34-5.58 2.5 2.5 0 0 0-1.32-4.24A2.5 2.5 0 0 0 14.5 2z"
            />
          </svg>
          <span class="nav-label">IntelliSense</span>
        </button>
        <button class="nav" data-tip="Hover Info">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <circle cx="12" cy="12" r="10" />
            <path d="M12 16v-4M12 8h.01" />
          </svg>
          <span class="nav-label">Hover Info</span>
        </button>
        <button class="nav" data-tip="Signature Help">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
            />
            <polyline points="14 2 14 8 20 8" />
          </svg>
          <span class="nav-label">Signature Help</span>
        </button>
        <button class="nav" data-tip="Definition">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <circle cx="11" cy="11" r="8" />
            <line x1="21" y1="21" x2="16.65" y2="16.65" />
          </svg>
          <span class="nav-label">Definition</span>
        </button>
        <button class="nav active" data-tip="Code Actions" aria-current="page">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M9 18h6M10 22h4M12 2a7 7 0 0 0-4 12.7V17h8v-2.3A7 7 0 0 0 12 2z"
            />
          </svg>
          <span class="nav-label">Code Actions</span>
        </button>
      </nav>
      <div class="sidebar-foot">
        <button
          class="nav"
          id="sbToggle"
          data-tip="Toggle Sidebar"
          aria-label="Toggle Sidebar"
        >
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <rect x="3" y="3" width="18" height="18" rx="2" />
            <line x1="9" y1="3" x2="9" y2="21" />
          </svg>
          <span class="nav-label">Collapse Menu</span>
        </button>
      </div>
    </aside>
    <header class="topbar">
      <div class="topbar-left">
        <button class="hamburger" id="mobileMenuBtn" aria-label="Open Menu">
          <svg
            viewBox="0 0 24 24"
            width="24"
            height="24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <line x1="3" y1="12" x2="21" y2="12"></line>
            <line x1="3" y1="6" x2="21" y2="6"></line>
            <line x1="3" y1="18" x2="21" y2="18"></line>
          </svg>
        </button>
        <div class="crumbs" aria-label="Breadcrumb">
          <span>Docs</span><span class="s" aria-hidden="true">/</span>
          <span>Formatting &amp; Editing</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Range Formatting</span>
        </div>
      </div>
      <button
        class="icon-btn"
        id="focusBtn"
        title="Toggle Focus Mode (ESC)"
        aria-label="Toggle Focus Mode"
      >
        <svg
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          aria-hidden="true"
        >
          <path d="M15 3h6v6M9 21H3v-6M21 3l-7 7M3 21l7-7" />
        </svg>
      </button>
    </header>
    <main class="main" id="main-content">
      <header class="page-head">
        <div class="title-row">
          <h1>Range Formatting (Format Selection)</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Formatting &amp; Editing</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >"Format Selection" command — format active region/selection
            only</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/rangeFormatting</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>eglot-format (with region)
              OR lazy-ruff<span class="route-arrow">→</span>ruff format
              --range</code
            >
          </div>
        </div>
      </header>
      <article class="acc">
        <button
          class="acc-head open"
          aria-expanded="true"
          aria-controls="sect-overview"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <rect x="3" y="3" width="7" height="7" />
              <rect x="14" y="3" width="7" height="7" />
              <rect x="14" y="14" width="7" height="7" />
              <rect x="3" y="14" width="7" height="7" />
            </svg>
            Feature Overview
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body open" id="sect-overview" role="region">
          <div>
            <div class="acc-inner">
              <div class="sec-title">Behavioral Parity Matrix</div>
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>VS Code Behavior</th>
                      <th>Emacs 31 Equivalent</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td><kbd>Shift+Alt+F</kbd> formats selection</td>
                      <td>
                        <kbd>SPC c f</kbd> with an active visual region triggers
                        <code>lazy-ruff-format-region</code> or
                        <code>eglot-format</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Respects <code>.ruff.toml</code> config</td>
                      <td>
                        <code>lazy-ruff</code> automatically inherits the
                        project's Ruff configuration from the workspace root.
                      </td>
                    </tr>
                    <tr>
                      <td>Formats embedded code (e.g., Jupyter/Org)</td>
                      <td>
                        <code>lazy-ruff-format-org-src</code> handles Python
                        blocks inside Org-mode seamlessly.
                      </td>
                    </tr>
                    <tr>
                      <td>No main-thread blocking</td>
                      <td>
                        CLI execution is asynchronous or near-instantaneous due
                        to Ruff's Rust-based speed.
                      </td>
                    </tr>
                    <tr>
                      <td>Fallback to whole-file if no region</td>
                      <td>
                        If no region is active, the command gracefully falls
                        back to <code>apheleia-format-buffer</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Cursor stability after format</td>
                      <td>
                        Region-based formatting inherently preserves the rest of
                        the buffer's undo history and cursor position.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-ecosystem"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <circle cx="12" cy="12" r="10" />
              <path
                d="M2 12h20M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"
              />
            </svg>
            Ecosystem Integration
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-ecosystem" role="region">
          <div>
            <div class="acc-inner">
              <div class="grid-2">
                <div class="eco-card">
                  <div class="eco-top">
                    <div class="eco-ic">
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <circle cx="12" cy="12" r="3" />
                        <path d="M12 1v6m0 6v6" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eglot</div>
                      <div class="eco-sub">LSP Client</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Natively supports
                    <code>textDocument/rangeFormatting</code> when an active
                    region is present, delegating to the language server for
                    non-Python languages.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <path
                          d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">lazy-ruff</div>
                      <div class="eco-sub">Python CLI Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Lightweight integration that invokes the Ruff CLI directly
                    (<code>ruff format --range</code>) for marked regions and
                    org src blocks, bypassing LSP entirely for zero-latency
                    Python formatting.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <path
                          d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                        />
                        <polyline points="14 2 14 8 20 8" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">org-mode</div>
                      <div class="eco-sub">Literate Programming</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>lazy-ruff</code> explicitly supports
                    <code>org-src</code> blocks, making it the definitive choice
                    for literate programming workflows where Python snippets are
                    embedded in documentation.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <path
                          d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">apheleia</div>
                      <div class="eco-sub">Whole-file Fallback</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    For languages where a dedicated CLI range formatter isn't
                    available, <code>apheleia</code> remains the gold standard
                    for whole-file async formatting, while
                    <code>eglot</code> handles the LSP range formatting gap.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-stack"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <path
                d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"
              />
            </svg>
            Implementation Stack
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-stack" role="region">
          <div>
            <div class="acc-inner">
              <div class="grid-2">
                <div class="stack-card">
                  <div class="stack-ic">
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <circle cx="12" cy="12" r="3" />
                      <path d="M12 1v6m0 6v6" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">eglot</div>
                    <div class="stack-role">LSP Range Formatting</div>
                    <div class="stack-desc">
                      Built-in. Natively supports
                      <code>textDocument/rangeFormatting</code> when an active
                      region is present, delegating to the language server.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    "
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <path
                        d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"
                      />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">lazy-ruff</div>
                    <div class="stack-role">Python CLI Range</div>
                    <div class="stack-desc">
                      Invokes the Ruff CLI directly (<code
                        >ruff format --range</code
                      >) for marked regions, bypassing LSP entirely for
                      zero-latency Python formatting.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    "
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <path
                        d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                      />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">treesit / expreg</div>
                    <div class="stack-role">AST Region Precision</div>
                    <div class="stack-desc">
                      Combined with <code>expreg</code> or
                      <code>evil-textobj-tree-sitter</code>, you can select
                      precise AST nodes (e.g., a single function) and format
                      only that structural block.
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-commands"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <rect x="2" y="4" width="20" height="16" rx="2" />
              <path
                d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"
              />
            </svg>
            Commands &amp; Keybindings
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-commands" role="region">
          <div>
            <div class="acc-inner">
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>Action</th>
                      <th>Command</th>
                      <th>Keybinding</th>
                      <th>Notes</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>Format active region (LSP)</td>
                      <td><code>eglot-format</code></td>
                      <td><kbd>SPC c f</kbd> (with region)</td>
                      <td>
                        Eglot automatically detects the active region and
                        requests <code>textDocument/rangeFormatting</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Format region (Ruff CLI)</td>
                      <td><code>lazy-ruff-format-region</code></td>
                      <td><kbd>SPC c f</kbd> (with region)</td>
                      <td>
                        Invokes <code>ruff format --range</code> on the selected
                        text, ideal for Python without LSP overhead.
                      </td>
                    </tr>
                    <tr>
                      <td>Format org src block</td>
                      <td><code>lazy-ruff-format-org-src</code></td>
                      <td><kbd>C-c C-c</kbd> (in src block)</td>
                      <td>
                        Specifically targets Python code blocks within Org-mode
                        files.
                      </td>
                    </tr>
                    <tr>
                      <td>Fallback to whole-file</td>
                      <td><code>apheleia-format-buffer</code></td>
                      <td><kbd>SPC c f</kbd> (no region)</td>
                      <td>
                        If no region is active, the binding gracefully falls
                        back to whole-file async formatting.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-config"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <polyline points="16 18 22 12 16 6" />
              <polyline points="8 6 2 12 8 18" />
            </svg>
            Configuration
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-config" role="region">
          <div>
            <div class="acc-inner">
              <div class="code-win">
                <div class="code-head">
                  <div style="display: flex; align-items: center">
                    <div class="dots" aria-hidden="true">
                      <span></span><span></span><span></span>
                    </div>
                    <span class="fname">init-range-formatting.el</span>
                  </div>
                  <button
                    class="copy"
                    aria-label="Copy code snippet"
                    onclick="copyCode(this)"
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <rect x="9" y="9" width="13" height="13" rx="2" />
                      <path
                        d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
                      />
                    </svg>
                    Copy
                  </button>
                </div>
                <pre><code class="language-lisp">;; ==========================================
;; 1. LAZY-RUFF (Python CLI Range Formatting)
;; ==========================================
(use-package lazy-ruff
  :ensure t
  :defer t
  :commands (lazy-ruff-format-region
             lazy-ruff-format-buffer
             lazy-ruff-format-org-src)
  :custom
  ;; Target only the active region when a region is selected.
  (lazy-ruff-only-format-region t)
  ;; Pass specific arguments to Ruff CLI if needed (e.g., config file).
  (lazy-ruff-args '("format" "--quiet"))
  :config
  ;; Bind to a convenient key for region formatting
  (general-define-key
   :states '(normal visual)
   "SPC c f" #'lazy-ruff-format-region))

;; ==========================================
;; 2. EGLOT NATIVE RANGE FORMATTING (Fallback/Other Languages)
;; ==========================================
;; Eglot natively handles region formatting. If a region is active,
;; `eglot-format` automatically sends `textDocument/rangeFormatting`.
;; No extra configuration is needed beyond the base `eglot` setup.</code></pre>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-arch"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <path
                d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"
              />
            </svg>
            Architecture &amp; Enhancements
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-arch" role="region">
          <div>
            <div class="acc-inner">
              <div class="sec-title">Why This Approach?</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="vs-card ok">
                  <h4>✓ lazy-ruff / eglot · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP Coupling</span>
                      <span class="val"
                        ><code>lazy-ruff</code> requires zero LSP integration,
                        using pure CLI.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Ruff CLI is written in Rust and formats ranges in
                        milliseconds.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate (or
                        bypasses it cleanly via CLI).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Org-mode Synergy</span>
                      <span class="val"
                        >Natively supports formatting Python code inside Org src
                        blocks.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ Legacy python-format / Heavy LSP UI · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP Coupling</span>
                      <span class="val"
                        >Tightly coupled to <code>lsp-mode</code> or specific
                        language servers.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Synchronous LSP requests can block the main thread on
                        large files.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Often requires forbidden
                        <code>lsp-mode</code> ecosystem packages.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Org-mode Synergy</span>
                      <span class="val"
                        >Most LSP formatters struggle with embedded code
                        blocks.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="enh-card g">
                  <div class="enh-title">Native Region Awareness</div>
                  <p class="enh-desc">
                    Emacs 31's refined <code>eglot</code> implementation
                    seamlessly detects <code>(region-active-p)</code> and routes
                    to <code>textDocument/rangeFormatting</code> without
                    requiring separate commands.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Tree-sitter Region Precision</div>
                  <p class="enh-desc">
                    When combined with <code>expreg</code> or
                    <code>evil-textobj-tree-sitter</code>, you can select
                    precise AST nodes (e.g., a single function or class) and
                    format only that structural block, avoiding the fragility of
                    text-based line selection.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">
                    Ruff CLI <code>--range</code> Support
                  </div>
                  <p class="enh-desc">
                    Modern Ruff versions natively support the
                    <code>--range</code> flag (e.g.,
                    <code>--range=10:1-20:1</code>), allowing the CLI to format
                    specific line ranges without touching the rest of the file.
                    <code>lazy-ruff</code> translates Emacs region bounds into
                    this exact CLI syntax.
                  </p>
                </div>
              </div>
              <div class="sec-title">Troubleshooting</div>
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>Issue</th>
                      <th>Cause &amp; Solution</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>Region Formatting Falls Back to Whole File</td>
                      <td>
                        Ensure <code>lazy-ruff-only-format-region</code> is set
                        to <code>t</code>, or verify that the region is actively
                        highlighted (e.g., via <code>evil-visual-state</code>)
                        before invoking the command.
                      </td>
                    </tr>
                    <tr>
                      <td>Ruff CLI Not Found</td>
                      <td>
                        <code>lazy-ruff</code> requires the
                        <code>ruff</code> binary to be in your system's
                        <code>exec-path</code>. Verify this with
                        <kbd>M-x exec-path</kbd> or install it via your OS
                        package manager.
                      </td>
                    </tr>
                    <tr>
                      <td>Org Src Block Formatting Fails</td>
                      <td>
                        Ensure the source block is explicitly declared as
                        <code>python</code> (e.g.,
                        <code>#+begin_src python</code>).
                        <code>lazy-ruff</code> uses the block's language
                        identifier to route the formatting command correctly.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
    </main>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-lisp.min.js"></script>
    <script src="shared-scripts.js"></script>
  </body>
</html>
```

Filename: on-type-formatting.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — On-type Formatting</title>
    <link
      href="https://cdn.jsdelivr.net/npm/@fontsource/jetbrains-mono@5.0.18/index.min.css"
      rel="stylesheet"
    />
    <link
      href="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/themes/prism-tomorrow.min.css"
      rel="stylesheet"
    />
    <link rel="stylesheet" href="shared-styles.css" />
  </head>
  <body>
    <div class="overlay" id="overlay" aria-hidden="true"></div>
    <div class="focus-hint" id="focusHint" aria-live="polite">
      <kbd>ESC</kbd> <span>Exit Focus Mode</span>
    </div>
    <aside class="sidebar" id="sb" aria-label="Main Navigation">
      <div class="sidebar-head">
        <svg
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          aria-hidden="true"
        >
          <path d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5" />
        </svg>
        <span class="brand">Emacs IDE</span>
      </div>
      <nav class="sidebar-nav" aria-label="Sidebar Menu">
        <button class="nav" data-tip="IntelliSense">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M9.5 2A2.5 2.5 0 0 1 12 4.5v15a2.5 2.5 0 0 1-4.96.44 2.5 2.5 0 0 1-2.96-3.08 3 3 0 0 1-.34-5.58 2.5 2.5 0 0 1 1.32-4.24A2.5 2.5 0 0 1 9.5 2z"
            />
            <path
              d="M14.5 2A2.5 2.5 0 0 0 12 4.5v15a2.5 2.5 0 0 0 4.96.44 2.5 2.5 0 0 0 2.96-3.08 3 3 0 0 0 .34-5.58 2.5 2.5 0 0 0-1.32-4.24A2.5 2.5 0 0 0 14.5 2z"
            />
          </svg>
          <span class="nav-label">IntelliSense</span>
        </button>
        <button class="nav" data-tip="Hover Info">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <circle cx="12" cy="12" r="10" />
            <path d="M12 16v-4M12 8h.01" />
          </svg>
          <span class="nav-label">Hover Info</span>
        </button>
        <button class="nav" data-tip="Signature Help">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
            />
            <polyline points="14 2 14 8 20 8" />
          </svg>
          <span class="nav-label">Signature Help</span>
        </button>
        <button class="nav" data-tip="Definition">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <circle cx="11" cy="11" r="8" />
            <line x1="21" y1="21" x2="16.65" y2="16.65" />
          </svg>
          <span class="nav-label">Definition</span>
        </button>
        <button class="nav active" data-tip="Code Actions" aria-current="page">
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <path
              d="M9 18h6M10 22h4M12 2a7 7 0 0 0-4 12.7V17h8v-2.3A7 7 0 0 0 12 2z"
            />
          </svg>
          <span class="nav-label">Code Actions</span>
        </button>
      </nav>
      <div class="sidebar-foot">
        <button
          class="nav"
          id="sbToggle"
          data-tip="Toggle Sidebar"
          aria-label="Toggle Sidebar"
        >
          <svg
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <rect x="3" y="3" width="18" height="18" rx="2" />
            <line x1="9" y1="3" x2="9" y2="21" />
          </svg>
          <span class="nav-label">Collapse Menu</span>
        </button>
      </div>
    </aside>
    <header class="topbar">
      <div class="topbar-left">
        <button class="hamburger" id="mobileMenuBtn" aria-label="Open Menu">
          <svg
            viewBox="0 0 24 24"
            width="24"
            height="24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <line x1="3" y1="12" x2="21" y2="12"></line>
            <line x1="3" y1="6" x2="21" y2="6"></line>
            <line x1="3" y1="18" x2="21" y2="18"></line>
          </svg>
        </button>
        <div class="crumbs" aria-label="Breadcrumb">
          <span>Docs</span><span class="s" aria-hidden="true">/</span>
          <span>Formatting &amp; Editing</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">On-type Formatting</span>
        </div>
      </div>
      <button
        class="icon-btn"
        id="focusBtn"
        title="Toggle Focus Mode (ESC)"
        aria-label="Toggle Focus Mode"
      >
        <svg
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          aria-hidden="true"
        >
          <path d="M15 3h6v6M9 21H3v-6M21 3l-7 7M3 21l7-7" />
        </svg>
      </button>
    </header>
    <main class="main" id="main-content">
      <header class="page-head">
        <div class="title-row">
          <h1>On-type Formatting</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Formatting &amp; Editing</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Auto-indent and auto-pairing on trigger characters (
            <code>}</code>, <code>;</code>, <code>(</code>, etc.)</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/onTypeFormatting</code>
            <span class="meta-sep" aria-hidden="true">·</span>
            <span style="color: var(--text-dim); font-size: 12px"
              >(Explicitly ignored)</span
            >
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >electric-indent-mode + electric-pair-mode +
              electric-layout-mode</code
            >
          </div>
        </div>
      </header>
      <article class="acc">
        <button
          class="acc-head open"
          aria-expanded="true"
          aria-controls="sect-overview"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <rect x="3" y="3" width="7" height="7" />
              <rect x="14" y="3" width="7" height="7" />
              <rect x="14" y="14" width="7" height="7" />
              <rect x="3" y="14" width="7" height="7" />
            </svg>
            Feature Overview
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body open" id="sect-overview" role="region">
          <div>
            <div class="acc-inner">
              <div class="sec-title">Behavioral Parity Matrix</div>
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>VS Code Behavior</th>
                      <th>Emacs 31 Equivalent</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>
                        Auto-indents when typing <code>}</code> or
                        <code>;</code>
                      </td>
                      <td>
                        <code>electric-indent-mode</code> re-indents instantly
                        on trigger characters.
                      </td>
                    </tr>
                    <tr>
                      <td>
                        Auto-inserts closing <code>)</code> or <code>}</code>
                      </td>
                      <td>
                        <code>electric-pair-mode</code> inserts the matching
                        delimiter.
                      </td>
                    </tr>
                    <tr>
                      <td>Auto-inserts <code>/*</code> and <code>*/</code></td>
                      <td>
                        Emacs 31 <code>electric-pair-pairs</code> natively
                        supports multi-character string pairs.
                      </td>
                    </tr>
                    <tr>
                      <td>Adds newlines around <code>{</code> automatically</td>
                      <td>
                        <code>electric-layout-mode</code> enforces structural
                        newline rules.
                      </td>
                    </tr>
                    <tr>
                      <td>No lag or stutter while typing</td>
                      <td>
                        Native C-level execution guarantees 0ms latency, unlike
                        LSP network requests.
                      </td>
                    </tr>
                    <tr>
                      <td>Cursor stays in the correct position</td>
                      <td>
                        Native modes preserve point and mark perfectly; LSP
                        on-type formatting is known to displace the cursor.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-ecosystem"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <circle cx="12" cy="12" r="10" />
              <path
                d="M2 12h20M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"
              />
            </svg>
            Ecosystem Integration
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-ecosystem" role="region">
          <div>
            <div class="acc-inner">
              <div class="grid-2">
                <div class="eco-card">
                  <div class="eco-top">
                    <div class="eco-ic">
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <circle cx="12" cy="12" r="3" />
                        <path d="M12 1v6m0 6v6" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eglot</div>
                      <div class="eco-sub">LSP Guard</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    By adding <code>:documentOnTypeFormattingProvider</code> to
                    <code>eglot-ignored-server-capabilities</code>, we
                    proactively prevent the language server from attempting to
                    format on every keystroke, preserving a buttery-smooth
                    typing experience.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(187, 154, 247, 0.1);
                        color: var(--purple);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <path
                          d="M12 20h9M16.5 3.5a2.121 2.121 0 0 1 3 3L7 19l-4 1 1-4L16.5 3.5z"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">apheleia</div>
                      <div class="eco-sub">On-save Formatting</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    While <code>electric-*</code> modes handle on-type
                    structural formatting, <code>apheleia</code> handles on-save
                    whole-file formatting. This separation of concerns ensures
                    zero latency while typing.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(125, 207, 255, 0.1);
                        color: var(--cyan);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <rect x="2" y="4" width="20" height="16" rx="2" />
                        <path
                          d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">evil-mode</div>
                      <div class="eco-sub">Modal Compatibility</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Electric modes are fully compatible with Evil. Auto-pairing
                    and auto-indentation trigger correctly whether you are in
                    <code>insert-state</code> or using <kbd>R</kbd> (replace)
                    state.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(158, 206, 106, 0.1);
                        color: var(--green);
                      "
                    >
                      <svg
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        stroke-width="2"
                        aria-hidden="true"
                      >
                        <path
                          d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                        />
                        <polyline points="14 2 14 8 20 8" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">org-mode</div>
                      <div class="eco-sub">Literate Programming</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>electric-pair-mode</code> seamlessly auto-pairs
                    delimiters inside Org source blocks, improving the literate
                    programming experience without requiring LSP intervention.
                  </p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-stack"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <path
                d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"
              />
            </svg>
            Implementation Stack
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-stack" role="region">
          <div>
            <div class="acc-inner">
              <div class="grid-2">
                <div class="stack-card">
                  <div class="stack-ic">
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <path d="M4 7h16M4 12h10M4 17h16" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">electric-indent-mode</div>
                    <div class="stack-role">Auto-Indentation</div>
                    <div class="stack-desc">
                      Built-in. Re-indents the current line or block instantly
                      upon typing trigger characters like <kbd>RET</kbd>,
                      <code>}</code>, or <code>;</code> with zero network
                      latency.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="
                      background: rgba(187, 154, 247, 0.1);
                      color: var(--purple);
                    "
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <circle cx="12" cy="12" r="10" />
                      <path d="M8 12h8M12 8v8" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">electric-pair-mode</div>
                    <div class="stack-role">Auto-Pairing</div>
                    <div class="stack-desc">
                      Built-in. Automatically inserts closing delimiters
                      (<code>()</code>, <code>{}</code>, <code>[]</code>,
                      <code>""</code>). Emacs 31 natively extends this to
                      multi-character pairs (e.g., <code>/*</code> and
                      <code>*/</code>).
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="
                      background: rgba(125, 207, 255, 0.1);
                      color: var(--cyan);
                    "
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <path
                        d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                      />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">electric-layout-mode</div>
                    <div class="stack-role">Layout Control</div>
                    <div class="stack-desc">
                      Built-in. Dictates exactly where newlines are inserted
                      automatically (e.g., adding a newline before and after
                      <code>{</code> in C-like languages).
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="
                      background: rgba(247, 118, 142, 0.1);
                      color: var(--red);
                    "
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <circle cx="12" cy="12" r="10" />
                      <line x1="4.93" y1="4.93" x2="19.07" y2="19.07" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">
                      eglot-ignored-server-capabilities
                    </div>
                    <div class="stack-role">LSP Guard</div>
                    <div class="stack-desc">
                      Explicitly disables
                      <code>:documentOnTypeFormattingProvider</code> to prevent
                      main-thread blocking and cursor-jumping bugs from LSP
                      servers.
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-commands"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <rect x="2" y="4" width="20" height="16" rx="2" />
              <path
                d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"
              />
            </svg>
            Commands &amp; Keybindings
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-commands" role="region">
          <div>
            <div class="acc-inner">
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>Action</th>
                      <th>Command</th>
                      <th>Keybinding</th>
                      <th>Notes</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>Toggle auto-indent</td>
                      <td><code>electric-indent-mode</code></td>
                      <td><kbd>SPC t i</kbd></td>
                      <td>
                        Re-indents on trigger characters. Enabled globally by
                        default.
                      </td>
                    </tr>
                    <tr>
                      <td>Toggle auto-pairing</td>
                      <td><code>electric-pair-mode</code></td>
                      <td><kbd>SPC t p</kbd></td>
                      <td>
                        Inserts matching closing delimiters. Enabled globally by
                        default.
                      </td>
                    </tr>
                    <tr>
                      <td>Toggle layout rules</td>
                      <td><code>electric-layout-mode</code></td>
                      <td><kbd>SPC t l</kbd></td>
                      <td>
                        Enforces structural newline insertion (e.g., around
                        <code>{}</code>).
                      </td>
                    </tr>
                    <tr>
                      <td>Manually indent line</td>
                      <td><code>indent-according-to-mode</code></td>
                      <td><kbd>TAB</kbd></td>
                      <td>
                        Fallback manual indentation if electric modes miss a
                        trigger.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-config"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <polyline points="16 18 22 12 16 6" />
              <polyline points="8 6 2 12 8 18" />
            </svg>
            Configuration
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-config" role="region">
          <div>
            <div class="acc-inner">
              <div class="code-win">
                <div class="code-head">
                  <div style="display: flex; align-items: center">
                    <div class="dots" aria-hidden="true">
                      <span></span><span></span><span></span>
                    </div>
                    <span class="fname">init-on-type-formatting.el</span>
                  </div>
                  <button
                    class="copy"
                    aria-label="Copy code snippet"
                    onclick="copyCode(this)"
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <rect x="9" y="9" width="13" height="13" rx="2" />
                      <path
                        d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
                      />
                    </svg>
                    Copy
                  </button>
                </div>
                <pre><code class="language-lisp">;; ==========================================
;; 1. EGLOT: DISABLE LSP ON-TYPE FORMATTING
;; ==========================================
(use-package eglot
  :ensure nil
  :custom
  ;; Explicitly ignore onTypeFormatting to prevent main-thread blocking
  ;; and cursor-jumping issues on every keystroke.
  (eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

;; ==========================================
;; 2. NATIVE ELECTRIC MODES (Zero-Latency Formatting)
;; ==========================================
(use-package elec-pair
  :ensure nil
  :custom
  ;; Emacs 31 NEW: Support for multi-character paired delimiters.
  ;; Automatically pairs "/*" with "*/" and handles spacing intelligently.
  (electric-pair-pairs '(("/*" . "*/")))
  (electric-pair-text-pairs '(("/*" . "*/")))
  ;; Prevent pairing inside strings/comments where it causes syntax errors.
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (electric-pair-mode 1))

(use-package electric
  :ensure nil
  :custom
  ;; Re-indent automatically when typing trigger characters like '}' or ';'.
  (electric-indent-mode 1)
  ;; Enforce structural newlines (e.g., adding newlines around '{' in C/JS).
  (electric-layout-mode 1))</code></pre>
              </div>
            </div>
          </div>
        </div>
      </article>
      <article class="acc">
        <button
          class="acc-head"
          aria-expanded="false"
          aria-controls="sect-arch"
        >
          <span class="t">
            <svg
              class="ic"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              stroke-width="2"
              aria-hidden="true"
            >
              <path
                d="M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2zM22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"
              />
            </svg>
            Architecture &amp; Enhancements
          </span>
          <svg
            class="chev"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            aria-hidden="true"
          >
            <polyline points="6 9 12 15 18 9" />
          </svg>
        </button>
        <div class="acc-body" id="sect-arch" role="region">
          <div>
            <div class="acc-inner">
              <div class="sec-title">Why This Approach?</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="vs-card ok">
                  <h4>✓ Native electric-* modes · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Latency</span>
                      <span class="val"
                        ><b>0ms.</b> Executed in C-level Emacs core.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">UI Stability</span>
                      <span class="val"
                        ><b>Perfect.</b> No main-thread blocking or cursor
                        jumping.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack by explicitly
                        disabling problematic LSP features.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Undo History</span>
                      <span class="val"
                        ><b>Clean.</b> Native Emacs commands integrate
                        seamlessly with the undo tree.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ LSP textDocument/onTypeFormatting · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Latency</span>
                      <span class="val"
                        ><b>High.</b> Requires a synchronous network roundtrip
                        to the LSP server on every keystroke.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">UI Stability</span>
                      <span class="val"
                        ><b>Poor.</b> Known to cause severe UI stutter and
                        incorrect point placement.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Violates the principle of a responsive editor; heavily
                        discouraged in the eglot community.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Undo History</span>
                      <span class="val"
                        ><b>Fragmented.</b> LSP text edits applied mid-typing
                        often corrupt or split undo boundaries.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="enh-card g">
                  <div class="enh-title">
                    Multi-Character Electric Pairs (NEW)
                  </div>
                  <p class="enh-desc">
                    Emacs 31's <code>electric-pair-mode</code> now natively
                    supports multi-character paired delimiters. You can define
                    pairs like <code>("/*" . "*/")</code> in
                    <code>electric-pair-pairs</code>, and Emacs will
                    intelligently auto-complete them with optional auto-spacing,
                    a feature previously requiring heavy third-party packages
                    like <code>smartparens</code>.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Conservative Inhibition</div>
                  <p class="enh-desc">
                    The <code>electric-pair-conservative-inhibit</code>
                    predicate (standard in modern Emacs) prevents auto-pairing
                    from triggering inside strings or comments, eliminating the
                    "phantom bracket" syntax errors that plagued older
                    configurations.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Treesit Integration</div>
                  <p class="enh-desc">
                    Native electric modes work flawlessly alongside
                    <code>treesit</code> major modes (e.g.,
                    <code>python-ts-mode</code>, <code>rust-ts-mode</code>), as
                    they operate on the syntax table and character level,
                    completely independent of the LSP server's parsing speed.
                  </p>
                </div>
              </div>
              <div class="sec-title">Troubleshooting</div>
              <div class="tbl-wrap">
                <table class="tbl">
                  <thead>
                    <tr>
                      <th>Issue</th>
                      <th>Cause &amp; Solution</th>
                    </tr>
                  </thead>
                  <tbody>
                    <tr>
                      <td>Cursor Jumps or Stutters When Typing</td>
                      <td>
                        This is the hallmark symptom of LSP
                        <code>onTypeFormatting</code> being active. Verify that
                        <code>:documentOnTypeFormattingProvider</code> is
                        present in your
                        <code>eglot-ignored-server-capabilities</code> list.
                      </td>
                    </tr>
                    <tr>
                      <td>Auto-Pairing Triggers Inside Strings/Comments</td>
                      <td>
                        Ensure <code>electric-pair-inhibit-predicate</code> is
                        set to <code>'electric-pair-conservative-inhibit</code>.
                        This prevents Emacs from inserting closing quotes or
                        brackets where they would break string/comment syntax.
                      </td>
                    </tr>
                    <tr>
                      <td>Multi-Character Pairs Not Working</td>
                      <td>
                        Verify you are running Emacs 31 or later, as
                        multi-character string support in
                        <code>electric-pair-pairs</code> is a recent
                        enhancement. For older versions, third-party packages
                        like <code>smartparens</code> were required.
                      </td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </article>
    </main>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-lisp.min.js"></script>
    <script src="shared-scripts.js"></script>
  </body>
</html>
```
