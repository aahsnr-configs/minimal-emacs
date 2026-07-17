Filename: problems-panel.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Problems Panel</title>
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
          <span>Diagnostics &amp; Symbols</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Problems Panel</span>
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
          <h1>Problems Panel</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Diagnostics &amp; Symbols</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >"Problems" tab aggregating workspace diagnostics with severity
            filtering</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/publishDiagnostics</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>flymake<span
                class="route-arrow"
                >→</span
              >consult-flymake / flymake-show-project-diagnostics</code
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
                      <td>Centralized Problems panel</td>
                      <td>
                        <kbd>SPC c P</kbd>
                        (<code>flymake-show-project-diagnostics</code>) or
                        <kbd>SPC c E</kbd>
                        (<code>consult-flymake-project</code>).
                      </td>
                    </tr>
                    <tr>
                      <td>Filter by severity (Errors / Warnings)</td>
                      <td>
                        Native <kbd>/</kbd> filter in tabulated-list +
                        <code>consult-flymake</code> for fuzzy matching.
                      </td>
                    </tr>
                    <tr>
                      <td>Click error to jump to location</td>
                      <td>
                        <kbd>RET</kbd> on any row in the diagnostics buffer, or
                        fringe/margin clicks.
                      </td>
                    </tr>
                    <tr>
                      <td>"Quick Fix" from panel</td>
                      <td>
                        <kbd>SPC c a</kbd> (<code>eglot-code-actions</code>) at
                        the diagnostic location.
                      </td>
                    </tr>
                    <tr>
                      <td>Squiggles under erroneous code</td>
                      <td>
                        <code>flymake</code> renders squiggles via
                        <code>flymake-error</code> /
                        <code>flymake-warning</code>
                        faces.
                      </td>
                    </tr>
                    <tr>
                      <td>Inline message on hover</td>
                      <td>
                        Emacs 31 <code>'fancy</code> end-of-line rendering
                        displays the message contextually.
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
                    Natively intercepts
                    <code>textDocument/publishDiagnostics</code> and routes the
                    payload directly into <code>flymake</code>'s API, requiring
                    zero custom translation layers.
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
                        <circle cx="11" cy="11" r="8" />
                        <line x1="21" y1="21" x2="16.65" y2="16.65" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">consult</div>
                      <div class="eco-sub">Fuzzy Filtering</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>consult-flymake</code> leverages
                    <code>vertico</code> and <code>orderless</code> to provide
                    instant, space-separated fuzzy filtering of diagnostic
                    messages, complete with live buffer previews.
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
                        <path d="M12 2v20M2 12h20" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">doom-modeline</div>
                      <div class="eco-sub">Status Bar Integration</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    The <code>doom-modeline-lsp</code> segment automatically
                    displays live error and warning counts in the mode line,
                    mirroring the VS Code status bar badge.
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
                        <rect x="2" y="4" width="20" height="16" rx="2" />
                        <path
                          d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">evil-collection</div>
                      <div class="eco-sub">Modal Navigation</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides Unimpaired-style <kbd>[ e</kbd> / <kbd>] e</kbd>
                    bracket navigation for rapidly cycling through errors
                    without leaving normal state.
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
                    <div class="stack-role">LSP Client</div>
                    <div class="stack-desc">
                      Built-in. Receives
                      <code>textDocument/publishDiagnostics</code> payloads and
                      translates them into native <code>flymake</code>
                      diagnostics.
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
                        d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                      />
                      <polyline points="14 2 14 8 20 8" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">flymake</div>
                    <div class="stack-role">Diagnostic Engine</div>
                    <div class="stack-desc">
                      Built-in. Manages the lifecycle of diagnostics, rendering
                      squiggles and aggregating them into tabulated lists.
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
                      <path d="M4 6h16M4 12h16M4 18h10" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">
                      flymake-show-project-diagnostics
                    </div>
                    <div class="stack-role">Workspace Aggregator</div>
                    <div class="stack-desc">
                      Emacs 31 NEW. Natively lists all pulled diagnostics across
                      the entire workspace in a single, filterable tabulated
                      buffer.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
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
                      <circle cx="11" cy="11" r="8" />
                      <line x1="21" y1="21" x2="16.65" y2="16.65" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">consult-flymake</div>
                    <div class="stack-role">Fuzzy Filtering</div>
                    <div class="stack-desc">
                      Provides a Vertico-powered, live-preview dropdown for
                      rapidly searching and jumping to diagnostics across the
                      buffer or project.
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
                      <td>Buffer diagnostics search</td>
                      <td><code>consult-flymake</code></td>
                      <td><kbd>SPC c e</kbd></td>
                      <td>
                        Fuzzy-searches diagnostics in the current buffer with
                        live preview.
                      </td>
                    </tr>
                    <tr>
                      <td>Project diagnostics search</td>
                      <td><code>consult-flymake-project</code></td>
                      <td><kbd>SPC c E</kbd></td>
                      <td>
                        Fuzzy-searches all diagnostics across the entire
                        workspace.
                      </td>
                    </tr>
                    <tr>
                      <td>Project diagnostics panel</td>
                      <td><code>flymake-show-project-diagnostics</code></td>
                      <td><kbd>SPC c P</kbd></td>
                      <td>
                        Emacs 31 NEW — opens a centralized, filterable tabulated
                        list of all project errors.
                      </td>
                    </tr>
                    <tr>
                      <td>Next error</td>
                      <td><code>flymake-goto-next-error</code></td>
                      <td><kbd>SPC c n</kbd> / <kbd>] e</kbd></td>
                      <td>
                        Jumps to the next diagnostic in the current buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Previous error</td>
                      <td><code>flymake-goto-prev-error</code></td>
                      <td><kbd>SPC c p</kbd> / <kbd>[ e</kbd></td>
                      <td>
                        Jumps to the previous diagnostic in the current buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Force recheck</td>
                      <td><code>flymake-start</code></td>
                      <td><kbd>SPC c !</kbd></td>
                      <td>Manually triggers a diagnostic refresh.</td>
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
                    <span class="fname">init-problems-panel.el</span>
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
 ;; FLYMAKE CORE (Emacs 31 Native Diagnostics)
 ;; ==========================================
 (use-package flymake
   :ensure nil
   :custom
   ;; Emacs 31 NEW: 'fancy renders Unicode arrow graphics below the affected line.
   (flymake-show-diagnostics-at-end-of-line 'fancy)
   ;; Emacs 31 NEW: 'auto prefers fringes on GUI frames, falls back to margins on TTY.
   (flymake-indicator-type 'auto)
   ;; Suppress the legacy echo-area summary to keep the minibuffer clean.
   (flymake-suppress-zero-count-warnings t)
   :config
   ;; Enable flymake globally in programming buffers.
   (add-hook 'prog-mode-hook #'flymake-mode))
 ;; ==========================================
 ;; CONSULT-FLYMAKE (Vertico-powered filtering)
 ;; ==========================================
 (use-package consult-flymake
   :ensure nil  ;; Bundled with consult
   :after (consult flymake)
   :config
   ;; Wrapper for project-wide diagnostic searching.
   (defun consult-flymake-project ()
     "Invoke consult-flymake across all project buffers."
     (interactive)
     (consult-flymake t)))
 ;; ==========================================
 ;; GENERAL.EL KEYBINDINGS (registered eagerly)
 ;; ==========================================
 (ar/global-leader
   "c" '(:ignore t :wk "code")
   "c e" '(consult-flymake :wk "Search errors (buffer)")
   "c E" '(consult-flymake-project :wk "Search errors (project)")
   "c n" '(flymake-goto-next-error :wk "Next error")
   "c p" '(flymake-goto-prev-error :wk "Prev error")
   "c !" '(flymake-start :wk "Force recheck")
   "c P" '(flymake-show-project-diagnostics :wk "Project diagnostics panel"))
 (general-define-key
   :states 'motion
   "] e" #'flymake-goto-next-error
   "[ e" #'flymake-goto-prev-error)</code></pre>
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
                  <h4>✓ flymake native · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP client coupling</span>
                      <span class="val"
                        >Works seamlessly with <code>eglot</code> out of the
                        box.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol compliance</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Emacs 31 synergy</span>
                      <span class="val"
                        >Leverages new
                        <code>flymake-show-project-diagnostics</code> for native
                        workspace aggregation.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Built into Emacs core; zero additional packages or
                        background processes.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui / lsp-mode · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP client coupling</span>
                      <span class="val"
                        >Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol compliance</span>
                      <span class="val"
                        >Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Emacs 31 synergy</span>
                      <span class="val"
                        >Relies on legacy, third-party workspace diagnostic
                        buffers.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Heavy child-frame overhead on every diagnostic
                        update.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="enh-card g">
                  <div class="enh-title">
                    flymake-show-project-diagnostics (NEW)
                  </div>
                  <p class="desc">
                    A game-changer for workspace diagnostics. Instead of
                    querying files individually, this Emacs 31 command requests
                    and lists every diagnostic across the entire workspace in
                    one
                    <code>*Flymake diagnostics*</code> tabulated buffer,
                    perfectly mirroring VS Code's Problems panel.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">
                    flymake-show-diagnostics-at-end-of-line 'fancy
                  </div>
                  <p class="desc">
                    Instead of truncating messages in the echo area, Emacs 31
                    lays out diagnostics below the affected line using Unicode
                    graphics that point back to the exact locus of the error.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Dynamic Column Widths</div>
                  <p class="desc">
                    The tabulated list dynamically adjusts column widths to fit
                    content, preventing truncation of long file paths or verbose
                    LSP diagnostic messages in monorepos.
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
                      <td>Project Diagnostics Panel is Empty</td>
                      <td>
                        Verify Project Context:
                        <code>flymake-show-project-diagnostics</code> requires
                        an active <code>project-current</code>. Ensure your
                        project has a <code>.git</code> directory or a
                        <code>pyproject.toml</code>/<code>package.json</code> at
                        the root. Check Eglot Connection: Ensure
                        <code>eglot</code> is actively connected.
                      </td>
                    </tr>
                    <tr>
                      <td>Diagnostics Feel Laggy</td>
                      <td>
                        If workspace diagnostic requests cause noticeable lag on
                        massive projects, rely on buffer-local pulls (<kbd
                          >SPC c !</kbd
                        >) for immediate feedback, and reserve
                        <code>consult-flymake-project</code> (<kbd>SPC c E</kbd
                        >) for targeted, fuzzy-filtered searches rather than
                        full workspace refreshes.
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

Filename: sticky-scroll.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Sticky Scroll</title>
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
        <button class="nav active" data-tip="Definition" aria-current="page">
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
        <button class="nav" data-tip="Code Actions">
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
          <span>Navigation &amp; Visual Enhancements</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Sticky Scroll</span>
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
          <h1>Sticky Scroll</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Navigation &amp; Visual Enhancements</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Sticky Scroll (keeps relevant scope headers in view while scrolling
            through large files)</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <span style="color: var(--text-dim); font-size: 12px"
              >Indirectly leverages <code>textDocument/documentSymbol</code> via
              eglot → imenu enrichment</span
            >
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>imenu / treesit<span
                class="route-arrow"
                >→</span
              >topsy or sticky-scroll-mode<span class="route-arrow">→</span
              >header-line overlay</code
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
                      <td>Pins function/class name at top of viewport</td>
                      <td>
                        <code>topsy-mode</code> displays the enclosing
                        definition in the header line.
                      </td>
                    </tr>
                    <tr>
                      <td>Multi-line sticky scroll for nested blocks</td>
                      <td>
                        <code>sticky-scroll-mode</code> (Option B) pins multiple
                        indentation levels simultaneously.
                      </td>
                    </tr>
                    <tr>
                      <td>Updates dynamically while scrolling</td>
                      <td>
                        Both packages hook into
                        <code>window-scroll-functions</code> to update the
                        header instantly.
                      </td>
                    </tr>
                    <tr>
                      <td>Clickable header to jump to definition</td>
                      <td>
                        <code>topsy</code> headers can be made clickable, or
                        users can rely on
                        <kbd>M-g M-i</kbd> (<code>imenu</code>).
                      </td>
                    </tr>
                    <tr>
                      <td>Works across all major modes</td>
                      <td>
                        Hooks into <code>prog-mode</code> and
                        <code>text-mode</code>, covering Python, Rust,
                        TypeScript, Org, etc.
                      </td>
                    </tr>
                    <tr>
                      <td>No UI jitter or text shifting</td>
                      <td>
                        Uses native header-line or overlay rendering, keeping
                        the buffer text perfectly stable.
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
                    Automatically populates
                    <code>imenu-create-index-function</code> with LSP document
                    symbols, which <code>topsy</code> consumes natively without
                    any manual bridging.
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
                          d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">treesit</div>
                      <div class="eco-sub">AST Foundation</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides the foundational AST that
                    <code>sticky-scroll-mode</code> can query for precise
                    structural boundaries, ensuring the sticky lines align
                    perfectly with syntactic scopes.
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
                        <path d="M12 2v20M2 12h20" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">doom-modeline</div>
                      <div class="eco-sub">UI Harmony</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    By routing the sticky content to the
                    <code>header-line-format</code> (or a dedicated overlay),
                    the bottom mode line remains uncluttered, allowing
                    <code>doom-modeline</code> to focus on Git status and LSP
                    diagnostics.
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
                        <rect x="2" y="4" width="20" height="16" rx="2" />
                        <path
                          d="M6 8h.01M10 8h.01M14 8h.01M18 8h.01M8 12h.01M12 12h.01M16 12h.01M7 16h10"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">general.el</div>
                      <div class="eco-sub">Keybindings</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Eagerly registers the <kbd>SPC t s</kbd> leader binding,
                    providing a consistent, mnemonic toggle for sticky scroll
                    across all programming buffers.
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
                    <div class="stack-role">LSP Client</div>
                    <div class="stack-desc">
                      Built-in. Enriches the native <code>imenu</code> index
                      with deep, nested document symbols, providing the
                      structural data needed for accurate scoping.
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
                      <path d="M4 6h16M4 12h16M4 18h10" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">topsy</div>
                    <div class="stack-role">Definition Engine</div>
                    <div class="stack-desc">
                      GNU ELPA package. A lightweight sticky header that shows
                      which definition the top line of the window is within.
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
                      <rect x="3" y="3" width="18" height="18" rx="2" />
                      <path d="M9 9h6v6H9z" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">sticky-scroll-mode</div>
                    <div class="stack-role">Indentation Engine</div>
                    <div class="stack-desc">
                      MELPA alternative. Uses an indentation-based approach to
                      find offscreen lines that are levels of indentation lower
                      than the current point.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
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
                      <path d="M12 2v20M2 12h20" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">header-line-format</div>
                    <div class="stack-role">Rendering Surface</div>
                    <div class="stack-desc">
                      Draws the sticky content at the very top of the window
                      without shifting the main buffer's text or causing
                      redisplay jitter.
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
                      <td>Toggle sticky scroll</td>
                      <td>
                        <code>topsy-mode</code> /
                        <code>sticky-scroll-mode</code>
                      </td>
                      <td><kbd>SPC t s</kbd></td>
                      <td>
                        Enables/disables the sticky header for the current
                        buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Toggle globally</td>
                      <td><code>global-topsy-mode</code></td>
                      <td>—</td>
                      <td>
                        Enables sticky headers across all programming buffers.
                      </td>
                    </tr>
                    <tr>
                      <td>Jump to enclosing symbol</td>
                      <td><code>imenu</code></td>
                      <td><kbd>M-g M-i</kbd></td>
                      <td>
                        Native fallback to jump directly to the symbol currently
                        pinned in the sticky header.
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
                    <span class="fname">init-sticky-scroll.el</span>
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
 ;; OPTION A: TOPSY (Definition-Based Sticky Header)
 ;; Recommended for its simplicity and perfect synergy with eglot's imenu.
 ;; ==========================================
 (use-package topsy
   :ensure t
   :hook ((prog-mode text-mode) . topsy-mode)
   :custom
   ;; Maximum number of lines the sticky header can occupy (for multi-line signatures).
   (topsy-max-header-lines 3)
   :config
   ;; Ensure topsy uses the enriched imenu data provided by eglot.
   (add-to-list 'topsy-mode-functions #'imenu--make-index-alist))
 ;; ==========================================
 ;; OPTION B: STICKY-SCROLL-MODE (Indentation-Based)
 ;; Uncomment to use VS Code-style multi-line indentation tracking instead of topsy.
 ;; ==========================================
 ;; (use-package sticky-scroll-mode
 ;;   :ensure t
 ;;   :hook ((prog-mode text-mode) . sticky-scroll-mode)
 ;;   :custom
 ;;   ;; Maximum number of sticky lines to display at the top of the viewport.
 ;;   (sticky-scroll-max-lines 3)
 ;;   ;; Use treesit indentation if available, falling back to standard indentation.
 ;;   (sticky-scroll-use-treesit t))
 ;; ==========================================
 ;; GENERAL.EL KEYBINDINGS (registered eagerly)
 ;; ==========================================
 (ar/global-leader
   "t" '(:ignore t :wk "toggle")
   "t s" '(topsy-mode :wk "Toggle sticky scroll"))</code></pre>
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
                  <h4>✓ topsy / sticky-scroll-mode · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP client coupling</span>
                      <span class="val"
                        >Agnostic; works seamlessly with <code>eglot</code> and
                        native <code>imenu</code> / <code>treesit</code>.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol compliance</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Near-zero overhead. <code>topsy</code> only evaluates
                        the header when the window scrolls.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Visual Polish</span>
                      <span class="val"
                        >Renders cleanly in the header line, preserving the mode
                        line for diagnostics.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui / semantic-stickyfunc · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">LSP client coupling</span>
                      <span class="val"
                        >Hard-bound to the <code>lsp-mode</code> ecosystem or
                        legacy CEDET.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol compliance</span>
                      <span class="val"
                        >Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        ><code>semantic-stickyfunc-mode</code> is notoriously
                        slow and prone to freezing on large files.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Visual Polish</span>
                      <span class="val"
                        >Often clashes with <code>doom-modeline</code> or custom
                        header-line configurations.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2" style="margin-bottom: 24px">
                <div class="enh-card g">
                  <div class="enh-title">Eglot Imenu Enrichment</div>
                  <p class="desc">
                    As of recent <code>eglot</code> updates, managed buffers
                    receive extra region info added to the
                    <code>imenu</code> index, allowing <code>topsy</code> to
                    show richer, deeply nested paths (e.g.,
                    <code>Class › Method</code>) rather than flat, ambiguous
                    lists.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Treesit Indentation Awareness</div>
                  <p class="desc">
                    If using <code>sticky-scroll-mode</code>, Emacs 31's native
                    <code>treesit</code> integration allows the package to query
                    the AST for precise structural indentation, avoiding the
                    false positives that plagued legacy regex-based trackers.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Pixel-Perfect Scrolling</div>
                  <p class="desc">
                    Emacs 31's refined <code>pixel-scroll-precision-mode</code>
                    interacts smoothly with sticky headers, ensuring that the
                    header remains firmly anchored at the top of the window even
                    during smooth, fractional-line scrolling.
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
                      <td>Sticky Header Shows Incorrect or Flat Names</td>
                      <td>
                        Ensure <code>eglot</code> is actively connected and has
                        populated the <code>imenu</code> index. You can verify
                        this by running <kbd>M-x imenu</kbd> and checking if the
                        menu shows deeply nested structures.
                      </td>
                    </tr>
                    <tr>
                      <td>Header Flickers During Fast Scrolling</td>
                      <td>
                        If you experience visual flicker, ensure
                        <code>pixel-scroll-precision-mode</code> is enabled, or
                        increase the debounce/throttle in
                        <code>sticky-scroll-mode</code> (if using Option B).
                      </td>
                    </tr>
                    <tr>
                      <td>Multi-Line Signatures Are Truncated</td>
                      <td>
                        Increase <code>topsy-max-header-lines</code> to
                        <code>3</code> or <code>4</code> to accommodate lengthy
                        function signatures without clipping the sticky header.
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
