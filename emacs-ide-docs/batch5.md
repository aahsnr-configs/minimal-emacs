Filename: find-all-references.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Find All References</title>
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
          <span>Navigation &amp; Code Jumping</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Find All References</span>
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
          <h1>Find All References</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Navigation &amp; Code Jumping</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Shift+F12 / "Find All References" centralized panel with editable
            results</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/references</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>xref-find-references<span
                class="route-arrow"
                >→</span
              >consult-xref OR native *xref* buffer</code
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
                      <td><kbd>Shift+F12</kbd> lists all references</td>
                      <td>
                        <kbd>M-?</kbd> or <kbd>SPC c D</kbd>
                        (<code>xref-find-references</code>)
                      </td>
                    </tr>
                    <tr>
                      <td>Centralized References panel</td>
                      <td>
                        <code>*xref*</code> buffer or
                        <code>consult-xref</code> vertico dropdown
                      </td>
                    </tr>
                    <tr>
                      <td>Click reference to jump to file</td>
                      <td>
                        <kbd>RET</kbd> on any row in the <code>*xref*</code>
                        buffer
                      </td>
                    </tr>
                    <tr>
                      <td>Filter references by file/path</td>
                      <td>
                        Type filename in <code>consult-xref</code> or use native
                        <kbd>/</kbd> filter in <code>*xref*</code>
                      </td>
                    </tr>
                    <tr>
                      <td>Edit multiple references simultaneously</td>
                      <td>
                        Emacs 31
                        <code>xref-change-to-xref-edit-mode</code> (<kbd>e</kbd>
                        in <code>*xref*</code> buffer)
                      </td>
                    </tr>
                    <tr>
                      <td><kbd>Alt+Left</kbd> returns to origin</td>
                      <td>
                        <kbd>M-,</kbd> (<code>xref-go-back</code>) via the
                        native xref history ring
                      </td>
                    </tr>
                    <tr>
                      <td><kbd>Ctrl+Click</kbd> on symbol</td>
                      <td>
                        Emacs 31 <code>global-xref-mouse-mode</code>
                        (<kbd>C-&lt;mouse-1&gt;</kbd>)
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
                    Automatically registers <code>eglot-xref-backend</code> in
                    <code>xref-backend-functions</code> for managed buffers,
                    routing <kbd>M-?</kbd> to
                    <code>textDocument/references</code>.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(187, 154, 247, 0.1); color: var(--purple);"
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
                    <div>
                      <div class="eco-name">consult</div>
                      <div class="eco-sub">Preview Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>consult-xref</code> intercepts the xref display
                    functions to provide vertico-powered previews for ambiguous
                    reference targets.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);"
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
                      <div class="eco-name">evil-collection</div>
                      <div class="eco-sub">Modal Navigation</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Standardizes <kbd>[</kbd> / <kbd>]</kbd> or
                    <kbd>g</kbd> motions across all major modes, ensuring Vim
                    muscle memory is preserved when navigating the
                    <code>*xref*</code> buffer.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(158, 206, 106, 0.1); color: var(--green);"
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
                    <div>
                      <div class="eco-name">apheleia</div>
                      <div class="eco-sub">Post-Edit Formatting</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    If bulk edits are made via
                    <code>xref-change-to-xref-edit-mode</code>, saving the
                    modified files automatically triggers
                    <code>apheleia</code> to format the updated code.
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
                      Built-in. Queries <code>textDocument/references</code> and
                      injects the location payloads into the native
                      <code>xref</code> framework.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(187, 154, 247, 0.1); color: var(--purple);"
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
                    <div class="stack-name">xref</div>
                    <div class="stack-role">Navigation Framework</div>
                    <div class="stack-desc">
                      Built-in. Manages location abstraction, shared history
                      ring, and cross-buffer jumping.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);"
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
                    <div class="stack-name">consult-xref</div>
                    <div class="stack-role">Preview Engine</div>
                    <div class="stack-desc">
                      Intercepts <code>xref-show-xrefs-function</code> to render
                      a vertico-powered dropdown with live buffer previews.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(158, 206, 106, 0.1); color: var(--green);"
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
                    <div class="stack-name">xref-edit-mode</div>
                    <div class="stack-role">Bulk Mutation</div>
                    <div class="stack-desc">
                      Emacs 31 NEW. Transforms the <code>*xref*</code> buffer
                      into a writable surface (Grep-Edit style) for simultaneous
                      edits.
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
                      <td>Find all references</td>
                      <td><code>xref-find-references</code></td>
                      <td><kbd>M-?</kbd> / <kbd>SPC c D</kbd></td>
                      <td>
                        Lists all usages; opens
                        <code>consult-xref</code> dropdown or
                        <code>*xref*</code> buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Go back (history)</td>
                      <td><code>xref-go-back</code></td>
                      <td><kbd>M-,</kbd> / <kbd>g ,</kbd></td>
                      <td>
                        Returns to the exact cursor position before the jump.
                      </td>
                    </tr>
                    <tr>
                      <td>Edit references in place</td>
                      <td><code>xref-change-to-xref-edit-mode</code></td>
                      <td><kbd>e</kbd> (in <code>*xref*</code>)</td>
                      <td>
                        Emacs 31 NEW — enables writable reference buffer for
                        bulk mutation.
                      </td>
                    </tr>
                    <tr>
                      <td>Next reference</td>
                      <td><code>xref-next-line</code></td>
                      <td><kbd>n</kbd> (in <code>*xref*</code>)</td>
                      <td>Navigates down the reference list.</td>
                    </tr>
                    <tr>
                      <td>Previous reference</td>
                      <td><code>xref-prev-line</code></td>
                      <td><kbd>p</kbd> (in <code>*xref*</code>)</td>
                      <td>Navigates up the reference list.</td>
                    </tr>
                    <tr>
                      <td>Filter references</td>
                      <td><code>consult-xref</code></td>
                      <td><kbd>SPC c E</kbd></td>
                      <td>
                        Fuzzy-filters the reference list with live preview.
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
                    <span class="fname">init-references.el</span>
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
;; 1. XREF &amp; CONSULT INTEGRATION (Preview Engine)
;; ==========================================
(use-package xref
  :ensure nil
  :custom
  ;; Route xref location prompts through Consult for live previews.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Emacs 31 NEW: Enable Ctrl+Click jump-to-definition globally.
  (global-xref-mouse-mode 1))

;; ==========================================
;; 2. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(general-define-key
  :states 'motion
  "g D" #'xref-find-references  ;; Go to References (Shift+F12 parity)
  "g ," #'xref-go-back)         ;; Go Back (Alt+Left)

(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c D" '(xref-find-references :wk "Find all references"))</code></pre>
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
                  <h4>✓ xref + consult-xref · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Works with <i>any</i> xref backend (eglot, dumb-jump,
                        etags).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Bulk Mutation</span>
                      <span class="val"
                        >Emacs 31 native
                        <code>xref-change-to-xref-edit-mode</code> (Grep-Edit
                        parity).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Preview</span>
                      <span class="val"
                        ><code>consult-xref</code> leverages
                        <code>vertico</code> for fuzzy filtering and live
                        previews.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui-references / lsp-mode · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Requires the forbidden
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Bulk Mutation</span>
                      <span class="val"
                        >Requires fragile third-party wrappers or manual text
                        replacement.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Preview</span>
                      <span class="val"
                        >Custom child-frame pipeline with heavy rendering
                        overhead.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">
                    xref-change-to-xref-edit-mode (NEW)
                  </div>
                  <p class="enh-desc">
                    Bound to <kbd>e</kbd> inside the <code>*xref*</code> buffer,
                    it transforms the read-only list into a writable surface.
                    Perform bulk text replacements across all listed references
                    simultaneously. Upon saving, edits propagate natively back
                    to the originating source files.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">xref-mouse-mode (NEW)</div>
                  <p class="enh-desc">
                    Emacs 31 introduces native mouse-driven code navigation.
                    Enabling <code>global-xref-mouse-mode</code> binds
                    <kbd>C-&lt;down-mouse-1&gt;</kbd> to xref jumps, perfectly
                    mirroring VS Code's Ctrl+Click convention.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">consult-xref synergy</div>
                  <p class="enh-desc">
                    Ambiguous references are presented in a highly performant,
                    searchable Vertico dropdown with instant buffer previews,
                    eliminating the need to cycle through blind
                    <code>*xref*</code> buffer splits.
                  </p>
                </div>
                <div class="enh-card g">
                  <div class="enh-title">Unified xref history</div>
                  <p class="enh-desc">
                    <code>xref-go-back</code> (<kbd>M-,</kbd>) treats reference
                    jumps identically to definition and implementation jumps —
                    the entire navigation chain is preserved in a single ring.
                  </p>
                </div>
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

Filename: document-highlight.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Document Highlight</title>
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
          <span class="cur" aria-current="page">Document Highlight</span>
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
          <h1>Document Highlight</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Navigation &amp; Visual Enhancements</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Auto-highlighting of all references to the symbol at the cursor
            position</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/documentHighlight</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span
              >:documentHighlightProvider<span class="route-arrow">→</span
              >native overlay application</code
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
                      <td>Auto-highlights symbol on cursor stop</td>
                      <td>
                        <code>eglot</code> natively triggers
                        <code>textDocument/documentHighlight</code> on cursor
                        idle.
                      </td>
                    </tr>
                    <tr>
                      <td>Highlights read/write occurrences differently</td>
                      <td>
                        <code>eglot</code> parses the <code>kind</code>
                        (read/write/text) from the LSP response and applies
                        distinct faces.
                      </td>
                    </tr>
                    <tr>
                      <td>Highlight clears on cursor move</td>
                      <td>
                        Overlays are automatically destroyed when the cursor
                        moves to a new symbol or buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Works across the entire visible buffer</td>
                      <td>
                        <code>eglot</code> requests highlights for the current
                        file scope and renders them as buffer overlays.
                      </td>
                    </tr>
                    <tr>
                      <td>Fallback when LSP is slow/disconnected</td>
                      <td>
                        Native
                        <code>isearch-forward-symbol-at-point</code> (<kbd
                          >M-s .</kbd
                        >) provides instant local regex-based highlighting.
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
                    Natively routes
                    <code>textDocument/documentHighlight</code> payloads to the
                    overlay engine without requiring manual hook registration.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(187, 154, 247, 0.1); color: var(--purple);"
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
                      <div class="eco-sub">Fallback Parser</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides the underlying structural awareness for local
                    fallback highlighting, ensuring that even without LSP,
                    symbol boundaries are respected.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);"
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
                      <div class="eco-sub">Navigation Bridge</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    If the user needs to navigate the highlighted occurrences,
                    <kbd>M-?</kbd> (<code>xref-find-references</code>) instantly
                    bridges the visual highlight to a searchable dropdown.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="background: rgba(158, 206, 106, 0.1); color: var(--green);"
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
                      <div class="eco-name">doom-themes</div>
                      <div class="eco-sub">Visual Styling</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Ensures the highlight is visible but recessive enough not to
                    compete with <code>hl-line-mode</code> or syntax
                    highlighting.
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
                      Built-in. Queries
                      <code>textDocument/documentHighlight</code> on cursor idle
                      and parses the returned range array.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(187, 154, 247, 0.1); color: var(--purple);"
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
                    <div class="stack-name">Overlay Engine</div>
                    <div class="stack-role">Rendering</div>
                    <div class="stack-desc">
                      Natively applies overlays to matching symbols via its
                      <code>:documentHighlightProvider</code> capability.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(125, 207, 255, 0.1); color: var(--cyan);"
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
                    <div class="stack-name">highlight face</div>
                    <div class="stack-role">Visual Styling</div>
                    <div class="stack-desc">
                      The standard Emacs face applied to the highlighted ranges,
                      customizable via theme.
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
                    style="background: rgba(158, 206, 106, 0.1); color: var(--green);"
                  >
                    <svg
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      stroke-width="2"
                      aria-hidden="true"
                    >
                      <circle cx="12" cy="12" r="10" />
                      <path d="M12 8v4l3 3" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">eglot--managed-mode</div>
                    <div class="stack-role">Performance Guard</div>
                    <div class="stack-desc">
                      Automatically enables highlighting in LSP-managed buffers,
                      respecting idle delays to prevent spam.
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
                      <td>Cycle through highlights</td>
                      <td><code>xref-find-references</code></td>
                      <td><kbd>M-?</kbd></td>
                      <td>
                        If more context is needed, jump to the full reference
                        list.
                      </td>
                    </tr>
                    <tr>
                      <td>Jump to next occurrence</td>
                      <td><code>isearch-forward-symbol-at-point</code></td>
                      <td><kbd>M-s .</kbd></td>
                      <td>
                        Native Emacs fallback to cycle through local occurrences
                        if LSP is disconnected.
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
                    <span class="fname">init-highlight.el</span>
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
;; EGLOT DOCUMENT HIGHLIGHT (Built-in)
;; ==========================================
;; Eglot natively handles `textDocument/documentHighlight` via its
;; `:documentHighlightProvider` capability. When the server supports it,
;; Eglot automatically applies overlays to matching symbols on cursor idle.
;; No explicit minor mode hooks or third-party packages are required.

;; ==========================================
;; PERFORMANCE TUNING (Global)
;; ==========================================
;; The global `jit-lock-defer-time` of 0.05 (50ms) perfectly guards
;; eglot's document highlight queries. This prevents the LSP server
;; from being spammed during rapid cursor movement.
(setq jit-lock-defer-time 0.05)

;; ==========================================
;; VISUAL STYLING (Tokyo Night Synergy)
;; ==========================================
(custom-set-faces
 '(highlight ((t (:background "#292e42" :foreground "#c0caf5" :weight bold)))))</code></pre>
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
                  <h4>✓ eglot native · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Works exclusively with <code>eglot</code>.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Lightweight overlays, respects
                        <code>jit-lock-defer-time</code> (50ms).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Dependencies</span>
                      <span class="val"
                        >Zero. Built directly into <code>eglot.el</code>.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui / third-party highlighters · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Hard-bound to the
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Requires forbidden
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Heavy sideline rendering, prone to main-thread
                        blocking.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Dependencies</span>
                      <span class="val"
                        >Requires <code>lsp-ui</code> and its complex
                        child-frame pipeline.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">jit-lock-defer-time Synergy</div>
                  <p class="enh-desc">
                    The global <code>jit-lock-defer-time</code> of
                    <code>0.05</code> (50ms) perfectly guards
                    <code>eglot</code>'s document highlight queries. Prevents
                    LSP spam during rapid cursor movement, eliminating
                    micro-stutters.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Native Overlay Efficiency</div>
                  <p class="enh-desc">
                    Emacs 31's C-level overlay rendering is highly optimized.
                    <code>eglot</code> draws highlight rectangles with zero
                    redisplay lag, even in files with hundreds of occurrences.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Treesit Fallback Readiness</div>
                  <p class="enh-desc">
                    If the LSP server stalls, <kbd>M-s .</kbd>
                    (<code>isearch-forward-symbol-at-point</code>) utilizes
                    <code>treesit</code> or syntax tables to highlight local
                    occurrences instantly without network latency.
                  </p>
                </div>
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
