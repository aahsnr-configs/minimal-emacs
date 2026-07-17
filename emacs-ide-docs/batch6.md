Filename: document-symbols.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Document Symbols</title>
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
          <span>Diagnostics &amp; Symbols</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Document Symbols</span>
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
          <h1>Document Symbols / Outline View</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Diagnostics &amp; Symbols</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Outline sidebar, Ctrl+Shift+O (Go to Symbol in File), and top
            breadcrumb bar</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/documentSymbol</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>imenu<span
                class="route-arrow"
                >→</span
              >consult-eglot-symbols OR breadcrumb</code
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
                      <td><kbd>Ctrl+Shift+O</kbd> opens file outline</td>
                      <td>
                        <kbd>SPC c s</kbd> (<code>consult-eglot-symbols</code>)
                        opens minibuffer outline with live preview.
                      </td>
                    </tr>
                    <tr>
                      <td>Fuzzy filter symbols by name</td>
                      <td>
                        <code>consult</code> + <code>orderless</code> allows
                        space-separated, out-of-order fuzzy matching.
                      </td>
                    </tr>
                    <tr>
                      <td>Click symbol to jump to definition</td>
                      <td>
                        <kbd>RET</kbd> in <code>consult</code> dropdown or
                        <kbd>mouse-1</kbd> on breadcrumb segments.
                      </td>
                    </tr>
                    <tr>
                      <td>Top bar shows <code>file › class › method</code></td>
                      <td>
                        <code>breadcrumb-mode</code> renders this exact
                        hierarchy in the header line.
                      </td>
                    </tr>
                    <tr>
                      <td>Workspace-wide symbol search (<kbd>Ctrl+T</kbd>)</td>
                      <td>
                        <kbd>C-u SPC c s</kbd> or <kbd>SPC s w</kbd> triggers
                        <code>workspace/symbol</code> via <code>consult</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Icons for classes/functions in outline</td>
                      <td>
                        <code>nerd-icons-completion</code> automatically injects
                        glyphs into the <code>consult</code> dropdown.
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
                    Natively maps <code>textDocument/documentSymbol</code>
                    responses to the buffer-local
                    <code>imenu-create-index-function</code>.
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
                      <div class="eco-sub">Preview Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>consult-eglot-symbols</code> intercepts the
                    <code>imenu</code> index, transforming it into a searchable,
                    preview-enabled Vertico menu.
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
                        <path d="M4 6h16M4 12h16M4 18h10" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">orderless</div>
                      <div class="eco-sub">Filtering Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides the fuzzy matching engine, allowing queries like
                    <code>init conf</code> to instantly find
                    <code>initialize_configuration</code>.
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
                          d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">breadcrumb</div>
                      <div class="eco-sub">Spatial Orientation</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides the persistent, clickable breadcrumb trail at the
                    top of the window, updating dynamically as the cursor moves
                    through different scopes.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(224, 175, 104, 0.1);
                        color: var(--yellow);
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
                    <div>
                      <div class="eco-name">nerd-icons-completion</div>
                      <div class="eco-sub">Visual Glyphs</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Injects visual glyphs (e.g., 🏛️ for classes, ⚙️ for
                    functions) into the <code>consult</code> dropdown, matching
                    the VS Code outline sidebar aesthetic.
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
                      <code>textDocument/documentSymbol</code> and maps the
                      hierarchical response to Emacs' native
                      <code>imenu</code> index.
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
                      <circle cx="11" cy="11" r="8" />
                      <line x1="21" y1="21" x2="16.65" y2="16.65" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">consult-eglot</div>
                    <div class="stack-role">Outline Engine</div>
                    <div class="stack-desc">
                      Renders a live-preview, fuzzy-filtered outline tree in the
                      minibuffer using <code>vertico</code> and
                      <code>orderless</code>.
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
                    <div class="stack-name">breadcrumb</div>
                    <div class="stack-role">Breadcrumb Engine</div>
                    <div class="stack-desc">
                      GNU ELPA package that displays a clickable, hierarchical
                      path (e.g., <code>file › class › method</code>) in the
                      header line.
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
                      <path
                        d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                      />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">treesit</div>
                    <div class="stack-role">Fallback Parser</div>
                    <div class="stack-desc">
                      Built-in. Provides native AST-based <code>imenu</code>
                      generation if the LSP server is slow or disconnected.
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
                      <td>Go to symbol in file</td>
                      <td><code>consult-eglot-symbols</code></td>
                      <td><kbd>SPC c s</kbd></td>
                      <td>
                        Opens a vertico-powered, preview-enabled outline
                        dropdown.
                      </td>
                    </tr>
                    <tr>
                      <td>Go to symbol in workspace</td>
                      <td>
                        <code>consult-eglot-symbols</code> (with <kbd>C-u</kbd>)
                      </td>
                      <td><kbd>C-u SPC c s</kbd></td>
                      <td>
                        Searches across the entire project via
                        <code>workspace/symbol</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Toggle breadcrumbs</td>
                      <td><code>breadcrumb-mode</code></td>
                      <td><kbd>SPC t b</kbd></td>
                      <td>
                        Enables the clickable path bar at the top of the buffer.
                      </td>
                    </tr>
                    <tr>
                      <td>Native imenu jump</td>
                      <td><code>imenu</code></td>
                      <td><kbd>M-g M-i</kbd></td>
                      <td>
                        Fallback to native Emacs imenu if LSP is unavailable.
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
                    <span class="fname">init-symbols.el</span>
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
;; 1. BREADCRUMB (Header-line Breadcrumbs)
;; ==========================================
(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode)
  :custom
  ;; Optional: Customize the separator for breadcrumbs
  (breadcrumb-imenu-crumb-separator " › ")
  (breadcrumb-project-crumb-separator " / "))

;; ==========================================
;; 2. CONSULT-EGLOT (Outline &amp; Workspace Symbols)
;; ==========================================
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (("M-g s" . consult-eglot-symbols)      ; Go to symbol in file
         ("M-g S" . consult-eglot-symbols))     ; With C-u, goes to workspace symbol
  :config
  ;; Ensure consult-eglot uses the current project root for workspace symbols
  (setq consult-eglot-symbols-kind nil))       ; nil = all kinds, or filter like '(class function)

;; ==========================================
;; 3. GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "c" '(:ignore t :wk "code")
  "c s" '(consult-eglot-symbols :wk "Document symbols (outline)")
  "t" '(:ignore t :wk "toggle")
  "t b" '(breadcrumb-mode :wk "Toggle breadcrumbs"))</code></pre>
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
                  <h4>✓ eglot + consult + breadcrumb · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Works exclusively with <code>eglot</code> and native
                        <code>imenu</code>.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Preview</span>
                      <span class="val"
                        ><code>consult</code> provides live, asynchronous buffer
                        previews while scrolling the symbol tree.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Breadcrumbs</span>
                      <span class="val"
                        >Native <code>header-line-format</code> via
                        <code>breadcrumb</code>, zero third-party
                        dependencies.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui / lsp-mode · Rejected</h4>
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
                      <span class="lab">Preview</span>
                      <span class="val"
                        ><code>lsp-ui</code> uses heavy, custom child-frame
                        rendering that can stutter.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Breadcrumbs</span>
                      <span class="val"
                        >Requires <code>lsp-mode</code>'s custom breadcrumb
                        implementation.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">
                    treesit-aggregated-simple-imenu-settings
                  </div>
                  <p class="enh-desc">
                    Emacs 31 introduces native support for multi-language imenu
                    trees. For mixed-language buffers (e.g.,
                    <code>mhtml-ts-mode</code>, <code>php-ts-mode</code>), the
                    outline view seamlessly aggregates symbols from HTML, CSS,
                    and PHP tree-sitter parsers without relying solely on the
                    LSP server.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Enhanced breadcrumb integration</div>
                  <p class="enh-desc">
                    The breadcrumb bar in Emacs 31 is more robust, correctly
                    handling deep nesting and long symbol names by truncating
                    gracefully or allowing horizontal scrolling within the
                    header line.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Native Fallback Parity</div>
                  <p class="enh-desc">
                    If the LSP server crashes or is slow to respond, Emacs 31's
                    <code>treesit</code> automatically populates the
                    <code>imenu</code> index, ensuring
                    <code>consult-eglot-symbols</code> (which falls back to
                    <code>imenu</code>) still provides a highly accurate,
                    AST-aware outline view with zero network latency.
                  </p>
                </div>
                <div class="enh-card g">
                  <div class="enh-title">Eglot Imenu Enrichment</div>
                  <p class="enh-desc">
                    As of <code>eglot</code> 1.14+, managed buffers receive
                    extra region info added to the <code>imenu</code> index,
                    allowing <code>breadcrumb</code> to show "richer", deeply
                    nested paths (e.g., <code>Namespace › Class › Method</code>)
                    rather than flat lists.
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

Filename: workspace-symbol-search.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Workspace Symbol Search</title>
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
          <span>Diagnostics &amp; Symbols</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Workspace Symbol Search</span>
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
          <h1>Workspace Symbol Search</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Diagnostics &amp; Symbols</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Ctrl+T "Go to Symbol in Workspace" — fuzzy search symbols across
            the whole project</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>workspace/symbol</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>consult-eglot-symbols<span
                class="route-arrow"
                >→</span
              >vertico + orderless</code
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
                      <td><kbd>Ctrl+T</kbd> opens workspace symbol search</td>
                      <td>
                        <kbd>C-u SPC s w</kbd> or <kbd>C-u M-g S</kbd> triggers
                        workspace-wide <code>consult-eglot-symbols</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Fuzzy filter symbols by name</td>
                      <td>
                        <code>orderless</code> matching styles allow
                        space-separated, out-of-order fuzzy matching.
                      </td>
                    </tr>
                    <tr>
                      <td>Live preview of symbol definition</td>
                      <td>
                        <code>consult</code> temporarily visits the file and
                        shows the definition context in a side window.
                      </td>
                    </tr>
                    <tr>
                      <td>Filter by symbol kind (class, function)</td>
                      <td>
                        <code>consult-eglot-symbols-kind</code> can be set to
                        filter specific LSP symbol kinds.
                      </td>
                    </tr>
                    <tr>
                      <td>Click/Enter to jump to target</td>
                      <td>
                        <kbd>RET</kbd> in <code>consult</code> dropdown jumps to
                        the exact location via <code>xref</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Icons for classes/functions in list</td>
                      <td>
                        <code>nerd-icons-completion</code> automatically injects
                        glyphs into the <code>consult</code> dropdown.
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
                    Natively implements <code>workspace/symbol</code> and
                    formats the response into a structure that
                    <code>consult</code> can easily parse.
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
                      <div class="eco-sub">Preview Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>consult-eglot-symbols</code> intercepts the payload,
                    sorts it, and provides the live preview via
                    <code>consult--buffer-preview</code>.
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
                        <path d="M4 6h16M4 12h16M4 18h10" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">orderless</div>
                      <div class="eco-sub">Filtering Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides the fuzzy matching engine, allowing queries like
                    <code>init conf</code> to instantly find
                    <code>initialize_configuration</code> across the entire
                    codebase.
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
                        <rect x="3" y="3" width="18" height="18" rx="2" />
                        <path d="M9 9h6v6H9z" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">marginalia</div>
                      <div class="eco-sub">Annotations</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Appends the file path and line number to each candidate,
                    providing crucial spatial context before jumping.
                  </p>
                </div>
                <div class="eco-card">
                  <div class="eco-top">
                    <div
                      class="eco-ic"
                      style="
                        background: rgba(224, 175, 104, 0.1);
                        color: var(--yellow);
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
                      <div class="eco-name">nerd-icons-completion</div>
                      <div class="eco-sub">Visual Glyphs</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Injects visual glyphs (e.g., 🏛️ for classes, ⚙️ for
                    functions) into the <code>consult</code> dropdown, matching
                    the VS Code outline sidebar aesthetic.
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
                      Built-in. Queries <code>workspace/symbol</code> and
                      returns a flat list of project-wide symbol candidates.
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
                      <circle cx="11" cy="11" r="8" />
                      <line x1="21" y1="21" x2="16.65" y2="16.65" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">consult-eglot-symbols</div>
                    <div class="stack-role">Preview Engine</div>
                    <div class="stack-desc">
                      Intercepts the LSP payload and renders it in the
                      minibuffer with live, asynchronous buffer previews.
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
                    <div class="stack-name">orderless</div>
                    <div class="stack-role">Filtering Engine</div>
                    <div class="stack-desc">
                      Provides space-separated, out-of-order fuzzy matching
                      (e.g., typing <code>usr cnt</code> matches
                      <code>UserContext</code>).
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
                      <rect x="2" y="7" width="20" height="14" rx="2" />
                      <path d="M16 21V5a2 2 0 0 0-2-2h-4a2 2 0 0 0-2 2v16" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">vertico</div>
                    <div class="stack-role">UI Renderer</div>
                    <div class="stack-desc">
                      Displays the filtered candidates in a clean, vertically
                      scrolling list with marginalia annotations.
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
                      <td>Workspace symbol search</td>
                      <td><code>consult-eglot-symbols</code></td>
                      <td><kbd>C-u SPC s w</kbd></td>
                      <td>
                        Prefix argument (<kbd>C-u</kbd>) forces workspace-wide
                        search instead of buffer-local.
                      </td>
                    </tr>
                    <tr>
                      <td>Buffer-local symbol search</td>
                      <td><code>consult-eglot-symbols</code></td>
                      <td><kbd>SPC c s</kbd></td>
                      <td>
                        Default behavior (no prefix) searches only the current
                        file via <code>textDocument/documentSymbol</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Built-in workspace search</td>
                      <td><code>eglot-workspace-symbols</code></td>
                      <td><kbd>M-x eglot-workspace-symbols</kbd></td>
                      <td>Fallback native command without live preview.</td>
                    </tr>
                    <tr>
                      <td>Apropos search (fallback)</td>
                      <td><code>xref-find-apropos</code></td>
                      <td><kbd>SPC c A</kbd></td>
                      <td>
                        Searches all registered xref backends (including eglot)
                        for a regex pattern.
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
                    <span class="fname">init-workspace-symbols.el</span>
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
;; CONSULT-EGLOT (Outline &amp; Workspace Symbols)
;; ==========================================
(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (("M-g s" . consult-eglot-symbols)      ; Go to symbol in file
         ("M-g S" . consult-eglot-symbols))     ; With C-u, goes to workspace symbol
  :config
  ;; Ensure consult-eglot uses the current project root for workspace symbols
  (setq consult-eglot-symbols-kind nil))       ; nil = all kinds, or filter like '(class function)

;; ==========================================
;; GENERAL.EL KEYBINDINGS (registered eagerly)
;; ==========================================
(ar/global-leader
  "s" '(:ignore t :wk "search")
  "s w" '(consult-eglot-symbols :wk "Workspace symbols (use C-u)"))

;; Note: To explicitly trigger the workspace search without remembering
;; the prefix, you can bind a dedicated wrapper:
(defun ar/consult-eglot-workspace-symbols ()
  "Force workspace-wide symbol search via consult-eglot."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'consult-eglot-symbols)))

(ar/global-leader
  "s W" '(ar/consult-eglot-workspace-symbols :wk "Workspace symbols (force)"))</code></pre>
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
                  <h4>✓ eglot + consult · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Works exclusively with <code>eglot</code> and native
                        <code>xref</code>.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Honors the <code>eglot</code>-only stack mandate.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Preview</span>
                      <span class="val"
                        ><code>consult</code> provides live, asynchronous buffer
                        previews while scrolling the symbol tree.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Filtering</span>
                      <span class="val"
                        >Integrates seamlessly with <code>orderless</code> for
                        out-of-order fuzzy matching.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui / lsp-mode · Rejected</h4>
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
                      <span class="lab">Preview</span>
                      <span class="val"
                        ><code>lsp-ui</code> uses heavy, custom child-frame
                        rendering that can stutter.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Filtering</span>
                      <span class="val"
                        >Requires custom matchers; struggles with
                        space-separated queries.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">consult-eglot Prefix Intelligence</div>
                  <p class="enh-desc">
                    The <code>consult-eglot-symbols</code> command natively
                    checks <code>(called-interactively-p 'any)</code> and the
                    <code>current-prefix-arg</code>. If a prefix is present, it
                    routes the query to <code>workspace/symbol</code>;
                    otherwise, it falls back to the faster, buffer-local
                    <code>textDocument/documentSymbol</code>. This eliminates
                    the need for two separate commands.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">treesit Fallback Parity</div>
                  <p class="enh-desc">
                    If the LSP server is disconnected or slow to respond to
                    <code>workspace/symbol</code>,
                    <code>consult-imenu-multi</code> can be used as a
                    zero-latency, AST-aware fallback that searches across all
                    open project buffers using Emacs 31's native
                    <code>treesit-aggregated-simple-imenu-settings</code>.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">Optimized Preview Debouncing</div>
                  <p class="enh-desc">
                    The <code>consult-customize</code> block applies a
                    <code>:debounce 0.4</code> to
                    <code>consult-eglot-symbols</code>, preventing the LSP
                    server from being spammed with file-read requests while
                    rapidly scrolling through hundreds of workspace candidates.
                  </p>
                </div>
                <div class="enh-card g">
                  <div class="enh-title">Marginalia Annotations</div>
                  <p class="enh-desc">
                    Emacs 31's refined <code>marginalia</code> integration
                    ensures that workspace symbol candidates display their
                    originating file path and symbol kind (e.g.,
                    <code>[Class] src/utils.ts</code>) directly in the
                    minibuffer margin, providing crucial spatial context before
                    jumping.
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
