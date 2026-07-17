Filename: hover-info-childframe.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Hover Info (Childframe)</title>
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
        <button class="nav active" data-tip="Hover Info" aria-current="page">
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
          <span>Completion &amp; Intelligence</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Hover Info (Childframe)</span>
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
          <h1>Hover Info (Childframe Parity)</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Completion &amp; Intelligence</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span
            >Floating tooltip with rich markdown, type info, and
            signatures</span
          >
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/hover</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span>eldoc<span
                class="route-arrow"
                >→</span
              >eldoc-box (childframe) OR *eldoc* buffer (TTY)</code
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
                      <td>Floating tooltip on cursor idle</td>
                      <td>
                        <code>eldoc-box-hover-at-point-mode</code> spawns
                        childframe after <code>eldoc-idle-delay</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Rich markdown rendering</td>
                      <td>
                        <code>markdown-ts-mode</code> fontifies code blocks
                        inside the <code>eldoc-box</code> childframe.
                      </td>
                    </tr>
                    <tr>
                      <td>Tooltip disappears on cursor move</td>
                      <td>
                        <code>eldoc-box-clear-after-use t</code> destroys the
                        childframe instantly.
                      </td>
                    </tr>
                    <tr>
                      <td>Single-line hints in status bar</td>
                      <td>
                        <code>eldoc-box-only-multi-line t</code> keeps 1-liners
                        in the echo area.
                      </td>
                    </tr>
                    <tr>
                      <td>Scroll long documentation</td>
                      <td>
                        <kbd>C-M-v</kbd> / <kbd>C-M-S-v</kbd> scrolls the
                        <code>eldoc-box</code> childframe window.
                      </td>
                    </tr>
                    <tr>
                      <td>Hover on completion candidate</td>
                      <td>
                        <code>corfu-popupinfo-toggle</code> (<kbd>M-h</kbd>)
                        spawns a childframe for
                        <code>completionItem/resolve</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Works over SSH / Terminal</td>
                      <td>
                        Emacs 31
                        <code>eldoc-echo-area-prefer-doc-buffer</code> routes to
                        <code>*eldoc*</code> buffer natively.
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
                    Drives <code>textDocument/hover</code>, returning Markdown
                    payloads natively via
                    <code>eglot-hover-eldoc-function</code>.
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
                          d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                        />
                        <polyline points="14 2 14 8 20 8" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eldoc</div>
                      <div class="eco-sub">Documentation Router</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Aggregates hover payloads and routes them to the active
                    display backend (childframe or TTY fallback).
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
                        <rect x="3" y="3" width="18" height="18" rx="2" />
                        <path d="M9 9h6v6H9z" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eldoc-box</div>
                      <div class="eco-sub">GUI Rendering Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Spawns a floating childframe anchored to the cursor,
                    rendering rich markdown without shifting buffer text.
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
                      <div class="eco-name">markdown-ts-mode</div>
                      <div class="eco-sub">Markdown Fontification</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Provides C-level tree-sitter syntax highlighting for code
                    blocks inside the hover tooltip.
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
                      Built-in. Drives <code>textDocument/hover</code>,
                      returning Markdown payloads natively.
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
                    <div class="stack-name">eldoc</div>
                    <div class="stack-role">Documentation Router</div>
                    <div class="stack-desc">
                      Built-in. Aggregates hover payloads and routes them to the
                      active display backend.
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
                    <div class="stack-name">eldoc-box</div>
                    <div class="stack-role">GUI Rendering Engine</div>
                    <div class="stack-desc">
                      Spawns a floating childframe anchored to the cursor,
                      rendering rich markdown without shifting buffer text.
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
                    <div class="stack-name">eldoc (TTY Fallback)</div>
                    <div class="stack-role">TTY Fallback Engine</div>
                    <div class="stack-desc">
                      Routes payloads to an ephemeral <code>*eldoc*</code>
                      buffer when childframes are unavailable (e.g., over SSH).
                    </div>
                  </div>
                </div>
                <div class="stack-card">
                  <div
                    class="stack-ic"
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
                      <circle cx="12" cy="12" r="10" />
                      <path d="M12 16v-4M12 8h.01" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">markdown-ts-mode</div>
                    <div class="stack-role">Markdown Fontification</div>
                    <div class="stack-desc">
                      Built-in. Provides C-level tree-sitter syntax highlighting
                      for code blocks inside the hover tooltip.
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
                      <td>Hover at point (keyboard)</td>
                      <td><code>eldoc</code></td>
                      <td><kbd>K</kbd> (Evil normal)</td>
                      <td>
                        Triggers <code>textDocument/hover</code> and spawns the
                        childframe.
                      </td>
                    </tr>
                    <tr>
                      <td>Help at point</td>
                      <td><code>help-at-point</code></td>
                      <td><kbd>C-h .</kbd></td>
                      <td>
                        Native Emacs help surfacing via
                        <code>eldoc-help-at-pt</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Scroll hover tooltip</td>
                      <td>
                        <code>eldoc-box-scroll-up</code> /
                        <code>down</code>
                      </td>
                      <td><kbd>C-M-v</kbd> / <kbd>C-M-S-v</kbd></td>
                      <td>
                        Scrolls the childframe when docstrings exceed the
                        viewport.
                      </td>
                    </tr>
                    <tr>
                      <td>Toggle candidate docs</td>
                      <td><code>corfu-popupinfo-toggle</code></td>
                      <td><kbd>M-h</kbd> (in <code>corfu-map</code>)</td>
                      <td>
                        Shows/hides childframe docs for the active completion
                        candidate.
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
                    <span class="fname">init-hover-childframe.el</span>
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
;; 1. ELDOC-BOX (GUI Childframe Hover)
;; ==========================================
(use-package eldoc-box
  :ensure t
  :after eglot
  :custom
  ;; Clear the childframe immediately when the cursor moves off the symbol.
  (eldoc-box-clear-after-use t)
  ;; Only spawn the childframe for multi-line payloads.
  (eldoc-box-only-multi-line t)
  ;; Position the childframe slightly offset from the cursor.
  (eldoc-box-offset '(10 10 10))
  :custom-face
  ;; Tokyo Night synergy: Match the childframe background and border.
  (eldoc-box-border ((t (:background "#292e42"))))
  (eldoc-box-default-face ((t (:background "#1a1b26" :foreground "#c0caf5"))))
  :config
  ;; Enable hover-at-point tracking.
  (eldoc-box-hover-at-point-mode 1))

;; ==========================================
;; 2. ELDOC CORE (Emacs 31 TTY Fallback &amp; Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-help-at-pt t)
  ;; TTY Fallback: Route long docs to the ephemeral `*eldoc*` buffer.
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p t)
  (eldoc-idle-delay 0.5)
  :config
  (global-eldoc-mode 1))</code></pre>
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
                  <h4>✓ eldoc-box · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Rendering</span>
                      <span class="val"
                        >Childframe (Floating GUI window). Floats above text;
                        zero layout shift.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Works with <i>any</i> eldoc backend (eglot).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Use Case</span>
                      <span class="val"
                        >Hover Info (VS Code Tooltip parity).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Lightweight, respects
                        <code>eldoc-box-only-multi-line</code>.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui-doc / peek · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Rendering</span>
                      <span class="val"
                        ><code>lsp-ui-doc</code> requires forbidden lsp-mode;
                        <code>peek</code> uses overlays (shifts text).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Protocol</span>
                      <span class="val"
                        >Hard-bound to <code>lsp-mode</code> ecosystem or
                        designed for inline panels.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Use Case</span>
                      <span class="val"
                        ><code>peek</code> is strictly for Peek Definition
                        (Alt+F12).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Heavy child-frame pipeline or high redisplay
                        overhead.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">PGTK Child-Frame Pixel Accuracy</div>
                  <p class="enh-desc">
                    Emacs 31 fixes severe child-frame positioning bugs on
                    Wayland (PGTK builds). <code>eldoc-box</code> tooltips now
                    anchor perfectly to the cursor baseline without drifting.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">Native TTY Degradation</div>
                  <p class="enh-desc">
                    If <code>eldoc-box</code> detects a TTY frame, Emacs 31's
                    native <code>eldoc-echo-area-prefer-doc-buffer</code>
                    seamlessly intercepts the payload and routes it to a split
                    <code>*eldoc*</code> buffer.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">markdown-ts-mode Integration</div>
                  <p class="enh-desc">
                    Emacs 31's native tree-sitter markdown mode fontifies the
                    childframe buffer at C-speed, providing syntax-highlighted
                    code blocks inside the hover tooltip.
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

Filename: signature-help.html

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Emacs IDE — Signature Help</title>
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
        <button
          class="nav active"
          data-tip="Signature Help"
          aria-current="page"
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
          <span>Completion &amp; Intelligence</span
          ><span class="s" aria-hidden="true">/</span>
          <span class="cur" aria-current="page">Signature Help</span>
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
          <h1>Signature Help</h1>
          <span class="status" role="status">Working</span>
        </div>
        <div class="category">Completion &amp; Intelligence</div>
        <div class="parity">
          <b>VS Code Parity</b>
          <span>Parameter hints shown while typing inside a function call</span>
        </div>
        <div class="meta-bar">
          <div class="meta-item">
            <span class="k">LSP</span>
            <code>textDocument/signatureHelp</code>
          </div>
          <div class="meta-item">
            <span class="k">Routing</span>
            <code
              >eglot<span class="route-arrow">→</span
              >eglot-signature-eldoc-function<span class="route-arrow">→</span
              >eldoc (echo area / ephemeral buffer)</code
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
                      <td>Auto-trigger on <code>(</code> or <code>,</code></td>
                      <td>
                        <code>eglot</code> registers trigger characters via
                        <code>textDocument/signatureHelp</code> capabilities;
                        <code>eldoc</code> fires on
                        <code>post-command-hook</code>.
                      </td>
                    </tr>
                    <tr>
                      <td>Highlights active parameter</td>
                      <td>
                        <code>eglot</code> applies
                        <code>eldoc-highlight-function-argument</code> face to
                        the active parameter index.
                      </td>
                    </tr>
                    <tr>
                      <td>Cycles through overloads</td>
                      <td>
                        <code>eldoc</code> natively supports multiple
                        signatures; <kbd>C-h .</kbd> or arrow keys can cycle if
                        the server returns an array of signatures.
                      </td>
                    </tr>
                    <tr>
                      <td>Floating tooltip for long signatures</td>
                      <td>
                        Emacs 31
                        <code>eldoc-echo-area-prefer-doc-buffer t</code> routes
                        long signatures to the <code>*eldoc*</code> buffer
                        without shifting window layouts.
                      </td>
                    </tr>
                    <tr>
                      <td>Manual trigger shortcut</td>
                      <td>
                        <kbd>C-h .</kbd> (<code>help-at-point</code>) or
                        <kbd>K</kbd> (<code>eldoc</code>).
                      </td>
                    </tr>
                    <tr>
                      <td>Dismiss on cursor move</td>
                      <td>
                        <code>eldoc</code> automatically clears the echo area or
                        hides the ephemeral buffer when the cursor leaves the
                        callable scope.
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
                    Intercepts trigger characters (<code>(</code>,
                    <code>,</code>), queries
                    <code>textDocument/signatureHelp</code>, and parses the
                    active parameter index.
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
                          d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
                        />
                        <polyline points="14 2 14 8 20 8" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eldoc</div>
                      <div class="eco-sub">Documentation Router</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Aggregates the signature payload and routes it to the echo
                    area or ephemeral buffer based on length constraints.
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
                          d="M12 2L2 7l10 5 10-5-10-5zM2 17l10 5 10-5M2 12l10 5 10-5"
                        />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">markdown-ts-mode</div>
                      <div class="eco-sub">Rendering Engine</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    Highlights the active parameter using the
                    <code>eldoc-highlight-function-argument</code> face and
                    fontifies code blocks in the <code>*eldoc*</code> buffer.
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
                        <circle cx="12" cy="12" r="10" />
                        <path d="M12 8v4l3 3" />
                      </svg>
                    </div>
                    <div>
                      <div class="eco-name">eldoc-documentation-functions</div>
                      <div class="eco-sub">Trigger Mechanism</div>
                    </div>
                  </div>
                  <p class="eco-desc">
                    <code>eglot</code> injects
                    <code>eglot-signature-eldoc-function</code> into this hook,
                    triggering automatically on
                    <code>post-command-hook</code> when inside a callable scope.
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
                      Built-in. Intercepts trigger characters and parses the
                      active parameter index from the LSP payload.
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
                    <div class="stack-name">eldoc</div>
                    <div class="stack-role">Documentation Router</div>
                    <div class="stack-desc">
                      Built-in. Aggregates the signature payload and routes it
                      to the echo area or ephemeral buffer.
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
                    <div class="stack-name">eldoc + markdown-ts-mode</div>
                    <div class="stack-role">Rendering Engine</div>
                    <div class="stack-desc">
                      Highlights the active parameter and fontifies code blocks
                      in the <code>*eldoc*</code> buffer at C-speed.
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
                      <circle cx="12" cy="12" r="10" />
                      <path d="M12 8v4l3 3" />
                    </svg>
                  </div>
                  <div class="stack-ct">
                    <div class="stack-name">eldoc-documentation-functions</div>
                    <div class="stack-role">Trigger Mechanism</div>
                    <div class="stack-desc">
                      Hook where <code>eglot</code> injects its signature
                      function to trigger automatically on
                      <code>post-command-hook</code>.
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
                      <td>Manual signature trigger</td>
                      <td><code>eldoc</code></td>
                      <td><kbd>C-h .</kbd> / <kbd>K</kbd></td>
                      <td>
                        Forces a <code>textDocument/signatureHelp</code> query
                        if the automatic trigger was missed.
                      </td>
                    </tr>
                    <tr>
                      <td>Scroll long signature</td>
                      <td><code>scroll-other-window</code></td>
                      <td><kbd>C-M-v</kbd></td>
                      <td>
                        Scrolls the <code>*eldoc*</code> ephemeral buffer when a
                        signature exceeds the echo area.
                      </td>
                    </tr>
                    <tr>
                      <td>Scroll signature (back)</td>
                      <td><code>scroll-other-window-down</code></td>
                      <td><kbd>C-M-S-v</kbd></td>
                      <td>
                        Reverse scroll for massive C++/Rust generic signatures.
                      </td>
                    </tr>
                    <tr>
                      <td>Help at point</td>
                      <td><code>help-at-point</code></td>
                      <td><kbd>C-h .</kbd></td>
                      <td>
                        Native Emacs help surfacing that integrates with eldoc
                        payloads.
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
                    <span class="fname">init-signature-help.el</span>
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
;; 1. ELDOC CORE (Signature &amp; Hover Routing)
;; ==========================================
(use-package eldoc
  :ensure nil
  :custom
  ;; Emacs 31 NEW: When a signature exceeds the echo area,
  ;; automatically route it to the ephemeral `*eldoc*` buffer.
  (eldoc-echo-area-prefer-doc-buffer t)
  ;; Allow multi-line signatures in the echo area if they fit within 3 lines.
  (eldoc-echo-area-use-multiline-p t)
  ;; Idle delay before triggering signature/hover queries.
  (eldoc-idle-delay 0.5)
  ;; Emacs 31 NEW: Surface `help-at-point-kbd-string` through the eldoc pipeline.
  (eldoc-help-at-pt t)
  :config
  ;; Enable eldoc globally. eglot-managed buffers automatically inject
  ;; `eglot-signature-eldoc-function` into `eldoc-documentation-functions`.
  (global-eldoc-mode 1))</code></pre>
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
                  <h4>✓ eldoc native · Chosen</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Works with <i>any</i> eldoc backend (eglot, native
                        elisp).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">UI Physics</span>
                      <span class="val"
                        >Echo area (fast) or ephemeral buffer (no layout
                        shift).</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >Zero additional packages, native C-level echo
                        area.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Maintenance</span>
                      <span class="val"
                        >Maintained by GNU Emacs core team.</span
                      >
                    </div>
                  </div>
                </div>
                <div class="vs-card no">
                  <h4>✕ lsp-ui-sideline / lsp-signature · Rejected</h4>
                  <div class="vs-list">
                    <div class="vs-row">
                      <span class="lab">Coupling</span>
                      <span class="val"
                        >Hard-bound to the forbidden
                        <code>lsp-mode</code> ecosystem.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">UI Physics</span>
                      <span class="val"
                        >Renders in margins/sidelines (causes text shifting) or
                        heavy child-frame overlays.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Performance</span>
                      <span class="val"
                        >High redisplay overhead on every keystroke or
                        child-frame rendering latency.</span
                      >
                    </div>
                    <div class="vs-row">
                      <span class="lab">Maintenance</span>
                      <span class="val"
                        >Stale — tracks <code>lsp-mode</code> lifecycle.</span
                      >
                    </div>
                  </div>
                </div>
              </div>
              <div class="sec-title">Emacs 31 Specific Enhancements</div>
              <div class="grid-2">
                <div class="enh-card g">
                  <div class="enh-title">
                    eldoc-echo-area-prefer-doc-buffer (NEW)
                  </div>
                  <p class="enh-desc">
                    Automatically routes oversized C++ template signatures or
                    Rust generic bounds to a dedicated, scrollable
                    <code>*eldoc*</code> buffer, preventing severe UI jitter.
                  </p>
                </div>
                <div class="enh-card p">
                  <div class="enh-title">markdown-ts-mode Integration</div>
                  <p class="enh-desc">
                    When routed to the <code>*eldoc*</code> buffer, Emacs 31's
                    native tree-sitter markdown mode fontifies embedded code
                    blocks and type annotations at C-speed.
                  </p>
                </div>
                <div class="enh-card y">
                  <div class="enh-title">
                    elisp-eldoc-funcall-with-docstring
                  </div>
                  <p class="enh-desc">
                    For Emacs Lisp buffers, Emacs 31's native eldoc engine
                    merges the function signature with its docstring in a
                    single, highly optimized payload.
                  </p>
                </div>
                <div class="enh-card g">
                  <div class="enh-title">TTY-Safe Degradation</div>
                  <p class="enh-desc">
                    Degrades gracefully to the echo area or standard window
                    splits over SSH/TTY, ensuring signature help is always
                    accessible in terminal environments.
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
